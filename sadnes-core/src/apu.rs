use memory::Memory;
use sink::*;
use cpu::{Cpu, Interrupt, CPU_FREQUENCY};

use std::f32::consts::PI;

pub const SAMPLE_RATE: u32 = 22_000;
pub const CYCLES_PER_SAMPLE: u64 = CPU_FREQUENCY / (SAMPLE_RATE as u64);

static DUTY_CYCLE_TABLE: &'static [[u8; 8]] = &[
    [0, 1, 0, 0, 0, 0, 0, 0],
    [0, 1, 1, 0, 0, 0, 0, 0],
    [0, 1, 1, 1, 1, 0, 0, 0],
    [1, 0, 0, 1, 1, 1, 1, 1],
];

static LENGTH_TABLE: &'static [u8] = &[
    10, 254, 20,  2, 40,  4, 80,  6, 160,  8, 60, 10, 14, 12, 26, 14,
	12,  16, 24, 18, 48, 20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30,
];

static TRIANGLE_TABLE: &'static [u8] = &[
    15, 14, 13, 12, 11, 10,  9,  8,  7,  6,  5,  4,  3,  2,  1,  0,
     0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15,
];

static NOISE_TABLE: &'static [u16] = &[
    4, 8, 16, 32, 64, 96, 128, 160, 202, 254, 380, 508, 762, 1016, 2034, 4068,
];

static DMC_TABLE: &'static [u8] = &[
    214, 190, 170, 160, 143, 127, 113, 107, 95, 80, 71, 64, 53, 42, 36, 27,
];

pub struct Apu {
    cycles: u64,

    pulse_table: [f32; 31],
    tnd_table: [f32; 203],

    pulse_1: Pulse,
    pulse_2: Pulse,
    triangle: Triangle,
    noise: Noise,
    dmc: Dmc,
    frame_counter: FrameCounter,

    filter_chain: FilterChain,
}

impl Apu {
    pub fn new() -> Apu {
        let mut pulse_table = [0f32; 31];
        for n in 0..31 {
            pulse_table[n] = 95.52 / (8128.0 / (n as f32) + 100.0);
        }

        let mut tnd_table = [0f32; 203];
        for n in 0..203 {
            tnd_table[n] = 163.67 / (24329.0 / (n as f32) + 100.0);
        }

        let filter_chain = FilterChain::new()
            .add(Box::new(
                FirstOrderFilter::new_high_pass(
                    SAMPLE_RATE as f32,
                    90.0)))
            .add(Box::new(
                FirstOrderFilter::new_high_pass(
                    SAMPLE_RATE as f32,
                    440.0)))
            .add(Box::new(
                FirstOrderFilter::new_low_pass(
                    SAMPLE_RATE as f32,
                    14000.0)));

        Apu {
            cycles: 0,
            pulse_table,
            tnd_table,
            pulse_1: Pulse::new(SweepNegationType::OnesComplement),
            pulse_2: Pulse::new(SweepNegationType::TwosComplement),
            triangle: Triangle::new(),
            noise: Noise::new(),
            dmc: Dmc::new(),
            frame_counter: FrameCounter::new(),
            filter_chain,
        }
    }

    // Run for the given number of cpu cycles
    pub fn cycles(&mut self, cpu: &mut Cpu, cycles: u32, audio_frame_sink: &mut Sink<AudioFrame>) {
        for _ in 0 .. cycles {
            self.step(cpu, audio_frame_sink);
        }
    }

    fn step(&mut self, cpu: &mut Cpu, audio_frame_sink: &mut Sink<AudioFrame>) {
        self.step_timer();
        self.step_frame_counter(cpu);
        self.step_samples(audio_frame_sink);

        self.cycles += 1;
    }

    fn step_samples(&mut self, audio_frame_sink: &mut Sink<AudioFrame>) {
        if self.cycles % CYCLES_PER_SAMPLE != 0 {
           return;
        }

        audio_frame_sink.append(self.generate_sample());
    }

    fn generate_sample(&mut self) -> f32 {
        let pulse1 = self.pulse_1.output();
        let pulse2 = self.pulse_2.output();
        let triangle = self.triangle.output();
        let noise = self.noise.output();
        let dmc = self.dmc.output();

        let pulse_out = self.pulse_table[pulse1 as usize + pulse2 as usize];
        let tnd_out = self.tnd_table[3 * triangle as usize + 2 * noise as usize + dmc as usize];

        self.filter_chain.step(pulse_out + tnd_out)
    }

    fn step_frame_counter(&mut self, cpu: &mut Cpu) {
        let frame = ((self.cycles as f64) / FrameCounter::RATE) as u64;
        if frame == self.frame_counter.frame {
            return;
        }

        self.frame_counter.frame = frame;

        match self.frame_counter.mode {
            FrameCounterMode::FourStep => {
                self.frame_counter.sequence_frame = (self.frame_counter.sequence_frame + 1) % 4;
                match self.frame_counter.sequence_frame {
                    0 | 2 => {
                        self.step_envelope_and_linear_counter();
                    },
                    1 => {
                        self.step_envelope_and_linear_counter();
                        self.step_sweep();
                        self.step_length_counter();
                    },
                    3 => {
                        self.step_envelope_and_linear_counter();
                        self.step_sweep();
                        self.step_length_counter();
                        if !self.frame_counter.interrupt_inhibit {
                            cpu.request_interrupt(Interrupt::Irq);
                        }
                    },
                    _ => (),
                }
            },
            FrameCounterMode::FiveStep => {
                self.frame_counter.sequence_frame = (self.frame_counter.sequence_frame + 1) % 5;
                match self.frame_counter.sequence_frame {
                    0 | 2 => {
                        self.step_envelope_and_linear_counter();
                        self.step_sweep();
                        self.step_length_counter();
                    },
                    1 | 3 => {
                        self.step_envelope_and_linear_counter();
                    },
                    _ => (),
                }
            }
        }
    }

    fn step_length_counter(&mut self) {
        self.pulse_1.step_length_counter();
        self.pulse_2.step_length_counter();
        self.triangle.step_length_counter();
        self.noise.step_length_counter();
    }

    fn step_sweep(&mut self) {
        self.pulse_1.step_sweep();
        self.pulse_2.step_sweep();
    }

    fn step_envelope_and_linear_counter(&mut self) {
        self.pulse_1.step_envelope();
        self.pulse_2.step_envelope();
        self.triangle.step_linear_counter();
        self.noise.step_envelope();
    }

    fn step_timer(&mut self) {
        if self.cycles % 2 == 0 {
            self.pulse_1.step_timer();
            self.pulse_2.step_timer();
            self.noise.step_timer();
            self.dmc.step_timer();
        }

        self.triangle.step_timer();
    }

    fn read_status(&mut self) -> u8 {
        self.frame_counter.interrupt_inhibit = false;

        let mut status = 0x00;

        if self.pulse_1.length_counter > 0 {
            status |= 0x01;
        }

        if self.pulse_2.length_counter > 0 {
            status |= 0x02;
        }

        if self.triangle.length_counter > 0 {
            status |= 0x04;
        }

        if self.noise.length_counter > 0 {
            status |= 0x08;
        }

        if self.dmc.current_length > 0 {
            status |= 0x10;
        }

        status
    }

    fn write_status(&mut self, value: u8) {
        self.pulse_1.enable_flag = (value & 0x01) != 0;
        if !self.pulse_1.enable_flag {
            self.pulse_1.length_counter = 0;
        }

        self.pulse_2.enable_flag = (value & 0x02) != 0;
        if !self.pulse_2.enable_flag {
            self.pulse_2.length_counter = 0;
        }

        self.triangle.enable_flag = (value & 0x04) != 0;
        if !self.triangle.enable_flag {
            self.triangle.length_counter = 0;
        }

        self.noise.enable_flag = (value & 0x08) != 0;
        if !self.noise.enable_flag {
            self.noise.length_counter = 0;
        }

        self.dmc.enable_flag = (value & 0x10) != 0;
        if !self.dmc.enable_flag {
            self.dmc.current_length = 0;
        } else if self.dmc.current_length == 0 {
            self.dmc.restart();
        }
    }

    fn write_frame_counter(&mut self, value: u8) {
        self.frame_counter.mode = if value & 0x80 == 0 {
            FrameCounterMode::FourStep
        } else {
            FrameCounterMode::FiveStep
        };

        self.frame_counter.interrupt_inhibit = value & 0x40 != 0;

        if self.frame_counter.mode == FrameCounterMode::FiveStep {
            self.step_length_counter();
            self.step_sweep();
            self.step_envelope_and_linear_counter();
        }
    }
}

impl Memory for Apu {
    fn read_byte(&mut self, address: u16) -> u8 {
        if address == 0x4015 {
            self.read_status()
        } else {
            0
        }
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        match address {
            0x4000 => self.pulse_1.write_control(value),
            0x4001 => self.pulse_1.write_sweep(value),
            0x4002 => self.pulse_1.write_timer_lo(value),
            0x4003 => self.pulse_1.write_timer_hi(value),
            0x4004 => self.pulse_2.write_control(value),
            0x4005 => self.pulse_2.write_sweep(value),
            0x4006 => self.pulse_2.write_timer_lo(value),
            0x4007 => self.pulse_2.write_timer_hi(value),
            0x4008 => self.triangle.write_linear_counter(value),
            0x400A => self.triangle.write_timer_lo(value),
            0x400B => self.triangle.write_length_counter_and_timer_hi(value),
            0x400C => self.noise.write_control(value),
            0x400E => self.noise.write_mode_and_timer_period(value),
            0x400F => self.noise.write_length_counter_and_envelope_restart(value),
            0x4010 => self.dmc.write_control(value),
            0x4011 => self.dmc.write_value(value),
            0x4012 => self.dmc.write_sample_address(value),
            0x4013 => self.dmc.write_sample_length(value),
            0x4015 => self.write_status(value),
            0x4017 => self.write_frame_counter(value),
            _ => (),
        }
    }
}

enum SweepNegationType {
    OnesComplement,
    TwosComplement,
}

struct Envelope {
    enable_flag: bool,
    start_flag: bool,
    loop_flag: bool,
    constant_volume_flag: bool,
    volume: u8,
    value: u8,
    period: u8,
}

impl Envelope {
    fn new() -> Envelope {
        Envelope {
            enable_flag: false,
            start_flag: false,
            loop_flag: false,
            constant_volume_flag: false,
            volume: 0,
            value: 0,
            period: 0,
        }
    }

    fn step(&mut self) {
        if self.start_flag {
            self.start_flag = false;
            self.volume = 15;
            self.value = self.period;
        } else if self.value > 0 {
            self.value -= 1;
        } else {
            if self.volume > 0 {
                self.volume -= 1;
            } else if self.loop_flag {
                self.volume = 15;
            }

            self.value = self.period;
        }
    }
}

struct Sweep {
    enable_flag: bool,
    negate_flag: bool,
    reload_flag: bool,
    divider: u8,
    period: u8,
    shift_count: u8,
}

impl Sweep {
    fn new() -> Sweep {
        Sweep {
            enable_flag: false,
            negate_flag: false,
            reload_flag: false,
            divider: 0,
            period: 0,
            shift_count: 0,
        }
    }
}

struct Pulse {
    enable_flag: bool,
    negation_type: SweepNegationType,
    timer_value: u16,
    timer_period: u16,
    duty_mode: u8,
    duty_cycle: u8,
    length_counter_enable: bool,
    length_counter: u8,
    envelope: Envelope,
    sweep: Sweep,
    constant_volume: u8,
}

impl Pulse {
    fn new(negation_type: SweepNegationType) -> Pulse {
        Pulse {
            enable_flag: false,
            negation_type,
            timer_value: 0,
            timer_period: 0,
            duty_mode: 0,
            duty_cycle: 0,
            length_counter_enable: false,
            length_counter: 0,
            envelope: Envelope::new(),
            sweep: Sweep::new(),
            constant_volume: 0,
        }
    }

    fn write_control(&mut self, value: u8) {
        self.duty_cycle = value >> 6;
        self.length_counter_enable = (value & 0x20) == 0;
        self.envelope.loop_flag = !self.length_counter_enable;
        self.envelope.constant_volume_flag = (value & 0x10)  == 0;
        self.constant_volume = value & 0x0F;
        self.envelope.period = self.constant_volume;
        self.envelope.start_flag = true;
    }

    fn write_sweep(&mut self, value: u8) {
        self.sweep.enable_flag = (value & 0x80) != 0;
        self.sweep.period = ((value >> 4) & 0x07) + 1;
        self.sweep.negate_flag = (value & 0x08) != 0;
        self.sweep.shift_count = value & 0x07;
        self.sweep.reload_flag = true;
    }

    fn write_timer_lo(&mut self, value: u8) {
        self.timer_period = (self.timer_period & 0xFF00) | (value as u16);
    }

    fn write_timer_hi(&mut self, value: u8) {
        self.timer_period = (self.timer_period & 0x00FF) | (((value & 0x07) as u16) << 8);
        self.length_counter = LENGTH_TABLE[(value>>3) as usize];
        self.envelope.start_flag = true;
        self.duty_cycle = 0;
    }

    fn step_length_counter(&mut self) {
        if self.length_counter > 0 {
            self.length_counter -= 1;
        }
    }

    fn step_sweep(&mut self) {
        if self.sweep.reload_flag {
            if self.sweep.enable_flag && self.sweep.divider == 0 {
                self.set_timer_period_from_sweep();
            }
            self.sweep.divider = self.sweep.period;
            self.sweep.reload_flag = false;
        } else if self.sweep.divider > 0 {
            self.sweep.divider -= 1;
        } else {
            if self.sweep.enable_flag {
                self.set_timer_period_from_sweep();
            }
            self.sweep.divider = self.sweep.period;
        }
    }

    fn set_timer_period_from_sweep(&mut self) {
        let delta = self.timer_period >> self.sweep.shift_count;
        if self.sweep.negate_flag {
            match self.negation_type {
                SweepNegationType::TwosComplement => {
                    self.timer_period -= delta;
                },
                SweepNegationType::OnesComplement => {
                    self.timer_period += !delta;
                }
            }
        } else {
            self.timer_period += delta;
        }
    }

    fn step_envelope(&mut self) {
        self.envelope.step();
    }

    fn step_timer(&mut self) {
        if self.timer_value == 0 {
            self.timer_value = self.timer_period;
            self.duty_cycle = (self.duty_cycle + 1) % 8;
        } else {
            self.timer_value -= 1;
        }
    }

    fn output(&self) -> u8 {
        if !self.enable_flag ||
            self.length_counter == 0 ||
            DUTY_CYCLE_TABLE[self.duty_mode as usize][self.duty_cycle as usize] == 0 ||
            self.timer_period < 8 ||
            self.timer_period > 0x7FF {
            0
        } else if self.envelope.enable_flag {
            self.envelope.volume
        } else {
            self.constant_volume
        }
    }
}

struct Triangle {
    enable_flag: bool,
    control_flag: bool,
    timer_value: u16,
    timer_period: u16,
    length_counter_enable: bool,
    length_counter: u8,
    linear_counter_period: u8,
    linear_counter_value: u8,
    linear_counter_reload: bool,
    duty_cycle: u8,
}

impl Triangle {
    fn new() -> Triangle {
        Triangle {
            enable_flag: false,
            control_flag: false,
            timer_value: 0,
            timer_period: 0,
            length_counter_enable: false,
            length_counter: 0,
            linear_counter_period: 0,
            linear_counter_value: 0,
            linear_counter_reload: false,
            duty_cycle: 0,
        }
    }

    fn write_linear_counter(&mut self, value: u8) {
        self.control_flag = value & 0x80 != 0;
        self.length_counter_enable = !self.control_flag;
        self.linear_counter_period = value & 0x7F;
    }

    fn write_timer_lo(&mut self, value: u8) {
        self.timer_period = (self.timer_period & 0xFF00) | (value as u16);
    }

    fn write_length_counter_and_timer_hi(&mut self, value: u8) {
        self.length_counter = LENGTH_TABLE[(value>>3) as usize];
        self.timer_period = (self.timer_period & 0x00FF) | (((value & 0x07) as u16) << 8);
        self.timer_value = self.timer_period;
        self.linear_counter_reload = true;
    }

    fn step_timer(&mut self) {
        if self.timer_value == 0 {
            self.timer_value = self.timer_period;
            if self.length_counter > 0 && self.linear_counter_value > 0 {
                self.duty_cycle = (self.duty_cycle + 1) % 32;
            }
        } else {
            self.timer_value -= 1;
        }
    }

    fn step_length_counter(&mut self) {
        if self.length_counter_enable && self.length_counter > 0 {
            self.length_counter -= 1;
        }
    }

    fn step_linear_counter(&mut self) {
        if self.linear_counter_reload {
            self.linear_counter_value = self.linear_counter_period;
        } else if self.linear_counter_value > 0 {
            self.linear_counter_value -= 1;
        }

        if self.length_counter_enable {
            self.linear_counter_reload = false;
        }
    }

    fn output(&self) -> u8 {
        if !self.enable_flag || self.length_counter == 0 || self.linear_counter_value == 0 {
            0
        } else {
            TRIANGLE_TABLE[self.duty_cycle as usize]
        }
    }
}

struct Noise {
    enable_flag: bool,
    mode: bool,
    linear_feedback_shift: u16,
    timer_value: u16,
    timer_period: u16,
    length_counter_enable: bool,
    length_counter: u8,
    envelope: Envelope,
    constant_volume: u8,
}

impl Noise {
    fn new() -> Noise {
        Noise {
            enable_flag: false,
            mode: false,
            linear_feedback_shift: 0,
            timer_value: 0,
            timer_period: 0,
            length_counter_enable: false,
            length_counter: 0,
            envelope: Envelope::new(),
            constant_volume: 0,
        }
    }

    fn write_control(&mut self, value: u8) {
        self.length_counter_enable = (value & 0x20) == 0;
        self.envelope.loop_flag = !self.length_counter_enable;
        self.envelope.enable_flag = (value & 0x10) == 0;
        self.constant_volume = value & 0x0F;
        self.envelope.period = self.constant_volume;
        self.envelope.start_flag = true;
    }

    fn write_mode_and_timer_period(&mut self, value: u8) {
        self.mode = (value & 0x80) != 0;
        self.timer_period = NOISE_TABLE[(value & 0x0F) as usize];
    }

    fn write_length_counter_and_envelope_restart(&mut self, value: u8) {
        self.length_counter = LENGTH_TABLE[(value >> 3) as usize];
        self.envelope.start_flag = true;
    }

    fn step_timer(&mut self) {
        if self.timer_value == 0 {
            self.timer_value = self.timer_period;
            let shift = if self.mode { 6 } else { 1 };
            let b1 = self.linear_feedback_shift & 0x0001;
            let b2 = (self.linear_feedback_shift >> shift) & 0x0001;
            self.linear_feedback_shift >>= 1;
            self.linear_feedback_shift |= (b1 ^ b2) << 14;
        } else {
            self.timer_value -= 1;
        }
    }

    fn step_envelope(&mut self) {
        self.envelope.step();
    }

    fn step_length_counter(&mut self) {
        if self.length_counter_enable && self.length_counter > 0 {
            self.length_counter -= 1;
        }
    }

    fn output(&self) -> u8 {
        if !self.enable_flag || self.length_counter == 0 || self.linear_feedback_shift & 0x0001 == 1 {
            0
        } else if self.envelope.enable_flag {
            self.envelope.volume
        } else {
            self.constant_volume
        }
    }
}

struct Dmc {
    enable_flag: bool,
    loop_flag: bool,
    irq_flag: bool,
    value: u8,
    sample_address: u16,
    sample_length: u16,
    current_address: u16,
    current_length: u16,
    shift_register: u8,
    bit_count: u8,
    tick_period: u8,
    tick_value: u8,
}

impl Dmc {
    fn new() -> Dmc {
        Dmc {
            enable_flag: false,
            loop_flag: false,
            irq_flag: false,
            value: 0,
            sample_address: 0,
            sample_length: 0,
            current_address: 0,
            current_length: 0,
            shift_register: 0,
            bit_count: 0,
            tick_period: 0,
            tick_value: 0,
        }
    }

    fn write_control(&mut self, value: u8) {
        self.irq_flag = value & 0x80 != 0;
        self.loop_flag = value & 0x40 != 0;
        self.tick_period = DMC_TABLE[(value & 0x0F) as usize];
    }

    fn write_value(&mut self, value: u8) {
        self.value = value & 0x7F;
    }

    fn write_sample_address(&mut self, value: u8) {
        self.sample_address = 0xC000 | ((value as u16) << 6);
    }

    fn write_sample_length(&mut self, value: u8) {
        self.sample_length = ((value as u16) << 4) | 0x0001;
    }

    fn restart(&mut self) {
        self.current_address = self.sample_address;
        self.current_length = self.sample_length;
    }

    fn step_timer(&mut self) {
        if self.enable_flag {
            self.step_reader();
            if self.tick_value == 0 {
                self.tick_value = self.tick_period;
                self.step_shifter();
            } else {
                self.tick_value -= 1;
            }
        }
    }

    fn step_reader(&mut self) {
        // TODO: Implement
    }

    fn step_shifter(&mut self) {
        // TODO: Implement
    }

    fn output(&self) -> u8 {
        // TODO: Implement
        0
    }
}

#[derive(Eq, PartialEq)]
enum FrameCounterMode {
    FourStep,
    FiveStep,
}

struct FrameCounter {
    frame: u64,
    sequence_frame: u8,
    mode: FrameCounterMode,
    interrupt_inhibit: bool,
}

impl FrameCounter {
    const RATE: f64 = (CPU_FREQUENCY as f64) / 240.0;

    fn new() -> FrameCounter {
        FrameCounter {
            frame: 0,
            sequence_frame: 0,
            mode: FrameCounterMode::FourStep,
            interrupt_inhibit: false,
        }
    }
}

trait Filter {
    fn step(&mut self, x: f32) -> f32;
}

struct FirstOrderFilter {
    b0: f32,
    b1: f32,
    a1: f32,
    prev_x: f32,
    prev_y: f32,
}

impl Filter for FirstOrderFilter {
    fn step(&mut self, x: f32) -> f32 {
        let y = self.b0 * x + self.b1 * self.prev_x - self.a1 * self.prev_y;
        self.prev_x = x;
        self.prev_y = y;
        y
    }
}

impl FirstOrderFilter {
    fn new_low_pass(sample_rate: f32, cutoff_freq: f32) -> FirstOrderFilter {
        let c = sample_rate / PI / cutoff_freq;
        let a0i = 1.0 / (1.0 + c);

        FirstOrderFilter {
            b0: a0i,
            b1: a0i,
            a1: (1.0 - c) * a0i,
            prev_x: 0.0,
            prev_y: 0.0,
        }
    }

    fn new_high_pass(sample_rate: f32, cutoff_freq: f32) -> FirstOrderFilter {
        let c = sample_rate / PI / cutoff_freq;
        let a0i = 1.0 / (1.0 + c);

        FirstOrderFilter {
            b0: c * a0i,
            b1: -c * a0i,
            a1: (1.0 - c) * a0i,
            prev_x: 0.0,
            prev_y: 0.0,
        }
    }
}

struct FilterChain {
    filters: Vec<Box<Filter>>
}

impl FilterChain {
    fn new() -> FilterChain {
        FilterChain {
            filters: Vec::new(),
        }
    }

    fn add(mut self, filter: Box<Filter>) -> FilterChain {
        self.filters.push(filter);
        self
    }

    fn step(&mut self, x: f32) -> f32 {
        let mut x = x;
        for filter in self.filters.iter_mut() {
           x = filter.step(x);
        }

        x
    }
}

