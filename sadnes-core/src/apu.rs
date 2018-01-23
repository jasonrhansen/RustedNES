use memory::Memory;
use sink::*;
use cpu::{Cpu, Interrupt, CPU_FREQUENCY};


static DUTY_CYCLE_TABLE: &'static [[u8; 8]] = &[
    [0, 1, 0, 0, 0, 0, 0, 0],
    [0, 1, 1, 0, 0, 0, 0, 0],
    [0, 1, 1, 1, 1, 0, 0, 0],
    [1, 0, 0, 1, 1, 1, 1, 1],
];

static LENGTH_TABLE: &'static [u8] = &[
    10, 254, 20, 2, 40, 4, 80, 6, 160, 8, 60, 10, 14, 12, 26, 14,
	12, 16, 24, 18, 48, 20, 96, 22, 192, 24, 72, 26, 16, 28, 32, 30,
];


pub struct Apu {
    cycles: u64,

    sample_rate: u64,

    pulse_table: [f32; 31],
    tnd_table: [f32; 203],

    pulse_1: Pulse,
    pulse_2: Pulse,
    triangle: Triangle,
    noise: Noise,
    dmc: Dmc,
    frame_counter: FrameCounter,
}

impl Apu {
    pub fn new(sample_rate: u64) -> Apu {
        let mut pulse_table = [0f32; 31];
        for n in 0..31 {
            pulse_table[n] = 95.52 / (8128.0 / (n as f32) + 100.0);
        }

        let mut tnd_table = [0f32; 203];
        for n in 0..203 {
            tnd_table[n] = 163.67 / (24329.0 / (n as f32) + 100.0);
        }

        Apu {
            cycles: 0,
            sample_rate,
            pulse_table,
            tnd_table,
            pulse_1: Pulse::new(SweepNegationType::OnesComplement),
            pulse_2: Pulse::new(SweepNegationType::TwosComplement),
            triangle: Triangle::new(),
            noise: Noise::new(),
            dmc: Dmc::new(),
            frame_counter: FrameCounter::new(),
        }
    }

    // Run for the given number of cpu cycles
    pub fn cycles(&mut self, cpu: &mut Cpu, cycles: u32, audio_frame_sink: &mut Sink<AudioFrame>) {
        for _ in 0 .. cycles {
            self.step(cpu, audio_frame_sink);
        }
    }

    fn step(&mut self, cpu: &mut Cpu, audio_frame_sink: &mut Sink<AudioFrame>) {
        self.cycles += 1;

        self.step_timer();
        self.step_frame_counter(cpu);
        self.step_samples(audio_frame_sink);
    }

    fn step_samples(&mut self, audio_frame_sink: &mut Sink<AudioFrame>) {
        if self.cycles % self.sample_rate != 0 {
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

        pulse_out + tnd_out
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
                    0 => {
                        self.step_envelope_and_linear_counter();
                    },
                    1 => {
                        self.step_length_counter();
                        self.step_sweep();
                        self.step_envelope_and_linear_counter();
                    },
                    2 => {
                        self.step_envelope_and_linear_counter();
                    }
                    3 => {
                        self.step_length_counter();
                        self.step_sweep();
                        self.step_envelope_and_linear_counter();
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
                    0 => {
                        self.step_length_counter();
                        self.step_sweep();
                        self.step_envelope_and_linear_counter();
                    },
                    1 => {
                        self.step_envelope_and_linear_counter();
                    },
                    2 => {
                        self.step_length_counter();
                        self.step_sweep();
                        self.step_envelope_and_linear_counter();
                    },
                    3 => {
                        self.step_length_counter();
                        self.step_sweep();
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
    }

    fn step_sweep(&mut self) {
        self.pulse_1.step_sweep();
        self.pulse_2.step_sweep();
    }

    fn step_envelope_and_linear_counter(&mut self) {
        self.pulse_1.step_envelope();
        self.pulse_2.step_envelope();
    }

    fn step_timer(&mut self) {
        if self.cycles % 2 == 0 {
            self.pulse_1.step_timer();
            self.pulse_2.step_timer();
        }
    }

    fn read_status(&mut self) -> u8 {
        self.frame_counter.interrupt_inhibit = false;

        (if self.pulse_1.length_counter > 0 { 1 } else { 0 }) |
        ((if self.pulse_2.length_counter > 0 { 1 } else { 0 }) << 1)
        // TODO: Add remaining status bits
    }

    fn write_status(&mut self, value: u8) {
        self.pulse_1.enable_flag = (value & 0x01) != 0;
        self.pulse_2.enable_flag = (value & 0x02) != 0;
        // TODO: Enable Triangle, Noise, and DMC
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
            0x4015 => self.write_status(value),
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
    divider: u8,
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
            divider: 0,
            period: 0,
        }
    }

    fn step(&mut self) {
        if self.start_flag {
            self.start_flag = false;
            self.volume = 15;
            self.divider = self.period;
        } else if self.divider == 0 {
            if self.volume > 0 {
                self.volume -= 1;
            } else if self.loop_flag {
                self.volume = 15;
            }

            self.divider = self.period;
        } else {
            self.divider -= 1;
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
        if !self.enable_flag {
            return 0
        } else if self.length_counter == 0 {
            return 0
        } else if DUTY_CYCLE_TABLE[self.duty_mode as usize][self.duty_cycle as usize] == 0 {
            return 0
        } else if self.timer_period < 8 || self.timer_period > 0x7FF {
            return 0
        }

        if self.envelope.enable_flag {
            return self.envelope.volume
        } else {
            return self.constant_volume;
        }
    }
}

struct Triangle {

}

impl Triangle {
    fn new() -> Triangle {
        Triangle {}
    }

    fn output(&self) -> u8 {
        0
    }
}

struct Noise {

}

impl Noise {
    fn new() -> Noise {
        Noise {}
    }

    fn output(&self) -> u8 {
        0
    }
}

struct Dmc {
}

impl Dmc {
    fn new() -> Dmc {
        Dmc {}
    }

    fn output(&self) -> u8 {
        0
    }
}

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
