use rustednes_common::debugger::{DebugEmulator, Debugger};
use rustednes_common::emulation_mode::EmulationMode;
use rustednes_common::state_manager::StateManager;
use rustednes_common::time_source::TimeSource;

use rustednes_core::cartridge::Cartridge;
use rustednes_core::cpu::CPU_FREQUENCY;
use rustednes_core::input::Button;
use rustednes_core::nes::Nes;
use rustednes_core::ppu::{SCREEN_HEIGHT, SCREEN_WIDTH};
use rustednes_core::sink::*;

use minifb::{Key, KeyRepeat, Scale, ScaleMode, Window, WindowOptions};

use std::path::PathBuf;
use std::thread;
use std::time::Duration;

const CPU_CYCLE_TIME_NS: u64 = (1e9_f64 / CPU_FREQUENCY as f64) as u64 + 1;
const NUMBER_KEYS: &[Key] = &[
    Key::Key0,
    Key::Key1,
    Key::Key2,
    Key::Key3,
    Key::Key4,
    Key::Key5,
    Key::Key6,
    Key::Key7,
    Key::Key8,
    Key::Key9,
];

pub struct Emulator<A: AudioSink, T: TimeSource> {
    window: Window,

    nes: Nes,
    mode: EmulationMode,

    audio_frame_sink: A,

    time_source: T,
    start_time_ns: u64,

    emulated_cycles: u64,
    emulated_instructions: u64,

    state_manager: StateManager,
}

impl<A, T> Emulator<A, T>
where
    A: AudioSink,
    T: TimeSource,
{
    pub fn new(
        cartridge: Cartridge,
        audio_frame_sink: A,
        time_source: T,
        rom_path: PathBuf,
    ) -> Emulator<A, T>
    where
        A: AudioSink,
        T: TimeSource,
    {
        Emulator {
            window: Window::new(
                "RustedNES",
                SCREEN_WIDTH,
                SCREEN_HEIGHT,
                WindowOptions {
                    borderless: false,
                    title: true,
                    resize: false,
                    scale: Scale::X4,
                    scale_mode: ScaleMode::AspectRatioStretch,
                    topmost: false,
                    transparency: false,
                    none: false,
                },
            )
            .unwrap(),

            nes: Nes::new(cartridge),
            mode: EmulationMode::Running,

            audio_frame_sink,

            time_source,
            start_time_ns: 0,

            emulated_cycles: 0,
            emulated_instructions: 0,

            state_manager: StateManager::new(rom_path, NUMBER_KEYS.len()),
        }
    }

    pub fn run(&mut self, start_debugger: bool) {
        self.start_time_ns = self.time_source.time_ns();

        let mut debugger = Debugger::new();

        if start_debugger {
            self.mode = EmulationMode::Debugging;
            debugger.start(&mut self.nes);
        }

        let mut pixel_buffer = vec![0; SCREEN_WIDTH * SCREEN_HEIGHT];

        while self.window.is_open() && !self.window.is_key_down(Key::Escape) {
            let mut video_frame_sink = Xrgb8888VideoSink::new(&mut pixel_buffer);

            let target_time_ns = self.time_source.time_ns() - self.start_time_ns;
            let target_cycles = target_time_ns / CPU_CYCLE_TIME_NS;

            match self.mode {
                EmulationMode::Running => {
                    let mut start_debugger = false;
                    while self.emulated_cycles < target_cycles && !start_debugger {
                        let (_cycles, trigger_watchpoint) = self.step(&mut video_frame_sink);

                        if trigger_watchpoint || debugger.at_breakpoint(&self.nes) {
                            start_debugger = true;
                        }
                    }

                    if start_debugger {
                        self.mode = EmulationMode::Debugging;
                        debugger.start(&mut self.nes);
                    }
                }
                EmulationMode::Debugging => {
                    if debugger.run_commands(self, &mut video_frame_sink) {
                        break;
                    }

                    self.window.update();
                }
            }

            if video_frame_sink.frame_written() {
                self.window
                    .update_with_buffer(&pixel_buffer, SCREEN_WIDTH, SCREEN_HEIGHT)
                    .unwrap();

                if self.mode == EmulationMode::Running {
                    self.read_input_keys();
                    if self.window.is_key_pressed(Key::F12, KeyRepeat::No) {
                        self.mode = EmulationMode::Debugging;
                        debugger.start(&mut self.nes);
                    }

                    if self.window.is_key_pressed(Key::P, KeyRepeat::No) {
                        let settings = &mut self.nes.interconnect.apu.settings;
                        settings.pulse_1_enabled = !settings.pulse_1_enabled;
                    }

                    if self.window.is_key_pressed(Key::LeftBracket, KeyRepeat::No) {
                        let settings = &mut self.nes.interconnect.apu.settings;
                        settings.pulse_2_enabled = !settings.pulse_2_enabled;
                    }

                    if self.window.is_key_pressed(Key::T, KeyRepeat::No) {
                        let settings = &mut self.nes.interconnect.apu.settings;
                        settings.triangle_enabled = !settings.triangle_enabled;
                    }

                    if self.window.is_key_pressed(Key::N, KeyRepeat::No) {
                        let settings = &mut self.nes.interconnect.apu.settings;
                        settings.noise_enabled = !settings.noise_enabled;
                    }

                    if self.window.is_key_pressed(Key::D, KeyRepeat::No) {
                        let settings = &mut self.nes.interconnect.apu.settings;
                        settings.dmc_enabled = !settings.dmc_enabled;
                    }

                    if self.window.is_key_pressed(Key::F, KeyRepeat::No) {
                        let settings = &mut self.nes.interconnect.apu.settings;
                        settings.filter_enabled = !settings.filter_enabled;
                    }

                    let ctrl_down = self.window.is_key_down(Key::LeftCtrl)
                        || self.window.is_key_down(Key::RightCtrl);

                    for (slot, &key) in NUMBER_KEYS.iter().enumerate() {
                        if self.window.is_key_pressed(key, KeyRepeat::No) {
                            if ctrl_down {
                                self.state_manager.load_state(&mut self.nes, slot);
                            } else {
                                self.state_manager.save_state(&self.nes, slot);
                            }
                        }
                    }
                }
            }

            thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
        }

        self.state_manager.write_state_to_files();
    }

    fn step<V: VideoSink>(&mut self, video_frame_sink: &mut V) -> (u32, bool) {
        let (cycles, trigger_watchpoint) =
            self.nes.step(video_frame_sink, &mut self.audio_frame_sink);

        self.emulated_cycles += cycles as u64;
        self.emulated_instructions += 1;

        (cycles, trigger_watchpoint)
    }

    fn read_input_keys(&mut self) {
        let game_pad_1 = &mut self.nes.interconnect.input.game_pad_1;

        game_pad_1.set_button_pressed(Button::A, self.window.is_key_down(Key::X));
        game_pad_1.set_button_pressed(Button::B, self.window.is_key_down(Key::Z));
        game_pad_1.set_button_pressed(Button::Select, self.window.is_key_down(Key::Space));
        game_pad_1.set_button_pressed(Button::Start, self.window.is_key_down(Key::Enter));
        game_pad_1.set_button_pressed(Button::Up, self.window.is_key_down(Key::Up));
        game_pad_1.set_button_pressed(Button::Down, self.window.is_key_down(Key::Down));
        game_pad_1.set_button_pressed(Button::Left, self.window.is_key_down(Key::Left));
        game_pad_1.set_button_pressed(Button::Right, self.window.is_key_down(Key::Right));
    }
}

impl<A, V, T> DebugEmulator<A, V> for Emulator<A, T>
where
    A: AudioSink,
    V: VideoSink,
    T: TimeSource,
{
    fn nes(&mut self) -> &mut Nes {
        &mut self.nes
    }

    fn emulated_cycles(&self) -> u64 {
        self.emulated_cycles
    }

    fn emulated_instructions(&self) -> u64 {
        self.emulated_instructions
    }

    fn audio_frame_sink(&mut self) -> &mut A {
        &mut self.audio_frame_sink
    }

    fn mode(&self) -> EmulationMode {
        self.mode
    }

    fn set_mode(&mut self, mode: EmulationMode) {
        self.mode = mode;
    }

    fn reset_start_time(&mut self) {
        self.start_time_ns =
            self.time_source.time_ns() - (self.emulated_cycles * CPU_CYCLE_TIME_NS);
    }

    fn step(&mut self, video_frame_sink: &mut V) -> (u32, bool) {
        Emulator::step(self, video_frame_sink)
    }
}
