use rustednes_common::debugger::{DebugEmulator, Debugger};
use rustednes_common::emulation_mode::EmulationMode;
use rustednes_core::cartridge::Cartridge;
use rustednes_core::cpu::CPU_FREQUENCY;
use rustednes_core::input::Button;
use rustednes_core::nes::Nes;
use rustednes_core::ppu::{SCREEN_HEIGHT, SCREEN_WIDTH};
use rustednes_core::serialize;
use rustednes_core::sink::*;
use rustednes_core::time_source::TimeSource;

use minifb::{Key, KeyRepeat, Scale, ScaleMode, Window, WindowOptions};

use std::fs::OpenOptions;
use std::io::{BufReader, BufWriter, Read, Write};
use std::path::PathBuf;
use std::thread;
use std::time::Duration;

const CPU_CYCLE_TIME_NS: u64 = (1e9_f64 / CPU_FREQUENCY as f64) as u64 + 1;

pub struct Emulator<A: AudioSink, T: TimeSource> {
    window: Window,

    nes: Nes,
    mode: EmulationMode,

    audio_frame_sink: A,

    time_source: T,
    start_time_ns: u64,

    emulated_cycles: u64,
    emulated_instructions: u64,

    serialized: Option<String>,

    rom_path: PathBuf,
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

            serialized: None,
            rom_path,
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

                    if self.window.is_key_pressed(Key::Key1, KeyRepeat::No) {
                        self.serialized =
                            serde_json::to_string(&serialize::get_state(&self.nes)).ok();
                    }

                    if self.window.is_key_pressed(Key::F1, KeyRepeat::No) {
                        if self.serialized.is_none() {
                            self.load_state_from_file();
                        }
                        if let Some(ref s) = self.serialized {
                            match serde_json::from_str(s) {
                                Ok(state) => {
                                    serialize::apply_state(&mut self.nes, state);
                                }
                                Err(e) => {
                                    eprintln!("error applying save state: {}", e);
                                }
                            }
                        }
                    }
                }
            }

            thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
        }

        self.save_state_to_file();
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

    fn save_state_to_file(&mut self) {
        if let Some(ref s) = self.serialized {
            let save_file = OpenOptions::new()
                .write(true)
                .truncate(true)
                .create(true)
                .open(&self.save_state_file_path());
            match save_file {
                Ok(save_file) => {
                    let mut save_writer = BufWriter::new(save_file);
                    let _ = save_writer.write_all(s.as_bytes());
                }
                Err(e) => {
                    eprintln!("unable to open file to save state: {}", e);
                }
            }
        }
    }

    fn load_state_from_file(&mut self) {
        let save_file = OpenOptions::new()
            .read(true)
            .write(false)
            .create(false)
            .open(&self.save_state_file_path());
        if let Ok(save_file) = save_file {
            println!("Loading save file");
            let mut save_reader = BufReader::new(save_file);
            let mut serialized = String::new();
            let _ = save_reader.read_to_string(&mut serialized);
            self.serialized = Some(serialized);
        }
    }

    fn save_state_file_path(&self) -> PathBuf {
        self.rom_path.with_extension("sav")
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
