use iced::widget::{column, image, text};
use iced::{
    executor, keyboard, subscription, window, Application, Command, Event, Length, Subscription,
    Theme,
};
use rustednes_common::state::StateManager;
use rustednes_common::time::{SystemTimeSource, TimeSource};

use rustednes_core::cartridge::Cartridge;
use rustednes_core::cpu::CPU_FREQUENCY;
use rustednes_core::input::Button;
use rustednes_core::nes::Nes;
use rustednes_core::ppu::{SCREEN_HEIGHT, SCREEN_WIDTH};
use rustednes_core::sink::*;

use std::collections::HashMap;
use std::mem;
use std::path::PathBuf;
use std::sync::{Arc, Mutex};

const CPU_CYCLE_TIME_NS: u64 = (1e9_f64 / CPU_FREQUENCY as f64) as u64 + 1;

pub struct Emulator {
    nes: Nes,
    rom_path: PathBuf,

    time_source: SystemTimeSource,
    start_time_ns: u64,

    emulated_cycles: u64,
    emulated_instructions: u64,

    state_manager: StateManager,

    keymap: HashMap<keyboard::KeyCode, Button>,

    pixels: Arc<Mutex<PixelBuffer>>,
}

#[derive(Debug, Clone)]
pub enum Message {
    EventOccurred(Event),
    Tick,
}

#[derive(Default)]
pub struct Flags {
    pub rom: Option<Cartridge>,
    pub rom_path: PathBuf,
}

impl Application for Emulator {
    type Executor = executor::Default;
    type Message = Message;
    type Theme = Theme;
    type Flags = Flags;

    fn new(flags: Self::Flags) -> (Self, Command<Self::Message>) {
        let mut keymap = HashMap::new();
        keymap.insert(keyboard::KeyCode::X, Button::A);
        keymap.insert(keyboard::KeyCode::Z, Button::B);
        keymap.insert(keyboard::KeyCode::Space, Button::Select);
        keymap.insert(keyboard::KeyCode::Enter, Button::Start);
        keymap.insert(keyboard::KeyCode::Up, Button::Up);
        keymap.insert(keyboard::KeyCode::Down, Button::Down);
        keymap.insert(keyboard::KeyCode::Left, Button::Left);
        keymap.insert(keyboard::KeyCode::Right, Button::Right);

        let time_source = SystemTimeSource {};
        let start_time_ns = time_source.time_ns();

        println!("rom path: {:?}", flags.rom_path);

        let emulator = Self {
            nes: Nes::new(flags.rom.expect("No ROM passed to emulator")),

            rom_path: flags.rom_path.clone(),

            time_source,
            start_time_ns,

            emulated_cycles: 0,
            emulated_instructions: 0,

            state_manager: StateManager::new(flags.rom_path, 10),

            keymap,
            pixels: Arc::new(Mutex::new([0u8; SCREEN_WIDTH * SCREEN_HEIGHT * 4])),
        };

        (emulator, Command::none())
    }

    fn title(&self) -> String {
        if let Some(rom_name) = self.rom_path.file_name() {
            format!("RustedNES - {}", rom_name.to_string_lossy())
        } else {
            "RustedNES".to_string()
        }
    }

    fn update(&mut self, message: Self::Message) -> Command<Self::Message> {
        match message {
            Message::EventOccurred(event) => match event {
                Event::Keyboard(event) => match event {
                    keyboard::Event::KeyPressed { key_code, .. } => {
                        if let Some(button) = self.keymap.get(&key_code) {
                            self.nes
                                .interconnect
                                .input
                                .game_pad_1
                                .set_button_pressed(*button, true)
                        }

                        Command::none()
                    }
                    keyboard::Event::KeyReleased { key_code, .. } => {
                        if let Some(button) = self.keymap.get(&key_code) {
                            self.nes
                                .interconnect
                                .input
                                .game_pad_1
                                .set_button_pressed(*button, false)
                        }

                        Command::none()
                    }
                    _ => Command::none(),
                },
                _ => Command::none(),
            },
            Message::Tick => {
                let pixels = Arc::clone(&self.pixels);
                let mut pixels = pixels.lock().unwrap();
                let mut video_sink = VideoFrameSink::new(&mut pixels);

                let target_time_ns = self.time_source.time_ns() - self.start_time_ns;
                let target_cycles = target_time_ns / CPU_CYCLE_TIME_NS;

                while self.emulated_cycles < target_cycles {
                    let (cycles, _) = self.nes.step(&mut video_sink, &mut NullAudioSink {});

                    self.emulated_cycles += cycles as u64;
                    self.emulated_instructions += 1;
                }

                Command::none()
            }
        }
    }

    fn view(&self) -> iced::Element<Self::Message> {
        let pixels = Arc::clone(&self.pixels);
        let pixels = pixels.lock().unwrap();

        let image_handle =
            image::Handle::from_pixels(SCREEN_WIDTH as u32, SCREEN_HEIGHT as u32, *pixels);

        column!(
            text(format!(
                "Elapsed nanoseconds: {}",
                self.time_source.time_ns() - self.start_time_ns
            )),
            text(format!("Emulated cycles: {}", self.emulated_cycles)),
            iced::widget::image(image_handle)
                .width(Length::Fill)
                .height(Length::Fill),
        )
        .into()
    }

    fn subscription(&self) -> iced::Subscription<Self::Message> {
        Subscription::batch([
            subscription::events().map(Message::EventOccurred),
            window::frames().map(|_| Message::Tick),
        ])
    }
}

pub struct NullAudioSink;

impl AudioSink for NullAudioSink {
    fn write_sample(&mut self, _frame: f32) {
        // Do nothing
    }

    fn samples_written(&self) -> usize {
        0
    }
}

type PixelBuffer = [u8; SCREEN_WIDTH * SCREEN_HEIGHT * 4];

pub struct VideoFrameSink<'a> {
    pixels: &'a mut PixelBuffer,
    frame_written: bool,
}

impl<'a> VideoFrameSink<'a> {
    pub fn new(pixels: &'a mut PixelBuffer) -> Self {
        VideoFrameSink {
            pixels,
            frame_written: false,
        }
    }
}

impl<'a> VideoSink for VideoFrameSink<'a> {
    fn write_frame(&mut self, frame_buffer: &[u8]) {
        for (i, palette_index) in frame_buffer.iter().enumerate() {
            let pixel = XRGB8888_PALETTE[*palette_index as usize];
            let offset = i * 4;

            self.pixels[offset] = (pixel >> 16) as u8;
            self.pixels[offset + 1] = (pixel >> 8) as u8;
            self.pixels[offset + 2] = pixel as u8;
            self.pixels[offset + 3] = (pixel >> 24) as u8;
        }
        self.frame_written = true;
    }

    fn frame_written(&self) -> bool {
        self.frame_written
    }

    fn pixel_size(&self) -> usize {
        mem::size_of::<u32>()
    }
}
