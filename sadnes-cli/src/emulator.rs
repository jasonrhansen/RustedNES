use minifb::{WindowOptions, Window, Key, KeyRepeat, Scale};

use command::*;

use sadnes_core::cartridge::{Cartridge, LoadError};
use sadnes_core::disassembler::Disassembler;
use sadnes_core::nes::Nes;
use sadnes_core::ppu::{SCREEN_WIDTH, SCREEN_HEIGHT};
use sadnes_core::sinks::*;

use std::collections::{HashSet, HashMap};
use std::env;
use std::fs::File;
use std::io::{stdin, stdout, Write};
use std::thread::{self, JoinHandle};
use std::time;

#[derive(PartialEq, Eq)]
enum Mode {
    Running,
    Debugging,
}

pub struct Emulator {
    window: Window,

    nes: Nes,
    mode: Mode,

    breakpoints: HashSet<u16>,
    labels: HashMap<String, u16>,

    cursor: u16,
    last_command: Option<Command>,

    emulated_cycles: u64,
}

impl Emulator {
    pub fn new(cartridge: Cartridge) -> Emulator {
        Emulator {
            window: Window::new("sadNES",
                                SCREEN_WIDTH, SCREEN_HEIGHT,
                                WindowOptions {
                                    borderless: false,
                                    title: true,
                                    resize: false,
                                    scale: Scale::X2,
                                }
            ).unwrap(),

            nes: Nes::new(cartridge),
            mode: Mode::Running,

            breakpoints: HashSet::new(),
            labels: HashMap::new(),

            cursor: 0,
            last_command: None,

            emulated_cycles: 0,
        }
    }

    pub fn run(&mut self) {
        let mut frame_buffer: Vec<u32> = vec![0; SCREEN_WIDTH * SCREEN_HEIGHT];

        while self.window.is_open() && !self.window.is_key_down(Key::Escape) {
            self.window.update_with_buffer(&frame_buffer).unwrap();

            match self.mode {
                Mode::Running => {
                    let mut start_debugger = false;

                    while !start_debugger {

                    }

                    if start_debugger {
                        self.start_debugger();
                    }
                },
                Mode::Debugging => {
                    if self.run_debugger_commands() {
                        break;
                    }

                    self.window.update();
                }
            }

            thread::sleep(time::Duration::from_millis(3));
        }
    }

    fn step(&mut self,
            video_frame_sink: &mut Sink<VideoFrame>,
            audio_frame_sink: &mut Sink<AudioFrame>) -> (u32, bool) {
        let (cycles, trigger_watchpoint) =
            self.nes.step(video_frame_sink, audio_frame_sink);

        self.emulated_cycles += cycles as u64;

        (cycles, trigger_watchpoint)
    }

    fn start_debugger(&mut self) {

    }

    fn run_debugger_commands(&mut self) -> bool {
        false
    }

    fn disassemble_instruction(&mut self) -> u16 {
        let mut d = Disassembler::new(self.cursor);
        println!("{}", d.disassemble_next(&mut self.nes.interconnect));
        d.pc
    }

    fn print_cursor(&self) {
        print!("(sadnes-debug 0x{:04x}) > ", self.cursor);
        stdout().flush().unwrap();
    }

    fn print_labels_at_cursor(&mut self) {
        for (name, _) in self.labels.iter().filter(|x| *x.1 == self.cursor) {
            println!(".{}:", name);
        }
    }
}

fn read_stdin() -> String {
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();
    input.trim().into()
}