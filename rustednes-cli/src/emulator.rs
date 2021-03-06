use crate::command::*;

use rustednes_core::cartridge::Cartridge;
use rustednes_core::cpu::CPU_FREQUENCY;
use rustednes_core::disassembler::Disassembler;
use rustednes_core::input::Button;
use rustednes_core::memory::Memory;
use rustednes_core::nes::Nes;
use rustednes_core::ppu::{SCREEN_HEIGHT, SCREEN_WIDTH};
use rustednes_core::serialize;
use rustednes_core::sink::*;
use rustednes_core::time_source::TimeSource;

use minifb::{Key, KeyRepeat, Scale, ScaleMode, Window, WindowOptions};
use rustyline::error::ReadlineError;
use rustyline::Editor;
use serde_json;

use std::cmp::min;
use std::collections::{HashMap, HashSet};
use std::fs::OpenOptions;
use std::io::{BufReader, BufWriter, Read, Write};
use std::path::PathBuf;
use std::sync::mpsc::{channel, Receiver, Sender};
use std::thread;
use std::time;

const CPU_CYCLE_TIME_NS: u64 = (1e9 as f64 / CPU_FREQUENCY as f64) as u64 + 1;

#[derive(PartialEq, Eq)]
enum Mode {
    Running,
    Debugging,
}

pub struct Emulator<A: AudioSink, T: TimeSource> {
    window: Window,

    nes: Nes,
    mode: Mode,

    breakpoints: HashSet<u16>,
    labels: HashMap<String, u16>,

    cursor: u16,
    last_command: Option<Command>,

    prompt_sender: Sender<String>,
    stdin_receiver: Receiver<String>,

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
        let (prompt_sender, prompt_receiver) = channel();
        let (stdin_sender, stdin_receiver) = channel();
        let _stdin_thread = thread::spawn(move || {
            input_loop(stdin_sender, prompt_receiver);
        });

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
            mode: Mode::Running,

            breakpoints: HashSet::new(),
            labels: HashMap::new(),

            prompt_sender,
            stdin_receiver,

            audio_frame_sink,

            cursor: 0,
            last_command: None,

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

        if start_debugger {
            self.start_debugger();
        }

        let mut pixel_buffer = vec![0; SCREEN_WIDTH * SCREEN_HEIGHT];

        while self.window.is_open() && !self.window.is_key_down(Key::Escape) {
            let mut video_frame_sink = Xrgb8888VideoSink::new(&mut pixel_buffer);

            let target_time_ns = self.time_source.time_ns() - self.start_time_ns;
            let target_cycles = target_time_ns / CPU_CYCLE_TIME_NS;

            match self.mode {
                Mode::Running => {
                    let mut start_debugger = false;
                    while self.emulated_cycles < target_cycles && !start_debugger {
                        let (_cycles, trigger_watchpoint) = self.step(&mut video_frame_sink);

                        self.emulated_instructions += 1;

                        if trigger_watchpoint
                            || (self.breakpoints.len() != 0
                                && self.breakpoints.contains(&self.nes.cpu.regs().pc))
                        {
                            start_debugger = true;
                        }
                    }

                    if start_debugger {
                        self.start_debugger();
                    }
                }
                Mode::Debugging => {
                    if self.run_debugger_commands(&mut video_frame_sink) {
                        break;
                    }

                    self.window.update();
                }
            }

            if video_frame_sink.frame_written() {
                self.window
                    .update_with_buffer(&pixel_buffer, SCREEN_WIDTH, SCREEN_HEIGHT)
                    .unwrap();

                if self.mode == Mode::Running {
                    self.read_input_keys();
                    if self.window.is_key_pressed(Key::F12, KeyRepeat::No) {
                        self.start_debugger();
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
                            match serde_json::from_str(&s) {
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

            thread::sleep(time::Duration::from_millis(10));
        }

        self.save_state_to_file();
    }

    fn step<V: VideoSink>(&mut self, video_frame_sink: &mut V) -> (u32, bool) {
        let (cycles, trigger_watchpoint) =
            self.nes.step(video_frame_sink, &mut self.audio_frame_sink);

        self.emulated_cycles += cycles as u64;

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

    fn start_debugger(&mut self) {
        self.mode = Mode::Debugging;

        self.cursor = self.nes.cpu.regs().pc;

        print!("0x{:04x}  ", self.cursor);
        self.disassemble_instruction();

        self.print_cursor();
    }

    fn run_debugger_commands<V: VideoSink>(&mut self, video_frame_sink: &mut V) -> bool {
        while let Ok(command_string) = self.stdin_receiver.try_recv() {
            let command = match (command_string.parse(), self.last_command.clone()) {
                (Ok(Command::Repeat), Some(c)) => Ok(c),
                (Ok(Command::Repeat), None) => Err("No last command".into()),
                (Ok(c), _) => Ok(c),
                (Err(e), _) => Err(e),
            };

            match command {
                Ok(command) => {
                    if self.run_debugger_command(command, video_frame_sink) {
                        return true;
                    }
                }
                Err(e) => {
                    println!("{}", e);
                }
            }

            if self.mode == Mode::Debugging {
                self.print_cursor();
            }
        }

        false
    }

    fn run_debugger_command<V: VideoSink>(
        &mut self,
        command: Command,
        video_frame_sink: &mut V,
    ) -> bool {
        match command {
            Command::ShowRegs => {
                let regs = self.nes.cpu.regs();
                let flags = self.nes.cpu.flags();
                let status: u8 = flags.into();
                println!("pc: 0x{:04x}", regs.pc);
                println!("a: 0x{:02x}", regs.a);
                println!("x: 0x{:02x}", regs.x);
                println!("y: 0x{:02x}", regs.y);
                println!("sp: 0x{:02x}", regs.sp);
                println!("status: 0x{:02x}", status);
                println!("Flags: {:?}", flags);
            }
            Command::Step(count) => {
                for _ in 0..count {
                    self.nes.step(video_frame_sink, &mut self.audio_frame_sink);
                    self.emulated_instructions += 1;
                    self.cursor = self.nes.cpu.regs().pc;
                    print!("{} 0x{:04x}  ", self.emulated_instructions, self.cursor);
                    self.disassemble_instruction();
                }
            }
            Command::Continue => {
                self.mode = Mode::Running;
                self.start_time_ns =
                    self.time_source.time_ns() - (self.emulated_cycles * CPU_CYCLE_TIME_NS);
            }
            Command::Goto(address) => {
                self.cursor = address;
            }
            Command::ShowMem(address) => {
                if let Some(address) = address {
                    self.cursor = address;
                }

                self.print_labels_at_cursor();

                const NUM_ROWS: u32 = 16;
                const NUM_COLS: u32 = 16;
                for _ in 0..NUM_ROWS {
                    print!("0x{:04x}  ", self.cursor);
                    for x in 0..NUM_COLS {
                        let byte = self.nes.interconnect.read_byte(self.cursor);
                        self.cursor = self.cursor.wrapping_add(1);
                        print!("{:02x}", byte);
                        if x < NUM_COLS - 1 {
                            print!(" ");
                        }
                    }
                    println!();
                }
            }
            Command::ShowPpuMem(address) => {
                let mut cursor = address;

                const NUM_ROWS: u32 = 16;
                const NUM_COLS: u32 = 16;
                for _ in 0..NUM_ROWS {
                    print!("0x{:04x}  ", cursor);
                    for x in 0..NUM_COLS {
                        let byte = self.nes.interconnect.ppu.mem.read_byte(cursor);
                        cursor = (cursor + 1) % 0x4000;
                        print!("{:02x}", byte);
                        if x < NUM_COLS - 1 {
                            print!(" ");
                        }
                    }
                    println!();
                }
            }
            Command::ShowStack => {
                let sp = self.nes.cpu.regs().sp;
                let addr = 0x0100 | sp as u16;

                for i in 0..min(10, 0x01FF - addr + 1) {
                    let byte = self.nes.interconnect.read_byte(addr + i);
                    println!("0x{:04x}  {:02x}", addr + i, byte);
                }
            }
            Command::Disassemble(count) => {
                for _ in 0..count {
                    self.cursor = self.disassemble_instruction();
                }
            }
            Command::Label => {
                for (label, address) in self.labels.iter() {
                    println!(".{}: 0x{:04x}", label, address);
                }
            }
            Command::AddLabel(ref label, address) => {
                self.labels.insert(label.clone(), address);
            }
            Command::RemoveLabel(ref label) => {
                if let None = self.labels.remove(label) {
                    println!("Label .{} doesn't exist", label);
                }
            }
            Command::Breakpoint => {
                for address in self.breakpoints.iter() {
                    println!("* 0x{:04x}", address);
                }
            }
            Command::AddBreakpoint(address) => {
                self.breakpoints.insert(address);
            }
            Command::RemoveBreakpoint(address) => {
                if !self.breakpoints.remove(&address) {
                    println!("Breakpoint at 0x{:04x} doesn't exist", address);
                }
            }
            Command::Watchpoint => {
                for address in self.nes.cpu.watchpoints.iter() {
                    println!("* 0x{:04x}", address);
                }
            }
            Command::AddWatchpoint(address) => {
                self.nes.cpu.watchpoints.insert(address);
            }
            Command::RemoveWatchpoint(address) => {
                if !self.nes.cpu.watchpoints.remove(&address) {
                    println!("Watchpoint at 0x{:04x} doesn't exist", address);
                }
            }
            Command::Exit => {
                return true;
            }
            Command::Repeat => unreachable!(),
        }

        self.last_command = Some(command);

        false
    }

    fn disassemble_instruction(&mut self) -> u16 {
        self.print_labels_at_cursor();
        let mut d = Disassembler::new(self.cursor);
        println!("{}", d.disassemble_next(&mut self.nes.interconnect));
        d.pc
    }

    fn print_cursor(&self) {
        self.prompt_sender
            .send(format!("(rustednes-debug 0x{:04x}) > ", self.cursor))
            .unwrap();
    }

    fn print_labels_at_cursor(&mut self) {
        for (name, _) in self.labels.iter().filter(|x| *x.1 == self.cursor) {
            println!(".{}:", name);
        }
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

fn input_loop(stdin_sender: Sender<String>, prompt_receiver: Receiver<String>) {
    let history_filename = "history.txt";
    let mut rl = Editor::<()>::new();
    if rl.load_history(history_filename).is_err() {
        println!("No previous history.");
    }
    loop {
        if let Ok(prompt) = prompt_receiver.recv() {
            let readline = rl.readline(&prompt);
            match readline {
                Ok(line) => {
                    rl.add_history_entry(line.as_str());
                    stdin_sender.send(line.as_str().into()).unwrap();
                }
                Err(ReadlineError::Interrupted) => {
                    println!("CTRL-C");
                    break;
                }
                Err(ReadlineError::Eof) => {
                    println!("CTRL-D");
                    break;
                }
                Err(err) => {
                    eprintln!("error: {:?}", err);
                    break;
                }
            }
        }
    }
    rl.save_history(history_filename).unwrap();
}
