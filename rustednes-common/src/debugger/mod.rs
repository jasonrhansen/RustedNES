mod command;
mod debug_emulator;

pub use debug_emulator::DebugEmulator;

use std::cmp::min;
use std::collections::{HashMap, HashSet};
use std::sync::mpsc::{channel, Receiver, Sender};
use std::thread;

use rustednes_core::disassembler::Disassembler;
use rustednes_core::memory::Memory;
use rustednes_core::nes::Nes;
use rustednes_core::sink::{AudioSink, VideoSink};

use crate::emulation_mode::EmulationMode;

use command::Command;

use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use tracing::{debug, error};

pub struct Debugger {
    pub breakpoints: HashSet<u16>,
    labels: HashMap<String, u16>,

    cursor: u16,
    last_command: Option<Command>,

    prompt_sender: Sender<String>,
    stdin_receiver: Receiver<String>,
}

impl Default for Debugger {
    fn default() -> Self {
        Self::new()
    }
}

impl Debugger {
    pub fn new() -> Self {
        let (prompt_sender, prompt_receiver) = channel();
        let (stdin_sender, stdin_receiver) = channel();
        let _stdin_thread = thread::spawn(move || {
            Self::input_loop(stdin_sender, prompt_receiver);
        });

        Self {
            breakpoints: Default::default(),
            labels: Default::default(),
            cursor: 0,
            last_command: None,
            prompt_sender,
            stdin_receiver,
        }
    }

    fn input_loop(stdin_sender: Sender<String>, prompt_receiver: Receiver<String>) {
        let history_filename = "history.txt";
        let mut rl = DefaultEditor::new().unwrap();
        if rl.load_history(history_filename).is_err() {
            debug!("No previous history.");
        }
        loop {
            if let Ok(prompt) = prompt_receiver.recv() {
                let readline = rl.readline(&prompt);
                match readline {
                    Ok(line) => {
                        rl.add_history_entry(line.as_str()).unwrap();
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
                        error!("error: {:?}", err);
                        break;
                    }
                }
            }
        }
        rl.save_history(history_filename).unwrap();
    }

    pub fn start(&mut self, nes: &mut Nes) {
        self.cursor = nes.cpu.regs().pc;

        for _ in 1..3 {
            print!("0x{:04x}  ", self.cursor);
            self.disassemble_instruction(nes);
        }

        self.print_cursor();
    }

    pub fn at_breakpoint(&self, nes: &Nes) -> bool {
        self.breakpoints.contains(&nes.cpu.regs().pc)
    }

    pub fn run_commands<A, V>(
        &mut self,
        emulator: &mut dyn DebugEmulator<A, V>,
        video_frame_sink: &mut V,
    ) -> bool
    where
        V: VideoSink,
        A: AudioSink,
    {
        while let Ok(command_string) = self.stdin_receiver.try_recv() {
            let command = match (command_string.parse(), self.last_command.clone()) {
                (Ok(Command::Repeat), Some(c)) => Ok(c),
                (Ok(Command::Repeat), None) => Err("No last command".into()),
                (Ok(c), _) => Ok(c),
                (Err(e), _) => Err(e),
            };

            match command {
                Ok(command) => {
                    if self.run_command(emulator, command, video_frame_sink) {
                        return true;
                    }
                }
                Err(e) => {
                    println!("{}", e);
                }
            }

            if emulator.mode() == EmulationMode::Debugging {
                self.print_cursor();
            }
        }

        false
    }

    fn run_command<A, V>(
        &mut self,
        emulator: &mut dyn DebugEmulator<A, V>,
        command: Command,
        video_frame_sink: &mut V,
    ) -> bool
    where
        V: VideoSink,
        A: AudioSink,
    {
        match command {
            Command::ShowRegs => {
                let nes = emulator.nes();
                let regs = nes.cpu.regs();
                let flags = nes.cpu.flags();
                let status: u8 = flags.into();
                println!("pc: 0x{:04x}", regs.pc);
                println!("a: 0x{:02x}", regs.a);
                println!("x: 0x{:02x}", regs.x);
                println!("y: 0x{:02x}", regs.y);
                println!("sp: 0x{:02x}", regs.sp);
                println!("status: 0x{:02x}", status);
                println!("flags: {:?}", flags);
            }
            Command::Step(count) => {
                for _ in 0..count {
                    emulator.step(video_frame_sink);
                    self.cursor = emulator.nes().cpu.regs().pc;
                    print!(
                        "{} 0x{:04x}  ",
                        emulator.emulated_instructions(),
                        self.cursor
                    );
                    self.disassemble_instruction(emulator.nes());
                }
            }
            Command::Continue => {
                emulator.set_mode(EmulationMode::Running);
                emulator.reset_start_time();
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
                        let byte = emulator.nes().interconnect.read_byte(self.cursor);
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
                        let byte = emulator.nes().interconnect.ppu.mem.read_byte(cursor);
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
                let sp = emulator.nes().cpu.regs().sp;
                let addr = 0x0100 | sp as u16;

                for i in 0..min(10, 0x01FF - addr + 1) {
                    let byte = emulator.nes().interconnect.read_byte(addr + i);
                    println!("0x{:04x}  {:02x}", addr + i, byte);
                }
            }
            Command::Disassemble(count) => {
                for _ in 0..count {
                    self.cursor = self.disassemble_instruction(emulator.nes());
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
                if self.labels.remove(label).is_none() {
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
                for address in emulator.nes().cpu.watchpoints.iter() {
                    println!("* 0x{:04x}", address);
                }
            }
            Command::AddWatchpoint(address) => {
                emulator.nes().cpu.watchpoints.insert(address);
            }
            Command::RemoveWatchpoint(address) => {
                if !emulator.nes().cpu.watchpoints.remove(&address) {
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

    fn disassemble_instruction(&mut self, nes: &mut Nes) -> u16 {
        self.print_labels_at_cursor();
        let mut d = Disassembler::new(self.cursor);
        println!("{}", d.disassemble_next(&mut nes.interconnect));
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
}
