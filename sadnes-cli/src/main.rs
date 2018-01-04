#[macro_use]
extern crate clap;

extern crate time;

extern crate combine;
extern crate minifb;

extern crate sadnes_core;

mod audio_frame_sink;
mod video_frame_sink;
mod argparse;
mod command;
mod emulator;

use std::env;
use std::fs::File;
use std::io;

use sadnes_core::cartridge::*;

use argparse::*;
use emulator::*;

fn main() {
    let config = parse_args();

    match load_rom(&config.rom_path) {
        Ok(rom) => {
            println!("{:?}", rom);
            run_rom(rom, config.debug);
        },
        Err(e) => println!("Error: {}", e),
    }
}

fn load_rom(filename: &str) -> Result<Cartridge, LoadError> {
    let mut file = File::open(filename)?;

    Cartridge::load(&mut file)
}

fn run_rom(rom: Cartridge, start_debugger: bool) {
    let mut emulator = Emulator::new(rom);

    emulator.run(start_debugger);
}

