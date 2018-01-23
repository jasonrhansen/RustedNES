#[macro_use]
extern crate clap;

extern crate time;

extern crate combine;
extern crate minifb;
extern crate liner;
extern crate cpal;
extern crate futures;

extern crate sadnes_core;

mod audio_frame_sink;
mod video_frame_sink;
mod argparse;
mod command;
mod emulator;
mod cpal_driver;

use std::fs::File;

use sadnes_core::cartridge::*;

use argparse::*;
use emulator::*;
use cpal_driver::*;

const SAMPLE_RATE: u32 = 44000;

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
    let audio_driver = CpalDriver::new(SAMPLE_RATE, 100).unwrap();

    let mut emulator = Emulator::new(rom, audio_driver);

    emulator.run(start_debugger);
}

