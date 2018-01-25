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
use sadnes_core::apu::SAMPLE_RATE;

use argparse::*;
use emulator::*;
use cpal_driver::*;


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

    println!("output sample rate: {}", audio_driver.output_sample_rate);

    let time_source = audio_driver.time_source();

    let mut emulator = Emulator::new(rom, audio_driver.sink(), time_source);

    emulator.run(start_debugger);
}

