#[macro_use]
extern crate clap;
extern crate combine;
extern crate cpal;
extern crate futures;
extern crate liner;
extern crate minifb;
extern crate rustednes_core;
extern crate serde;
extern crate serde_json;
extern crate time;

use rustednes_core::apu::SAMPLE_RATE;

use argparse::*;
use audio_driver::*;
use cpal_driver::*;
use emulator::*;
use null_audio_driver::*;
use rustednes_core::cartridge::*;
use std::fs::File;
use system_time_source::*;

mod argparse;
mod command;
mod emulator;
mod cpal_driver;
mod system_time_source;
mod audio_driver;
mod null_audio_driver;

fn main() {
    let config = parse_args();

    match load_rom(&config.rom_path) {
        Ok(rom) => {
            println!("{:?}", rom);
            run_rom(rom, config);
        },
        Err(e) => println!("Error: {}", e),
    }
}

fn load_rom(filename: &str) -> Result<Cartridge, LoadError> {
    let mut file = File::open(filename)?;

    Cartridge::load(&mut file)
}

fn run_rom(rom: Cartridge, config: CommandLineConfig) {
    let mut emulator = if config.enable_audio {
        let audio_driver = Box::new(CpalDriver::new(SAMPLE_RATE).unwrap());
        let time_source = audio_driver.time_source();
        println!("Audio sample rate: {}", audio_driver.sample_rate());
        Emulator::new(rom, audio_driver.sink(), time_source)
    } else {
        let audio_driver = Box::new(NullAudioDriver{});
        let time_source = Box::new(SystemTimeSource{});
        println!("Audio disabled");
        Emulator::new(rom, audio_driver.sink(), time_source)
    };

    emulator.run(config.debug);
}

