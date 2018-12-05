#[global_allocator]
static GLOBAL: System = System;

use crate::argparse::*;
use crate::audio_driver::*;
use crate::cpal_driver::*;
use crate::emulator::*;
use crate::null_audio_driver::*;
use crate::system_time_source::*;

use rustednes_core::apu::SAMPLE_RATE;
use rustednes_core::cartridge::*;

use std::alloc::System;
use std::fs::File;

mod argparse;
mod audio_driver;
mod command;
mod cpal_driver;
mod emulator;
mod null_audio_driver;
mod system_time_source;

fn main() {
    let config = parse_args();

    match load_rom(&config.rom_path) {
        Ok(rom) => {
            println!("{:?}", rom);
            run_rom(rom, config);
        }
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
        let audio_driver = Box::new(NullAudioDriver {});
        let time_source = Box::new(SystemTimeSource {});
        println!("Audio disabled");
        Emulator::new(rom, audio_driver.sink(), time_source)
    };

    emulator.run(config.debug);
}
