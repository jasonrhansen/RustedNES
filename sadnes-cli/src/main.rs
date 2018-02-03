#[macro_use]
extern crate clap;

extern crate time;

extern crate combine;
extern crate minifb;
extern crate liner;
extern crate cpal;
extern crate futures;

extern crate sadnes_core;

mod video_frame_sink;
mod argparse;
mod command;
mod emulator;
mod cpal_driver;
mod system_time_source;
mod null_audio_sink;

use std::fs::File;

use sadnes_core::cartridge::*;
use sadnes_core::apu::SAMPLE_RATE;

use argparse::*;
use emulator::*;
use cpal_driver::*;
use system_time_source::*;
use null_audio_sink::*;


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
    let audio_sink = if config.enable_audio {
        let audio_driver = CpalDriver::new(SAMPLE_RATE, 100).unwrap();
        println!("Output sample rate: {}", audio_driver.output_sample_rate);
        audio_driver.sink()
    } else {
        println!("Audio disabled");
        Box::new(NullAudioSink{})
    };

    let time_source = Box::new(SystemTimeSource{});

    let mut emulator = Emulator::new(rom, audio_sink, time_source);
    emulator.run(config.debug);
}

