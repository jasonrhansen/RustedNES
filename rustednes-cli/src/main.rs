#[global_allocator]
static GLOBAL: System = System;

use crate::audio_driver::*;
use crate::cpal_driver::*;
use crate::emulator::*;
use crate::null_audio_driver::*;
use crate::system_time_source::*;

use rustednes_core::apu::SAMPLE_RATE as NES_SAMPLE_RATE;
use rustednes_core::cartridge::*;

use structopt::StructOpt;

use std::alloc::System;
use std::fs::File;
use std::path::{Path, PathBuf};

mod audio_driver;
mod command;
mod cpal_driver;
mod emulator;
mod null_audio_driver;
mod system_time_source;

const DESIRED_OUTPUT_SAMPLE_RATE: u32 = 44_100;

#[derive(Debug, StructOpt)]
#[structopt(name = "RustedNES", about = "A CLI frontend to the RustedNES emulator")]
struct Opt {
    /// The name of the ROM to load
    #[structopt(name = "ROM", parse(from_os_str))]
    rom_path: PathBuf,

    /// Start in debug mode
    #[structopt(long = "debug", short = "d")]
    debug: bool,

    /// Disable audio
    #[structopt(long = "noaudio")]
    disable_audio: bool,
}

fn main() {
    let opt = Opt::from_args();

    match load_rom(&opt.rom_path) {
        Ok(rom) => {
            println!("{:?}", rom);
            let rom_path = opt.rom_path.to_path_buf();
            run_rom(rom, opt, rom_path);
        }
        Err(e) => println!("Error: {}", e),
    }
}

fn load_rom(filename: &Path) -> Result<Cartridge, LoadError> {
    let mut file = File::open(filename)?;

    Cartridge::load(&mut file)
}

fn run_rom(rom: Cartridge, opt: Opt, rom_path: PathBuf) {
    let mut emulator = if opt.disable_audio {
        let audio_driver = Box::new(NullAudioDriver {});
        let time_source = Box::new(SystemTimeSource {});
        println!("Audio disabled");
        Emulator::new(rom, audio_driver.sink(), time_source, rom_path)
    } else {
        let audio_driver =
            Box::new(CpalDriver::new(NES_SAMPLE_RATE, DESIRED_OUTPUT_SAMPLE_RATE).unwrap());
        let time_source = audio_driver.time_source();
        println!("Audio sample rate: {}", audio_driver.sample_rate());
        Emulator::new(rom, audio_driver.sink(), time_source, rom_path)
    };

    emulator.run(opt.debug);
}
