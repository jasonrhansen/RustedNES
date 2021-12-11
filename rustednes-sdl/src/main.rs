#[global_allocator]
static GLOBAL: System = System;

use crate::audio_driver::*;
use crate::emulator::*;
use crate::null_audio_driver::*;
use crate::sdl_audio_driver::*;
use crate::system_time_source::*;

use rustednes_core::apu::SAMPLE_RATE as NES_SAMPLE_RATE;
use rustednes_core::cartridge::*;

use structopt::StructOpt;

use std::alloc::System;
use std::error::Error;
use std::fs::File;
use std::path::{Path, PathBuf};

mod audio_driver;
mod emulator;
mod null_audio_driver;
mod sdl_audio_driver;
mod system_time_source;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "RustedNES",
    about = "An SDL2 frontend to the RustedNES emulator"
)]
struct Opt {
    /// The name of the ROM to load
    #[structopt(name = "ROM", parse(from_os_str))]
    rom_path: PathBuf,

    /// Disable audio
    #[structopt(long = "noaudio")]
    disable_audio: bool,
}

fn main() {
    let opt = Opt::from_args();

    match load_rom(&opt.rom_path) {
        Ok(rom) => {
            println!("{:?}", rom);
            run_rom(rom, opt);
        }
        Err(e) => println!("Error: {}", e),
    }
}

fn load_rom(filename: &Path) -> Result<Cartridge, Box<dyn Error>> {
    let file = File::open(filename)?;

    let cartridge = match filename.extension() {
        Some(ext) if ext == "zip" => {
            println!("Unzipping {}", filename.display());
            let mut zip = zip::ZipArchive::new(&file)?;
            let mut zip_file = zip.by_index(0)?;
            Cartridge::load(&mut zip_file)?
        }
        _ => {
            let mut file = file;
            Cartridge::load(&mut file)?
        }
    };

    Ok(cartridge)
}

fn run_rom(rom: Cartridge, opt: Opt) {
    let sdl_context = sdl2::init().unwrap();

    if opt.disable_audio {
        let audio_driver = NullAudioDriver {};
        let time_source = SystemTimeSource {};
        println!("Audio disabled");
        let mut emulator = Emulator::new(sdl_context, rom, audio_driver.sink(), time_source);
        emulator.run();
    } else {
        let audio_driver =
            Box::new(SdlAudioDriver::new(sdl_context.clone(), NES_SAMPLE_RATE).unwrap());
        let time_source = audio_driver.time_source();
        println!("Audio sample rate: {}", audio_driver.sample_rate());
        let mut emulator = Emulator::new(sdl_context, rom, audio_driver.sink(), time_source);
        emulator.run();
    };
}
