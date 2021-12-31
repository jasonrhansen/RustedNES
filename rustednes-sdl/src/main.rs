#[global_allocator]
static GLOBAL: System = System;

use crate::emulator::*;
use crate::sdl_audio_driver::*;

use rustednes_core::apu::SAMPLE_RATE as NES_SAMPLE_RATE;
use rustednes_core::cartridge::*;

use rustednes_common::audio_driver::*;
use rustednes_common::null_audio_driver::*;
use rustednes_common::system_time_source::*;

use structopt::StructOpt;

use std::alloc::System;
use std::error::Error;
use std::fs::File;
use std::path::{Path, PathBuf};

mod emulator;
mod sdl_audio_driver;

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
            let rom_path = opt.rom_path.to_path_buf();
            run_rom(rom, opt, rom_path);
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

fn run_rom(rom: Cartridge, opt: Opt, rom_path: PathBuf) {
    let sdl_context = sdl2::init().unwrap();

    if opt.disable_audio {
        let audio_driver = NullAudioDriver {};
        let time_source = SystemTimeSource {};
        println!("Audio disabled");
        let mut emulator =
            Emulator::new(sdl_context, rom, audio_driver.sink(), time_source, rom_path);
        emulator.run();
    } else {
        let audio_driver =
            Box::new(SdlAudioDriver::new(sdl_context.clone(), NES_SAMPLE_RATE).unwrap());
        let time_source = audio_driver.time_source();
        println!("Audio sample rate: {}", audio_driver.sample_rate());
        let mut emulator =
            Emulator::new(sdl_context, rom, audio_driver.sink(), time_source, rom_path);
        emulator.run();
    };
}
