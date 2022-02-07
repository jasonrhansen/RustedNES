#[global_allocator]
static GLOBAL: System = System;

use crate::cpal_driver::*;
use crate::emulator::*;

use rustednes_common::audio::*;
use rustednes_common::logger;
use rustednes_common::time::*;

use rustednes_core::apu::SAMPLE_RATE as NES_SAMPLE_RATE;
use rustednes_core::cartridge::*;

use tracing::{error, info};

use std::alloc::System;
use std::error::Error;
use std::fs::File;
use std::path::{Path, PathBuf};

mod cpal_driver;
mod emulator;

#[derive(Debug, clap::Parser)]
#[clap(name = "RustedNES", about = "A CLI frontend to the RustedNES emulator")]
struct Opt {
    /// The name of the ROM to load
    #[clap(name = "ROM", parse(from_os_str))]
    rom_path: PathBuf,

    /// Start in debug mode
    #[clap(long = "debug", short = 'd')]
    debug: bool,

    /// Disable audio
    #[clap(long = "noaudio")]
    disable_audio: bool,

    /// Silence all log output
    #[clap(short = 'q', long = "quiet")]
    quiet: bool,

    /// Verbose logging mode (-v, -vv, -vvv)
    #[clap(short = 'v', long = "verbose", parse(from_occurrences))]
    verbose: usize,
}

fn main() {
    let opt: Opt = clap::Parser::parse();

    if !opt.quiet {
        logger::initialize(opt.verbose);
    }

    match load_rom(&opt.rom_path) {
        Ok(rom) => {
            info!("{:?}", rom);
            let rom_path = opt.rom_path.to_path_buf();
            run_rom(rom, opt, rom_path);
        }
        Err(e) => error!("Error: {}", e),
    }
}

fn load_rom(filename: &Path) -> Result<Cartridge, Box<dyn Error>> {
    let file = File::open(filename)?;

    let cartridge = match filename.extension() {
        Some(ext) if ext == "zip" => {
            info!("Unzipping {}", filename.display());
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
    if opt.disable_audio {
        let audio_driver = NullAudioDriver {};
        let time_source = SystemTimeSource {};
        info!("Audio disabled");
        let mut emulator = Emulator::new(rom, audio_driver.sink(), time_source, rom_path);
        emulator.run(opt.debug);
    } else {
        let audio_driver = Box::new(CpalDriver::new(NES_SAMPLE_RATE).unwrap());
        let time_source = audio_driver.time_source();
        info!("Audio sample rate: {}", audio_driver.sample_rate());
        let mut emulator = Emulator::new(rom, audio_driver.sink(), time_source, rom_path);
        emulator.run(opt.debug);
    };
}
