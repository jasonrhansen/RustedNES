#[global_allocator]
static GLOBAL: System = System;

use crate::emulator::*;

use iced::{window, Application, Settings};
use rustednes_common::logger;
use rustednes_core::cartridge::*;

use clap::Parser;
use clap_verbosity_flag::{InfoLevel, Verbosity};
use tracing::{error, info};

use std::alloc::System;
use std::error::Error;
use std::fs::File;
use std::path::{Path, PathBuf};

mod emulator;

#[derive(Debug, Parser)]
#[command(author, version, about, long_about = None)]
struct Opt {
    /// The name of the ROM to load
    #[arg(name = "ROM")]
    rom_path: PathBuf,

    #[clap(flatten)]
    verbose: Verbosity<InfoLevel>,
}

fn main() -> Result<(), Box<dyn Error>> {
    let opt: Opt = clap::Parser::parse();

    logger::initialize(&opt.verbose);

    match load_rom(&opt.rom_path) {
        Ok(rom) => {
            info!("{:?}", rom);
            let rom_path = opt.rom_path.to_path_buf();
            run_rom(rom, opt, rom_path)?;
        }
        Err(e) => error!("Error: {}", e),
    };

    Ok(())
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

fn run_rom(rom: Cartridge, opt: Opt, rom_path: PathBuf) -> iced::Result {
    Emulator::run(Settings {
        default_text_size: 24.0,
        window: window::Settings {
            position: window::Position::Centered,
            ..iced::window::Settings::default()
        },
        flags: Flags {
            rom: Some(rom),
            rom_path,
        },
        ..Settings::default()
    })
}
