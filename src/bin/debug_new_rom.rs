extern crate sadnes;

use std::env;
use std::fs::File;

use sadnes::rom::{NesRom, LoadError};

fn main() {
    if let Some(rom_filename) = env::args().nth(1) {
        println!("filename: {}", &rom_filename);
        match load_rom(&rom_filename) {
            Ok(rom) => println!("{:?}", rom),
            Err(e) => println!("Error: {}", e),
        }
    } else {
        println!("First argument must be an NES rom file name");
    }
}

fn load_rom(filename: &str) -> Result<NesRom, LoadError> {
    let mut file = File::open(filename)?;

    NesRom::load(&mut file)
}