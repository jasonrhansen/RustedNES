extern crate sadnes;

use std::env;
use std::fs::File;

use sadnes::cartridge::{Cartridge, LoadError};

fn main() {
    if let Some(filename) = env::args().nth(1) {
        println!("filename: {}", &filename);
        match load_rom(&filename) {
            Ok(rom) => println!("{:?}", rom),
            Err(e) => println!("Error: {}", e),
        }
    } else {
        println!("First argument must be an NES rom file name");
    }
}

fn load_rom(filename: &str) -> Result<Cartridge, LoadError> {
    let mut file = File::open(filename)?;

    Cartridge::load(&mut file)
}