extern crate sadnes;

use std::cell::RefCell;
use std::env;
use std::fs::File;
use std::rc::Rc;

use sadnes::memory::Interconnect;
use sadnes::mapper;
use sadnes::mapper::Mapper;
use sadnes::cartridge::{Cartridge, LoadError};
use sadnes::ppu::Ppu;
use sadnes::apu::Apu;
use sadnes::input::Input;
use sadnes::cpu::Cpu;

fn main() {
    if let Some(filename) = env::args().nth(1) {
        run_cartridge(&filename);
    } else {
        println!("First argument must be an NES rom file name");
    }

    match load_rom(&filename) {
        Ok(rom) => {
            println!("{:?}", rom);
            run_rom(rom);
        },
        Err(e) => println!("Error: {}", e),
    }
}

fn load_cartridge(filename: &str) -> Result<Cartridge, LoadError> {
    let mut file = File::open(filename)?;

    Cartridge::load(&mut file)
}

fn run_cartridge(filename: &str) {
    println!("filename: {}", &filename);
    match load_cartridge(&filename) {
        Ok(cartridge) => {
            println!("{:?}", cartridge);
            sadnes::start(cartridge);
        },
        Err(e) => println!("Error: {}", e),
    }
}
