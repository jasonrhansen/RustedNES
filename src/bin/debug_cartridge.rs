extern crate sadnes;

use std::cell::RefCell;
use std::env;
use std::fs::File;
use std::rc::Rc;

use sadnes::interconnect::Interconnect;
use sadnes::mapper;
use sadnes::mapper::Mapper;
use sadnes::cartridge::{Cartridge, LoadError};
use sadnes::ppu::Ppu;
use sadnes::apu::Apu;
use sadnes::input::Input;
use sadnes::cpu::Cpu;

fn main() {
    if let Some(filename) = env::args().nth(1) {
        debug_rom(&filename);
    } else {
        println!("First argument must be an NES rom file name");
    }
}

fn debug_rom(filename: &str) {
    println!("filename: {}", &filename);
    match load_rom(&filename) {
        Ok(rom) => {
            println!("{:?}", rom);
            run_rom(rom);
        },
        Err(e) => println!("Error: {}", e),
    }
}

fn load_rom(filename: &str) -> Result<Cartridge, LoadError> {
    let mut file = File::open(filename)?;

    Cartridge::load(&mut file)
}

fn run_rom(rom: Cartridge) {
    let mapper = Rc::new(
        RefCell::new(
            mapper::create_mapper(Box::new(rom))
        )
    );

    let mut interconnect = Interconnect::new(mapper);

    let mut cpu = Cpu::new();

    cpu.reset(&mut interconnect);

    println!("Stepping through instructions");

    for _ in 0..100 {
        cpu.step_debug(&mut interconnect);
    }
}