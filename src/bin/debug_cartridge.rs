extern crate sadnes;

use std::env;
use std::fs::File;

use sadnes::cartridge::{Cartridge, LoadError};
use sadnes::disassembler::Disassembler;
use sadnes::nes::Nes;

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
    let mut nes = Nes::new(rom);

    for _ in 0..100000 {
        {
            let cpu = nes.cpu.borrow_mut();
            let regs = cpu.regs();
            let mut d = Disassembler::new(regs.pc);
            println!("{:?}", regs);
            println!("{}", d.disassemble_next(&mut nes.interconnect));
        }

        let cycles = nes.step();
        println!("cycles: {}", cycles);
    }
}