extern crate sadnes;

use std::cell::RefCell;
use std::env;
use std::fs::File;
use std::rc::Rc;

use sadnes::apu::Apu;
use sadnes::cartridge::{Cartridge, LoadError};
use sadnes::disassembler::Disassembler;
use sadnes::interconnect::Interconnect;
use sadnes::mapper;
use sadnes::mapper::Mapper;
use sadnes::nes::Nes;
use sadnes::ppu::Ppu;
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
    let mut nes = Nes::new(rom);

    for _ in 0..100 {
        {
            let regs = nes.cpu.regs();
            let mut d = Disassembler::new(regs.pc);
            println!("{:?}", regs);
            println!("{}", d.disassemble_next(&mut nes.interconnect));
        }

        let cycles = nes.cpu.step(&mut nes.interconnect);
        println!("cycles: {}", cycles);
    }
}