#[macro_use]
extern crate bitflags;

extern crate byteorder;

#[macro_use]
mod opcode;

pub mod cartridge;
pub mod mapper;
pub mod memory;
pub mod cpu;
pub mod ppu;
pub mod apu;
pub mod input;
mod disassembler;
mod interconnect;

use std::cell::RefCell;
use std::env;
use std::fs::File;
use std::rc::Rc;

use interconnect::Interconnect;
use mapper::Mapper;
use cartridge::{Cartridge, LoadError};
use ppu::Ppu;
use apu::Apu;
use input::Input;
use cpu::Cpu;

fn start_emulation(cartridge: Cartridge) {
    let mapper = Rc::new(
        RefCell::new(
            mapper::create_mapper(Box::new(cartridge))
        )
    );

    let mut interconnect = Interconnect::new(
        Ppu::new(mapper.clone()),
        Apu{},
        Input{}, mapper
    );

    let mut cpu = Cpu::new(interconnect);

    // Main emulation loop
    loop {
        let cpu_cycles = cpu.step();

        interconnect.cycles(cpu_cycles);

        for i in 0..cpu_cycles * 3 {
           interconnect.ppu.step(&mut cpu);
        }
    }
}
