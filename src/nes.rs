use std::cell::RefCell;
use std::rc::Rc;

use interconnect::Interconnect;
use mapper;
use mapper::Mapper;
use cartridge::Cartridge;
use ppu::Ppu;
use apu::Apu;
use input::Input;
use cpu::{Cpu, Interrupt};

pub struct Nes {
    pub interconnect: Interconnect,
    pub cpu: Cpu,
}

impl Nes {
    pub fn new(cartridge: Cartridge) -> Nes {
        let mapper = Rc::new(
            RefCell::new(
                mapper::create_mapper(Box::new(cartridge))
            )
        );

        let mut interconnect = Interconnect::new(mapper);
        let mut cpu = Cpu::new();

        cpu.reset(&mut interconnect);

        Nes {
            interconnect,
            cpu,
        }
    }

    fn step(&mut self, cartridge: Cartridge) -> u32 {
        let cpu_cycles = self.cpu.step(&mut self.interconnect);

        if let Some(interrupt) = self.interconnect.cycles(cpu_cycles) {
            self.cpu.request_interrupt(interrupt);
        }

        cpu_cycles
    }
}