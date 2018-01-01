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

    fn step(&mut self, cartridge: Cartridge) {
        let cpu_cycles = self.cpu.step(&mut self.interconnect);
        let interrupt = self.interconnect.cycles(cpu_cycles);
        match interrupt {
            Interrupt::Nmi => self.cpu.request_nmi(),
            Interrupt::Irq => self.cpu.request_irq(),
            _ => (),
        }
    }
}