use std::cell::RefCell;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use apu::Apu;
use cpu::Interrupt;
use input::Input;
use mapper::Mapper;
use memory::{Memory, Ram};
use ppu::Ppu;

pub struct Interconnect {
    pub ram: Ram,
    pub ppu: Ppu,
    pub apu: Apu,
    pub input: Input,
    pub mapper: Rc<RefCell<Box<Mapper>>>,
}

impl Interconnect {
    pub fn new(mapper: Rc<RefCell<Box<Mapper>>>) -> Self {
        Interconnect {
            ram: Ram::new(),
            ppu: Ppu::new(mapper.clone()),
            apu: Apu{},
            input: Input{},
            mapper,
        }
    }
}

impl Memory for Interconnect {
    fn read_byte(&mut self, address: u16) -> u8 {
        if address < 0x2000 {
            self.ram.read_byte(address)
        } else if address < 0x4000 {
            self.ppu.read_byte(address)
        } else if address < 0x4015 {
            self.apu.read_byte(address)
        } else if address < 0x4018 {
            self.input.read_byte(address)
        } else {
            let mut mapper = self.mapper.borrow_mut();
            mapper.prg_read_byte(address)
        }
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        if address < 0x2000 {
            self.ram.write_byte(address, value);
        } else if address < 0x4000 {
            self.ppu.write_byte(address, value);
        } else if address < 0x4015 {
            self.apu.write_byte(address, value);
        } else if address < 0x4018 {
            self.input.write_byte(address, value);
        } else {
            let mut mapper = self.mapper.borrow_mut();
            mapper.prg_write_byte(address, value);
        }
    }
}

impl Interconnect {
    pub fn cycles(&mut self, cycles: u32) -> Option<Interrupt> {
        self.ppu.cycles(cycles)
    }
}
