use std::cell::RefCell;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use ppu::Ppu;
use apu::Apu;
use input::Input;
use mapper::Mapper;

pub trait Memory {
    fn read_byte(&mut self, address: u16) -> u8;
    fn write_byte(&mut self, address: u16, value: u8);

    fn read_word(&mut self, address: u16) -> u16 {
        self.read_byte(address) as u16 |
            ((self.read_byte(address + 1) as u16) << 8)
    }

    fn write_word(&mut self, address: u16, value: u16) {
        self.write_byte(address, (value >> 8) as u8);
        self.write_byte(address + 1, (value & 0xff) as u8);
    }
}

// 2KB internal RAM
const RAM_SIZE: usize = 0x0800;

pub struct Ram { bytes: [u8; RAM_SIZE] }

impl Ram {
    fn new() -> Self {
        Ram {
           bytes: [0; RAM_SIZE],
        }
    }
}

// For the RAM we only use the bottom 11 bits of the address.
// This will prevent index out of bounds, and will support mirroring.
impl Memory for Ram {
    fn read_byte(&mut self, address: u16) -> u8 {
        self[address as usize & (RAM_SIZE - 1)]
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        self[address as usize & (RAM_SIZE - 1)] = value
    }
}

impl Deref for Ram {
    type Target = [u8; RAM_SIZE];

    fn deref(&self) -> &Self::Target {
        &self.bytes
    }
}

impl DerefMut for Ram {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.bytes
    }
}


pub struct CpuMemMap {
    pub ram: Ram,
    pub ppu: Ppu,
    pub apu: Apu,
    pub input: Input,
    pub mapper: Rc<RefCell<Box<Mapper>>>,
}

impl CpuMemMap {
    pub fn new(ppu: Ppu, apu: Apu, input: Input,
               mapper: Rc<RefCell<Box<Mapper>>>) -> Self {
        CpuMemMap {
            ram: Ram::new(),
            ppu,
            apu,
            input,
            mapper,
        }
    }
}

impl Memory for CpuMemMap {
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
