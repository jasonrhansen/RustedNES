use std::ops::{Deref, DerefMut};

use ppu::Ppu;
use apu::Apu;
use input::Input;
use mapper::Mapper;

pub trait Memory {
    fn load_byte(&self, address: u16) -> u8;
    fn store_byte(&mut self, address: u16, value: u8);

    fn load_word(&self, address: u16) -> u16 {
        self.load_byte(address) as u16 |
            ((self.load_byte(address + 1) as u16) << 8)
    }

    fn store_word(&mut self, address: u16, value: u16) {
        self.store_byte(address, (value >> 8) as u8);
        self.store_byte(address + 1, (value & 0xff) as u8);
    }
}

// 2KB internal RAM
const RAM_SIZE: usize = 0x0800;

pub struct Ram { buf: [u8; RAM_SIZE] }

impl Ram {
    fn new() -> Self {
        Ram {
           buf: [0; RAM_SIZE],
        }
    }
}

// For the RAM we only use the bottom 11 bits of the address.
// This will prevent index out of bounds, and will support mirroring.
impl Memory for Ram {
    fn load_byte(&self, address: u16) -> u8 {
        self.buf[address as usize & (RAM_SIZE - 1)]
    }

    fn store_byte(&mut self, address: u16, value: u8) {
        self.buf[address as usize & (RAM_SIZE - 1)] = value
    }
}

impl Deref for Ram {
    type Target = [u8; RAM_SIZE];

    fn deref(&self) -> &Self::Target {
        &self.buf
    }
}

impl DerefMut for Ram {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.buf
    }
}


pub struct CpuMemMap {
    pub ram: Ram,
    pub ppu: Ppu,
    pub apu: Apu,
    pub input: Input,
    pub mapper: Box<Mapper>
}

impl CpuMemMap {
    pub fn new(ppu: Ppu, apu: Apu, input: Input, mapper: Box<Mapper>) -> Self {
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
    fn load_byte(&self, address: u16) -> u8 {
        if address < 0x2000 {
            self.ram.load_byte(address)
        } else if address < 0x4000 {
            self.ppu.load_byte(address)
        } else if address < 0x4015 {
            self.apu.load_byte(address)
        } else if address < 0x4018 {
            self.input.load_byte(address)
        } else {
            self.mapper.prg_load_byte(address)
        }
    }

    fn store_byte(&mut self, address: u16, value: u8) {
        if address < 0x2000 {
            self.ram.store_byte(address, value);
        } else if address < 0x4000 {
            self.ppu.store_byte(address, value);
        } else if address < 0x4015 {
            self.apu.store_byte(address, value);
        } else if address < 0x4018 {
            self.input.store_byte(address, value);
        } else {
            self.mapper.prg_store_byte(address, value);
        }
    }
}
