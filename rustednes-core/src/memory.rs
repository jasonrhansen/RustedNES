use std::ops::{Deref, DerefMut};

pub trait Memory {
    fn read_byte(&mut self, address: u16) -> u8;
    fn write_byte(&mut self, address: u16, value: u8);

    fn read_word(&mut self, address: u16) -> u16 {
        self.read_byte(address) as u16 | ((self.read_byte(address + 1) as u16) << 8)
    }

    fn write_word(&mut self, address: u16, value: u16) {
        self.write_byte(address, (value >> 8) as u8);
        self.write_byte(address + 1, (value & 0xff) as u8);
    }
}

// 2KB internal RAM
const RAM_SIZE: usize = 0x0800;

pub struct Ram {
    bytes: [u8; RAM_SIZE],
}

impl Ram {
    pub fn new() -> Self {
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
