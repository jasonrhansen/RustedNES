use crate::apu::Apu;
use crate::game_genie::Cheat;
use crate::input::Input;
use crate::mapper::{Mapper, MapperEnum};
use crate::memory::{Memory, Ram};
use crate::oam_dma::OamDma;
use crate::ppu::Ppu;

use std::collections::HashMap;

pub const OAMDMA_ADDRESS: u16 = 0x4014;

pub struct SystemBus<'a> {
    ram: &'a mut Ram,
    mapper: &'a mut MapperEnum,
    ppu: &'a mut Ppu,
    apu: &'a mut Apu,
    oam_dma: &'a mut OamDma,
    input: &'a mut Input,
    cheats: &'a HashMap<u16, Cheat>,
}

impl<'a> SystemBus<'a> {
    pub fn new(
        ram: &'a mut Ram,
        mapper: &'a mut MapperEnum,
        ppu: &'a mut Ppu,
        apu: &'a mut Apu,
        oam_dma: &'a mut OamDma,
        input: &'a mut Input,
        cheats: &'a HashMap<u16, Cheat>,
    ) -> Self {
        Self {
            ram,
            mapper,
            ppu,
            apu,
            oam_dma,
            input,
            cheats,
        }
    }

    pub fn read_byte(&mut self, addr: u16) -> u8 {
        let byte = match addr {
            0x0000..=0x1FFF => self.ram.read_byte(addr),
            0x2000..=0x3FFF => self.ppu.read_byte(self.mapper, addr),
            0x4000..=0x4015 | 0x4018..=0x401F => self.apu.read_byte(addr),
            0x4016..=0x4017 => self.input.read_byte(addr),
            0x4020..=0xFFFF => self.mapper.prg_read_byte(addr),
        };

        if let Some(cheat) = self.cheats.get(&addr) {
            let compare = cheat.compare();
            if compare.is_none() || compare.unwrap() == byte {
                return cheat.data();
            }
        }

        byte
    }

    // Same as read_byte, but without side effects.
    pub fn peek_byte(&self, addr: u16) -> u8 {
        match addr {
            0x0000..=0x1FFF => self.ram.peek_byte(addr),
            0x2000..=0x3FFF => self.ppu.peek_byte(addr),
            0x4000..=0x401F => self.apu.peek_byte(addr),
            0x4020..=0xFFFF => self.mapper.prg_peek_byte(addr),
        }
    }

    pub fn write_byte(&mut self, address: u16, value: u8) {
        if address == OAMDMA_ADDRESS {
            self.oam_dma.activate(value);
        } else if address < 0x2000 {
            self.ram.write_byte(address, value);
        } else if address < 0x4000 {
            self.ppu.write_byte(self.mapper, address, value);
        } else if address < 0x4016 || address == 0x4017 {
            self.apu.write_byte(address, value);
        } else if address < 0x4018 {
            self.input.write_byte(address, value);
        } else {
            self.mapper.prg_write_byte(address, value);
        }
    }

    pub fn read_word(&mut self, address: u16) -> u16 {
        self.read_byte(address) as u16 | ((self.read_byte(address + 1) as u16) << 8)
    }

    pub fn write_word(&mut self, address: u16, value: u16) {
        self.write_byte(address, (value >> 8) as u8);
        self.write_byte(address + 1, (value & 0xff) as u8);
    }

    pub fn ppu_scanline(&self) -> u16 {
        self.ppu.scanline
    }

    pub fn ppu_scanline_cycle(&self) -> u64 {
        self.ppu.scanline_cycle()
    }
}
