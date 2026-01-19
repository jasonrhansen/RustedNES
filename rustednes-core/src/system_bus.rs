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

    pub fn read_byte(&mut self, address: u16) -> u8 {
        let byte = if address < 0x2000 {
            self.ram.read_byte(address)
        } else if address < 0x4000 {
            self.ppu.read_byte(self.mapper, address & 0x2007)
        } else if address < 0x4016 {
            self.apu.read_byte(address)
        } else if address < 0x4018 {
            self.input.read_byte(address)
        } else {
            self.mapper.prg_read_byte(address)
        };

        if let Some(cheat) = self.cheats.get(&address) {
            let compare = cheat.compare();
            if compare.is_none() || compare.unwrap() == byte {
                return cheat.data();
            }
        }

        byte
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
}
