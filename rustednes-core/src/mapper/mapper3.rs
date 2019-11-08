use crate::cartridge::{self, Cartridge};
use crate::mapper::{self, Mapper};
use crate::memory::Memory;
use crate::ppu::Vram;

use serde_derive::{Deserialize, Serialize};

pub struct Mapper3 {
    cartridge: Cartridge,
    chr_bank: u8,
}

#[derive(Deserialize, Serialize)]
pub struct State {
    pub cartridge: cartridge::State,
    pub chr_bank: u8,
}

impl Mapper3 {
    pub fn new(cartridge: Cartridge) -> Self {
        Mapper3 {
            cartridge,
            chr_bank: 0,
        }
    }

    fn chr_address(&self, bank: u8, address: u16) -> usize {
        (bank as usize * 0x2000 as usize) | (address as usize & 0x1FFF)
    }

    fn read_chr(&mut self, address: u16) -> u8 {
        let rom_addr = self.chr_address(self.chr_bank, address);
        self.cartridge.chr[rom_addr as usize]
    }

    fn mirror_address(&self, address: u16) -> u16 {
        self.cartridge.mirroring.mirror_address(address)
    }
}

impl Mapper for Mapper3 {
    fn prg_read_byte(&mut self, address: u16) -> u8 {
        if address < 0x8000 {
            0
        } else {
            self.cartridge.prg_rom[(address - 0x8000) as usize]
        }
    }

    fn prg_write_byte(&mut self, address: u16, value: u8) {
        if address >= 0x8000 {
            self.chr_bank = ((value as usize) % (self.cartridge.prg_rom.len() / 0x2000)) as u8;
        }
    }

    fn ppu_read_byte(&mut self, vram: &mut Vram, address: u16) -> u8 {
        if address < 0x2000 {
            self.read_chr(address)
        } else {
            vram.read_byte(self.mirror_address(address) - 0x2000)
        }
    }

    fn ppu_write_byte(&mut self, vram: &mut Vram, address: u16, value: u8) {
        if address >= 0x2000 {
            vram.write_byte(self.mirror_address(address) - 0x2000, value);
        }
    }

    fn reset(&mut self) {
        self.chr_bank = 0;
    }

    fn get_state(&self) -> mapper::State {
        mapper::State::State3(State {
            cartridge: self.cartridge.get_state(),
            chr_bank: self.chr_bank,
        })
    }

    fn apply_state(&mut self, state: &mapper::State) {
        match state {
            mapper::State::State3(state) => {
                self.cartridge.apply_state(&state.cartridge);
                self.chr_bank = state.chr_bank;
            }
            _ => panic!("Invalid mapper state enum variant in apply_state"),
        }
    }
}
