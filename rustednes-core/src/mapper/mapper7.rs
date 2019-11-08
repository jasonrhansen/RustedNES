use crate::cartridge::{self, Cartridge, Mirroring};
use crate::mapper::{self, Mapper};
use crate::memory::Memory;
use crate::ppu::Vram;

use serde_derive::{Deserialize, Serialize};

pub struct Mapper7 {
    cartridge: Cartridge,
    prg_rom_bank: u8,
}

#[derive(Deserialize, Serialize)]
pub struct State {
    pub cartridge: cartridge::State,
    pub prg_rom_bank: u8,
}

impl Mapper7 {
    pub fn new(cartridge: Cartridge) -> Self {
        Mapper7 {
            cartridge,
            prg_rom_bank: 0,
        }
    }

    fn prg_rom_address(bank: u8, address: u16) -> usize {
        (bank as usize * 0x8000 as usize) | (address as usize & 0x7FFF)
    }

    fn mirror_address(&self, address: u16) -> u16 {
        self.cartridge.mirroring.mirror_address(address)
    }

    fn read_prg_rom(&mut self, address: u16) -> u8 {
        let rom_addr = Mapper7::prg_rom_address(self.prg_rom_bank, address);
        self.cartridge.prg_rom[rom_addr as usize]
    }
}

impl Mapper for Mapper7 {
    fn prg_read_byte(&mut self, address: u16) -> u8 {
        if address < 0x6000 {
            0
        } else {
            self.read_prg_rom(address)
        }
    }

    fn prg_write_byte(&mut self, address: u16, value: u8) {
        if address >= 0x8000 {
            self.prg_rom_bank = value & 0x07;
            self.cartridge.mirroring = if value & 0x10 == 0 {
                Mirroring::OneScreenLower
            } else {
                Mirroring::OneScreenUpper
            };
        }
    }

    fn ppu_read_byte(&mut self, vram: &mut Vram, address: u16) -> u8 {
        if address < 0x2000 {
            self.cartridge.chr[address as usize]
        } else {
            vram.read_byte(self.mirror_address(address) - 0x2000)
        }
    }

    fn ppu_write_byte(&mut self, vram: &mut Vram, address: u16, value: u8) {
        if address < 0x2000 {
            self.cartridge.chr[address as usize] = value;
        } else {
            vram.write_byte(self.mirror_address(address) - 0x2000, value);
        }
    }

    fn reset(&mut self) {
        self.cartridge.mirroring = self.cartridge.default_mirroring;
        self.prg_rom_bank = 0;
    }

    fn get_state(&self) -> mapper::State {
        mapper::State::State7(State {
            cartridge: self.cartridge.get_state(),
            prg_rom_bank: self.prg_rom_bank,
        })
    }

    fn apply_state(&mut self, state: &mapper::State) {
        match state {
            mapper::State::State7(state) => {
                self.cartridge.apply_state(&state.cartridge);
                self.prg_rom_bank = state.prg_rom_bank;
            }
            _ => panic!("Invalid mapper state enum variant in apply_state"),
        }
    }
}
