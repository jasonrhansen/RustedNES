use crate::cartridge::{self, Cartridge, Mirroring};
use crate::mapper::{self, Mapper};

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
        (bank as usize * 0x8000) | (address as usize & 0x7FFF)
    }

    fn read_prg_rom(&mut self, address: u16) -> u8 {
        let rom_addr = Mapper7::prg_rom_address(self.prg_rom_bank, address);
        self.cartridge.prg_rom[rom_addr]
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
                Mirroring::OneScreenUpper
            } else {
                Mirroring::OneScreenLower
            };
        }
    }

    fn chr_read_byte(&mut self, address: u16) -> u8 {
        self.cartridge.chr[address as usize]
    }

    fn chr_write_byte(&mut self, address: u16, value: u8) {
        self.cartridge.chr[address as usize] = value;
    }

    fn mirroring(&self) -> Mirroring {
        self.cartridge.mirroring
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
