use crate::cartridge::{self, Cartridge, Mirroring};
use crate::mapper::{self, Mapper};

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
        (bank as usize * 0x2000) | (address as usize & 0x1FFF)
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

    fn chr_read_byte(&mut self, address: u16) -> u8 {
        let rom_addr = self.chr_address(self.chr_bank, address);
        self.cartridge.chr[rom_addr as usize]
    }

    fn chr_write_byte(&mut self, _address: u16, _value: u8) {
        panic!("attempt to write to CHR ROM in mapper 3");
    }

    fn mirroring(&self) -> Mirroring {
        self.cartridge.mirroring
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
