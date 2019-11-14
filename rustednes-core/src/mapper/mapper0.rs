use crate::cartridge::{self, Cartridge, Mirroring, PRG_ROM_BANK_SIZE};
use crate::mapper::{self, Mapper};
use serde_derive::{Deserialize, Serialize};

pub struct Mapper0 {
    cartridge: Cartridge,
}

impl Mapper0 {
    pub fn new(cartridge: Cartridge) -> Self {
        Mapper0 { cartridge }
    }
}

#[derive(Deserialize, Serialize)]
pub struct State {
    pub cartridge: cartridge::State,
}

impl Mapper for Mapper0 {
    fn prg_read_byte(&mut self, address: u16) -> u8 {
        if address < 0x6000 {
            0
        } else if address < 0x8000 {
            self.cartridge.prg_ram[(address & 0x1FFF) as usize]
        } else if self.cartridge.prg_rom.len() > PRG_ROM_BANK_SIZE as usize {
            self.cartridge.prg_rom[(address & 0x7FFF) as usize]
        } else {
            // Mirror second bank to first
            self.cartridge.prg_rom[(address & 0x3FFF) as usize]
        }
    }

    fn prg_write_byte(&mut self, address: u16, value: u8) {
        if 0x6000 <= address && address < 0x8000 {
            self.cartridge.prg_ram[(address & 0x0100) as usize] = value;
        }
    }

    fn chr_read_byte(&mut self, address: u16) -> u8 {
        self.cartridge.chr[address as usize]
    }

    fn chr_write_byte(&mut self, _address: u16, _value: u8) {
        panic!("attempted to write to CHR ROM in mapper 0");
    }

    fn mirroring(&self) -> Mirroring {
        self.cartridge.mirroring
    }

    fn reset(&mut self) {
        // Nothing to reset
    }

    fn get_state(&self) -> mapper::State {
        mapper::State::State0(State {
            cartridge: self.cartridge.get_state(),
        })
    }

    fn apply_state(&mut self, state: &mapper::State) {
        match state {
            mapper::State::State0(state) => {
                self.cartridge.apply_state(&state.cartridge);
            }
            _ => panic!("Invalid mapper state enum variant in apply_state"),
        }
    }
}
