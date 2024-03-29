use crate::cartridge::{self, Cartridge, Mirroring, PRG_ROM_BANK_SIZE};
use crate::mapper::{self, Mapper};
use serde_derive::{Deserialize, Serialize};

pub struct Mapper2 {
    cartridge: Cartridge,
    switchable_bank: u8,
}

#[derive(Deserialize, Serialize)]
pub struct State {
    pub cartridge: cartridge::State,
    pub switchable_bank: u8,
}

impl Mapper2 {
    pub fn new(cartridge: Cartridge) -> Self {
        Mapper2 {
            cartridge,
            switchable_bank: 0,
        }
    }

    fn prg_rom_address(bank: u8, address: u16) -> usize {
        (bank as usize * PRG_ROM_BANK_SIZE as usize)
            | (address as usize & (PRG_ROM_BANK_SIZE as usize - 1))
    }

    fn read_prg_rom(&mut self, address: u16) -> u8 {
        let bank = if address < 0xC000 {
            self.switchable_bank
        } else {
            self.cartridge.prg_rom_num_banks - 1
        };

        let rom_addr = Mapper2::prg_rom_address(bank, address);
        self.cartridge.prg_rom[rom_addr]
    }
}

impl Mapper for Mapper2 {
    fn prg_read_byte(&mut self, address: u16) -> u8 {
        if address < 0x6000 {
            0
        } else {
            self.read_prg_rom(address)
        }
    }

    fn prg_write_byte(&mut self, address: u16, value: u8) {
        if address >= 0x8000 {
            self.switchable_bank =
                ((value as usize) % (self.cartridge.prg_rom_num_banks) as usize) as u8;
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
        self.switchable_bank = 0;
    }

    fn get_state(&self) -> mapper::State {
        mapper::State::State2(State {
            cartridge: self.cartridge.get_state(),
            switchable_bank: self.switchable_bank,
        })
    }

    fn apply_state(&mut self, state: &mapper::State) {
        match state {
            mapper::State::State2(state) => {
                self.cartridge.apply_state(&state.cartridge);
                self.switchable_bank = state.switchable_bank;
            }
            _ => panic!("Invalid mapper state enum variant in apply_state"),
        }
    }
}
