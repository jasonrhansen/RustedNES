use cartridge;
use cartridge::Cartridge;
use mapper::Mapper;
use memory::Memory;
use ppu::Vram;

use serde_json;

pub struct Mapper3 {
    cartridge: Box<Cartridge>,
    chr_bank: u8,
}

#[derive(Deserialize, Serialize)]
pub struct State {
    pub cartridge: cartridge::State,
    pub chr_bank: u8,
}

impl Mapper3 {
    pub fn new(cartridge: Box<Cartridge>) -> Self {
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

    fn get_state(&self) -> Vec<u8> {
        let state = State {
            cartridge: self.cartridge.get_state(),
            chr_bank: self.chr_bank,
        };

        serde_json::to_vec(&state).unwrap_or(vec![])
    }

    fn apply_state(&mut self, state: &[u8]) {
        if let Ok(ref state) = serde_json::from_slice::<State>(&state) {
            self.cartridge.apply_state(&state.cartridge);
            self.chr_bank = state.chr_bank;
        }
    }
}
