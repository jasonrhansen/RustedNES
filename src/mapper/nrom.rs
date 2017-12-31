use cartridge::{Cartridge, PRG_ROM_BANK_SIZE};
use mapper::Mapper;
use memory::Memory;
use ppu::Vram;

pub struct Nrom {
    cartridge: Box<Cartridge>,
}

impl Nrom {
    pub fn new(cartridge: Box<Cartridge>) -> Self {
        Nrom {
            cartridge,
        }
    }
}

impl Mapper for Nrom {
    fn prg_read_byte(&mut self, address: u16) -> u8 {
        if address < 0x6000 {
            0
        } else if address < 0x8000 {
            self.cartridge.prg_ram[(address & 0x1FFF) as usize]
        } else if self.cartridge.prg_rom.len() > PRG_ROM_BANK_SIZE as usize {
            // Mirror second bank to first
            self.cartridge.prg_rom[(address & 0x7FFF) as usize]
        } else {
            self.cartridge.prg_rom[(address & 0x3FFF) as usize]
        }
    }

    fn prg_write_byte(&mut self, address: u16, value: u8) {
        if 0x6000 <= address && address < 0x8000 {
            self.cartridge.prg_ram[(address & 0x0100) as usize] = value;
        }

        // Ignore other address since we can't store to PRG_ROM
    }

    fn ppu_read_byte(&mut self, vram: &mut Vram, address: u16) -> u8 {
        if address < 0x2000 {
            self.cartridge.chr_rom[address as usize]
        } else {
            // TODO: Handle mirroring
            vram.read_byte(address)
        }
    }

    fn ppu_write_byte(&mut self, vram: &mut Vram, address: u16, value: u8) {
        if address >= 0x2000 {
            // TODO: Handle mirroring
            vram.write_byte(address, value);
        }
    }
}
