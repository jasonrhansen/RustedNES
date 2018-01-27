use cartridge::{Cartridge, PRG_ROM_BANK_SIZE};
use mapper::Mapper;
use memory::Memory;
use ppu::{self, Ppu, Vram};
use cpu::{Cpu, Interrupt};


pub struct Mapper2 {
    cartridge: Box<Cartridge>,
    switchable_bank: u8,
}

impl Mapper2 {
    pub fn new(cartridge: Box<Cartridge>) -> Self {
        Mapper2 {
            cartridge,
            switchable_bank: 0,
        }
    }

    fn prg_rom_address(bank: u8, address: u16) -> usize {
        (bank as usize * PRG_ROM_BANK_SIZE as usize) |
            (address as usize & (PRG_ROM_BANK_SIZE as usize - 1))
    }

    fn mirror_address(&self, address: u16) -> u16 {
        self.cartridge.mirroring.mirror_address(address)
    }

    fn read_prg_rom(&mut self, address: u16) -> u8 {
        let bank = if address < 0xC000 {
            self.switchable_bank
        } else {
            self.cartridge.prg_rom_num_banks - 1
        };

        let rom_addr = Mapper2::prg_rom_address(bank, address);
        self.cartridge.prg_rom[rom_addr as usize]
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
            self.switchable_bank = value;
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
}
