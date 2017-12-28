use mapper::Mapper;
use rom::{NesRom, PRG_ROM_BANK_SIZE};

pub struct Nrom {
    rom: Box<NesRom>,
    prg_ram: Vec<u8>,
}

impl Nrom {
    pub fn new(rom: Box<NesRom>) -> Self {
        let prg_ram_size = rom.prg_ram_size;
        Nrom {
            rom,
            prg_ram: vec![0u8; prg_ram_size as usize],
        }
    }
}

impl Mapper for Nrom {
    fn prg_load_byte(&self, address: u16) -> u8 {
        if address < 0x6000 {
            0
        } else if address < 0x8000 {
            self.prg_ram[(address & 0x0100) as usize]
        } else if address >= PRG_ROM_BANK_SIZE {
            // Mirror second bank to first
            self.rom.prg_rom[(address & 0x3FFF) as usize]
        } else {
            self.rom.prg_rom[(address & 0x7FFF) as usize]
        }
    }

    fn prg_store_byte(&mut self, address: u16, value: u8) {
        if 0x6000 <= address && address < 0x8000 {
            self.prg_ram[(address & 0x0100) as usize] = value;
        }

        // Ignore other address since we can't store to PRG_ROM
    }

    fn chr_load_byte(&self, address: u16) -> u8 {
        self.rom.chr_rom[address as usize]
    }

    fn chr_store_byte(&mut self, address: u16, value: u8) {
        // Do nothing since we can't store to CHR-ROM
    }
}
