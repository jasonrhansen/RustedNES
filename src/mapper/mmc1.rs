use mapper::Mapper;
use cartridge::{Cartridge, PRG_ROM_BANK_SIZE};

#[derive(Default)]
struct Regs {
    control: u8,
    prg_bank: u8,
    chr_bank_0: u8,
    chr_bank_1: u8,
}

pub struct Mmc1 {
    cartridge: Box<Cartridge>,
    shift: u8,
    write_count: u8,
    regs: Regs,
}

enum Mirroring {
    OneScreenLower,
    OneScreenUpper,
    Vertical,
    Horizontal,
}

enum PrgRomMode {
    Switch32Kb,    // Switch 32 KB at $8000, ignoring low bit of bank number
    FixFirstBank,  // Fix first bank at $8000 and switch 16 KB bank at $C000
    FixLastBank,   // Fix last bank at $C000 and switch 16 KB bank at $8000
}

enum ChrRomMode {
    Switch8Kb,     // Switch 8 KB at a time
    Switch4Kb,     // Switch two separate 4 KB banks
}

impl Mmc1 {
    pub fn new(cartridge: Box<Cartridge>) -> Self {
        Mmc1 {
            cartridge,
            shift: 0x10,
            write_count: 0,
            regs: Regs::default(),
        }
    }

    fn mirroring(&self) -> Mirroring {
        match self.regs.control & 0x03 {
            0 => Mirroring::OneScreenLower,
            1 => Mirroring::OneScreenUpper,
            2 => Mirroring::Vertical,
            _ => Mirroring::Horizontal,
        }
    }

    fn prg_rom_mode(&self) -> PrgRomMode {
        let control = (self.regs.control & 0x0F) >> 2;
        match control {
            0 | 1 => PrgRomMode::Switch32Kb,
            2 => PrgRomMode::FixFirstBank,
            _ => PrgRomMode::FixLastBank,
        }
    }

    fn chr_rom_mode(&self) -> ChrRomMode {
        if self.regs.control & 0x10 == 0 {
            ChrRomMode::Switch8Kb
        } else {
            ChrRomMode::Switch4Kb
        }
    }

    fn write_register(&mut self, address: u16, shift: u8) {
        if address < 0xA000 {
            self.regs.control = shift;
        } else if address < 0xC000 {
            self.regs.chr_bank_0 = shift;
        } else if address <  0xE00 {
            self.regs.chr_bank_1 = shift;
        } else {
            self.regs.prg_bank = shift;
        }
    }

    fn prg_rom_bank_first(&self) -> u8 {
        match self.prg_rom_mode() {
            PrgRomMode::Switch32Kb => self.regs.prg_bank & 0x0E,
            PrgRomMode::FixFirstBank => 0,
            PrgRomMode::FixLastBank => (self.regs.prg_bank & 0x0F),
        }
    }

    fn prg_rom_bank_last(&self) -> u8 {
        match self.prg_rom_mode() {
            PrgRomMode::Switch32Kb => (self.regs.prg_bank & 0x0E) | 0x01,
            PrgRomMode::FixFirstBank => (self.regs.prg_bank & 0x0F),
            PrgRomMode::FixLastBank => {
                (self.cartridge.prg_rom.len() - PRG_ROM_BANK_SIZE as usize) as u8
            },
        }
    }

    fn prg_rom_address(bank: u8, address: u16) -> u16 {
        ((bank as u16) * PRG_ROM_BANK_SIZE) | (address & (PRG_ROM_BANK_SIZE - 1))
    }
}

impl Mapper for Mmc1 {
    fn prg_load_byte(&self, address: u16) -> u8 {
        if address < 0x6000 {
            0
        } else if address < 0x8000 {
            self.cartridge.prg_ram[(address & 0x1FFF) as usize]
        } else if address < 0xC000 {
            let rom_addr = Mmc1::prg_rom_address(self.prg_rom_bank_first(), address);
            self.cartridge.prg_rom[rom_addr as usize]
        } else {
            let rom_addr = Mmc1::prg_rom_address(self.prg_rom_bank_last(), address);
            self.cartridge.prg_rom[rom_addr as usize]
        }
    }

    fn prg_store_byte(&mut self, address: u16, value: u8) {
        if address < 0x8000 {
            self.cartridge.prg_ram[(address & 0x1FFF) as usize] = value;
        } else {
            if (value & 0x80) == 0 {
                let shift = self.shift;
                self.shift |= (value & 0x01) << (self.write_count as usize);
                if self.write_count < 4 {
                    self.write_count += 1;
                } else {
                    self.write_register(address, shift);
                    self.shift = 0;
                    self.write_count = 0;
                }
            } else {
                self.shift = 0;
                self.write_count = 0;
                self.regs.control |= 0x0C;
            }
        }
    }

    fn chr_load_byte(&self, address: u16) -> u8 {
        self.cartridge.chr_rom[address as usize]
    }

    fn chr_store_byte(&mut self, address: u16, value: u8) {
        self.cartridge.chr_rom[address as usize] = value
    }
}
