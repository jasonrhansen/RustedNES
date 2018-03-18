use cartridge::{Cartridge, Mirroring, PRG_ROM_BANK_SIZE};
use mapper::Mapper;
use memory::Memory;
use ppu::Vram;

struct Regs {
    control: u8,
    prg_bank: u8,
    chr_bank_0: u8,
    chr_bank_1: u8,
}

pub struct Mapper1 {
    cartridge: Box<Cartridge>,
    shift: u8,
    regs: Regs,
}

#[derive(Debug)]
enum PrgRomMode {
    Switch32Kb,    // Switch 32 KB at $8000, ignoring low bit of bank number
    FixFirstBank,  // Fix first bank at $8000 and switch 16 KB bank at $C000
    FixLastBank,   // Fix last bank at $C000 and switch 16 KB bank at $8000
}

#[derive(Debug)]
enum ChrRomMode {
    Switch8Kb,     // Switch 8 KB at a time
    Switch4Kb,     // Switch two separate 4 KB banks
}

// Put a 1 in bit 4 so we can detect when we've shifted enough to write to a register
const SHIFT_REGISTER_DEFAULT: u8 = 0x10;

impl Mapper1 {
    pub fn new(cartridge: Box<Cartridge>) -> Self {
        Mapper1 {
            cartridge,
            shift: SHIFT_REGISTER_DEFAULT,
            regs: Regs {
                control: 0x0C,
                prg_bank: 0,
                chr_bank_0: 0,
                chr_bank_1: 0,
            },
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
            self.regs.control = shift & 0x1F;
            self.cartridge.mirroring = match self.regs.control & 0x03 {
                0 => Mirroring::OneScreenLower,
                1 => Mirroring::OneScreenUpper,
                2 => Mirroring::Vertical,
                _ => Mirroring::Horizontal,
            };
        } else if address < 0xC000 {
            self.regs.chr_bank_0 = shift & 0x1F;
        } else if address <  0xE000 {
            self.regs.chr_bank_1 = shift & 0x1F;
        } else {
            self.regs.prg_bank = shift & 0x0F;
        }

    }

    fn prg_rom_bank_first(&self) -> u8 {
        match self.prg_rom_mode() {
            PrgRomMode::Switch32Kb => self.regs.prg_bank & 0xFE,
            PrgRomMode::FixFirstBank => 0,
            PrgRomMode::FixLastBank => self.regs.prg_bank,
        }
    }

    fn prg_rom_bank_last(&self) -> u8 {
        match self.prg_rom_mode() {
            PrgRomMode::Switch32Kb => (self.regs.prg_bank & 0xFE) | 0x01,
            PrgRomMode::FixFirstBank => self.regs.prg_bank,
            PrgRomMode::FixLastBank =>  self.cartridge.prg_rom_num_banks - 1,
        }
    }

    fn prg_rom_address(&self, bank: u8, address: u16) -> usize {
        (bank as usize * PRG_ROM_BANK_SIZE as usize) |
            (address as usize & (PRG_ROM_BANK_SIZE as usize - 1))
    }

    fn chr_address(&self, address: u16) -> usize {
        match self.chr_rom_mode() {
            ChrRomMode::Switch4Kb => {
                let bank = if address < 0x1000 {
                    self.regs.chr_bank_0
                } else {
                    self.regs.chr_bank_1
                };
                (bank as usize * 0x1000) | (address as usize & 0x0FFF)
            },
            ChrRomMode::Switch8Kb => {
                let bank = self.regs.chr_bank_0;
                (bank as usize * 0x2000) | (address as usize & 0x1FFF)
            }
        }
    }

    fn mirror_address(&self, address: u16) -> u16 {
        self.cartridge.mirroring.mirror_address(address)
    }
}

impl Mapper for Mapper1 {
    fn prg_read_byte(&mut self, address: u16) -> u8 {
        if address < 0x6000 {
            0
        } else if address < 0x8000 {
            self.cartridge.prg_ram[(address - 0x6000) as usize]
        } else if address < 0xC000 {
            let rom_addr = self.prg_rom_address(self.prg_rom_bank_first(), address);
            self.cartridge.prg_rom[rom_addr as usize]
        } else {
            let rom_addr = self.prg_rom_address(self.prg_rom_bank_last(), address);
            self.cartridge.prg_rom[rom_addr as usize]
        }
    }

    fn prg_write_byte(&mut self, address: u16, value: u8) {
        if address < 0x6000 {
            // Do nothing
        } else if address < 0x8000 {
            self.cartridge.prg_ram[(address - 0x6000) as usize] = value
        } else {
            if (value & 0x80) == 0 {
                // If a 1 has been shifted into bit 0, it's time to write to a register
                let is_last_shift = (self.shift & 0x01) != 0;

                // Bit 0 of the value gets shifted into the shift
                // register from the left, starting at bit 4.
                self.shift = (self.shift >> 1) | ((value & 0x01) << 4);

                if is_last_shift {
                    let shift = self.shift;
                    self.write_register(address, shift);
                    self.shift = SHIFT_REGISTER_DEFAULT;
                }
            } else {
                // Writing a value with bit 7 set clears the shift register to its initial state
                self.shift = SHIFT_REGISTER_DEFAULT;
                self.regs.control |= 0x0C;
            }
        }
    }

    fn ppu_read_byte(&mut self, vram: &mut Vram, address: u16) -> u8 {
        if address < 0x2000 {
            let chr_addr = self.chr_address(address);
            self.cartridge.chr[chr_addr as usize]
        } else {
            vram.read_byte(self.mirror_address(address) - 0x2000)
        }
    }

    fn ppu_write_byte(&mut self, vram: &mut Vram, address: u16, value: u8) {
        if address < 0x2000 {
            let chr_addr = self.chr_address(address);
            self.cartridge.chr[chr_addr as usize] = value
        } else {
            vram.write_byte(self.mirror_address(address) - 0x2000, value);
        }
    }
}
