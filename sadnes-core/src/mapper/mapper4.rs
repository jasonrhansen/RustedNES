use cartridge::{Cartridge, Mirroring, CHR_ROM_BANK_SIZE, PRG_ROM_BANK_SIZE};
use mapper::Mapper;
use memory::Memory;
use ppu::{self, Ppu, Vram};
use cpu::{Cpu, Interrupt};


pub struct Mapper4 {
    cartridge: Box<Cartridge>,

    next_bank_register: u8,
    bank_registers: [u8; 8],

    prg_rom_mode: PrgRomMode,
    chr_a12_inversion: ChrA12Inversion,

    irq_enable: bool,
    irq_reload: bool,
    irq_counter: u8,
    irq_counter_reload_value: u8,
}

#[derive(Debug)]
enum PrgRomMode {
    Zero,       // $8000-$9FFF swappable, $C000-$DFFF fixed to second-last bank
    One,        // $C000-$DFFF swappable, $8000-$9FFF fixed to second-last bank
}

#[derive(Debug)]
enum ChrA12Inversion {
    Zero,       // Two 2 KB banks at $0000-$0FFF, four 1 KB banks at $1000-$1FFF
    One,        // Two 2 KB banks at $1000-$1FFF, four 1 KB banks at $0000-$0FFF
}

impl Mapper4 {
    pub fn new(cartridge: Box<Cartridge>) -> Self {
        Mapper4 {
            cartridge,
            next_bank_register: 0,
            bank_registers: [0, 0, 0, 0 , 0, 0, 0, 1],
            prg_rom_mode: PrgRomMode::Zero,
            chr_a12_inversion: ChrA12Inversion::Zero,
            irq_enable: false,
            irq_reload: false,
            irq_counter: 0,
            irq_counter_reload_value: 0,
        }
    }

    fn write_bank_select(&mut self, value: u8) {
        self.next_bank_register = value & 0x07;

        self.prg_rom_mode = if value & 0x40 == 0 {
            PrgRomMode::Zero
        } else {
            PrgRomMode::One
        };

        self.chr_a12_inversion = if value & 0x80 == 0 {
            ChrA12Inversion::Zero
        } else {
            ChrA12Inversion::One
        };
    }

    fn write_bank_data(&mut self, value: u8) {
        self.bank_registers[self.next_bank_register as usize] = value;
    }

    fn write_mirroring(&mut self, value: u8) {
        if self.cartridge.mirroring != Mirroring::FourScreen {
            self.cartridge.mirroring = if value & 0x01 == 0 {
                Mirroring::Vertical
            } else {
                Mirroring::Horizontal
            };
        }
    }

    fn write_prg_ram_protect(&mut self, _value: u8) {
        // Probably don't need to implement this
    }

    fn write_irq_latch(&mut self, value: u8) {
        self.irq_counter_reload_value = value;
    }

    fn chr_address(bank: u8, address: u16) -> usize {
        (bank as usize * 0x0400) | (address as usize & 0x03FF)
    }

    fn prg_rom_address(bank: u8, address: u16) -> usize {
        (bank as usize * 0x2000 as usize) | (address as usize & 0x1FFF)
    }

    fn mirror_address(&self, address: u16) -> u16 {
        self.cartridge.mirroring.mirror_address(address)
    }

    fn read_chr(&mut self, address: u16) -> u8 {
        let bank = if address < 0x0400 {
            match self.chr_a12_inversion {
                ChrA12Inversion::Zero => {
                    self.bank_registers[0] & 0xFE
                },
                ChrA12Inversion::One => {
                    self.bank_registers[2]
                },
            }
        } else if address < 0x0800 {
            match self.chr_a12_inversion {
                ChrA12Inversion::Zero => {
                    self.bank_registers[0] | 0x01
                },
                ChrA12Inversion::One => {
                    self.bank_registers[3]
                },
            }
        } else if address < 0x0C00 {
            match self.chr_a12_inversion {
                ChrA12Inversion::Zero => {
                    self.bank_registers[1] & 0xFE
                },
                ChrA12Inversion::One => {
                    self.bank_registers[4]
                },
            }
        } else if address < 0x1000 {
            match self.chr_a12_inversion {
                ChrA12Inversion::Zero => {
                    self.bank_registers[1] | 0x01
                },
                ChrA12Inversion::One => {
                    self.bank_registers[5]
                },
            }
        } else if address < 0x1400 {
            match self.chr_a12_inversion {
                ChrA12Inversion::Zero => {
                    self.bank_registers[2]
                },
                ChrA12Inversion::One => {
                    self.bank_registers[0] & 0xFE
                },
            }
        } else if address < 0x1800 {
            match self.chr_a12_inversion {
                ChrA12Inversion::Zero => {
                    self.bank_registers[3]
                },
                ChrA12Inversion::One => {
                    self.bank_registers[0] | 0x01
                },
            }
        } else if address < 0x1C00 {
            match self.chr_a12_inversion {
                ChrA12Inversion::Zero => {
                    self.bank_registers[4]
                },
                ChrA12Inversion::One => {
                    self.bank_registers[1] & 0xFE
                },
            }
        } else {
            match self.chr_a12_inversion {
                ChrA12Inversion::Zero => {
                    self.bank_registers[5]
                },
                ChrA12Inversion::One => {
                    self.bank_registers[1] | 0x01
                },
            }
        };

        let chr_addr = Mapper4::chr_address(bank, address);
        self.cartridge.chr[chr_addr as usize]
    }

    fn read_prg_rom(&mut self, address: u16) -> u8 {
        let bank = if address < 0xA000 {
            match self.prg_rom_mode {
                PrgRomMode::Zero => {
                    self.bank_registers[6]
                },
                PrgRomMode::One => {
                    self.cartridge.prg_rom_num_banks * 2 - 2
                },
            }
        } else if address < 0xC000 {
            self.bank_registers[7]
        } else if address < 0xE000 {
            match self.prg_rom_mode {
                PrgRomMode::Zero => {
                    self.cartridge.prg_rom_num_banks * 2 - 2
                },
                PrgRomMode::One => {
                    self.bank_registers[6]
                },
            }
        } else {
            self.cartridge.prg_rom_num_banks * 2 - 1
        };

        let rom_addr = Mapper4::prg_rom_address(bank, address);
        self.cartridge.prg_rom[rom_addr as usize]
    }

    fn handle_scanline(&mut self, cpu: &mut Cpu) {
        if self.irq_counter == 0 || self.irq_reload {
            self.irq_counter = self.irq_counter_reload_value;
        } else {
            self.irq_counter -= 1;

            if self.irq_counter == 0 && self.irq_enable {
                cpu.request_interrupt(Interrupt::Irq);
            }
        }
    }
}

impl Mapper for Mapper4 {
    fn prg_read_byte(&mut self, address: u16) -> u8 {
        if address < 0x6000 {
            0
        } else if address < 0x8000 {
            self.cartridge.prg_ram[(address - 0x6000) as usize]
        } else {
            self.read_prg_rom(address)
        }
    }

    fn prg_write_byte(&mut self, address: u16, value: u8) {
        if 0x6000 <= address && address < 0x8000 {
            self.cartridge.prg_ram[(address - 0x6000) as usize] = value
        } else if address < 0xA000 {
            if address & 0x01 == 0 {
                self.write_bank_select(value);
            } else {
                self.write_bank_data(value);
            }
        } else if address < 0xC000 {
            if address & 0x01 == 0 {
                self.write_mirroring(value);
            } else {
                self.write_prg_ram_protect(value);
            }
        } else if address < 0xE000 {
            if address & 0x01 == 0 {
                self.write_irq_latch(value);
            } else {
                self.irq_reload = true;
            }
        } else {
            if address & 0x01 == 0 {
                self.irq_enable = false;
            } else {
                self.irq_enable = true;
            }
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

    fn step(&mut self, cpu: &mut Cpu, ppu: &Ppu) {
        if ppu.rendering_enabled() && ppu.scanline <= ppu::VISIBLE_END_SCANLINE && ppu.scanline_cycle() == 260 {
            self.handle_scanline(cpu);
        }
    }
}
