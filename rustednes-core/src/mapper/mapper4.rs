use crate::cartridge::{self, Cartridge, Mirroring};
use crate::cpu::{Cpu, Interrupt};
use crate::mapper::{self, Mapper};
use crate::memory::Memory;
use crate::ppu::{self, Ppu, Vram};

use serde_derive::{Deserialize, Serialize};

pub struct Mapper4 {
    cartridge: Cartridge,

    next_bank_register: u8,
    bank_registers: [u8; 8],

    prg_rom_mode: PrgRomMode,
    chr_a12_inversion: ChrA12Inversion,

    irq_enable: bool,
    irq_counter: u8,
    irq_counter_reload_value: u8,

    prg_rom_bank_offsets: [usize; 4],
    chr_bank_offsets: [usize; 8],
}

#[derive(Debug, Copy, Clone, Deserialize, Serialize)]
pub enum PrgRomMode {
    Zero, // $8000-$9FFF swappable, $C000-$DFFF fixed to second-last bank
    One,  // $C000-$DFFF swappable, $8000-$9FFF fixed to second-last bank
}

#[derive(Debug, Copy, Clone, Deserialize, Serialize)]
pub enum ChrA12Inversion {
    Zero, // Two 2 KB banks at $0000-$0FFF, four 1 KB banks at $1000-$1FFF
    One,  // Two 2 KB banks at $1000-$1FFF, four 1 KB banks at $0000-$0FFF
}

#[derive(Deserialize, Serialize)]
pub struct State {
    pub cartridge: cartridge::State,
    pub next_bank_register: u8,
    pub bank_registers: [u8; 8],
    pub prg_rom_mode: PrgRomMode,
    pub chr_a12_inversion: ChrA12Inversion,
    pub irq_enable: bool,
    pub irq_counter: u8,
    pub irq_counter_reload_value: u8,
    pub prg_rom_bank_offsets: [usize; 4],
    pub chr_bank_offsets: [usize; 8],
}

impl Mapper4 {
    pub fn new(cartridge: Cartridge) -> Self {
        let mut m = Mapper4 {
            cartridge,
            next_bank_register: 0,
            bank_registers: [0, 0, 0, 0, 0, 0, 0, 1],
            prg_rom_mode: PrgRomMode::Zero,
            chr_a12_inversion: ChrA12Inversion::Zero,
            irq_enable: false,
            irq_counter: 0,
            irq_counter_reload_value: 0,
            prg_rom_bank_offsets: [0; 4],
            chr_bank_offsets: [0; 8],
        };
        m.update_banks();
        m
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
        self.update_banks();
    }

    fn write_bank_data(&mut self, value: u8) {
        self.bank_registers[self.next_bank_register as usize] = value;
        self.update_banks();
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

    fn prg_bank_address(&self, bank: u8) -> usize {
        let bank = (bank as usize) % (self.cartridge.prg_rom.len() / 0x2000);
        bank * 0x2000
    }

    fn chr_bank_address(&self, bank: u8) -> usize {
        let bank = (bank as usize) % (self.cartridge.chr.len() / 0x0400);
        bank * 0x0400
    }

    fn update_banks(&mut self) {
        self.prg_rom_bank_offsets[1] = self.prg_bank_address(self.bank_registers[7] & 0x3F);
        self.prg_rom_bank_offsets[3] =
            self.prg_bank_address(self.cartridge.prg_rom_num_banks * 2 - 1);

        match self.prg_rom_mode {
            PrgRomMode::Zero => {
                self.prg_rom_bank_offsets[0] = self.prg_bank_address(self.bank_registers[6] & 0x3F);
                self.prg_rom_bank_offsets[2] =
                    self.prg_bank_address(self.cartridge.prg_rom_num_banks * 2 - 2);
            }
            PrgRomMode::One => {
                self.prg_rom_bank_offsets[0] =
                    self.prg_bank_address(self.cartridge.prg_rom_num_banks * 2 - 2);
                self.prg_rom_bank_offsets[2] = self.prg_bank_address(self.bank_registers[6] & 0x3F);
            }
        }

        match self.chr_a12_inversion {
            ChrA12Inversion::Zero => {
                self.chr_bank_offsets[0] = self.chr_bank_address(self.bank_registers[0] & 0xFE);
                self.chr_bank_offsets[1] = self.chr_bank_address(self.bank_registers[0] | 0x01);
                self.chr_bank_offsets[2] = self.chr_bank_address(self.bank_registers[1] & 0xFE);
                self.chr_bank_offsets[3] = self.chr_bank_address(self.bank_registers[1] | 0x01);
                self.chr_bank_offsets[4] = self.chr_bank_address(self.bank_registers[2]);
                self.chr_bank_offsets[5] = self.chr_bank_address(self.bank_registers[3]);
                self.chr_bank_offsets[6] = self.chr_bank_address(self.bank_registers[4]);
                self.chr_bank_offsets[7] = self.chr_bank_address(self.bank_registers[5]);
            }
            ChrA12Inversion::One => {
                self.chr_bank_offsets[0] = self.chr_bank_address(self.bank_registers[2]);
                self.chr_bank_offsets[1] = self.chr_bank_address(self.bank_registers[3]);
                self.chr_bank_offsets[2] = self.chr_bank_address(self.bank_registers[4]);
                self.chr_bank_offsets[3] = self.chr_bank_address(self.bank_registers[5]);
                self.chr_bank_offsets[4] = self.chr_bank_address(self.bank_registers[0] & 0xFE);
                self.chr_bank_offsets[5] = self.chr_bank_address(self.bank_registers[0] | 0x01);
                self.chr_bank_offsets[6] = self.chr_bank_address(self.bank_registers[1] & 0xFE);
                self.chr_bank_offsets[7] = self.chr_bank_address(self.bank_registers[1] | 0x01);
            }
        }
    }

    fn mirror_address(&self, address: u16) -> u16 {
        self.cartridge.mirroring.mirror_address(address)
    }

    fn handle_scanline(&mut self, cpu: &mut Cpu) {
        if self.irq_counter == 0 {
            self.irq_counter = self.irq_counter_reload_value;
        } else {
            self.irq_counter -= 1;

            if self.irq_counter == 0 && self.irq_enable {
                cpu.request_interrupt(Interrupt::Irq);
            }
        }
    }

    fn read_prg_rom(&self, address: u16) -> u8 {
        let addr = self.prg_rom_bank_offsets[(address as usize - 0x8000) / 0x2000]
            | (address as usize & 0x1FFF);
        self.cartridge.prg_rom[addr]
    }

    fn chr_address(&self, address: u16) -> usize {
        self.chr_bank_offsets[(address as usize) / 0x0400] | (address as usize & 0x03FF)
    }

    fn read_chr(&self, address: u16) -> u8 {
        self.cartridge.chr[self.chr_address(address)]
    }

    fn write_chr(&mut self, address: u16, value: u8) {
        let addr = self.chr_address(address);
        self.cartridge.chr[addr] = value;
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
        if address < 0x6000 {
        } else if address < 0x8000 {
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
                self.irq_counter = 0;
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
        if address < 0x2000 {
            self.write_chr(address, value);
        } else {
            vram.write_byte(self.mirror_address(address) - 0x2000, value);
        }
    }

    fn step(&mut self, cpu: &mut Cpu, ppu: &Ppu) {
        if ppu.rendering_enabled()
            && ppu.scanline <= ppu::VISIBLE_END_SCANLINE
            && ppu.scanline_cycle() == 260
        {
            self.handle_scanline(cpu);
        }
    }

    fn sram(&mut self) -> *mut &[u8] {
        self.cartridge.prg_ram.as_mut_ptr() as *mut _
    }

    fn sram_size(&self) -> usize {
        self.cartridge.prg_ram.len()
    }

    fn reset(&mut self) {
        self.cartridge.mirroring = self.cartridge.default_mirroring;
        self.next_bank_register = 0;
        self.bank_registers = [0, 0, 0, 0, 0, 0, 0, 1];
        self.prg_rom_mode = PrgRomMode::Zero;
        self.chr_a12_inversion = ChrA12Inversion::Zero;
        self.irq_enable = false;
        self.irq_counter = 0;
        self.irq_counter_reload_value = 0;
        self.prg_rom_bank_offsets = [0; 4];
        self.chr_bank_offsets = [0; 8];
        self.update_banks();
    }

    fn get_state(&self) -> mapper::State {
        mapper::State::State4(State {
            cartridge: self.cartridge.get_state(),
            next_bank_register: self.next_bank_register,
            bank_registers: self.bank_registers,
            prg_rom_mode: self.prg_rom_mode,
            chr_a12_inversion: self.chr_a12_inversion,
            irq_enable: self.irq_enable,
            irq_counter: self.irq_counter,
            irq_counter_reload_value: self.irq_counter_reload_value,
            prg_rom_bank_offsets: self.prg_rom_bank_offsets,
            chr_bank_offsets: self.chr_bank_offsets,
        })
    }

    fn apply_state(&mut self, state: &mapper::State) {
        match state {
            &mapper::State::State4(ref state) => {
                self.cartridge.apply_state(&state.cartridge);
                self.next_bank_register = state.next_bank_register;
                self.bank_registers = state.bank_registers;
                self.prg_rom_mode = state.prg_rom_mode;
                self.chr_a12_inversion = state.chr_a12_inversion;
                self.irq_enable = state.irq_enable;
                self.irq_counter = state.irq_counter;
                self.irq_counter_reload_value = state.irq_counter_reload_value;
                self.prg_rom_bank_offsets = state.prg_rom_bank_offsets;
                self.chr_bank_offsets = state.chr_bank_offsets;
            }
            _ => panic!("Invalid mapper state enum variant in apply_state"),
        }
    }
}
