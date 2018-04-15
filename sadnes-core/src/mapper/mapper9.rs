use cartridge;
use cartridge::{Cartridge, Mirroring};
use mapper;
use mapper::Mapper;
use memory::Memory;
use ppu::Vram;

pub struct Mapper9 {
    cartridge: Box<Cartridge>,

    latch_0: u8,
    latch_1: u8,
    prg_rom_switchable_bank: u8,
    prg_rom_fixed_bank_1: u8,
    prg_rom_fixed_bank_2: u8,
    prg_rom_fixed_bank_3: u8,
    chr_fd_0000_bank: u8,
    chr_fe_0000_bank: u8,
    chr_fd_1000_bank: u8,
    chr_fe_1000_bank: u8,
}

#[derive(Deserialize, Serialize)]
pub struct State {
    pub cartridge: cartridge::State,
    pub latch_0: u8,
    pub latch_1: u8,
    pub prg_rom_switchable_bank: u8,
    pub prg_rom_fixed_bank_1: u8,
    pub prg_rom_fixed_bank_2: u8,
    pub prg_rom_fixed_bank_3: u8,
    pub chr_fd_0000_bank: u8,
    pub chr_fe_0000_bank: u8,
    pub chr_fd_1000_bank: u8,
    pub chr_fe_1000_bank: u8,
}

impl Mapper9 {
    pub fn new(cartridge: Box<Cartridge>) -> Self {
        let prg_rom_fixed_bank_1 = cartridge.prg_rom_num_banks * 2 - 3;
        let prg_rom_fixed_bank_2 = cartridge.prg_rom_num_banks * 2 - 2;
        let prg_rom_fixed_bank_3 = cartridge.prg_rom_num_banks * 2 - 1;
        Mapper9 {
            cartridge,
            prg_rom_switchable_bank: 0,
            prg_rom_fixed_bank_1,
            prg_rom_fixed_bank_2,
            prg_rom_fixed_bank_3,
            latch_0: 0,
            latch_1: 0,
            chr_fd_0000_bank: 0,
            chr_fe_0000_bank: 0,
            chr_fd_1000_bank: 0,
            chr_fe_1000_bank: 0,
        }
    }

    fn prg_rom_address(bank: u8, address: u16) -> usize {
        (bank as usize * 0x2000 as usize) | (address as usize & 0x1FFF)
    }

    fn chr_address(&self, address: u16) -> usize {
        let bank = if address < 0x1000 {
            if self.latch_0 == 0xFD {
                self.chr_fd_0000_bank
            } else {
                self.chr_fe_0000_bank
            }
        } else {
            if self.latch_1 == 0xFD {
                self.chr_fd_1000_bank
            } else {
                self.chr_fe_1000_bank
            }
        };

        (bank as usize * 0x1000 as usize) | (address as usize & 0x0FFF)
    }

    fn mirror_address(&self, address: u16) -> u16 {
        self.cartridge.mirroring.mirror_address(address)
    }
}

impl Mapper for Mapper9 {
    fn prg_read_byte(&mut self, address: u16) -> u8 {
        if address < 0x6000 {
            0
        } else if address < 0x8000 {
            self.cartridge.prg_ram[(address - 0x6000) as usize]
        } else if address < 0xA000 {
            let rom_addr = Mapper9::prg_rom_address(self.prg_rom_switchable_bank, address);
            self.cartridge.prg_rom[rom_addr as usize]
        } else if address < 0xC000 {
            let rom_addr = Mapper9::prg_rom_address(self.prg_rom_fixed_bank_1, address);
            self.cartridge.prg_rom[rom_addr as usize]
        } else if address < 0xE000 {
            let rom_addr = Mapper9::prg_rom_address(self.prg_rom_fixed_bank_2, address);
            self.cartridge.prg_rom[rom_addr as usize]
        } else {
            let rom_addr = Mapper9::prg_rom_address(self.prg_rom_fixed_bank_3, address);
            self.cartridge.prg_rom[rom_addr as usize]
        }
    }

    fn prg_write_byte(&mut self, address: u16, value: u8) {
        if address < 0x6000 {
        } else if address < 0x8000 {
            self.cartridge.prg_ram[(address - 0x6000) as usize] = value
        } else if address < 0xA000 {
        } else if address < 0xB000 {
            self.prg_rom_switchable_bank = value & 0x0F;
        } else if address < 0xC000 {
            self.chr_fd_0000_bank = value & 0x1F;
        } else if address < 0xD000 {
            self.chr_fe_0000_bank = value & 0x1F;
        } else if address < 0xE000 {
            self.chr_fd_1000_bank = value & 0x1F;
        } else if address < 0xF000 {
            self.chr_fe_1000_bank = value & 0x1F;
        } else {
            if value & 0x01 == 0 {
                self.cartridge.mirroring = Mirroring::Vertical;
            } else {
                self.cartridge.mirroring = Mirroring::Horizontal;
            }
        }
    }

    fn ppu_read_byte(&mut self, vram: &mut Vram, address: u16) -> u8 {
        if address < 0x2000 {
            let chr_addr = self.chr_address(address);
            if address == 0x0FD8 {
                self.latch_0 = 0xFD;
            } else if address == 0x0FE8 {
                self.latch_0 = 0xFE;
            } else if 0x1FD8 <= address && address <= 0x1FDF {
                self.latch_1 = 0xFD;
            } else if 0x1FE8 <= address && address <= 0x1FEF {
                self.latch_1 = 0xFE;
            }
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

    fn reset(&mut self) {
        self.cartridge.mirroring = self.cartridge.default_mirroring;
        self.prg_rom_switchable_bank = 0;
        self.prg_rom_fixed_bank_1;
        self.prg_rom_fixed_bank_2;
        self.prg_rom_fixed_bank_3;
        self.latch_0 = 0;
        self.latch_1 = 0;
        self.chr_fd_0000_bank = 0;
        self.chr_fe_0000_bank = 0;
        self.chr_fd_1000_bank = 0;
        self.chr_fe_1000_bank = 0;
    }

    fn get_state(&self) -> mapper::State {
        mapper::State::State9(
            State {
                cartridge: self.cartridge.get_state(),
                latch_0: self.latch_0,
                latch_1: self.latch_1,
                prg_rom_switchable_bank: self.prg_rom_switchable_bank,
                prg_rom_fixed_bank_1: self.prg_rom_fixed_bank_1,
                prg_rom_fixed_bank_2: self.prg_rom_fixed_bank_2,
                prg_rom_fixed_bank_3: self.prg_rom_fixed_bank_3,
                chr_fd_0000_bank: self.chr_fd_0000_bank,
                chr_fe_0000_bank: self.chr_fe_0000_bank,
                chr_fd_1000_bank: self.chr_fd_1000_bank,
                chr_fe_1000_bank: self.chr_fe_1000_bank,
            }
        )
    }

    fn apply_state(&mut self, state: &mapper::State) {
        match state {
            &mapper::State::State9(ref state) => {
                self.cartridge.apply_state(&state.cartridge);
                self.latch_0 = state.latch_0;
                self.latch_1 = state.latch_1;
                self.prg_rom_switchable_bank = state.prg_rom_switchable_bank;
                self.prg_rom_fixed_bank_1 = state.prg_rom_fixed_bank_1;
                self.prg_rom_fixed_bank_2 = state.prg_rom_fixed_bank_2;
                self.prg_rom_fixed_bank_3 = state.prg_rom_fixed_bank_3;
                self.chr_fd_0000_bank = state.chr_fd_0000_bank;
                self.chr_fe_0000_bank = state.chr_fe_0000_bank;
                self.chr_fd_1000_bank = state.chr_fd_1000_bank;
                self.chr_fe_1000_bank = state.chr_fe_1000_bank;
            },
            _ => panic!("Invalid mapper state enum variant in apply_state"),
        }
    }
}
