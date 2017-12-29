use mapper::Mapper;
use cartridge::Cartridge;

#[derive(Default)]
struct Regs {
    control: u8,
    prg_bank: u8,
    chr_bank_1: u8,
    chr_bank_2: u8,
}

pub struct Mmc1 {
    cartridge: Box<Cartridge>,
    shift: u8,
    write_count: u8,
    prg_mode: PrgRomMode,
    chr_mode: ChrRomMode,
    regs: Regs,
}

enum PrgRomMode {
    Mode1,
    Mode2,
    Mode3,
}

enum ChrRomMode {
    EightKb,
    FourKb,
}

impl Mmc1 {
    pub fn new(cartridge: Box<Cartridge>) -> Self {
        Mmc1 {
            cartridge,
            shift: 0x10,
            write_count: 0,
            prg_mode: PrgRomMode::Mode1,
            chr_mode: ChrRomMode::EightKb,
            regs: Regs::default(),
        }
    }
}

impl Mapper for Mmc1 {
    fn prg_load_byte(&self, address: u16) -> u8 {
        if address < 0x6000 {
            0
        } else if address < 0x8000 {
            self.cartridge.prg_ram[(address & 0x1FFF) as usize]
        } else if address < 0xC000 {
            let address = address & 0x7FFF;
            let bank = address >> 15;

            //TODO: Implement
            0
        } else {
            //TODO: Implement
            0
        }
    }

    fn prg_store_byte(&mut self, address: u16, value: u8) {
        //TODO: Implement
    }

    fn chr_load_byte(&self, address: u16) -> u8 {
        //TODO: Implement
        0
    }

    fn chr_store_byte(&mut self, address: u16, value: u8) {
        //TODO: Implement
    }
}
