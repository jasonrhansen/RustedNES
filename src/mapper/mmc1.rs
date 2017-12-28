use mapper::Mapper;
use rom::NesRom;

pub struct Mmc1 {
    rom: Box<NesRom>,
}

impl Mmc1 {
    pub fn new(rom: Box<NesRom>) -> Self {
        Mmc1 {
            rom,
        }
    }
}

impl Mapper for Mmc1 {
    fn prg_load_byte(&self, address: u16) -> u8 {
        //TODO: Implement
        0
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
