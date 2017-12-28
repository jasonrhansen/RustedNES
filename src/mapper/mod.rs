mod nrom;
mod mmc1;

use self::nrom::Nrom;
use self::mmc1::Mmc1;
use super::rom::NesRom;

pub trait Mapper {
    fn prg_load_byte(&self, address: u16) -> u8;
    fn prg_store_byte(&mut self, address: u16, value: u8);
    fn chr_load_byte(&self, address: u16) -> u8;
    fn chr_store_byte(&mut self, address: u16, value: u8);
}

pub fn create_mapper(rom: Box<NesRom>) -> Box<Mapper> {
    match rom.mapper {
        0 => Box::new(Nrom::new(rom)),
        1 => Box::new(Mmc1::new(rom)),
        _ => panic!("Unsupported mapper number: {}", rom.mapper)
    }
}
