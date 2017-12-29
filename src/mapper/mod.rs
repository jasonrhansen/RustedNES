mod nrom;
mod mmc1;

use self::nrom::Nrom;
use self::mmc1::Mmc1;
use super::cartridge::Cartridge;

pub trait Mapper {
    fn prg_read_byte(&self, address: u16) -> u8;
    fn prg_write_byte(&mut self, address: u16, value: u8);
    fn chr_read_byte(&self, address: u16) -> u8;
    fn chr_write_byte(&mut self, address: u16, value: u8);
}

pub fn create_mapper(cartridge: Box<Cartridge>) -> Box<Mapper> {
    match cartridge.mapper {
        0 => Box::new(Nrom::new(cartridge)),
        1 => Box::new(Mmc1::new(cartridge)),
        _ => panic!("Unsupported mapper number: {}", cartridge.mapper)
    }
}
