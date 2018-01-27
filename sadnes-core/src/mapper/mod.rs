mod mapper0;
mod mapper1;
mod mapper2;
mod mapper4;

use self::mapper0::Mapper0;
use self::mapper1::Mapper1;
use self::mapper2::Mapper2;
use self::mapper4::Mapper4;
use super::ppu::Vram;
use super::cartridge::Cartridge;
use super::cpu::Cpu;
use super::ppu::Ppu;

pub trait Mapper {
    fn prg_read_byte(&mut self, address: u16) -> u8;
    fn prg_write_byte(&mut self, address: u16, value: u8);
    fn ppu_read_byte(&mut self, vram: &mut Vram, address: u16) -> u8;
    fn ppu_write_byte(&mut self, vram: &mut Vram, address: u16, value: u8);
    fn step(&mut self, cpu: &mut Cpu, ppu: &Ppu) {}
}

pub fn create_mapper(cartridge: Box<Cartridge>) -> Box<Mapper> {
    match cartridge.mapper {
        0 => Box::new(Mapper0::new(cartridge)),
        1 => Box::new(Mapper1::new(cartridge)),
        2 => Box::new(Mapper2::new(cartridge)),
        4 => Box::new(Mapper4::new(cartridge)),
        _ => panic!("Unsupported mapper number: {}", cartridge.mapper)
    }
}
