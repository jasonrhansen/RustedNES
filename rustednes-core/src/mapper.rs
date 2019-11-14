mod mapper0;
mod mapper1;
mod mapper2;
mod mapper3;
mod mapper4;
mod mapper7;
mod mapper9;

use self::mapper0::Mapper0;
use self::mapper1::Mapper1;
use self::mapper2::Mapper2;
use self::mapper3::Mapper3;
use self::mapper4::Mapper4;
use self::mapper7::Mapper7;
use self::mapper9::Mapper9;
use super::cartridge::{Cartridge, Mirroring};
use super::cpu::Cpu;
use super::ppu::Ppu;

use serde_derive::{Deserialize, Serialize};

use std::ptr;

pub trait Mapper {
    fn prg_read_byte(&mut self, address: u16) -> u8;
    fn prg_write_byte(&mut self, address: u16, value: u8);
    fn chr_read_byte(&mut self, address: u16) -> u8;
    fn chr_write_byte(&mut self, address: u16, value: u8);

    fn mirroring(&self) -> Mirroring;

    // Called for every PPU cycle. Most mappers don't need to do anything.
    fn step(&mut self, _cpu: &mut Cpu, _ppu: &Ppu) {}

    fn sram(&mut self) -> *mut u8 {
        ptr::null_mut()
    }

    fn sram_size(&self) -> usize {
        0
    }

    fn reset(&mut self);
    fn get_state(&self) -> State;
    fn apply_state(&mut self, state: &State);
}

pub fn create_mapper(cartridge: Cartridge) -> Box<dyn Mapper> {
    match cartridge.mapper {
        0 => Box::new(Mapper0::new(cartridge)),
        1 => Box::new(Mapper1::new(cartridge)),
        2 => Box::new(Mapper2::new(cartridge)),
        3 => Box::new(Mapper3::new(cartridge)),
        4 => Box::new(Mapper4::new(cartridge)),
        7 => Box::new(Mapper7::new(cartridge)),
        9 => Box::new(Mapper9::new(cartridge)),
        _ => panic!("Unsupported mapper number: {}", cartridge.mapper),
    }
}

#[derive(Deserialize, Serialize)]
pub enum State {
    State0(mapper0::State),
    State1(mapper1::State),
    State2(mapper2::State),
    State3(mapper3::State),
    State4(mapper4::State),
    State7(mapper7::State),
    State9(mapper9::State),
}
