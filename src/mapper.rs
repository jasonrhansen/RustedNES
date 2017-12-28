use rom::NesRom;

pub trait Mapper {
    fn prg_load_byte(&self, address: u16) -> u8;
    fn prg_store_byte(&mut self, address: u16, value: u8);
    fn chr_load_byte(&self, address: u16) -> u8;
    fn chr_store_byte(&mut self, address: u16, value: u8);
}


pub fn create_mapper(rom: Box<NesRom>) -> Box<Mapper> {
    match rom.mapper {
        0 => Box::new(Nrom::new(rom)),
        _ => panic!("Unsupported mapper number: {}", rom.mapper)
    }
}

struct Nrom {
    rom: Box<NesRom>,
}

impl Nrom {
    fn new(rom: Box<NesRom>) -> Self {
       Nrom {
           rom,
       }
    }
}

impl Mapper for Nrom {
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