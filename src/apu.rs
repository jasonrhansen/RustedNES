use memory::Memory;

pub struct Apu {

}

impl Memory for Apu {
    fn read_byte(&self, address: u16) -> u8 {
        //TODO: Implement
        0
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        //TODO: Implement
    }
}