use memory::Memory;

pub struct Apu {

}

impl Apu {
    // Run for the given number of cpu cycles
    pub fn cycles(&mut self, cycles: u32) {
        unimplemented!()
    }
}

impl Memory for Apu {
    fn read_byte(&mut self, address: u16) -> u8 {
        unimplemented!()
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        unimplemented!()
    }
}
