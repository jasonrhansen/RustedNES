use cpu::Interrupt;
use memory::Memory;

pub struct Apu {

}

impl Apu {
    // Run for the given number of cpu cycles
    pub fn cycles(&mut self, cycles: u32) {
        // TODO: Implement
    }
}

impl Memory for Apu {
    fn read_byte(&mut self, address: u16) -> u8 {
        // TODO: Implement
        0
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        // TODO: Implement
    }
}
