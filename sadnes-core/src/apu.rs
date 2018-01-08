use memory::Memory;
use sink::*;

pub struct Apu {

}

impl Apu {
    // Run for the given number of cpu cycles
    pub fn cycles(&mut self, cycles: u32, audio_frame_sink: &mut Sink<AudioFrame>) {
//        unimplemented!()
    }
}

impl Memory for Apu {
    fn read_byte(&mut self, address: u16) -> u8 {
//        unimplemented!()
        0
    }

    fn write_byte(&mut self, address: u16, value: u8) {
//        unimplemented!()
    }
}
