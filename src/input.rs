use memory::Memory;

pub struct Input {

}

impl Memory for Input {
    fn read_byte(&mut self, address: u16) -> u8 {
        unimplemented!()
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        unimplemented!()
    }
}
