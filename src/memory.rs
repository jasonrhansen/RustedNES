pub trait Memory {
    fn load_byte(&self, address: u16) -> u8;
    fn store_byte(&mut self, address: u16, value: u8);

//    fn load_word(&self, address: u16) -> u16 {
//
//    }
//
//    fn store_word(&mut self, address: u16, value: u16) {
//
//    }
}

pub struct Ram { buf: [u8; 2048] }

impl Memory for Ram {
    fn load_byte(&self, address: u16) -> u8 {
        self.buf[address as usize]
    }

    fn store_byte(&mut self, address: u16, value: u8) {
        self.buf[address as usize] = value
    }
}

