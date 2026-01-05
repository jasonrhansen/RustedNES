pub struct OamDma {
    pub active: bool,
    pub page: u8,
    pub count: u16,
    pub data: u8,
    pub dummy_read: bool,
}

impl OamDma {
    pub fn new() -> Self {
        Self {
            active: false,
            page: 0,
            count: 0,
            data: 0,
            dummy_read: false,
        }
    }

    pub fn activate(&mut self, page: u8) {
        self.page = page;
        self.active = true;
        self.count = 0;
        self.dummy_read = true;
    }
}
