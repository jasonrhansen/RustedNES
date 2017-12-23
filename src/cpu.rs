use memory::Memory;

bitflags! {
    struct StatusFlags: u8 {
        const CARRY             = 1 << 0;
        const ZERO_RESULT       = 1 << 1;
        const INTERUPT_DISABLE  = 1 << 2;
        const DECIMAL_MODE      = 1 << 3;
        const BREAK_COMMAND     = 1 << 4;
        const EXPANSION         = 1 << 5;
        const OVERFLOW          = 1 << 6;
        const NEGATIVE_RESULT   = 1 << 7;
    }
}

impl Default for StatusFlags {
    // TODO: figure out what the initial values of the flags should be
    fn default() -> StatusFlags {
        StatusFlags::EXPANSION
    }
}

struct Regs {
    pc: u16,
    a: u8,
    x: u8,
    y: u8,
    sp: u8,
    status: StatusFlags,
}

impl Regs {
    fn new() -> Regs {
        Regs {
            pc: 0,
            a: 0,
            x: 0,
            y: 0,
            sp: 0xFD,
            status: StatusFlags::default(),
        }
    }
}

pub struct Cpu<M: Memory> {
    regs: Regs,
    mem: M,
}

impl<M: Memory> Memory for Cpu<M> {
    fn load_byte(&self, address: u16) -> u8 {
        self.mem.load_byte(address)
    }

    fn store_byte(&mut self, address: u16, value: u8) {
        self.mem.store_byte(address, value)
    }
}

impl<M: Memory> Cpu<M> {
    pub fn new(memory: M) -> Cpu<M> {
        Cpu {
            regs: Regs::new(),
            mem: memory,
        }
    }

    pub fn step(&mut self) {
        let op = self.next_pc_byte();
        match op {
            0x65 => self.adc(),
            _ => self.nop(),
        }
    }

    fn next_pc_byte(&mut self) -> u8 {
        let b = self.load_byte(self.regs.pc);
        self.regs.pc += 1;
        b
    }

    fn adc(&mut self) {

    }

    fn nop(&mut self) {

    }
}
