use memory::Memory;

bitflags! {
    struct StatusFlags: u8 {
        const CARRY            = 1 << 0;
        const ZERO_RESULT      = 1 << 1;
        const INTERUPT_DISABLE = 1 << 2;
        const DECIMAL_MODE     = 1 << 3;
        const BREAK_COMMAND    = 1 << 4;
        const EXPANSION        = 1 << 5;
        const OVERFLOW         = 1 << 6;
        const NEGATIVE_RESULT  = 1 << 7;
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

#[derive(Debug, Copy, Clone, PartialEq)]
enum Register8 {
    A,
    X,
    Y,
    Sp,
    Status,
}

#[derive(Copy, Clone, PartialEq)]
enum AddressMode {
    Immediate,
    Absolute,
    AbsoluteZeroPage,
    Indexed(Register8),
    IndexedZeroPage(Register8),
    IndexedIndirect(Register8),
    IndirectIndexed(Register8),
    Register(Register8),
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
            0x65 => self.adc(AddressMode::Absolute),
            _ => self.nop(),
        }
    }

    fn next_pc_byte(&mut self) -> u8 {
        let b = self.load_byte(self.regs.pc);
        self.regs.pc += 1;
        b
    }

    fn next_pc_word(&mut self) -> u16 {
        let w = self.load_word(self.regs.pc);
        self.regs.pc += 2;
        w
    }

    fn load_word_zero_page(&self, offset: u8) -> u16 {
        if offset == 255 {
            self.load_byte(255) as u16 +
                ((self.load_byte(0) as u16) << 8)
        } else {
            self.load_word(offset as u16)
        }
    }

    fn load(&mut self, am: AddressMode) -> u8 {
        use self::AddressMode::*;
        match am {
            Immediate => self.next_pc_byte(),
            Absolute => {
                let addr = self.next_pc_word();
                self.load_byte(addr)
            },
            AbsoluteZeroPage => {
                let addr = self.next_pc_byte() as u16;
                self.load_byte(addr)
            },
            Indexed(reg) => {
                let base = self.next_pc_word();
                let index = self.get_register(reg) as u16;
                self.load_byte(base + index)
            },
            IndexedZeroPage(reg) => {
                let base = self.next_pc_byte() as u16;
                let index = self.get_register(reg) as u16;
                self.load_byte(base + index)
            },
            IndexedIndirect(reg) => {
                let base = self.next_pc_byte();
                let index = self.get_register(reg);
                let addr = self.load_word_zero_page(base + index);
                self.load_byte(addr)
            },
            IndirectIndexed(reg) => {
                let zp_offset = self.next_pc_byte();
                let base = self.load_word_zero_page(zp_offset);
                let index = self.get_register(reg) as u16;
                self.load_byte(base + index)
            },
            Register(reg) => self.get_register(reg),
        }
    }

    fn get_flag(&self, sf: StatusFlags) -> bool {
        self.regs.status.contains(sf)
    }

    fn set_flags(&mut self, sf: StatusFlags, value: bool) {
        self.regs.status.set(sf, value);
    }

    fn set_zero_negative(&mut self, result: u8) {
        self.set_flags(StatusFlags::ZERO_RESULT, result == 0);
        self.set_flags(StatusFlags::NEGATIVE_RESULT, result | 0x80 != 0);
    }

    fn adc(&mut self, am: AddressMode) {
        let val = self.load(am);
        let a = self.regs.a;
        let result = a as u32 + val as u32 +
            if self.get_flag(StatusFlags::CARRY) { 1 } else { 0 };

        self.set_flags(StatusFlags::CARRY, result & 0x100 != 0);
        let result = result as u8;
        self.set_flags(StatusFlags::OVERFLOW,
                       ((a & 0x80) == (val & 0x80)) && (a & 0x80 != result & 0x80));
        self.set_zero_negative(result);

        self.regs.a = result;
    }

    fn sbc(&mut self, am: AddressMode) {
        let val = self.load(am);
        let a = self.regs.a;
        let result = a as u32 - val as u32 -
            if self.get_flag(StatusFlags::CARRY) { 0 } else { 1 };

        self.set_flags(StatusFlags::CARRY, result & 0x100 == 0);
        let result = result as u8;
        self.set_flags(StatusFlags::OVERFLOW,
                       !(((a & 0x80) != (val & 0x80)) && (a & 0x80 != result & 0x80)));
        self.set_zero_negative(result);

        self.regs.a = result;
    }

    fn nop(&mut self) {

    }

    fn get_register(&self, r: Register8) -> u8 {
        use self::Register8::*;
        match r {
            A      => self.regs.a,
            X      => self.regs.x,
            Y      => self.regs.y,
            Sp     => self.regs.sp,
            Status => self.regs.status.bits(),
        }
    }
}
