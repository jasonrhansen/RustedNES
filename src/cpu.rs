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

#[derive(Debug, Copy, Clone, PartialEq)]
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
    cycles: u64,
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
            cycles: 0,
            regs: Regs::new(),
            mem: memory,
        }
    }

    pub fn step(&mut self) {
        let op = self.next_pc_byte();
        match op {
            0xA9 => self.lda(AddressMode::Immediate),
            0xA5 => self.lda(AddressMode::AbsoluteZeroPage),
            0xB5 => self.lda(AddressMode::IndexedZeroPage(Register8::X)),
            0xAD => self.lda(AddressMode::Absolute),
            0xBD => self.lda(AddressMode::Indexed(Register8::X)),
            0xB9 => self.lda(AddressMode::Indexed(Register8::Y)),
            0xA1 => self.lda(AddressMode::IndexedIndirect(Register8::X)),
            0xB1 => self.lda(AddressMode::IndirectIndexed(Register8::Y)),

            0xA2 => self.ldx(AddressMode::Immediate),
            0xA6 => self.ldx(AddressMode::AbsoluteZeroPage),
            0xB6 => self.ldx(AddressMode::IndexedZeroPage(Register8::Y)),
            0xAE => self.ldx(AddressMode::Absolute),
            0xBE => self.ldx(AddressMode::Indexed(Register8::Y)),

            0xA0 => self.ldy(AddressMode::Immediate),
            0xA4 => self.ldy(AddressMode::AbsoluteZeroPage),
            0xB4 => self.ldy(AddressMode::IndexedZeroPage(Register8::X)),
            0xAC => self.ldy(AddressMode::Absolute),
            0xBC => self.ldy(AddressMode::Indexed(Register8::X)),

            0x85 => self.sta(AddressMode::AbsoluteZeroPage),
            0x95 => self.sta(AddressMode::IndexedZeroPage(Register8::X)),
            0x8D => self.sta(AddressMode::Absolute),
            0x9D => self.sta(AddressMode::Indexed(Register8::X)),
            0x99 => self.sta(AddressMode::Indexed(Register8::Y)),
            0x81 => self.sta(AddressMode::IndexedIndirect(Register8::X)),
            0x91 => self.sta(AddressMode::IndirectIndexed(Register8::Y)),

            0x86 => self.stx(AddressMode::AbsoluteZeroPage),
            0x96 => self.stx(AddressMode::IndexedZeroPage(Register8::Y)),
            0x8E => self.stx(AddressMode::Absolute),

            0x84 => self.sty(AddressMode::AbsoluteZeroPage),
            0x94 => self.sty(AddressMode::IndexedZeroPage(Register8::X)),
            0x8C => self.sty(AddressMode::Absolute),

            0x69 => self.adc(AddressMode::Immediate),
            0x65 => self.adc(AddressMode::AbsoluteZeroPage),
            0x75 => self.adc(AddressMode::IndexedZeroPage(Register8::X)),
            0x6D => self.adc(AddressMode::Absolute),
            0x7D => self.adc(AddressMode::Indexed(Register8::X)),
            0x79 => self.adc(AddressMode::Indexed(Register8::Y)),
            0x61 => self.adc(AddressMode::IndexedIndirect(Register8::X)),
            0x71 => self.adc(AddressMode::IndirectIndexed(Register8::Y)),

            0xE9 => self.sbc(AddressMode::Immediate),
            0xE5 => self.sbc(AddressMode::AbsoluteZeroPage),
            0xF5 => self.sbc(AddressMode::IndexedZeroPage(Register8::X)),
            0xED => self.sbc(AddressMode::Absolute),
            0xFD => self.sbc(AddressMode::Indexed(Register8::X)),
            0xF9 => self.sbc(AddressMode::Indexed(Register8::Y)),
            0xE1 => self.sbc(AddressMode::IndexedIndirect(Register8::X)),
            0xF1 => self.sbc(AddressMode::IndirectIndexed(Register8::Y)),
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

    fn store(&mut self, am: AddressMode, val: u8) {
        use self::AddressMode::*;
        match am {
            Absolute => {
                let addr = self.next_pc_word();
                self.store_byte(addr, val);
            },
            AbsoluteZeroPage => {
                let addr = self.next_pc_byte() as u16;
                self.store_byte(addr, val);
            },
            Indexed(reg) => {
                let base = self.next_pc_word();
                let index = self.get_register(reg) as u16;
                self.store_byte(base + index, val);
            },
            IndexedZeroPage(reg) => {
                let base = self.next_pc_byte() as u16;
                let index = self.get_register(reg) as u16;
                self.store_byte(base + index, val);
            },
            IndexedIndirect(reg) => {
                let base = self.next_pc_byte();
                let index = self.get_register(reg);
                let addr = self.load_word_zero_page(base + index);
                self.store_byte(addr, val);
            },
            IndirectIndexed(reg) => {
                let zp_offset = self.next_pc_byte();
                let base = self.load_word_zero_page(zp_offset);
                let index = self.get_register(reg) as u16;
                self.store_byte(base + index, val);
            },
            Register(reg) => self.set_register(reg, val),
            _ => panic!("Invalid address mode for store: {:?}", am),
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

    // Instructions helpers
    fn ld_reg(&mut self, am: AddressMode, r: Register8) {
        let val = self.load(am);
        self.set_zero_negative(val);
        self.set_register(r, val);
    }

    fn st_reg(&mut self, am: AddressMode, r: Register8) {
        let val = self.get_register(r);
        self.store(am, val);
    }

    // Instructions
    fn lda(&mut self, am: AddressMode) {
        self.ld_reg(am, Register8::A);
    }

    fn ldx(&mut self, am: AddressMode) {
        self.ld_reg(am, Register8::X);
    }

    fn ldy(&mut self, am: AddressMode) {
        self.ld_reg(am, Register8::Y);
    }

    fn sta(&mut self, am: AddressMode) {
        self.st_reg(am, Register8::A);
    }

    fn stx(&mut self, am: AddressMode) {
        self.st_reg(am, Register8::X);
    }

    fn sty(&mut self, am: AddressMode) {
        self.st_reg(am, Register8::Y);
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

    fn set_register(&mut self, r: Register8, val: u8) {
        use self::Register8::*;
        match r {
            A      => self.regs.a = val,
            X      => self.regs.x = val,
            Y      => self.regs.y = val,
            Sp     => self.regs.sp = val,
            Status => self.regs.status = StatusFlags::from_bits(val).unwrap(),
        }
    }
}
