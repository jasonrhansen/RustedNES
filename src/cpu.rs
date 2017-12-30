use std::fmt;
use std::fmt::{Debug, Display, Formatter};

use disassembler::Disassembler;
use memory::Memory;

bitflags! {
    struct StatusFlags: u8 {
        const NONE              = 0;
        const CARRY             = 1 << 0;
        const ZERO_RESULT       = 1 << 1;
        const INTERRUPT_DISABLE = 1 << 2;
        const DECIMAL_MODE      = 1 << 3;
        const BREAK_COMMAND     = 1 << 4;
        const EXPANSION         = 1 << 5;
        const OVERFLOW          = 1 << 6;
        const NEGATIVE_RESULT   = 1 << 7;
    }
}



impl fmt::Display for StatusFlags {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "Flags {{C: {}, Z: {}, I: {}, D: {}, B: {}, E: {}, O: {}, N: {}}}",
               self.contains(StatusFlags::CARRY) as u8,
               self.contains(StatusFlags::ZERO_RESULT) as u8,
               self.contains(StatusFlags::INTERRUPT_DISABLE) as u8,
               self.contains(StatusFlags::DECIMAL_MODE) as u8,
               self.contains(StatusFlags::BREAK_COMMAND) as u8,
               self.contains(StatusFlags::EXPANSION) as u8,
               self.contains(StatusFlags::OVERFLOW) as u8,
               self.contains(StatusFlags::NEGATIVE_RESULT) as u8,
        )
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum Interrupt {
    None,
    Nmi,
    Irq,
}

const NMI_VECTOR: u16 = 0xFFFA;
const IRQ_VECTOR: u16 = 0xFFFE;
const RESET_VECTOR: u16 = 0xFFFC;
const BRK_VECTOR: u16 = 0xFFFE;

// The number of cycles that each opcode takes.
// This doesn't include additional cycles for page crossing.
static OPCODE_CYCLES: &'static [u8] = &[
    7, 6, 2, 8, 3, 3, 5, 5, 3, 2, 2, 2, 4, 4, 6, 6,
    2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    6, 6, 2, 8, 3, 3, 5, 5, 4, 2, 2, 2, 4, 4, 6, 6,
    2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    6, 6, 2, 8, 3, 3, 5, 5, 3, 2, 2, 2, 3, 4, 6, 6,
    2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    6, 6, 2, 8, 3, 3, 5, 5, 4, 2, 2, 2, 5, 4, 6, 6,
    2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4,
    2, 6, 2, 6, 4, 4, 4, 4, 2, 5, 2, 5, 5, 5, 5, 5,
    2, 6, 2, 6, 3, 3, 3, 3, 2, 2, 2, 2, 4, 4, 4, 4,
    2, 5, 2, 5, 4, 4, 4, 4, 2, 4, 2, 4, 4, 4, 4, 4,
    2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6,
    2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
    2, 6, 2, 8, 3, 3, 5, 5, 2, 2, 2, 2, 4, 4, 6, 6,
    2, 5, 2, 8, 4, 4, 6, 6, 2, 4, 2, 7, 4, 4, 7, 7,
];

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
            sp: 0,
            status: StatusFlags::NONE,
        }
    }
}

impl Debug for Regs {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "pc: {:04X}, a: {:02X}, x: {:02X}, y: {:02X}, sp: {:02X}, status: {}",
               self.pc, self.a, self.x, self.y, self.sp, self.status)
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum Register8 {
    A,
    X,
    Y,
    Sp,
    Status,
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum AddressMode {
    Immediate,
    Absolute,
    ZeroPage,
    AbsoluteIndexed(Register8),
    ZeroPageIndexed(Register8),
    IndexedIndirect(Register8),
    IndirectIndexed(Register8),
    Register(Register8),
}

fn mem_pages_same(m1: u16, m2: u16) -> bool {
    (m1 & 0xFF00) == (m2 & 0xFF00)
}

pub struct Cpu<M: Memory> {
    cycles: u64,
    regs: Regs,
    mem: M,
    interrupt: Interrupt
}

impl<M: Memory> Memory for Cpu<M> {
    fn read_byte(&self, address: u16) -> u8 {
        self.mem.read_byte(address)
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        self.mem.write_byte(address, value)
    }
}

impl<M: Memory> Cpu<M> {
    pub fn new(memory: M) -> Cpu<M> {
        let mut cpu = Cpu {
            cycles: 0,
            regs: Regs::new(),
            mem: memory,
            interrupt: Interrupt::None,
        };

        cpu.reset();

        cpu
    }

    pub fn reset(&mut self) {
        self.regs.pc = self.read_word(RESET_VECTOR);
        self.regs.sp = 0xFD;
        self.regs.status = StatusFlags::INTERRUPT_DISABLE | StatusFlags::EXPANSION;
        self.interrupt = Interrupt::None;
    }

    pub fn step_debug(&mut self) {
        {
            let pc = self.regs.pc;
            let mut d = Disassembler::new(pc, &mut self.mem);
            println!("{:?}", self.regs);
            println!("{}", d.disassemble_next());
        }

        self.step();
        let cycles = self.step();
        println!("cycles: {}", cycles);
    }

    pub fn step(&mut self) -> u8 {
        let cycles = self.cycles;

        self.handle_interrupts();

        let opcode = self.next_pc_byte();
        handle_opcode!(opcode, self);

        self.cycles += OPCODE_CYCLES[opcode as usize] as u64;

        (self.cycles - cycles) as u8
    }

    fn next_pc_byte(&mut self) -> u8 {
        let b = self.read_byte(self.regs.pc);
        self.regs.pc += 1;
        b
    }

    fn next_pc_word(&mut self) -> u16 {
        let w = self.read_word(self.regs.pc);
        self.regs.pc += 2;
        w
    }

    fn load_word_zero_page(&self, offset: u8) -> u16 {
        if offset == 0xFF {
            self.read_byte(0xFF) as u16 +
                ((self.read_byte(0x00) as u16) << 8)
        } else {
            self.read_word(offset as u16)
        }
    }

    fn load(&mut self, am: AddressMode) -> u8 {
        use self::AddressMode::*;
        match am {
            Immediate => self.next_pc_byte(),
            Absolute => {
                let addr = self.next_pc_word();
                self.read_byte(addr)
            },
            ZeroPage => {
                let addr = self.next_pc_byte() as u16;
                self.read_byte(addr)
            },
            AbsoluteIndexed(reg) => {
                let base = self.next_pc_word();
                let index = self.get_register(reg) as u16;
                let addr = base + index;

                // Crossing page boundaries adds an extra cycle
                if !mem_pages_same(base, addr) {
                    self.cycles += 1;
                }

                self.read_byte(addr)
            },
            ZeroPageIndexed(reg) => {
                let base = self.next_pc_byte() as u16;
                let index = self.get_register(reg) as u16;
                self.read_byte(base + index)
            },
            IndexedIndirect(reg) => {
                let base = self.next_pc_byte();
                let index = self.get_register(reg);
                let addr = self.load_word_zero_page(base + index);
                self.read_byte(addr)
            },
            IndirectIndexed(reg) => {
                let zp_offset = self.next_pc_byte();
                let base = self.load_word_zero_page(zp_offset);
                let index = self.get_register(reg) as u16;
                let addr = base + index;

                // Crossing page boundaries adds an extra cycle
                if !mem_pages_same(base, addr) {
                    self.cycles += 1;
                }

                self.read_byte(addr)
            },
            Register(reg) => self.get_register(reg),
        }
    }

    fn store(&mut self, am: AddressMode, val: u8) {
        use self::AddressMode::*;
        match am {
            Absolute => {
                let addr = self.next_pc_word();
                self.write_byte(addr, val);
            },
            ZeroPage => {
                let addr = self.next_pc_byte() as u16;
                self.write_byte(addr, val);
            },
            AbsoluteIndexed(reg) => {
                let base = self.next_pc_word();
                let index = self.get_register(reg) as u16;
                self.write_byte(base + index, val);
            },
            ZeroPageIndexed(reg) => {
                let base = self.next_pc_byte() as u16;
                let index = self.get_register(reg) as u16;
                self.write_byte(base + index, val);
            },
            IndexedIndirect(reg) => {
                let base = self.next_pc_byte();
                let index = self.get_register(reg);
                let addr = self.load_word_zero_page(base + index);
                self.write_byte(addr, val);
            },
            IndirectIndexed(reg) => {
                let zp_offset = self.next_pc_byte();
                let base = self.load_word_zero_page(zp_offset);
                let index = self.get_register(reg) as u16;
                self.write_byte(base + index, val);
            },
            Register(reg) => self.set_register(reg, val),
            _ => panic!("Invalid address mode for store: {:?}", am),
        }
    }

    ///////////////////////
    // Flag helpers
    ///////////////////////

    fn get_flag(&self, sf: StatusFlags) -> bool {
        self.regs.status.contains(sf)
    }

    fn set_flags(&mut self, sf: StatusFlags, value: bool) {
        self.regs.status.set(sf, value);
    }

    fn set_zero_negative(&mut self, result: u8) {
        self.set_flags(StatusFlags::ZERO_RESULT, result == 0);
        self.set_flags(StatusFlags::NEGATIVE_RESULT, result & 0x80 != 0);
    }

    ///////////////////////
    // Register helpers
    ///////////////////////

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

    //////////////////////
    // Instruction helpers
    //////////////////////

    fn ld_reg(&mut self, am: AddressMode, r: Register8) {
        let m = self.load(am);
        self.set_zero_negative(m);
        self.set_register(r, m);
    }

    fn st_reg(&mut self, am: AddressMode, r: Register8) {
        let val = self.get_register(r);
        self.store(am, val);
    }

    fn branch(&mut self, cond: bool) {
        let offset = self.next_pc_byte() as i8;
        if cond {
            let addr = (self.regs.pc as i16 + offset as i16) as u16;

            // Add cycle for taking branch
            self.cycles += 1;

            // Add another cycle if the branching to a new page
            if !mem_pages_same(self.regs.pc, addr) {
                self.cycles += 1;
            }

            self.regs.pc = addr;
        }
    }

    fn compare(&mut self, am: AddressMode, reg: Register8) {
        let m = self.load(am);
        let r = self.get_register(reg);
        let result = r - m;

        self.set_zero_negative(result);
        self.set_flags(StatusFlags::CARRY, m <= r);
    }

    // Push byte onto the stack
    fn push_byte(&mut self, val: u8) {
        let s = self.regs.sp;
        self.write_byte(0x0100 | (s as u16), val);
        self.regs.sp = s - 1;
    }

    // Pull byte from the stack
    fn pull_byte(&mut self) -> u8 {
        let s = self.regs.sp + 1;
        self.regs.sp = s;

        self.read_byte(0x0100 | (s as u16))
    }

    // Push word onto the stack
    fn push_word(&mut self, val: u16) {
        self.push_byte((val >> 8) as u8);
        self.push_byte(val as u8);
    }

    // Pull word from the stack
    fn pull_word(&mut self) -> u16 {
        let lsb= self.pull_byte();
        let msb= self.pull_byte();

        ((msb as u16) << 8) | (lsb as u16)
    }

    ///////////////////
    // Instructions
    ///////////////////

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
        let m = self.load(am);
        let a = self.regs.a;
        let result = a as u32 + m as u32 +
            if self.get_flag(StatusFlags::CARRY) { 1 } else { 0 };

        self.set_flags(StatusFlags::CARRY, result & 0x100 != 0);
        let result = result as u8;
        self.set_flags(StatusFlags::OVERFLOW,
                       ((a & 0x80) == (m & 0x80)) && (a & 0x80 != result & 0x80));
        self.set_zero_negative(result);

        self.regs.a = result;
    }

    fn sbc(&mut self, am: AddressMode) {
        let m = self.load(am);
        let a = self.regs.a;
        let result = a as u32 - m as u32 -
            if self.get_flag(StatusFlags::CARRY) { 0 } else { 1 };

        self.set_flags(StatusFlags::CARRY, result & 0x100 == 0);
        let result = result as u8;
        self.set_flags(StatusFlags::OVERFLOW,
                       !(((a & 0x80) != (m & 0x80)) && (a & 0x80 != result & 0x80)));
        self.set_zero_negative(result);

        self.regs.a = result;
    }

    fn and(&mut self, am: AddressMode) {
        let m = self.load(am);
        let a = self.regs.a;
        let result = m & a;
        self.set_zero_negative(result);
        self.regs.a = result;
    }

    fn ora(&mut self, am: AddressMode) {
        let m = self.load(am);
        let a = self.regs.a;
        let result = m | a;
        self.set_zero_negative(result);
        self.regs.a = result;
    }

    fn eor(&mut self, am: AddressMode) {
        let m = self.load(am);
        let a = self.regs.a;
        let result = m ^ a;
        self.set_zero_negative(result);
        self.regs.a = result;
    }

    fn sec(&mut self) {
        self.set_flags(StatusFlags::CARRY, true);
    }

    fn clc(&mut self) {
        self.set_flags(StatusFlags::CARRY, false);
    }

    fn sei(&mut self) {
        self.set_flags(StatusFlags::INTERRUPT_DISABLE, true);
    }

    fn cli(&mut self) {
        self.set_flags(StatusFlags::INTERRUPT_DISABLE, false);
    }

    fn sed(&mut self) {
        self.set_flags(StatusFlags::DECIMAL_MODE, true);
    }

    fn cld(&mut self) {
        self.set_flags(StatusFlags::DECIMAL_MODE, false);
    }

    fn clv(&mut self) {
        self.set_flags(StatusFlags::OVERFLOW, false);
    }

    fn jmp(&mut self) {
        self.regs.pc = self.next_pc_word();
    }

    fn jmpi(&mut self) {
        let addr = self.next_pc_word();

        let lsb = self.read_byte(addr);

        // There is a hardware bug in this instruction. If the 16-bit argument of an indirect JMP is
        // located between 2 pages (0x01FF and 0x0200 for example), then the LSB will be read from
        // 0x01FF and the MSB will be read from 0x0100.
        let msb = self.read_byte(
            if (addr & 0xFF) == 0xFF {
                addr & 0xff00
            } else {
                addr + 1
            }
        );

        self.regs.pc = ((msb as u16) << 8) | (lsb as u16);
    }

    fn bmi(&mut self) {
        let cond = self.get_flag(StatusFlags::NEGATIVE_RESULT);
        self.branch(cond);
    }

    fn bpl(&mut self) {
        let cond = !self.get_flag(StatusFlags::NEGATIVE_RESULT);
        self.branch(cond);
    }

    fn bcc(&mut self) {
        let cond = !self.get_flag(StatusFlags::CARRY);
        self.branch(cond);
    }

    fn bcs(&mut self) {
        let cond = self.get_flag(StatusFlags::CARRY);
        self.branch(cond);
    }

    fn beq(&mut self) {
        let cond = self.get_flag(StatusFlags::ZERO_RESULT);
        self.branch(cond);
    }

    fn bne(&mut self) {
        let cond = !self.get_flag(StatusFlags::ZERO_RESULT);
        self.branch(cond);
    }

    fn bvs(&mut self) {
        let cond = self.get_flag(StatusFlags::OVERFLOW);
        self.branch(cond);
    }

    fn bvc(&mut self) {
        let cond = !self.get_flag(StatusFlags::OVERFLOW);
        self.branch(cond);
    }

    fn cmp(&mut self, am: AddressMode) {
        self.compare(am, Register8::A)
    }

    fn cpx(&mut self, am: AddressMode) {
        self.compare(am, Register8::X)
    }

    fn cpy(&mut self, am: AddressMode) {
        self.compare(am, Register8::Y)
    }

    fn bit(&mut self, am: AddressMode) {
        let m = self.load(am);
        let a = self.regs.a;

        self.set_flags(StatusFlags::NEGATIVE_RESULT, m & 0x80 != 0);
        self.set_flags(StatusFlags::OVERFLOW, m & 0x40 != 0);
        self.set_flags(StatusFlags::ZERO_RESULT, (m & a) == 0);
    }

    fn inc(&mut self, am: AddressMode) {
        let val = self.load(am) + 1;
        self.set_zero_negative(val);
        self.store(am, val);
    }

    fn dec(&mut self, am: AddressMode) {
        let val = self.load(am) - 1;
        self.set_zero_negative(val);
        self.store(am, val);
    }

    fn inx(&mut self) {
        let val = self.regs.x + 1;
        self.set_zero_negative(val);
        self.regs.x = val;
    }

    fn iny(&mut self) {
        let val = self.regs.y + 1;
        self.set_zero_negative(val);
        self.regs.y = val;
    }

    fn dex(&mut self) {
        let val = self.regs.x - 1;
        self.set_zero_negative(val);
        self.regs.x = val;
    }

    fn dey(&mut self) {
        let val = self.regs.y - 1;
        self.set_zero_negative(val);
        self.regs.y = val;
    }

    fn tax(&mut self) {
        let a = self.regs.a;
        self.set_zero_negative(a);
        self.regs.x = a;
    }

    fn txa(&mut self) {
        let x = self.regs.x;
        self.set_zero_negative(x);
        self.regs.a = x;
    }

    fn tay(&mut self) {
        let a = self.regs.a;
        self.set_zero_negative(a);
        self.regs.y = a;
    }

    fn tya(&mut self) {
        let y = self.regs.y;
        self.set_zero_negative(y);
        self.regs.a = y;
    }

    fn tsx(&mut self) {
        let s = self.regs.sp;
        self.set_zero_negative(s);
        self.regs.x = s;
    }

    fn txs(&mut self) {
        self.regs.sp = self.regs.x;
    }

    fn jsr(&mut self) {
        let pc = self.regs.pc;
        self.push_word(pc);
        let addr = self.next_pc_word();
        self.regs.pc = addr;
    }

    fn rts(&mut self) {
        self.regs.pc = self.pull_word();
    }

    fn pha(&mut self) {
        let a = self.regs.a;
        self.push_byte(a);
    }

    fn pla(&mut self) {
        let val = self.pull_byte();
        self.set_zero_negative(val);
        self.regs.a = val;
    }

    fn php(&mut self) {
        let p = self.regs.status.bits();
        self.push_byte(p);
    }

    fn plp(&mut self) {
        let val = self.pull_byte();
        self.regs.status = StatusFlags::from_bits(val).unwrap();
    }

    fn lsr(&mut self, am: AddressMode) {
        let val = self.load(am);
        let result = (val >> 1) & 0x7F;
        self.set_zero_negative(result);
        self.set_flags(StatusFlags::CARRY, (val & 0x01) != 0);
        self.store(am, result);
    }

    fn asl(&mut self, am: AddressMode) {
        let val = self.load(am);
        let result = (val << 1) & 0xFE;
        self.set_zero_negative(result);
        self.set_flags(StatusFlags::CARRY, (val & 0x80) != 0);
        self.store(am, result);
    }

    fn ror(&mut self, am: AddressMode) {
        let val = self.load(am);
        let carry: u8 = if self.get_flag(StatusFlags::CARRY) { 1 << 7 } else { 0 };
        let result = ((val >> 1) & 0x7F) | carry;
        self.set_zero_negative(result);
        self.set_flags(StatusFlags::CARRY, (val & 0x01) != 0);
        self.store(am, result);
    }

    fn rol(&mut self, am: AddressMode) {
        let val = self.load(am);
        let carry: u8 = if self.get_flag(StatusFlags::CARRY) { 1 } else { 0 };
        let result = ((val << 1) & 0xFE) | carry;
        self.set_zero_negative(result);
        self.set_flags(StatusFlags::CARRY, (val & 0x80) != 0);
        self.store(am, result);
    }

    fn brk(&mut self) {
        let pc = self.regs.pc;
        let status = self.regs.status.bits();
        self.push_word(pc);
        self.push_byte(status);
        self.set_flags(StatusFlags::INTERRUPT_DISABLE, true);
        self.regs.pc = self.read_word(BRK_VECTOR);
    }

    fn rti(&mut self) {
        let status = self.pull_byte();
        let pc = self.pull_word();

        self.regs.status = StatusFlags::from_bits(status).unwrap();
        self.regs.pc = pc;
    }

    fn nop(&mut self) {}

    ///////////////////////////
    // Unofficial Instructions
    ///////////////////////////

    fn nop_2_bytes(&mut self) {
        let pc = self.regs.pc;
        self.regs.pc = pc + 1;
    }

    // Used by "Gaau Hok Gwong Cheung (Ch)"
    // This instruction can be unpredictable.
    // See http://visual6502.org/wiki/index.php?title=6502_Opcode_8B_%28XAA,_ANE%29
    fn xaa(&mut self) {
        let imm = self.next_pc_byte();
        let a = self.regs.a;
        let x = self.regs.x;
        self.regs.a = a & x & imm;
    }

    // Used by "Super Cars (U)"
    fn lax(&mut self, am: AddressMode) {
        self.lda(am);
        self.tax();
    }

    // Used by "Disney's Aladdin (E)"
    fn slo(&mut self, am: AddressMode) {
        self.asl(am);
        self.ora(am);
    }

    ///////////////
    // Interrupts
    ///////////////

    // Request NMI (Non-Maskable Interrupt) to be run on next step
    pub fn request_nmi(&mut self) {
        self.interrupt = Interrupt::Nmi;
    }

    // Request IRQ to be run on next step
    pub fn request_irq(&mut self) {
        if !self.get_flag(StatusFlags::INTERRUPT_DISABLE) {
            self.interrupt = Interrupt::Irq;
        }
    }

    fn handle_interrupts(&mut self) {
        match self.interrupt {
            Interrupt::Nmi => self.handle_interrupt(NMI_VECTOR),
            Interrupt::Irq => self.handle_interrupt(IRQ_VECTOR),
            Interrupt::None => (),
        }
    }

    fn handle_interrupt(&mut self, vector: u16) {
        let pc = self.regs.pc;
        let status = self.regs.status.bits();
        self.push_word(pc);
        self.push_byte(status);
        self.set_flags(StatusFlags::INTERRUPT_DISABLE, true);
        self.regs.pc = self.read_word(vector);
        self.interrupt = Interrupt::None;
        self.cycles += 7;
    }
}
