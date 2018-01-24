use std::collections::HashSet;
use std::fmt;
use std::fmt::{Debug, Formatter};

use memory::Memory;

pub const CPU_FREQUENCY: u64 = 1_789_773;

bitflags! {
    pub struct StatusFlags: u8 {
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
        write!(f, "{{C: {}, Z: {}, I: {}, D: {}, B: {}, E: {}, V: {}, N: {}}}",
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
pub enum Interrupt {
    Nmi,        // NMI (Non-Maskable Interrupt)
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

#[derive(Copy, Clone)]
pub struct Regs {
    pub pc: u16,
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub sp: u8,
    pub status: StatusFlags,
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

pub struct Cpu {
    pub cycles: u64,
    regs: Regs,
    interrupt: Option<Interrupt>,

    pub watchpoints: HashSet<u16>,
    trigger_watchpoint: bool,
}

impl Cpu {
    pub fn new() -> Cpu {
        let cpu = Cpu {
            cycles: 0,
            regs: Regs::new(),
            interrupt: None,
            watchpoints: HashSet::new(),
            trigger_watchpoint: false,
        };

        cpu
    }

    pub fn regs(&self) -> Regs {
        self.regs
    }

    pub fn reset<M: Memory>(&mut self, mem: &mut M) {
        self.regs.pc = mem.read_word(RESET_VECTOR);
        self.regs.sp = 0xFD;
        self.regs.status = StatusFlags::INTERRUPT_DISABLE | StatusFlags::EXPANSION;
        self.interrupt = None;
    }

    pub fn step<M: Memory>(&mut self, mem: &mut M) -> (u32, bool) {
        self.trigger_watchpoint = false;
        let cycles = self.cycles;

        self.handle_interrupts(mem);

        let opcode = self.next_pc_byte(mem);
        handle_opcode!(opcode, self, mem);
        self.cycles += OPCODE_CYCLES[opcode as usize] as u64;

        let cycles = (self.cycles - cycles) as u32;

        (cycles, self.trigger_watchpoint)
    }

    fn check_watchpoints(&self, addr: u16) -> bool {
        self.watchpoints.len() != 0 && self.watchpoints.contains(&addr)
    }

    fn next_pc_byte<M: Memory>(&mut self, mem: &mut M) -> u8 {
        let pc = self.regs.pc;
        let b = mem.read_byte(pc);
        self.regs.pc += 1;
        b
    }

    fn next_pc_word<M: Memory>(&mut self, mem: &mut M) -> u16 {
        let pc = self.regs.pc;
        let w = mem.read_word(pc);
        self.regs.pc += 2;
        w
    }

    fn load_word_zero_page<M: Memory>(&mut self, mem: &mut M, offset: u8) -> u16 {
        if offset == 0xFF {
            mem.read_byte(0xFF) as u16 +
                ((mem.read_byte(0x00) as u16) << 8)
        } else {
            mem.read_word(offset as u16)
        }
    }

    fn load<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> (u8, Option<u16>) {
        use self::AddressMode::*;
        match am {
            Immediate => (self.next_pc_byte(mem), None),
            Absolute => {
                let addr = self.next_pc_word(mem);
                self.trigger_watchpoint |= self.check_watchpoints(addr);
                (mem.read_byte(addr), Some(addr))
            },
            ZeroPage => {
                let addr = self.next_pc_byte(mem) as u16;
                self.trigger_watchpoint |= self.check_watchpoints(addr);
                (mem.read_byte(addr), Some(addr))
            },
            AbsoluteIndexed(reg) => {
                let base = self.next_pc_word(mem);
                let index = self.get_register(reg) as u16;
                let addr = base + index;
                self.trigger_watchpoint |= self.check_watchpoints(addr);

                // Crossing page boundaries adds an extra cycle
                if !mem_pages_same(base, addr) {
                    self.cycles += 1;
                }

                (mem.read_byte(addr), Some(addr))
            },
            ZeroPageIndexed(reg) => {
                let base = self.next_pc_byte(mem) as u16;
                let index = self.get_register(reg) as u16;
                let addr = (base + index) % 0x0100;
                self.trigger_watchpoint |= self.check_watchpoints(addr);

                (mem.read_byte(addr), Some(addr))
            },
            IndexedIndirect(reg) => {
                let base = self.next_pc_byte(mem);
                let index = self.get_register(reg);
                let addr = self.load_word_zero_page(mem,base + index);
                self.trigger_watchpoint |= self.check_watchpoints(addr);

                (mem.read_byte(addr), Some(addr))
            },
            IndirectIndexed(reg) => {
                let zp_offset = self.next_pc_byte(mem);
                let base = self.load_word_zero_page(mem,zp_offset);
                let index = self.get_register(reg) as u16;
                let addr = base + index;
                self.trigger_watchpoint |= self.check_watchpoints(addr);

                // Crossing page boundaries adds an extra cycle
                if !mem_pages_same(base, addr) {
                    self.cycles += 1;
                }

                (mem.read_byte(addr), Some(addr))
            },
            Register(reg) => (self.get_register(reg), None),
        }
    }

    fn store<M: Memory>(&mut self, mem: &mut M, am: AddressMode, val: u8) {
        use self::AddressMode::*;
        match am {
            Absolute => {
                let addr = self.next_pc_word(mem);
                mem.write_byte(addr, val);
            },
            ZeroPage => {
                let addr = self.next_pc_byte(mem) as u16;
                mem.write_byte(addr, val);
            },
            AbsoluteIndexed(reg) => {
                let base = self.next_pc_word(mem);
                let index = self.get_register(reg) as u16;
                mem.write_byte(base + index, val);
            },
            ZeroPageIndexed(reg) => {
                let base = self.next_pc_byte(mem) as u16;
                let index = self.get_register(reg) as u16;
                let addr = (base + index) % 0x0100;
                mem.write_byte(addr, val);
            },
            IndexedIndirect(reg) => {
                let base = self.next_pc_byte(mem);
                let index = self.get_register(reg);
                let addr = self.load_word_zero_page(mem, base + index);
                mem.write_byte(addr, val);
            },
            IndirectIndexed(reg) => {
                let zp_offset = self.next_pc_byte(mem);
                let base = self.load_word_zero_page(mem, zp_offset);
                let index = self.get_register(reg) as u16;
                mem.write_byte(base + index, val);
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

    fn ld_reg<M: Memory>(&mut self, mem: &mut M, am: AddressMode, r: Register8) {
        let (m, _) = self.load(mem, am);
        self.set_zero_negative(m);
        self.set_register(r, m);
    }

    fn st_reg<M: Memory>(&mut self, mem: &mut M, am: AddressMode, r: Register8) {
        let val = self.get_register(r);
        self.store(mem, am, val);
    }

    fn branch<M: Memory>(&mut self, mem: &mut M, cond: bool) {
        let offset = self.next_pc_byte(mem) as i8;
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

    fn compare<M: Memory>(&mut self, mem: &mut M, am: AddressMode, reg: Register8) {
        let (m, _) = self.load(mem, am);
        let r = self.get_register(reg);
        let result = r.wrapping_sub(m);

        self.set_zero_negative(result);
        self.set_flags(StatusFlags::CARRY, m <= r);
    }

    // Push byte onto the stack
    fn push_byte<M: Memory>(&mut self, mem: &mut M, val: u8) {
        let s = self.regs.sp;
        mem.write_byte(0x0100 | (s as u16), val);

        self.regs.sp = s - 1;
    }

    // Pull byte from the stack
    fn pull_byte<M: Memory>(&mut self, mem: &mut M) -> u8 {
        let s = self.regs.sp + 1;
        self.regs.sp = s;

        mem.read_byte(0x0100 | (s as u16))
    }

    // Push word onto the stack
    fn push_word<M: Memory>(&mut self, mem: &mut M, val: u16) {
        self.push_byte(mem,(val >> 8) as u8);
        self.push_byte(mem,val as u8);
    }

    // Pull word from the stack
    fn pull_word<M: Memory>(&mut self, mem: &mut M) -> u16 {
        let lsb= self.pull_byte(mem);
        let msb= self.pull_byte(mem);

        ((msb as u16) << 8) | (lsb as u16)
    }

    ///////////////////
    // Instructions
    ///////////////////

    fn lda<M: Memory>(&mut self, mem: &mut M, am: AddressMode) {
        self.ld_reg(mem, am, Register8::A);
    }

    fn ldx<M: Memory>(&mut self, mem: &mut M, am: AddressMode) {
        self.ld_reg(mem, am, Register8::X);
    }

    fn ldy<M: Memory>(&mut self, mem: &mut M, am: AddressMode) {
        self.ld_reg(mem, am, Register8::Y);
    }

    fn sta<M: Memory>(&mut self, mem: &mut M, am: AddressMode) {
        self.st_reg(mem, am, Register8::A);
    }

    fn stx<M: Memory>(&mut self, mem: &mut M, am: AddressMode) {
        self.st_reg(mem, am, Register8::X);
    }

    fn sty<M: Memory>(&mut self, mem: &mut M, am: AddressMode) {
        self.st_reg(mem, am, Register8::Y);
    }

    fn adc<M: Memory>(&mut self, mem: &mut M, am: AddressMode) {
        let (m, _) = self.load(mem, am);
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

    fn sbc<M: Memory>(&mut self, mem: &mut M, am: AddressMode) {
        let (m, _) = self.load(mem, am);
        let a = self.regs.a;
        let result = a as i32 - m as i32 -
            if self.get_flag(StatusFlags::CARRY) { 0 } else { 1 };

        self.set_flags(StatusFlags::CARRY, result >= 0);

        let result = result as u8;
        self.set_flags(StatusFlags::OVERFLOW, (a ^ m) & 0x80 != 0 && (a ^ result) & 0x80 != 0);
        self.set_zero_negative(result);

        self.regs.a = result;
    }

    fn and<M: Memory>(&mut self, mem: &mut M, am: AddressMode) {
        let (m, _) = self.load(mem, am);
        let a = self.regs.a;
        let result = m & a;
        self.set_zero_negative(result);
        self.regs.a = result;
    }

    fn ora<M: Memory>(&mut self, mem: &mut M, am: AddressMode) {
        let (m, _) = self.load(mem, am);
        let a = self.regs.a;
        let result = m | a;
        self.set_zero_negative(result);
        self.regs.a = result;
    }

    fn eor<M: Memory>(&mut self, mem: &mut M, am: AddressMode) {
        let (m, _) = self.load(mem, am);
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

    fn jmp<M: Memory>(&mut self, mem: &mut M) {
        self.regs.pc = self.next_pc_word(mem);
    }

    fn jmpi<M: Memory>(&mut self, mem: &mut M) {
        let addr = self.next_pc_word(mem);

        let lsb = mem.read_byte(addr);

        // There is a hardware bug in this instruction. If the 16-bit argument of an indirect JMP is
        // located between 2 pages (0x01FF and 0x0200 for example), then the LSB will be read from
        // 0x01FF and the MSB will be read from 0x0100.
        let msb = mem.read_byte(
            if (addr & 0xFF) == 0xFF {
                addr & 0xff00
            } else {
                addr + 1
            }
        );

        self.regs.pc = ((msb as u16) << 8) | (lsb as u16);
    }

    fn bmi<M: Memory>(&mut self, mem: &mut M) {
        let cond = self.get_flag(StatusFlags::NEGATIVE_RESULT);
        self.branch(mem, cond);
    }

    fn bpl<M: Memory>(&mut self, mem: &mut M) {
        let cond = !self.get_flag(StatusFlags::NEGATIVE_RESULT);
        self.branch(mem, cond);
    }

    fn bcc<M: Memory>(&mut self, mem: &mut M) {
        let cond = !self.get_flag(StatusFlags::CARRY);
        self.branch(mem, cond);
    }

    fn bcs<M: Memory>(&mut self, mem: &mut M) {
        let cond = self.get_flag(StatusFlags::CARRY);
        self.branch(mem, cond);
    }

    fn beq<M: Memory>(&mut self, mem: &mut M) {
        let cond = self.get_flag(StatusFlags::ZERO_RESULT);
        self.branch(mem, cond);
    }

    fn bne<M: Memory>(&mut self, mem: &mut M) {
        let cond = !self.get_flag(StatusFlags::ZERO_RESULT);
        self.branch(mem, cond);
    }

    fn bvs<M: Memory>(&mut self, mem: &mut M) {
        let cond = self.get_flag(StatusFlags::OVERFLOW);
        self.branch(mem, cond);
    }

    fn bvc<M: Memory>(&mut self, mem: &mut M) {
        let cond = !self.get_flag(StatusFlags::OVERFLOW);
        self.branch(mem, cond);
    }

    fn cmp<M: Memory>(&mut self, mem: &mut M, am: AddressMode) {
        self.compare(mem, am, Register8::A)
    }

    fn cpx<M: Memory>(&mut self, mem: &mut M, am: AddressMode) {
        self.compare(mem, am, Register8::X)
    }

    fn cpy<M: Memory>(&mut self, mem: &mut M, am: AddressMode) {
        self.compare(mem, am, Register8::Y)
    }

    fn bit<M: Memory>(&mut self, mem: &mut M, am: AddressMode) {
        let (m, _) = self.load(mem, am);
        let a = self.regs.a;

        self.set_flags(StatusFlags::NEGATIVE_RESULT, m & 0x80 != 0);
        self.set_flags(StatusFlags::OVERFLOW, m & 0x40 != 0);
        self.set_flags(StatusFlags::ZERO_RESULT, (m & a) == 0);
    }

    fn inc<M: Memory>(&mut self, mem: &mut M, am: AddressMode) {
        if let (val, Some(addr)) = self.load(mem, am) {
            let result = val.wrapping_add(1);
            self.set_zero_negative(result);
            mem.write_byte(addr, result);
        }
    }

    fn dec<M: Memory>(&mut self, mem: &mut M, am: AddressMode) {
        if let (val, Some(addr)) = self.load(mem, am) {
            let result = val.wrapping_sub(1);
            self.set_zero_negative(result);
            mem.write_byte(addr, result);
        }
    }

    fn inx(&mut self) {
        let val = self.regs.x.wrapping_add(1);
        self.set_zero_negative(val);
        self.regs.x = val;
    }

    fn iny(&mut self) {
        let val = self.regs.y.wrapping_add(1);
        self.set_zero_negative(val);
        self.regs.y = val;
    }

    fn dex(&mut self) {
        let val = self.regs.x.wrapping_sub(1);
        self.set_zero_negative(val);
        self.regs.x = val;
    }

    fn dey(&mut self) {
        let val = self.regs.y.wrapping_sub(1);
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

    fn jsr<M: Memory>(&mut self, mem: &mut M) {
        let addr = self.next_pc_word(mem);
        let pc = self.regs.pc - 1;
        self.push_word(mem, pc);
        self.regs.pc = addr;
    }

    fn rts<M: Memory>(&mut self, mem: &mut M) {
        self.regs.pc = self.pull_word(mem) + 1;
    }

    fn pha<M: Memory>(&mut self, mem: &mut M) {
        let a = self.regs.a;
        self.push_byte(mem, a);
    }

    fn pla<M: Memory>(&mut self, mem: &mut M) {
        let val = self.pull_byte(mem);
        self.set_zero_negative(val);
        self.regs.a = val;
    }

    fn php<M: Memory>(&mut self, mem: &mut M) {
        let p = self.regs.status | StatusFlags::BREAK_COMMAND | StatusFlags::EXPANSION;
        self.push_byte(mem, p.bits);
    }

    fn plp<M: Memory>(&mut self, mem: &mut M) {
        let val = self.pull_byte(mem);
        self.regs.status = StatusFlags::from_bits(val).unwrap();
    }

    fn lsr<M: Memory>(&mut self, mem: &mut M, am: AddressMode) {
        let (val, addr) = self.load(mem, am);

        let result = (val >> 1) & 0x7F;
        self.set_zero_negative(result);
        self.set_flags(StatusFlags::CARRY, (val & 0x01) != 0);

        if let Some(addr) = addr {
            mem.write_byte(addr, result);
        } else if let AddressMode::Register(reg) = am {
            self.set_register(reg, result);
        }
    }

    fn asl<M: Memory>(&mut self, mem: &mut M, am: AddressMode) {
        let (val, addr) = self.load(mem, am);

        let result = (val << 1) & 0xFE;
        self.set_zero_negative(result);
        self.set_flags(StatusFlags::CARRY, (val & 0x80) != 0);

        if let Some(addr) = addr {
            mem.write_byte(addr, result);
        } else if let AddressMode::Register(reg) = am {
            self.set_register(reg, result);
        }
    }

    fn ror<M: Memory>(&mut self, mem: &mut M, am: AddressMode) {
        let (val, addr) = self.load(mem, am);

        let carry: u8 = if self.get_flag(StatusFlags::CARRY) { 1 << 7 } else { 0 };
        let result = ((val >> 1) & 0x7F) | carry;
        self.set_zero_negative(result);
        self.set_flags(StatusFlags::CARRY, (val & 0x01) != 0);

        if let Some(addr) = addr {
            mem.write_byte(addr, result);
        } else if let AddressMode::Register(reg) = am {
            self.set_register(reg, result);
        }
    }

    fn rol<M: Memory>(&mut self, mem: &mut M, am: AddressMode) {
        let (val, addr) = self.load(mem, am);

        let carry: u8 = if self.get_flag(StatusFlags::CARRY) { 1 } else { 0 };
        let result = ((val << 1) & 0xFE) | carry;
        self.set_zero_negative(result);
        self.set_flags(StatusFlags::CARRY, (val & 0x80) != 0);

        if let Some(addr) = addr {
            mem.write_byte(addr, result);
        } else if let AddressMode::Register(reg) = am {
            self.set_register(reg, result);
        }
    }

    fn brk<M: Memory>(&mut self, mem: &mut M) {
        let pc = self.regs.pc;
        let status = self.regs.status | StatusFlags::BREAK_COMMAND | StatusFlags::EXPANSION;
        self.push_word(mem, pc);
        self.push_byte(mem, status.bits());
        self.set_flags(StatusFlags::INTERRUPT_DISABLE, true);
        self.regs.pc = mem.read_word(BRK_VECTOR);
    }

    fn rti<M: Memory>(&mut self, mem: &mut M) {
        let status = self.pull_byte(mem);
        let pc = self.pull_word(mem);

        self.regs.status = StatusFlags::from_bits(status).unwrap();
        self.regs.pc = pc;
    }

    fn nop(&mut self) {}

    ///////////////////////////
    // Unofficial Instructions
    ///////////////////////////

    fn nop_2_bytes<M: Memory>(&mut self, mem: &mut M) {
        self.next_pc_byte(mem);
    }

    // Used by "Gaau Hok Gwong Cheung (Ch)"
    // This instruction can be unpredictable.
    // See http://visual6502.org/wiki/index.php?title=6502_Opcode_8B_%28XAA,_ANE%29
    fn xaa<M: Memory>(&mut self, mem: &mut M) {
        let imm = self.next_pc_byte(mem);
        let a = self.regs.a;
        let x = self.regs.x;
        self.regs.a = a & x & imm;
    }

    // Used by "Super Cars (U)"
    fn lax<M: Memory>(&mut self, mem: &mut M, am: AddressMode) {
        self.lda(mem, am);
        self.tax();
    }

    // Used by "Disney's Aladdin (E)"
    fn slo<M: Memory>(&mut self, mem: &mut M, am: AddressMode) {
        self.asl(mem, am);
        self.ora(mem,am);
    }

    ///////////////
    // Interrupts
    ///////////////

    // Request interrupt to be run on next step
    pub fn request_interrupt(&mut self, interrupt: Interrupt) {
        match interrupt {
            Interrupt::Nmi => self.interrupt = Some(interrupt),
            Interrupt::Irq => {
                if !self.get_flag(StatusFlags::INTERRUPT_DISABLE) {
                    self.interrupt = Some(interrupt);
                }
            }
        }
    }

    fn handle_interrupts<M: Memory>(&mut self, mem: &mut M) -> bool {
        if let Some(interrupt) = self.interrupt {
            match interrupt {
                Interrupt::Nmi => self.handle_interrupt(mem, NMI_VECTOR),
                Interrupt::Irq => self.handle_interrupt(mem, IRQ_VECTOR),
            }
            true
        } else {
            false
        }
    }

    fn handle_interrupt<M: Memory>(&mut self, mem: &mut M, vector: u16) {
        let pc = self.regs.pc;
        self.push_word(mem, pc);
        self.php(mem);
        self.regs.pc = mem.read_word(vector);
        self.set_flags(StatusFlags::INTERRUPT_DISABLE, true);
        self.interrupt = None;
        self.cycles += 7;
    }
}
