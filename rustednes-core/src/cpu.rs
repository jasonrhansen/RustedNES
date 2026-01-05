use crate::system_bus::SystemBus;

use serde_derive::{Deserialize, Serialize};

use std::collections::HashSet;
use std::fmt;
use std::fmt::{Debug, Formatter};

pub type CycleFn = fn(&mut Cpu, bus: &mut SystemBus) -> bool;

#[derive(Copy, Clone)]
pub struct Instruction {
    name: &'static str,
    cycles: &'static [CycleFn],
}

pub const OAMDATA_ADDRESS: u16 = 0x2004;
pub const OAMDMA_ADDRESS: u16 = 0x4014;
pub const CPU_FREQUENCY: u64 = 1_789_773;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Deserialize, Serialize)]
pub enum Interrupt {
    Nmi, // NMI (Non-Maskable Interrupt)
    Irq,
}

const NMI_VECTOR: u16 = 0xFFFA;
const IRQ_VECTOR: u16 = 0xFFFE;
const RESET_VECTOR: u16 = 0xFFFC;
const BRK_VECTOR: u16 = 0xFFFE;

#[derive(Copy, Clone, Default, Deserialize, Serialize)]
pub struct Flags {
    c: bool, // Carry
    z: bool, // Zero
    i: bool, // Interrupt inhibit
    d: bool, // Decimal (not used on NES)
    b: bool, // Break Command
    e: bool, // Expansion
    v: bool, // Overflow
    n: bool, // Negative
}

impl From<Flags> for u8 {
    fn from(flags: Flags) -> Self {
        (flags.c as u8)
            | ((flags.z as u8) << 1)
            | ((flags.i as u8) << 2)
            | ((flags.d as u8) << 3)
            | ((flags.b as u8) << 4)
            | ((flags.e as u8) << 5)
            | ((flags.v as u8) << 6)
            | ((flags.n as u8) << 7)
    }
}

impl From<u8> for Flags {
    fn from(bits: u8) -> Self {
        Flags {
            c: (bits & 0x01) != 0,
            z: (bits & 0x02) != 0,
            i: (bits & 0x04) != 0,
            d: (bits & 0x08) != 0,
            b: (bits & 0x10) != 0,
            e: (bits & 0x20) != 0,
            v: (bits & 0x40) != 0,
            n: (bits & 0x80) != 0,
        }
    }
}

impl Debug for Flags {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "N: {}, V: {}, e: {}, b: {}, d: {}, I: {}, Z: {}, C: {}",
            self.n as u8,
            self.v as u8,
            self.e as u8,
            self.b as u8,
            self.d as u8,
            self.i as u8,
            self.z as u8,
            self.c as u8
        )
    }
}

#[derive(Copy, Clone, Default, Deserialize, Serialize)]
pub struct Regs {
    pub pc: u16,
    pub a: u8,
    pub x: u8,
    pub y: u8,
    pub sp: u8,
}

impl Debug for Regs {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(
            f,
            "pc: {:04X}, a: {:02X}, x: {:02X}, y: {:02X}, sp: {:02X}",
            self.pc, self.a, self.x, self.y, self.sp
        )
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Register8 {
    A,
    X,
    Y,
    Sp,
    Status,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
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

#[derive(Default)]
pub struct Cpu {
    stall_cycles: u8,
    regs: Regs,
    flags: Flags,
    interrupt: Option<Interrupt>,

    pub watchpoints: HashSet<u16>,
    trigger_watchpoint: bool,

    // Tempory mid-instruction data to be able to run one cycle at a time.
    opcode: u8,
    cycle: u8,
    addr_abs: u16,
    rel_offset: i16,
    fetched_data: u8,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct State {
    pub stall_cycles: u8,
    pub regs: Regs,
    pub flags: Flags,
    pub interrupt: Option<Interrupt>,
}

impl Cpu {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn get_state(&self) -> State {
        State {
            stall_cycles: self.stall_cycles,
            regs: self.regs,
            flags: self.flags,
            interrupt: self.interrupt,
        }
    }

    pub fn apply_state(&mut self, state: &State) {
        self.stall_cycles = state.stall_cycles;
        self.regs = state.regs;
        self.flags = state.flags;
        self.interrupt = state.interrupt;
    }

    pub fn stall(&mut self, cycles: u8) {
        self.stall_cycles += cycles;
    }

    pub fn regs(&self) -> Regs {
        self.regs
    }

    pub fn flags(&self) -> Flags {
        self.flags
    }

    pub fn reset(&mut self, bus: &mut SystemBus) {
        self.regs.pc = bus.read_word(RESET_VECTOR);
        self.regs.sp = 0xFD;
        self.flags = Flags {
            c: false,
            z: false,
            i: true,
            d: false,
            b: false,
            e: true,
            v: false,
            n: false,
        };
        self.interrupt = None;
    }

    /// Runs a single CPU cycle.
    pub fn tick(&mut self, bus: &mut SystemBus) -> (bool, bool) {
        self.trigger_watchpoint = false;

        if self.stall_cycles > 0 {
            self.stall_cycles -= 1;
            return (false, false);
        }

        // TODO: Handle interrupts one cycle at a time.

        if self.cycle == 0 {
            // Fetch new instruction.
            self.opcode = self.next_pc_byte(bus);
            self.cycle = 1;
        } else {
            // Continue in-progress instruction.
            let instr = match &OPCODES[self.opcode as usize] {
                Some(instr) => instr,
                None => panic!("Unsupported opcode: {:X}", self.opcode),
            };

            let cycle_fn = instr.cycles[self.cycle as usize - 1];
            let completed = cycle_fn(self, bus);

            self.cycle += 1;

            if completed || self.cycle as usize > instr.cycles.len() {
                self.cycle = 0;
            }
        }

        (self.cycle == 0, self.trigger_watchpoint)
    }

    pub fn step(&mut self, bus: &mut SystemBus) -> (u32, bool) {
        if self.stall_cycles > 0 {
            self.stall_cycles -= 1;
            return (1, false);
        }

        self.trigger_watchpoint = false;
        let cycles = self.cycles;

        self.handle_interrupts(bus);

        handle_opcode!(self.next_pc_byte(bus), self, bus);

        let cycles = (self.cycles - cycles) as u32;

        (cycles, self.trigger_watchpoint)
    }

    fn check_watchpoints(&self, addr: u16) -> bool {
        !self.watchpoints.is_empty() && self.watchpoints.contains(&addr)
    }

    fn handle_oam_dma(&mut self, bus: &mut SystemBus, addr_hi: u8) {
        self.dummy_read(bus);

        // An extra cycle should be added on an odd CPU cycle
        // http://wiki.nesdev.com/w/index.php/PPU_registers#OAMDMA
        if self.cycles % 2 == 1 {
            self.cycles += 1;
        }

        let start = (addr_hi as u16) << 8;
        for i in 0..256 {
            let val = self.read_byte(bus, start + i);
            self.write_byte(bus, OAMDATA_ADDRESS, val);
        }
    }

    #[inline(always)]
    fn read_word(&mut self, bus: &mut SystemBus, address: u16) -> u16 {
        self.read_byte(bus, address) as u16 | ((self.read_byte(bus, address + 1) as u16) << 8)
    }

    #[inline(always)]
    fn dummy_read(&mut self, bus: &mut SystemBus) {
        self.read_byte(bus, self.regs.pc);
    }

    #[inline(always)]
    fn next_pc_byte(&mut self, bus: &mut SystemBus) -> u8 {
        let b = self.read_byte(bus, self.regs.pc);
        self.regs.pc += 1;
        b
    }

    #[inline(always)]
    fn next_pc_word(&mut self, bus: &mut SystemBus) -> u16 {
        let w = self.read_word(bus, self.regs.pc);
        self.regs.pc += 2;
        w
    }

    fn load_word_zero_page(&mut self, bus: &mut SystemBus, offset: u8) -> u16 {
        if offset == 0xFF {
            self.read_byte(bus, 0xFF) as u16 + ((self.read_byte(bus, 0x00) as u16) << 8)
        } else {
            self.read_word(bus, offset as u16)
        }
    }

    fn load(
        &mut self,
        bus: &mut SystemBus,
        am: AddressMode,
        is_modify_instruction: bool,
    ) -> (u8, Option<u16>) {
        use self::AddressMode::*;
        match am {
            Immediate => (self.next_pc_byte(bus), None),
            Absolute => {
                let addr = self.next_pc_word(bus);
                self.trigger_watchpoint |= self.check_watchpoints(addr);
                (self.read_byte(bus, addr), Some(addr))
            }
            ZeroPage => {
                let addr = self.next_pc_byte(bus) as u16;
                self.trigger_watchpoint |= self.check_watchpoints(addr);
                (self.read_byte(bus, addr), Some(addr))
            }
            AbsoluteIndexed(reg) => {
                let base = self.next_pc_word(bus);
                let index = self.get_register(reg) as u16;
                let addr = base + index;
                self.trigger_watchpoint |= self.check_watchpoints(addr);

                // When crossing page boundaries, we do an
                // extra read with an incorrect high byte.
                if is_modify_instruction || !mem_pages_same(base, addr) {
                    self.read_byte(bus, (base & 0xFF00) | (addr & 0x00FF));
                }

                (self.read_byte(bus, addr), Some(addr))
            }
            ZeroPageIndexed(reg) => {
                let base = self.next_pc_byte(bus) as u16;
                self.read_byte(bus, base);
                let index = self.get_register(reg) as u16;
                let addr = (base + index) % 0x0100;
                self.trigger_watchpoint |= self.check_watchpoints(addr);

                (self.read_byte(bus, addr), Some(addr))
            }
            IndexedIndirect(reg) => {
                let base = self.next_pc_byte(bus);
                self.read_byte(bus, base as u16);
                let index = self.get_register(reg);
                let addr = self.load_word_zero_page(bus, base + index);
                self.trigger_watchpoint |= self.check_watchpoints(addr);

                (self.read_byte(bus, addr), Some(addr))
            }
            IndirectIndexed(reg) => {
                let zp_offset = self.next_pc_byte(bus);
                let base = self.load_word_zero_page(bus, zp_offset);
                let index = self.get_register(reg) as u16;
                let addr = base + index;
                self.trigger_watchpoint |= self.check_watchpoints(addr);

                // When crossing page boundaries, we do an
                // extra read with an incorrect high byte.
                if is_modify_instruction || !mem_pages_same(base, addr) {
                    self.read_byte(bus, (base & 0xFF00) | (addr & 0x00FF));
                }

                (self.read_byte(bus, addr), Some(addr))
            }
            Register(reg) => {
                self.dummy_read(bus);
                (self.get_register(reg), None)
            }
        }
    }

    #[inline(always)]
    fn write_byte(&mut self, bus: &mut SystemBus, address: u16, value: u8) {
        if address == OAMDMA_ADDRESS {
            self.cycles += 1;
            self.handle_oam_dma(bus, value);
        } else {
            bus.write_byte(address, value);
            self.cycles += 1;
        }
    }

    fn store(&mut self, bus: &mut SystemBus, am: AddressMode, val: u8) {
        use self::AddressMode::*;
        match am {
            Absolute => {
                let addr = self.next_pc_word(bus);
                self.write_byte(bus, addr, val);
            }
            ZeroPage => {
                let addr = self.next_pc_byte(bus) as u16;
                self.write_byte(bus, addr, val);
            }
            AbsoluteIndexed(reg) => {
                let base = self.next_pc_word(bus);
                let index = self.get_register(reg) as u16;
                let addr = base + index;
                self.read_byte(bus, (base & 0xFF00) | (addr & 0x00FF));
                self.write_byte(bus, addr, val);
            }
            ZeroPageIndexed(reg) => {
                let base = self.next_pc_byte(bus) as u16;
                self.read_byte(bus, base);
                let index = self.get_register(reg) as u16;
                let addr = (base + index) % 0x0100;
                self.write_byte(bus, addr, val);
            }
            IndexedIndirect(reg) => {
                let base = self.next_pc_byte(bus);
                self.read_byte(bus, base as u16);
                let index = self.get_register(reg);
                let addr = self.load_word_zero_page(bus, base + index);
                self.write_byte(bus, addr, val);
            }
            IndirectIndexed(reg) => {
                let zp_offset = self.next_pc_byte(bus);
                let base = self.load_word_zero_page(bus, zp_offset);
                let index = self.get_register(reg) as u16;
                let addr = base + index;
                self.read_byte(bus, (base & 0xFF00) | (addr & 0x00FF));
                self.write_byte(bus, addr, val);
            }
            Register(reg) => self.set_register(reg, val),
            _ => panic!("Invalid address mode for store: {:?}", am),
        }
    }

    ///////////////////////
    // Flag helpers
    ///////////////////////

    #[inline(always)]
    fn set_zero_negative(&mut self, result: u8) {
        self.flags.z = result == 0;
        self.flags.n = (result & 0x80) != 0;
    }

    ///////////////////////
    // Register helpers
    ///////////////////////

    #[inline(always)]
    fn get_register(&self, r: Register8) -> u8 {
        use self::Register8::*;
        match r {
            A => self.regs.a,
            X => self.regs.x,
            Y => self.regs.y,
            Sp => self.regs.sp,
            Status => self.flags.into(),
        }
    }

    #[inline(always)]
    fn set_register(&mut self, r: Register8, val: u8) {
        use self::Register8::*;
        match r {
            A => self.regs.a = val,
            X => self.regs.x = val,
            Y => self.regs.y = val,
            Sp => self.regs.sp = val,
            Status => self.flags = val.into(),
        }
    }

    //////////////////////
    // Instruction helpers
    //////////////////////

    fn ld_reg(&mut self, bus: &mut SystemBus, am: AddressMode, r: Register8) {
        let (m, _) = self.load(bus, am, false);
        self.set_zero_negative(m);
        self.set_register(r, m);
    }

    fn st_reg(&mut self, bus: &mut SystemBus, am: AddressMode, r: Register8) {
        let val = self.get_register(r);
        self.store(bus, am, val);
    }

    fn branch(&mut self, bus: &mut SystemBus, cond: bool) {
        let offset = self.next_pc_byte(bus) as i8;
        if cond {
            self.dummy_read(bus);
            let addr = (self.regs.pc as i16 + offset as i16) as u16;

            // Add another cycle if the branching to a new page
            if !mem_pages_same(self.regs.pc, addr) {
                self.read_byte(bus, (self.regs.pc & 0xFF00) | (addr & 0x00FF));
            }

            self.regs.pc = addr;
        }
    }

    fn compare(&mut self, bus: &mut SystemBus, am: AddressMode, reg: Register8) {
        let (m, _) = self.load(bus, am, false);
        self.compare_value(m, reg);
    }

    fn compare_value(&mut self, value: u8, reg: Register8) {
        let r = self.get_register(reg);
        let result = r.wrapping_sub(value);

        self.set_zero_negative(result);
        self.flags.c = value <= r;
    }

    fn add_value(&mut self, value: u8) {
        let result = self.regs.a as u32 + value as u32 + self.flags.c as u32;

        self.flags.c = (result & 0x100) != 0;
        let result = result as u8;
        self.flags.v =
            ((self.regs.a & 0x80) == (value & 0x80)) && (self.regs.a & 0x80 != result & 0x80);
        self.set_zero_negative(result);

        self.regs.a = result;
    }

    fn sub_value(&mut self, value: u8) {
        let result = self.regs.a as i32 - value as i32 - (!self.flags.c) as i32;

        self.flags.c = result >= 0;

        let result = result as u8;
        self.flags.v =
            ((self.regs.a ^ value) & 0x80 != 0) && (((self.regs.a ^ result) & 0x80) != 0);
        self.set_zero_negative(result);

        self.regs.a = result;
    }

    fn and_value(&mut self, value: u8) -> u8 {
        let result = value & self.regs.a;
        self.set_zero_negative(result);
        self.regs.a = result;
        result
    }

    fn ora_value(&mut self, value: u8) {
        let result = value | self.regs.a;
        self.set_zero_negative(result);
        self.regs.a = result;
    }

    fn eor_value(&mut self, value: u8) {
        let result = value ^ self.regs.a;
        self.set_zero_negative(result);
        self.regs.a = result;
    }

    fn increment(&mut self, bus: &mut SystemBus, am: AddressMode) -> u8 {
        if let (val, Some(addr)) = self.load(bus, am, true) {
            self.write_byte(bus, addr, val);
            let result = val.wrapping_add(1);
            self.set_zero_negative(result);
            self.write_byte(bus, addr, result);
            result
        } else {
            unreachable!()
        }
    }

    fn decrement(&mut self, bus: &mut SystemBus, am: AddressMode) -> u8 {
        if let (val, Some(addr)) = self.load(bus, am, true) {
            self.write_byte(bus, addr, val);
            let result = val.wrapping_sub(1);
            self.set_zero_negative(result);
            self.write_byte(bus, addr, result);
            result
        } else {
            unreachable!()
        }
    }

    fn arithmetic_shift_left(&mut self, bus: &mut SystemBus, am: AddressMode) -> u8 {
        let (val, addr) = self.load(bus, am, true);
        let result = (val << 1) & 0xFE;
        self.set_zero_negative(result);
        self.flags.c = (val & 0x80) != 0;

        if let Some(addr) = addr {
            self.write_byte(bus, addr, val);
            self.write_byte(bus, addr, result);
        } else if let AddressMode::Register(reg) = am {
            self.set_register(reg, result);
        }

        result
    }

    fn rotate_right(&mut self, bus: &mut SystemBus, am: AddressMode) -> u8 {
        let (val, addr) = self.load(bus, am, true);
        let carry: u8 = if self.flags.c { 1 << 7 } else { 0 };
        let result = ((val >> 1) & 0x7F) | carry;
        self.set_zero_negative(result);
        self.flags.c = (val & 0x01) != 0;

        if let Some(addr) = addr {
            self.write_byte(bus, addr, val);
            self.write_byte(bus, addr, result);
        } else if let AddressMode::Register(reg) = am {
            self.set_register(reg, result);
        }

        result
    }

    fn rotate_left(&mut self, bus: &mut SystemBus, am: AddressMode) -> u8 {
        let (val, addr) = self.load(bus, am, true);
        let carry: u8 = u8::from(self.flags.c);
        let result = ((val << 1) & 0xFE) | carry;
        self.set_zero_negative(result);
        self.flags.c = (val & 0x80) != 0;

        if let Some(addr) = addr {
            self.write_byte(bus, addr, val);
            self.write_byte(bus, addr, result);
        } else if let AddressMode::Register(reg) = am {
            self.set_register(reg, result);
        }

        result
    }

    fn logical_shift_right(&mut self, bus: &mut SystemBus, am: AddressMode) -> u8 {
        let (val, addr) = self.load(bus, am, true);
        let result = (val >> 1) & 0x7F;
        self.set_zero_negative(result);
        self.flags.c = (val & 0x01) != 0;

        if let Some(addr) = addr {
            self.write_byte(bus, addr, val);
            self.write_byte(bus, addr, result);
        } else if let AddressMode::Register(reg) = am {
            self.set_register(reg, result);
        }

        result
    }

    // Push byte onto the stack
    fn push_byte(&mut self, bus: &mut SystemBus, val: u8) {
        let s = self.regs.sp;
        self.write_byte(bus, 0x0100 | (s as u16), val);
        self.regs.sp = s - 1;
    }

    // Pull byte from the stack
    fn pull_byte(&mut self, bus: &mut SystemBus) -> u8 {
        let s = self.regs.sp + 1;
        self.regs.sp = s;

        self.read_byte(bus, 0x0100 | (s as u16))
    }

    // Push word onto the stack
    fn push_word(&mut self, bus: &mut SystemBus, val: u16) {
        self.push_byte(bus, (val >> 8) as u8);
        self.push_byte(bus, val as u8);
    }

    // Pull word from the stack
    fn pull_word(&mut self, bus: &mut SystemBus) -> u16 {
        let lsb = self.pull_byte(bus);
        let msb = self.pull_byte(bus);

        ((msb as u16) << 8) | (lsb as u16)
    }

    ///////////////////
    // Instructions
    ///////////////////

    fn lda(&mut self, bus: &mut SystemBus, am: AddressMode) {
        self.ld_reg(bus, am, Register8::A);
    }

    fn ldx(&mut self, bus: &mut SystemBus, am: AddressMode) {
        self.ld_reg(bus, am, Register8::X);
    }

    fn ldy(&mut self, bus: &mut SystemBus, am: AddressMode) {
        self.ld_reg(bus, am, Register8::Y);
    }

    fn sta(&mut self, bus: &mut SystemBus, am: AddressMode) {
        self.st_reg(bus, am, Register8::A);
    }

    fn stx(&mut self, bus: &mut SystemBus, am: AddressMode) {
        self.st_reg(bus, am, Register8::X);
    }

    fn sty(&mut self, bus: &mut SystemBus, am: AddressMode) {
        self.st_reg(bus, am, Register8::Y);
    }

    fn adc(&mut self, bus: &mut SystemBus, am: AddressMode) {
        let (m, _) = self.load(bus, am, false);
        self.add_value(m);
    }

    fn sbc(&mut self, bus: &mut SystemBus, am: AddressMode) {
        let (m, _) = self.load(bus, am, false);
        self.sub_value(m);
    }

    fn and(&mut self, bus: &mut SystemBus, am: AddressMode) {
        let (m, _) = self.load(bus, am, false);
        self.and_value(m);
    }

    fn ora(&mut self, bus: &mut SystemBus, am: AddressMode) {
        let (m, _) = self.load(bus, am, false);
        self.ora_value(m);
    }

    fn eor(&mut self, bus: &mut SystemBus, am: AddressMode) {
        let (m, _) = self.load(bus, am, false);
        self.eor_value(m);
    }

    fn sec(&mut self, bus: &mut SystemBus) {
        self.dummy_read(bus);
        self.flags.c = true;
    }

    fn clc(&mut self, bus: &mut SystemBus) {
        self.dummy_read(bus);
        self.flags.c = false;
    }

    fn sei(&mut self, bus: &mut SystemBus) {
        self.dummy_read(bus);
        self.flags.i = true;
    }

    fn cli(&mut self, bus: &mut SystemBus) {
        self.dummy_read(bus);
        self.flags.i = false;
    }

    fn sed(&mut self, bus: &mut SystemBus) {
        self.dummy_read(bus);
        self.flags.d = true;
    }

    fn cld(&mut self, bus: &mut SystemBus) {
        self.dummy_read(bus);
        self.flags.d = false;
    }

    fn clv(&mut self, bus: &mut SystemBus) {
        self.dummy_read(bus);
        self.flags.v = false;
    }

    fn cmp(&mut self, bus: &mut SystemBus, am: AddressMode) {
        self.compare(bus, am, Register8::A)
    }

    fn cpx(&mut self, bus: &mut SystemBus, am: AddressMode) {
        self.compare(bus, am, Register8::X)
    }

    fn cpy(&mut self, bus: &mut SystemBus, am: AddressMode) {
        self.compare(bus, am, Register8::Y)
    }

    fn bit(&mut self, bus: &mut SystemBus, am: AddressMode) {
        let (m, _) = self.load(bus, am, false);
        let a = self.regs.a;

        self.flags.n = (m & 0x80) != 0;
        self.flags.v = (m & 0x40) != 0;
        self.flags.z = (m & a) == 0;
    }

    fn inc(&mut self, bus: &mut SystemBus, am: AddressMode) {
        self.increment(bus, am);
    }

    fn dec(&mut self, bus: &mut SystemBus, am: AddressMode) {
        self.decrement(bus, am);
    }

    fn inx(&mut self, bus: &mut SystemBus) {
        self.dummy_read(bus);
        let val = self.regs.x.wrapping_add(1);
        self.set_zero_negative(val);
        self.regs.x = val;
    }

    fn iny(&mut self, bus: &mut SystemBus) {
        self.dummy_read(bus);
        let val = self.regs.y.wrapping_add(1);
        self.set_zero_negative(val);
        self.regs.y = val;
    }

    fn dex(&mut self, bus: &mut SystemBus) {
        self.dummy_read(bus);
        let val = self.regs.x.wrapping_sub(1);
        self.set_zero_negative(val);
        self.regs.x = val;
    }

    fn dey(&mut self, bus: &mut SystemBus) {
        self.dummy_read(bus);
        let val = self.regs.y.wrapping_sub(1);
        self.set_zero_negative(val);
        self.regs.y = val;
    }

    fn tax(&mut self, bus: &mut SystemBus) {
        self.dummy_read(bus);
        self.set_zero_negative(self.regs.a);
        self.regs.x = self.regs.a;
    }

    fn txa(&mut self, bus: &mut SystemBus) {
        self.dummy_read(bus);
        self.set_zero_negative(self.regs.x);
        self.regs.a = self.regs.x;
    }

    fn tay(&mut self, bus: &mut SystemBus) {
        self.dummy_read(bus);
        self.set_zero_negative(self.regs.a);
        self.regs.y = self.regs.a;
    }

    fn tya(&mut self, bus: &mut SystemBus) {
        self.dummy_read(bus);
        self.set_zero_negative(self.regs.y);
        self.regs.a = self.regs.y;
    }

    fn tsx(&mut self, bus: &mut SystemBus) {
        self.dummy_read(bus);
        self.set_zero_negative(self.regs.sp);
        self.regs.x = self.regs.sp;
    }

    fn txs(&mut self, bus: &mut SystemBus) {
        self.dummy_read(bus);
        self.regs.sp = self.regs.x;
    }

    fn jsr(&mut self, bus: &mut SystemBus) {
        let addr_lo = self.next_pc_byte(bus);
        self.cycles += 1;
        self.push_word(bus, self.regs.pc);
        let addr_hi = self.next_pc_byte(bus);
        self.regs.pc = ((addr_hi as u16) << 8) | addr_lo as u16;
    }

    fn rts(&mut self, bus: &mut SystemBus) {
        self.dummy_read(bus);
        self.cycles += 1;
        self.regs.pc = self.pull_word(bus) + 1;
        self.cycles += 1;
    }

    fn pha(&mut self, bus: &mut SystemBus) {
        self.dummy_read(bus);
        self.push_byte(bus, self.regs.a);
    }

    fn pla(&mut self, bus: &mut SystemBus) {
        self.dummy_read(bus);
        self.cycles += 1;
        let val = self.pull_byte(bus);
        self.set_zero_negative(val);
        self.regs.a = val;
    }

    fn php(&mut self, bus: &mut SystemBus) {
        self.dummy_read(bus);
        let mut status = self.flags;
        status.b = true;
        status.e = true;
        self.push_byte(bus, status.into());
    }

    fn plp(&mut self, bus: &mut SystemBus) {
        self.dummy_read(bus);
        self.cycles += 1;
        let val = self.pull_byte(bus);
        self.flags = val.into();
    }

    fn lsr(&mut self, bus: &mut SystemBus, am: AddressMode) {
        self.logical_shift_right(bus, am);
    }

    fn asl(&mut self, bus: &mut SystemBus, am: AddressMode) {
        self.arithmetic_shift_left(bus, am);
    }

    fn ror(&mut self, bus: &mut SystemBus, am: AddressMode) {
        self.rotate_right(bus, am);
    }

    fn rol(&mut self, bus: &mut SystemBus, am: AddressMode) {
        self.rotate_left(bus, am);
    }

    fn brk(&mut self, bus: &mut SystemBus) {
        self.dummy_read(bus);
        self.push_word(bus, self.regs.pc + 1);
        let mut status = self.flags;
        status.b = true;
        status.e = true;
        self.push_byte(bus, status.into());
        self.flags.i = true;
        self.regs.pc = self.read_word(bus, BRK_VECTOR);
    }

    fn rti(&mut self, bus: &mut SystemBus) {
        self.dummy_read(bus);
        self.cycles += 1;
        let status = self.pull_byte(bus);
        let pc = self.pull_word(bus);

        self.flags = status.into();
        self.regs.pc = pc;
    }

    fn nop(&mut self, bus: &mut SystemBus, am: AddressMode) {
        let pc = self.regs.pc;
        self.load(bus, am, false);
        self.regs.pc = pc;
    }

    ///////////////////////////
    // Unofficial Instructions
    ///////////////////////////

    fn alr(&mut self, bus: &mut SystemBus) {
        self.and(bus, AddressMode::Immediate);
        self.lsr(bus, AddressMode::Register(Register8::A));
    }

    // Does AND #i, setting N and Z flags based on the result. Then it copies N (bit 7) to C
    fn anc(&mut self, bus: &mut SystemBus) {
        self.and(bus, AddressMode::Immediate);
        self.flags.c = self.flags.n;
    }

    fn arr(&mut self, bus: &mut SystemBus) {
        let imm = self.next_pc_byte(bus);

        self.regs.a = ((self.flags.c as u8) << 7) | ((self.regs.a & imm) >> 1);

        let a = self.regs.a;
        self.set_zero_negative(a);
        self.flags.c = (a & 0x40) != 0;
        self.flags.v = ((a ^ (a << 1)) & 0x40) != 0;
    }

    // Sets X to {(A AND X) - #value without borrow}, and updates NZC
    fn axs(&mut self, bus: &mut SystemBus) {
        let imm = self.next_pc_byte(bus);
        let a_and_x = self.regs.a & self.regs.x;
        let result = (a_and_x).wrapping_sub(imm);
        self.set_zero_negative(result);
        self.flags.c = imm <= a_and_x;
        self.regs.x = result;
    }

    // Stores the bitwise AND of A and X. As with STA and STX, no flags are affected.
    fn sax(&mut self, bus: &mut SystemBus, am: AddressMode) {
        self.store(bus, am, self.regs.a & self.regs.x);
    }

    // Equivalent to DEC value then CMP value
    fn dcp(&mut self, bus: &mut SystemBus, am: AddressMode) {
        let val = self.decrement(bus, am);
        self.compare_value(val, Register8::A)
    }

    // Equivalent to INC value then SBC value
    fn isc(&mut self, bus: &mut SystemBus, am: AddressMode) {
        let val = self.increment(bus, am);
        self.sub_value(val);
    }

    // Equivalent to ROL value then AND value
    fn rla(&mut self, bus: &mut SystemBus, am: AddressMode) {
        let val = self.rotate_left(bus, am);
        self.and_value(val);
    }

    // Equivalent to ROR value then ADC value
    fn rra(&mut self, bus: &mut SystemBus, am: AddressMode) {
        let val = self.rotate_right(bus, am);
        self.add_value(val);
    }

    fn sre(&mut self, bus: &mut SystemBus, am: AddressMode) {
        let val = self.logical_shift_right(bus, am);
        self.eor_value(val);
    }

    // Read an immediate byte and skip it, like a different address mode of NOP
    fn skb(&mut self, bus: &mut SystemBus) {
        self.next_pc_byte(bus);
    }

    // Reads from memory at the specified address and ignores the value. Affects no register nor flags.
    fn ign(&mut self, bus: &mut SystemBus, am: AddressMode) {
        let _ = self.load(bus, am, false);
    }

    // Used by "Gaau Hok Gwong Cheung (Ch)"
    // This instruction can be unpredictable.
    // See http://visual6502.org/wiki/index.php?title=6502_Opcode_8B_%28XAA,_ANE%29
    fn xaa(&mut self, bus: &mut SystemBus) {
        self.regs.a = self.regs.a & self.regs.x & self.next_pc_byte(bus);
    }

    // Used by "Super Cars (U)"
    fn lax(&mut self, bus: &mut SystemBus, am: AddressMode) {
        self.lda(bus, am);
        self.set_zero_negative(self.regs.a);
        self.regs.x = self.regs.a;
    }

    // Equivalent to ASL value then ORA value
    // Used by "Disney's Aladdin (E)"
    fn slo(&mut self, bus: &mut SystemBus, am: AddressMode) {
        let val = self.arithmetic_shift_left(bus, am);
        self.ora_value(val);
    }

    // {adr}:=A&X&H
    // Unstable in certain matters on real CPU
    fn ahx(&mut self, bus: &mut SystemBus, am: AddressMode) {
        if let (_, Some(addr)) = self.load(bus, am, true) {
            let h = (addr >> 8) as u8;
            self.write_byte(bus, addr, self.regs.a & h & self.regs.x);
        }
    }

    // Some unofficial write instructions have an internal bus conflict that causes strange behaviors.
    fn unofficial_strange_write(&mut self, bus: &mut SystemBus, value: u8, index: u8) {
        let base = self.next_pc_word(bus);
        let addr = base + index as u16;

        let result = value & ((base >> 8) + 1) as u8;

        let addr = if ((base ^ addr) & 0x100) != 0 {
            // Page crossed
            (addr & ((value as u16) << 8)) | (addr & 0x00FF)
        } else {
            addr
        };

        self.write_byte(bus, addr, result);
    }

    fn sya(&mut self, bus: &mut SystemBus) {
        let y = self.regs.y;
        let index = self.regs.x;

        self.unofficial_strange_write(bus, y, index);
    }

    fn sxa(&mut self, bus: &mut SystemBus) {
        let x = self.regs.x;
        let index = self.regs.y;

        self.unofficial_strange_write(bus, x, index);
    }

    ///////////////
    // Interrupts
    ///////////////

    // Request interrupt to be run on next step
    pub fn request_interrupt(&mut self, interrupt: Interrupt) {
        match interrupt {
            Interrupt::Nmi => self.interrupt = Some(interrupt),
            Interrupt::Irq => {
                if self.interrupt.is_none() {
                    self.interrupt = Some(interrupt);
                }
            }
        }
    }

    fn handle_interrupts(&mut self, bus: &mut SystemBus) -> bool {
        if let Some(interrupt) = self.interrupt {
            match interrupt {
                Interrupt::Nmi => self.handle_interrupt(bus, NMI_VECTOR),
                Interrupt::Irq => {
                    if !self.flags.i {
                        self.handle_interrupt(bus, IRQ_VECTOR);
                        self.flags.i = true;
                    }
                }
            }
            true
        } else {
            false
        }
    }

    fn handle_interrupt(&mut self, bus: &mut SystemBus, vector: u16) {
        self.dummy_read(bus);
        self.push_word(bus, self.regs.pc);
        let mut status = self.flags;
        status.b = true;
        status.e = true;
        self.push_byte(bus, status.into());
        self.regs.pc = self.read_word(bus, vector);
        self.interrupt = None;
    }

    fn fetch_rel_offset(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.rel_offset = self.next_pc_byte(bus) as i16;
        false
    }

    fn check_branch_n(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.check_branch(bus, self.flags.n)
    }

    fn check_branch_not_n(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.check_branch(bus, !self.flags.n)
    }

    fn check_branch_c(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.check_branch(bus, self.flags.c)
    }

    fn check_branch_not_c(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.check_branch(bus, !self.flags.c)
    }

    fn check_branch_z(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.check_branch(bus, self.flags.z)
    }

    fn check_branch_not_z(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.check_branch(bus, !self.flags.z)
    }

    fn check_branch_v(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.check_branch(bus, self.flags.v)
    }

    fn check_branch_not_v(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.check_branch(bus, !self.flags.v)
    }

    fn check_branch(self: &mut Cpu, bus: &mut SystemBus, cond: bool) -> bool {
        self.dummy_read(bus);

        if !cond {
            // Branch not taken.
            return true;
        }

        let addr = (self.regs.pc as i16 + self.rel_offset) as u16;

        let new_page = !mem_pages_same(self.regs.pc, addr);

        self.regs.pc = addr;

        // If no page was crossed, the branch is complete.
        // Otherwise another cycle is required to fix the pc high byte
        // (`fix_pc_high`).
        !new_page
    }

    // Only called if there was a page cross.
    fn branch_fix_pc_high(self: &mut Cpu, _bus: &mut SystemBus) -> bool {
        if self.rel_offset < 0x80 {
            self.regs.pc += 0x0100;
        } else {
            self.regs.pc -= 0x0100;
        }
        true
    }

    fn fetch_immediate(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.fetched_data = self.next_pc_byte(bus);
        false
    }

    fn fetch_abs_low(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.addr_abs = self.next_pc_byte(bus) as u16;
        false
    }

    fn fetch_abs_high(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let high = (self.next_pc_byte(bus) as u16) << 8;
        self.addr_abs |= high;
        false
    }

    fn lda_execute_fetched_data(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.regs.a = self.fetched_data;
        self.set_zero_negative(self.regs.a);
        true
    }

    fn lda_execute_addr_abs(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.regs.a = bus.read_byte(self.addr_abs);
        self.set_zero_negative(self.regs.a);
        true
    }

    fn jmp_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let high = (self.next_pc_byte(bus) as u16) << 8;
        self.regs.pc |= high;
        true
    }

    fn jni_read_pointer_low(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.fetched_data = bus.read_byte(self.addr_abs);
        false
    }

    fn jni_read_pointer_high(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        // There is a hardware bug in the JNI instruction. If the 16-bit argument of an indirect JMP is
        // located between 2 pages (0x01FF and 0x0200 for example), then the LSB will be read from
        // 0x01FF and the MSB will be read from 0x0100.
        let msb = bus.read_byte(if (self.addr_abs & 0xFF) == 0xFF {
            self.addr_abs & 0xff00
        } else {
            self.addr_abs + 1
        });

        let lsb = self.fetched_data;

        self.regs.pc = ((msb as u16) << 8) | (lsb as u16);

        true
    }
}

pub const OPCODES: [Option<Instruction>; 256] = {
    let mut opcodes = [None; 256];

    // LDA Immediate
    opcodes[0xA9] = Some(Instruction {
        name: "LDA",
        cycles: &[Cpu::fetch_immediate, Cpu::lda_execute_fetched_data],
    });

    // LDA Zero Page
    opcodes[0xA5] = Some(Instruction {
        name: "LDA",
        cycles: &[Cpu::fetch_abs_low, Cpu::lda_execute_addr_abs],
    });

    // LDA Zero Page Indexed X
    opcodes[0xA5] = Some(Instruction {
        name: "LDA",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::read_tmp_addr,
            Cpu::lda_execute_indexed_x,
        ],
    });

    // LDA Absolute
    opcodes[0xAD] = Some(Instruction {
        name: "LDA",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::lda_execute_addr_abs,
        ],
    });

    opcodes[0x4C] = Some(Instruction {
        name: "JMP",
        cycles: &[Cpu::fetch_abs_low, Cpu::jmp_abs_finish],
    });

    opcodes[0x6C] = Some(Instruction {
        name: "JMPI",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::jni_read_pointer_low,
            Cpu::jni_read_pointer_high,
        ],
    });

    opcodes[0x30] = Some(Instruction {
        name: "BMI",
        cycles: &[
            Cpu::fetch_rel_offset,
            Cpu::check_branch_n,
            Cpu::branch_fix_pc_high,
        ],
    });

    opcodes[0x10] = Some(Instruction {
        name: "BPL",
        cycles: &[
            Cpu::fetch_rel_offset,
            Cpu::check_branch_not_n,
            Cpu::branch_fix_pc_high,
        ],
    });

    opcodes[0x90] = Some(Instruction {
        name: "BCC",
        cycles: &[
            Cpu::fetch_rel_offset,
            Cpu::check_branch_not_c,
            Cpu::branch_fix_pc_high,
        ],
    });

    opcodes[0xB0] = Some(Instruction {
        name: "BCS",
        cycles: &[
            Cpu::fetch_rel_offset,
            Cpu::check_branch_c,
            Cpu::branch_fix_pc_high,
        ],
    });

    opcodes[0xF0] = Some(Instruction {
        name: "BEQ",
        cycles: &[
            Cpu::fetch_rel_offset,
            Cpu::check_branch_z,
            Cpu::branch_fix_pc_high,
        ],
    });

    opcodes[0xD0] = Some(Instruction {
        name: "BNE",
        cycles: &[
            Cpu::fetch_rel_offset,
            Cpu::check_branch_not_z,
            Cpu::branch_fix_pc_high,
        ],
    });

    opcodes[0x70] = Some(Instruction {
        name: "BVS",
        cycles: &[
            Cpu::fetch_rel_offset,
            Cpu::check_branch_v,
            Cpu::branch_fix_pc_high,
        ],
    });

    opcodes[0x50] = Some(Instruction {
        name: "BVC",
        cycles: &[
            Cpu::fetch_rel_offset,
            Cpu::check_branch_not_v,
            Cpu::branch_fix_pc_high,
        ],
    });

    opcodes
};
