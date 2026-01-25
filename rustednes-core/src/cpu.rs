use crate::system_bus::SystemBus;

use serde_derive::{Deserialize, Serialize};

use std::fmt;
use std::fmt::{Debug, Formatter};

pub type CycleFn = fn(&mut Cpu, bus: &mut SystemBus) -> bool;

#[derive(Copy, Clone)]
#[allow(dead_code)]
pub struct Instruction {
    name: &'static str,
    cycles: &'static [CycleFn],
}

pub const CPU_FREQUENCY: u64 = 1_789_773;

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

#[derive(Default)]
pub struct Cpu {
    stall_cycles: u8,
    regs: Regs,
    flags: Flags,
    instruction: Option<Instruction>,
    active_interrupt: Option<Instruction>,
    nmi_line_low: bool,
    nmi_pended: bool,
    pub irq_line_low: bool,

    // Tempory mid-instruction data to be able to run one cycle at a time.
    opcode: u8,
    cycle: u8,
    addr_abs: u16,
    temp_addr_low: u8,
    base_addr: u8,
    rel_offset: i16,
    fetched_data: u8,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct State {
    pub stall_cycles: u8,
    pub regs: Regs,
    pub flags: Flags,
    // TODO: Fix save state for cycle accurate emulation.
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
        }
    }

    pub fn apply_state(&mut self, state: &State) {
        self.stall_cycles = state.stall_cycles;
        self.regs = state.regs;
        self.flags = state.flags;
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
        self.active_interrupt = None;
    }

    // Run a single CPU cycle. Returns whether an instruction completed.
    pub fn tick(&mut self, bus: &mut SystemBus) -> bool {
        if self.stall_cycles > 0 {
            self.stall_cycles -= 1;
            return false;
        }

        if self.cycle == 0 {
            self.cycle = 1;

            if self.nmi_pended {
                self.nmi_pended = false;
                self.active_interrupt = Some(NMI_INTERRUPT);
            } else if self.irq_line_low && !self.flags.i {
                self.active_interrupt = Some(IRQ_INTERRUPT);
            } else {
                self.active_interrupt = None;
            }

            if let Some(interrupt) = self.active_interrupt {
                // Hijack fetching instruction and run interrupt sequence.
                self.instruction = Some(interrupt);
            } else {
                // Fetch new instruction.
                self.opcode = self.next_pc_byte(bus);
                self.instruction = OPCODES[self.opcode as usize];
                return false;
            }
        }

        // Continue in-progress instruction.
        let instr = match self.instruction {
            Some(instr) => instr,
            None => panic!(
                "Unsuppported opcode: {:#04x}, pc: {:#06X}",
                self.opcode, self.regs.pc
            ),
        };

        let cycle_fn = instr.cycles[self.cycle as usize - 1];
        let completed = cycle_fn(self, bus);

        self.cycle += 1;

        if completed || self.cycle as usize > instr.cycles.len() {
            self.cycle = 0;
            true
        } else {
            false
        }
    }

    #[inline(always)]
    fn next_pc_byte(&mut self, bus: &mut SystemBus) -> u8 {
        let b = bus.read_byte(self.regs.pc);
        self.regs.pc = self.regs.pc.wrapping_add(1);
        b
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

    #[inline(always)]
    fn add_value(&mut self, value: u8) {
        let result = self.regs.a as u32 + value as u32 + self.flags.c as u32;

        self.flags.c = (result & 0x100) != 0;
        let result = result as u8;
        self.flags.v =
            ((self.regs.a & 0x80) == (value & 0x80)) && (self.regs.a & 0x80 != result & 0x80);
        self.set_zero_negative(result);

        self.regs.a = result;
    }

    #[inline(always)]
    fn sub_value(&mut self, value: u8) {
        let result = self.regs.a as i32 - value as i32 - (!self.flags.c) as i32;

        self.flags.c = result >= 0;

        let result = result as u8;
        self.flags.v =
            ((self.regs.a ^ value) & 0x80 != 0) && (((self.regs.a ^ result) & 0x80) != 0);
        self.set_zero_negative(result);

        self.regs.a = result;
    }

    #[inline(always)]
    fn and_value(&mut self, value: u8) -> u8 {
        let result = value & self.regs.a;
        self.set_zero_negative(result);
        self.regs.a = result;
        result
    }

    #[inline(always)]
    fn ora_value(&mut self, value: u8) {
        let result = value | self.regs.a;
        self.set_zero_negative(result);
        self.regs.a = result;
    }

    #[inline(always)]
    fn eor_value(&mut self, value: u8) {
        let result = value ^ self.regs.a;
        self.set_zero_negative(result);
        self.regs.a = result;
    }

    // Push byte onto the stack
    #[inline(always)]
    fn push_byte(&mut self, bus: &mut SystemBus, val: u8) {
        let s = self.regs.sp;
        bus.write_byte(0x0100 | (s as u16), val);
        self.regs.sp = s.wrapping_sub(1);
    }

    // Pull byte from the stack
    #[inline(always)]
    fn pull_byte(&mut self, bus: &mut SystemBus) -> u8 {
        self.regs.sp = self.regs.sp.wrapping_add(1);

        bus.read_byte(0x0100 | (self.regs.sp as u16))
    }

    ///////////////////////////
    // Unofficial Instructions
    ///////////////////////////

    // fn alr(&mut self, bus: &mut SystemBus) {
    //     self.and(bus, AddressMode::Immediate);
    //     self.lsr(bus, AddressMode::Register(Register8::A));
    // }
    //
    // // Does AND #i, setting N and Z flags based on the result. Then it copies N (bit 7) to C
    // fn anc(&mut self, bus: &mut SystemBus) {
    //     self.and(bus, AddressMode::Immediate);
    //     self.flags.c = self.flags.n;
    // }
    //
    // fn arr(&mut self, bus: &mut SystemBus) {
    //     let imm = self.next_pc_byte(bus);
    //
    //     self.regs.a = ((self.flags.c as u8) << 7) | ((self.regs.a & imm) >> 1);
    //
    //     let a = self.regs.a;
    //     self.set_zero_negative(a);
    //     self.flags.c = (a & 0x40) != 0;
    //     self.flags.v = ((a ^ (a << 1)) & 0x40) != 0;
    // }
    //
    // // Sets X to {(A AND X) - #value without borrow}, and updates NZC
    // fn axs(&mut self, bus: &mut SystemBus) {
    //     let imm = self.next_pc_byte(bus);
    //     let a_and_x = self.regs.a & self.regs.x;
    //     let result = (a_and_x).wrapping_sub(imm);
    //     self.set_zero_negative(result);
    //     self.flags.c = imm <= a_and_x;
    //     self.regs.x = result;
    // }
    //
    // // Stores the bitwise AND of A and X. As with STA and STX, no flags are affected.
    // fn sax(&mut self, bus: &mut SystemBus, am: AddressMode) {
    //     self.store(bus, am, self.regs.a & self.regs.x);
    // }
    //
    // // Equivalent to DEC value then CMP value
    // fn dcp(&mut self, bus: &mut SystemBus, am: AddressMode) {
    //     let val = self.decrement(bus, am);
    //     self.compare_value(val, Register8::A)
    // }
    //
    // // Equivalent to INC value then SBC value
    // fn isc(&mut self, bus: &mut SystemBus, am: AddressMode) {
    //     let val = self.increment(bus, am);
    //     self.sub_value(val);
    // }
    //
    // // Equivalent to ROL value then AND value
    // fn rla(&mut self, bus: &mut SystemBus, am: AddressMode) {
    //     let val = self.rotate_left(bus, am);
    //     self.and_value(val);
    // }
    //
    // // Equivalent to ROR value then ADC value
    // fn rra(&mut self, bus: &mut SystemBus, am: AddressMode) {
    //     let val = self.rotate_right(bus, am);
    //     self.add_value(val);
    // }
    //
    // fn sre(&mut self, bus: &mut SystemBus, am: AddressMode) {
    //     let val = self.logical_shift_right(bus, am);
    //     self.eor_value(val);
    // }
    //
    // // Read an immediate byte and skip it, like a different address mode of NOP
    // fn skb(&mut self, bus: &mut SystemBus) {
    //     self.next_pc_byte(bus);
    // }
    //
    // // Reads from memory at the specified address and ignores the value. Affects no register nor flags.
    // fn ign(&mut self, bus: &mut SystemBus, am: AddressMode) {
    //     let _ = self.load(bus, am, false);
    // }
    //
    // // Used by "Gaau Hok Gwong Cheung (Ch)"
    // // This instruction can be unpredictable.
    // // See http://visual6502.org/wiki/index.php?title=6502_Opcode_8B_%28XAA,_ANE%29
    // fn xaa(&mut self, bus: &mut SystemBus) {
    //     self.regs.a = self.regs.a & self.regs.x & self.next_pc_byte(bus);
    // }
    //
    // // Used by "Super Cars (U)"
    // fn lax(&mut self, bus: &mut SystemBus, am: AddressMode) {
    //     self.lda(bus, am);
    //     self.set_zero_negative(self.regs.a);
    //     self.regs.x = self.regs.a;
    // }
    //
    // // Equivalent to ASL value then ORA value
    // // Used by "Disney's Aladdin (E)"
    // fn slo(&mut self, bus: &mut SystemBus, am: AddressMode) {
    //     let val = self.arithmetic_shift_left(bus, am);
    //     self.ora_value(val);
    // }
    //
    // // {adr}:=A&X&H
    // // Unstable in certain matters on real CPU
    // fn ahx(&mut self, bus: &mut SystemBus, am: AddressMode) {
    //     if let (_, Some(addr)) = self.load(bus, am, true) {
    //         let h = (addr >> 8) as u8;
    //         self.write_byte(bus, addr, self.regs.a & h & self.regs.x);
    //     }
    // }
    //
    // // Some unofficial write instructions have an internal bus conflict that causes strange behaviors.
    // fn unofficial_strange_write(&mut self, bus: &mut SystemBus, value: u8, index: u8) {
    //     let base = self.next_pc_word(bus);
    //     let addr = base + index as u16;
    //
    //     let result = value & ((base >> 8) + 1) as u8;
    //
    //     let addr = if ((base ^ addr) & 0x100) != 0 {
    //         // Page crossed
    //         (addr & ((value as u16) << 8)) | (addr & 0x00FF)
    //     } else {
    //         addr
    //     };
    //
    //     self.write_byte(bus, addr, result);
    // }
    //
    // fn sya(&mut self, bus: &mut SystemBus) {
    //     let y = self.regs.y;
    //     let index = self.regs.x;
    //
    //     self.unofficial_strange_write(bus, y, index);
    // }
    //
    // fn sxa(&mut self, bus: &mut SystemBus) {
    //     let x = self.regs.x;
    //     let index = self.regs.y;
    //
    //     self.unofficial_strange_write(bus, x, index);
    // }

    ///////////////
    // Interrupts
    ///////////////

    // Request NMI to be run on next instruction fetch attempt.
    pub fn set_nmi_line_low(&mut self, val: bool) {
        if !self.nmi_line_low && val {
            self.nmi_pended = true;
        }
        self.nmi_line_low = val;
    }

    // Request IRQ to be run on next instruction fetch attempt.
    pub fn set_irq_line_low(&mut self, val: bool) {
        self.irq_line_low = val;
    }

    // Instruction Cycle Functions

    fn fetch_rel_offset_and_check_branch_n(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.rel_offset = self.next_pc_byte(bus) as i8 as i16;
        !self.flags.n
    }

    fn fetch_rel_offset_and_check_branch_not_n(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.rel_offset = self.next_pc_byte(bus) as i8 as i16;
        self.flags.n
    }

    fn fetch_rel_offset_and_check_branch_c(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.rel_offset = self.next_pc_byte(bus) as i8 as i16;
        !self.flags.c
    }

    fn fetch_rel_offset_and_check_branch_not_c(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.rel_offset = self.next_pc_byte(bus) as i8 as i16;
        self.flags.c
    }

    fn fetch_rel_offset_and_check_branch_z(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.rel_offset = self.next_pc_byte(bus) as i8 as i16;
        !self.flags.z
    }

    fn fetch_rel_offset_and_check_branch_not_z(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.rel_offset = self.next_pc_byte(bus) as i8 as i16;
        self.flags.z
    }

    fn fetch_rel_offset_and_check_branch_v(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.rel_offset = self.next_pc_byte(bus) as i8 as i16;
        !self.flags.v
    }

    fn fetch_rel_offset_and_check_branch_not_v(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.rel_offset = self.next_pc_byte(bus) as i8 as i16;
        self.flags.v
    }

    fn take_branch(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let old_pc = self.regs.pc;
        let new_pc = self.regs.pc.wrapping_add_signed(self.rel_offset);

        // println!(
        //     "old_pc: {:#06X}, new_pc: {:#06X}, offset: {}",
        //     old_pc, new_pc, self.rel_offset
        // );

        // Dummy fetch using using old high byte and new low byte of the pc.
        let dummy_addr = (old_pc & 0xFF00) | (new_pc & 0x00FF);
        let _ = bus.read_byte(dummy_addr);

        self.regs.pc = new_pc;

        // If not crossing a page, this is the last cycle.
        if (old_pc & 0xFF00) == (new_pc & 0xFF00) {
            return true;
        }

        // When there is a page cross, there is an extra cycle to fixup the page.
        // We've already set the PC correctly, but we still need the extra cycle.
        false
    }

    // Only called if there was a page cross.
    fn branch_page_fixup(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        // Read from corrected PC.
        let _ = bus.read_byte(self.regs.pc);
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

    fn fetch_base_addr(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.base_addr = self.next_pc_byte(bus);
        false
    }

    fn dummy_read_base(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        bus.read_byte(self.base_addr as u16);
        false
    }

    fn dummy_read(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        bus.read_byte(self.regs.pc);
        false
    }

    fn dummy_fetch_and_inc_pc(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        bus.read_byte(self.regs.pc);
        self.regs.pc = self.regs.pc.wrapping_add(1);
        false
    }

    fn indexed_x_dummy_read_and_add(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        bus.read_byte(self.base_addr as u16);
        self.base_addr = self.base_addr.wrapping_add(self.regs.x);
        false
    }

    fn indexed_fetch_ptr_low(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.temp_addr_low = bus.read_byte(self.base_addr as u16);
        false
    }

    fn indexed_fetch_ptr_high(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let addr_high = bus.read_byte(self.base_addr.wrapping_add(1) as u16);
        self.addr_abs = ((addr_high as u16) << 8) | (self.temp_addr_low as u16);
        false
    }

    fn lda_fetched_data_finish(self: &mut Cpu, _bus: &mut SystemBus) -> bool {
        self.regs.a = self.fetched_data;
        self.set_zero_negative(self.regs.a);
        true
    }

    fn ldx_fetched_data_finish(self: &mut Cpu, _bus: &mut SystemBus) -> bool {
        self.regs.x = self.fetched_data;
        self.set_zero_negative(self.regs.x);
        true
    }

    fn ldy_fetched_data_finish(self: &mut Cpu, _bus: &mut SystemBus) -> bool {
        self.regs.y = self.fetched_data;
        self.set_zero_negative(self.regs.y);
        true
    }

    fn lda_abs_indexed_x_optimistic(&mut self, bus: &mut SystemBus) -> bool {
        self.ld_abs_indexed_optimistic(bus, Register8::A, Register8::X)
    }

    fn lda_abs_indexed_y_optimistic(&mut self, bus: &mut SystemBus) -> bool {
        self.ld_abs_indexed_optimistic(bus, Register8::A, Register8::Y)
    }

    fn ldx_abs_indexed_y_optimistic(&mut self, bus: &mut SystemBus) -> bool {
        self.ld_abs_indexed_optimistic(bus, Register8::X, Register8::Y)
    }

    fn ldy_abs_indexed_x_optimistic(&mut self, bus: &mut SystemBus) -> bool {
        self.ld_abs_indexed_optimistic(bus, Register8::Y, Register8::X)
    }

    fn ld_abs_indexed_optimistic(
        &mut self,
        bus: &mut SystemBus,
        ld_reg: Register8,
        index_reg: Register8,
    ) -> bool {
        let (low_byte, carry) = (self.addr_abs as u8).overflowing_add(self.get_register(index_reg));
        let uncorrected_addr = (self.addr_abs & 0xFF00) | (low_byte as u16);
        self.fetched_data = bus.read_byte(uncorrected_addr);

        if !carry {
            // No page cross, so the instruction can finish without a fixup cycle.
            self.set_register(ld_reg, self.fetched_data);
            self.set_zero_negative(self.get_register(ld_reg));
            return true;
        }

        // PAGE CROSS: We need to fix the address high byte next cycle
        // Update addr_abs to the correct high byte for the fixup cycle
        self.addr_abs = uncorrected_addr.wrapping_add(0x0100);
        false
    }

    fn lda_addr_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.regs.a = bus.read_byte(self.addr_abs);
        self.set_zero_negative(self.regs.a);
        true
    }

    fn ldx_addr_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.regs.x = bus.read_byte(self.addr_abs);
        self.set_zero_negative(self.regs.x);
        true
    }

    fn ldy_addr_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.regs.y = bus.read_byte(self.addr_abs);
        self.set_zero_negative(self.regs.y);
        true
    }

    fn lda_indexed_x_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let index = self.regs.x as u16;
        let addr = (self.base_addr as u16 + index) % 0x0100;
        self.regs.a = bus.read_byte(addr);
        self.set_zero_negative(self.regs.a);
        true
    }

    fn ldx_indexed_y_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let index = self.regs.y as u16;
        let addr = (self.base_addr as u16 + index) % 0x0100;
        self.regs.x = bus.read_byte(addr);
        self.set_zero_negative(self.regs.x);
        true
    }

    fn ldy_indexed_x_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let index = self.regs.x as u16;
        let addr = (self.base_addr as u16 + index) % 0x0100;
        self.regs.y = bus.read_byte(addr);
        self.set_zero_negative(self.regs.y);
        true
    }

    fn sta_write_byte_abs(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        bus.write_byte(self.addr_abs, self.regs.a);
        true
    }

    fn stx_write_byte_abs(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        bus.write_byte(self.addr_abs, self.regs.x);
        true
    }

    fn sty_write_byte_abs(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        bus.write_byte(self.addr_abs, self.regs.y);
        true
    }

    fn sta_write_byte_zp(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        bus.write_byte(self.addr_abs, self.regs.a);
        true
    }

    fn stx_write_byte_zp(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        bus.write_byte(self.addr_abs, self.regs.x);
        true
    }

    fn sty_write_byte_zp(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        bus.write_byte(self.addr_abs, self.regs.y);
        true
    }

    fn sta_write_byte_abs_indexed_x(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let index = self.regs.x;
        let addr = (self.base_addr as u16 + index as u16) % 0x0100;
        bus.write_byte(addr, self.regs.a);
        true
    }

    fn stx_write_byte_abs_indexed_y(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let index = self.regs.y;
        let addr = (self.base_addr as u16 + index as u16) % 0x0100;
        bus.write_byte(addr, self.regs.x);
        true
    }

    fn sty_write_byte_abs_indexed_x(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let index = self.regs.x;
        let addr = (self.base_addr as u16 + index as u16) % 0x0100;
        bus.write_byte(addr, self.regs.y);
        true
    }

    fn st_abs_indexed_x_dummy_read(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let index = self.regs.x as u16;
        let base_addr = self.addr_abs;
        self.addr_abs = base_addr + index;
        bus.read_byte((base_addr & 0xFF00) | (self.addr_abs & 0x00FF));

        false
    }

    fn st_abs_indexed_y_dummy_read(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let index = self.regs.y as u16;
        let base_addr = self.addr_abs;
        self.addr_abs = base_addr + index;
        bus.read_byte((base_addr & 0xFF00) | (self.addr_abs & 0x00FF));

        false
    }

    fn adc_addr_abs_indexed_x_optimistic(&mut self, bus: &mut SystemBus) -> bool {
        self.adc_addr_abs_indexed_optimistic(bus, Register8::X)
    }

    fn adc_addr_abs_indexed_y_optimistic(&mut self, bus: &mut SystemBus) -> bool {
        self.adc_addr_abs_indexed_optimistic(bus, Register8::Y)
    }

    fn adc_addr_abs_indexed_optimistic(
        &mut self,
        bus: &mut SystemBus,
        index_reg: Register8,
    ) -> bool {
        let (low_byte, carry) = (self.addr_abs as u8).overflowing_add(self.get_register(index_reg));
        let uncorrected_addr = (self.addr_abs & 0xFF00) | (low_byte as u16);
        self.fetched_data = bus.read_byte(uncorrected_addr);

        if !carry {
            // No page cross, so the instruction can finish without a fixup cycle.
            self.add_value(self.fetched_data);
            return true;
        }

        // PAGE CROSS: We need to fix the address high byte next cycle
        // Update addr_abs to the correct high byte for the fixup cycle
        self.addr_abs = uncorrected_addr.wrapping_add(0x0100);
        false // Proceed to Cycle 5
    }

    fn adc_immediate(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.fetch_immediate(bus);
        self.add_value(self.fetched_data);
        true
    }

    fn adc_zero_page_indexed_x_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let index = self.regs.x;
        let addr = (self.base_addr as u16 + index as u16) % 0x0100;
        let value = bus.read_byte(addr);
        self.add_value(value);
        true
    }

    fn adc_addr_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let value = bus.read_byte(self.addr_abs);
        self.add_value(value);
        true
    }

    fn sbc_addr_abs_indexed_x_optimistic(&mut self, bus: &mut SystemBus) -> bool {
        self.sbc_addr_abs_indexed_optimistic(bus, Register8::X)
    }

    fn sbc_addr_abs_indexed_y_optimistic(&mut self, bus: &mut SystemBus) -> bool {
        self.sbc_addr_abs_indexed_optimistic(bus, Register8::Y)
    }

    fn sbc_addr_abs_indexed_optimistic(
        &mut self,
        bus: &mut SystemBus,
        index_reg: Register8,
    ) -> bool {
        let (low_byte, carry) = (self.addr_abs as u8).overflowing_add(self.get_register(index_reg));
        let uncorrected_addr = (self.addr_abs & 0xFF00) | (low_byte as u16);
        self.fetched_data = bus.read_byte(uncorrected_addr);

        if !carry {
            // No page cross, so the instruction can finish without a fixup cycle.
            self.sub_value(self.fetched_data);
            return true;
        }

        // PAGE CROSS: We need to fix the address high byte next cycle
        // Update addr_abs to the correct high byte for the fixup cycle
        self.addr_abs = uncorrected_addr.wrapping_add(0x0100);
        false // Proceed to Cycle 5
    }

    fn sbc_immediate(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.fetch_immediate(bus);
        self.sub_value(self.fetched_data);
        true
    }

    fn sbc_zero_page_indexed_x_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let index = self.regs.x;
        let addr = (self.base_addr as u16 + index as u16) % 0x0100;
        let value = bus.read_byte(addr);
        self.sub_value(value);
        true
    }

    fn sbc_addr_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let value = bus.read_byte(self.addr_abs);
        self.sub_value(value);
        true
    }

    fn and_addr_abs_indexed_x_optimistic(&mut self, bus: &mut SystemBus) -> bool {
        self.and_addr_abs_indexed_optimistic(bus, Register8::X)
    }

    fn and_addr_abs_indexed_y_optimistic(&mut self, bus: &mut SystemBus) -> bool {
        self.and_addr_abs_indexed_optimistic(bus, Register8::Y)
    }

    fn and_addr_abs_indexed_optimistic(
        &mut self,
        bus: &mut SystemBus,
        index_reg: Register8,
    ) -> bool {
        let (low_byte, carry) = (self.addr_abs as u8).overflowing_add(self.get_register(index_reg));
        let uncorrected_addr = (self.addr_abs & 0xFF00) | (low_byte as u16);
        self.fetched_data = bus.read_byte(uncorrected_addr);

        if !carry {
            // No page cross, so the instruction can finish without a fixup cycle.
            self.and_value(self.fetched_data);
            return true;
        }

        // PAGE CROSS: We need to fix the address high byte next cycle
        // Update addr_abs to the correct high byte for the fixup cycle
        self.addr_abs = uncorrected_addr.wrapping_add(0x0100);
        false // Proceed to Cycle 5
    }

    fn and_immediate(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.fetch_immediate(bus);
        self.and_value(self.fetched_data);
        true
    }

    fn and_zero_page_indexed_x_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let index = self.regs.x;
        let addr = (self.base_addr as u16 + index as u16) % 0x0100;
        let value = bus.read_byte(addr);
        self.and_value(value);
        true
    }

    fn and_addr_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let value = bus.read_byte(self.addr_abs);
        self.and_value(value);
        true
    }

    fn ora_addr_abs_indexed_x_optimistic(&mut self, bus: &mut SystemBus) -> bool {
        self.ora_addr_abs_indexed_optimistic(bus, Register8::X)
    }

    fn ora_addr_abs_indexed_y_optimistic(&mut self, bus: &mut SystemBus) -> bool {
        self.ora_addr_abs_indexed_optimistic(bus, Register8::Y)
    }

    fn ora_addr_abs_indexed_optimistic(
        &mut self,
        bus: &mut SystemBus,
        index_reg: Register8,
    ) -> bool {
        let (low_byte, carry) = (self.addr_abs as u8).overflowing_add(self.get_register(index_reg));
        let uncorrected_addr = (self.addr_abs & 0xFF00) | (low_byte as u16);
        self.fetched_data = bus.read_byte(uncorrected_addr);

        if !carry {
            // No page cross, so the instruction can finish without a fixup cycle.
            self.ora_value(self.fetched_data);
            return true;
        }

        // PAGE CROSS: We need to fix the address high byte next cycle
        // Update addr_abs to the correct high byte for the fixup cycle
        self.addr_abs = uncorrected_addr.wrapping_add(0x0100);
        false // Proceed to Cycle 5
    }

    fn ora_immediate(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.fetch_immediate(bus);
        self.ora_value(self.fetched_data);
        true
    }

    fn ora_zero_page_indexed_x_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let index = self.regs.x;
        let addr = (self.base_addr as u16 + index as u16) % 0x0100;
        let value = bus.read_byte(addr);
        self.ora_value(value);
        true
    }

    fn ora_addr_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let value = bus.read_byte(self.addr_abs);
        self.ora_value(value);
        true
    }

    fn eor_addr_abs_indexed_x_optimistic(&mut self, bus: &mut SystemBus) -> bool {
        self.eor_addr_abs_indexed_optimistic(bus, Register8::X)
    }

    fn eor_addr_abs_indexed_y_optimistic(&mut self, bus: &mut SystemBus) -> bool {
        self.eor_addr_abs_indexed_optimistic(bus, Register8::Y)
    }

    fn eor_addr_abs_indexed_optimistic(
        &mut self,
        bus: &mut SystemBus,
        index_reg: Register8,
    ) -> bool {
        let (low_byte, carry) = (self.addr_abs as u8).overflowing_add(self.get_register(index_reg));
        let uncorrected_addr = (self.addr_abs & 0xFF00) | (low_byte as u16);
        self.fetched_data = bus.read_byte(uncorrected_addr);

        if !carry {
            // No page cross, so the instruction can finish without a fixup cycle.
            self.eor_value(self.fetched_data);
            return true;
        }

        // PAGE CROSS: We need to fix the address high byte next cycle
        // Update addr_abs to the correct high byte for the fixup cycle
        self.addr_abs = uncorrected_addr.wrapping_add(0x0100);
        false // Proceed to Cycle 5
    }

    fn eor_immediate(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.fetch_immediate(bus);
        self.eor_value(self.fetched_data);
        true
    }

    fn eor_zero_page_indexed_x_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let index = self.regs.x;
        let addr = (self.base_addr as u16 + index as u16) % 0x0100;
        let value = bus.read_byte(addr);
        self.eor_value(value);
        true
    }

    fn eor_addr_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let value = bus.read_byte(self.addr_abs);
        self.eor_value(value);
        true
    }

    fn sec(&mut self, bus: &mut SystemBus) -> bool {
        self.dummy_read(bus);
        self.flags.c = true;
        true
    }

    fn clc(&mut self, bus: &mut SystemBus) -> bool {
        self.dummy_read(bus);
        self.flags.c = false;
        true
    }

    fn sei(&mut self, bus: &mut SystemBus) -> bool {
        self.dummy_read(bus);
        self.flags.i = true;
        true
    }

    fn cli(&mut self, bus: &mut SystemBus) -> bool {
        self.dummy_read(bus);
        self.flags.i = false;
        true
    }

    fn sed(&mut self, bus: &mut SystemBus) -> bool {
        self.dummy_read(bus);
        self.flags.d = true;
        true
    }

    fn cld(&mut self, bus: &mut SystemBus) -> bool {
        self.dummy_read(bus);
        self.flags.d = false;
        true
    }

    fn clv(&mut self, bus: &mut SystemBus) -> bool {
        self.dummy_read(bus);
        self.flags.v = false;
        true
    }

    fn cmp_addr_abs_indexed_x_optimistic(&mut self, bus: &mut SystemBus) -> bool {
        self.cmp_addr_abs_indexed_optimistic(bus, Register8::X)
    }

    fn cmp_addr_abs_indexed_y_optimistic(&mut self, bus: &mut SystemBus) -> bool {
        self.cmp_addr_abs_indexed_optimistic(bus, Register8::Y)
    }

    fn cmp_addr_abs_indexed_optimistic(
        &mut self,
        bus: &mut SystemBus,
        index_reg: Register8,
    ) -> bool {
        let (low_byte, carry) = (self.addr_abs as u8).overflowing_add(self.get_register(index_reg));
        let uncorrected_addr = (self.addr_abs & 0xFF00) | (low_byte as u16);
        self.fetched_data = bus.read_byte(uncorrected_addr);

        if !carry {
            // No page cross, so the instruction can finish without a fixup cycle.
            self.compare_value(self.fetched_data, self.regs.a);
            return true;
        }

        // PAGE CROSS: We need to fix the address high byte next cycle
        // Update addr_abs to the correct high byte for the fixup cycle
        self.addr_abs = uncorrected_addr.wrapping_add(0x0100);
        false // Proceed to Cycle 5
    }

    fn compare_value(&mut self, value: u8, reg_value: u8) {
        let result = reg_value.wrapping_sub(value);
        self.set_zero_negative(result);
        self.flags.c = value <= reg_value;
    }

    fn cmp_immediate(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.fetch_immediate(bus);
        self.compare_value(self.fetched_data, self.regs.a);
        true
    }

    fn cpx_immediate(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.fetch_immediate(bus);
        self.compare_value(self.fetched_data, self.regs.x);
        true
    }

    fn cpy_immediate(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.fetch_immediate(bus);
        self.compare_value(self.fetched_data, self.regs.y);
        true
    }

    fn cmp_zero_page_indexed_x_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let index = self.regs.x;
        let addr = (self.base_addr as u16 + index as u16) % 0x0100;
        let value = bus.read_byte(addr);
        self.compare_value(value, self.regs.a);
        true
    }

    fn cmp_addr_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let value = bus.read_byte(self.addr_abs);
        self.compare_value(value, self.regs.a);
        true
    }

    fn cpx_addr_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let value = bus.read_byte(self.addr_abs);
        self.compare_value(value, self.regs.x);
        true
    }

    fn cpy_addr_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let value = bus.read_byte(self.addr_abs);
        self.compare_value(value, self.regs.y);
        true
    }

    fn bit_addr_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let m = bus.read_byte(self.addr_abs);
        let a = self.regs.a;

        self.flags.n = (m & 0x80) != 0;
        self.flags.v = (m & 0x40) != 0;
        self.flags.z = (m & a) == 0;

        true
    }

    fn read_abs_addr_data(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.fetched_data = bus.read_byte(self.addr_abs);
        false
    }

    fn read_zero_page_indexed_x_data(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let index = self.regs.x;
        self.addr_abs = (self.base_addr as u16 + index as u16) % 0x0100;
        self.fetched_data = bus.read_byte(self.addr_abs);
        false
    }

    fn rmw_abs_indexed_x_dummy_read(&mut self, bus: &mut SystemBus) -> bool {
        let lo = self.addr_abs.wrapping_add(self.regs.x as u16) & 0x00FF;
        let uncorrected_addr = (self.addr_abs & 0xFF00) | lo;
        bus.read_byte(uncorrected_addr);
        false
    }

    fn read_abs_indexed_x_data(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let index = self.regs.x;
        self.addr_abs = self.addr_abs.wrapping_add(index as u16);
        self.fetched_data = bus.read_byte(self.addr_abs);
        false
    }

    fn addr_abs_fetched_data_dummy_write(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        bus.write_byte(self.addr_abs, self.fetched_data);
        false
    }

    fn inc_addr_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let value = self.fetched_data.wrapping_add(1);
        self.set_zero_negative(value);
        bus.write_byte(self.addr_abs, value);
        true
    }

    fn dec_addr_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let value = self.fetched_data.wrapping_sub(1);
        self.set_zero_negative(value);
        bus.write_byte(self.addr_abs, value);
        true
    }

    fn inx(&mut self, bus: &mut SystemBus) -> bool {
        self.dummy_read(bus);
        let val = self.regs.x.wrapping_add(1);
        self.set_zero_negative(val);
        self.regs.x = val;
        true
    }

    fn iny(&mut self, bus: &mut SystemBus) -> bool {
        self.dummy_read(bus);
        let val = self.regs.y.wrapping_add(1);
        self.set_zero_negative(val);
        self.regs.y = val;
        true
    }

    fn dex(&mut self, bus: &mut SystemBus) -> bool {
        self.dummy_read(bus);
        let val = self.regs.x.wrapping_sub(1);
        self.set_zero_negative(val);
        self.regs.x = val;
        true
    }

    fn dey(&mut self, bus: &mut SystemBus) -> bool {
        self.dummy_read(bus);
        let val = self.regs.y.wrapping_sub(1);
        self.set_zero_negative(val);
        self.regs.y = val;
        true
    }

    fn tax(&mut self, bus: &mut SystemBus) -> bool {
        self.dummy_read(bus);
        self.set_zero_negative(self.regs.a);
        self.regs.x = self.regs.a;
        true
    }

    fn txa(&mut self, bus: &mut SystemBus) -> bool {
        self.dummy_read(bus);
        self.set_zero_negative(self.regs.x);
        self.regs.a = self.regs.x;
        true
    }

    fn tay(&mut self, bus: &mut SystemBus) -> bool {
        self.dummy_read(bus);
        self.set_zero_negative(self.regs.a);
        self.regs.y = self.regs.a;
        true
    }

    fn tya(&mut self, bus: &mut SystemBus) -> bool {
        self.dummy_read(bus);
        self.set_zero_negative(self.regs.y);
        self.regs.a = self.regs.y;
        true
    }

    fn tsx(&mut self, bus: &mut SystemBus) -> bool {
        self.dummy_read(bus);
        self.set_zero_negative(self.regs.sp);
        self.regs.x = self.regs.sp;
        true
    }

    fn txs(&mut self, bus: &mut SystemBus) -> bool {
        self.dummy_read(bus);
        self.regs.sp = self.regs.x;
        true
    }

    fn dummy_stack_read(&mut self, bus: &mut SystemBus) -> bool {
        let _ = bus.read_byte(0x0100 | (self.regs.sp as u16));
        false
    }

    fn push_pc_high(&mut self, bus: &mut SystemBus) -> bool {
        self.push_byte(bus, (self.regs.pc >> 8) as u8);
        false
    }

    fn push_pc_low(&mut self, bus: &mut SystemBus) -> bool {
        self.push_byte(bus, (self.regs.pc & 0xFF) as u8);
        false
    }

    fn brk_push_status(&mut self, bus: &mut SystemBus) -> bool {
        let mut status = self.flags;
        status.b = true;
        status.e = true;
        self.push_byte(bus, status.into());
        self.flags.i = true;
        false
    }

    fn interrupt_push_status(&mut self, bus: &mut SystemBus) -> bool {
        let mut status = self.flags;
        status.b = false;
        status.e = true;
        self.push_byte(bus, status.into());
        false
    }

    fn pull_low(&mut self, bus: &mut SystemBus) -> bool {
        self.addr_abs = self.pull_byte(bus) as u16;
        false
    }

    fn pull_high(&mut self, bus: &mut SystemBus) -> bool {
        let lo = self.addr_abs;
        let hi = self.pull_byte(bus) as u16;
        self.addr_abs = (hi << 8) | lo;
        false
    }

    fn pull_status(&mut self, bus: &mut SystemBus) -> bool {
        self.flags = self.pull_byte(bus).into();
        self.flags.e = true;
        self.flags.b = false;
        false
    }

    fn jsr_finish(&mut self, bus: &mut SystemBus) -> bool {
        let addr_lo = self.addr_abs;
        let addr_hi = self.next_pc_byte(bus) as u16;
        self.regs.pc = (addr_hi << 8) | addr_lo;
        true
    }

    fn rts_finish(&mut self, _bus: &mut SystemBus) -> bool {
        self.regs.pc = self.addr_abs + 1;
        true
    }

    fn pha_finish(&mut self, bus: &mut SystemBus) -> bool {
        self.push_byte(bus, self.regs.a);
        true
    }

    fn inc_sp(&mut self, _bus: &mut SystemBus) -> bool {
        self.regs.sp = self.regs.sp.wrapping_add(1);
        false
    }

    fn pla_finish(&mut self, bus: &mut SystemBus) -> bool {
        let val = bus.read_byte(0x0100 | (self.regs.sp as u16));
        self.set_zero_negative(val);
        self.regs.a = val;
        true
    }

    fn php_finish(&mut self, bus: &mut SystemBus) -> bool {
        let mut status = self.flags;
        status.b = true;
        status.e = true;
        self.push_byte(bus, status.into());
        true
    }

    fn plp_finish(&mut self, bus: &mut SystemBus) -> bool {
        let val = bus.read_byte(0x0100 | (self.regs.sp as u16));
        let flags: u8 = self.flags.into();
        self.flags = ((val & 0xCF) | (flags & 0x30)).into();
        true
    }

    fn lsr_accumulator_finish(&mut self, _bus: &mut SystemBus) -> bool {
        self.flags.c = (self.regs.a & 0x01) != 0;
        self.regs.a = (self.regs.a >> 1) & 0x7F;
        self.set_zero_negative(self.regs.a);
        true
    }

    fn lsr_addr_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let value = (self.fetched_data >> 1) & 0x7F;
        self.set_zero_negative(value);
        self.flags.c = (self.fetched_data & 0x01) != 0;
        bus.write_byte(self.addr_abs, value);
        true
    }

    fn asl_accumulator_finish(&mut self, _bus: &mut SystemBus) -> bool {
        self.flags.c = (self.regs.a & 0x80) != 0;
        self.regs.a = (self.regs.a << 1) & 0xFE;
        self.set_zero_negative(self.regs.a);
        true
    }

    fn asl_addr_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.flags.c = (self.fetched_data & 0x80) != 0;
        let value = (self.fetched_data << 1) & 0xFE;
        self.set_zero_negative(value);
        bus.write_byte(self.addr_abs, value);
        true
    }

    fn ror_accumulator_finish(&mut self, _bus: &mut SystemBus) -> bool {
        let carry: u8 = if self.flags.c { 1 << 7 } else { 0 };
        self.flags.c = (self.regs.a & 0x01) != 0;
        self.regs.a = ((self.regs.a >> 1) & 0x7F) | carry;
        self.set_zero_negative(self.regs.a);
        true
    }

    fn ror_addr_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let carry: u8 = if self.flags.c { 1 << 7 } else { 0 };
        self.flags.c = (self.fetched_data & 0x01) != 0;
        let value = ((self.fetched_data >> 1) & 0x7F) | carry;
        self.set_zero_negative(value);
        bus.write_byte(self.addr_abs, value);
        true
    }

    fn rol_accumulator_finish(&mut self, _bus: &mut SystemBus) -> bool {
        let carry: u8 = self.flags.c.into();
        self.flags.c = (self.regs.a & 0x80) != 0;
        self.regs.a = ((self.regs.a << 1) & 0xFE) | carry;
        self.set_zero_negative(self.regs.a);
        true
    }

    fn rol_addr_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let carry: u8 = self.flags.c.into();
        self.flags.c = (self.fetched_data & 0x80) != 0;
        let value = ((self.fetched_data << 1) & 0xFE) | carry;
        self.set_zero_negative(value);
        bus.write_byte(self.addr_abs, value);
        true
    }

    fn read_brk_vector_low(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let vector_addr = if self.nmi_pended {
            NMI_VECTOR // Hijacked by NMI
        } else {
            BRK_VECTOR
        };
        self.temp_addr_low = bus.read_byte(vector_addr);
        false
    }

    fn brk_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let vector_addr = if self.nmi_pended {
            NMI_VECTOR // Hijacked by NMI
        } else {
            BRK_VECTOR
        };
        let hi = bus.read_byte(vector_addr + 1) as u16;
        self.regs.pc = (hi << 8) | (self.temp_addr_low as u16);

        // If hijacked by NMI, we need to clear it.
        if self.nmi_pended {
            self.nmi_pended = false;
        }

        true
    }

    fn read_nmi_vector_low(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.temp_addr_low = bus.read_byte(NMI_VECTOR);
        false
    }

    fn nmi_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let hi = bus.read_byte(NMI_VECTOR + 1) as u16;
        self.regs.pc = (hi << 8) | (self.temp_addr_low as u16);
        true
    }

    fn read_irq_vector_low(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.temp_addr_low = bus.read_byte(IRQ_VECTOR);
        false
    }

    fn irq_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let hi = bus.read_byte(IRQ_VECTOR + 1) as u16;
        self.regs.pc = (hi << 8) | (self.temp_addr_low as u16);
        true
    }

    fn rti_finish(self: &mut Cpu, _bus: &mut SystemBus) -> bool {
        self.regs.pc = self.addr_abs;
        true
    }

    fn nop_implied(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        bus.read_byte(self.regs.pc);
        true
    }

    fn jmp_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let hi = self.next_pc_byte(bus) as u16;
        self.regs.pc = (hi << 8) | self.addr_abs;
        true
    }

    fn jmp_indirect_read_pointer_low(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.fetched_data = bus.read_byte(self.addr_abs);
        false
    }

    fn jump_indirect_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
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

    opcodes[0xA9] = Some(Instruction {
        name: "LDA #Immediate",
        cycles: &[Cpu::fetch_immediate, Cpu::lda_fetched_data_finish],
    });

    opcodes[0xA5] = Some(Instruction {
        name: "LDA Zero Page",
        cycles: &[Cpu::fetch_abs_low, Cpu::lda_addr_abs_finish],
    });

    opcodes[0xB5] = Some(Instruction {
        name: "LDA Zero Page,X",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::lda_indexed_x_finish,
        ],
    });

    opcodes[0xAD] = Some(Instruction {
        name: "LDA Absolute",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::lda_addr_abs_finish,
        ],
    });

    opcodes[0xBD] = Some(Instruction {
        name: "LDA Absolute,X",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::lda_abs_indexed_x_optimistic,
            Cpu::lda_addr_abs_finish,
        ],
    });

    opcodes[0xB9] = Some(Instruction {
        name: "LDA Absolute,Y",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::lda_abs_indexed_y_optimistic,
            Cpu::lda_addr_abs_finish,
        ],
    });

    opcodes[0xA1] = Some(Instruction {
        name: "LDA (Indirect,X)",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_x_dummy_read_and_add,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::lda_addr_abs_finish,
        ],
    });

    opcodes[0xB1] = Some(Instruction {
        name: "LDA (Indirect),Y",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::lda_abs_indexed_y_optimistic,
            Cpu::lda_addr_abs_finish,
        ],
    });

    opcodes[0xA2] = Some(Instruction {
        name: "LDX #Immediate",
        cycles: &[Cpu::fetch_immediate, Cpu::ldx_fetched_data_finish],
    });

    opcodes[0xA6] = Some(Instruction {
        name: "LDX Zero Page",
        cycles: &[Cpu::fetch_abs_low, Cpu::ldx_addr_abs_finish],
    });

    opcodes[0xB6] = Some(Instruction {
        name: "LDX Zero Page,Y",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::ldx_indexed_y_finish,
        ],
    });

    opcodes[0xAE] = Some(Instruction {
        name: "LDX Absolute",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::ldx_addr_abs_finish,
        ],
    });

    opcodes[0xBE] = Some(Instruction {
        name: "LDX Absolute,Y",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::ldx_abs_indexed_y_optimistic,
            Cpu::ldx_addr_abs_finish,
        ],
    });

    opcodes[0xA0] = Some(Instruction {
        name: "LDY #Immediate",
        cycles: &[Cpu::fetch_immediate, Cpu::ldy_fetched_data_finish],
    });

    opcodes[0xA4] = Some(Instruction {
        name: "LDY Zero Page",
        cycles: &[Cpu::fetch_abs_low, Cpu::ldy_addr_abs_finish],
    });

    opcodes[0xB4] = Some(Instruction {
        name: "LDY Zero Page,X",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::ldy_indexed_x_finish,
        ],
    });

    opcodes[0xAC] = Some(Instruction {
        name: "LDY Absolute",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::ldy_addr_abs_finish,
        ],
    });

    opcodes[0xBC] = Some(Instruction {
        name: "LDY Absolute,X",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::ldy_abs_indexed_x_optimistic,
            Cpu::ldy_addr_abs_finish,
        ],
    });

    opcodes[0x85] = Some(Instruction {
        name: "STA Zero Page",
        cycles: &[Cpu::fetch_abs_low, Cpu::sta_write_byte_zp],
    });

    opcodes[0x95] = Some(Instruction {
        name: "STA Zero Page,X",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::sta_write_byte_abs_indexed_x,
        ],
    });

    opcodes[0x8D] = Some(Instruction {
        name: "STA Absolute",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::sta_write_byte_abs,
        ],
    });

    opcodes[0x9D] = Some(Instruction {
        name: "STA Absolute,X",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::st_abs_indexed_x_dummy_read,
            Cpu::sta_write_byte_abs,
        ],
    });

    opcodes[0x99] = Some(Instruction {
        name: "STA Absolute,Y",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::st_abs_indexed_y_dummy_read,
            Cpu::sta_write_byte_abs,
        ],
    });

    opcodes[0x81] = Some(Instruction {
        name: "STA (Indirect,X)",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_x_dummy_read_and_add,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::sta_write_byte_abs,
        ],
    });

    opcodes[0x91] = Some(Instruction {
        name: "STA (Indirect),Y",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::st_abs_indexed_y_dummy_read,
            Cpu::sta_write_byte_abs,
        ],
    });

    opcodes[0x86] = Some(Instruction {
        name: "STX Zero Page",
        cycles: &[Cpu::fetch_abs_low, Cpu::stx_write_byte_zp],
    });

    opcodes[0x96] = Some(Instruction {
        name: "STX Zero Page,Y",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::stx_write_byte_abs_indexed_y,
        ],
    });

    opcodes[0x8E] = Some(Instruction {
        name: "STX Absolute",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::stx_write_byte_abs,
        ],
    });

    opcodes[0x84] = Some(Instruction {
        name: "STY Zero Page",
        cycles: &[Cpu::fetch_abs_low, Cpu::sty_write_byte_zp],
    });

    opcodes[0x94] = Some(Instruction {
        name: "STY Zero Page,X",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::sty_write_byte_abs_indexed_x,
        ],
    });

    opcodes[0x8C] = Some(Instruction {
        name: "STY Absolute",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::sty_write_byte_abs,
        ],
    });

    opcodes[0x69] = Some(Instruction {
        name: "ADC #Immediate",
        cycles: &[Cpu::adc_immediate],
    });

    opcodes[0x65] = Some(Instruction {
        name: "ADC Zero Page",
        cycles: &[Cpu::fetch_abs_low, Cpu::adc_addr_abs_finish],
    });

    opcodes[0x75] = Some(Instruction {
        name: "ADC Zero Page,X",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::adc_zero_page_indexed_x_finish,
        ],
    });

    opcodes[0x6D] = Some(Instruction {
        name: "ADC Absolute",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::adc_addr_abs_finish,
        ],
    });

    opcodes[0x7D] = Some(Instruction {
        name: "ADC Absolute,X",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::adc_addr_abs_indexed_x_optimistic,
            Cpu::adc_addr_abs_finish,
        ],
    });

    opcodes[0x79] = Some(Instruction {
        name: "ADC Absolute,Y",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::adc_addr_abs_indexed_y_optimistic,
            Cpu::adc_addr_abs_finish,
        ],
    });

    opcodes[0x61] = Some(Instruction {
        name: "ADC (Indirect,X)",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_x_dummy_read_and_add,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::adc_addr_abs_finish,
        ],
    });

    opcodes[0x71] = Some(Instruction {
        name: "ADC (Indirect),Y",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::adc_addr_abs_indexed_y_optimistic,
            Cpu::adc_addr_abs_finish,
        ],
    });

    opcodes[0xE9] = Some(Instruction {
        name: "SBC #Immediate",
        cycles: &[Cpu::sbc_immediate],
    });

    opcodes[0xE5] = Some(Instruction {
        name: "SBC Zero Page",
        cycles: &[Cpu::fetch_abs_low, Cpu::sbc_addr_abs_finish],
    });

    opcodes[0xF5] = Some(Instruction {
        name: "SBC Zero Page,X",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::sbc_zero_page_indexed_x_finish,
        ],
    });

    opcodes[0xED] = Some(Instruction {
        name: "SBC Absolute",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::sbc_addr_abs_finish,
        ],
    });

    opcodes[0xFD] = Some(Instruction {
        name: "SBC Absolute,X",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::sbc_addr_abs_indexed_x_optimistic,
            Cpu::sbc_addr_abs_finish,
        ],
    });

    opcodes[0xF9] = Some(Instruction {
        name: "SBC Absolute,Y",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::sbc_addr_abs_indexed_y_optimistic,
            Cpu::sbc_addr_abs_finish,
        ],
    });

    opcodes[0xE1] = Some(Instruction {
        name: "SBC (Indirect,X)",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_x_dummy_read_and_add,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::sbc_addr_abs_finish,
        ],
    });

    opcodes[0xF1] = Some(Instruction {
        name: "SBC (Indirect),Y",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::sbc_addr_abs_indexed_y_optimistic,
            Cpu::sbc_addr_abs_finish,
        ],
    });

    opcodes[0x29] = Some(Instruction {
        name: "AND #Immediate",
        cycles: &[Cpu::and_immediate],
    });

    opcodes[0x25] = Some(Instruction {
        name: "AND Zero Page",
        cycles: &[Cpu::fetch_abs_low, Cpu::and_addr_abs_finish],
    });

    opcodes[0x35] = Some(Instruction {
        name: "AND Zero Page,X",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::and_zero_page_indexed_x_finish,
        ],
    });

    opcodes[0x2D] = Some(Instruction {
        name: "AND Absolute",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::and_addr_abs_finish,
        ],
    });

    opcodes[0x3D] = Some(Instruction {
        name: "AND Absolute,X",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::and_addr_abs_indexed_x_optimistic,
            Cpu::and_addr_abs_finish,
        ],
    });

    opcodes[0x39] = Some(Instruction {
        name: "AND Absolute,Y",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::and_addr_abs_indexed_y_optimistic,
            Cpu::and_addr_abs_finish,
        ],
    });

    opcodes[0x21] = Some(Instruction {
        name: "AND (Indirect,X)",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_x_dummy_read_and_add,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::and_addr_abs_finish,
        ],
    });

    opcodes[0x31] = Some(Instruction {
        name: "AND (Indirect),Y",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::and_addr_abs_indexed_y_optimistic,
            Cpu::and_addr_abs_finish,
        ],
    });

    opcodes[0x09] = Some(Instruction {
        name: "ORA #Immediate",
        cycles: &[Cpu::ora_immediate],
    });

    opcodes[0x05] = Some(Instruction {
        name: "ORA Zero Page",
        cycles: &[Cpu::fetch_abs_low, Cpu::ora_addr_abs_finish],
    });

    opcodes[0x15] = Some(Instruction {
        name: "ORA Zero Page,X",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::ora_zero_page_indexed_x_finish,
        ],
    });

    opcodes[0x0D] = Some(Instruction {
        name: "ORA Absolute",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::ora_addr_abs_finish,
        ],
    });

    opcodes[0x1D] = Some(Instruction {
        name: "ORA Absolute,X",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::ora_addr_abs_indexed_x_optimistic,
            Cpu::ora_addr_abs_finish,
        ],
    });

    opcodes[0x19] = Some(Instruction {
        name: "ORA Absolute,Y",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::ora_addr_abs_indexed_y_optimistic,
            Cpu::ora_addr_abs_finish,
        ],
    });

    opcodes[0x01] = Some(Instruction {
        name: "ORA (Indirect,X)",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_x_dummy_read_and_add,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::ora_addr_abs_finish,
        ],
    });

    opcodes[0x11] = Some(Instruction {
        name: "ORA (Indirect),Y",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::ora_addr_abs_indexed_y_optimistic,
            Cpu::ora_addr_abs_finish,
        ],
    });

    opcodes[0x49] = Some(Instruction {
        name: "EOR #Immediate",
        cycles: &[Cpu::eor_immediate],
    });

    opcodes[0x45] = Some(Instruction {
        name: "EOR Zero Page",
        cycles: &[Cpu::fetch_abs_low, Cpu::eor_addr_abs_finish],
    });

    opcodes[0x55] = Some(Instruction {
        name: "EOR Zero Page,X",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::eor_zero_page_indexed_x_finish,
        ],
    });

    opcodes[0x4D] = Some(Instruction {
        name: "EOR Absolute",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::eor_addr_abs_finish,
        ],
    });

    opcodes[0x5D] = Some(Instruction {
        name: "EOR Absolute,X",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::eor_addr_abs_indexed_x_optimistic,
            Cpu::eor_addr_abs_finish,
        ],
    });

    opcodes[0x59] = Some(Instruction {
        name: "EOR Absolute,Y",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::eor_addr_abs_indexed_y_optimistic,
            Cpu::eor_addr_abs_finish,
        ],
    });

    opcodes[0x41] = Some(Instruction {
        name: "EOR (Indirect,X)",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_x_dummy_read_and_add,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::eor_addr_abs_finish,
        ],
    });

    opcodes[0x51] = Some(Instruction {
        name: "EOR (Indirect),Y",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::eor_addr_abs_indexed_y_optimistic,
            Cpu::eor_addr_abs_finish,
        ],
    });

    opcodes[0x38] = Some(Instruction {
        name: "SEC",
        cycles: &[Cpu::sec],
    });

    opcodes[0x18] = Some(Instruction {
        name: "CLC",
        cycles: &[Cpu::clc],
    });

    opcodes[0x78] = Some(Instruction {
        name: "SEI",
        cycles: &[Cpu::sei],
    });

    opcodes[0x58] = Some(Instruction {
        name: "CLI",
        cycles: &[Cpu::cli],
    });

    opcodes[0xF8] = Some(Instruction {
        name: "SED",
        cycles: &[Cpu::sed],
    });

    opcodes[0xD8] = Some(Instruction {
        name: "CLD",
        cycles: &[Cpu::cld],
    });

    opcodes[0xB8] = Some(Instruction {
        name: "CLV",
        cycles: &[Cpu::clv],
    });

    opcodes[0x4C] = Some(Instruction {
        name: "JMP Absolute",
        cycles: &[Cpu::fetch_abs_low, Cpu::jmp_abs_finish],
    });

    opcodes[0x6C] = Some(Instruction {
        name: "JMP (Indirect)",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::jmp_indirect_read_pointer_low,
            Cpu::jump_indirect_finish,
        ],
    });

    opcodes[0x30] = Some(Instruction {
        name: "BMI",
        cycles: &[
            Cpu::fetch_rel_offset_and_check_branch_n,
            Cpu::take_branch,
            Cpu::branch_page_fixup,
        ],
    });

    opcodes[0x10] = Some(Instruction {
        name: "BPL",
        cycles: &[
            Cpu::fetch_rel_offset_and_check_branch_not_n,
            Cpu::take_branch,
            Cpu::branch_page_fixup,
        ],
    });

    opcodes[0x90] = Some(Instruction {
        name: "BCC",
        cycles: &[
            Cpu::fetch_rel_offset_and_check_branch_not_c,
            Cpu::take_branch,
            Cpu::branch_page_fixup,
        ],
    });

    opcodes[0xB0] = Some(Instruction {
        name: "BCS",
        cycles: &[
            Cpu::fetch_rel_offset_and_check_branch_c,
            Cpu::take_branch,
            Cpu::branch_page_fixup,
        ],
    });

    opcodes[0xF0] = Some(Instruction {
        name: "BEQ",
        cycles: &[
            Cpu::fetch_rel_offset_and_check_branch_z,
            Cpu::take_branch,
            Cpu::branch_page_fixup,
        ],
    });

    opcodes[0xD0] = Some(Instruction {
        name: "BNE",
        cycles: &[
            Cpu::fetch_rel_offset_and_check_branch_not_z,
            Cpu::take_branch,
            Cpu::branch_page_fixup,
        ],
    });

    opcodes[0x70] = Some(Instruction {
        name: "BVS",
        cycles: &[
            Cpu::fetch_rel_offset_and_check_branch_v,
            Cpu::take_branch,
            Cpu::branch_page_fixup,
        ],
    });

    opcodes[0x50] = Some(Instruction {
        name: "BVC",
        cycles: &[
            Cpu::fetch_rel_offset_and_check_branch_not_v,
            Cpu::take_branch,
            Cpu::branch_page_fixup,
        ],
    });

    opcodes[0xC9] = Some(Instruction {
        name: "CMP #Immediate",
        cycles: &[Cpu::cmp_immediate],
    });

    opcodes[0xC5] = Some(Instruction {
        name: "CMP Zero Page",
        cycles: &[Cpu::fetch_abs_low, Cpu::cmp_addr_abs_finish],
    });

    opcodes[0xD5] = Some(Instruction {
        name: "CMP Zero Page,X",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::cmp_zero_page_indexed_x_finish,
        ],
    });

    opcodes[0xCD] = Some(Instruction {
        name: "CMP Absolute",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::cmp_addr_abs_finish,
        ],
    });

    opcodes[0xDD] = Some(Instruction {
        name: "CMP Absolute,X",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::cmp_addr_abs_indexed_x_optimistic,
            Cpu::cmp_addr_abs_finish,
        ],
    });

    opcodes[0xD9] = Some(Instruction {
        name: "CMP Absolute,Y",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::cmp_addr_abs_indexed_y_optimistic,
            Cpu::cmp_addr_abs_finish,
        ],
    });

    opcodes[0xC1] = Some(Instruction {
        name: "CMP (Indirect,X)",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_x_dummy_read_and_add,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::cmp_addr_abs_finish,
        ],
    });

    opcodes[0xD1] = Some(Instruction {
        name: "CMP (Indirect),Y",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::cmp_addr_abs_indexed_y_optimistic,
            Cpu::cmp_addr_abs_finish,
        ],
    });

    opcodes[0xE0] = Some(Instruction {
        name: "CPX #Immediate",
        cycles: &[Cpu::cpx_immediate],
    });

    opcodes[0xE4] = Some(Instruction {
        name: "CPX Zero Page",
        cycles: &[Cpu::fetch_abs_low, Cpu::cpx_addr_abs_finish],
    });

    opcodes[0xEC] = Some(Instruction {
        name: "CPX Absolute",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::cpx_addr_abs_finish,
        ],
    });

    opcodes[0xC0] = Some(Instruction {
        name: "CPY #Immediate",
        cycles: &[Cpu::cpy_immediate],
    });

    opcodes[0xC4] = Some(Instruction {
        name: "CPY Zero Page",
        cycles: &[Cpu::fetch_abs_low, Cpu::cpy_addr_abs_finish],
    });

    opcodes[0xCC] = Some(Instruction {
        name: "CPY Absolute",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::cpy_addr_abs_finish,
        ],
    });

    opcodes[0x24] = Some(Instruction {
        name: "BIT Zero Page",
        cycles: &[Cpu::fetch_abs_low, Cpu::bit_addr_abs_finish],
    });

    opcodes[0x2C] = Some(Instruction {
        name: "BIT Absolute",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::bit_addr_abs_finish,
        ],
    });

    opcodes[0xE6] = Some(Instruction {
        name: "INC Zero Page",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::inc_addr_abs_finish,
        ],
    });

    opcodes[0xF6] = Some(Instruction {
        name: "INC Zero Page,X",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::read_zero_page_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::inc_addr_abs_finish,
        ],
    });

    opcodes[0xEE] = Some(Instruction {
        name: "INC Absolute",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::inc_addr_abs_finish,
        ],
    });

    opcodes[0xFE] = Some(Instruction {
        name: "INC Absolute,X",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::rmw_abs_indexed_x_dummy_read,
            Cpu::read_abs_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::inc_addr_abs_finish,
        ],
    });

    opcodes[0xC6] = Some(Instruction {
        name: "DEC Zero Page",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::dec_addr_abs_finish,
        ],
    });

    opcodes[0xD6] = Some(Instruction {
        name: "DEC Zero Page,X",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::read_zero_page_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::dec_addr_abs_finish,
        ],
    });

    opcodes[0xCE] = Some(Instruction {
        name: "DEC Absolute",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::dec_addr_abs_finish,
        ],
    });

    opcodes[0xDE] = Some(Instruction {
        name: "DEC Absolute,X",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::rmw_abs_indexed_x_dummy_read,
            Cpu::read_abs_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::dec_addr_abs_finish,
        ],
    });

    opcodes[0xE8] = Some(Instruction {
        name: "INX",
        cycles: &[Cpu::inx],
    });

    opcodes[0xC8] = Some(Instruction {
        name: "INY",
        cycles: &[Cpu::iny],
    });

    opcodes[0xCA] = Some(Instruction {
        name: "DEX",
        cycles: &[Cpu::dex],
    });

    opcodes[0x88] = Some(Instruction {
        name: "DEY",
        cycles: &[Cpu::dey],
    });

    opcodes[0xAA] = Some(Instruction {
        name: "TAX",
        cycles: &[Cpu::tax],
    });

    opcodes[0x8A] = Some(Instruction {
        name: "TXA",
        cycles: &[Cpu::txa],
    });

    opcodes[0xA8] = Some(Instruction {
        name: "TAY",
        cycles: &[Cpu::tay],
    });

    opcodes[0x98] = Some(Instruction {
        name: "TYA",
        cycles: &[Cpu::tya],
    });

    opcodes[0xBA] = Some(Instruction {
        name: "TSX",
        cycles: &[Cpu::tsx],
    });

    opcodes[0x9A] = Some(Instruction {
        name: "TXS",
        cycles: &[Cpu::txs],
    });

    opcodes[0x20] = Some(Instruction {
        name: "JSR",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::dummy_stack_read,
            Cpu::push_pc_high,
            Cpu::push_pc_low,
            Cpu::jsr_finish,
        ],
    });

    opcodes[0x60] = Some(Instruction {
        name: "RTS",
        cycles: &[
            Cpu::dummy_read,
            Cpu::pull_low,
            Cpu::pull_high,
            Cpu::dummy_read,
            Cpu::rts_finish,
        ],
    });

    opcodes[0x48] = Some(Instruction {
        name: "PHA",
        cycles: &[Cpu::dummy_read, Cpu::pha_finish],
    });

    opcodes[0x68] = Some(Instruction {
        name: "PLA",
        cycles: &[Cpu::dummy_read, Cpu::inc_sp, Cpu::pla_finish],
    });

    opcodes[0x08] = Some(Instruction {
        name: "PHP",
        cycles: &[Cpu::dummy_read, Cpu::php_finish],
    });

    opcodes[0x28] = Some(Instruction {
        name: "PLP",
        cycles: &[Cpu::dummy_read, Cpu::inc_sp, Cpu::plp_finish],
    });

    opcodes[0x4A] = Some(Instruction {
        name: "LSR Accumulator",
        cycles: &[Cpu::dummy_read, Cpu::lsr_accumulator_finish],
    });

    opcodes[0x46] = Some(Instruction {
        name: "LSR Zero Page",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::lsr_addr_abs_finish,
        ],
    });

    opcodes[0x56] = Some(Instruction {
        name: "LSR Zero Page,X",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::read_zero_page_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::lsr_addr_abs_finish,
        ],
    });

    opcodes[0x4E] = Some(Instruction {
        name: "LSR Absolute",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::lsr_addr_abs_finish,
        ],
    });

    opcodes[0x5E] = Some(Instruction {
        name: "LSR Absolute,X",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::rmw_abs_indexed_x_dummy_read,
            Cpu::read_abs_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::lsr_addr_abs_finish,
        ],
    });

    opcodes[0x0A] = Some(Instruction {
        name: "ASL Accumulator",
        cycles: &[Cpu::dummy_read, Cpu::asl_accumulator_finish],
    });

    opcodes[0x06] = Some(Instruction {
        name: "ASL Zero Page",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::asl_addr_abs_finish,
        ],
    });

    opcodes[0x16] = Some(Instruction {
        name: "ASL Zero Page,X",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::read_zero_page_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::asl_addr_abs_finish,
        ],
    });

    opcodes[0x0E] = Some(Instruction {
        name: "ASL Absolute",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::asl_addr_abs_finish,
        ],
    });

    opcodes[0x1E] = Some(Instruction {
        name: "ASL Absolute,X",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::rmw_abs_indexed_x_dummy_read,
            Cpu::read_abs_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::asl_addr_abs_finish,
        ],
    });

    opcodes[0x6A] = Some(Instruction {
        name: "ROR Accumulator",
        cycles: &[Cpu::dummy_read, Cpu::ror_accumulator_finish],
    });

    opcodes[0x66] = Some(Instruction {
        name: "ROR Zero Page",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::ror_addr_abs_finish,
        ],
    });

    opcodes[0x76] = Some(Instruction {
        name: "ROR Zero Page,X",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::read_zero_page_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::ror_addr_abs_finish,
        ],
    });

    opcodes[0x6E] = Some(Instruction {
        name: "ROR Absolute",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::ror_addr_abs_finish,
        ],
    });

    opcodes[0x7E] = Some(Instruction {
        name: "ROR Absolute,X",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::rmw_abs_indexed_x_dummy_read,
            Cpu::read_abs_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::ror_addr_abs_finish,
        ],
    });

    opcodes[0x2A] = Some(Instruction {
        name: "ROL Accumulator",
        cycles: &[Cpu::dummy_read, Cpu::rol_accumulator_finish],
    });

    opcodes[0x26] = Some(Instruction {
        name: "ROL Zero Page",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::rol_addr_abs_finish,
        ],
    });

    opcodes[0x36] = Some(Instruction {
        name: "ROL Zero Page,X",
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::read_zero_page_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::rol_addr_abs_finish,
        ],
    });

    opcodes[0x2E] = Some(Instruction {
        name: "ROL Absolute",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::rol_addr_abs_finish,
        ],
    });

    opcodes[0x3E] = Some(Instruction {
        name: "ROL Absolute,X",
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::rmw_abs_indexed_x_dummy_read,
            Cpu::read_abs_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::rol_addr_abs_finish,
        ],
    });

    opcodes[0x00] = Some(Instruction {
        name: "BRK",
        cycles: &[
            Cpu::dummy_fetch_and_inc_pc,
            Cpu::push_pc_high,
            Cpu::push_pc_low,
            Cpu::brk_push_status,
            Cpu::read_brk_vector_low,
            Cpu::brk_finish,
        ],
    });

    opcodes[0x40] = Some(Instruction {
        name: "RTI",
        cycles: &[
            Cpu::dummy_read,
            Cpu::pull_status,
            Cpu::pull_low,
            Cpu::pull_high,
            Cpu::rti_finish,
        ],
    });

    opcodes[0xEA] = Some(Instruction {
        name: "NOP Implied",
        cycles: &[Cpu::nop_implied],
    });

    // TODO: Unofficial opcodes below.

    opcodes
};

pub const NMI_INTERRUPT: Instruction = Instruction {
    name: "NMI",
    cycles: &[
        Cpu::dummy_read,
        Cpu::push_pc_high,
        Cpu::push_pc_low,
        Cpu::interrupt_push_status,
        Cpu::read_nmi_vector_low,
        Cpu::nmi_finish,
    ],
};

pub const IRQ_INTERRUPT: Instruction = Instruction {
    name: "IRQ",
    cycles: &[
        Cpu::dummy_read,
        Cpu::push_pc_high,
        Cpu::push_pc_low,
        Cpu::interrupt_push_status,
        Cpu::read_irq_vector_low,
        Cpu::irq_finish,
    ],
};
