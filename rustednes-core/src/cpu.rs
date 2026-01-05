use crate::system_bus::SystemBus;

use serde_derive::{Deserialize, Serialize};

use std::fmt;
use std::fmt::{Debug, Formatter};

pub type CycleFn = fn(&mut Cpu, bus: &mut SystemBus) -> bool;

#[derive(Copy, Clone)]
#[allow(dead_code)]
pub struct Instruction {
    name: &'static str,
    length: u8,
    mode: AddressMode,
    official: bool,
    cycles: &'static [CycleFn],
}

impl Instruction {
    pub fn nestest_asm_op(&self, cpu: &Cpu, bus: &SystemBus) -> String {
        let ops = [
            bus.peek_byte(cpu.regs.pc + 1),
            bus.peek_byte(cpu.regs.pc + 2),
        ];

        match self.mode {
            AddressMode::Immediate => format!("#${:02X}", ops[0]),

            AddressMode::ZeroPage => {
                let addr = ops[0];
                let val = bus.peek_byte(addr as u16);
                format!("${:02X} = {:02X}", addr, val)
            }

            AddressMode::ZeroPageIndexed(reg) => {
                let addr = ops[0];
                let reg_val = cpu.get_register(reg);
                let target = addr.wrapping_add(reg_val);
                let val = bus.peek_byte(target as u16);
                format!("${:02X},{:?} @ {:02X} = {:02X}", addr, reg, target, val)
            }

            AddressMode::Absolute => {
                let addr = u16::from_le_bytes([ops[0], ops[1]]);
                if self.name == "JMP" || self.name == "JSR" {
                    format!("${:04X}", addr)
                } else {
                    let val = bus.peek_byte(addr);
                    format!("${:04X} = {:02X}", addr, val)
                }
            }

            AddressMode::AbsoluteIndexed(reg) => {
                let base_addr = u16::from_le_bytes([ops[0], ops[1]]);
                let reg_val = cpu.get_register(reg) as u16;
                let target = base_addr.wrapping_add(reg_val);
                let val = bus.peek_byte(target);
                format!(
                    "${:04X},{:?} @ {:04X} = {:02X}",
                    base_addr, reg, target, val
                )
            }

            AddressMode::Indirect => {
                let ptr = u16::from_le_bytes([ops[0], ops[1]]);
                // 6502 JMP Indirect bug: if ptr is $xxFF, it wraps to $xx00 for the high byte.
                let hi_ptr = if (ptr & 0x00FF) == 0x00FF {
                    ptr & 0xFF00
                } else {
                    ptr + 1
                };
                let target = u16::from_le_bytes([bus.peek_byte(ptr), bus.peek_byte(hi_ptr)]);
                format!("(${:04X}) = {:04X}", ptr, target)
            }

            AddressMode::IndexedIndirect(reg) => {
                let base = ops[0];
                let ptr = base.wrapping_add(cpu.get_register(reg));
                let lo = bus.peek_byte(ptr as u16);
                let hi = bus.peek_byte(ptr.wrapping_add(1) as u16);
                let target = u16::from_le_bytes([lo, hi]);
                let val = bus.peek_byte(target);
                format!(
                    "(${:02X},X) @ {:02X} = {:04X} = {:02X}",
                    base, ptr, target, val
                )
            }

            AddressMode::IndirectIndexed(reg) => {
                let ptr = ops[0];
                let lo = bus.peek_byte(ptr as u16);
                let hi = bus.peek_byte(ptr.wrapping_add(1) as u16);
                let base_target = u16::from_le_bytes([lo, hi]);
                let target = base_target.wrapping_add(cpu.get_register(reg) as u16);
                let val = bus.peek_byte(target);
                format!(
                    "(${:02X}),Y = {:04X} @ {:04X} = {:02X}",
                    ptr, base_target, target, val
                )
            }

            AddressMode::Relative => {
                let offset = ops[0] as i8;
                let target = cpu.regs.pc.wrapping_add(2).wrapping_add(offset as u16);
                format!("${:04X}", target)
            }

            AddressMode::Accumulator => "A".to_string(),
            AddressMode::Implied => "".to_string(),
        }
    }
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
    Accumulator,
    Implied,
    Relative,
    Indirect,
}

#[derive(Default)]
pub struct Cpu {
    stall_cycles: u8,
    regs: Regs,
    flags: Flags,
    instruction: Option<Instruction>,
    nmi_line_low: bool,
    nmi_pended: bool,
    pub irq_line_low: bool,
    cycles_total: u64,

    // Temporary mid-instruction data to be able to run one cycle at a time.
    opcode: u8,
    cycle: u8,
    addr_abs: u16,
    temp_addr_low: u8,
    base_addr: u8,
    rel_offset: i8,
    fetched_data: u8,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct State {
    pub stall_cycles: u8,
    pub regs: Regs,
    pub flags: Flags,
    pub nmi_line_low: bool,
    pub nmi_pended: bool,
    pub irq_line_low: bool,
    pub cycles_total: u64,
    pub opcode: u8,
    pub cycle: u8,
    pub addr_abs: u16,
    pub temp_addr_low: u8,
    pub base_addr: u8,
    pub rel_offset: i8,
    pub fetched_data: u8,
    pub active_interrupt: Option<Interrupt>,
}

#[derive(Debug, Deserialize, Serialize)]
pub enum Interrupt {
    Nmi,
    Irq,
}

impl Cpu {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn get_state(&self) -> State {
        let active_interrupt = self
            .instruction
            .map(|inst| match inst.name {
                "NMI" => Some(Interrupt::Nmi),
                "IRQ" => Some(Interrupt::Irq),
                _ => None,
            })
            .flatten();

        State {
            stall_cycles: self.stall_cycles,
            regs: self.regs,
            flags: self.flags,
            nmi_line_low: self.nmi_line_low,
            nmi_pended: self.nmi_pended,
            irq_line_low: self.irq_line_low,
            cycles_total: self.cycles_total,
            opcode: self.opcode,
            cycle: self.cycle,
            addr_abs: self.addr_abs,
            temp_addr_low: self.temp_addr_low,
            base_addr: self.base_addr,
            rel_offset: self.rel_offset,
            fetched_data: self.fetched_data,
            active_interrupt,
        }
    }

    pub fn apply_state(&mut self, state: &State) {
        self.stall_cycles = state.stall_cycles;
        self.regs = state.regs;
        self.flags = state.flags;
        self.nmi_line_low = state.nmi_line_low;
        self.nmi_pended = state.nmi_pended;
        self.irq_line_low = state.irq_line_low;
        self.cycles_total = state.cycles_total;
        self.opcode = state.opcode;
        self.cycle = state.cycle;
        self.addr_abs = state.addr_abs;
        self.temp_addr_low = state.temp_addr_low;
        self.base_addr = state.base_addr;
        self.rel_offset = state.rel_offset;
        self.fetched_data = state.fetched_data;

        if let Some(interrupt) = &state.active_interrupt {
            match interrupt {
                Interrupt::Nmi => self.instruction = Some(NMI_INTERRUPT),
                Interrupt::Irq => self.instruction = Some(IRQ_INTERRUPT),
            }
        } else {
            self.instruction = OPCODES[self.opcode as usize];
        }
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
    }

    pub fn initialize_nestest(&mut self) {
        self.regs.pc = 0xC000;
        self.regs.sp = 0xFD;
        self.regs.a = 0x00;
        self.regs.x = 0x00;
        self.regs.y = 0x00;
        self.flags = 0x24.into();
        self.cycles_total = 7;
    }

    // Run a single CPU cycle. Returns whether an instruction completed.
    pub fn tick(&mut self, bus: &mut SystemBus) -> bool {
        if self.stall_cycles > 0 {
            self.stall_cycles -= 1;
            return false;
        }

        if self.cycle == 0 {
            self.cycle = 1;
            self.cycles_total += 1;

            if self.instruction.is_none() {
                // Fetch new instruction.
                self.opcode = self.next_pc_byte(bus);
                self.instruction = OPCODES[self.opcode as usize];
            }

            return false;
        }

        // Continue in-progress instruction.
        let instr = match self.instruction {
            Some(instr) => instr,
            None => panic!(
                "Unimplemented opcode: {:#04x}, pc: {:#06X}",
                self.opcode, self.regs.pc
            ),
        };

        let cycle_fn = instr.cycles[self.cycle as usize - 1];
        let completed = cycle_fn(self, bus);

        self.cycle += 1;
        self.cycles_total += 1;

        if completed || self.cycle as usize > instr.cycles.len() {
            self.cycle = 0;
            self.poll_interrupts();
            true
        } else {
            false
        }
    }

    pub fn trace(&self, bus: &SystemBus) -> String {
        let opcode = bus.peek_byte(self.regs.pc);
        let instruction = match OPCODES[opcode as usize] {
            Some(instruction) => instruction,
            None => panic!("Unimplemented opcode: {:#04x}", opcode),
        };

        let mut hex_extra = Vec::new();
        for i in 1..instruction.length {
            hex_extra.push(bus.peek_byte(self.regs.pc + i as u16));
        }

        let hex_extra = hex_extra
            .iter()
            .map(|z| format!("{:02X}", z))
            .collect::<Vec<String>>()
            .join(" ");

        let asm_op = instruction.nestest_asm_op(self, bus);

        format!(
            "{:04X}  {:02X} {:<5} {}{:<3} {:<27} A:{:02X} X:{:02X} Y:{:02X} P:{:02X} SP:{:02X} PPU:{:>3},{:>3} CYC:{}",
            self.regs.pc,
            opcode,
            hex_extra,
            if instruction.official { ' ' } else { '*' },
            instruction.name,
            asm_op,
            self.regs.a,
            self.regs.x,
            self.regs.y,
            u8::from(self.flags),
            self.regs.sp,
            bus.ppu_scanline(),
            bus.ppu_scanline_cycle(),
            self.cycles_total,
        )
    }

    fn poll_interrupts(&mut self) {
        if self.nmi_pended {
            self.instruction = Some(NMI_INTERRUPT);
        } else if self.irq_line_low && !self.flags.i {
            self.instruction = Some(IRQ_INTERRUPT);
        } else {
            // No interrupt? Then we proceed to fetch a normal opcode.
            // We set instruction to None (or a Fetch state) so Cycle 0
            // knows to read from PC.
            self.instruction = None;
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

    ///////////////
    // Interrupts
    ///////////////

    // Request NMI to be run on next instruction fetch attempt.
    pub fn set_nmi_line(&mut self, is_low: bool) {
        if !self.nmi_line_low && is_low {
            self.nmi_pended = true;
        }
        self.nmi_line_low = is_low;
    }

    // Request IRQ to be run on next instruction fetch attempt.
    pub fn set_irq_line_low(&mut self, val: bool) {
        self.irq_line_low = val;
    }

    ///////////////////////////////////////
    // Official Instruction Cycle Functions
    ///////////////////////////////////////

    fn fetch_rel_offset_and_check_branch_n(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.rel_offset = self.next_pc_byte(bus) as i8;
        !self.flags.n
    }

    fn fetch_rel_offset_and_check_branch_not_n(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.rel_offset = self.next_pc_byte(bus) as i8;
        self.flags.n
    }

    fn fetch_rel_offset_and_check_branch_c(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.rel_offset = self.next_pc_byte(bus) as i8;
        !self.flags.c
    }

    fn fetch_rel_offset_and_check_branch_not_c(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.rel_offset = self.next_pc_byte(bus) as i8;
        self.flags.c
    }

    fn fetch_rel_offset_and_check_branch_z(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.rel_offset = self.next_pc_byte(bus) as i8;
        !self.flags.z
    }

    fn fetch_rel_offset_and_check_branch_not_z(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.rel_offset = self.next_pc_byte(bus) as i8;
        self.flags.z
    }

    fn fetch_rel_offset_and_check_branch_v(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.rel_offset = self.next_pc_byte(bus) as i8;
        !self.flags.v
    }

    fn fetch_rel_offset_and_check_branch_not_v(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.rel_offset = self.next_pc_byte(bus) as i8;
        self.flags.v
    }

    fn take_branch(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let old_pc = self.regs.pc;
        let new_pc = self.regs.pc.wrapping_add_signed(self.rel_offset as i16);

        // Dummy fetch using using old high byte and new low byte of the pc.
        let dummy_addr = (old_pc & 0xFF00) | (new_pc & 0x00FF);
        let _ = bus.read_byte(dummy_addr);

        self.regs.pc = new_pc;

        // If not crossing a page, this is the last cycle.
        if (old_pc & 0xFF00) == (new_pc & 0xFF00) {
            return true;
        }

        // When there is a page cross, there is an extra cycle to fix up the page.
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

    fn lda_immediate(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.fetch_immediate(bus);
        self.regs.a = self.fetched_data;
        self.set_zero_negative(self.regs.a);
        true
    }

    fn ldx_immediate(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.fetch_immediate(bus);
        self.regs.x = self.fetched_data;
        self.set_zero_negative(self.regs.x);
        true
    }

    fn ldy_immediate(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.fetch_immediate(bus);
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

    #[inline(always)]
    // Returns true if page crossed.
    fn indexed_optimistic_fetch(&mut self, bus: &mut SystemBus, index: u8) -> bool {
        let speculative_addr =
            (self.addr_abs & 0xFF00) | ((self.addr_abs as u8).wrapping_add(index) as u16);
        self.addr_abs = self.addr_abs.wrapping_add(index as u16);
        self.fetched_data = bus.read_byte(speculative_addr);

        (speculative_addr & 0xFF00) != (self.addr_abs & 0xFF00)
    }

    #[inline(always)]
    fn ld_abs_indexed_optimistic(
        &mut self,
        bus: &mut SystemBus,
        ld_reg: Register8,
        index_reg: Register8,
    ) -> bool {
        let index = self.get_register(index_reg);
        if !self.indexed_optimistic_fetch(bus, index) {
            // No page cross, so the instruction can finish without an extra cycle.
            self.set_register(ld_reg, self.fetched_data);
            self.set_zero_negative(self.get_register(ld_reg));
            true
        } else {
            // Page cross. We need  an extra cycle
            false
        }
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
        let index = self.get_register(index_reg);
        if !self.indexed_optimistic_fetch(bus, index) {
            // No page cross, so the instruction can finish without an extra cycle.
            self.add_value(self.fetched_data);
            true
        } else {
            // Page cross. We need  an extra cycle
            false
        }
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
        let index = self.get_register(index_reg);
        if !self.indexed_optimistic_fetch(bus, index) {
            // No page cross, so the instruction can finish without an extra cycle.
            self.sub_value(self.fetched_data);
            true
        } else {
            // Page cross. We need  an extra cycle
            false
        }
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

    #[inline(always)]
    fn and_addr_abs_indexed_optimistic(
        &mut self,
        bus: &mut SystemBus,
        index_reg: Register8,
    ) -> bool {
        let index = self.get_register(index_reg);
        if !self.indexed_optimistic_fetch(bus, index) {
            // No page cross, so the instruction can finish without an extra cycle.
            self.and_value(self.fetched_data);
            true
        } else {
            // Page cross. We need  an extra cycle
            false
        }
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

    #[inline(always)]
    fn ora_addr_abs_indexed_optimistic(
        &mut self,
        bus: &mut SystemBus,
        index_reg: Register8,
    ) -> bool {
        let index = self.get_register(index_reg);
        if !self.indexed_optimistic_fetch(bus, index) {
            // No page cross, so the instruction can finish without an extra cycle.
            self.ora_value(self.fetched_data);
            true
        } else {
            // Page cross. We need  an extra cycle
            false
        }
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
        let index = self.get_register(index_reg);
        if !self.indexed_optimistic_fetch(bus, index) {
            // No page cross, so the instruction can finish without an extra cycle.
            self.eor_value(self.fetched_data);
            true
        } else {
            // Page cross. We need  an extra cycle
            false
        }
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
        let index = self.get_register(index_reg);
        if !self.indexed_optimistic_fetch(bus, index) {
            // No page cross, so the instruction can finish without an extra cycle.
            self.compare_value(self.fetched_data, self.regs.a);
            true
        } else {
            // Page cross. We need  an extra cycle
            false
        }
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

    fn rmw_abs_indexed_y_dummy_read(&mut self, bus: &mut SystemBus) -> bool {
        let lo = self.addr_abs.wrapping_add(self.regs.y as u16) & 0x00FF;
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

    fn read_abs_indexed_y_data(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let index = self.regs.y;
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
        self.nmi_pended = false;
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

    ///////////////////////////////////////
    // Unofficial Instruction Cycle Functions
    ///////////////////////////////////////

    fn isb_addr_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let value = self.fetched_data.wrapping_add(1);
        bus.write_byte(self.addr_abs, value);
        self.sub_value(value);
        true
    }

    fn skb(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        bus.read_byte(self.regs.pc);
        self.regs.pc = self.regs.pc.wrapping_add(1);
        true
    }

    fn alr(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let value = self.next_pc_byte(bus);
        self.and_value(value);
        self.lsr_accumulator_finish(bus);
        true
    }

    fn anc(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let value = self.next_pc_byte(bus);
        self.and_value(value);
        self.flags.c = self.flags.n;
        true
    }

    fn arr(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let imm = self.next_pc_byte(bus);
        self.regs.a = ((self.flags.c as u8) << 7) | ((self.regs.a & imm) >> 1);

        let a = self.regs.a;
        self.set_zero_negative(a);
        self.flags.c = (a & 0x40) != 0;
        self.flags.v = ((a ^ (a << 1)) & 0x40) != 0;
        true
    }

    fn axs(&mut self, bus: &mut SystemBus) -> bool {
        let imm = self.next_pc_byte(bus);
        let a_and_x = self.regs.a & self.regs.x;
        let result = (a_and_x).wrapping_sub(imm);
        self.set_zero_negative(result);
        self.flags.c = imm <= a_and_x;
        self.regs.x = result;
        true
    }

    fn lax_abs_indexed_y_optimistic(&mut self, bus: &mut SystemBus) -> bool {
        let completed = self.ld_abs_indexed_optimistic(bus, Register8::A, Register8::Y);
        self.regs.x = self.regs.a;
        completed
    }

    fn lax_immediate(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.lda_immediate(bus);
        self.regs.x = self.regs.a;
        true
    }

    fn lax_addr_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.lda_addr_abs_finish(bus);
        self.regs.x = self.regs.a;
        true
    }

    fn lax_indexed_y_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let index = self.regs.y as u16;
        let addr = (self.base_addr as u16 + index) % 0x0100;
        self.regs.a = bus.read_byte(addr);
        self.set_zero_negative(self.regs.a);
        self.regs.x = self.regs.a;
        true
    }

    fn dcp_fetched_data_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let value = self.fetched_data.wrapping_sub(1);
        bus.write_byte(self.addr_abs, value);
        self.compare_value(value, self.regs.a);
        true
    }

    fn ign_abs_indexed_x_optimistic(&mut self, bus: &mut SystemBus) -> bool {
        !self.indexed_optimistic_fetch(bus, self.regs.x)
    }

    fn ign_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        bus.read_byte(self.addr_abs);
        true
    }

    // Used by "Gaau Hok Gwong Cheung (Ch)"
    // This instruction can be unpredictable.
    // See http://visual6502.org/wiki/index.php?title=6502_Opcode_8B_%28XAA,_ANE%29
    fn xaa(&mut self, bus: &mut SystemBus) -> bool {
        self.regs.a = self.regs.a & self.regs.x & self.next_pc_byte(bus);
        true
    }

    fn rla_addr_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let carry: u8 = self.flags.c.into();
        self.flags.c = (self.fetched_data & 0x80) != 0;
        let value = ((self.fetched_data << 1) & 0xFE) | carry;
        bus.write_byte(self.addr_abs, value);
        self.and_value(value);
        true
    }

    fn rra_addr_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let carry: u8 = if self.flags.c { 1 << 7 } else { 0 };
        self.flags.c = (self.fetched_data & 0x01) != 0;
        let value = ((self.fetched_data >> 1) & 0x7F) | carry;
        bus.write_byte(self.addr_abs, value);
        self.add_value(value);
        true
    }

    fn slo_addr_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        self.flags.c = (self.fetched_data & 0x80) != 0;
        let value = (self.fetched_data << 1) & 0xFE;
        bus.write_byte(self.addr_abs, value);
        self.ora_value(value);
        true
    }

    fn sre_addr_abs_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let value = (self.fetched_data >> 1) & 0x7F;
        self.flags.c = (self.fetched_data & 0x01) != 0;
        bus.write_byte(self.addr_abs, value);
        self.eor_value(value);
        true
    }

    fn sax_write_byte_abs(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        bus.write_byte(self.addr_abs, self.regs.a & self.regs.x);
        true
    }

    fn sax_write_byte_abs_indexed_y(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        let index = self.regs.y;
        let addr = (self.base_addr as u16 + index as u16) % 0x0100;
        bus.write_byte(addr, self.regs.a & self.regs.x);
        true
    }

    fn shy_logic(self: &mut Cpu, _bus: &mut SystemBus) -> bool {
        let base = self.addr_abs;
        let value = self.regs.y;
        let index = self.regs.x;

        let addr = base + index as u16;

        self.fetched_data = value & ((base >> 8) + 1) as u8;

        self.addr_abs = if ((base ^ addr) & 0x100) != 0 {
            // Page crossed
            (addr & ((value as u16) << 8)) | (addr & 0x00FF)
        } else {
            addr
        };

        false
    }

    fn shy_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        bus.write_byte(self.addr_abs, self.fetched_data);
        true
    }

    fn shx_logic(self: &mut Cpu, _bus: &mut SystemBus) -> bool {
        let base = self.addr_abs;
        let value = self.regs.x;
        let index = self.regs.y;

        let addr = base + index as u16;

        self.fetched_data = value & ((base >> 8) + 1) as u8;

        self.addr_abs = if ((base ^ addr) & 0x100) != 0 {
            // Page crossed
            (addr & ((value as u16) << 8)) | (addr & 0x00FF)
        } else {
            addr
        };

        false
    }

    fn shx_finish(self: &mut Cpu, bus: &mut SystemBus) -> bool {
        bus.write_byte(self.addr_abs, self.fetched_data);
        true
    }
}

pub const OPCODES: [Option<Instruction>; 256] = {
    let mut opcodes = [None; 256];

    opcodes[0xA9] = Some(Instruction {
        name: "LDA",
        length: 2,
        mode: AddressMode::Immediate,
        official: true,
        cycles: &[Cpu::lda_immediate],
    });

    opcodes[0xA5] = Some(Instruction {
        name: "LDA",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: true,
        cycles: &[Cpu::fetch_abs_low, Cpu::lda_addr_abs_finish],
    });

    opcodes[0xB5] = Some(Instruction {
        name: "LDA",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::lda_indexed_x_finish,
        ],
    });

    opcodes[0xAD] = Some(Instruction {
        name: "LDA",
        length: 3,
        mode: AddressMode::Absolute,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::lda_addr_abs_finish,
        ],
    });

    opcodes[0xBD] = Some(Instruction {
        name: "LDA",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::lda_abs_indexed_x_optimistic,
            Cpu::lda_addr_abs_finish,
        ],
    });

    opcodes[0xB9] = Some(Instruction {
        name: "LDA",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::Y),
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::lda_abs_indexed_y_optimistic,
            Cpu::lda_addr_abs_finish,
        ],
    });

    opcodes[0xA1] = Some(Instruction {
        name: "LDA",
        length: 2,
        mode: AddressMode::IndexedIndirect(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_x_dummy_read_and_add,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::lda_addr_abs_finish,
        ],
    });

    opcodes[0xB1] = Some(Instruction {
        name: "LDA",
        length: 2,
        mode: AddressMode::IndirectIndexed(Register8::Y),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::lda_abs_indexed_y_optimistic,
            Cpu::lda_addr_abs_finish,
        ],
    });

    opcodes[0xA2] = Some(Instruction {
        name: "LDX",
        length: 2,
        mode: AddressMode::Immediate,
        official: true,
        cycles: &[Cpu::ldx_immediate],
    });

    opcodes[0xA6] = Some(Instruction {
        name: "LDX",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: true,
        cycles: &[Cpu::fetch_abs_low, Cpu::ldx_addr_abs_finish],
    });

    opcodes[0xB6] = Some(Instruction {
        name: "LDX",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::Y),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::ldx_indexed_y_finish,
        ],
    });

    opcodes[0xAE] = Some(Instruction {
        name: "LDX",
        length: 3,
        mode: AddressMode::Absolute,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::ldx_addr_abs_finish,
        ],
    });

    opcodes[0xBE] = Some(Instruction {
        name: "LDX",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::Y),
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::ldx_abs_indexed_y_optimistic,
            Cpu::ldx_addr_abs_finish,
        ],
    });

    opcodes[0xA0] = Some(Instruction {
        name: "LDY",
        length: 2,
        mode: AddressMode::Immediate,
        official: true,
        cycles: &[Cpu::ldy_immediate],
    });

    opcodes[0xA4] = Some(Instruction {
        name: "LDY",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: true,
        cycles: &[Cpu::fetch_abs_low, Cpu::ldy_addr_abs_finish],
    });

    opcodes[0xB4] = Some(Instruction {
        name: "LDY",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::ldy_indexed_x_finish,
        ],
    });

    opcodes[0xAC] = Some(Instruction {
        name: "LDY",
        length: 3,
        mode: AddressMode::Absolute,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::ldy_addr_abs_finish,
        ],
    });

    opcodes[0xBC] = Some(Instruction {
        name: "LDY",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::ldy_abs_indexed_x_optimistic,
            Cpu::ldy_addr_abs_finish,
        ],
    });

    opcodes[0x85] = Some(Instruction {
        name: "STA",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: true,
        cycles: &[Cpu::fetch_abs_low, Cpu::sta_write_byte_abs],
    });

    opcodes[0x95] = Some(Instruction {
        name: "STA",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::sta_write_byte_abs_indexed_x,
        ],
    });

    opcodes[0x8D] = Some(Instruction {
        name: "STA",
        length: 3,
        mode: AddressMode::Absolute,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::sta_write_byte_abs,
        ],
    });

    opcodes[0x9D] = Some(Instruction {
        name: "STA",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::st_abs_indexed_x_dummy_read,
            Cpu::sta_write_byte_abs,
        ],
    });

    opcodes[0x99] = Some(Instruction {
        name: "STA",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::Y),
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::st_abs_indexed_y_dummy_read,
            Cpu::sta_write_byte_abs,
        ],
    });

    opcodes[0x81] = Some(Instruction {
        name: "STA",
        length: 2,
        mode: AddressMode::IndexedIndirect(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_x_dummy_read_and_add,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::sta_write_byte_abs,
        ],
    });

    opcodes[0x91] = Some(Instruction {
        name: "STA",
        length: 2,
        mode: AddressMode::IndirectIndexed(Register8::Y),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::st_abs_indexed_y_dummy_read,
            Cpu::sta_write_byte_abs,
        ],
    });

    opcodes[0x86] = Some(Instruction {
        name: "STX",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: true,
        cycles: &[Cpu::fetch_abs_low, Cpu::stx_write_byte_abs],
    });

    opcodes[0x96] = Some(Instruction {
        name: "STX",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::Y),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::stx_write_byte_abs_indexed_y,
        ],
    });

    opcodes[0x8E] = Some(Instruction {
        name: "STX",
        length: 3,
        mode: AddressMode::Absolute,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::stx_write_byte_abs,
        ],
    });

    opcodes[0x84] = Some(Instruction {
        name: "STY",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: true,
        cycles: &[Cpu::fetch_abs_low, Cpu::sty_write_byte_abs],
    });

    opcodes[0x94] = Some(Instruction {
        name: "STY",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::sty_write_byte_abs_indexed_x,
        ],
    });

    opcodes[0x8C] = Some(Instruction {
        name: "STY",
        length: 3,
        mode: AddressMode::Absolute,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::sty_write_byte_abs,
        ],
    });

    opcodes[0x69] = Some(Instruction {
        name: "ADC",
        length: 2,
        mode: AddressMode::Immediate,
        official: true,
        cycles: &[Cpu::adc_immediate],
    });

    opcodes[0x65] = Some(Instruction {
        name: "ADC",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: true,
        cycles: &[Cpu::fetch_abs_low, Cpu::adc_addr_abs_finish],
    });

    opcodes[0x75] = Some(Instruction {
        name: "ADC",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::adc_zero_page_indexed_x_finish,
        ],
    });

    opcodes[0x6D] = Some(Instruction {
        name: "ADC",
        length: 3,
        mode: AddressMode::Absolute,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::adc_addr_abs_finish,
        ],
    });

    opcodes[0x7D] = Some(Instruction {
        name: "ADC",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::adc_addr_abs_indexed_x_optimistic,
            Cpu::adc_addr_abs_finish,
        ],
    });

    opcodes[0x79] = Some(Instruction {
        name: "ADC",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::Y),
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::adc_addr_abs_indexed_y_optimistic,
            Cpu::adc_addr_abs_finish,
        ],
    });

    opcodes[0x61] = Some(Instruction {
        name: "ADC",
        length: 2,
        mode: AddressMode::IndexedIndirect(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_x_dummy_read_and_add,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::adc_addr_abs_finish,
        ],
    });

    opcodes[0x71] = Some(Instruction {
        name: "ADC",
        length: 2,
        mode: AddressMode::IndirectIndexed(Register8::Y),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::adc_addr_abs_indexed_y_optimistic,
            Cpu::adc_addr_abs_finish,
        ],
    });

    opcodes[0xE9] = Some(Instruction {
        name: "SBC",
        length: 2,
        mode: AddressMode::Immediate,
        official: true,
        cycles: &[Cpu::sbc_immediate],
    });

    opcodes[0xE5] = Some(Instruction {
        name: "SBC",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: true,
        cycles: &[Cpu::fetch_abs_low, Cpu::sbc_addr_abs_finish],
    });

    opcodes[0xF5] = Some(Instruction {
        name: "SBC",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::sbc_zero_page_indexed_x_finish,
        ],
    });

    opcodes[0xED] = Some(Instruction {
        name: "SBC",
        length: 3,
        mode: AddressMode::Absolute,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::sbc_addr_abs_finish,
        ],
    });

    opcodes[0xFD] = Some(Instruction {
        name: "SBC",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::sbc_addr_abs_indexed_x_optimistic,
            Cpu::sbc_addr_abs_finish,
        ],
    });

    opcodes[0xF9] = Some(Instruction {
        name: "SBC",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::Y),
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::sbc_addr_abs_indexed_y_optimistic,
            Cpu::sbc_addr_abs_finish,
        ],
    });

    opcodes[0xE1] = Some(Instruction {
        name: "SBC",
        length: 2,
        mode: AddressMode::IndexedIndirect(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_x_dummy_read_and_add,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::sbc_addr_abs_finish,
        ],
    });

    opcodes[0xF1] = Some(Instruction {
        name: "SBC",
        length: 2,
        mode: AddressMode::IndirectIndexed(Register8::Y),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::sbc_addr_abs_indexed_y_optimistic,
            Cpu::sbc_addr_abs_finish,
        ],
    });

    opcodes[0x29] = Some(Instruction {
        name: "AND",
        length: 2,
        mode: AddressMode::Immediate,
        official: true,
        cycles: &[Cpu::and_immediate],
    });

    opcodes[0x25] = Some(Instruction {
        name: "AND",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: true,
        cycles: &[Cpu::fetch_abs_low, Cpu::and_addr_abs_finish],
    });

    opcodes[0x35] = Some(Instruction {
        name: "AND",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::and_zero_page_indexed_x_finish,
        ],
    });

    opcodes[0x2D] = Some(Instruction {
        name: "AND",
        length: 3,
        mode: AddressMode::Absolute,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::and_addr_abs_finish,
        ],
    });

    opcodes[0x3D] = Some(Instruction {
        name: "AND",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::and_addr_abs_indexed_x_optimistic,
            Cpu::and_addr_abs_finish,
        ],
    });

    opcodes[0x39] = Some(Instruction {
        name: "AND",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::Y),
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::and_addr_abs_indexed_y_optimistic,
            Cpu::and_addr_abs_finish,
        ],
    });

    opcodes[0x21] = Some(Instruction {
        name: "AND",
        length: 2,
        mode: AddressMode::IndexedIndirect(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_x_dummy_read_and_add,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::and_addr_abs_finish,
        ],
    });

    opcodes[0x31] = Some(Instruction {
        name: "AND",
        length: 2,
        mode: AddressMode::IndirectIndexed(Register8::Y),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::and_addr_abs_indexed_y_optimistic,
            Cpu::and_addr_abs_finish,
        ],
    });

    opcodes[0x09] = Some(Instruction {
        name: "ORA",
        length: 2,
        mode: AddressMode::Immediate,
        official: true,
        cycles: &[Cpu::ora_immediate],
    });

    opcodes[0x05] = Some(Instruction {
        name: "ORA",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: true,
        cycles: &[Cpu::fetch_abs_low, Cpu::ora_addr_abs_finish],
    });

    opcodes[0x15] = Some(Instruction {
        name: "ORA",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::ora_zero_page_indexed_x_finish,
        ],
    });

    opcodes[0x0D] = Some(Instruction {
        name: "ORA",
        length: 3,
        mode: AddressMode::Absolute,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::ora_addr_abs_finish,
        ],
    });

    opcodes[0x1D] = Some(Instruction {
        name: "ORA",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::ora_addr_abs_indexed_x_optimistic,
            Cpu::ora_addr_abs_finish,
        ],
    });

    opcodes[0x19] = Some(Instruction {
        name: "ORA",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::Y),
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::ora_addr_abs_indexed_y_optimistic,
            Cpu::ora_addr_abs_finish,
        ],
    });

    opcodes[0x01] = Some(Instruction {
        name: "ORA",
        length: 2,
        mode: AddressMode::IndexedIndirect(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_x_dummy_read_and_add,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::ora_addr_abs_finish,
        ],
    });

    opcodes[0x11] = Some(Instruction {
        name: "ORA",
        length: 2,
        mode: AddressMode::IndirectIndexed(Register8::Y),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::ora_addr_abs_indexed_y_optimistic,
            Cpu::ora_addr_abs_finish,
        ],
    });

    opcodes[0x49] = Some(Instruction {
        name: "EOR",
        length: 2,
        mode: AddressMode::Immediate,
        official: true,
        cycles: &[Cpu::eor_immediate],
    });

    opcodes[0x45] = Some(Instruction {
        name: "EOR",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: true,
        cycles: &[Cpu::fetch_abs_low, Cpu::eor_addr_abs_finish],
    });

    opcodes[0x55] = Some(Instruction {
        name: "EOR",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::eor_zero_page_indexed_x_finish,
        ],
    });

    opcodes[0x4D] = Some(Instruction {
        name: "EOR",
        length: 3,
        mode: AddressMode::Absolute,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::eor_addr_abs_finish,
        ],
    });

    opcodes[0x5D] = Some(Instruction {
        name: "EOR",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::eor_addr_abs_indexed_x_optimistic,
            Cpu::eor_addr_abs_finish,
        ],
    });

    opcodes[0x59] = Some(Instruction {
        name: "EOR",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::Y),
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::eor_addr_abs_indexed_y_optimistic,
            Cpu::eor_addr_abs_finish,
        ],
    });

    opcodes[0x41] = Some(Instruction {
        name: "EOR",
        length: 2,
        mode: AddressMode::IndexedIndirect(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_x_dummy_read_and_add,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::eor_addr_abs_finish,
        ],
    });

    opcodes[0x51] = Some(Instruction {
        name: "EOR",
        length: 2,
        mode: AddressMode::IndirectIndexed(Register8::Y),
        official: true,
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
        length: 1,
        mode: AddressMode::Implied,
        official: true,
        cycles: &[Cpu::sec],
    });

    opcodes[0x18] = Some(Instruction {
        name: "CLC",
        length: 1,
        mode: AddressMode::Implied,
        official: true,
        cycles: &[Cpu::clc],
    });

    opcodes[0x78] = Some(Instruction {
        name: "SEI",
        length: 1,
        mode: AddressMode::Implied,
        official: true,
        cycles: &[Cpu::sei],
    });

    opcodes[0x58] = Some(Instruction {
        name: "CLI",
        length: 1,
        mode: AddressMode::Implied,
        official: true,
        cycles: &[Cpu::cli],
    });

    opcodes[0xF8] = Some(Instruction {
        name: "SED",
        length: 1,
        mode: AddressMode::Implied,
        official: true,
        cycles: &[Cpu::sed],
    });

    opcodes[0xD8] = Some(Instruction {
        name: "CLD",
        length: 1,
        mode: AddressMode::Implied,
        official: true,
        cycles: &[Cpu::cld],
    });

    opcodes[0xB8] = Some(Instruction {
        name: "CLV",
        length: 1,
        mode: AddressMode::Implied,
        official: true,
        cycles: &[Cpu::clv],
    });

    opcodes[0x4C] = Some(Instruction {
        name: "JMP",
        length: 3,
        mode: AddressMode::Absolute,
        official: true,
        cycles: &[Cpu::fetch_abs_low, Cpu::jmp_abs_finish],
    });

    opcodes[0x6C] = Some(Instruction {
        name: "JMP",
        length: 3,
        mode: AddressMode::Indirect, // Pure Indirect (non-indexed)
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::jmp_indirect_read_pointer_low,
            Cpu::jump_indirect_finish,
        ],
    });

    opcodes[0x30] = Some(Instruction {
        name: "BMI",
        length: 2,
        mode: AddressMode::Relative,
        official: true,
        cycles: &[
            Cpu::fetch_rel_offset_and_check_branch_n,
            Cpu::take_branch,
            Cpu::branch_page_fixup,
        ],
    });

    opcodes[0x10] = Some(Instruction {
        name: "BPL",
        length: 2,
        mode: AddressMode::Relative,
        official: true,
        cycles: &[
            Cpu::fetch_rel_offset_and_check_branch_not_n,
            Cpu::take_branch,
            Cpu::branch_page_fixup,
        ],
    });

    opcodes[0x90] = Some(Instruction {
        name: "BCC",
        length: 2,
        mode: AddressMode::Relative,
        official: true,
        cycles: &[
            Cpu::fetch_rel_offset_and_check_branch_not_c,
            Cpu::take_branch,
            Cpu::branch_page_fixup,
        ],
    });

    opcodes[0xB0] = Some(Instruction {
        name: "BCS",
        length: 2,
        mode: AddressMode::Relative,
        official: true,
        cycles: &[
            Cpu::fetch_rel_offset_and_check_branch_c,
            Cpu::take_branch,
            Cpu::branch_page_fixup,
        ],
    });

    opcodes[0xF0] = Some(Instruction {
        name: "BEQ",
        length: 2,
        mode: AddressMode::Relative,
        official: true,
        cycles: &[
            Cpu::fetch_rel_offset_and_check_branch_z,
            Cpu::take_branch,
            Cpu::branch_page_fixup,
        ],
    });

    opcodes[0xD0] = Some(Instruction {
        name: "BNE",
        length: 2,
        mode: AddressMode::Relative,
        official: true,
        cycles: &[
            Cpu::fetch_rel_offset_and_check_branch_not_z,
            Cpu::take_branch,
            Cpu::branch_page_fixup,
        ],
    });

    opcodes[0x70] = Some(Instruction {
        name: "BVS",
        length: 2,
        mode: AddressMode::Relative,
        official: true,
        cycles: &[
            Cpu::fetch_rel_offset_and_check_branch_v,
            Cpu::take_branch,
            Cpu::branch_page_fixup,
        ],
    });

    opcodes[0x50] = Some(Instruction {
        name: "BVC",
        length: 2,
        mode: AddressMode::Relative,
        official: true,
        cycles: &[
            Cpu::fetch_rel_offset_and_check_branch_not_v,
            Cpu::take_branch,
            Cpu::branch_page_fixup,
        ],
    });

    opcodes[0xC9] = Some(Instruction {
        name: "CMP",
        length: 2,
        mode: AddressMode::Immediate,
        official: true,
        cycles: &[Cpu::cmp_immediate],
    });

    opcodes[0xC5] = Some(Instruction {
        name: "CMP",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: true,
        cycles: &[Cpu::fetch_abs_low, Cpu::cmp_addr_abs_finish],
    });

    opcodes[0xD5] = Some(Instruction {
        name: "CMP",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::cmp_zero_page_indexed_x_finish,
        ],
    });

    opcodes[0xCD] = Some(Instruction {
        name: "CMP",
        length: 3,
        mode: AddressMode::Absolute,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::cmp_addr_abs_finish,
        ],
    });

    opcodes[0xDD] = Some(Instruction {
        name: "CMP",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::cmp_addr_abs_indexed_x_optimistic,
            Cpu::cmp_addr_abs_finish,
        ],
    });

    opcodes[0xD9] = Some(Instruction {
        name: "CMP",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::Y),
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::cmp_addr_abs_indexed_y_optimistic,
            Cpu::cmp_addr_abs_finish,
        ],
    });

    opcodes[0xC1] = Some(Instruction {
        name: "CMP",
        length: 2,
        mode: AddressMode::IndexedIndirect(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_x_dummy_read_and_add,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::cmp_addr_abs_finish,
        ],
    });

    opcodes[0xD1] = Some(Instruction {
        name: "CMP",
        length: 2,
        mode: AddressMode::IndirectIndexed(Register8::Y),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::cmp_addr_abs_indexed_y_optimistic,
            Cpu::cmp_addr_abs_finish,
        ],
    });

    opcodes[0xE0] = Some(Instruction {
        name: "CPX",
        length: 2,
        mode: AddressMode::Immediate,
        official: true,
        cycles: &[Cpu::cpx_immediate],
    });

    opcodes[0xE4] = Some(Instruction {
        name: "CPX",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: true,
        cycles: &[Cpu::fetch_abs_low, Cpu::cpx_addr_abs_finish],
    });

    opcodes[0xEC] = Some(Instruction {
        name: "CPX",
        length: 3,
        mode: AddressMode::Absolute,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::cpx_addr_abs_finish,
        ],
    });

    opcodes[0xC0] = Some(Instruction {
        name: "CPY",
        length: 2,
        mode: AddressMode::Immediate,
        official: true,
        cycles: &[Cpu::cpy_immediate],
    });

    opcodes[0xC4] = Some(Instruction {
        name: "CPY",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: true,
        cycles: &[Cpu::fetch_abs_low, Cpu::cpy_addr_abs_finish],
    });

    opcodes[0xCC] = Some(Instruction {
        name: "CPY",
        length: 3,
        mode: AddressMode::Absolute,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::cpy_addr_abs_finish,
        ],
    });

    opcodes[0x24] = Some(Instruction {
        name: "BIT",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: true,
        cycles: &[Cpu::fetch_abs_low, Cpu::bit_addr_abs_finish],
    });

    opcodes[0x2C] = Some(Instruction {
        name: "BIT",
        length: 3,
        mode: AddressMode::Absolute,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::bit_addr_abs_finish,
        ],
    });

    opcodes[0xE6] = Some(Instruction {
        name: "INC",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::inc_addr_abs_finish,
        ],
    });

    opcodes[0xF6] = Some(Instruction {
        name: "INC",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::read_zero_page_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::inc_addr_abs_finish,
        ],
    });

    opcodes[0xEE] = Some(Instruction {
        name: "INC",
        length: 3,
        mode: AddressMode::Absolute,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::inc_addr_abs_finish,
        ],
    });

    opcodes[0xFE] = Some(Instruction {
        name: "INC",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::X),
        official: true,
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
        name: "DEC",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::dec_addr_abs_finish,
        ],
    });

    opcodes[0xD6] = Some(Instruction {
        name: "DEC",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::read_zero_page_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::dec_addr_abs_finish,
        ],
    });

    opcodes[0xCE] = Some(Instruction {
        name: "DEC",
        length: 3,
        mode: AddressMode::Absolute,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::dec_addr_abs_finish,
        ],
    });

    opcodes[0xDE] = Some(Instruction {
        name: "DEC",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::X),
        official: true,
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
        length: 1,
        mode: AddressMode::Implied,
        official: true,
        cycles: &[Cpu::inx],
    });

    opcodes[0xC8] = Some(Instruction {
        name: "INY",
        length: 1,
        mode: AddressMode::Implied,
        official: true,
        cycles: &[Cpu::iny],
    });

    opcodes[0xCA] = Some(Instruction {
        name: "DEX",
        length: 1,
        mode: AddressMode::Implied,
        official: true,
        cycles: &[Cpu::dex],
    });

    opcodes[0x88] = Some(Instruction {
        name: "DEY",
        length: 1,
        mode: AddressMode::Implied,
        official: true,
        cycles: &[Cpu::dey],
    });

    opcodes[0xAA] = Some(Instruction {
        name: "TAX",
        length: 1,
        mode: AddressMode::Implied,
        official: true,
        cycles: &[Cpu::tax],
    });

    opcodes[0x8A] = Some(Instruction {
        name: "TXA",
        length: 1,
        mode: AddressMode::Implied,
        official: true,
        cycles: &[Cpu::txa],
    });

    opcodes[0xA8] = Some(Instruction {
        name: "TAY",
        length: 1,
        mode: AddressMode::Implied,
        official: true,
        cycles: &[Cpu::tay],
    });

    opcodes[0x98] = Some(Instruction {
        name: "TYA",
        length: 1,
        mode: AddressMode::Implied,
        official: true,
        cycles: &[Cpu::tya],
    });

    opcodes[0xBA] = Some(Instruction {
        name: "TSX",
        length: 1,
        mode: AddressMode::Implied,
        official: true,
        cycles: &[Cpu::tsx],
    });

    opcodes[0x9A] = Some(Instruction {
        name: "TXS",
        length: 1,
        mode: AddressMode::Implied,
        official: true,
        cycles: &[Cpu::txs],
    });

    opcodes[0x20] = Some(Instruction {
        name: "JSR",
        length: 3,
        mode: AddressMode::Absolute,
        official: true,
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
        length: 1,
        mode: AddressMode::Implied,
        official: true,
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
        length: 1,
        mode: AddressMode::Implied,
        official: true,
        cycles: &[Cpu::dummy_read, Cpu::pha_finish],
    });

    opcodes[0x68] = Some(Instruction {
        name: "PLA",
        length: 1,
        mode: AddressMode::Implied,
        official: true,
        cycles: &[Cpu::dummy_read, Cpu::inc_sp, Cpu::pla_finish],
    });

    opcodes[0x08] = Some(Instruction {
        name: "PHP",
        length: 1,
        mode: AddressMode::Implied,
        official: true,
        cycles: &[Cpu::dummy_read, Cpu::php_finish],
    });

    opcodes[0x28] = Some(Instruction {
        name: "PLP",
        length: 1,
        mode: AddressMode::Implied,
        official: true,
        cycles: &[Cpu::dummy_read, Cpu::inc_sp, Cpu::plp_finish],
    });

    opcodes[0x4A] = Some(Instruction {
        name: "LSR",
        length: 1,
        mode: AddressMode::Accumulator,
        official: true,
        cycles: &[Cpu::lsr_accumulator_finish],
    });

    opcodes[0x46] = Some(Instruction {
        name: "LSR",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::lsr_addr_abs_finish,
        ],
    });

    opcodes[0x56] = Some(Instruction {
        name: "LSR",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::read_zero_page_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::lsr_addr_abs_finish,
        ],
    });

    opcodes[0x4E] = Some(Instruction {
        name: "LSR",
        length: 3,
        mode: AddressMode::Absolute,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::lsr_addr_abs_finish,
        ],
    });

    opcodes[0x5E] = Some(Instruction {
        name: "LSR",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::X),
        official: true,
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
        name: "ASL",
        length: 1,
        mode: AddressMode::Accumulator,
        official: true,
        cycles: &[Cpu::asl_accumulator_finish],
    });

    opcodes[0x06] = Some(Instruction {
        name: "ASL",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::asl_addr_abs_finish,
        ],
    });

    opcodes[0x16] = Some(Instruction {
        name: "ASL",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::read_zero_page_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::asl_addr_abs_finish,
        ],
    });

    opcodes[0x0E] = Some(Instruction {
        name: "ASL",
        length: 3,
        mode: AddressMode::Absolute,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::asl_addr_abs_finish,
        ],
    });

    opcodes[0x1E] = Some(Instruction {
        name: "ASL",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::X),
        official: true,
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
        name: "ROR",
        length: 1,
        mode: AddressMode::Accumulator,
        official: true,
        cycles: &[Cpu::ror_accumulator_finish],
    });

    opcodes[0x66] = Some(Instruction {
        name: "ROR",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::ror_addr_abs_finish,
        ],
    });

    opcodes[0x76] = Some(Instruction {
        name: "ROR",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::read_zero_page_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::ror_addr_abs_finish,
        ],
    });

    opcodes[0x6E] = Some(Instruction {
        name: "ROR",
        length: 3,
        mode: AddressMode::Absolute,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::ror_addr_abs_finish,
        ],
    });

    opcodes[0x7E] = Some(Instruction {
        name: "ROR",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::X),
        official: true,
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
        name: "ROL",
        length: 1,
        mode: AddressMode::Accumulator,
        official: true,
        cycles: &[Cpu::rol_accumulator_finish],
    });

    opcodes[0x26] = Some(Instruction {
        name: "ROL",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::rol_addr_abs_finish,
        ],
    });

    opcodes[0x36] = Some(Instruction {
        name: "ROL",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::X),
        official: true,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::read_zero_page_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::rol_addr_abs_finish,
        ],
    });

    opcodes[0x2E] = Some(Instruction {
        name: "ROL",
        length: 3,
        mode: AddressMode::Absolute,
        official: true,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::rol_addr_abs_finish,
        ],
    });

    opcodes[0x3E] = Some(Instruction {
        name: "ROL",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::X),
        official: true,
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
        length: 2,
        mode: AddressMode::Implied,
        official: true,
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
        length: 1,
        mode: AddressMode::Implied,
        official: true,
        cycles: &[
            Cpu::dummy_read,
            Cpu::pull_status,
            Cpu::pull_low,
            Cpu::pull_high,
            Cpu::rti_finish,
        ],
    });

    opcodes[0xEA] = Some(Instruction {
        name: "NOP",
        length: 1,
        mode: AddressMode::Implied,
        official: true,
        cycles: &[Cpu::nop_implied],
    });

    // Unofficial opcodes.

    opcodes[0x4B] = Some(Instruction {
        name: "ALR",
        length: 2,
        mode: AddressMode::Immediate,
        official: false,
        cycles: &[Cpu::alr],
    });

    opcodes[0x0B] = Some(Instruction {
        name: "ANC",
        length: 2,
        mode: AddressMode::Immediate,
        official: false,
        cycles: &[Cpu::anc],
    });

    opcodes[0x2B] = Some(Instruction {
        name: "ANC",
        length: 2,
        mode: AddressMode::Immediate,
        official: false,
        cycles: &[Cpu::anc],
    });

    opcodes[0x6B] = Some(Instruction {
        name: "ARR",
        length: 2,
        mode: AddressMode::Immediate,
        official: false,
        cycles: &[Cpu::arr],
    });

    opcodes[0xCB] = Some(Instruction {
        name: "AXS",
        length: 2,
        mode: AddressMode::Immediate,
        official: false,
        cycles: &[Cpu::axs],
    });

    opcodes[0xA3] = Some(Instruction {
        name: "LAX",
        length: 2,
        mode: AddressMode::IndexedIndirect(Register8::X),
        official: false,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_x_dummy_read_and_add,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::lax_addr_abs_finish,
        ],
    });

    opcodes[0xA7] = Some(Instruction {
        name: "LAX",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: false,
        cycles: &[Cpu::fetch_abs_low, Cpu::lax_addr_abs_finish],
    });

    opcodes[0xAB] = Some(Instruction {
        name: "LAX",
        length: 2,
        mode: AddressMode::Immediate,
        official: false,
        cycles: &[Cpu::lax_immediate],
    });

    opcodes[0xAF] = Some(Instruction {
        name: "LAX",
        length: 3,
        mode: AddressMode::Absolute,
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::lax_addr_abs_finish,
        ],
    });

    opcodes[0xB3] = Some(Instruction {
        name: "LAX",
        length: 2,
        mode: AddressMode::IndirectIndexed(Register8::Y),
        official: false,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::lax_abs_indexed_y_optimistic,
            Cpu::lax_addr_abs_finish,
        ],
    });

    opcodes[0xB7] = Some(Instruction {
        name: "LAX",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::Y),
        official: false,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::lax_indexed_y_finish,
        ],
    });

    opcodes[0xBF] = Some(Instruction {
        name: "LAX",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::Y),
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::lax_abs_indexed_y_optimistic,
            Cpu::lax_addr_abs_finish,
        ],
    });

    opcodes[0x83] = Some(Instruction {
        name: "SAX",
        length: 2,
        mode: AddressMode::IndexedIndirect(Register8::X),
        official: false,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_x_dummy_read_and_add,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::sax_write_byte_abs,
        ],
    });

    opcodes[0x87] = Some(Instruction {
        name: "SAX",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: false,
        cycles: &[Cpu::fetch_abs_low, Cpu::sax_write_byte_abs],
    });

    opcodes[0x8F] = Some(Instruction {
        name: "SAX",
        length: 3,
        mode: AddressMode::Absolute,
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::sax_write_byte_abs,
        ],
    });

    opcodes[0x97] = Some(Instruction {
        name: "SAX",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::Y),
        official: false,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::sax_write_byte_abs_indexed_y,
        ],
    });

    opcodes[0xC7] = Some(Instruction {
        name: "DCP",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::dcp_fetched_data_finish,
        ],
    });

    opcodes[0xCF] = Some(Instruction {
        name: "DCP",
        length: 3,
        mode: AddressMode::Absolute,
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::dcp_fetched_data_finish,
        ],
    });

    opcodes[0xD7] = Some(Instruction {
        name: "DCP",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::X),
        official: false,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::read_zero_page_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::dcp_fetched_data_finish,
        ],
    });

    opcodes[0xDF] = Some(Instruction {
        name: "DCP",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::X),
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::rmw_abs_indexed_x_dummy_read,
            Cpu::read_abs_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::dcp_fetched_data_finish,
        ],
    });

    opcodes[0xDB] = Some(Instruction {
        name: "DCP",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::Y),
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::rmw_abs_indexed_y_dummy_read,
            Cpu::read_abs_indexed_y_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::dcp_fetched_data_finish,
        ],
    });

    opcodes[0xC3] = Some(Instruction {
        name: "DCP",
        length: 2,
        mode: AddressMode::IndexedIndirect(Register8::X),
        official: false,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_x_dummy_read_and_add,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::dcp_fetched_data_finish,
        ],
    });

    opcodes[0xD3] = Some(Instruction {
        name: "DCP",
        length: 2,
        mode: AddressMode::IndirectIndexed(Register8::Y),
        official: false,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::rmw_abs_indexed_y_dummy_read,
            Cpu::read_abs_indexed_y_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::dcp_fetched_data_finish,
        ],
    });

    opcodes[0xE3] = Some(Instruction {
        name: "ISB",
        length: 2,
        mode: AddressMode::IndexedIndirect(Register8::X),
        official: false,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_x_dummy_read_and_add,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::isb_addr_abs_finish,
        ],
    });

    opcodes[0xE7] = Some(Instruction {
        name: "ISB",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::isb_addr_abs_finish,
        ],
    });

    opcodes[0xF3] = Some(Instruction {
        name: "ISB",
        length: 2,
        mode: AddressMode::IndirectIndexed(Register8::Y),
        official: false,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::rmw_abs_indexed_y_dummy_read,
            Cpu::read_abs_indexed_y_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::isb_addr_abs_finish,
        ],
    });

    opcodes[0xF7] = Some(Instruction {
        name: "ISB",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::X),
        official: false,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::read_zero_page_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::isb_addr_abs_finish,
        ],
    });

    opcodes[0xFB] = Some(Instruction {
        name: "ISB",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::Y),
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::rmw_abs_indexed_y_dummy_read,
            Cpu::read_abs_indexed_y_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::isb_addr_abs_finish,
        ],
    });

    opcodes[0xEF] = Some(Instruction {
        name: "ISB",
        length: 3,
        mode: AddressMode::Absolute,
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::isb_addr_abs_finish,
        ],
    });

    opcodes[0xFF] = Some(Instruction {
        name: "ISB",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::X),
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::rmw_abs_indexed_x_dummy_read,
            Cpu::read_abs_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::isb_addr_abs_finish,
        ],
    });

    opcodes[0xEB] = Some(Instruction {
        name: "SBC",
        length: 2,
        mode: AddressMode::Immediate,
        official: false,
        cycles: &[Cpu::sbc_immediate],
    });

    opcodes[0x23] = Some(Instruction {
        name: "RLA",
        length: 2,
        mode: AddressMode::IndexedIndirect(Register8::X),
        official: false,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_x_dummy_read_and_add,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::rla_addr_abs_finish,
        ],
    });

    opcodes[0x27] = Some(Instruction {
        name: "RLA",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::rla_addr_abs_finish,
        ],
    });

    opcodes[0x2F] = Some(Instruction {
        name: "RLA",
        length: 3,
        mode: AddressMode::Absolute,
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::rla_addr_abs_finish,
        ],
    });

    opcodes[0x33] = Some(Instruction {
        name: "RLA",
        length: 2,
        mode: AddressMode::IndirectIndexed(Register8::Y),
        official: false,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::rmw_abs_indexed_y_dummy_read,
            Cpu::read_abs_indexed_y_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::rla_addr_abs_finish,
        ],
    });

    opcodes[0x37] = Some(Instruction {
        name: "RLA",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::X),
        official: false,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::read_zero_page_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::rla_addr_abs_finish,
        ],
    });

    opcodes[0x3B] = Some(Instruction {
        name: "RLA",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::Y),
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::rmw_abs_indexed_y_dummy_read,
            Cpu::read_abs_indexed_y_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::rla_addr_abs_finish,
        ],
    });

    opcodes[0x3F] = Some(Instruction {
        name: "RLA",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::X),
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::rmw_abs_indexed_x_dummy_read,
            Cpu::read_abs_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::rla_addr_abs_finish,
        ],
    });

    opcodes[0x63] = Some(Instruction {
        name: "RRA",
        length: 2,
        mode: AddressMode::IndexedIndirect(Register8::X),
        official: false,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_x_dummy_read_and_add,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::rra_addr_abs_finish,
        ],
    });

    opcodes[0x67] = Some(Instruction {
        name: "RRA",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::rra_addr_abs_finish,
        ],
    });

    opcodes[0x6F] = Some(Instruction {
        name: "RRA",
        length: 3,
        mode: AddressMode::Absolute,
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::rra_addr_abs_finish,
        ],
    });

    opcodes[0x73] = Some(Instruction {
        name: "RRA",
        length: 2,
        mode: AddressMode::IndirectIndexed(Register8::Y),
        official: false,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::rmw_abs_indexed_y_dummy_read,
            Cpu::read_abs_indexed_y_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::rra_addr_abs_finish,
        ],
    });

    opcodes[0x77] = Some(Instruction {
        name: "RRA",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::X),
        official: false,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::read_zero_page_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::rra_addr_abs_finish,
        ],
    });

    opcodes[0x7B] = Some(Instruction {
        name: "RRA",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::Y),
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::rmw_abs_indexed_y_dummy_read,
            Cpu::read_abs_indexed_y_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::rra_addr_abs_finish,
        ],
    });

    opcodes[0x7F] = Some(Instruction {
        name: "RRA",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::X),
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::rmw_abs_indexed_x_dummy_read,
            Cpu::read_abs_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::rra_addr_abs_finish,
        ],
    });

    opcodes[0x03] = Some(Instruction {
        name: "SLO",
        length: 2,
        mode: AddressMode::IndexedIndirect(Register8::X),
        official: false,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_x_dummy_read_and_add,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::slo_addr_abs_finish,
        ],
    });

    opcodes[0x07] = Some(Instruction {
        name: "SLO",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::slo_addr_abs_finish,
        ],
    });

    opcodes[0x0F] = Some(Instruction {
        name: "SLO",
        length: 3,
        mode: AddressMode::Absolute,
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::slo_addr_abs_finish,
        ],
    });

    opcodes[0x13] = Some(Instruction {
        name: "SLO",
        length: 2,
        mode: AddressMode::IndirectIndexed(Register8::Y),
        official: false,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::rmw_abs_indexed_y_dummy_read,
            Cpu::read_abs_indexed_y_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::slo_addr_abs_finish,
        ],
    });

    opcodes[0x17] = Some(Instruction {
        name: "SLO",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::X),
        official: false,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::read_zero_page_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::slo_addr_abs_finish,
        ],
    });

    opcodes[0x1B] = Some(Instruction {
        name: "SLO",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::Y),
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::rmw_abs_indexed_y_dummy_read,
            Cpu::read_abs_indexed_y_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::slo_addr_abs_finish,
        ],
    });

    opcodes[0x1F] = Some(Instruction {
        name: "SLO",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::X),
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::rmw_abs_indexed_x_dummy_read,
            Cpu::read_abs_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::slo_addr_abs_finish,
        ],
    });

    opcodes[0x43] = Some(Instruction {
        name: "SRE",
        length: 2,
        mode: AddressMode::IndexedIndirect(Register8::X),
        official: false,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_x_dummy_read_and_add,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::sre_addr_abs_finish,
        ],
    });

    opcodes[0x47] = Some(Instruction {
        name: "SRE",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::sre_addr_abs_finish,
        ],
    });

    opcodes[0x4F] = Some(Instruction {
        name: "SRE",
        length: 3,
        mode: AddressMode::Absolute,
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::read_abs_addr_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::sre_addr_abs_finish,
        ],
    });

    opcodes[0x53] = Some(Instruction {
        name: "SRE",
        length: 2,
        mode: AddressMode::IndirectIndexed(Register8::Y),
        official: false,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::indexed_fetch_ptr_low,
            Cpu::indexed_fetch_ptr_high,
            Cpu::rmw_abs_indexed_y_dummy_read,
            Cpu::read_abs_indexed_y_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::sre_addr_abs_finish,
        ],
    });

    opcodes[0x5B] = Some(Instruction {
        name: "SRE",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::Y),
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::rmw_abs_indexed_y_dummy_read,
            Cpu::read_abs_indexed_y_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::sre_addr_abs_finish,
        ],
    });

    opcodes[0x57] = Some(Instruction {
        name: "SRE",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::X),
        official: false,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::dummy_read_base,
            Cpu::read_zero_page_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::sre_addr_abs_finish,
        ],
    });

    opcodes[0x5F] = Some(Instruction {
        name: "SRE",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::X),
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::rmw_abs_indexed_x_dummy_read,
            Cpu::read_abs_indexed_x_data,
            Cpu::addr_abs_fetched_data_dummy_write,
            Cpu::sre_addr_abs_finish,
        ],
    });

    let nop_implied = Instruction {
        name: "NOP",
        length: 1,
        mode: AddressMode::Implied,
        official: false,
        cycles: &[Cpu::nop_implied],
    };
    opcodes[0x1A] = Some(nop_implied);
    opcodes[0x3A] = Some(nop_implied);
    opcodes[0x5A] = Some(nop_implied);
    opcodes[0x7A] = Some(nop_implied);
    opcodes[0xDA] = Some(nop_implied);
    opcodes[0xFA] = Some(nop_implied);

    let skb = Instruction {
        name: "NOP",
        length: 2,
        mode: AddressMode::Immediate,
        official: false,
        cycles: &[Cpu::skb],
    };
    opcodes[0x80] = Some(skb);
    opcodes[0x82] = Some(skb);
    opcodes[0x89] = Some(skb);
    opcodes[0xC2] = Some(skb);
    opcodes[0xE2] = Some(skb);

    opcodes[0x0C] = Some(Instruction {
        name: "NOP",
        length: 3,
        mode: AddressMode::Absolute,
        official: false,
        cycles: &[Cpu::fetch_abs_low, Cpu::fetch_abs_high, Cpu::ign_finish],
    });

    let ign_abs_x = Instruction {
        name: "NOP",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::X),
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::ign_abs_indexed_x_optimistic,
            Cpu::ign_finish,
        ],
    };
    opcodes[0x1C] = Some(ign_abs_x);
    opcodes[0x3C] = Some(ign_abs_x);
    opcodes[0x5C] = Some(ign_abs_x);
    opcodes[0x7C] = Some(ign_abs_x);
    opcodes[0xDC] = Some(ign_abs_x);
    opcodes[0xFC] = Some(ign_abs_x);

    let ign_zp = Instruction {
        name: "NOP",
        length: 2,
        mode: AddressMode::ZeroPage,
        official: false,
        cycles: &[Cpu::fetch_abs_low, Cpu::ign_finish],
    };
    opcodes[0x04] = Some(ign_zp);
    opcodes[0x44] = Some(ign_zp);
    opcodes[0x64] = Some(ign_zp);

    let ign_zp_x = Instruction {
        name: "NOP",
        length: 2,
        mode: AddressMode::ZeroPageIndexed(Register8::X),
        official: false,
        cycles: &[
            Cpu::fetch_base_addr,
            Cpu::read_zero_page_indexed_x_data,
            Cpu::ign_finish,
        ],
    };
    opcodes[0x14] = Some(ign_zp_x);
    opcodes[0x34] = Some(ign_zp_x);
    opcodes[0x54] = Some(ign_zp_x);
    opcodes[0x74] = Some(ign_zp_x);
    opcodes[0xD4] = Some(ign_zp_x);
    opcodes[0xF4] = Some(ign_zp_x);

    opcodes[0x8B] = Some(Instruction {
        name: "XAA",
        length: 2,
        mode: AddressMode::Immediate,
        official: false,
        cycles: &[Cpu::xaa],
    });

    opcodes[0x9C] = Some(Instruction {
        name: "SHY",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::X),
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::shy_logic,
            Cpu::shy_finish,
        ],
    });

    opcodes[0x9E] = Some(Instruction {
        name: "SHX",
        length: 3,
        mode: AddressMode::AbsoluteIndexed(Register8::Y),
        official: false,
        cycles: &[
            Cpu::fetch_abs_low,
            Cpu::fetch_abs_high,
            Cpu::shx_logic,
            Cpu::shx_finish,
        ],
    });

    opcodes
};

pub const NMI_INTERRUPT: Instruction = Instruction {
    name: "NMI",
    length: 0,
    mode: AddressMode::Implied,
    official: true,
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
    length: 0,
    mode: AddressMode::Implied,
    official: true,
    cycles: &[
        Cpu::dummy_read,
        Cpu::push_pc_high,
        Cpu::push_pc_low,
        Cpu::interrupt_push_status,
        Cpu::read_irq_vector_low,
        Cpu::irq_finish,
    ],
};
