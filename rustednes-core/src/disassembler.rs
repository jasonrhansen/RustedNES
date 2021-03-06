use crate::cpu::{AddressMode, Register8};
use crate::memory::Memory;

pub struct Disassembler {
    pub pc: u16,
}

impl Disassembler {
    pub fn new(pc: u16) -> Disassembler {
        Disassembler { pc }
    }

    pub fn disassemble_next<M: Memory>(&mut self, mem: &mut M) -> String {
        let op = self.next_pc_byte(mem);
        handle_opcode!(op, self, mem)
    }

    fn next_pc_byte<M: Memory>(&mut self, mem: &mut M) -> u8 {
        let pc = self.pc;
        let b = mem.read_byte(pc);
        self.pc += 1;
        b
    }

    fn next_pc_word<M: Memory>(&mut self, mem: &mut M) -> u16 {
        let pc = self.pc;
        let w = mem.read_word(pc);
        self.pc += 2;
        w
    }

    fn dis_pc_byte<M: Memory>(&mut self, mem: &mut M) -> String {
        format!("${:02x}", self.next_pc_byte(mem))
    }

    fn dis_pc_word<M: Memory>(&mut self, mem: &mut M) -> String {
        format!("${:04x}", self.next_pc_word(mem))
    }

    fn dis_am<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> String {
        use crate::cpu::AddressMode::*;
        match am {
            Immediate => format!("#{}", self.dis_pc_byte(mem)),
            Absolute => self.dis_pc_word(mem),
            ZeroPage => self.dis_pc_byte(mem),
            AbsoluteIndexed(reg) => format!("{},{}", self.dis_pc_word(mem), self.dis_reg(reg)),
            ZeroPageIndexed(reg) => format!("{},{}", self.dis_pc_byte(mem), self.dis_reg(reg)),
            IndexedIndirect(reg) => format!("({},{})", self.dis_pc_byte(mem), self.dis_reg(reg)),
            IndirectIndexed(reg) => format!("({}),{}", self.dis_pc_byte(mem), self.dis_reg(reg)),
            Register(reg) => self.dis_reg(reg),
        }
    }

    fn dis_reg(&self, r: Register8) -> String {
        use crate::cpu::Register8::*;
        match r {
            A => "a".into(),
            X => "x".into(),
            Y => "y".into(),
            Sp => "s".into(),
            Status => "p".into(),
        }
    }

    fn dis_instruction<M: Memory>(
        &mut self,
        instruction: &str,
        mem: &mut M,
        am: AddressMode,
    ) -> String {
        format!("{} {}", instruction, self.dis_am(mem, am))
    }

    fn dis_branch<M: Memory>(&mut self, instruction: &str, mem: &mut M) -> String {
        format!("{} {}", instruction, self.next_pc_byte(mem) as i8)
    }

    ///////////////////
    // Instructions
    ///////////////////

    fn lda<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> String {
        self.dis_instruction("lda", mem, am)
    }

    fn ldx<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> String {
        self.dis_instruction("ldx", mem, am)
    }

    fn ldy<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> String {
        self.dis_instruction("ldy", mem, am)
    }

    fn sta<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> String {
        self.dis_instruction("sta", mem, am)
    }

    fn stx<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> String {
        self.dis_instruction("stx", mem, am)
    }

    fn sty<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> String {
        self.dis_instruction("sty", mem, am)
    }

    fn adc<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> String {
        self.dis_instruction("adc", mem, am)
    }

    fn sbc<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> String {
        self.dis_instruction("sbc", mem, am)
    }

    fn and<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> String {
        self.dis_instruction("and", mem, am)
    }

    fn ora<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> String {
        self.dis_instruction("ora", mem, am)
    }

    fn eor<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> String {
        self.dis_instruction("eor", mem, am)
    }

    fn sec(&mut self, _mem: &mut impl Memory) -> String {
        "sec".into()
    }

    fn clc(&mut self, _mem: &mut impl Memory) -> String {
        "clc".into()
    }

    fn sei(&mut self, _mem: &mut impl Memory) -> String {
        "sei".into()
    }

    fn cli(&mut self, _mem: &mut impl Memory) -> String {
        "cli".into()
    }

    fn sed(&mut self, _mem: &mut impl Memory) -> String {
        "sed".into()
    }

    fn cld(&mut self, _mem: &mut impl Memory) -> String {
        "cld".into()
    }

    fn clv(&mut self, _mem: &mut impl Memory) -> String {
        "clv".into()
    }

    fn jmp<M: Memory>(&mut self, mem: &mut M) -> String {
        format!("jmp {}", self.dis_pc_word(mem))
    }

    fn jmpi<M: Memory>(&mut self, mem: &mut M) -> String {
        format!("jmp ({})", self.dis_pc_word(mem))
    }

    fn bmi<M: Memory>(&mut self, mem: &mut M) -> String {
        self.dis_branch("bmi", mem)
    }

    fn bpl<M: Memory>(&mut self, mem: &mut M) -> String {
        self.dis_branch("bpl", mem)
    }

    fn bcc<M: Memory>(&mut self, mem: &mut M) -> String {
        self.dis_branch("bcc", mem)
    }

    fn bcs<M: Memory>(&mut self, mem: &mut M) -> String {
        self.dis_branch("bcs", mem)
    }

    fn beq<M: Memory>(&mut self, mem: &mut M) -> String {
        self.dis_branch("beq", mem)
    }

    fn bne<M: Memory>(&mut self, mem: &mut M) -> String {
        self.dis_branch("bne", mem)
    }

    fn bvs<M: Memory>(&mut self, mem: &mut M) -> String {
        self.dis_branch("bvs", mem)
    }

    fn bvc<M: Memory>(&mut self, mem: &mut M) -> String {
        self.dis_branch("bvc", mem)
    }

    fn cmp<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> String {
        self.dis_instruction("cmp", mem, am)
    }

    fn cpx<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> String {
        self.dis_instruction("cpx", mem, am)
    }

    fn cpy<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> String {
        self.dis_instruction("cpy", mem, am)
    }

    fn bit<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> String {
        self.dis_instruction("bit", mem, am)
    }

    fn inc<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> String {
        self.dis_instruction("inc", mem, am)
    }

    fn dec<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> String {
        self.dis_instruction("dec", mem, am)
    }

    fn inx(&mut self, _mem: &mut impl Memory) -> String {
        "inx".into()
    }

    fn iny(&mut self, _mem: &mut impl Memory) -> String {
        "iny".into()
    }

    fn dex(&mut self, _mem: &mut impl Memory) -> String {
        "dex".into()
    }

    fn dey(&mut self, _mem: &mut impl Memory) -> String {
        "dey".into()
    }

    fn tax(&mut self, _mem: &mut impl Memory) -> String {
        "tax".into()
    }

    fn txa(&mut self, _mem: &mut impl Memory) -> String {
        "txa".into()
    }

    fn tay(&mut self, _mem: &mut impl Memory) -> String {
        "tay".into()
    }

    fn tya(&mut self, _mem: &mut impl Memory) -> String {
        "tya".into()
    }

    fn tsx(&mut self, _mem: &mut impl Memory) -> String {
        "tsx".into()
    }

    fn txs(&mut self, _mem: &mut impl Memory) -> String {
        "txs".into()
    }

    fn jsr<M: Memory>(&mut self, mem: &mut M) -> String {
        format!("jsr {}", self.dis_pc_word(mem))
    }

    fn rts<M: Memory>(&mut self, _: &mut M) -> String {
        "rts".into()
    }

    fn pha<M: Memory>(&mut self, _: &mut M) -> String {
        "pha".into()
    }

    fn pla<M: Memory>(&mut self, _: &mut M) -> String {
        "pla".into()
    }

    fn php<M: Memory>(&mut self, _: &mut M) -> String {
        "php".into()
    }

    fn plp<M: Memory>(&mut self, _: &mut M) -> String {
        "plp".into()
    }

    fn lsr<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> String {
        self.dis_instruction("lsr", mem, am)
    }

    fn asl<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> String {
        self.dis_instruction("asl", mem, am)
    }

    fn ror<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> String {
        self.dis_instruction("ror", mem, am)
    }

    fn rol<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> String {
        self.dis_instruction("rol", mem, am)
    }

    fn brk<M: Memory>(&mut self, _mem: &mut M) -> String {
        "brk".into()
    }

    fn rti<M: Memory>(&mut self, _mem: &mut M) -> String {
        "rti".into()
    }

    fn nop(&mut self, _mem: &mut impl Memory, _am: AddressMode) -> String {
        "nop".into()
    }

    ///////////////////////////
    // Unofficial Instructions
    ///////////////////////////

    fn alr(&mut self, mem: &mut impl Memory) -> String {
        self.dis_instruction("alr", mem, AddressMode::Immediate)
    }

    fn anc(&mut self, mem: &mut impl Memory) -> String {
        self.dis_instruction("anc", mem, AddressMode::Immediate)
    }

    fn arr(&mut self, mem: &mut impl Memory) -> String {
        self.dis_instruction("arr", mem, AddressMode::Immediate)
    }

    fn axs(&mut self, mem: &mut impl Memory) -> String {
        self.dis_instruction("axs", mem, AddressMode::Immediate)
    }

    fn sax(&mut self, mem: &mut impl Memory, am: AddressMode) -> String {
        self.dis_instruction("sax", mem, am)
    }

    fn dcp(&mut self, mem: &mut impl Memory, am: AddressMode) -> String {
        self.dis_instruction("dcp", mem, am)
    }

    fn isc(&mut self, mem: &mut impl Memory, am: AddressMode) -> String {
        self.dis_instruction("isc", mem, am)
    }

    fn rla(&mut self, mem: &mut impl Memory, am: AddressMode) -> String {
        self.dis_instruction("rla", mem, am)
    }

    fn rra(&mut self, mem: &mut impl Memory, am: AddressMode) -> String {
        self.dis_instruction("rra", mem, am)
    }

    fn sre(&mut self, mem: &mut impl Memory, am: AddressMode) -> String {
        self.dis_instruction("sre", mem, am)
    }

    fn skb<M: Memory>(&mut self, mem: &mut M) -> String {
        format!("skb {}", self.dis_pc_byte(mem))
    }

    fn ign(&mut self, mem: &mut impl Memory, am: AddressMode) -> String {
        self.dis_instruction("ign", mem, am)
    }

    fn xaa<M: Memory>(&mut self, _mem: &mut M) -> String {
        "xaa".into()
    }

    fn lax<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> String {
        self.dis_instruction("lax", mem, am)
    }

    fn slo<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> String {
        self.dis_instruction("slo", mem, am)
    }

    fn ahx<M: Memory>(&mut self, mem: &mut M, am: AddressMode) -> String {
        self.dis_instruction("ahx", mem, am)
    }

    fn sya<M: Memory>(&mut self, mem: &mut M) -> String {
        self.dis_instruction("sya", mem, AddressMode::AbsoluteIndexed(Register8::X))
    }

    fn sxa<M: Memory>(&mut self, mem: &mut M) -> String {
        self.dis_instruction("sxa", mem, AddressMode::AbsoluteIndexed(Register8::Y))
    }
}
