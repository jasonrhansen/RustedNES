use memory::Memory;
use cpu::{AddressMode, Register8};

pub struct Disassembler<'a, M: Memory + 'a> {
    pc: u16,
    mem: &'a mut M,
}

impl<'a, M> Memory for Disassembler<'a, M>
    where M: Memory + 'a {
    fn read_byte(&mut self, address: u16) -> u8 {
        self.mem.read_byte(address)
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        self.mem.write_byte(address, value)
    }
}

impl<'a, M> Disassembler<'a, M>
    where M: Memory + 'a {
    pub fn new(pc: u16, mem: &mut M) -> Disassembler<M> {
        Disassembler {
            pc,
            mem
        }
    }

    pub fn disassemble_next(&mut self) -> String {
        let op = self.next_pc_byte();
        handle_opcode!(op, self)
    }

    fn next_pc_byte(&mut self) -> u8 {
        let pc = self.pc;
        let b = self.read_byte(pc);
        self.pc += 1;
        b
    }

    fn next_pc_word(&mut self) -> u16 {
        let pc = self.pc;
        let w = self.read_word(pc);
        self.pc += 2;
        w
    }

    fn dis_pc_byte(&mut self) -> String {
        format!("${:02X}", self.next_pc_byte())
    }

    fn dis_pc_word(&mut self) -> String {
        format!("${:04X}", self.next_pc_word())
    }

    fn dis_am(&mut self, am: AddressMode) -> String {
        use cpu::AddressMode::*;
        match am {
            Immediate => format!("#{}", self.dis_pc_byte()).into(),
            Absolute => self.dis_pc_word(),
            ZeroPage => self.dis_pc_byte(),
            AbsoluteIndexed(reg) => {
                format!("{},{}", self.dis_pc_word(), self.dis_reg(reg))
            },
            ZeroPageIndexed(reg) => {
                format!("{},{}", self.dis_pc_byte(), self.dis_reg(reg))
            },
            IndexedIndirect(reg) => {
                format!("({},{})", self.dis_pc_byte(), self.dis_reg(reg))
            },
            IndirectIndexed(reg) => {
                format!("({}),{}", self.dis_pc_byte(), self.dis_reg(reg))
            },
            Register(reg) => self.dis_reg(reg),
        }
    }

    fn dis_reg(&self, r: Register8) -> String {
        use cpu::Register8::*;
        match r {
            A => "a".into(),
            X => "x".into(),
            Y => "y".into(),
            Sp => "s".into(),
            Status => "p".into(),
        }
    }

    fn dis_instruction(&mut self, instruction: &str, am: AddressMode) -> String {
        format!("{} {}", instruction, self.dis_am(am)).into()
    }

    fn dis_branch(&mut self, instruction: &str) -> String {
        format!("{} {}", instruction, self.dis_pc_byte()).into()
    }

    ///////////////////
    // Instructions
    ///////////////////

    fn lda(&mut self, am: AddressMode) -> String {
        self.dis_instruction("lda", am)
    }

    fn ldx(&mut self, am: AddressMode) -> String {
        self.dis_instruction("ldx", am)
    }

    fn ldy(&mut self, am: AddressMode) -> String {
        self.dis_instruction("ldy", am)
    }

    fn sta(&mut self, am: AddressMode) -> String {
        self.dis_instruction("sta", am)
    }

    fn stx(&mut self, am: AddressMode) -> String {
        self.dis_instruction("stx", am)
    }

    fn sty(&mut self, am: AddressMode) -> String {
        self.dis_instruction("sty", am)
    }

    fn adc(&mut self, am: AddressMode) -> String {
        self.dis_instruction("adc", am)
    }

    fn sbc(&mut self, am: AddressMode) -> String {
        self.dis_instruction("sbc", am)
    }

    fn and(&mut self, am: AddressMode) -> String {
        self.dis_instruction("and", am)
    }

    fn ora(&mut self, am: AddressMode) -> String {
        self.dis_instruction("ora", am)
    }

    fn eor(&mut self, am: AddressMode) -> String {
        self.dis_instruction("eor", am)
    }

    fn sec(&mut self) -> String {
        "sec".into()
    }

    fn clc(&mut self) -> String {
        "clc".into()
    }

    fn sei(&mut self) -> String {
        "sei".into()
    }

    fn cli(&mut self) -> String {
        "cli".into()
    }

    fn sed(&mut self) -> String {
        "sed".into()
    }

    fn cld(&mut self) -> String {
        "cld".into()
    }

    fn clv(&mut self) -> String {
        "clv".into()
    }

    fn jmp(&mut self) -> String {
        format!("jmp {}", self.dis_pc_word()).into()
    }

    fn jmpi(&mut self) -> String {
        format!("jmp ({})", self.dis_pc_word()).into()
    }

    fn bmi(&mut self) -> String {
        self.dis_branch("bmi")
    }

    fn bpl(&mut self) -> String {
        self.dis_branch("bpl")
    }

    fn bcc(&mut self) -> String {
        self.dis_branch("bcc")
    }

    fn bcs(&mut self) -> String {
        self.dis_branch("bcs")
    }

    fn beq(&mut self) -> String {
        self.dis_branch("beq")
    }

    fn bne(&mut self) -> String {
        self.dis_branch("bne")
    }

    fn bvs(&mut self) -> String {
        self.dis_branch("bvs")
    }

    fn bvc(&mut self) -> String {
        self.dis_branch("bvc")
    }

    fn cmp(&mut self, am: AddressMode) -> String {
        self.dis_instruction("cmp", am)
    }

    fn cpx(&mut self, am: AddressMode) -> String {
        self.dis_instruction("cpx", am)
    }

    fn cpy(&mut self, am: AddressMode) -> String {
        self.dis_instruction("cpy", am)
    }

    fn bit(&mut self, am: AddressMode) -> String {
        self.dis_instruction("bit", am)
    }

    fn inc(&mut self, am: AddressMode) -> String {
        self.dis_instruction("inc", am)
    }

    fn dec(&mut self, am: AddressMode) -> String {
        self.dis_instruction("dec", am)
    }

    fn inx(&mut self) -> String {
        "inx".into()
    }

    fn iny(&mut self) -> String {
        "iny".into()
    }

    fn dex(&mut self) -> String {
        "dex".into()
    }

    fn dey(&mut self) -> String {
        "dey".into()
    }

    fn tax(&mut self) -> String {
        "tax".into()
    }

    fn txa(&mut self) -> String {
        "txa".into()
    }

    fn tay(&mut self) -> String {
        "tay".into()
    }

    fn tya(&mut self) -> String {
        "tya".into()
    }

    fn tsx(&mut self) -> String {
        "tsx".into()
    }

    fn txs(&mut self) -> String {
        "txs".into()
    }

    fn jsr(&mut self) -> String {
        format!("jsr {}", self.dis_pc_word()).into()
    }

    fn rts(&mut self) -> String {
        "rts".into()
    }

    fn pha(&mut self) -> String {
        "pha".into()
    }

    fn pla(&mut self) -> String {
        "pla".into()
    }

    fn php(&mut self) -> String {
        "php".into()
    }

    fn plp(&mut self) -> String {
        "plp".into()
    }

    fn lsr(&mut self, am: AddressMode) -> String {
        self.dis_instruction("cpy", am)
    }

    fn asl(&mut self, am: AddressMode) -> String {
        self.dis_instruction("asl", am)
    }

    fn ror(&mut self, am: AddressMode) -> String {
        self.dis_instruction("ror", am)
    }

    fn rol(&mut self, am: AddressMode) -> String {
        self.dis_instruction("rol", am)
    }

    fn brk(&mut self) -> String {
        "brk".into()
    }

    fn rti(&mut self) -> String {
        "rti".into()
    }

    fn nop(&mut self) -> String {
        "nop".into()
    }

    ///////////////////////////
    // Unofficial Instructions
    ///////////////////////////

    fn nop_2_bytes(&mut self) -> String {
        format!("nop2 {}", self.dis_pc_byte()).into()
    }

    fn xaa(&mut self) -> String {
        "xaa".into()
    }

    // Used by "Super Cars (U)"
    fn lax(&mut self, am: AddressMode) -> String {
        self.dis_instruction("lax", am)
    }

    fn slo(&mut self, am: AddressMode) -> String {
        self.dis_instruction("slo", am)
    }
}

