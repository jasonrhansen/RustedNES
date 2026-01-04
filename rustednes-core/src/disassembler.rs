use crate::cpu::{AddressMode, Register8};
use crate::system_bus::SystemBus;

pub struct Disassembler {
    pub pc: u16,
}

impl Disassembler {
    pub fn new(pc: u16) -> Disassembler {
        Disassembler { pc }
    }

    pub fn disassemble_next(&mut self, bus: &mut SystemBus) -> String {
        let op = self.next_pc_byte(bus);
        handle_opcode!(op, self, bus)
    }

    fn next_pc_byte(&mut self, bus: &mut SystemBus) -> u8 {
        let pc = self.pc;
        let b = bus.read_byte(pc);
        self.pc += 1;
        b
    }

    fn next_pc_word(&mut self, bus: &mut SystemBus) -> u16 {
        let pc = self.pc;
        let w = bus.read_word(pc);
        self.pc += 2;
        w
    }

    fn dis_pc_byte(&mut self, bus: &mut SystemBus) -> String {
        format!("${:02x}", self.next_pc_byte(bus))
    }

    fn dis_pc_word(&mut self, bus: &mut SystemBus) -> String {
        format!("${:04x}", self.next_pc_word(bus))
    }

    fn dis_am(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        use crate::cpu::AddressMode::*;
        match am {
            Immediate => format!("#{}", self.dis_pc_byte(bus)),
            Absolute => self.dis_pc_word(bus),
            ZeroPage => self.dis_pc_byte(bus),
            AbsoluteIndexed(reg) => format!("{},{}", self.dis_pc_word(bus), self.dis_reg(reg)),
            ZeroPageIndexed(reg) => format!("{},{}", self.dis_pc_byte(bus), self.dis_reg(reg)),
            IndexedIndirect(reg) => format!("({},{})", self.dis_pc_byte(bus), self.dis_reg(reg)),
            IndirectIndexed(reg) => format!("({}),{}", self.dis_pc_byte(bus), self.dis_reg(reg)),
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

    fn dis_instruction(
        &mut self,
        instruction: &str,
        bus: &mut SystemBus,
        am: AddressMode,
    ) -> String {
        format!("{} {}", instruction, self.dis_am(bus, am))
    }

    fn dis_branch(&mut self, instruction: &str, bus: &mut SystemBus) -> String {
        format!("{} {}", instruction, self.next_pc_byte(bus) as i8)
    }

    ///////////////////
    // Instructions
    ///////////////////

    fn lda(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("lda", bus, am)
    }

    fn ldx(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("ldx", bus, am)
    }

    fn ldy(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("ldy", bus, am)
    }

    fn sta(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("sta", bus, am)
    }

    fn stx(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("stx", bus, am)
    }

    fn sty(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("sty", bus, am)
    }

    fn adc(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("adc", bus, am)
    }

    fn sbc(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("sbc", bus, am)
    }

    fn and(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("and", bus, am)
    }

    fn ora(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("ora", bus, am)
    }

    fn eor(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("eor", bus, am)
    }

    fn sec(&mut self, _bus: &mut SystemBus) -> String {
        "sec".into()
    }

    fn clc(&mut self, _bus: &mut SystemBus) -> String {
        "clc".into()
    }

    fn sei(&mut self, _bus: &mut SystemBus) -> String {
        "sei".into()
    }

    fn cli(&mut self, _bus: &mut SystemBus) -> String {
        "cli".into()
    }

    fn sed(&mut self, _bus: &mut SystemBus) -> String {
        "sed".into()
    }

    fn cld(&mut self, _bus: &mut SystemBus) -> String {
        "cld".into()
    }

    fn clv(&mut self, _bus: &mut SystemBus) -> String {
        "clv".into()
    }

    fn jmp(&mut self, bus: &mut SystemBus) -> String {
        format!("jmp {}", self.dis_pc_word(bus))
    }

    fn jmpi(&mut self, bus: &mut SystemBus) -> String {
        format!("jmp ({})", self.dis_pc_word(bus))
    }

    fn bmi(&mut self, bus: &mut SystemBus) -> String {
        self.dis_branch("bmi", bus)
    }

    fn bpl(&mut self, bus: &mut SystemBus) -> String {
        self.dis_branch("bpl", bus)
    }

    fn bcc(&mut self, bus: &mut SystemBus) -> String {
        self.dis_branch("bcc", bus)
    }

    fn bcs(&mut self, bus: &mut SystemBus) -> String {
        self.dis_branch("bcs", bus)
    }

    fn beq(&mut self, bus: &mut SystemBus) -> String {
        self.dis_branch("beq", bus)
    }

    fn bne(&mut self, bus: &mut SystemBus) -> String {
        self.dis_branch("bne", bus)
    }

    fn bvs(&mut self, bus: &mut SystemBus) -> String {
        self.dis_branch("bvs", bus)
    }

    fn bvc(&mut self, bus: &mut SystemBus) -> String {
        self.dis_branch("bvc", bus)
    }

    fn cmp(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("cmp", bus, am)
    }

    fn cpx(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("cpx", bus, am)
    }

    fn cpy(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("cpy", bus, am)
    }

    fn bit(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("bit", bus, am)
    }

    fn inc(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("inc", bus, am)
    }

    fn dec(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("dec", bus, am)
    }

    fn inx(&mut self, _bus: &mut SystemBus) -> String {
        "inx".into()
    }

    fn iny(&mut self, _bus: &mut SystemBus) -> String {
        "iny".into()
    }

    fn dex(&mut self, _bus: &mut SystemBus) -> String {
        "dex".into()
    }

    fn dey(&mut self, _bus: &mut SystemBus) -> String {
        "dey".into()
    }

    fn tax(&mut self, _bus: &mut SystemBus) -> String {
        "tax".into()
    }

    fn txa(&mut self, _bus: &mut SystemBus) -> String {
        "txa".into()
    }

    fn tay(&mut self, _bus: &mut SystemBus) -> String {
        "tay".into()
    }

    fn tya(&mut self, _bus: &mut SystemBus) -> String {
        "tya".into()
    }

    fn tsx(&mut self, _bus: &mut SystemBus) -> String {
        "tsx".into()
    }

    fn txs(&mut self, _bus: &mut SystemBus) -> String {
        "txs".into()
    }

    fn jsr(&mut self, bus: &mut SystemBus) -> String {
        format!("jsr {}", self.dis_pc_word(bus))
    }

    fn rts(&mut self, _: &mut SystemBus) -> String {
        "rts".into()
    }

    fn pha(&mut self, _: &mut SystemBus) -> String {
        "pha".into()
    }

    fn pla(&mut self, _: &mut SystemBus) -> String {
        "pla".into()
    }

    fn php(&mut self, _: &mut SystemBus) -> String {
        "php".into()
    }

    fn plp(&mut self, _: &mut SystemBus) -> String {
        "plp".into()
    }

    fn lsr(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("lsr", bus, am)
    }

    fn asl(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("asl", bus, am)
    }

    fn ror(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("ror", bus, am)
    }

    fn rol(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("rol", bus, am)
    }

    fn brk(&mut self, _bus: &mut SystemBus) -> String {
        "brk".into()
    }

    fn rti(&mut self, _bus: &mut SystemBus) -> String {
        "rti".into()
    }

    fn nop(&mut self, _bus: &mut SystemBus, _am: AddressMode) -> String {
        "nop".into()
    }

    ///////////////////////////
    // Unofficial Instructions
    ///////////////////////////

    fn alr(&mut self, bus: &mut SystemBus) -> String {
        self.dis_instruction("alr", bus, AddressMode::Immediate)
    }

    fn anc(&mut self, bus: &mut SystemBus) -> String {
        self.dis_instruction("anc", bus, AddressMode::Immediate)
    }

    fn arr(&mut self, bus: &mut SystemBus) -> String {
        self.dis_instruction("arr", bus, AddressMode::Immediate)
    }

    fn axs(&mut self, bus: &mut SystemBus) -> String {
        self.dis_instruction("axs", bus, AddressMode::Immediate)
    }

    fn sax(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("sax", bus, am)
    }

    fn dcp(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("dcp", bus, am)
    }

    fn isc(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("isc", bus, am)
    }

    fn rla(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("rla", bus, am)
    }

    fn rra(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("rra", bus, am)
    }

    fn sre(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("sre", bus, am)
    }

    fn skb(&mut self, bus: &mut SystemBus) -> String {
        format!("skb {}", self.dis_pc_byte(bus))
    }

    fn ign(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("ign", bus, am)
    }

    fn xaa(&mut self, _bus: &mut SystemBus) -> String {
        "xaa".into()
    }

    fn lax(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("lax", bus, am)
    }

    fn slo(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("slo", bus, am)
    }

    fn ahx(&mut self, bus: &mut SystemBus, am: AddressMode) -> String {
        self.dis_instruction("ahx", bus, am)
    }

    fn sya(&mut self, bus: &mut SystemBus) -> String {
        self.dis_instruction("sya", bus, AddressMode::AbsoluteIndexed(Register8::X))
    }

    fn sxa(&mut self, bus: &mut SystemBus) -> String {
        self.dis_instruction("sxa", bus, AddressMode::AbsoluteIndexed(Register8::Y))
    }
}
