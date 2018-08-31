// Used by Cpu and Disassembler to decode opcodes and handle the instructions
macro_rules! handle_opcode {
($opcode:expr, $this:ident, $mem:ident) => {
    match $opcode {
        0xA9 => $this.lda($mem, AddressMode::Immediate),
        0xA5 => $this.lda($mem, AddressMode::ZeroPage),
        0xB5 => $this.lda($mem, AddressMode::ZeroPageIndexed(Register8::X)),
        0xAD => $this.lda($mem, AddressMode::Absolute),
        0xBD => $this.lda($mem, AddressMode::AbsoluteIndexed(Register8::X)),
        0xB9 => $this.lda($mem, AddressMode::AbsoluteIndexed(Register8::Y)),
        0xA1 => $this.lda($mem, AddressMode::IndexedIndirect(Register8::X)),
        0xB1 => $this.lda($mem, AddressMode::IndirectIndexed(Register8::Y)),

        0xA2 => $this.ldx($mem, AddressMode::Immediate),
        0xA6 => $this.ldx($mem, AddressMode::ZeroPage),
        0xB6 => $this.ldx($mem, AddressMode::ZeroPageIndexed(Register8::Y)),
        0xAE => $this.ldx($mem, AddressMode::Absolute),
        0xBE => $this.ldx($mem, AddressMode::AbsoluteIndexed(Register8::Y)),

        0xA0 => $this.ldy($mem, AddressMode::Immediate),
        0xA4 => $this.ldy($mem, AddressMode::ZeroPage),
        0xB4 => $this.ldy($mem, AddressMode::ZeroPageIndexed(Register8::X)),
        0xAC => $this.ldy($mem, AddressMode::Absolute),
        0xBC => $this.ldy($mem, AddressMode::AbsoluteIndexed(Register8::X)),

        0x85 => $this.sta($mem, AddressMode::ZeroPage),
        0x95 => $this.sta($mem, AddressMode::ZeroPageIndexed(Register8::X)),
        0x8D => $this.sta($mem, AddressMode::Absolute),
        0x9D => $this.sta($mem, AddressMode::AbsoluteIndexed(Register8::X)),
        0x99 => $this.sta($mem, AddressMode::AbsoluteIndexed(Register8::Y)),
        0x81 => $this.sta($mem, AddressMode::IndexedIndirect(Register8::X)),
        0x91 => $this.sta($mem, AddressMode::IndirectIndexed(Register8::Y)),

        0x86 => $this.stx($mem, AddressMode::ZeroPage),
        0x96 => $this.stx($mem, AddressMode::ZeroPageIndexed(Register8::Y)),
        0x8E => $this.stx($mem, AddressMode::Absolute),

        0x84 => $this.sty($mem, AddressMode::ZeroPage),
        0x94 => $this.sty($mem, AddressMode::ZeroPageIndexed(Register8::X)),
        0x8C => $this.sty($mem, AddressMode::Absolute),

        0x69 => $this.adc($mem, AddressMode::Immediate),
        0x65 => $this.adc($mem, AddressMode::ZeroPage),
        0x75 => $this.adc($mem, AddressMode::ZeroPageIndexed(Register8::X)),
        0x6D => $this.adc($mem, AddressMode::Absolute),
        0x7D => $this.adc($mem, AddressMode::AbsoluteIndexed(Register8::X)),
        0x79 => $this.adc($mem, AddressMode::AbsoluteIndexed(Register8::Y)),
        0x61 => $this.adc($mem, AddressMode::IndexedIndirect(Register8::X)),
        0x71 => $this.adc($mem, AddressMode::IndirectIndexed(Register8::Y)),

        0xE9 => $this.sbc($mem, AddressMode::Immediate),
        0xE5 => $this.sbc($mem, AddressMode::ZeroPage),
        0xF5 => $this.sbc($mem, AddressMode::ZeroPageIndexed(Register8::X)),
        0xED => $this.sbc($mem, AddressMode::Absolute),
        0xFD => $this.sbc($mem, AddressMode::AbsoluteIndexed(Register8::X)),
        0xF9 => $this.sbc($mem, AddressMode::AbsoluteIndexed(Register8::Y)),
        0xE1 => $this.sbc($mem, AddressMode::IndexedIndirect(Register8::X)),
        0xF1 => $this.sbc($mem, AddressMode::IndirectIndexed(Register8::Y)),

        0x29 => $this.and($mem, AddressMode::Immediate),
        0x25 => $this.and($mem, AddressMode::ZeroPage),
        0x35 => $this.and($mem, AddressMode::ZeroPageIndexed(Register8::X)),
        0x2D => $this.and($mem, AddressMode::Absolute),
        0x3D => $this.and($mem, AddressMode::AbsoluteIndexed(Register8::X)),
        0x39 => $this.and($mem, AddressMode::AbsoluteIndexed(Register8::Y)),
        0x21 => $this.and($mem, AddressMode::IndexedIndirect(Register8::X)),
        0x31 => $this.and($mem, AddressMode::IndirectIndexed(Register8::Y)),

        0x09 => $this.ora($mem, AddressMode::Immediate),
        0x05 => $this.ora($mem, AddressMode::ZeroPage),
        0x15 => $this.ora($mem, AddressMode::ZeroPageIndexed(Register8::X)),
        0x0D => $this.ora($mem, AddressMode::Absolute),
        0x1D => $this.ora($mem, AddressMode::AbsoluteIndexed(Register8::X)),
        0x19 => $this.ora($mem, AddressMode::AbsoluteIndexed(Register8::Y)),
        0x01 => $this.ora($mem, AddressMode::IndexedIndirect(Register8::X)),
        0x11 => $this.ora($mem, AddressMode::IndirectIndexed(Register8::Y)),

        0x49 => $this.eor($mem, AddressMode::Immediate),
        0x45 => $this.eor($mem, AddressMode::ZeroPage),
        0x55 => $this.eor($mem, AddressMode::ZeroPageIndexed(Register8::X)),
        0x4D => $this.eor($mem, AddressMode::Absolute),
        0x5D => $this.eor($mem, AddressMode::AbsoluteIndexed(Register8::X)),
        0x59 => $this.eor($mem, AddressMode::AbsoluteIndexed(Register8::Y)),
        0x41 => $this.eor($mem, AddressMode::IndexedIndirect(Register8::X)),
        0x51 => $this.eor($mem, AddressMode::IndirectIndexed(Register8::Y)),

        0x38 => $this.sec($mem),
        0x18 => $this.clc($mem),
        0x78 => $this.sei($mem),
        0x58 => $this.cli($mem),
        0xF8 => $this.sed($mem),
        0xD8 => $this.cld($mem),
        0xB8 => $this.clv($mem),

        0x4C => $this.jmp($mem),
        0x6C => $this.jmpi($mem),
        0x30 => $this.bmi($mem),
        0x10 => $this.bpl($mem),
        0x90 => $this.bcc($mem),
        0xB0 => $this.bcs($mem),
        0xF0 => $this.beq($mem),
        0xD0 => $this.bne($mem),
        0x70 => $this.bvs($mem),
        0x50 => $this.bvc($mem),

        0xC9 => $this.cmp($mem, AddressMode::Immediate),
        0xC5 => $this.cmp($mem, AddressMode::ZeroPage),
        0xD5 => $this.cmp($mem, AddressMode::ZeroPageIndexed(Register8::X)),
        0xCD => $this.cmp($mem, AddressMode::Absolute),
        0xDD => $this.cmp($mem, AddressMode::AbsoluteIndexed(Register8::X)),
        0xD9 => $this.cmp($mem, AddressMode::AbsoluteIndexed(Register8::Y)),
        0xC1 => $this.cmp($mem, AddressMode::IndexedIndirect(Register8::X)),
        0xD1 => $this.cmp($mem, AddressMode::IndirectIndexed(Register8::Y)),

        0xE0 => $this.cpx($mem, AddressMode::Immediate),
        0xE4 => $this.cpx($mem, AddressMode::ZeroPage),
        0xEC => $this.cpx($mem, AddressMode::Absolute),

        0xC0 => $this.cpy($mem, AddressMode::Immediate),
        0xC4 => $this.cpy($mem, AddressMode::ZeroPage),
        0xCC => $this.cpy($mem, AddressMode::Absolute),

        0x24 => $this.bit($mem, AddressMode::ZeroPage),
        0x2C => $this.bit($mem, AddressMode::Absolute),

        0xE6 => $this.inc($mem, AddressMode::ZeroPage),
        0xF6 => $this.inc($mem, AddressMode::ZeroPageIndexed(Register8::X)),
        0xEE => $this.inc($mem, AddressMode::Absolute),
        0xFE => $this.inc($mem, AddressMode::AbsoluteIndexed(Register8::X)),

        0xC6 => $this.dec($mem, AddressMode::ZeroPage),
        0xD6 => $this.dec($mem, AddressMode::ZeroPageIndexed(Register8::X)),
        0xCE => $this.dec($mem, AddressMode::Absolute),
        0xDE => $this.dec($mem, AddressMode::AbsoluteIndexed(Register8::X)),

        0xE8 => $this.inx($mem),
        0xC8 => $this.iny($mem),
        0xCA => $this.dex($mem),
        0x88 => $this.dey($mem),

        0xAA => $this.tax($mem),
        0x8A => $this.txa($mem),
        0xA8 => $this.tay($mem),
        0x98 => $this.tya($mem),
        0xBA => $this.tsx($mem),
        0x9A => $this.txs($mem),

        0x20 => $this.jsr($mem),
        0x60 => $this.rts($mem),

        0x48 => $this.pha($mem),
        0x68 => $this.pla($mem),
        0x08 => $this.php($mem),
        0x28 => $this.plp($mem),

        0x4A => $this.lsr($mem, AddressMode::Register(Register8::A)),
        0x46 => $this.lsr($mem, AddressMode::ZeroPage),
        0x56 => $this.lsr($mem, AddressMode::ZeroPageIndexed(Register8::X)),
        0x4E => $this.lsr($mem, AddressMode::Absolute),
        0x5E => $this.lsr($mem, AddressMode::AbsoluteIndexed(Register8::X)),

        0x0A => $this.asl($mem, AddressMode::Register(Register8::A)),
        0x06 => $this.asl($mem, AddressMode::ZeroPage),
        0x16 => $this.asl($mem, AddressMode::ZeroPageIndexed(Register8::X)),
        0x0E => $this.asl($mem, AddressMode::Absolute),
        0x1E => $this.asl($mem, AddressMode::AbsoluteIndexed(Register8::X)),

        0x6A => $this.ror($mem, AddressMode::Register(Register8::A)),
        0x66 => $this.ror($mem, AddressMode::ZeroPage),
        0x76 => $this.ror($mem, AddressMode::ZeroPageIndexed(Register8::X)),
        0x6E => $this.ror($mem, AddressMode::Absolute),
        0x7E => $this.ror($mem, AddressMode::AbsoluteIndexed(Register8::X)),

        0x2A => $this.rol($mem, AddressMode::Register(Register8::A)),
        0x26 => $this.rol($mem, AddressMode::ZeroPage),
        0x36 => $this.rol($mem, AddressMode::ZeroPageIndexed(Register8::X)),
        0x2E => $this.rol($mem, AddressMode::Absolute),
        0x3E => $this.rol($mem, AddressMode::AbsoluteIndexed(Register8::X)),

        0x00 => $this.brk($mem),
        0x40 => $this.rti($mem),

        0xEA => $this.nop($mem),

        // Unofficial opcodes
        0x4B => $this.alr($mem),
        0x0B | 0x2B => $this.anc($mem),
        0x6B => $this.arr($mem),
        0xCB => $this.axs($mem),
        0xA3 => $this.lax($mem, AddressMode::IndexedIndirect(Register8::X)),
        0xA7 => $this.lax($mem, AddressMode::ZeroPage),
        0xAB => $this.lax($mem, AddressMode::Immediate),
        0xAF => $this.lax($mem, AddressMode::Absolute),
        0xB3 => $this.lax($mem, AddressMode::IndirectIndexed(Register8::Y)),
        0xB7 => $this.lax($mem, AddressMode::ZeroPageIndexed(Register8::Y)),
        0xBF => $this.lax($mem, AddressMode::AbsoluteIndexed(Register8::Y)),
        0x83 => $this.sax($mem, AddressMode::IndexedIndirect(Register8::X)),
        0x87 => $this.sax($mem, AddressMode::ZeroPage),
        0x8F => $this.sax($mem, AddressMode::Absolute),
        0x97 => $this.sax($mem, AddressMode::ZeroPageIndexed(Register8::Y)),
        0xC3 => $this.dcp($mem, AddressMode::IndexedIndirect(Register8::X)),
        0xC7 => $this.dcp($mem, AddressMode::ZeroPage),
        0xCF => $this.dcp($mem, AddressMode::Absolute),
        0xD3 => $this.dcp($mem, AddressMode::IndirectIndexed(Register8::Y)),
        0xD7 => $this.dcp($mem, AddressMode::ZeroPageIndexed(Register8::X)),
        0xDB => $this.dcp($mem, AddressMode::AbsoluteIndexed(Register8::Y)),
        0xDF => $this.dcp($mem, AddressMode::AbsoluteIndexed(Register8::X)),
        0xE3 => $this.isc($mem, AddressMode::IndexedIndirect(Register8::X)),
        0xE7 => $this.isc($mem, AddressMode::ZeroPage),
        0xEF => $this.isc($mem, AddressMode::Absolute),
        0xF3 => $this.isc($mem, AddressMode::IndirectIndexed(Register8::Y)),
        0xF7 => $this.isc($mem, AddressMode::ZeroPageIndexed(Register8::X)),
        0xFB => $this.isc($mem, AddressMode::AbsoluteIndexed(Register8::Y)),
        0xFF => $this.isc($mem, AddressMode::AbsoluteIndexed(Register8::X)),
        0x23 => $this.rla($mem, AddressMode::IndexedIndirect(Register8::X)),
        0x27 => $this.rla($mem, AddressMode::ZeroPage),
        0x2F => $this.rla($mem, AddressMode::Absolute),
        0x33 => $this.rla($mem, AddressMode::IndirectIndexed(Register8::Y)),
        0x37 => $this.rla($mem, AddressMode::ZeroPageIndexed(Register8::X)),
        0x3B => $this.rla($mem, AddressMode::AbsoluteIndexed(Register8::Y)),
        0x3F => $this.rla($mem, AddressMode::AbsoluteIndexed(Register8::X)),
        0x63 => $this.rra($mem, AddressMode::IndexedIndirect(Register8::X)),
        0x67 => $this.rra($mem, AddressMode::ZeroPage),
        0x6F => $this.rra($mem, AddressMode::Absolute),
        0x73 => $this.rra($mem, AddressMode::IndirectIndexed(Register8::Y)),
        0x77 => $this.rra($mem, AddressMode::ZeroPageIndexed(Register8::X)),
        0x7B => $this.rra($mem, AddressMode::AbsoluteIndexed(Register8::Y)),
        0x7F => $this.rra($mem, AddressMode::AbsoluteIndexed(Register8::X)),
        0x03 => $this.slo($mem, AddressMode::IndexedIndirect(Register8::X)),
        0x07 => $this.slo($mem, AddressMode::ZeroPage),
        0x0F => $this.slo($mem, AddressMode::Absolute),
        0x13 => $this.slo($mem, AddressMode::IndirectIndexed(Register8::Y)),
        0x17 => $this.slo($mem, AddressMode::ZeroPageIndexed(Register8::X)),
        0x1B => $this.slo($mem, AddressMode::AbsoluteIndexed(Register8::Y)),
        0x1F => $this.slo($mem, AddressMode::AbsoluteIndexed(Register8::X)),
        0x43 => $this.sre($mem, AddressMode::IndexedIndirect(Register8::X)),
        0x47 => $this.sre($mem, AddressMode::ZeroPage),
        0x4F => $this.sre($mem, AddressMode::Absolute),
        0x53 => $this.sre($mem, AddressMode::IndirectIndexed(Register8::Y)),
        0x57 => $this.sre($mem, AddressMode::ZeroPageIndexed(Register8::X)),
        0x5B => $this.sre($mem, AddressMode::AbsoluteIndexed(Register8::Y)),
        0x5F => $this.sre($mem, AddressMode::AbsoluteIndexed(Register8::X)),
        0xEB => $this.sbc($mem, AddressMode::Immediate),
        0x1A | 0x3A | 0x5A | 0x7A | 0xDA | 0xFA => $this.nop($mem),
        0x80 | 0x82 | 0x89 | 0xC2 | 0xE2 => $this.skb($mem),
        0x0C => $this.ign($mem, AddressMode::Absolute),
        0x1C | 0x3C | 0x5C | 0x7C | 0xDC | 0xFC => $this.ign($mem, AddressMode::AbsoluteIndexed(Register8::X)),
        0x04 | 0x44 | 0x64 => $this.ign($mem, AddressMode::ZeroPage),
        0x14 | 0x34 | 0x54 | 0x74 | 0xD4 | 0xF4 => $this.ign($mem, AddressMode::ZeroPageIndexed(Register8::X)),
        0x8B => $this.xaa($mem),
        0x93 => $this.ahx($mem, AddressMode::ZeroPageIndexed(Register8::Y)),
        0x9F => $this.ahx($mem, AddressMode::AbsoluteIndexed(Register8::Y)),
        0x9C => $this.sya($mem),
        0x9E => $this.sxa($mem),

        _ => panic!("Unimplemented op code {:02X}", $opcode),
    }
}
}
