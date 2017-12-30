use std::ops::{Deref, DerefMut};
use std::default::Default;

use memory::Memory;

pub struct Ppu {
    regs: Regs,
    oam: Oam,

    // The PPU has an internal data bus that it uses for communication with the CPU.
    // This bus, called _io_db in Visual 2C02 and PPUGenLatch in FCEUX,[1] behaves as an
    // 8-bit dynamic latch due to capacitance of very long traces that run to various parts
    // of the PPU. Writing any value to any PPU port, even to the nominally read-only
    // PPUSTATUS, will fill this latch. Reading any readable port (PPUSTATUS, OAMDATA, or PPUDATA)
    // also fills the latch with the bits read. Reading a nominally "write-only" register returns
    // the latch's current value, as do the unused bits of PPUSTATUS.
    ppu_gen_latch: u8,
}

impl Ppu {
    pub fn new() -> Ppu {
        Ppu {
            regs: Regs::new(),
            oam: Oam::new(),
            ppu_gen_latch: 0,
        }
    }

    fn read_oam_byte(&self) -> u8 {
        self.oam[self.regs.oam_addr as usize]
    }

    fn write_oam_byte(&mut self, val: u8) {
        // TODO: Ignore writes during rendering
        // http://wiki.nesdev.com/w/index.php/PPU_registers#OAM_data_.28.242004.29_.3C.3E_read.2Fwrite

        self.oam[self.regs.oam_addr as usize] = val;
        self.regs.oam_addr += 1;
    }

    fn read_ppu_data_byte(&self) -> u8 {
        // TODO: Implement
        0
    }

    fn write_ppu_data_byte(&mut self, val: u8) {
        // TODO: Implement
    }
}

impl Memory for Ppu {
    fn read_byte(&mut self, address: u16) -> u8 {
        if !(0x2000 <= address && address < 0x4000) {
            panic!("Invalid read from PPU, address: {:X}", address)
        }

        let address = address & 0x2007;

        let val = match address & 0x2007 {
            0x2002 => *self.regs.ppu_status | (self.ppu_gen_latch & 0x1F),
            0x2004 => self.read_oam_byte(),
            0x2007 => self.read_ppu_data_byte(),
            _ => self.ppu_gen_latch,
        };

        self.ppu_gen_latch = val;

        val
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        if !(0x2000 <= address && address < 0x4000) {
            panic!("Invalid write in PPU, address: {:X}, value: {}", address, value)
        }

        let address = address & 0x2007;

        self.ppu_gen_latch = value;

        match address & 0x2007 {
            0x2000 => *self.regs.ppu_ctrl = value,
            0x2001 => *self.regs.ppu_mask = value,
            0x2003 => self.regs.oam_addr = value,
            0x2004 => self.write_oam_byte(value),
            0x2005 => self.regs.ppu_scroll.write_byte(value),
            0x2006 => self.regs.ppu_addr.write_byte(value),
            0x2007 => self.write_ppu_data_byte(value),
            _ => ()
        }
    }
}

//VRAM address increment per CPU read/write of PPUDATA
enum VramAddressIncrement {
    Add1Accross,         // Add 1, going across
    Add32Down,           // Add 32, going down
}

enum SpriteSize {
    Size8x8,
    Size8x16,
}

// PPU master/slave select
enum MasterSlaveSelect {
    ReadBackdrop,        // Read backdrop from EXT pins
    OutputColor,         // Output color on EXT pins
}

struct PpuCtrl { val: u8 }

impl PpuCtrl {
    fn base_name_table_address(&self) -> u16 {
        match self.val & 0x03 {
            0 => 0x2000,
            1 => 0x2400,
            2 => 0x2800,
            3 => 0x2C00,
            _ => 0, // Unreachable
        }
    }

    fn vram_address_increment(&self) -> VramAddressIncrement {
        if (self.val & 0x04) == 0 {
            VramAddressIncrement::Add1Accross
        } else {
            VramAddressIncrement::Add32Down
        }
    }

    // For 8x8 sprites (ignored in 8x16 mode)
    fn sprite_pattern_table_address(&self) -> u16 {
        if (self.val & 0x08) == 0 { 0x0000 } else { 0x1000 }
    }

    fn background_pattern_table_address(&self) -> u16 {
        if (self.val & 0x10) == 0 { 0x0000 } else { 0x1000 }
    }

    fn sprite_size(&self) -> SpriteSize {
        if (self.val & 0x20) == 0 {
            SpriteSize::Size8x8
        } else {
            SpriteSize::Size8x16
        }
    }

    fn master_slave_select(&self) -> MasterSlaveSelect {
        if (self.val & 0x40) == 0 {
            MasterSlaveSelect::ReadBackdrop
        } else {
            MasterSlaveSelect::OutputColor
        }
    }

    // Generate an NMI at the start of the vertical blanking interval
    fn generate_nmi_vblank(&self) -> bool {
        (self.val & 0x80) != 0
    }
}

impl Deref for PpuCtrl {
    type Target = u8;

    fn deref(&self) -> &u8 {
        &self.val
    }
}

impl DerefMut for PpuCtrl {
    fn deref_mut(&mut self) -> &mut u8 {
        &mut self.val
    }
}

bitflags! {
    struct PpuMask: u8 {
        const NONE                   = 0;

        // Greyscale (0: normal color, 1: produce a greyscale display)
        const GREYSCALE              = 1 << 0;

        // 1: Show background in leftmost 8 pixels of screen, 0: Hide
        const SHOW_BACKGROUND_LEFT_8 = 1 << 1;

        // 1: Show sprites in leftmost 8 pixels of screen, 0: Hide
        const SHOW_SPRITES_LEFT_8    = 1 << 2;

        const SHOW_BACKGROUND        = 1 << 3;
        const SHOW_SPRITES           = 1 << 4;

        // On PAL, red and green bits are switched
        const EMPHASIZE_RED          = 1 << 5;
        const EMPHASIZE_GREEN        = 1 << 6;
        const EMPHASIZE_BLUE         = 1 << 7;
    }
}

impl Deref for PpuMask {
    type Target = u8;

    fn deref(&self) -> &u8 {
        &self.bits
    }
}

impl DerefMut for PpuMask {
    fn deref_mut(&mut self) -> &mut u8 {
        &mut self.bits
    }
}

bitflags! {
    struct PpuStatus: u8 {
        const NONE            = 0;
        const SPRITE_OVERFLOW = 1 << 5;
        const SPRITE_ZERO_HIT = 1 << 6;
        const VBLANK_STARTED  = 1 << 7;
    }
}

impl Deref for PpuStatus {
    type Target = u8;

    fn deref(&self) -> &u8 {
        &self.bits
    }
}

impl DerefMut for PpuStatus {
    fn deref_mut(&mut self) -> &mut u8 {
        &mut self.bits
    }
}

enum PpuScrollAxis {
    X,
    Y,
}

struct PpuScroll {
    position_x: u8,
    position_y: u8,
    next_axis: PpuScrollAxis
}

impl PpuScroll {
    fn write_byte(&mut self, val: u8) {
        use self::PpuScrollAxis::*;
        match self.next_axis {
            X => {
                self.position_x = val;
                self.next_axis = Y;
            }
            Y => {
                self.position_y = val;
                self.next_axis = X;
            }
        }
    }
}

impl Default for PpuScroll {
    fn default() -> PpuScroll {
        PpuScroll {
            position_x: 0,
            position_y: 0,
            next_axis: PpuScrollAxis::X,
        }
    }
}

enum WordByte {
    HI,
    LO,
}

struct PpuAddr {
    address: u16,
    next_byte: WordByte,
}

impl PpuAddr {
    fn write_byte(&mut self, val: u8) {
        use self::WordByte::*;
        match self.next_byte {
            HI => {
                self.address = (self.address & 0x0F) | ((val as u16) << 8);
                self.next_byte = LO;
            }
            LO => {
                self.address = (self.address & 0xF0) | (val as u16);
                self.next_byte = HI;
            }
        }
    }
}

impl Default for PpuAddr {
    fn default() -> PpuAddr {
        PpuAddr {
            address: 0,
            next_byte: WordByte::HI,
        }
    }
}

struct Regs {
    ppu_ctrl: PpuCtrl,
    ppu_mask: PpuMask,
    ppu_status: PpuStatus,
    oam_addr: u8,
    ppu_scroll: PpuScroll,
    ppu_addr: PpuAddr,
}

impl Regs {
    fn new() -> Regs {
        Regs {
            ppu_ctrl: PpuCtrl { val: 0 },
            ppu_mask: PpuMask::NONE,
            ppu_status: PpuStatus::NONE,
            oam_addr: 0,
            ppu_scroll: PpuScroll::default(),
            ppu_addr: PpuAddr::default(),
        }
    }
}

// 64 sprites, each sprite uses 4 bytes
const OAM_SIZE: usize = 64 * 4;

struct Oam {
    buf: [u8; OAM_SIZE],
}

impl Oam {
    fn new() -> Oam {
        Oam {
           buf: [0u8; OAM_SIZE],
        }
    }
}

impl Memory for Oam {
    fn read_byte(&mut self, address: u16) -> u8 {
        self[address as usize]
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        self[address as usize] = value;
    }
}

impl Deref for Oam {
    type Target = [u8; 64 * 4];

    fn deref(&self) -> &Self::Target {
        &self.buf
    }
}

impl DerefMut for Oam {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.buf
    }
}
