use std::ops::{Deref, DerefMut};

use memory::Memory;

pub struct Ppu {
    regs: Regs,
}

impl Ppu {
    pub fn new() -> Ppu {
        Ppu {
            regs: Regs::new(),
        }
    }
}

impl Memory for Ppu {
    fn read_byte(&self, address: u16) -> u8 {
        //TODO: Implement
        0
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        //TODO: Implement
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

struct Regs {
    ppu_ctrl: PpuCtrl,
    ppu_mask: PpuMask,
    ppu_status: PpuStatus,
    oam_addr: u8,
    oam_data: u8,
    ppu_scroll: u8,
    ppu_addr: u8,
    ppu_data: u8,
    oam_dma: u8,
}

impl Regs {
    fn new() -> Regs {
        Regs {
            ppu_ctrl: PpuCtrl { val: 0 },
            ppu_mask: PpuMask::NONE,
            ppu_status: PpuStatus::NONE,
            oam_addr: 0,
            oam_data: 0,
            ppu_scroll: 0,
            ppu_addr: 0,
            ppu_data: 0,
            oam_dma: 0,
        }
    }
}
