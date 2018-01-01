#![feature(associated_consts)]

use std::cell::RefCell;
use std::default::Default;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use cpu::{Cpu, Interrupt};
use mapper::Mapper;
use memory::Memory;

pub struct Ppu {
    cycles: u64,
    regs: Regs,

    // When reading while the VRAM address is in the range 0-$3EFF (i.e., before the palettes),
    // the read will return the contents of an internal read buffer. This internal buffer is
    // updated only when reading PPUDATA, and so is preserved across frames. After the CPU reads
    // and gets the contents of the internal buffer, the PPU will immediately update the internal
    // buffer with the byte at the current VRAM address. Thus, after setting the VRAM address, one
    // should first read this register and discard the result.
    // Reading palette data from $3F00-$3FFF works differently. The palette data is placed
    // immediately on the data bus, and hence no dummy read is required. Reading the palettes still
    // updates the internal buffer though, but the data placed in it is the mirrored nametable data
    // that would appear "underneath" the palette.
    ppu_data_read_buffer: u8,

    // PPU address space
    mem: MemMap,

    // Object Attribute Memory
    oam: Oam,

    // Sprites to draw on current scanline
    sprites: [Option<Sprite>; 8],

    scanline: i16,

    // The cycle that the current scanline started at
    scanline_start_cycle: u64,

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
    pub fn new(mapper: Rc<RefCell<Box<Mapper>>>) -> Ppu {
        Ppu {
            cycles: 0,
            regs: Regs::new(),
            ppu_data_read_buffer: 0,
            mem: MemMap::new(mapper),
            oam: Oam::new(),
            sprites: Default::default(),
            scanline: 0,
            scanline_start_cycle: 0,
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

    fn read_ppu_data_byte(&mut self) -> u8 {
        let address = *self.regs.ppu_addr;

        let read_buffer = self.ppu_data_read_buffer;
        self.ppu_data_read_buffer = self.mem.read_byte(address);
        if address < PaletteRam::START_ADDRESS {
            // Return contents of read buffer before the read.
            read_buffer
        } else {
            // Palette data is returned immediately. No dummy read is required.
            self.ppu_data_read_buffer
        }
    }

    fn write_ppu_data_byte(&mut self, val: u8) {
        let address = *self.regs.ppu_addr;
        self.mem.write_byte(address, val)
    }

    fn is_sprite_at_y_on_scanline(&mut self, y: u8) -> bool {
        if self.scanline < 0 {
            return false;
        }

        let scanline = self.scanline as u16;
        let y = y as u16;
        let height = self.regs.ppu_ctrl.sprite_size().height() as u16;

        y < scanline && scanline <= y + height
    }

    fn evaluate_sprites_for_scanline(&mut self) {
        let mut count = 0;

        let mut n = 0;

        while n < 64 {
            let index = 4 * n;
            let y = self.oam[index];

            if self.is_sprite_at_y_on_scanline(y) {
                self.sprites[count] =
                    Some(Sprite::from_oam_bytes(&self.regs.ppu_ctrl, &self.oam[index..index+5]));
                count += 1;
            }

            n += 1;

            if count >= 8 {
                break;
            }
        }

        let mut m = 0;

        // Implement sprite overflow, including hardware bug
        // where m gets incremented when it doesn't make sense
        while n < 64 {
            let index = 4 * n + m;
            let y = self.oam[index];

            if self.is_sprite_at_y_on_scanline(y) {
                self.regs.ppu_status.set(PpuStatus::SPRITE_OVERFLOW, true);
                m += 3;
                if m > 3 {
                    m = 0;
                    n += 1;
                }
            } else {
                n += 1;
                m += 1;
            }
        }

        // Clear any remaining sprites if there were less than 8 on scanline
        while count < 8 {
            self.sprites[count] = None;
            count += 1;
        }
    }

    // Run for the given number of cpu cycles
    pub fn cycles(&mut self, cycles: u32) -> Interrupt {
        let mut interrupt = Interrupt::None;
        // 3 PPU cycles per CPU cycle
        for i in 0..cycles * 3 {
            let step_interrupt = self.step();
            if step_interrupt != Interrupt::None {
                interrupt = step_interrupt;
            }
        }

        interrupt
    }

    fn step(&mut self) -> Interrupt {
        let scanline_cycle = self.cycles - self.scanline_start_cycle;
        let mut interrupt = Interrupt::None;

        // TODO: Handle other important scanlines and cycles

        match self.scanline {
            -1 => {
                match scanline_cycle {
                    1 => {
                        self.regs.ppu_status.set(PpuStatus::VBLANK_STARTED, false);
                        self.regs.ppu_status.set(PpuStatus::SPRITE_OVERFLOW, false);
                    },
                    _ => (),
                }
            },
            240 => {
                // TODO: Render scanline
            },
            241 => {
                match scanline_cycle {
                    1 => {
                        self.regs.ppu_status.set(PpuStatus::VBLANK_STARTED, true);
                        interrupt = Interrupt::Nmi;
                    },
                    _ => ()
                }
            },
            260 => {
                self.regs.ppu_status.set(PpuStatus::VBLANK_STARTED, false);
            },
            _ => (),
        }

        if scanline_cycle == 351 {
            self.scanline_start_cycle = self.cycles;
            self.scanline += 1;
        }

        self.cycles += 1;

        interrupt
    }
}

// Implements mapping of PPU registers into CPU address space
impl Memory for Ppu {
    fn read_byte(&mut self, address: u16) -> u8 {
        if !(0x2000 <= address && address < 0x4000) {
            panic!("Invalid read from PPU memory-mapped registers: {:X}", address)
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
            panic!("Invalid write to PPU memory-mapped registers, address: {:X}, value: {}", address, value)
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
    Add1Across,         // Add 1, going across
    Add32Down,           // Add 32, going down
}

enum SpriteSize {
    Size8x8,
    Size8x16,
}

impl SpriteSize {
    fn height(&self) -> u8 {
        match *self {
            SpriteSize::Size8x8 => 8,
            SpriteSize::Size8x16 => 16,
        }
    }
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
            VramAddressIncrement::Add1Across
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

impl Deref for PpuAddr {
    type Target = u16;

    fn deref(&self) -> &Self::Target {
        &self.address
    }
}

impl DerefMut for PpuAddr {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.address
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


// OAM (Object Attribute Memory) is internal memory inside the PPU that contains
// a display list of up to 64 sprites, where each sprite's information occupies 4 bytes
struct Oam {
    bytes: [u8; Oam::SIZE],
}

impl Oam {
    // 64 sprites, each sprite uses 4 bytes
    const SIZE: usize = 64 * 4;

    fn new() -> Oam {
        Oam {
           bytes: [0u8; Oam::SIZE],
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
    type Target = [u8; Oam::SIZE];

    fn deref(&self) -> &Self::Target {
        &self.bytes
    }
}

impl DerefMut for Oam {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.bytes
    }
}

// 2KB internal dedicated Video RAM
pub struct Vram { bytes: [u8; Vram::SIZE] }

impl Vram {
    const SIZE: usize = 0x0800;

    fn new() -> Vram {
        Vram {
            bytes: [0u8; Vram::SIZE],
        }
    }
}

impl Memory for Vram {
    fn read_byte(&mut self, address: u16) -> u8 {
        self[address as usize]
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        self[address as usize] = value
    }
}

impl Deref for Vram {
    type Target = [u8; Vram::SIZE];

    fn deref(&self) -> &Self::Target {
        &self.bytes
    }
}

impl DerefMut for Vram {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.bytes
    }
}

pub struct PaletteRam { bytes: [u8; PaletteRam::SIZE] }

impl PaletteRam {
    const SIZE: usize = 32;
    const START_ADDRESS: u16 = 0x3FF0;
    const MIRROR_MASK: u16 = 0x3F1F;

    fn new() -> PaletteRam {
        PaletteRam {
            bytes: [0u8; PaletteRam::SIZE],
        }
    }
}

impl Memory for PaletteRam {
    fn read_byte(&mut self, address: u16) -> u8 {
        self[(address & PaletteRam::MIRROR_MASK) as usize]
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        self[(address & PaletteRam::MIRROR_MASK) as usize] = value
    }
}

impl Deref for PaletteRam {
    type Target = [u8; PaletteRam::SIZE];

    fn deref(&self) -> &Self::Target {
        &self.bytes
    }
}

impl DerefMut for PaletteRam {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.bytes
    }
}

pub struct MemMap {
    vram: Vram,
    palette_ram: PaletteRam,
    mapper: Rc<RefCell<Box<Mapper>>>,
}

impl MemMap {
    pub fn new(mapper: Rc<RefCell<Box<Mapper>>>) -> Self {
        MemMap {
            vram: Vram::new(),
            palette_ram: PaletteRam::new(),
            mapper,
        }
    }
}

impl Memory for MemMap {
    fn read_byte(&mut self, address: u16) -> u8 {
        if address < PaletteRam::START_ADDRESS {
            let mut mapper = self.mapper.borrow_mut();
            mapper.ppu_read_byte(&mut self.vram, address)
        } else if address < 0x4000 {
            // Palette RAM is not configurable, always mapped to the
            // internal palette control in VRAM.
            self.palette_ram.read_byte(address)
        } else {
            panic!("Invalid read from PPU space memory: {:X}", address)
        }
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        if address < PaletteRam::START_ADDRESS {
            let mut mapper = self.mapper.borrow_mut();
            mapper.ppu_write_byte(&mut self.vram, address, value);
        } else if address < 0x4000 {
            // Palette RAM is not configurable, always mapped to the
            // internal palette control in VRAM.
            self.palette_ram.write_byte(address, value);
        } else {
            panic!("Invalid write to PPU-space memory, address: {:X}, value: {}", address, value)
        }
    }
}

enum SpritePriority {
    FrontOfBackground,
    BehindBackground,
}

struct Sprite {
    x: u8,
    y: u8,
    size: SpriteSize,
    tile_number: u8,
    bank_address: u16,
    palette: u8,
    priority: SpritePriority,
    flip_horizontally: bool,
    flip_vertically: bool,
}

impl Sprite {
    fn from_oam_bytes(ppu_ctrl: &PpuCtrl, oam_bytes: &[u8]) -> Sprite {
        let size = ppu_ctrl.sprite_size();

        let tile_number = match size {
            SpriteSize::Size8x8 => oam_bytes[1],
            SpriteSize::Size8x16 => oam_bytes[1] & 0xFE,
        };

        let bank_address = match size {
            SpriteSize::Size8x8 => if (oam_bytes[1] | 0x01) == 0 {
                0x0000
            } else {
                0x1000
            },
            SpriteSize::Size8x16 => ppu_ctrl.sprite_pattern_table_address(),
        };

        Sprite {
            x: oam_bytes[3],
            y: oam_bytes[0],
            size,
            tile_number,
            bank_address,
            palette: (oam_bytes[2] & 0x03) + 4,
            priority: if (oam_bytes[2] & 0x20) == 0 {
                SpritePriority::FrontOfBackground
            } else {
                SpritePriority::BehindBackground
            },
            flip_horizontally: (oam_bytes[2] & 0x40) != 0,
            flip_vertically: (oam_bytes[2] & 0x40) != 0,
        }
    }
}

impl Default for Sprite {
    fn default() -> Sprite {
        Sprite {
            x: 0,
            y: 0,
            size: SpriteSize::Size8x8,
            tile_number: 0,
            bank_address: 0x0000,
            palette: 4,
            priority: SpritePriority::FrontOfBackground,
            flip_horizontally: false,
            flip_vertically: false,
        }
    }
}