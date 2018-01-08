use std::cell::RefCell;
use std::default::Default;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use cpu::Interrupt;
use mapper::Mapper;
use memory::Memory;
use sink::*;

pub const SCREEN_WIDTH: usize = 256;
pub const SCREEN_HEIGHT: usize = 240;

const CYCLES_PER_SCANLINE: u64 = 341;

const PRE_RENDER_SCANLINE: i16 = -1;
const VISIBLE_START_SCANLINE: i16 = 0;
const VISIBLE_END_SCANLINE: i16 = 239;
const POST_RENDER_SCANLINE: i16 = 240;
const VBLANK_START_SCANLINE: i16 = 241;
const VBLANK_END_SCANLINE: i16 = 260;

// Memory-mapped register addresses
const PPUCTRL_ADDRESS: u16 = 0x2000;
const PPUMASK_ADDRESS: u16 = 0x2001;
const PPUSTATUS_ADDRESS: u16 = 0x2002;
const OAMADDR_ADDRESS: u16 = 0x2003;
pub const OAMDATA_ADDRESS: u16 = 0x2004;
const PPUSCROLL_ADDRESS: u16 = 0x2005;
const PPUADDR_ADDRESS: u16 = 0x2006;
const PPUDATA_ADDRESS: u16 = 0x2007;

static PALETTE: &'static [u32] = &[
    0x666666, 0x002A88, 0x1412A7, 0x3B00A4, 0x5C007E, 0x6E0040, 0x6C0600, 0x561D00,
    0x333500, 0x0B4800, 0x005200, 0x004F08, 0x00404D, 0x000000, 0x000000, 0x000000,
    0xADADAD, 0x155FD9, 0x4240FF, 0x7527FE, 0xA01ACC, 0xB71E7B, 0xB53120, 0x994E00,
    0x6B6D00, 0x388700, 0x0C9300, 0x008F32, 0x007C8D, 0x000000, 0x000000, 0x000000,
    0xFFFEFF, 0x64B0FF, 0x9290FF, 0xC676FF, 0xF36AFF, 0xFE6ECC, 0xFE8170, 0xEA9E22,
    0xBCBE00, 0x88D800, 0x5CE430, 0x45E082, 0x48CDDE, 0x4F4F4F, 0x000000, 0x000000,
    0xFFFEFF, 0xC0DFFF, 0xD3D2FF, 0xE8C8FF, 0xFBC2FF, 0xFEC4EA, 0xFECCC5, 0xF7D8A5,
    0xE4E594, 0xCFEF96, 0xBDF4AB, 0xB3F3CC, 0xB5EBF2, 0xB8B8B8, 0x000000, 0x000000,
];

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

    frame: u64,

    frame_buffer: Box<[u8]>,
    frame_index: usize,

    // The PPU has an internal data bus that it uses for communication with the CPU.
    // This bus, called _io_db in Visual 2C02 and PPUGenLatch in FCEUX,[1] behaves as an
    // 8-bit dynamic latch due to capacitance of very long traces that run to various parts
    // of the PPU. Writing any value to any PPU port, even to the nominally read-only
    // PPUSTATUS, will fill this latch. Reading any readable port (PPUSTATUS, OAMDATA, or PPUDATA)
    // also fills the latch with the bits read. Reading a nominally "write-only" register returns
    // the latch's current value, as do the unused bits of PPUSTATUS.
    ppu_gen_latch: u8,

    background_pattern_shift_lo: u16,
    background_pattern_shift_hi: u16,
    background_palette: u8,
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
            frame: 0,
            frame_buffer: Box::new([0; SCREEN_WIDTH * SCREEN_HEIGHT]),
            frame_index: 0,
            ppu_gen_latch: 0,
            background_pattern_shift_lo: 0,
            background_pattern_shift_hi: 0,
            background_palette: 0,
        }
    }

    pub fn reset(&mut self) {
        self.cycles = 0;
        *self.regs.ppu_ctrl = 0;
        *self.regs.ppu_mask = 0;
        *self.regs.ppu_status != 0x80;
        self.ppu_data_read_buffer = 0;
    }

    fn read_ppu_status(&mut self) -> u8 {
        // http://wiki.nesdev.com/w/index.php/PPU_scrolling#.242002_read
        self.regs.w = WriteToggle::FirstWrite;

        *self.regs.ppu_status | (self.ppu_gen_latch & 0x1F)
    }

    fn read_oam_byte(&self) -> u8 {
        self.oam[self.regs.oam_addr as usize]
    }

    fn write_oam_byte(&mut self, val: u8) {
        // TODO: Ignore writes during rendering
        // http://wiki.nesdev.com/w/index.php/PPU_registers#OAM_data_.28.242004.29_.3C.3E_read.2Fwrite

        self.oam[self.regs.oam_addr as usize] = val;
        self.regs.oam_addr = self.regs.oam_addr.wrapping_add(1);
    }

    fn write_ppu_ctrl(&mut self, value: u8) {
        // http://wiki.nesdev.com/w/index.php/PPU_scrolling#.242000_write
        self.regs.t = (self.regs.t & 0x73FF) | ((value as u16 & 0x03) << 10);
        *self.regs.ppu_ctrl = value;
    }

    fn write_ppu_scroll(&mut self, value: u8) {
        match self.regs.w {
            WriteToggle::FirstWrite => {
                // http://wiki.nesdev.com/w/index.php/PPU_scrolling#.242005_first_write_.28w_is_0.29
                self.regs.t = (self.regs.t & 0xFFE0) |
                    (((value as u16) >> 3) & 0x01F);
                self.regs.x = value & 0x07;
                self.regs.w = WriteToggle::SecondWrite;

                self.regs.ppu_scroll.position_x = value;
            },
            WriteToggle::SecondWrite => {
                // http://wiki.nesdev.com/w/index.php/PPU_scrolling#.242005_second_write_.28w_is_1.29
                self.regs.t = (self.regs.t & 0x0C1F) |
                    ((value as u16 & 0x07) << 12) |
                    ((value as u16 & 0xF8) << 2);
                self.regs.w = WriteToggle::FirstWrite;

                self.regs.ppu_scroll.position_y = value;
            },
        }
    }

    fn inc_ppu_addr(&mut self) {
        self.regs.ppu_addr += match self.regs.ppu_ctrl.vram_address_increment() {
            VramAddressIncrement::Add1Across => 1,
            VramAddressIncrement::Add32Down => 32,
        };

        // http://wiki.nesdev.com/w/index.php/PPU_scrolling#.242007_reads_and_writes
        if self.rendering_enabled() && self.scanline < 240 {
            self.inc_coarse_x_with_wrap();
            self.inc_y_with_wrap();
        } else {
            self.regs.v += match self.regs.ppu_ctrl.vram_address_increment() {
                VramAddressIncrement::Add1Across => 1,
                VramAddressIncrement::Add32Down => 32,
            };
        }
    }

    fn write_ppu_addr(&mut self, value: u8) {
        match self.regs.w {
            WriteToggle::FirstWrite => {
                // http://wiki.nesdev.com/w/index.php/PPU_scrolling#.242006_first_write_.28w_is_0.29
                self.regs.t = (self.regs.t & 0x00FF) | ((value as u16 & 0x3F) << 8);
                self.regs.w = WriteToggle::SecondWrite;

                self.regs.ppu_addr = (self.regs.ppu_addr & 0x00FF) | ((value as u16) << 8);
            },
            WriteToggle::SecondWrite => {
                // http://wiki.nesdev.com/w/index.php/PPU_scrolling#.242006_second_write_.28w_is_1.29
                self.regs.t = (self.regs.t & 0xFF00) | (value as u16);
                self.regs.v = self.regs.t;
                self.regs.w = WriteToggle::FirstWrite;

                self.regs.ppu_addr = (self.regs.ppu_addr & 0xFF00) | (value as u16);
            },
        }
    }

    fn read_ppu_data_byte(&mut self) -> u8 {
        let address = self.regs.ppu_addr;

        let read_buffer = self.ppu_data_read_buffer;
        self.ppu_data_read_buffer = self.mem.read_byte(address);
        let data = if address < PaletteRam::START_ADDRESS {
            // Return contents of read buffer before the read.
            read_buffer
        } else {
            // Palette data is returned immediately. No dummy read is required.
            self.ppu_data_read_buffer
        };

        self.inc_ppu_addr();

        data
    }

    fn write_ppu_data_byte(&mut self, val: u8) {
        let address = self.regs.ppu_addr;
        self.mem.write_byte(address, val);
        self.inc_ppu_addr();
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

    fn rendering_enabled(&self) -> bool {
        self.regs.ppu_mask.contains(PpuMask::SHOW_BACKGROUND) ||
            self.regs.ppu_mask.contains(PpuMask::SHOW_SPRITES)
    }

    fn current_tile_address(&self) -> u16 {
        0x2000 | (self.regs.v & 0x0FFF)
    }

    fn current_attribute_address(&self) -> u16 {
        let v = self.regs.v;
        0x23C0 | (v & 0x0C00) | ((v >> 4) & 0x38) | ((v >> 2) & 0x07)
    }

    fn fetch_tile(&mut self) {
        let name_addr = self.current_tile_address();
        let name_entry = self.mem.read_byte(name_addr);

        let fine_y = ((self.regs.v >> 12) & 0x07) as u8;

        let pattern_index = self.regs.ppu_ctrl.background_pattern_table_address() +
            (name_entry * 16 + fine_y) as u16;
        let pattern_lo = self.mem.read_byte(pattern_index);
        let pattern_hi = self.mem.read_byte(pattern_index + 8);

        self.background_pattern_shift_lo =
            ((self.background_pattern_shift_lo >> 8) & 0x00FF) | ((pattern_lo as u16) << 8);
        self.background_pattern_shift_hi =
            ((self.background_pattern_shift_hi >> 8) & 0x00FF) | ((pattern_hi as u16) << 8);

        let attr_addr = self.current_attribute_address();
        let attr_entry = self.mem.read_byte(attr_addr);

        let mut attr_shift = 0;
        if self.regs.v & 0x0001 == 1 {
            attr_shift += 2;
        }
        if self.regs.v & 0x0020 == 0x0020 {
            attr_shift += 4;
        }
        self.background_palette = (attr_entry >> attr_shift) & 0x03;
    }

    fn color_from_palette_index(&mut self, index: u8) -> u8 {
        self.mem.read_byte(0x3F00 | (index & 0x1F) as u16)
    }

    fn render_pixel(&mut self) {
        let fine_x = self.regs.x;

        let palette_index =
                ((self.background_pattern_shift_lo as u8 >> (7 - fine_x)) & 0x01) |
                ((self.background_pattern_shift_hi as u8 >> (6 - fine_x)) & 0x02) |
                ((self.background_palette << 2) & 0x0C);

        let background_color = self.color_from_palette_index(palette_index);

        for sprite in self.sprites.iter() {
            if let &Some(ref sprite) = sprite {
                let x = sprite.x;
                let y = sprite.y;
            }
        }

        self.frame_buffer[self.frame_index] = self.color_from_palette_index(palette_index);

        self.frame_index = (self.frame_index + 1) % ((SCREEN_WIDTH * SCREEN_HEIGHT) - 1);
    }


    fn inc_coarse_x_with_wrap(&mut self) {
        if (self.regs.v & 0x001F) == 31 {
            self.regs.v &= !0x001F;         // course X = 0
            self.regs.v ^= 0x0400;          // switch horizontal nametable
        } else {
            self.regs.v += 1;
        }
    }

    fn inc_y_with_wrap(&mut self) {
        if (self.regs.v & 0x7000) != 0x7000 {
            self.regs.v += 0x1000;
        } else {
            self.regs.v &= !0x7000;
            let mut y = (self.regs.v & 0x03E0) >> 5;
            if y == 29 {
                y = 0;
                self.regs.v ^= 0x0800;
            } else if y == 31 {
                y = 0;
            } else {
                y += 1;
            }

            self.regs.v = (self.regs.v & !0x03E0) | (y << 5);
        }
    }

    // Run for the given number of cpu cycles
    pub fn cycles(&mut self,
                  cycles: u32,
                  video_frame_sink: &mut Sink<VideoFrame>) -> Option<Interrupt> {
        let mut interrupt = None;
        // 3 PPU cycles per CPU cycle
        for _ in 0..cycles * 3 {
            if let Some(step_interrupt) = self.step(video_frame_sink) {
                interrupt = Some(step_interrupt);
            }
        }

        interrupt
    }

    fn step(&mut self, video_frame_sink: &mut Sink<VideoFrame>) -> Option<Interrupt> {
        let scanline_cycle = self.cycles - self.scanline_start_cycle;
        let mut interrupt = None;

        if self.rendering_enabled() {
            if scanline_cycle == 256 {
                self.inc_y_with_wrap();
            } else if scanline_cycle == 257 {
                // Copy bits related to horizontal position from t to v
                self.regs.v = (self.regs.v & !0x041F) | (self.regs.t & 0x041F);
            } else if (scanline_cycle >= 328 || scanline_cycle <= 256) && scanline_cycle % 8 == 0 {
                // Increment the effective x scroll coordinate every 8 cycles
                self.inc_coarse_x_with_wrap();
            }
        }

        match self.scanline {
            PRE_RENDER_SCANLINE => {
                if scanline_cycle == 0 {
                    self.regs.ppu_status.set(PpuStatus::SPRITE_OVERFLOW, false);
                } else if self.rendering_enabled() &&
                    280 <= scanline_cycle && scanline_cycle <= 304 {
                    // Copy bits related to vertical position from t to v
                    self.regs.v = (self.regs.v & !0x7BE0) | (self.regs.t & 0x7BE0);
                }
            },
            VISIBLE_START_SCANLINE ... VISIBLE_END_SCANLINE => {
                if self.rendering_enabled() {
                    if scanline_cycle == 1 {
                        self.evaluate_sprites_for_scanline();
                    }

                    // Background tiles
                    match scanline_cycle {
                        1 ... 256 => {
                            self.render_pixel();
                            if (scanline_cycle - 1) % 8 == 0 {
                                self.fetch_tile();
                            }
                        },
                        257 ... 320 => {
                            // Fetch sprite tile data
                        },
                        // Fetch two tiles for next scanline
                        321 => { self.fetch_tile() },
                        329 => { self.fetch_tile() },
                        337 ... 340 => {
                            // Fetch two nametable bytes
                        },
                        _ => (),
                    }

                    // Sprites
                    match scanline_cycle {
                        1 ... 64 => {},
                        _ => (),
                    }
                }
            }
            POST_RENDER_SCANLINE => {
                if scanline_cycle == 0 {
//                    let mut buffer: Box<[u32]> = vec![PALETTE[(self.frame % 64) as usize]; SCREEN_WIDTH * SCREEN_HEIGHT].into_boxed_slice();
                    let buffer: Vec<u32> = self.frame_buffer.iter()
                        .map(|b| PALETTE[(b & 0x3F) as usize]).collect();
                    video_frame_sink.append(buffer.into_boxed_slice());
                }
            },
            VBLANK_START_SCANLINE => {
                if scanline_cycle == 1 {
                    self.regs.ppu_status.set(PpuStatus::VBLANK_STARTED, true);
                    if self.regs.ppu_ctrl.generate_nmi_vblank() {
                        interrupt = Some(Interrupt::Nmi);
                    }
                }
            },
            VBLANK_END_SCANLINE => {
                if scanline_cycle == 0 {
                    self.regs.ppu_status.set(PpuStatus::VBLANK_STARTED, false);
                }
            },
            _ => (),
        }

        self.cycles += 1;

        if scanline_cycle >= CYCLES_PER_SCANLINE ||
            // On pre-render scanline, for odd frames,
            // the cycle at the end of the scanline is skipped
            (self.scanline == PRE_RENDER_SCANLINE &&
                scanline_cycle == CYCLES_PER_SCANLINE - 1 &&
                self.frame % 2 != 0) {
            self.scanline_start_cycle = self.cycles;
            self.scanline += 1;
        }

        if self.scanline > VBLANK_END_SCANLINE {
            self.scanline = PRE_RENDER_SCANLINE;
            self.frame += 1;
        }

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
            PPUSTATUS_ADDRESS => self.read_ppu_status(),
            OAMDATA_ADDRESS => self.read_oam_byte(),
            PPUDATA_ADDRESS => self.read_ppu_data_byte(),
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

        // Writes to the following registers are ignored if earlier than
        // ~29658 CPU clocks after reset: PPUCTRL, PPUMASK, PPUSCROLL, PPUADDR
        if self.cycles < 3 * 29658 &&
            (address == PPUCTRL_ADDRESS ||
                address == PPUMASK_ADDRESS ||
                address == PPUSCROLL_ADDRESS ||
                address == PPUADDR_ADDRESS) {
            return;
        }

        self.ppu_gen_latch = value;

        match address {
            PPUCTRL_ADDRESS => self.write_ppu_ctrl(value),
            PPUMASK_ADDRESS => *self.regs.ppu_mask = value,
            OAMADDR_ADDRESS => self.regs.oam_addr = value,
            OAMDATA_ADDRESS => self.write_oam_byte(value),
            PPUSCROLL_ADDRESS => self.write_ppu_scroll(value),
            PPUADDR_ADDRESS => self.write_ppu_addr(value),
            PPUDATA_ADDRESS => self.write_ppu_data_byte(value),
            _ => ()
        }
    }
}

// VRAM address increment per CPU read/write of PPUDATA
enum VramAddressIncrement {
    Add1Across,         // Add 1, going across
    Add32Down,          // Add 32, going down
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

    fn base_name_table_x_inc(&self) -> u16 {
        if self.val & 0x01 == 0x01 { 256 } else { 0 }
    }

    fn base_name_table_y_inc(&self) -> u16 {
        if self.val & 0x02 == 0x02 { 240 } else { 0 }
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
}

impl Default for PpuScroll {
    fn default() -> PpuScroll {
        PpuScroll {
            position_x: 0,
            position_y: 0,
        }
    }
}

enum WriteToggle {
    FirstWrite,
    SecondWrite,
}

struct Regs {
    // Registers mapped from CPU address space
    ppu_ctrl: PpuCtrl,          // 0x2000
    ppu_mask: PpuMask,          // 0x2001
    ppu_status: PpuStatus,      // 0x2002
    oam_addr: u8,               // 0x2003
    ppu_scroll: PpuScroll,      // 0x2005
    ppu_addr: u16,              // 0x2006

    // Internal registers
    v: u16,                     // Current VRAM address (15 bits)
    t: u16,                     // Temporary VRAM address (15 bits)
    x: u8,                      // Fine X scroll (3 bits)
    w: WriteToggle,             // First of second write toggle


}

impl Regs {
    fn new() -> Regs {
        Regs {
            ppu_ctrl: PpuCtrl { val: 0 },
            ppu_mask: PpuMask::NONE,
            ppu_status: PpuStatus::NONE,
            oam_addr: 0,
            ppu_scroll: PpuScroll::default(),
            ppu_addr: 0,
            v: 0,
            t: 0,
            x: 0,
            w: WriteToggle::FirstWrite,
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
    InFrontOfBackground,
    BehindBackground,
}

struct Sprite {
    y: u8,
    tile_index: u8,
    attributes: u8,
    x: u8,
}

impl Sprite {
    fn from_oam_bytes(ppu_ctrl: &PpuCtrl, oam_bytes: &[u8]) -> Sprite {
        Sprite {
            y: oam_bytes[0],
            tile_index: oam_bytes[1],
            attributes: oam_bytes[2],
            x: oam_bytes[3],
        }
    }

    fn palette(&self) -> u8 {
        (self.attributes & 0x03) + 4
    }

    fn priority(&self) -> SpritePriority {
        if self.attributes & 0x20 == 0 {
            SpritePriority::InFrontOfBackground
        } else {
            SpritePriority::BehindBackground
        }
    }

    fn flip_horizontally(&self) -> bool {
        self.attributes & 0x40 == 0x40
    }

    fn flip_vertically(&self) -> bool {
        self.attributes & 0x80 == 0x80
    }
}

impl Default for Sprite {
    fn default() -> Sprite {
        Sprite {
            y: 0,
            tile_index: 0,
            attributes: 0,
            x: 0,
        }
    }
}