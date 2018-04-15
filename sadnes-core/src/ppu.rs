use std::cell::RefCell;
use std::default::Default;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

use bit_reverse::ParallelReverse;
use serde_bytes;

use cpu::{Cpu, Interrupt};
use mapper::Mapper;
use memory::Memory;
use sink::*;

pub const SCREEN_WIDTH: usize = 256;
pub const SCREEN_HEIGHT: usize = 240;

const CYCLES_PER_SCANLINE: u64 = 341;

const PRE_RENDER_SCANLINE: i16 = -1;
const VISIBLE_START_SCANLINE: i16 = 0;
pub const VISIBLE_END_SCANLINE: i16 = 239;
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

static PALETTE: &[u32] = &[
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
    pub mem: MemMap,

    // Object Attribute Memory
    oam: Oam,

    pub scanline: i16,

    // The cycle that the current scanline started at
    scanline_start_cycle: u64,

    frame: u64,

    frame_buffer: Box<[u8]>,

    // The PPU has an internal data bus that it uses for communication with the CPU.
    // This bus, called _io_db in Visual 2C02 and PPUGenLatch in FCEUX,[1] behaves as an
    // 8-bit dynamic latch due to capacitance of very long traces that run to various parts
    // of the PPU. Writing any value to any PPU port, even to the nominally read-only
    // PPUSTATUS, will fill this latch. Reading any readable port (PPUSTATUS, OAMDATA, or PPUDATA)
    // also fills the latch with the bits read. Reading a nominally "write-only" register returns
    // the latch's current value, as do the unused bits of PPUSTATUS.
    ppu_gen_latch: u8,

    // Registers used during rendering to hold background data
    name_table_byte: u8,
    attribute_table_byte: u8,
    tile_bitmap_byte_lo: u8,
    tile_bitmap_byte_hi: u8,
    background_pattern_shift_lo: u16,
    background_pattern_shift_hi: u16,
    background_palette_shift_lo: u16,
    background_palette_shift_hi: u16,

    // Registers used during rendering to hold sprite data
    sprite_pattern_shifts_lo: [u8; 8],
    sprite_pattern_shifts_hi: [u8; 8],
    sprite_attribute_latches: [SpriteAttributes; 8],
    sprite_x_counters: [u8; 8],
    sprite_0_on_scanline: bool,

    nmi_occurred: bool,
    nmi_output: bool,
}


#[derive(Deserialize, Serialize)]
pub struct State {
    pub cycles: u64,
    pub regs: Regs,
    pub ppu_data_read_buffer: u8,
    pub vram: Vram,
    pub palette_ram: PaletteRam,
    pub oam: Oam,
    pub scanline: i16,
    pub scanline_start_cycle: u64,
    pub frame: u64,
    pub ppu_gen_latch: u8,
    pub name_table_byte: u8,
    pub attribute_table_byte: u8,
    pub tile_bitmap_byte_lo: u8,
    pub tile_bitmap_byte_hi: u8,
    pub background_pattern_shift_lo: u16,
    pub background_pattern_shift_hi: u16,
    pub background_palette_shift_lo: u16,
    pub background_palette_shift_hi: u16,
    pub sprite_pattern_shifts_lo: [u8; 8],
    pub sprite_pattern_shifts_hi: [u8; 8],
    pub sprite_attribute_latches: [SpriteAttributes; 8],
    pub sprite_x_counters: [u8; 8],
    pub sprite_0_on_scanline: bool,
    pub nmi_occurred: bool,
    pub nmi_output: bool,
}

impl Ppu {
    pub fn new(mapper: Rc<RefCell<Box<Mapper>>>) -> Ppu {
        Ppu {
            cycles: 0,
            regs: Regs::new(),
            ppu_data_read_buffer: 0,
            mem: MemMap::new(mapper),
            oam: Oam::new(),
            scanline: 0,
            scanline_start_cycle: 0,
            frame: 0,
            frame_buffer: Box::new([0; SCREEN_WIDTH * SCREEN_HEIGHT]),
            ppu_gen_latch: 0,
            name_table_byte: 0,
            attribute_table_byte: 0,
            tile_bitmap_byte_lo: 0,
            tile_bitmap_byte_hi: 0,
            background_pattern_shift_lo: 0,
            background_pattern_shift_hi: 0,
            background_palette_shift_lo: 0,
            background_palette_shift_hi: 0,
            sprite_pattern_shifts_lo: [0; 8],
            sprite_pattern_shifts_hi: [0; 8],
            sprite_attribute_latches: [SpriteAttributes(0); 8],
            sprite_x_counters: [0; 8],
            sprite_0_on_scanline: false,
            nmi_occurred: false,
            nmi_output: false,
        }
    }

    pub fn get_state(&self) -> State {
        State {
            cycles: self.cycles,
            regs: self.regs,
            ppu_data_read_buffer: self.ppu_data_read_buffer,
            vram: self.mem.vram.clone(),
            palette_ram: self.mem.palette_ram.clone(),
            oam: self.oam.clone(),
            scanline: self.scanline,
            scanline_start_cycle: self.scanline_start_cycle,
            frame: self.frame,
            ppu_gen_latch: self.ppu_gen_latch,
            name_table_byte: self.name_table_byte,
            attribute_table_byte: self.attribute_table_byte,
            tile_bitmap_byte_lo: self.tile_bitmap_byte_lo,
            tile_bitmap_byte_hi: self.tile_bitmap_byte_hi,
            background_pattern_shift_lo: self.background_pattern_shift_lo,
            background_pattern_shift_hi: self.background_pattern_shift_hi,
            background_palette_shift_lo: self.background_palette_shift_lo,
            background_palette_shift_hi: self.background_palette_shift_hi,
            sprite_pattern_shifts_lo: self.sprite_pattern_shifts_lo,
            sprite_pattern_shifts_hi: self.sprite_pattern_shifts_hi,
            sprite_attribute_latches: self.sprite_attribute_latches,
            sprite_x_counters: self.sprite_x_counters,
            sprite_0_on_scanline: self.sprite_0_on_scanline,
            nmi_occurred: self.nmi_occurred,
            nmi_output: self.nmi_output,
        }
    }

    pub fn apply_state(&mut self, state: &State) {
        self.cycles = state.cycles;
        self.regs = state.regs;
        self.ppu_data_read_buffer = state.ppu_data_read_buffer;
        self.mem.vram = state.vram.clone();
        self.mem.palette_ram = state.palette_ram.clone();
        self.oam = state.oam.clone();
        self.scanline = state.scanline;
        self.scanline_start_cycle = state.scanline_start_cycle;
        self.frame = state.frame;
        self.ppu_gen_latch = state.ppu_gen_latch;
        self.name_table_byte = state.name_table_byte;
        self.attribute_table_byte = state.attribute_table_byte;
        self.tile_bitmap_byte_lo = state.tile_bitmap_byte_lo;
        self.tile_bitmap_byte_hi = state.tile_bitmap_byte_hi;
        self.background_pattern_shift_lo = state.background_pattern_shift_lo;
        self.background_pattern_shift_hi = state.background_pattern_shift_hi;
        self.background_palette_shift_lo = state.background_palette_shift_lo;
        self.background_palette_shift_hi = state.background_palette_shift_hi;
        self.sprite_pattern_shifts_lo = state.sprite_pattern_shifts_lo;
        self.sprite_pattern_shifts_hi = state.sprite_pattern_shifts_hi;
        self.sprite_attribute_latches = state.sprite_attribute_latches;
        self.sprite_x_counters = state.sprite_x_counters;
        self.sprite_0_on_scanline = state.sprite_0_on_scanline;
        self.nmi_occurred = state.nmi_occurred;
        self.nmi_output = state.nmi_output;
    }

    pub fn reset(&mut self) {
        self.cycles = 0;
        self.frame = 0;
        *self.regs.ppu_ctrl = 0;
        *self.regs.ppu_mask = 0;
        *self.regs.ppu_status != 0x80;
        self.ppu_data_read_buffer = 0;
    }

    fn read_ppu_status(&mut self) -> u8 {
        // http://wiki.nesdev.com/w/index.php/PPU_scrolling#.242002_read
        self.regs.w = WriteToggle::FirstWrite;

        let vblank = if self.nmi_occurred { 0x80 } else { 0x00 };
        let status = vblank | (*self.regs.ppu_status & 0x60) | (self.ppu_gen_latch & 0x1F);

        self.nmi_occurred = false;

        status
    }

    fn read_oam_byte(&self) -> u8 {
        // http://wiki.nesdev.com/w/index.php/PPU_sprite_evaluation
        if VISIBLE_START_SCANLINE <= self.scanline && self.scanline <= VISIBLE_END_SCANLINE {
            let scanline_cycle = self.scanline_cycle();
            if 1 <= scanline_cycle && scanline_cycle <= 64 {
                return 0xFF;
            }
        }

        self.oam[self.regs.oam_addr as usize]
    }

    fn write_oam_byte(&mut self, val: u8) {
        // Ignore writes during rendering
        // http://wiki.nesdev.com/w/index.php/PPU_registers#OAM_data_.28.242004.29_.3C.3E_read.2Fwrite
        if self.rendering_enabled() &&
            VISIBLE_START_SCANLINE <= self.scanline && self.scanline <= VISIBLE_END_SCANLINE {
            return;
        }

        self.oam[self.regs.oam_addr as usize] = val;
        self.regs.oam_addr = self.regs.oam_addr.wrapping_add(1);
    }

    fn write_ppu_ctrl(&mut self, value: u8) {
        // http://wiki.nesdev.com/w/index.php/PPU_scrolling#.242000_write
        self.regs.t = (self.regs.t & 0xF3FF) | ((value as u16 & 0x03) << 10);
        *self.regs.ppu_ctrl = value;

        self.nmi_output = value & 0x80 != 0;
    }

    fn write_ppu_scroll(&mut self, value: u8) {
        match self.regs.w {
            WriteToggle::FirstWrite => {
                // http://wiki.nesdev.com/w/index.php/PPU_scrolling#.242005_first_write_.28w_is_0.29
                self.regs.t = (self.regs.t & 0xFFE0) |
                    (((value as u16) >> 3) & 0x01F);
                self.regs.x = value & 0x07;
                self.regs.w = WriteToggle::SecondWrite;
            },
            WriteToggle::SecondWrite => {
                // http://wiki.nesdev.com/w/index.php/PPU_scrolling#.242005_second_write_.28w_is_1.29
                self.regs.t = (self.regs.t & 0x0C1F) |
                    (((value & 0x07) as u16) << 12) |
                    (((value & 0xF8) as u16) << 2);
                self.regs.w = WriteToggle::FirstWrite;
            },
        }
    }

    fn inc_ppu_addr(&mut self) {
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
            },
            WriteToggle::SecondWrite => {
                // http://wiki.nesdev.com/w/index.php/PPU_scrolling#.242006_second_write_.28w_is_1.29
                self.regs.t = (self.regs.t & 0xFF00) | (value as u16);
                self.regs.v = self.regs.t;
                self.regs.w = WriteToggle::FirstWrite;
            },
        }
    }

    fn read_ppu_data_byte(&mut self) -> u8 {
        let address = self.regs.v;

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
        let address = self.regs.v;
        self.mem.write_byte(address, val);
        self.inc_ppu_addr();
    }

    fn is_sprite_at_y_on_scanline(&self, y: u8) -> bool {
        if self.scanline < 0 {
            return false;
        }

        let scanline = self.scanline as u16;
        let y = y as u16;
        let height = self.regs.ppu_ctrl.sprite_size().height() as u16;

        y <= scanline && scanline < y + height
    }

    fn sprite_evaluation_init(&mut self) {
        self.oam.secondary_write_index = 0;
        self.oam.n = 0;
        self.oam.m = 0;
        self.sprite_0_on_scanline = self.oam.sprite_0_found;
        self.oam.sprite_0_found = false;
    }

    fn sprite_evaluation_read_byte(&mut self) {
        let index = (self.oam.n * Oam::BYTES_PER_SPRITE) + self.oam.m;
        if index < self.oam.primary.len() {
            self.oam.last_read_byte = self.oam.primary[index];
        }
    }

    fn sprite_evaluation_write_byte(&mut self) {
        if self.oam.n < Oam::PRIMARY_MAX_SPRITES {
            if self.oam.secondary_write_index < Oam::SECONDARY_SIZE {
                self.oam.secondary[self.oam.secondary_write_index] = self.oam.last_read_byte;
                if self.oam.m == 0 && !self.is_sprite_at_y_on_scanline(self.oam.last_read_byte) {
                    self.oam.n += 1;
                } else {
                    if self.oam.n == 0 && self.oam.m == 0 {
                        self.oam.sprite_0_found = true;
                    }
                    self.oam.secondary_write_index += 1;
                    self.oam.m += 1;
                }
            } else {
                let y = self.oam.last_read_byte;
                if self.is_sprite_at_y_on_scanline(y) {
                    self.regs.ppu_status.set(PpuStatus::SPRITE_OVERFLOW, true);
                    self.oam.m += 1;
                } else {
                    self.oam.n += 1;
                    self.oam.m += 1;
                }
            }

            if self.oam.m == 4 {
                self.oam.m = 0;
                self.oam.n += 1;
            }
        }
    }

    pub fn rendering_enabled(&self) -> bool {
        self.regs.ppu_mask.contains(PpuMask::SHOW_BACKGROUND) ||
            self.regs.ppu_mask.contains(PpuMask::SHOW_SPRITES)
    }

    fn current_name_address(&self) -> u16 {
        0x2000 | (self.regs.v & 0x0FFF)
    }

    fn current_attribute_address(&self) -> u16 {
        let v = self.regs.v;
        0x23C0 | (v & 0x0C00) | ((v >> 4) & 0x38) | ((v >> 2) & 0x07)
    }

    fn current_pattern_address(&self) -> u16 {
        let fine_y = ((self.regs.v >> 12) & 0x07) as u16;
        self.regs.ppu_ctrl.background_pattern_table_address() +
            (self.name_table_byte as u16 * 16 + fine_y)
    }

    fn fetch_name_table_byte(&mut self) {
        let name_addr = self.current_name_address();
        self.name_table_byte = self.mem.read_byte(name_addr);
    }

    fn fetch_attribute_table_byte(&mut self) {
        let attr_addr = self.current_attribute_address();
        self.attribute_table_byte = self.mem.read_byte(attr_addr);
    }

    fn fetch_bitmap_byte_lo(&mut self) {
        let addr = self.current_pattern_address();
        self.tile_bitmap_byte_lo = self.mem.read_byte(addr);
    }

    fn fetch_bitmap_byte_hi(&mut self) {
        let addr = self.current_pattern_address() + 8;
        self.tile_bitmap_byte_hi = self.mem.read_byte(addr);
    }

    fn load_background_registers(&mut self) {
        self.background_pattern_shift_lo =
            (self.background_pattern_shift_lo & 0xFF00) | (self.tile_bitmap_byte_lo as u16);
        self.background_pattern_shift_hi =
            (self.background_pattern_shift_hi & 0xFF00) | (self.tile_bitmap_byte_hi as u16);

        let palette_shift = ((self.regs.v >> 4) & 0x04) | (self.regs.v & 0x02);
        let palette = ((self.attribute_table_byte >> palette_shift) & 0x03) as u16;

        self.background_palette_shift_lo =
            (self.background_palette_shift_lo & 0xFF00) |
                (if (palette & 0x01) == 0x01 { 0xFF } else { 0x00 });
        self.background_palette_shift_hi =
            (self.background_palette_shift_hi & 0xFF00) |
                (if (palette & 0x02) == 0x02 { 0xFF } else { 0x00 });
    }

    fn shift_background_registers(&mut self) {
        self.background_pattern_shift_lo <<= 1;
        self.background_pattern_shift_hi <<= 1;
        self.background_palette_shift_lo <<= 1;
        self.background_palette_shift_hi <<= 1;
    }

    fn fetch_sprite_tile(&mut self, sprite_index: usize) {
        let index = sprite_index * Oam::BYTES_PER_SPRITE;
        let sprite = Sprite::from_oam_bytes(&self.oam.secondary[index..(index + Oam::BYTES_PER_SPRITE)]);

        let mut row = self.scanline as u16 - sprite.y as u16;
        let size = self.regs.ppu_ctrl.sprite_size();
        let pattern_addr = match size {
            SpriteSize::Size8x8 => {
                if sprite.flip_vertically() {
                    row = 7 - row;
                }

                (sprite.tile_index as u16) * 16 +
                    self.regs.ppu_ctrl.sprite_pattern_table_address() + row
            },
            SpriteSize::Size8x16 => {
                if sprite.flip_vertically() {
                    row = 15 - row;
                }

                let table: u16 = if sprite.tile_index & 0x01 == 0 { 0x0000 } else { 0x1000 };
                let mut tile_index = sprite.tile_index & 0xFE;
                if row > 7 {
                    // Jump to second tile
                    tile_index += 1;
                    row -= 8;
                }

                table + (tile_index as u16) * 16 + row
            }
        };

        let mut pattern_lo = self.mem.read_byte(pattern_addr);
        let mut pattern_hi = self.mem.read_byte(pattern_addr + 8);

        if sprite.flip_horizontally() {
            pattern_lo = pattern_lo.swap_bits();
            pattern_hi = pattern_hi.swap_bits();
        }

        self.sprite_pattern_shifts_lo[sprite_index] = pattern_lo;
        self.sprite_pattern_shifts_hi[sprite_index] = pattern_hi;
        self.sprite_attribute_latches[sprite_index] = sprite.attributes;
        self.sprite_x_counters[sprite_index] = sprite.x;
    }

    fn update_sprite_rendering_registers(&mut self) {
        for i in 0..8 {
            let counter = &mut self.sprite_x_counters[i];

            if *counter == 0 {
                self.sprite_pattern_shifts_lo[i] <<= 1;
                self.sprite_pattern_shifts_hi[i] <<= 1;
            } else {
                *counter -= 1;
            }
        }
    }

    fn color_from_palette_index(&mut self, index: u8) -> u8 {
        self.mem.read_byte(PaletteRam::START_ADDRESS | (index & 0x1F) as u16)
    }

    fn render_pixel(&mut self) {
        let x = self.scanline_cycle() - 1;
        let y = self.scanline as u16;

        let background_pixel = self.background_pixel();
        let (sprite_pixel, sprite_index) = self.sprite_pixel_and_index();

        let background_pattern = background_pixel & 0x03;
        let sprite_pattern = sprite_pixel & 0x03;

        let color = if background_pattern == 0 && sprite_pattern == 0 {
            self.color_from_palette_index(0x00)
        } else if background_pattern == 0 && sprite_pattern > 0 {
            self.color_from_palette_index(sprite_pixel)
        } else if background_pattern > 0 && sprite_pattern == 0 {
            self.color_from_palette_index(background_pixel)
        } else {
            // Sprite 0 hit is not detected at x=255
            if self.sprite_0_on_scanline && sprite_index == 0 && x != 255 {
                self.regs.ppu_status.set(PpuStatus::SPRITE_ZERO_HIT, true);
            }

            let attrs = self.sprite_attribute_latches[sprite_index];

            match attrs.priority() {
                SpritePriority::InFrontOfBackground =>
                    self.color_from_palette_index(sprite_pixel),
                SpritePriority::BehindBackground =>
                    self.color_from_palette_index(background_pixel),
            }
        };

        self.frame_buffer[(y as usize * SCREEN_WIDTH) + x as usize] = color;
    }

    fn background_pixel(&self) -> u8 {
        let x = self.scanline_cycle() - 1;
        if !self.regs.ppu_mask.contains(PpuMask::SHOW_BACKGROUND) ||
            x < 8 && !self.regs.ppu_mask.contains(PpuMask::SHOW_BACKGROUND_LEFT_8) {
            return 0;
        }

        let fine_x = self.regs.x;

        ((self.background_pattern_shift_lo >> (15 - fine_x)) & 0x01) as u8 |
            ((self.background_pattern_shift_hi >> (14 - fine_x)) & 0x02) as u8 |
            ((self.background_palette_shift_lo >> (13 - fine_x)) & 0x04) as u8 |
            ((self.background_palette_shift_hi >> (12 - fine_x)) & 0x08) as u8
    }

    fn sprite_pixel_and_index(&mut self) -> (u8, usize) {
        let x = self.scanline_cycle() - 1;
        if !self.regs.ppu_mask.contains(PpuMask::SHOW_SPRITES) ||
            x < 8 && !self.regs.ppu_mask.contains(PpuMask::SHOW_SPRITES_LEFT_8) {
            return (0, 0);
        }

        for i in 0..8 {
            if self.sprite_x_counters[i] == 0 && self.sprite_attribute_latches[i].0 != 0xFF {
                let pixel =
                    ((self.sprite_pattern_shifts_lo[i] as u8 >> 7) & 0x01) |
                        ((self.sprite_pattern_shifts_hi[i] as u8 >> 6) & 0x02) |
                        ((self.sprite_attribute_latches[i].palette() << 2) & 0x1C);

                if pixel & 0x03 != 0 {
                    return (pixel, i);
                }
            }
        }

        (0, 0)
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

    fn set_vblank(&mut self) {
        self.nmi_occurred = true;
    }

    fn clear_vblank(&mut self) {
        self.nmi_occurred = false;
    }

    pub fn scanline_cycle(&self) -> u64 {
        self.cycles - self.scanline_start_cycle
    }

    pub fn step(&mut self, cpu: &mut Cpu, video_frame_sink: &mut Sink<VideoFrame>) {
        let scanline_cycle = self.scanline_cycle();

        let on_visible_scanline =
            VISIBLE_START_SCANLINE <= self.scanline && self.scanline <= VISIBLE_END_SCANLINE;
        let on_visible_cycle =
            1 <= scanline_cycle && scanline_cycle <= 256;

        let on_fetch_scanline = self.scanline <= VISIBLE_END_SCANLINE;
        let on_fetch_cycle =
                on_visible_cycle ||
                    // Pre-fetch tiles for the next scanline
                    321 <= scanline_cycle && scanline_cycle <= 336;


        // Handle backgrounds
        if self.rendering_enabled() {
            if on_visible_scanline && on_visible_cycle {
                self.render_pixel();
            }

            if on_fetch_scanline && on_fetch_cycle {
                self.shift_background_registers();
                match scanline_cycle % 8 {
                    1 => self.fetch_name_table_byte(),
                    3 => self.fetch_attribute_table_byte(),
                    5 => self.fetch_bitmap_byte_lo(),
                    7 => self.fetch_bitmap_byte_hi(),
                    0 => self.load_background_registers(),
                    _ => (),
                }
            }

            if on_fetch_scanline {
                if on_fetch_cycle &&
                    scanline_cycle % 8 == 0 {
                    // Increment the effective x scroll coordinate every 8 cycles
                    self.inc_coarse_x_with_wrap();
                }

                if scanline_cycle == 256 {
                    self.inc_y_with_wrap();
                } else if scanline_cycle == 257 {
                    // Copy bits related to horizontal position from t to v
                    self.regs.v = (self.regs.v & !0x041F) | (self.regs.t & 0x041F);
                }
            }

            if self.scanline == PRE_RENDER_SCANLINE &&
                280 <= scanline_cycle && scanline_cycle <= 304 {
                // Copy bits related to vertical position from t to v
                self.regs.v = (self.regs.v & !0x7BE0) | (self.regs.t & 0x7BE0);
            }
        }

        // Handle sprites
        if self.rendering_enabled() {
            if on_visible_scanline {
                if on_visible_cycle {
                    self.update_sprite_rendering_registers();
                }

                match scanline_cycle {
                    1...64 => {
                        if scanline_cycle == 1 {
                            self.sprite_evaluation_init();
                        }
                        if scanline_cycle % 2 == 0 {
                            self.oam.secondary[((scanline_cycle / 2) - 1) as usize] = 0xFF;
                        }
                    },
                    65...256 => {
                        if scanline_cycle % 2 == 1 {
                            self.sprite_evaluation_read_byte();
                        } else {
                            self.sprite_evaluation_write_byte();
                        }
                    },
                    _ => (),
                }
            }
        }

        if on_visible_scanline && 257 <= scanline_cycle && scanline_cycle <= 320 {
            if scanline_cycle % 8 == 0 {
                self.fetch_sprite_tile(((scanline_cycle - 264) / 8) as usize);
            }
        }

        match self.scanline {
            PRE_RENDER_SCANLINE => {
                if scanline_cycle == 1 {
                    self.clear_vblank();
                    self.regs.ppu_status.set(PpuStatus::SPRITE_OVERFLOW, false);
                    self.regs.ppu_status.set(PpuStatus::SPRITE_ZERO_HIT, false);
                }
            },
            POST_RENDER_SCANLINE => {
                if scanline_cycle == 0 {
                    let buffer: Vec<u32> = self.frame_buffer.iter()
                        .map(|b| PALETTE[(b & 0x3F) as usize]).collect();
                    video_frame_sink.append(buffer.into_boxed_slice());
                }
            },
            VBLANK_START_SCANLINE => {
                if scanline_cycle == 1 {
                    self.set_vblank();
                    if self.nmi_output && self.nmi_occurred {
                        cpu.request_interrupt(Interrupt::Nmi);
                    }
                }
            },
            _ => (),
        }


        self.cycles += 1;

        if scanline_cycle >= CYCLES_PER_SCANLINE ||
            // On pre-render scanline, for odd frames,
            // the cycle at the end of the scanline is skipped
            (self.rendering_enabled() &&
                self.scanline == PRE_RENDER_SCANLINE &&
                scanline_cycle == CYCLES_PER_SCANLINE - 1 &&
                self.frame % 2 != 0) {
            self.scanline_start_cycle = self.cycles;
            self.scanline += 1;
        }

        if self.scanline > VBLANK_END_SCANLINE {
            self.scanline = PRE_RENDER_SCANLINE;
            self.frame += 1;
        }
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

#[derive(Copy, Clone, Deserialize, Serialize)]
struct PpuCtrl { val: u8 }

impl PpuCtrl {
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
    #[derive(Deserialize, Serialize)]
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
    #[derive(Deserialize, Serialize)]
    struct PpuStatus: u8 {
        const NONE            = 0;
        const SPRITE_OVERFLOW = 1 << 5;
        const SPRITE_ZERO_HIT = 1 << 6;
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

#[derive(Copy, Clone, Deserialize, Serialize)]
enum WriteToggle {
    FirstWrite,
    SecondWrite,
}

#[derive(Copy, Clone, Deserialize, Serialize)]
pub struct Regs {
    // Registers mapped from CPU address space
    ppu_ctrl: PpuCtrl,          // 0x2000
    ppu_mask: PpuMask,          // 0x2001
    ppu_status: PpuStatus,      // 0x2002
    oam_addr: u8,               // 0x2003

    // Internal registers (for scrolling)
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
            v: 0,
            t: 0,
            x: 0,
            w: WriteToggle::FirstWrite,
        }
    }
}


// OAM (Object Attribute Memory) is internal memory inside the PPU that contains
// a display list of up to 64 sprites, where each sprite's information occupies 4 bytes
#[derive(Clone, Deserialize, Serialize)]
pub struct Oam {
    #[serde(with = "serde_bytes")]
    primary: Vec<u8>,
    #[serde(with = "serde_bytes")]
    secondary: Vec<u8>,

    // Used during sprite evaluation
    last_read_byte: u8,
    secondary_write_index: usize,
    sprite_0_found: bool,
    n: usize,
    m: usize,
}

impl Oam {
    const BYTES_PER_SPRITE: usize = 4;
    const PRIMARY_MAX_SPRITES: usize = 64;
    const PRIMARY_SIZE: usize = Oam::PRIMARY_MAX_SPRITES * Oam::BYTES_PER_SPRITE;
    const SECONDARY_MAX_SPRITES: usize = 8;
    const SECONDARY_SIZE: usize = Oam::SECONDARY_MAX_SPRITES * Oam::BYTES_PER_SPRITE;

    fn new() -> Oam {
        Oam {
            primary: vec![0u8; Oam::PRIMARY_SIZE],
            secondary: vec![0u8; Oam::SECONDARY_SIZE],
            last_read_byte: 0,
            secondary_write_index: 0,
            sprite_0_found: false,
            n: 0,
            m: 0,
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
    type Target = Vec<u8>;

    fn deref(&self) -> &Self::Target {
        &self.primary
    }
}

impl DerefMut for Oam {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.primary
    }
}

// 2KB internal dedicated Video RAM
#[derive(Clone, Deserialize, Serialize)]
pub struct Vram {
    #[serde(with = "serde_bytes")]
    bytes: Vec<u8>,
}

impl Vram {
    const SIZE: usize = 0x0800;

    fn new() -> Vram {
        Vram {
            bytes: vec![0u8; Vram::SIZE],
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
    type Target = Vec<u8>;

    fn deref(&self) -> &Self::Target {
        &self.bytes
    }
}

impl DerefMut for Vram {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.bytes
    }
}

#[derive(Clone, Deserialize, Serialize)]
pub struct PaletteRam { bytes: [u8; PaletteRam::SIZE] }

impl PaletteRam {
    const SIZE: usize = 32;
    const START_ADDRESS: u16 = 0x3F00;

    fn new() -> PaletteRam {
        PaletteRam {
            bytes: [0u8; PaletteRam::SIZE],
        }
    }

    fn index(address: u16) -> usize {
        let index = (address & 0x001F) as usize;

        // Addresses 0x3F10/0x3F14/0x3F18/0x3F1C are mirrors of 0x3F00/0x3F04/0x3F08/0x3F0C
        if index >= 0x10 && index % 4 == 0 {
            index - 0x10
        } else {
            index
        }
    }
}

impl Memory for PaletteRam {
    fn read_byte(&mut self, address: u16) -> u8 {
        self[PaletteRam::index(address)]
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        self[PaletteRam::index(address)] = value
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
        let address = address & 0x3FFF;

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
        let address = address & 0x3FFF;

        if address < PaletteRam::START_ADDRESS {
            let mut mapper = self.mapper.borrow_mut();
            mapper.ppu_write_byte(&mut self.vram, address, value);
        } else if address < 0x4000 {
            // Palette RAM is not configurable, always mapped to the
            // internal palette control in VRAM.
            self.palette_ram.write_byte(address, value);
        } else {
            panic!("Invalid write to PPU-space memory, address: 0x{:04x}, value: 0x{:02x}", address, value)
        }
    }
}

enum SpritePriority {
    InFrontOfBackground,
    BehindBackground,
}

#[derive(Clone, Copy, Deserialize, Serialize)]
pub struct SpriteAttributes(u8);

impl SpriteAttributes {
    fn palette(&self) -> u8 {
        (self.0 & 0x03) | 0x04
    }

    fn priority(&self) -> SpritePriority {
        if self.0 & 0x20 == 0 {
            SpritePriority::InFrontOfBackground
        } else {
            SpritePriority::BehindBackground
        }
    }

    fn flip_horizontally(&self) -> bool {
        self.0 & 0x40 == 0x40
    }

    fn flip_vertically(&self) -> bool {
        self.0 & 0x80 == 0x80
    }
}

#[derive(Clone, Copy)]
struct Sprite {
    y: u8,
    tile_index: u8,
    attributes: SpriteAttributes,
    x: u8,
}

impl Sprite {
    fn from_oam_bytes(oam_bytes: &[u8]) -> Sprite {
        Sprite {
            y: oam_bytes[0],
            tile_index: oam_bytes[1],
            attributes: SpriteAttributes(oam_bytes[2]),
            x: oam_bytes[3],
        }
    }

    fn flip_horizontally(&self) -> bool {
        self.attributes.flip_horizontally()
    }

    fn flip_vertically(&self) -> bool {
        self.attributes.flip_vertically()
    }
}

impl Default for Sprite {
    fn default() -> Sprite {
        Sprite {
            y: 0,
            tile_index: 0,
            attributes: SpriteAttributes(0),
            x: 0,
        }
    }
}