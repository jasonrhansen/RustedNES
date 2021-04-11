use lazy_static::lazy_static;

use std::mem;

pub trait AudioSink {
    fn write_sample(&mut self, sample: f32);
    fn samples_written(&self) -> usize;
}

pub struct AudioSinkF32<'a> {
    buffer: &'a mut [(f32, f32)],
    buffer_pos: usize,
}

impl<'a> AudioSinkF32<'a> {
    pub fn new(buffer: &'a mut [(f32, f32)]) -> Self {
        AudioSinkF32 {
            buffer,
            buffer_pos: 0,
        }
    }
}

impl<'a> AudioSink for AudioSinkF32<'a> {
    fn write_sample(&mut self, sample: f32) {
        self.buffer[self.buffer_pos] = (sample, sample);
        self.buffer_pos += 1;
    }

    fn samples_written(&self) -> usize {
        self.buffer_pos
    }
}

pub struct AudioSinkI16<'a> {
    buffer: &'a mut [(i16, i16)],
    buffer_pos: usize,
}

impl<'a> AudioSinkI16<'a> {
    pub fn new(buffer: &'a mut [(i16, i16)]) -> Self {
        AudioSinkI16 {
            buffer,
            buffer_pos: 0,
        }
    }
}

impl<'a> AudioSink for AudioSinkI16<'a> {
    fn write_sample(&mut self, sample: f32) {
        let sample = (sample * 32768.0) as i16;
        self.buffer[self.buffer_pos] = (sample, sample);
        self.buffer_pos += 1;
    }

    fn samples_written(&self) -> usize {
        self.buffer_pos
    }
}

pub struct AudioSinkU16<'a> {
    buffer: &'a mut [(u16, u16)],
    buffer_pos: usize,
}

impl<'a> AudioSinkU16<'a> {
    pub fn new(buffer: &'a mut [(u16, u16)]) -> Self {
        AudioSinkU16 {
            buffer,
            buffer_pos: 0,
        }
    }
}

impl<'a> AudioSink for AudioSinkU16<'a> {
    fn write_sample(&mut self, sample: f32) {
        let sample = ((sample * 32768.0) + 32768.0) as u16;
        self.buffer[self.buffer_pos] = (sample, sample);
        self.buffer_pos += 1;
    }

    fn samples_written(&self) -> usize {
        self.buffer_pos
    }
}

pub trait VideoSink {
    fn write_frame(&mut self, frame_buffer: &[u8]);
    fn frame_written(&self) -> bool;
    fn pixel_size(&self) -> usize;
}

impl<S: VideoSink + ?Sized> VideoSink for Box<S> {
    fn write_frame(&mut self, frame_buffer: &[u8]) {
        (**self).write_frame(frame_buffer);
    }

    fn frame_written(&self) -> bool {
        (**self).frame_written()
    }

    fn pixel_size(&self) -> usize {
        (**self).pixel_size()
    }
}

pub struct Rgb565VideoSink<'a> {
    buffer: &'a mut [u16],
    frame_written: bool,
}

impl<'a> Rgb565VideoSink<'a> {
    pub fn new(buffer: &'a mut [u16]) -> Self {
        Rgb565VideoSink {
            buffer,
            frame_written: false,
        }
    }
}

impl<'a> VideoSink for Rgb565VideoSink<'a> {
    fn write_frame(&mut self, frame_buffer: &[u8]) {
        for (i, palette_index) in frame_buffer.iter().enumerate() {
            self.buffer[i] = RGB565_PALETTE[*palette_index as usize];
        }
        self.frame_written = true;
    }

    fn frame_written(&self) -> bool {
        self.frame_written
    }

    fn pixel_size(&self) -> usize {
        mem::size_of::<u16>()
    }
}

pub struct Xrgb1555VideoSink<'a> {
    buffer: &'a mut [u16],
    frame_written: bool,
}

impl<'a> Xrgb1555VideoSink<'a> {
    pub fn new(buffer: &'a mut [u16]) -> Self {
        Xrgb1555VideoSink {
            buffer,
            frame_written: false,
        }
    }
}

impl<'a> VideoSink for Xrgb1555VideoSink<'a> {
    fn write_frame(&mut self, frame_buffer: &[u8]) {
        for (i, palette_index) in frame_buffer.iter().enumerate() {
            self.buffer[i] = XRGB1555_PALETTE[*palette_index as usize];
        }
        self.frame_written = true;
    }

    fn frame_written(&self) -> bool {
        self.frame_written
    }

    fn pixel_size(&self) -> usize {
        mem::size_of::<u16>()
    }
}

// Appropriate for use with javascript ImageData
pub struct WebVideoSink<'a> {
    buffer: &'a mut [u32],
    frame_written: bool,
}

impl<'a> WebVideoSink<'a> {
    pub fn new(buffer: &'a mut [u32]) -> Self {
        WebVideoSink {
            buffer,
            frame_written: false,
        }
    }
}

impl<'a> VideoSink for WebVideoSink<'a> {
    fn write_frame(&mut self, frame_buffer: &[u8]) {
        for (i, palette_index) in frame_buffer.iter().enumerate() {
            self.buffer[i] = WEB_PALETTE[*palette_index as usize];
        }
        self.frame_written = true;
    }

    fn frame_written(&self) -> bool {
        self.frame_written
    }

    fn pixel_size(&self) -> usize {
        mem::size_of::<u32>()
    }
}

pub struct Xrgb8888VideoSink<'a> {
    buffer: &'a mut [u32],
    frame_written: bool,
}

impl<'a> Xrgb8888VideoSink<'a> {
    pub fn new(buffer: &'a mut [u32]) -> Self {
        Xrgb8888VideoSink {
            buffer,
            frame_written: false,
        }
    }
}

impl<'a> VideoSink for Xrgb8888VideoSink<'a> {
    fn write_frame(&mut self, frame_buffer: &[u8]) {
        for (i, palette_index) in frame_buffer.iter().enumerate() {
            self.buffer[i] = XRGB8888_PALETTE[*palette_index as usize];
        }
        self.frame_written = true;
    }

    fn frame_written(&self) -> bool {
        self.frame_written
    }

    fn pixel_size(&self) -> usize {
        mem::size_of::<u32>()
    }
}

#[allow(clippy::unreadable_literal)]
static XRGB8888_PALETTE: &[u32] = &[
    0x666666, 0x002A88, 0x1412A7, 0x3B00A4, 0x5C007E, 0x6E0040, 0x6C0600, 0x561D00, 0x333500,
    0x0B4800, 0x005200, 0x004F08, 0x00404D, 0x000000, 0x000000, 0x000000, 0xADADAD, 0x155FD9,
    0x4240FF, 0x7527FE, 0xA01ACC, 0xB71E7B, 0xB53120, 0x994E00, 0x6B6D00, 0x388700, 0x0C9300,
    0x008F32, 0x007C8D, 0x000000, 0x000000, 0x000000, 0xFFFEFF, 0x64B0FF, 0x9290FF, 0xC676FF,
    0xF36AFF, 0xFE6ECC, 0xFE8170, 0xEA9E22, 0xBCBE00, 0x88D800, 0x5CE430, 0x45E082, 0x48CDDE,
    0x4F4F4F, 0x000000, 0x000000, 0xFFFEFF, 0xC0DFFF, 0xD3D2FF, 0xE8C8FF, 0xFBC2FF, 0xFEC4EA,
    0xFECCC5, 0xF7D8A5, 0xE4E594, 0xCFEF96, 0xBDF4AB, 0xB3F3CC, 0xB5EBF2, 0xB8B8B8, 0x000000,
    0x000000,
];

lazy_static! {
    static ref XRGB1555_PALETTE: [u16; 64] = {
        let mut palette = [0; 64];
        for n in 0..64 {
            let color = XRGB8888_PALETTE[n];
            let r = ((color >> 19) & 0x1F) as u16;
            let g = ((color >> 11) & 0x1F) as u16;
            let b = ((color >> 3) & 0x1F) as u16;
            palette[n] = (r << 10) | (g << 5) | b;
        }
        palette
    };
    static ref RGB565_PALETTE: [u16; 64] = {
        let mut palette = [0; 64];
        for n in 0..64 {
            let color = XRGB8888_PALETTE[n];
            let r = ((color >> 19) & 0x1F) as u16;
            let g = ((color >> 10) & 0x3F) as u16;
            let b = ((color >> 3) & 0x1F) as u16;
            palette[n] = (r << 11) | (g << 5) | b;
        }
        palette
    };
    static ref WEB_PALETTE: [u32; 64] = {
        let mut palette = [0; 64];
        for n in 0..64 {
            let color = XRGB8888_PALETTE[n];
            let r = ((color >> 16) & 0xFF) as u32;
            let g = ((color >> 8) & 0xFF) as u32;
            let b = (color & 0xFF) as u32;
            palette[n] = 0xFF00_0000 | (b << 16) | (g << 8) | r;
        }
        palette
    };
}
