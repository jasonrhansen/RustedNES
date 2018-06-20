pub type AudioFrame = (i16, i16);

pub struct AudioSink<'a> {
    pub buffer: &'a mut [AudioFrame],
    pub buffer_pos: usize,
}

impl<'a> AudioSink<'a> {
    pub fn append(&mut self, frame: AudioFrame) {
        self.buffer[self.buffer_pos] = frame;
        self.buffer_pos += 1;
    }
}

pub type VideoFrame = Box<[u32]>;

pub enum PixelBuffer<'a> {
    Xrgb1555(&'a mut [u16], usize),
    Rgb565(&'a mut [u16], usize),
    Xrgb8888(&'a mut [u32], usize),
}

impl<'a> PixelBuffer<'a> {
    pub fn pitch(&self) -> usize {
        match self {
            &PixelBuffer::Xrgb1555(_, pitch) => pitch,
            &PixelBuffer::Rgb565(_, pitch) => pitch,
            &PixelBuffer::Xrgb8888(_, pitch) => pitch,
        }
    }
}

pub struct VideoSink<'a> {
    pub buffer: PixelBuffer<'a>,
    pub is_populated: bool,
}
