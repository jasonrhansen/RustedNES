pub trait AudioSink {
    fn write_sample(&mut self, sample: f32);
    fn samples_written(&self) -> usize;
}

impl<A: AudioSink + ?Sized> AudioSink for Box<A> {
    fn write_sample(&mut self, sample: f32) {
        (**self).write_sample(sample);
    }

    fn samples_written(&self) -> usize {
        (**self).samples_written()
    }
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
