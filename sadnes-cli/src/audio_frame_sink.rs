use sadnes_core::sink::*;

use std::collections::VecDeque;

pub struct AudioFrameSink {
    inner: VecDeque<AudioFrame>,
}

impl AudioFrameSink {
    pub fn new() -> AudioFrameSink {
        AudioFrameSink {
            inner: VecDeque::new(),
        }
    }

    pub fn as_slices(&self) -> (&[AudioFrame], &[AudioFrame]) {
        self.inner.as_slices()
    }
}

impl Sink<AudioFrame> for AudioFrameSink {
    fn append(&mut self, frame: AudioFrame) {
        self.inner.push_back(frame);
    }
}
