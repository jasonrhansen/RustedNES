use rustednes_core::sink::AudioSink;

use super::AudioDriver;

pub struct NullAudioDriver;

impl AudioDriver for NullAudioDriver {
    type Sink = NullAudioSink;

    fn sink(&self) -> Self::Sink {
        Self::Sink {}
    }

    fn sample_rate(&self) -> u32 {
        1
    }
}

pub struct NullAudioSink;

impl AudioSink for NullAudioSink {
    fn write_sample(&mut self, _frame: f32) {
        // Do nothing
    }

    fn samples_written(&self) -> usize {
        0
    }
}
