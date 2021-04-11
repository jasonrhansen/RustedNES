use crate::audio_driver::AudioDriver;

use rustednes_core::sink::AudioSink;

pub struct NullAudioDriver;

impl AudioDriver for NullAudioDriver {
    type S = NullAudioSink;

    fn sink(&self) -> NullAudioSink {
        NullAudioSink {}
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
