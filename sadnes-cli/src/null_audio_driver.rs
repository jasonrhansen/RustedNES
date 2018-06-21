use audio_driver::AudioDriver;
use sadnes_core::sink::AudioSink;

pub struct NullAudioDriver;

impl AudioDriver for NullAudioDriver {
    fn sink(&self) -> Box<AudioSink> {
        Box::new(NullAudioSink{})
    }

    fn sample_rate(&self) -> u32 {
        1
    }
}

struct NullAudioSink;

impl AudioSink for NullAudioSink {
    fn write_sample(&mut self, _frame: f32) {
        // Do nothing
    }

    fn samples_written(&self) -> usize {
        0
    }
}
