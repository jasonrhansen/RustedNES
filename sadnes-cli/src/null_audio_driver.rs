use audio_driver::AudioDriver;
use sadnes_core::sink::{AudioFrame, Sink};

pub struct NullAudioDriver {}

impl AudioDriver for NullAudioDriver {
    fn sink(&self) -> Box<Sink<AudioFrame>> {
        Box::new(NullAudioSink{})
    }

    fn sample_rate(&self) -> u32 {
        1
    }
}

struct NullAudioSink {}

impl Sink<AudioFrame> for NullAudioSink {
    fn append(&mut self, _frame: AudioFrame) {
        // Do nothing
    }
}
