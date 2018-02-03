use sadnes_core::sink::{AudioFrame, Sink};

pub struct NullAudioSink {}

impl Sink<AudioFrame> for NullAudioSink {
    fn append(&mut self, frame: AudioFrame) {
        // Do nothing
    }
}
