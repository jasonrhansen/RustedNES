use rustednes_core::sink::AudioSink;

pub trait AudioDriver {
    type S: AudioSink;

    fn sink(&self) -> Self::S;
    fn sample_rate(&self) -> u32;
}
