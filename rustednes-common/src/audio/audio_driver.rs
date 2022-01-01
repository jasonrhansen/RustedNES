use rustednes_core::sink::AudioSink;

pub trait AudioDriver {
    type Sink: AudioSink;

    fn sink(&self) -> Self::Sink;
    fn sample_rate(&self) -> u32;
}
