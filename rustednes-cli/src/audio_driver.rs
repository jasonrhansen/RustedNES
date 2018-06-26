use rustednes_core::sink::AudioSink;

pub trait AudioDriver {
    fn sink(&self) -> Box<AudioSink>;
    fn sample_rate(&self) -> u32;
}