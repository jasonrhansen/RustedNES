use rustednes_core::sink::AudioSink;

pub trait AudioDriver {
    fn sink(&self) -> Box<dyn AudioSink>;
    fn sample_rate(&self) -> u32;
}
