use sadnes_core::sink::{AudioFrame, Sink};

pub trait AudioDriver {
    fn sink(&self) -> Box<Sink<AudioFrame>>;
    fn sample_rate(&self) -> u32;
}