use sink::{AudioFrame, SinkRef};

pub trait AudioDriver {
    fn sink(&self) -> Box<SinkRef<[AudioFrame]>>;
    fn sample_rate(&self) -> u64;
}