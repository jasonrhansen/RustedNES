pub trait Sink<T> {
    fn append(&mut self, value: T);
}


pub type VideoFrame = Box<[u32]>;

pub type AudioFrame = i16;
