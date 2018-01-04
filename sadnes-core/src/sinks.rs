pub trait Sink<T> {
    fn append(&mut self, value: T);
}


pub type VideoFrame = Box<[u8]>;

pub type AudioFrame = i16;
