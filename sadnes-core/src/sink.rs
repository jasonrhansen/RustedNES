pub trait Sink<T> {
    fn append(&mut self, value: T);
}

pub trait SinkRef<T: ?Sized> {
    fn append(&mut self, value: &T);
}


pub type VideoFrame = Box<[u32]>;

pub type AudioFrame = f32;
