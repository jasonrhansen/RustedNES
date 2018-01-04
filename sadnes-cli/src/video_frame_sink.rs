use sadnes_core::sinks::*;

pub struct VideoFrameSink {
    frame: Option<VideoFrame>
}

impl VideoFrameSink {
    pub fn new() -> VideoFrameSink {
        VideoFrameSink { frame: None }
    }

    pub fn has_frame(&self) -> bool {
        self.frame.is_some()
    }

    pub fn into_frame(self) -> Option<VideoFrame> {
        self.frame
    }
}

impl Sink<VideoFrame> for VideoFrameSink {
    fn append(&mut self, frame: VideoFrame) {
        self.frame = Some(frame);
    }
}