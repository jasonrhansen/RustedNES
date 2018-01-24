use sadnes_core::sink::*;

pub struct VideoFrameSink {
    frame: Option<VideoFrame>
}

impl VideoFrameSink {
    pub fn new() -> VideoFrameSink {
        VideoFrameSink { frame: None }
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