use std::collections::VecDeque;

pub struct SampleBuffer {
    samples: VecDeque<f32>,
    samples_written: usize,
    max_length: usize,
}

impl SampleBuffer {
    pub fn with_max_length(max_length: usize) -> SampleBuffer {
        SampleBuffer {
            samples: Default::default(),
            samples_written: 0,
            max_length,
        }
    }

    pub fn push(&mut self, value: f32) {
        if self.samples.len() < self.max_length {
            self.samples.push_back(value);
        }
        self.samples_written += 1;
    }

    pub fn samples_written(&self) -> usize {
        self.samples_written
    }
}

impl Iterator for SampleBuffer {
    type Item = f32;

    fn next(&mut self) -> Option<f32> {
        self.samples.pop_front()
    }
}
