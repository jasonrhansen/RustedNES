#![allow(dead_code)]

use cpal::{EventLoop, Voice, UnknownTypeBuffer, default_endpoint};

use futures::stream::Stream;
use futures::task::{self, Executor, Run};

use sadnes_core::sink::{AudioFrame, Sink};
use sadnes_core::time_source::TimeSource;

use std::borrow::Cow;
use std::sync::{Arc, Mutex};
use std::iter::Iterator;
use std::thread::{self, JoinHandle};
use std::collections::VecDeque;

use std::cmp::Ordering;

pub type CpalDriverError = Cow<'static, str>;

pub struct SampleBuffer {
    samples: VecDeque<f32>,
    samples_read: u64,
    samples_written: u64,
}

impl SampleBuffer {
    fn new() -> SampleBuffer {
        SampleBuffer {
            samples: VecDeque::new(),
            samples_read: 0,
            samples_written: 0,
        }
    }

    fn push(&mut self, value: f32) {
        self.samples.push_back(value);
        self.samples_written += 1;
    }
}

impl Iterator for SampleBuffer {
    type Item = f32;

    fn next(&mut self) -> Option<f32> {
        self.samples_read += 1;

        if self.samples_written < 100 {
            return None;
        }

        self.samples.pop_front()
    }
}

struct CpalDriverBufferSink {
    sample_buffer: Arc<Mutex<SampleBuffer>>,
}

impl Sink<AudioFrame> for CpalDriverBufferSink {
    fn append(&mut self, frame: AudioFrame) {
        let mut sample_buffer = self.sample_buffer.lock().unwrap();
        sample_buffer.push(frame);
    }
}

struct CpalDriverTimeSource {
    sample_buffer: Arc<Mutex<SampleBuffer>>,
    sample_rate: u32,
}

impl TimeSource for CpalDriverTimeSource {
    fn time_ns(&self) -> u64 {
        let sample_buffer = self.sample_buffer.lock().unwrap();
        1_000_000_000 * (sample_buffer.samples_read) / (self.sample_rate as u64)
    }
}

struct CpalDriverExecutor;

impl Executor for CpalDriverExecutor {
    fn execute(&self, r: Run) {
        r.run();
    }
}

pub struct CpalDriver {
    sample_buffer: Arc<Mutex<SampleBuffer>>,
    pub sample_rate: u32,
    pub output_sample_rate: u32,

    _voice: Voice,
    _join_handle: JoinHandle<()>,
}

impl CpalDriver {
    pub fn new(sample_rate: u32, desired_latency_ms: u32) -> Result<CpalDriver, CpalDriverError> {
        if desired_latency_ms == 0 {
            return Err(format!("desired_latency_ms must be greater than 0").into());
        }

        let endpoint = default_endpoint().expect("Failed to get audio endpoint");

        let compare_sample_rates = |x: u32, y:u32| -> Ordering {
            if x < sample_rate && y > sample_rate {
                return Ordering::Greater;
            } else if x > sample_rate && y < sample_rate {
                return Ordering::Less;
            } else if x < sample_rate && y < sample_rate {
                return x.cmp(&y).reverse();
            } else {
                return x.cmp(&y);
            }
        };

        let format = endpoint.supported_formats()
            .expect("Failed to get supported format list for endpoint")
            .filter(|format| format.channels.len() == 2)
            .min_by(|x, y| compare_sample_rates(x.samples_rate.0, y.samples_rate.0))
            .expect("Failed to find format with 2 channels");

        let output_sample_rate = format.samples_rate.0;

        let sample_buffer = Arc::new(Mutex::new(SampleBuffer::new()));

        let event_loop = EventLoop::new();

        let (mut voice, stream) = Voice::new(&endpoint, &format, &event_loop).expect("Failed to create voice");
        voice.play();

        let mut resampler = LinearResampler::new(sample_rate, output_sample_rate);

        let read_sample_buffer = sample_buffer.clone();
        task::spawn(stream.for_each(move |output_buffer| {
            let mut read_ring_buffer = read_sample_buffer.lock().unwrap();

            match output_buffer {
                UnknownTypeBuffer::I16(mut buffer) => {
                    for sample in buffer.chunks_mut(format.channels.len()) {
                        let val = (resampler.next(&mut *read_ring_buffer) * 32768.0) as i16;
                        for out in sample.iter_mut() {
                            *out = val;
                        }
                    }
                },
                UnknownTypeBuffer::U16(mut buffer) => {
                    for sample in buffer.chunks_mut(format.channels.len()) {
                        let val = ((resampler.next(&mut *read_ring_buffer) * 32768.0) + 32768.0) as u16;
                        for out in sample.iter_mut() {
                            *out = val;
                        }
                    }
                },
                UnknownTypeBuffer::F32(mut buffer) => {
                    for sample in buffer.chunks_mut(format.channels.len()) {
                        let val = resampler.next(&mut *read_ring_buffer);
                        for out in sample.iter_mut() {
                            *out = val;
                        }
                    }
                },
            }

            Ok(())
        })).execute(Arc::new(CpalDriverExecutor));

        let join_handle = thread::spawn(move || {
            event_loop.run();
        });

        Ok(CpalDriver {
            sample_buffer,
            sample_rate,
            output_sample_rate,

            _voice: voice,
            _join_handle: join_handle,
        })
    }


    pub fn time_source(&self) -> Box<TimeSource> {
        Box::new(CpalDriverTimeSource {
            sample_buffer: self.sample_buffer.clone(),
            sample_rate: self.sample_rate,
        })
    }

    pub fn sink(&self) -> Box<Sink<AudioFrame>> {
        Box::new(CpalDriverBufferSink {
            sample_buffer: self.sample_buffer.clone(),
        })
    }
}

struct LinearResampler {
    from_sample_rate: u32,
    to_sample_rate: u32,

    current_from_sample: f32,
    next_from_sample: f32,
    from_fract_pos: u32,
}

impl LinearResampler {
    fn new(from_sample_rate: u32, to_sample_rate: u32) -> LinearResampler {
        let sample_rate_gcd = {
            fn gcd(a: u32, b: u32) -> u32 {
                if b == 0 {
                    a
                } else {
                    gcd(b, a % b)
                }
            }

            gcd(from_sample_rate, to_sample_rate)
        };

        LinearResampler {
            from_sample_rate: from_sample_rate / sample_rate_gcd,
            to_sample_rate: to_sample_rate / sample_rate_gcd,

            current_from_sample: 0.0,
            next_from_sample: 0.0,
            from_fract_pos: 0,
        }
    }

    fn next(&mut self, input: &mut Iterator<Item = f32>) -> f32 {
        fn interpolate(a: f32, b: f32, num: u32, denom: u32) -> f32 {
            ((a * ((denom - num) as f32) + b * (num as f32)) / (denom as f32))
        }

        let ret = interpolate(self.current_from_sample, self.next_from_sample,
                              self.from_fract_pos, self.to_sample_rate);

        self.from_fract_pos += self.from_sample_rate;
        while self.from_fract_pos > self.to_sample_rate {
            self.from_fract_pos -= self.to_sample_rate;

            self.current_from_sample = self.next_from_sample;
            self.next_from_sample = input.next().unwrap_or(0.0);
        }

        ret
    }
}