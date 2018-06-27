use audio_driver::AudioDriver;
use cpal::{default_endpoint, EventLoop, UnknownTypeBuffer, Voice};
use futures::stream::Stream;
use futures::task::{self, Executor, Run};
use rustednes_core::sink::AudioSink;
use rustednes_core::time_source::TimeSource;
use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::VecDeque;
use std::iter::Iterator;
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};

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

        self.samples.pop_front()
    }
}

struct CpalDriverBufferSink {
    sample_buffer: Arc<Mutex<SampleBuffer>>,
}

impl AudioSink for CpalDriverBufferSink {
    fn write_sample(&mut self, sample: f32) {
        let mut sample_buffer = self.sample_buffer.lock().unwrap();
        sample_buffer.push(sample);
    }

    fn samples_written(&self) -> usize {
        let sample_buffer = self.sample_buffer.lock().unwrap();
        sample_buffer.samples_written as usize
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
    sample_rate: u32,

    _voice: Voice,
    _join_handle: JoinHandle<()>,
}

impl CpalDriver {
    pub fn new(desired_sample_rate: u32) -> Result<CpalDriver, CpalDriverError> {
        let endpoint = default_endpoint().expect("Failed to get audio endpoint");

        let compare_sample_rates = |x: u32, y:u32| -> Ordering {
            if x < desired_sample_rate && y > desired_sample_rate {
                return Ordering::Greater;
            } else if x > desired_sample_rate && y < desired_sample_rate {
                return Ordering::Less;
            } else if x < desired_sample_rate && y < desired_sample_rate {
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

        let sample_rate = format.samples_rate.0;

        let sample_buffer = Arc::new(Mutex::new(SampleBuffer::new()));

        let event_loop = EventLoop::new();

        let (mut voice, stream) = Voice::new(&endpoint, &format, &event_loop).expect("Failed to create voice");
        voice.play();

        let mut resampler = LinearResampler::new(desired_sample_rate, sample_rate);

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
}

impl AudioDriver for CpalDriver {
    fn sink(&self) -> Box<AudioSink> {
        Box::new(CpalDriverBufferSink {
            sample_buffer: self.sample_buffer.clone(),
        })
    }

    fn sample_rate(&self) -> u32 {
        self.sample_rate
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