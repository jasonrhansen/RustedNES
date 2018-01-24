#![allow(dead_code)]

use cpal::{EventLoop, Voice, UnknownTypeBuffer, default_endpoint};

use futures::stream::Stream;
use futures::task::{self, Executor, Run};

use sadnes_core::sink::{AudioFrame, SinkRef};
use sadnes_core::time_source::TimeSource;
use sadnes_core::audio_driver::AudioDriver;

use std::borrow::Cow;
use std::sync::{Arc, Mutex};
use std::iter::Iterator;
use std::thread::{self, JoinHandle};

use std::cmp::Ordering;

pub type CpalDriverError = Cow<'static, str>;

pub struct RingBuffer {
    inner: Box<[f32]>,

    write_pos: usize,
    read_pos: usize,

    samples_read: u64,
}

impl RingBuffer {
    fn push(&mut self, value: f32) {
        self.inner[self.write_pos] = value;

        self.write_pos += 1;
        if self.write_pos >= self.inner.len() {
            self.write_pos = 0;
        }
    }
}

impl Iterator for RingBuffer {
    type Item = f32;

    fn next(&mut self) -> Option<f32> {
        let ret = self.inner[self.read_pos];

        self.read_pos += 1;
        if self.read_pos >= self.inner.len() {
            self.read_pos = 0;
        }

        self.samples_read += 1;

        Some(ret)
    }
}

struct CpalDriverBufferSink {
    ring_buffer: Arc<Mutex<RingBuffer>>,
}

impl SinkRef<[AudioFrame]> for CpalDriverBufferSink {
    fn append(&mut self, buffer: &[AudioFrame]) {
        let mut ring_buffer = self.ring_buffer.lock().unwrap();
        for &sample in buffer {
            ring_buffer.push(sample);
            ring_buffer.push(sample);
        }
    }
}

struct CpalDriverTimeSource {
    ring_buffer: Arc<Mutex<RingBuffer>>,
    sample_rate: u32,
}

impl TimeSource for CpalDriverTimeSource {
    fn time_ns(&self) -> u64 {
        let ring_buffer = self.ring_buffer.lock().unwrap();
        1_000_000_000 * (ring_buffer.samples_read / 2) / (self.sample_rate as u64)
    }
}

struct CpalDriverExecutor;

impl Executor for CpalDriverExecutor {
    fn execute(&self, r: Run) {
        r.run();
    }
}

pub struct CpalDriver {
    ring_buffer: Arc<Mutex<RingBuffer>>,
    sample_rate: u32,

    _voice: Voice,
    _join_handle: JoinHandle<()>,
}

impl CpalDriver {
    pub fn new(desired_sample_rate: u32, desired_latency_ms: u32) -> Result<CpalDriver, CpalDriverError> {
        if desired_latency_ms == 0 {
            return Err(format!("desired_latency_ms must be greater than 0").into());
        }

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

        let buffer_frames = (desired_sample_rate * desired_latency_ms / 1000 * 2) as usize;
        let ring_buffer = Arc::new(Mutex::new(RingBuffer {
            inner: vec![0.0; buffer_frames].into_boxed_slice(),

            write_pos: 0,
            read_pos: 0,

            samples_read: 0,
        }));

        let event_loop = EventLoop::new();

        let (mut voice, stream) = Voice::new(&endpoint, &format, &event_loop).expect("Failed to create voice");
        voice.play();

        let read_ring_buffer = ring_buffer.clone();
        task::spawn(stream.for_each(move |output_buffer| {
            let mut read_ring_buffer = read_ring_buffer.lock().unwrap();

            match output_buffer {
                UnknownTypeBuffer::I16(mut buffer) => {
                    for sample in buffer.chunks_mut(format.channels.len()) {
                        for out in sample.iter_mut() {
                            *out = (read_ring_buffer.next().unwrap_or(0.0) * 32768.0) as i16;
                        }
                    }
                },
                UnknownTypeBuffer::U16(mut buffer) => {
                    for sample in buffer.chunks_mut(format.channels.len()) {
                        for out in sample.iter_mut() {
                            *out = ((read_ring_buffer.next().unwrap_or(0.0) * 32768.0) + 32768.0) as u16;
                        }
                    }
                },
                UnknownTypeBuffer::F32(mut buffer) => {
                    for sample in buffer.chunks_mut(format.channels.len()) {
                        for out in sample.iter_mut() {
                            *out = read_ring_buffer.next().unwrap_or(0.0);
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
            ring_buffer,
            sample_rate,

            _voice: voice,
            _join_handle: join_handle,
        })
    }


    pub fn time_source(&self) -> Box<TimeSource> {
        Box::new(CpalDriverTimeSource {
            ring_buffer: self.ring_buffer.clone(),
            sample_rate: self.sample_rate,
        })
    }
}

impl AudioDriver for CpalDriver {
    fn sink(&self) -> Box<SinkRef<[AudioFrame]>> {
        Box::new(CpalDriverBufferSink {
            ring_buffer: self.ring_buffer.clone(),
        })
    }

    fn sample_rate(&self) -> u64 {
        self.sample_rate as u64
    }
}
