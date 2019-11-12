use crate::audio_driver::AudioDriver;

use rustednes_core::sink::AudioSink;
use rustednes_core::time_source::TimeSource;

use std::borrow::Cow;
use std::cmp::Ordering;
use std::collections::VecDeque;
use std::iter::Iterator;
use std::sync::atomic::{self, AtomicU64};
use std::sync::{Arc, Mutex};
use std::thread::{self, JoinHandle};

use cpal::traits::DeviceTrait;
use cpal::traits::EventLoopTrait;
use cpal::traits::HostTrait;

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
    samples_written: Arc<AtomicU64>,
    sample_rate: u32,
}

impl TimeSource for CpalDriverTimeSource {
    fn time_ns(&self) -> u64 {
        let samples_written = self.samples_written.load(atomic::Ordering::Relaxed);
        1_000_000_000 * samples_written / (self.sample_rate as u64)
    }
}

pub struct CpalDriver {
    sample_buffer: Arc<Mutex<SampleBuffer>>,
    samples_written: Arc<AtomicU64>,
    sample_rate: u32,

    _join_handle: JoinHandle<()>,
}

impl CpalDriver {
    pub fn new(
        input_sample_rate: u32,
        desired_output_sample_rate: u32,
    ) -> Result<CpalDriver, CpalDriverError> {
        let host = cpal::default_host();
        let device = host
            .default_output_device()
            .expect("failed to get default output device");

        let compare_sample_rates = |x: u32, y: u32| -> Ordering {
            if x < desired_output_sample_rate && y > desired_output_sample_rate {
                return Ordering::Greater;
            } else if x > desired_output_sample_rate && y < desired_output_sample_rate {
                return Ordering::Less;
            } else if x < desired_output_sample_rate && y < desired_output_sample_rate {
                return x.cmp(&y).reverse();
            } else {
                return x.cmp(&y);
            }
        };

        let format = device
            .supported_output_formats()
            .expect("failed to get supported format list for device")
            .filter(|format| format.channels == 2)
            .min_by(|x, y| compare_sample_rates(x.min_sample_rate.0, y.min_sample_rate.0))
            .expect("failed to find format with 2 channels");

        let format = cpal::Format {
            channels: format.channels,
            sample_rate: format.min_sample_rate,
            data_type: format.data_type,
        };

        let output_sample_rate = format.sample_rate.0;

        let sample_buffer = Arc::new(Mutex::new(SampleBuffer::new()));
        let samples_written = Arc::new(AtomicU64::new(0));

        let event_loop = host.event_loop();

        let stream_id = event_loop.build_output_stream(&device, &format).unwrap();
        event_loop
            .play_stream(stream_id)
            .expect("failed to play stream");

        let mut resampler = LinearResampler::new(input_sample_rate, output_sample_rate);

        let read_sample_buffer = sample_buffer.clone();
        let output_samples_written = samples_written.clone();

        let join_handle = thread::spawn(move || {
            event_loop.run(move |stream_id, stream_result| {
                let stream_data = match stream_result {
                    Ok(data) => data,
                    Err(err) => {
                        eprintln!("an error occurred on stream {:?}: {}", stream_id, err);
                        return;
                    }
                };

                let mut read_ring_buffer = read_sample_buffer.lock().unwrap();

                match stream_data {
                    cpal::StreamData::Output {
                        buffer: cpal::UnknownTypeOutputBuffer::I16(mut buffer),
                    } => {
                        for sample in buffer.chunks_mut(format.channels as usize) {
                            let val = (resampler.next(&mut *read_ring_buffer) * 32768.0) as i16;
                            for out in sample.iter_mut() {
                                *out = val;
                            }
                            output_samples_written.fetch_add(1, atomic::Ordering::Relaxed);
                        }
                    }
                    cpal::StreamData::Output {
                        buffer: cpal::UnknownTypeOutputBuffer::U16(mut buffer),
                    } => {
                        for sample in buffer.chunks_mut(format.channels as usize) {
                            let val = ((resampler.next(&mut *read_ring_buffer) * 32768.0) + 32768.0)
                                as u16;
                            for out in sample.iter_mut() {
                                *out = val;
                            }
                            output_samples_written.fetch_add(1, atomic::Ordering::Relaxed);
                        }
                    }
                    cpal::StreamData::Output {
                        buffer: cpal::UnknownTypeOutputBuffer::F32(mut buffer),
                    } => {
                        for sample in buffer.chunks_mut(format.channels as usize) {
                            let val = resampler.next(&mut *read_ring_buffer);
                            for out in sample.iter_mut() {
                                *out = val;
                            }
                            output_samples_written.fetch_add(1, atomic::Ordering::Relaxed);
                        }
                    }
                    _ => (),
                }
            });
        });

        Ok(CpalDriver {
            sample_buffer,
            samples_written: samples_written,
            sample_rate: output_sample_rate,
            _join_handle: join_handle,
        })
    }

    pub fn time_source(&self) -> Box<dyn TimeSource> {
        Box::new(CpalDriverTimeSource {
            samples_written: self.samples_written.clone(),
            sample_rate: self.sample_rate,
        })
    }
}

impl AudioDriver for CpalDriver {
    fn sink(&self) -> Box<dyn AudioSink> {
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

    fn next(&mut self, input: &mut dyn Iterator<Item = f32>) -> f32 {
        fn interpolate(a: f32, b: f32, num: u32, denom: u32) -> f32 {
            ((a * ((denom - num) as f32) + b * (num as f32)) / (denom as f32))
        }

        let ret = interpolate(
            self.current_from_sample,
            self.next_from_sample,
            self.from_fract_pos,
            self.to_sample_rate,
        );

        self.from_fract_pos += self.from_sample_rate;
        while self.from_fract_pos > self.to_sample_rate {
            self.from_fract_pos -= self.to_sample_rate;

            self.current_from_sample = self.next_from_sample;
            self.next_from_sample = input.next().unwrap_or(self.current_from_sample);
        }

        ret
    }
}
