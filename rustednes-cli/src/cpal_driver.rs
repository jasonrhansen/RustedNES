use crate::audio_driver::AudioDriver;

use rustednes_core::sink::AudioSink;
use rustednes_core::time_source::TimeSource;

use std::error;
use std::iter::Iterator;
use std::sync::atomic::{self, AtomicU64};
use std::sync::{Arc, Mutex};
use std::{collections::VecDeque, thread};

use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::SampleFormat;

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

pub struct CpalDriverBufferSink {
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

pub struct CpalDriverTimeSource {
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
}

impl CpalDriver {
    pub fn new(input_sample_rate: u32) -> Result<CpalDriver, Box<dyn error::Error>> {
        let host = cpal::default_host();
        let device = host
            .default_output_device()
            .expect("failed to get default output device");

        let supported_config = device
            .supported_output_configs()
            .expect("failed to get supported output configs for device")
            .next()
            .expect("failed to find stream config")
            .with_max_sample_rate();
        let config = supported_config.config();
        let channels = config.channels as usize;
        let output_sample_rate = config.sample_rate.0;

        let sample_buffer = Arc::new(Mutex::new(SampleBuffer::new()));
        let samples_written = Arc::new(AtomicU64::new(0));

        let mut resampler = LinearResampler::new(input_sample_rate, output_sample_rate);
        let read_sample_buffer = sample_buffer.clone();
        let output_samples_written = samples_written.clone();

        thread::spawn(move || {
            let err_fn = |err| eprintln!("an error occurred on the output audio stream: {}", err);
            let stream = match supported_config.sample_format() {
                SampleFormat::F32 => device.build_output_stream(
                    &config,
                    move |data: &mut [f32], _: &cpal::OutputCallbackInfo| {
                        let mut read_ring_buffer = read_sample_buffer.lock().unwrap();
                        for sample in data.chunks_mut(channels) {
                            let val = resampler.next(&mut *read_ring_buffer);
                            for out in sample.iter_mut() {
                                *out = val;
                            }
                            output_samples_written.fetch_add(1, atomic::Ordering::Relaxed);
                        }
                    },
                    err_fn,
                ),
                SampleFormat::I16 => device.build_output_stream(
                    &config,
                    move |data: &mut [i16], _: &cpal::OutputCallbackInfo| {
                        let mut read_ring_buffer = read_sample_buffer.lock().unwrap();
                        for sample in data.chunks_mut(channels) {
                            let val = (resampler.next(&mut *read_ring_buffer) * 32768.0) as i16;
                            for out in sample.iter_mut() {
                                *out = val;
                            }
                            output_samples_written.fetch_add(1, atomic::Ordering::Relaxed);
                        }
                    },
                    err_fn,
                ),
                SampleFormat::U16 => device.build_output_stream(
                    &config,
                    move |data: &mut [u16], _: &cpal::OutputCallbackInfo| {
                        let mut read_ring_buffer = read_sample_buffer.lock().unwrap();
                        for sample in data.chunks_mut(channels) {
                            let val = ((resampler.next(&mut *read_ring_buffer) * 32768.0) + 32768.0)
                                as u16;
                            for out in sample.iter_mut() {
                                *out = val;
                            }
                            output_samples_written.fetch_add(1, atomic::Ordering::Relaxed);
                        }
                    },
                    err_fn,
                ),
            };

            match stream {
                Ok(stream) => {
                    if let Err(e) = stream.play() {
                        eprintln!("unable to play audio stream: {}", e);
                    } else {
                        // Keep thread alive or stream doesn't play.
                        loop {
                            thread::sleep(std::time::Duration::from_secs(1))
                        }
                    }
                }
                Err(e) => {
                    eprintln!("unable to build audio stream: {}", e);
                }
            }
        });

        Ok(CpalDriver {
            sample_buffer,
            samples_written,
            sample_rate: output_sample_rate,
        })
    }

    pub fn time_source(&self) -> CpalDriverTimeSource {
        CpalDriverTimeSource {
            samples_written: self.samples_written.clone(),
            sample_rate: self.sample_rate,
        }
    }
}

impl AudioDriver for CpalDriver {
    type S = CpalDriverBufferSink;

    fn sink(&self) -> CpalDriverBufferSink {
        CpalDriverBufferSink {
            sample_buffer: self.sample_buffer.clone(),
        }
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
            (a * ((denom - num) as f32) + b * (num as f32)) / (denom as f32)
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
