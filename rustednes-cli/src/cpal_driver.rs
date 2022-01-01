use rustednes_common::audio::{AudioDriver, LinearResampler, SampleBuffer};
use rustednes_common::time::TimeSource;

use rustednes_core::sink::AudioSink;

use std::error;
use std::sync::atomic::{self, AtomicU64};
use std::sync::{Arc, Mutex};

use cpal::traits::{DeviceTrait, HostTrait, StreamTrait};
use cpal::{SampleFormat, Stream};

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
        sample_buffer.samples_written()
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
    _stream: Stream,
    sample_buffer: Arc<Mutex<SampleBuffer>>,
    samples_written: Arc<AtomicU64>,
    sample_rate: u32,
}

impl CpalDriver {
    pub fn new(input_sample_rate: u32) -> Result<CpalDriver, Box<dyn error::Error>> {
        let host = cpal::default_host();
        let device = host
            .default_output_device()
            .ok_or("failed to get default output device")?;

        let supported_config = device
            .supported_output_configs()?
            .next()
            .ok_or("failed to find stream config")?
            .with_max_sample_rate();

        let config = supported_config.config();
        let channels = config.channels as usize;
        let output_sample_rate = config.sample_rate.0;

        let sample_buffer = Arc::new(Mutex::new(SampleBuffer::with_max_length(32 * 1024)));
        let samples_written = Arc::new(AtomicU64::new(0));

        let mut resampler = LinearResampler::new(input_sample_rate, output_sample_rate);
        let read_sample_buffer = sample_buffer.clone();
        let output_samples_written = samples_written.clone();

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
                        let val =
                            ((resampler.next(&mut *read_ring_buffer) * 32768.0) + 32768.0) as u16;
                        for out in sample.iter_mut() {
                            *out = val;
                        }
                        output_samples_written.fetch_add(1, atomic::Ordering::Relaxed);
                    }
                },
                err_fn,
            ),
        };

        let stream = stream?;
        stream.play()?;

        Ok(CpalDriver {
            _stream: stream,
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
    type Sink = CpalDriverBufferSink;

    fn sink(&self) -> Self::Sink {
        Self::Sink {
            sample_buffer: self.sample_buffer.clone(),
        }
    }

    fn sample_rate(&self) -> u32 {
        self.sample_rate
    }
}
