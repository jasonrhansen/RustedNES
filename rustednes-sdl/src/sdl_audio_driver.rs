use rustednes_common::audio_driver::AudioDriver;

use rustednes_core::sink::AudioSink;
use rustednes_core::time_source::TimeSource;

use std::collections::VecDeque;
use std::error;
use std::iter::Iterator;
use std::sync::atomic::{self, AtomicU64};
use std::sync::{Arc, Mutex};

use sdl2::audio::{AudioCallback, AudioDevice, AudioSpecDesired};
use sdl2::Sdl;

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
    }
}

impl Iterator for SampleBuffer {
    type Item = f32;

    fn next(&mut self) -> Option<f32> {
        self.samples_read += 1;
        self.samples.pop_front()
    }
}

pub struct SdlBufferSink {
    sample_buffer: Arc<Mutex<SampleBuffer>>,
}

impl AudioSink for SdlBufferSink {
    fn write_sample(&mut self, sample: f32) {
        let mut sample_buffer = self.sample_buffer.lock().unwrap();
        sample_buffer.push(sample);
    }

    fn samples_written(&self) -> usize {
        let sample_buffer = self.sample_buffer.lock().unwrap();
        sample_buffer.samples_written as usize
    }
}

pub struct SdlTimeSource {
    samples_written: Arc<AtomicU64>,
    sample_rate: u32,
}

impl TimeSource for SdlTimeSource {
    fn time_ns(&self) -> u64 {
        let samples_written = self.samples_written.load(atomic::Ordering::Relaxed);
        1_000_000_000 * samples_written / (self.sample_rate as u64)
    }
}

pub struct SdlAudioDriver {
    device: AudioDevice<SampleCallback>,
    sample_buffer: Arc<Mutex<SampleBuffer>>,
    samples_written: Arc<AtomicU64>,
}

impl SdlAudioDriver {
    pub fn new(
        sdl_context: Sdl,
        input_sample_rate: u32,
    ) -> Result<SdlAudioDriver, Box<dyn error::Error>> {
        let sample_buffer = Arc::new(Mutex::new(SampleBuffer::new()));
        let samples_written = Arc::new(AtomicU64::new(0));

        let audio_subsystem = sdl_context.audio()?;

        let desired_spec = AudioSpecDesired {
            freq: Some(44100),
            channels: Some(1), // mono
            samples: None,     // default sample size
        };

        let device = audio_subsystem.open_playback(None, &desired_spec, |spec| SampleCallback {
            sample_buffer: sample_buffer.clone(),
            samples_written: samples_written.clone(),
            resampler: LinearResampler::new(input_sample_rate, spec.freq as u32),
        })?;

        // Start playback
        device.resume();

        Ok(SdlAudioDriver {
            device,
            sample_buffer,
            samples_written,
        })
    }

    pub fn time_source(&self) -> SdlTimeSource {
        SdlTimeSource {
            samples_written: self.samples_written.clone(),
            sample_rate: self.device.spec().freq as u32,
        }
    }
}

impl AudioDriver for SdlAudioDriver {
    type Sink = SdlBufferSink;

    fn sink(&self) -> Self::Sink {
        Self::Sink {
            sample_buffer: self.sample_buffer.clone(),
        }
    }

    fn sample_rate(&self) -> u32 {
        self.device.spec().freq as u32
    }
}

struct SampleCallback {
    sample_buffer: Arc<Mutex<SampleBuffer>>,
    samples_written: Arc<AtomicU64>,
    resampler: LinearResampler,
}

impl AudioCallback for SampleCallback {
    type Channel = f32;

    fn callback(&mut self, out: &mut [f32]) {
        let mut read_buffer = self.sample_buffer.lock().unwrap();

        for x in out.iter_mut() {
            let val = self.resampler.next(&mut *read_buffer);
            *x = val;
            self.samples_written.fetch_add(1, atomic::Ordering::Relaxed);
        }
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
