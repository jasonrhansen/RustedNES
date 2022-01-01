use rustednes_common::audio::{AudioDriver, LinearResampler, SampleBuffer};
use rustednes_common::time::TimeSource;

use rustednes_core::sink::AudioSink;

use std::error;
use std::sync::atomic::{self, AtomicU64};
use std::sync::{Arc, Mutex};

use sdl2::audio::{AudioCallback, AudioDevice, AudioSpecDesired};
use sdl2::Sdl;

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
        sample_buffer.samples_written()
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
        let sample_buffer = Arc::new(Mutex::new(SampleBuffer::with_max_length(32 * 1024)));
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
