use rustednes_core::sink::{AudioSink, VideoSink};

use crate::emulation_mode::EmulationMode;

use rustednes_core::nes::Nes;

pub trait DebugEmulator<A, V>
where
    A: AudioSink + Sized,
    V: VideoSink,
{
    fn nes(&mut self) -> &mut Nes;
    fn audio_frame_sink(&mut self) -> &mut A;
    fn emulated_cycles(&self) -> u64;
    fn emulated_instructions(&self) -> u64;
    fn mode(&self) -> EmulationMode;
    fn set_mode(&mut self, mode: EmulationMode);
    fn reset_start_time(&mut self);
    fn step(&mut self, video_frame_sink: &mut V) -> (u32, bool);
}
