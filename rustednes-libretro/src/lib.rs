#![allow(dead_code)]
#![allow(clippy::missing_safety_doc)]
#![allow(static_mut_refs)]

// This module is based on the rustual-boy-libretro module (https://github.com/emu-rs/rustual-boy/blob/libretro/rustual-boy-libretro).

extern crate libc;

extern crate rustednes_core;
extern crate serde;
extern crate serde_cbor;

mod callbacks;
mod game_info;
mod input;
mod retro;
mod system_av_info;
mod system_info;

use libc::*;

use rustednes_core::apu::SAMPLE_RATE;
use rustednes_core::cartridge::*;
use rustednes_core::game_genie::Cheat;
use rustednes_core::input::*;
use rustednes_core::mapper::Mapper;
use rustednes_core::nes::*;
use rustednes_core::ppu::*;
use rustednes_core::serialize;
use rustednes_core::sink::*;

use callbacks::*;
use game_info::*;
use input::*;
use retro::*;
use system_av_info::*;
use system_info::*;

use std::ffi::CStr;
use std::io::Cursor;
use std::slice;
use std::{mem, ptr};

const DISPLAY_PIXELS: usize = SCREEN_WIDTH * SCREEN_HEIGHT;

pub enum OutputBuffer {
    Xrgb1555(Vec<u16>),
    Rgb565(Vec<u16>),
    Xrgb8888(Vec<u32>),
}

struct System {
    nes: Nes,
}

impl System {
    fn new(cartridge: Cartridge) -> System {
        System {
            nes: Nes::new(cartridge),
        }
    }

    fn reset(&mut self) {
        self.nes.reset();
    }

    fn get_nes_state(&mut self) -> serialize::VersionedState {
        serialize::get_state(&self.nes)
    }

    fn apply_nes_state(&mut self, state: serialize::VersionedState) {
        serialize::apply_state(&mut self.nes, state)
    }
}

pub struct Context {
    system: Option<System>,
    video_output_frame_buffer: OutputBuffer,
    audio_frame_buffer: Vec<(i16, i16)>,
    serialized: Option<Vec<u8>>,
}

impl Context {
    fn new() -> Context {
        Context {
            system: None,
            video_output_frame_buffer: OutputBuffer::Xrgb1555(vec![0; DISPLAY_PIXELS]),
            audio_frame_buffer: vec![(0, 0); (SAMPLE_RATE as usize) / 60 * 2], // double space needed for 1 frame for lots of skid room
            serialized: None,
        }
    }

    fn load_game(&mut self, game_info: &GameInfo) -> bool {
        unsafe {
            // It seems retroarch (and possibly other frontends) is a bit finicky with accepting the set pixel format
            //  callback, so we should call it a few times before giving up. We don't want to loop forever here though,
            //  as it's possible that a given frontend just doesn't support changing the pixel format. In that case, we
            //  will want to fall back to using the default pixel format.
            let desired_pixel_format = PixelFormat::Rgb565; // Recommended libretro pixel format
            let mut actual_pixel_format = PixelFormat::Xrgb1555; // Default frontend pixel format
            for _ in 0..10 {
                if CALLBACKS.set_pixel_format(desired_pixel_format) {
                    actual_pixel_format = desired_pixel_format;
                    break;
                }
            }

            self.video_output_frame_buffer = match actual_pixel_format {
                PixelFormat::Xrgb1555 => OutputBuffer::Xrgb1555(vec![0; DISPLAY_PIXELS]),
                PixelFormat::Rgb565 => OutputBuffer::Rgb565(vec![0; DISPLAY_PIXELS]),
                PixelFormat::Xrgb8888 => OutputBuffer::Xrgb8888(vec![0; DISPLAY_PIXELS]),
            };

            match Cartridge::load(&mut Cursor::new(game_info.data_ref())) {
                Ok(cartridge) => {
                    self.system = Some(System::new(cartridge));
                    true
                }
                Err(_) => false,
            }
        }
    }

    fn unload_game(&mut self) {
        self.system = None;
    }

    fn system_av_info(&self) -> SystemAvInfo {
        SystemAvInfo {
            geometry: SystemGameGeometry {
                base_width: SCREEN_WIDTH as u32,
                base_height: SCREEN_HEIGHT as u32,
                max_width: SCREEN_WIDTH as u32,
                max_height: SCREEN_HEIGHT as u32,

                // Optional
                aspect_ratio: 0.0,
            },
            timing: SystemTiming {
                fps: 60.1,
                sample_rate: f64::from(SAMPLE_RATE),
            },
        }
    }

    fn reset(&mut self) {
        if let Some(ref mut system) = self.system {
            system.reset();
        }
    }

    fn run_frame(&mut self) {
        unsafe {
            CALLBACKS.input_poll();

            if let Some(ref mut system) = self.system {
                Context::handle_input(system);

                let (pixel_buffer_ptr, mut video_output_sink): (_, Box<dyn VideoSink>) =
                    match CALLBACKS
                        .get_current_software_framebuffer(SCREEN_WIDTH as u32, SCREEN_HEIGHT as u32)
                    {
                        Some(fb) => match fb.format {
                            0 => (
                                fb.data,
                                Box::new(Xrgb1555VideoSink::new(slice::from_raw_parts_mut(
                                    fb.data as *mut _,
                                    (fb.height as usize) * fb.pitch / mem::size_of::<u16>(),
                                ))),
                            ),
                            1 => (
                                fb.data,
                                Box::new(Xrgb8888VideoSink::new(slice::from_raw_parts_mut(
                                    fb.data as *mut _,
                                    (fb.height as usize) * fb.pitch / mem::size_of::<u32>(),
                                ))),
                            ),
                            2 => (
                                fb.data,
                                Box::new(Rgb565VideoSink::new(slice::from_raw_parts_mut(
                                    fb.data as *mut _,
                                    (fb.height as usize) * fb.pitch / mem::size_of::<u16>(),
                                ))),
                            ),
                            _ => panic!(
                                "Host returned framebuffer with unrecognized pixel format format"
                            ),
                        },
                        _ => match self.video_output_frame_buffer {
                            OutputBuffer::Xrgb1555(ref mut buffer) => (
                                buffer.as_mut_ptr() as *mut c_void,
                                Box::new(Xrgb1555VideoSink::new(buffer)),
                            ),
                            OutputBuffer::Xrgb8888(ref mut buffer) => (
                                buffer.as_mut_ptr() as *mut c_void,
                                Box::new(Xrgb8888VideoSink::new(buffer)),
                            ),
                            OutputBuffer::Rgb565(ref mut buffer) => (
                                buffer.as_mut_ptr() as *mut c_void,
                                Box::new(Rgb565VideoSink::new(buffer)),
                            ),
                        },
                    };

                let rendered_audio_frames = {
                    let mut audio_output_sink = AudioSinkI16::new(&mut self.audio_frame_buffer);

                    while !video_output_sink.frame_written() {
                        system
                            .nes
                            .step(&mut video_output_sink, &mut audio_output_sink);
                    }

                    audio_output_sink.samples_written()
                };

                (CALLBACKS.video_refresh.unwrap())(
                    pixel_buffer_ptr,
                    SCREEN_WIDTH as u32,
                    SCREEN_HEIGHT as u32,
                    video_output_sink.pixel_size() * SCREEN_WIDTH,
                );
                (CALLBACKS.audio_sample_batch.unwrap())(
                    self.audio_frame_buffer.as_mut_ptr() as *mut _,
                    rendered_audio_frames as _,
                );
            }
        }
    }

    unsafe fn handle_input(system: &mut System) {
        unsafe {
            {
                let game_pad = &mut system.nes.interconnect.input.game_pad_1;
                Context::handle_game_pad(game_pad, 0);
            }
            {
                let game_pad = &mut system.nes.interconnect.input.game_pad_2;
                Context::handle_game_pad(game_pad, 1);
            }
        }
    }

    unsafe fn handle_game_pad(game_pad: &mut GamePad, index: u32) {
        unsafe {
            game_pad.set_button_pressed(Button::A, CALLBACKS.joypad_button(JoypadButton::A, index));
            game_pad.set_button_pressed(Button::B, CALLBACKS.joypad_button(JoypadButton::B, index));
            game_pad.set_button_pressed(
                Button::Start,
                CALLBACKS.joypad_button(JoypadButton::Start, index),
            );
            game_pad.set_button_pressed(
                Button::Select,
                CALLBACKS.joypad_button(JoypadButton::Select, index),
            );

            let joypad_left_pressed = CALLBACKS.joypad_button(JoypadButton::Left, index);
            let joypad_right_pressed = CALLBACKS.joypad_button(JoypadButton::Right, index);
            let joypad_up_pressed = CALLBACKS.joypad_button(JoypadButton::Up, index);
            let joypad_down_pressed = CALLBACKS.joypad_button(JoypadButton::Down, index);

            const ANALOG_THRESHOLD: i16 = 0x7fff / 2;

            let (left_x, left_y) = CALLBACKS.analog_xy(AnalogStick::Left, index);
            game_pad.set_button_pressed(
                Button::Left,
                left_x < -ANALOG_THRESHOLD || joypad_left_pressed,
            );
            game_pad.set_button_pressed(
                Button::Right,
                left_x > ANALOG_THRESHOLD || joypad_right_pressed,
            );
            game_pad
                .set_button_pressed(Button::Up, left_y < -ANALOG_THRESHOLD || joypad_up_pressed);
            game_pad.set_button_pressed(
                Button::Down,
                left_y > ANALOG_THRESHOLD || joypad_down_pressed,
            );
        }
    }
}

static mut CALLBACKS: Callbacks = Callbacks {
    video_refresh: None,
    audio_sample: None,
    audio_sample_batch: None,
    input_poll: None,
    input_state: None,
    environment: None,
};

static mut CONTEXT: *mut Context = 0 as *mut _;

#[unsafe(no_mangle)]
pub extern "C" fn retro_api_version() -> u32 {
    API_VERSION
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn retro_init() {
    unsafe {
        CONTEXT = Box::into_raw(Box::new(Context::new()));
        CALLBACKS.set_support_achievements(true);
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn retro_deinit() {
    unsafe {
        drop(Box::from_raw(CONTEXT)); // Take ownership of CONTEXT and drop it
        CONTEXT = ptr::null_mut();
    }
}

// These `retro_set` fn's can be called _before_ retro_init, so they can't touch any context state

#[unsafe(no_mangle)]
pub unsafe extern "C" fn retro_set_video_refresh(callback: VideoRefreshCallback) {
    unsafe {
        CALLBACKS.video_refresh = Some(callback);
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn retro_set_audio_sample(callback: AudioSampleCallback) {
    unsafe {
        CALLBACKS.audio_sample = Some(callback);
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn retro_set_audio_sample_batch(callback: AudioSampleBatchCallback) {
    unsafe {
        CALLBACKS.audio_sample_batch = Some(callback);
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn retro_set_input_poll(callback: InputPollCallback) {
    unsafe {
        CALLBACKS.input_poll = Some(callback);
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn retro_set_input_state(callback: InputStateCallback) {
    unsafe {
        CALLBACKS.input_state = Some(callback);
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn retro_set_environment(callback: EnvironmentCallback) {
    unsafe {
        CALLBACKS.environment = Some(callback);
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn retro_set_controller_port_device(_port: u32, _device: u32) {
    // TODO
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn retro_get_system_info(info: *mut SystemInfo) {
    unsafe {
        // This can be called _before_ retro_init, so this can't be part of the context
        *info = SystemInfo::new();
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn retro_load_game(game_info: *const GameInfo) -> bool {
    unsafe { (*CONTEXT).load_game(&*game_info) }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn retro_load_game_special(
    _game_type: u32,
    _game_infos: *const GameInfo,
    _num_game_infos: size_t,
) -> bool {
    // Neither required nor recommended
    false
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn retro_get_system_av_info(av_info: *mut SystemAvInfo) {
    unsafe {
        *av_info = (*CONTEXT).system_av_info();
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn retro_unload_game() {
    unsafe {
        (*CONTEXT).unload_game();
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn retro_reset() {
    unsafe {
        (*CONTEXT).reset();
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn retro_run() {
    unsafe {
        (*CONTEXT).run_frame();
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn retro_get_region() -> u32 {
    REGION_NTSC
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn retro_get_memory_data(id: u32) -> *mut c_void {
    unsafe {
        match (*CONTEXT).system {
            Some(ref mut system) => match id & MEMORY_MASK {
                MEMORY_SYSTEM_RAM => system.nes.interconnect.ram.as_mut_ptr() as *mut _,
                MEMORY_VIDEO_RAM => system.nes.interconnect.ppu.mem.vram.as_mut_ptr() as *mut _,
                MEMORY_SAVE_RAM => {
                    let mut mapper = system.nes.interconnect.mapper.borrow_mut();
                    mapper.sram() as *mut _
                }
                _ => ptr::null_mut(),
            },
            _ => ptr::null_mut(),
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn retro_get_memory_size(id: u32) -> size_t {
    unsafe {
        match (*CONTEXT).system {
            Some(ref mut system) => match id & MEMORY_MASK {
                MEMORY_SYSTEM_RAM => system.nes.interconnect.ram.len(),
                MEMORY_VIDEO_RAM => system.nes.interconnect.ppu.mem.vram.len(),
                MEMORY_SAVE_RAM => {
                    let mapper = system.nes.interconnect.mapper.borrow_mut();
                    mapper.sram_size()
                }
                _ => 0,
            },
            _ => 0,
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn retro_serialize_size() -> size_t {
    unsafe {
        if let Some(ref mut system) = (*CONTEXT).system {
            let state = system.get_nes_state();

            // Serialize to get size and cache result to use in retro_serialize
            (*CONTEXT).serialized = match (*CONTEXT).serialized {
                Some(_) => serde_cbor::ser::to_vec_packed(&state).ok(),
                // The first time we get the size return non packed version to make sure we have enough room for all future save states
                None => serde_cbor::ser::to_vec(&state).ok(),
            };

            if let Some(ref s) = (*CONTEXT).serialized {
                return s.len();
            }
        }

        0
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn retro_serialize(data: *mut c_void, size: size_t) -> bool {
    unsafe {
        // Use cached serialization that was created in retro_serialize_size
        if let Some(ref serialized) = (*CONTEXT).serialized {
            if serialized.len() <= size {
                ptr::copy_nonoverlapping(serialized.as_ptr(), data as *mut u8, serialized.len());

                return true;
            } else {
                println!(
                    "Couldn't serialize. Size of serialize buffer is smaller than the serialized data"
                );
            }
        }

        false
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn retro_unserialize(data: *const c_void, size: size_t) -> bool {
    unsafe {
        if let Some(ref mut system) = (*CONTEXT).system {
            match serde_cbor::from_slice(slice::from_raw_parts(data as _, size)) {
                Ok(state) => {
                    system.apply_nes_state(state);
                    return true;
                }
                Err(e) => {
                    println!("Unable to deserialize data. {:?}", e);
                }
            }
        }

        false
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn retro_cheat_reset() {
    unsafe {
        if let Some(ref mut system) = (*CONTEXT).system {
            system.nes.clear_cheats();
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn retro_cheat_set(_index: u32, enabled: bool, code: *const c_char) {
    unsafe {
        if let Some(ref mut system) = (*CONTEXT).system {
            let code = CStr::from_ptr(code).to_bytes();

            if let Ok(cheat) = Cheat::from_code(code) {
                if enabled {
                    system.nes.add_cheat(cheat);
                } else {
                    system.nes.remove_cheat(cheat);
                }
            }
        }
    }
}
