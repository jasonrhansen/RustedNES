// This module is based on the rustual-boy-libretro module (https://github.com/emu-rs/rustual-boy/blob/libretro/rustual-boy-libretro).

extern crate libc;

extern crate sadnes_core;
extern crate serde;
extern crate serde_json;

mod callbacks;
mod game_info;
mod input;
mod retro;
mod system_av_info;
mod system_info;

use libc::*;

use sadnes_core::cartridge::*;
use sadnes_core::input::*;
use sadnes_core::nes::*;
use sadnes_core::ppu::*;
use sadnes_core::serialize;
use sadnes_core::sink::*;

use callbacks::*;
use game_info::*;
use input::*;
use retro::*;
use system_av_info::*;
use system_info::*;

use std::io::Cursor;
use std::slice;
use std::{mem, ptr};

const AUDIO_SAMPLE_RATE: u32 = 44_100;
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
            nes: Nes::new(cartridge, AUDIO_SAMPLE_RATE),
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
}

impl Context {
    fn new() -> Context {
        Context {
            system: None,
            video_output_frame_buffer: OutputBuffer::Xrgb1555(vec![0; DISPLAY_PIXELS]),
            audio_frame_buffer: vec![(0, 0); (AUDIO_SAMPLE_RATE as usize) / 29 * 2], // double space needed for 1 frame for lots of skid room
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
                PixelFormat::Xrgb1555 => OutputBuffer::Xrgb1555(vec![0; DISPLAY_PIXELS as usize]),
                PixelFormat::Rgb565 => OutputBuffer::Rgb565(vec![0; DISPLAY_PIXELS as usize]),
                PixelFormat::Xrgb8888 => OutputBuffer::Xrgb8888(vec![0; DISPLAY_PIXELS as usize]),
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
                fps: 29.97,
                sample_rate: AUDIO_SAMPLE_RATE as f64,
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
                {
                    let game_pad = &mut system.nes.interconnect.input.game_pad_1;

                    game_pad
                        .set_button_pressed(Button::A, CALLBACKS.joypad_button(JoypadButton::A));
                    game_pad
                        .set_button_pressed(Button::B, CALLBACKS.joypad_button(JoypadButton::B));
                    game_pad.set_button_pressed(
                        Button::Start,
                        CALLBACKS.joypad_button(JoypadButton::Start),
                    );
                    game_pad.set_button_pressed(
                        Button::Select,
                        CALLBACKS.joypad_button(JoypadButton::Select),
                    );

                    let joypad_left_pressed = CALLBACKS.joypad_button(JoypadButton::Left);
                    let joypad_right_pressed = CALLBACKS.joypad_button(JoypadButton::Right);
                    let joypad_up_pressed = CALLBACKS.joypad_button(JoypadButton::Up);
                    let joypad_down_pressed = CALLBACKS.joypad_button(JoypadButton::Down);

                    const ANALOG_THRESHOLD: i16 = 0x7fff / 2;

                    let (left_x, left_y) = CALLBACKS.analog_xy(AnalogStick::Left);
                    game_pad.set_button_pressed(
                        Button::Left,
                        left_x < -ANALOG_THRESHOLD || joypad_left_pressed,
                    );
                    game_pad.set_button_pressed(
                        Button::Right,
                        left_x > ANALOG_THRESHOLD || joypad_right_pressed,
                    );
                    game_pad.set_button_pressed(
                        Button::Up,
                        left_y < -ANALOG_THRESHOLD || joypad_up_pressed,
                    );
                    game_pad.set_button_pressed(
                        Button::Down,
                        left_y > ANALOG_THRESHOLD || joypad_down_pressed,
                    );
                }

                let (pixel_buffer_ptr, mut video_output_sink): (_, Box<VideoSink>) = match CALLBACKS
                    .get_current_software_framebuffer(SCREEN_WIDTH as u32, SCREEN_HEIGHT as u32)
                {
                    Some(fb) => match fb.format {
                        0 => (
                            fb.data,
                            Box::new(Xrgb1555VideoSink::new(slice::from_raw_parts_mut(fb.data as *mut _, (fb.height as usize) * (fb.pitch as usize) / mem::size_of::<u16>()))),
                        ),
                        1 => (
                            fb.data,
                            Box::new(Xrgb8888VideoSink::new(slice::from_raw_parts_mut(fb.data as *mut _, (fb.height as usize) * (fb.pitch as usize) / mem::size_of::<u32>()))),
                        ),
                        2 => (
                            fb.data,
                            Box::new(Rgb565VideoSink::new(slice::from_raw_parts_mut(fb.data as *mut _, (fb.height as usize) * (fb.pitch as usize) / mem::size_of::<u16>()))),
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
                            .step(video_output_sink.as_mut(), &mut audio_output_sink);
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

#[no_mangle]
pub extern "C" fn retro_api_version() -> u32 {
    API_VERSION
}

#[no_mangle]
pub unsafe extern "C" fn retro_init() {
    CONTEXT = Box::into_raw(Box::new(Context::new()));
    CALLBACKS.set_support_achievements(true);
}

#[no_mangle]
pub unsafe extern "C" fn retro_deinit() {
    Box::from_raw(CONTEXT); // Take ownership of CONTEXT and drop it
    CONTEXT = ptr::null_mut();
}

// These `retro_set` fn's can be called _before_ retro_init, so they can't touch any context state

#[no_mangle]
pub unsafe extern "C" fn retro_set_video_refresh(callback: VideoRefreshCallback) {
    CALLBACKS.video_refresh = Some(callback);
}

#[no_mangle]
pub unsafe extern "C" fn retro_set_audio_sample(callback: AudioSampleCallback) {
    CALLBACKS.audio_sample = Some(callback);
}

#[no_mangle]
pub unsafe extern "C" fn retro_set_audio_sample_batch(callback: AudioSampleBatchCallback) {
    CALLBACKS.audio_sample_batch = Some(callback);
}

#[no_mangle]
pub unsafe extern "C" fn retro_set_input_poll(callback: InputPollCallback) {
    CALLBACKS.input_poll = Some(callback);
}

#[no_mangle]
pub unsafe extern "C" fn retro_set_input_state(callback: InputStateCallback) {
    CALLBACKS.input_state = Some(callback);
}

#[no_mangle]
pub unsafe extern "C" fn retro_set_environment(callback: EnvironmentCallback) {
    CALLBACKS.environment = Some(callback);
}

#[no_mangle]
pub unsafe extern "C" fn retro_set_controller_port_device(_port: u32, _device: u32) {
    // TODO
}

#[no_mangle]
pub unsafe extern "C" fn retro_get_system_info(info: *mut SystemInfo) {
    // This can be called _before_ retro_init, so this can't be part of the context
    *info = SystemInfo::new();
}

#[no_mangle]
pub unsafe extern "C" fn retro_load_game(game_info: *const GameInfo) -> bool {
    (*CONTEXT).load_game(&*game_info)
}

#[no_mangle]
pub unsafe extern "C" fn retro_load_game_special(
    _game_type: u32,
    _game_infos: *const GameInfo,
    _num_game_infos: size_t,
) -> bool {
    // Neither required nor recommended
    false
}

#[no_mangle]
pub unsafe extern "C" fn retro_get_system_av_info(av_info: *mut SystemAvInfo) {
    *av_info = (*CONTEXT).system_av_info();
}

#[no_mangle]
pub unsafe extern "C" fn retro_unload_game() {
    (*CONTEXT).unload_game();
}

#[no_mangle]
pub unsafe extern "C" fn retro_reset() {
    (*CONTEXT).reset();
}

#[no_mangle]
pub unsafe extern "C" fn retro_run() {
    (*CONTEXT).run_frame();
}

#[no_mangle]
pub unsafe extern "C" fn retro_get_region() -> u32 {
    REGION_NTSC
}

#[no_mangle]
pub unsafe extern "C" fn retro_get_memory_data(id: u32) -> *mut c_void {
    if let Some(ref mut system) = (*CONTEXT).system {
        match id & MEMORY_MASK {
            MEMORY_SYSTEM_RAM => {
                system.nes.interconnect.ram.as_mut_ptr() as *mut _
            }
            MEMORY_VIDEO_RAM => {
                system.nes.interconnect.ppu.mem.vram.as_mut_ptr() as *mut _
            }
            MEMORY_SAVE_RAM => {
                let mut mapper = system.nes.interconnect.mapper.borrow_mut();
                mapper.sram() as *mut _
            }
            _ => ptr::null_mut()
        }
    } else {
        ptr::null_mut()
    }
}

#[no_mangle]
pub unsafe extern "C" fn retro_get_memory_size(id: u32) -> size_t {
    if let Some(ref mut system) = (*CONTEXT).system {
        match id & MEMORY_MASK {
            MEMORY_SYSTEM_RAM => {
                system.nes.interconnect.ram.len()
            }
            MEMORY_VIDEO_RAM => {
                system.nes.interconnect.ppu.mem.vram.len()
            }
            MEMORY_SAVE_RAM => {
                let mut mapper = system.nes.interconnect.mapper.borrow_mut();
                mapper.sram_size()
            }
            _ => 0
        }
    } else {
        0
    }
}

#[no_mangle]
pub unsafe extern "C" fn retro_serialize_size() -> size_t {
    if let Some(ref mut system) = (*CONTEXT).system {
        let state = system.get_nes_state();
        if let Ok(serialized) = serde_json::to_vec(&state) {
            return serialized.len();
        }
    }

    0
}

#[no_mangle]
pub unsafe extern "C" fn retro_serialize(data: *mut c_void, size: size_t) -> bool {
    if let Some(ref mut system) = (*CONTEXT).system {
        let state = system.get_nes_state();
        if let Ok(serialized) = serde_json::to_vec(&state) {
            if serialized.len() <= size {
                ptr::copy_nonoverlapping(serialized.as_ptr(), data as *mut u8, serialized.len());

                return true;
            } else {
                println!("Couldn't serialize. Size of serialize buffer if smaller than the serialized data");
            }
        }
    }

    false
}

#[no_mangle]
pub unsafe extern "C" fn retro_unserialize(data: *const c_void, size: size_t) -> bool {
    if let Some(ref mut system) = (*CONTEXT).system {
        if let Ok(state) = serde_json::from_slice(slice::from_raw_parts(data as _, size)) {
            system.apply_nes_state(state);
            return true;
        }
    }

    false
}

#[no_mangle]
pub unsafe extern "C" fn retro_cheat_reset() {
    unimplemented!("retro_cheat_reset");
}

#[no_mangle]
pub unsafe extern "C" fn retro_cheat_set(_index: u32, _enabled: bool, _code: *const c_char) {
    unimplemented!("retro_cheat_set");
}
