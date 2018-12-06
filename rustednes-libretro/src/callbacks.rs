use libc::*;

use std::ptr;

const EXPERIMENTAL: isize = 0x10000;

#[allow(dead_code)]
pub enum EnvironmentCommand {
    SetRotation = 1,
    GetOverscan = 2,
    GetCanDupe = 3,
    SetMessage = 6,
    Shutdown = 7,
    SetPerformanceLevel = 8,
    GetSystemDirectory = 9,
    SetPixelFormat = 10,
    SetInputDescriptors = 11,
    SetKeyboardCallback = 12,
    SetDiskControlInterface = 13,
    SetHwRender = 14,
    GetVariable = 15,
    SetVariables = 16,
    GetVariableUpdate = 17,
    SetSupportNoGame = 18,
    GetLibretroPath = 19,
    SetAudioCallback = 22,
    SetFrameTimeCallback = 21,
    GetRumbleInterface = 23,
    GetInputDeviceCapabilities = 24,
    GetSensorInterface = (25 | EXPERIMENTAL),
    GetCameraInterface = (26 | EXPERIMENTAL),
    GetLogInterface = 27,
    GetPerfInterface = 28,
    GetLocationInterface = 29,
    GetCoreAssetsDirectory = 30,
    GetSaveDirectory = 31,
    SetSystemAvInfo = 32,
    SetProcAddressCallback = 33,
    SetSubsystemInfo = 34,
    SetControllerInfo = 35,
    SetMemoryMaps = (36 | EXPERIMENTAL),
    SetGeometry = 37,
    GetUsername = 38,
    GetLanguage = 39,
    GetCurrentSoftwareFramebuffer = (40 | EXPERIMENTAL),
    GetHwRenderInterface = (41 | EXPERIMENTAL),
    SetSupportAchievements = (42 | EXPERIMENTAL),
    SetHwRenderContextNegotiationInterface = (43 | EXPERIMENTAL),
    SetSerializationQuirks = 44,
}

#[repr(C)]
pub struct FrameTimeCallback {
    pub callback: extern "C" fn(i64),
    pub reference: i64,
}

#[derive(Clone, Copy)]
pub enum PixelFormat {
    Xrgb1555 = 0,
    Xrgb8888 = 1,
    Rgb565 = 2,
}

pub const MEMORY_ACCESS_WRITE: c_uint = 1;
pub const MEMORY_ACCESS_READ: c_uint = 1 << 1;

pub const MEMORY_TYPE_CACHED: c_uint = 1;

#[repr(C)]
pub struct Framebuffer {
    pub data: *mut c_void,
    pub width: c_uint,
    pub height: c_uint,
    pub pitch: size_t,
    pub format: c_int,
    pub access_flags: c_uint,
    pub memory_flags: c_uint,
}

pub type VideoRefreshCallback = extern "C" fn(*const c_void, u32, u32, size_t);
pub type AudioSampleCallback = extern "C" fn(i16, i16);
pub type AudioSampleBatchCallback = extern "C" fn(*const i16, size_t);
pub type InputPollCallback = extern "C" fn();
pub type InputStateCallback = extern "C" fn(u32, u32, u32, u32) -> i16;
pub type EnvironmentCallback = extern "C" fn(u32, *mut c_void) -> bool;

pub struct Callbacks {
    pub video_refresh: Option<VideoRefreshCallback>,
    pub audio_sample: Option<AudioSampleCallback>,
    pub audio_sample_batch: Option<AudioSampleBatchCallback>,
    pub input_poll: Option<InputPollCallback>,
    pub input_state: Option<InputStateCallback>,
    pub environment: Option<EnvironmentCallback>,
}

impl Callbacks {
    pub fn video_refresh(&self, data: *const c_void, width: u32, height: u32, pitch: size_t) {
        (self.video_refresh.unwrap())(data, width, height, pitch)
    }

    pub fn audio_sample(&self, left: i16, right: i16) {
        (self.audio_sample.unwrap())(left, right);
    }

    pub fn audio_sample_batch(&self, data: *const i16, frames: size_t) {
        (self.audio_sample_batch.unwrap())(data, frames);
    }

    pub fn input_poll(&self) {
        (self.input_poll.unwrap())()
    }

    pub fn input_state(&self, port: u32, device: u32, index: u32, id: u32) -> i16 {
        (self.input_state.unwrap())(port, device, index, id)
    }

    pub fn environment(&self, cmd: u32, data: *mut c_void) -> bool {
        (self.environment.unwrap())(cmd, data)
    }

    pub fn set_pixel_format(&self, mut format: PixelFormat) -> bool {
        self.environment(
            EnvironmentCommand::SetPixelFormat as u32,
            &mut format as *mut _ as *mut _,
        )
    }

    pub fn get_current_software_framebuffer(&self, width: u32, height: u32) -> Option<Framebuffer> {
        let mut fb = Framebuffer {
            data: ptr::null_mut(),
            width: width as _,
            height: height as _,
            pitch: 0,
            format: 0,
            access_flags: MEMORY_ACCESS_WRITE,
            memory_flags: 0,
        };
        if self.environment(
            EnvironmentCommand::GetCurrentSoftwareFramebuffer as _,
            &mut fb as *mut _ as *mut _,
        ) {
            Some(fb)
        } else {
            None
        }
    }

    pub fn set_support_achievements(&self, mut support: bool) -> bool {
        self.environment(
            EnvironmentCommand::SetSupportAchievements as u32,
            &mut support as *mut _ as *mut _,
        )
    }
}
