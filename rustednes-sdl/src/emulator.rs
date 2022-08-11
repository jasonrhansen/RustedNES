use rustednes_common::debugger::{DebugEmulator, Debugger};
use rustednes_common::emulation_mode::EmulationMode;
use rustednes_common::state::StateManager;
use rustednes_common::time::TimeSource;

use rustednes_core::cartridge::Cartridge;
use rustednes_core::cpu::CPU_FREQUENCY;
use rustednes_core::input::Button;
use rustednes_core::mapper::Mapper;
use rustednes_core::memory::Memory;
use rustednes_core::nes::Nes;
use rustednes_core::ppu::{SCREEN_HEIGHT, SCREEN_WIDTH};
use rustednes_core::sink::*;

use sdl2::event::{Event, WindowEvent};
use sdl2::keyboard::{KeyboardState, Keycode, Mod, Scancode};
use sdl2::pixels::{Color, PixelFormat, PixelFormatEnum};
use sdl2::rect::Rect;
use sdl2::render::{Canvas, Texture};
use sdl2::video::{FullscreenType, Window};
use sdl2::{EventPump, Sdl};
use tracing::error;

use std::path::PathBuf;
use std::time::Duration;
use std::{mem, thread};

const CPU_CYCLE_TIME_NS: u64 = (1e9_f64 / CPU_FREQUENCY as f64) as u64 + 1;
const DEBUG_WIDTH: u32 = 256;
const DEBUG_HEIGHT: u32 = 176;
const NUMBER_KEYCODES: &[Keycode] = &[
    Keycode::Num0,
    Keycode::Num1,
    Keycode::Num2,
    Keycode::Num3,
    Keycode::Num4,
    Keycode::Num5,
    Keycode::Num6,
    Keycode::Num7,
    Keycode::Num8,
    Keycode::Num9,
];

pub struct Emulator<A: AudioSink, T: TimeSource> {
    nes: Nes,

    sdl_context: Sdl,

    mode: EmulationMode,
    audio_frame_sink: A,
    time_source: T,
    start_time_ns: u64,

    emulated_cycles: u64,
    emulated_instructions: u64,

    debugging_graphics: bool,
    debug_palette_selector: usize,

    state_manager: StateManager,
}

impl<A, T> Emulator<A, T>
where
    A: AudioSink,
    T: TimeSource,
{
    pub fn new(
        sdl_context: Sdl,
        cartridge: Cartridge,
        audio_frame_sink: A,
        time_source: T,
        rom_path: PathBuf,
    ) -> Emulator<A, T>
    where
        A: AudioSink,
        T: TimeSource,
    {
        Emulator {
            nes: Nes::new(cartridge),

            sdl_context,

            mode: EmulationMode::Running,
            audio_frame_sink,
            time_source,
            start_time_ns: 0,

            emulated_cycles: 0,
            emulated_instructions: 0,

            debugging_graphics: false,
            debug_palette_selector: 0,

            state_manager: StateManager::new(rom_path, NUMBER_KEYCODES.len()),
        }
    }

    pub fn run(&mut self, start_debugger: bool) {
        let video_subsystem = self.sdl_context.video().unwrap();

        let debug_scale = 4;
        let debug_window = video_subsystem
            .window(
                "Debug",
                DEBUG_WIDTH * debug_scale,
                DEBUG_HEIGHT * debug_scale,
            )
            .resizable()
            .hidden()
            .build()
            .unwrap();
        let mut debug_canvas = debug_window.into_canvas().build().unwrap();
        debug_canvas.set_draw_color(Color::BLACK);
        debug_canvas.clear();
        debug_canvas.present();

        let window = video_subsystem
            .window(
                "RustedNES",
                SCREEN_WIDTH as u32 * 2,
                SCREEN_HEIGHT as u32 * 2,
            )
            .position_centered()
            .resizable()
            .maximized()
            .build()
            .unwrap();

        let mut canvas = window.into_canvas().present_vsync().build().unwrap();

        canvas.set_draw_color(Color::BLACK);
        canvas.clear();
        canvas.present();

        let texture_creator = canvas.texture_creator();
        let mut texture = texture_creator
            .create_texture(
                Some(PixelFormatEnum::RGB888),
                sdl2::render::TextureAccess::Target,
                SCREEN_WIDTH as u32,
                SCREEN_HEIGHT as u32,
            )
            .unwrap();

        self.start_time_ns = self.time_source.time_ns();

        let mut debugger = Debugger::new();

        if start_debugger {
            self.mode = EmulationMode::Debugging;
            debugger.start(&mut self.nes);
        }

        // Main event/emulation loop
        let mut event_pump = self.sdl_context.event_pump().unwrap();
        loop {
            if !self.handle_events(
                &mut event_pump,
                &mut debugger,
                &mut canvas,
                &mut debug_canvas,
            ) {
                break;
            }

            let mut frame_written = false;
            let mut quit = false;
            canvas
                .with_texture_canvas(&mut texture, |canvas| {
                    // Run enough emulator cycles to catch up with the time that has passed since the
                    // previous loop iteration.
                    let mut video_frame_sink = CanvasVideoSink::new(canvas);
                    let target_time_ns = self.time_source.time_ns() - self.start_time_ns;
                    let target_cycles = target_time_ns / CPU_CYCLE_TIME_NS;

                    match self.mode {
                        EmulationMode::Running => {
                            let mut start_debugger = false;
                            while self.emulated_cycles < target_cycles && !start_debugger {
                                let (cycles, trigger_watchpoint) = self
                                    .nes
                                    .step(&mut video_frame_sink, &mut self.audio_frame_sink);

                                self.emulated_cycles += cycles as u64;
                                self.emulated_instructions += 1;

                                if trigger_watchpoint || debugger.at_breakpoint(&self.nes) {
                                    start_debugger = true;
                                }
                            }

                            if start_debugger {
                                self.mode = EmulationMode::Debugging;
                                debugger.start(&mut self.nes);
                            }
                        }
                        EmulationMode::Debugging => {
                            if debugger.run_commands(self, &mut video_frame_sink) {
                                quit = true;
                                return;
                            }
                        }
                    }

                    frame_written = video_frame_sink.frame_written();
                })
                .unwrap();

            if quit {
                break;
            }

            if frame_written {
                self.render_frame(&mut canvas, &mut texture);
                if self.mode == EmulationMode::Running {
                    self.update_gamepad(event_pump.keyboard_state());
                }
            }

            if self.debugging_graphics {
                self.render_debug_window(&mut debug_canvas);
            }

            thread::sleep(Duration::new(0, 1_000_000_000 / 60));
        }

        self.cleanup(&mut canvas);
    }

    fn step<V: VideoSink>(&mut self, video_frame_sink: &mut V) -> (u32, bool) {
        let (cycles, trigger_watchpoint) =
            self.nes.step(video_frame_sink, &mut self.audio_frame_sink);

        self.emulated_cycles += cycles as u64;
        self.emulated_instructions += 1;

        (cycles, trigger_watchpoint)
    }

    /// Returns false to signal to end emulation.
    fn handle_events(
        &mut self,
        events: &mut EventPump,
        debugger: &mut Debugger,
        canvas: &mut Canvas<Window>,
        debug_canvas: &mut Canvas<Window>,
    ) -> bool {
        for event in events.poll_iter() {
            match event {
                Event::KeyDown {
                    window_id,
                    keycode: Some(keycode),
                    keymod,
                    ..
                } => {
                    let main_window = canvas.window().id() == window_id;
                    let debug_window = debug_canvas.window().id() == window_id;

                    // We only care about these mods. Ignore all others.
                    let keymod = keymod.intersection(
                        Mod::LSHIFTMOD
                            | Mod::RSHIFTMOD
                            | Mod::LCTRLMOD
                            | Mod::RCTRLMOD
                            | Mod::LALTMOD
                            | Mod::RALTMOD,
                    );

                    match (keycode, keymod) {
                        (Keycode::Escape, Mod::NOMOD) if main_window => {
                            return false;
                        }
                        (Keycode::F11, Mod::NOMOD)
                        | (Keycode::F, Mod::LCTRLMOD | Mod::RCTRLMOD)
                        | (Keycode::Return, Mod::LALTMOD | Mod::RALTMOD)
                            if main_window =>
                        {
                            self.toggle_fullscreen(canvas);
                        }
                        (Keycode::F12, Mod::NOMOD) => {
                            self.mode = EmulationMode::Debugging;
                            debugger.start(&mut self.nes);
                        }
                        (Keycode::F12, Mod::LSHIFTMOD | Mod::RSHIFTMOD) if main_window => {
                            self.toggle_debugging(debug_canvas);
                        }
                        (Keycode::Space, Mod::NOMOD) if debug_window => {
                            self.cycle_debug_palette_selector();
                        }
                        (Keycode::P, Mod::NOMOD) => {
                            let settings = &mut self.nes.interconnect.apu.settings;
                            settings.pulse_1_enabled = !settings.pulse_1_enabled;
                        }
                        (Keycode::LeftBracket, Mod::NOMOD) => {
                            let settings = &mut self.nes.interconnect.apu.settings;
                            settings.pulse_2_enabled = !settings.pulse_2_enabled;
                        }
                        (Keycode::T, Mod::NOMOD) => {
                            let settings = &mut self.nes.interconnect.apu.settings;
                            settings.triangle_enabled = !settings.triangle_enabled;
                        }
                        (Keycode::N, Mod::NOMOD) => {
                            let settings = &mut self.nes.interconnect.apu.settings;
                            settings.noise_enabled = !settings.noise_enabled;
                        }
                        (Keycode::D, Mod::NOMOD) => {
                            let settings = &mut self.nes.interconnect.apu.settings;
                            settings.dmc_enabled = !settings.dmc_enabled;
                        }
                        (Keycode::F, Mod::NOMOD) => {
                            let settings = &mut self.nes.interconnect.apu.settings;
                            settings.filter_enabled = !settings.filter_enabled;
                        }
                        _ => {}
                    }

                    let ctrl_mod = matches!(keymod, Mod::LCTRLMOD | Mod::RCTRLMOD);
                    for (slot, &num_keycode) in NUMBER_KEYCODES.iter().enumerate() {
                        if keycode == num_keycode {
                            if ctrl_mod {
                                self.state_manager.load_state(&mut self.nes, slot);
                            } else {
                                self.state_manager.save_state(&self.nes, slot);
                            }
                        }
                    }
                }
                Event::Window {
                    window_id,
                    win_event,
                    ..
                } => {
                    let main_window = canvas.window().id() == window_id;
                    let debug_window = debug_canvas.window().id() == window_id;

                    match win_event {
                        WindowEvent::Close if main_window => {
                            return false;
                        }
                        WindowEvent::Close if debug_window => {
                            self.toggle_debugging(debug_canvas);
                        }
                        _ => {}
                    }

                    self.start_time_ns =
                        self.time_source.time_ns() - (self.emulated_cycles * CPU_CYCLE_TIME_NS);
                }
                Event::Quit { .. } => return false,
                _ => {}
            }
        }

        true
    }

    fn update_gamepad(&mut self, keyboard_state: KeyboardState) {
        let game_pad_1 = &mut self.nes.interconnect.input.game_pad_1;

        game_pad_1.set_button_pressed(Button::A, keyboard_state.is_scancode_pressed(Scancode::X));
        game_pad_1.set_button_pressed(Button::B, keyboard_state.is_scancode_pressed(Scancode::Z));
        game_pad_1.set_button_pressed(
            Button::Select,
            keyboard_state.is_scancode_pressed(Scancode::Space),
        );
        game_pad_1.set_button_pressed(
            Button::Start,
            keyboard_state.is_scancode_pressed(Scancode::Return),
        );
        game_pad_1.set_button_pressed(Button::Up, keyboard_state.is_scancode_pressed(Scancode::Up));
        game_pad_1.set_button_pressed(
            Button::Down,
            keyboard_state.is_scancode_pressed(Scancode::Down),
        );
        game_pad_1.set_button_pressed(
            Button::Left,
            keyboard_state.is_scancode_pressed(Scancode::Left),
        );
        game_pad_1.set_button_pressed(
            Button::Right,
            keyboard_state.is_scancode_pressed(Scancode::Right),
        );
    }

    fn set_fullscreen(&mut self, canvas: &mut Canvas<Window>, fullscreen: bool) {
        let state = if fullscreen {
            FullscreenType::Desktop
        } else {
            FullscreenType::Off
        };
        canvas
            .window_mut()
            .set_fullscreen(state)
            .unwrap_or_else(|e| {
                error!("Unable to change fullscreen state: {:?}", e);
            });
    }

    fn toggle_fullscreen(&mut self, canvas: &mut Canvas<Window>) {
        self.set_fullscreen(
            canvas,
            matches!(canvas.window().fullscreen_state(), FullscreenType::Off),
        );
    }

    fn toggle_debugging(&mut self, debug_canvas: &mut Canvas<Window>) {
        if self.debugging_graphics {
            debug_canvas.window_mut().hide();
        } else {
            debug_canvas.window_mut().show();
        }

        self.debugging_graphics = !self.debugging_graphics;
    }

    /// Render a frame of emulation.
    fn render_frame(&mut self, canvas: &mut Canvas<Window>, texture: &mut Texture) {
        let (canvas_width, canvas_height) = canvas.window().drawable_size();
        let dest_rect = scale_to_canvas(
            SCREEN_WIDTH as u32,
            SCREEN_HEIGHT as u32,
            canvas_width,
            canvas_height,
        );

        canvas.set_draw_color(Color::BLACK);
        canvas.clear();
        canvas.copy(texture, None, Some(dest_rect)).unwrap();
        canvas.present();
    }

    /// Render debug info
    fn render_debug_window(&mut self, debug_canvas: &mut Canvas<Window>) {
        // Load palette colors
        //
        // $3F00 	Universal background color
        // $3F01-$3F03 	Background palette 0
        // $3F05-$3F07 	Background palette 1
        // $3F09-$3F0B 	Background palette 2
        // $3F0D-$3F0F 	Background palette 3
        // $3F11-$3F13 	Sprite palette 0
        // $3F15-$3F17 	Sprite palette 1
        // $3F19-$3F1B 	Sprite palette 2
        // $3F1D-$3F1F 	Sprite palette 3
        let palette: Vec<u32> = (0x3F00..=0x3F1F)
            .map(|addr| {
                let addr = if addr % 4 == 0 { 0x3F00 } else { addr };
                XRGB8888_PALETTE[(self.nes.interconnect.ppu.mem.read_byte(addr) & 0x3F) as usize]
            })
            .collect();

        let pixel_format: PixelFormat = PixelFormatEnum::RGB888.try_into().unwrap();

        let mut mapper = self.nes.interconnect.mapper.borrow_mut();

        let texture_creator = debug_canvas.texture_creator();
        let mut texture = texture_creator
            .create_texture(
                None,
                sdl2::render::TextureAccess::Target,
                DEBUG_WIDTH,
                DEBUG_HEIGHT,
            )
            .unwrap();
        debug_canvas
            .with_texture_canvas(&mut texture, |canvas| {
                // Draw pattern tables
                //
                // DCBA98 76543210
                // ---------------
                // 0HRRRR CCCCPTTT
                // |||||| |||||+++- T: Fine Y offset, the row number within a tile
                // |||||| ||||+---- P: Bit plane (0: "lower"; 1: "upper")
                // |||||| ++++----- C: Tile column
                // ||++++---------- R: Tile row
                // |+-------------- H: Half of sprite table (0: "left"; 1: "right")
                // +--------------- 0: Pattern table is at $0000-$1FFF
                for half in 0..=1 {
                    for row in 0..16 {
                        for y in 0..8 {
                            let point_y = (row * 8 + y) as i32;
                            for col in 0..16 {
                                let tile_x = (half * 128 + col * 8) as i32;
                                let lower_addr: u16 = half << 12 | row << 8 | col << 4 | y;
                                let upper_addr = lower_addr | 0x08;
                                let lower_byte = mapper.chr_read_byte(lower_addr);
                                let upper_byte = mapper.chr_read_byte(upper_addr);

                                for bit in 0..8 {
                                    let palette_index = (((lower_byte & (1 << bit)) >> bit)
                                        | ((upper_byte & (1 << bit)) >> (bit - 1)))
                                        as usize;
                                    canvas.set_draw_color(Color::from_u32(
                                        &pixel_format,
                                        palette[self.debug_palette_selector * 4 + palette_index],
                                    ));
                                    canvas
                                        .draw_point((tile_x + (7 - bit) as i32, point_y))
                                        .unwrap();
                                }
                            }
                        }
                    }
                }

                // Draw palettes
                for (i, &color) in palette.iter().enumerate() {
                    canvas.set_draw_color(Color::from_u32(&pixel_format, color));

                    canvas
                        .fill_rect(Rect::new(
                            i as i32 % 16 * 16,
                            144 + ((i as i32 / 16) * 16),
                            16,
                            16,
                        ))
                        .unwrap();
                }

                // Draw rectangle around selected palette
                canvas.set_draw_color(Color::WHITE);
                canvas
                    .draw_rect(Rect::new(
                        self.debug_palette_selector as i32 % 4 * 16 * 4,
                        144 + ((self.debug_palette_selector as i32 / 4) * 16),
                        16 * 4,
                        16,
                    ))
                    .unwrap();
            })
            .unwrap();

        let (canvas_width, canvas_height) = debug_canvas.window().drawable_size();
        let dest_rect = scale_to_canvas(DEBUG_WIDTH, DEBUG_HEIGHT, canvas_width, canvas_height);
        debug_canvas.set_draw_color(Color::BLACK);
        debug_canvas.clear();
        debug_canvas.copy(&texture, None, Some(dest_rect)).unwrap();
        debug_canvas.present();
    }

    fn cycle_debug_palette_selector(&mut self) {
        self.debug_palette_selector = (self.debug_palette_selector + 1) % 8;
    }

    fn cleanup(&mut self, canvas: &mut Canvas<Window>) {
        self.set_fullscreen(canvas, false);
        self.state_manager.write_state_to_files();
    }
}

fn scale_to_canvas(src_width: u32, src_height: u32, canvas_width: u32, canvas_height: u32) -> Rect {
    let src_ratio = src_width as f32 / src_height as f32;
    let dst_ratio = canvas_width as f32 / canvas_height as f32;
    if src_ratio <= dst_ratio {
        let width = (src_width as f32 * canvas_height as f32 / src_height as f32) as u32;
        Rect::new((canvas_width - width) as i32 / 2, 0, width, canvas_height)
    } else {
        let height = (src_height as f32 * canvas_width as f32 / src_width as f32) as u32;
        Rect::new(0, (canvas_height - height) as i32 / 2, canvas_width, height)
    }
}

pub struct CanvasVideoSink<'a> {
    canvas: &'a mut Canvas<Window>,
    frame_written: bool,
}

impl<'a> CanvasVideoSink<'a> {
    pub fn new(canvas: &'a mut Canvas<Window>) -> Self {
        CanvasVideoSink {
            canvas,
            frame_written: false,
        }
    }
}

impl<'a> VideoSink for CanvasVideoSink<'a> {
    fn write_frame(&mut self, frame_buffer: &[u8]) {
        let pixel_format = PixelFormatEnum::RGB888.try_into().unwrap();
        for (i, palette_index) in frame_buffer.iter().enumerate() {
            self.canvas.set_draw_color(Color::from_u32(
                &pixel_format,
                XRGB8888_PALETTE[*palette_index as usize],
            ));
            self.canvas
                .draw_point(((i % SCREEN_WIDTH) as i32, (i / SCREEN_WIDTH) as i32))
                .unwrap();
        }
        self.frame_written = true;
    }

    fn frame_written(&self) -> bool {
        self.frame_written
    }

    fn pixel_size(&self) -> usize {
        mem::size_of::<u32>()
    }
}

impl<A, V, T> DebugEmulator<A, V> for Emulator<A, T>
where
    A: AudioSink,
    V: VideoSink,
    T: TimeSource,
{
    fn nes(&mut self) -> &mut Nes {
        &mut self.nes
    }

    fn emulated_cycles(&self) -> u64 {
        self.emulated_cycles
    }

    fn emulated_instructions(&self) -> u64 {
        self.emulated_instructions
    }

    fn audio_frame_sink(&mut self) -> &mut A {
        &mut self.audio_frame_sink
    }

    fn mode(&self) -> EmulationMode {
        self.mode
    }

    fn set_mode(&mut self, mode: EmulationMode) {
        self.mode = mode;
    }

    fn reset_start_time(&mut self) {
        self.start_time_ns =
            self.time_source.time_ns() - (self.emulated_cycles * CPU_CYCLE_TIME_NS);
    }

    fn step(&mut self, video_frame_sink: &mut V) -> (u32, bool) {
        Emulator::step(self, video_frame_sink)
    }
}
