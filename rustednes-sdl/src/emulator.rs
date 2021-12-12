use rustednes_core::cartridge::Cartridge;
use rustednes_core::cpu::CPU_FREQUENCY;
use rustednes_core::input::Button;
use rustednes_core::nes::Nes;
use rustednes_core::ppu::{SCREEN_HEIGHT, SCREEN_WIDTH};
use rustednes_core::sink::*;
use rustednes_core::time_source::TimeSource;

use sdl2::event::Event;
use sdl2::keyboard::{KeyboardState, Keycode, Mod, Scancode};
use sdl2::pixels::{Color, PixelFormatEnum};
use sdl2::rect::Rect;
use sdl2::render::Canvas;
use sdl2::video::{FullscreenType, Window};
use sdl2::Sdl;

use std::thread;
use std::time::Duration;

const CPU_CYCLE_TIME_NS: u64 = (1e9_f64 / CPU_FREQUENCY as f64) as u64 + 1;
const SCREEN_RATIO: f32 = SCREEN_WIDTH as f32 / SCREEN_HEIGHT as f32;

pub struct Emulator<A: AudioSink, T: TimeSource> {
    nes: Nes,

    sdl_context: Sdl,
    canvas: Canvas<Window>,
    audio_frame_sink: A,
    time_source: T,
    start_time_ns: u64,

    emulated_cycles: u64,
    emulated_instructions: u64,

    debugging: bool,
    debug_canvas: Canvas<Window>,
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
    ) -> Emulator<A, T>
    where
        A: AudioSink,
        T: TimeSource,
    {
        let video_subsystem = sdl_context.video().unwrap();

        let debug_window = video_subsystem
            .window("Debug", 256, 128)
            .hidden()
            .build()
            .unwrap();
        let mut debug_canvas = debug_window.into_canvas().build().unwrap();
        debug_canvas.set_draw_color(Color::RGB(0, 0, 0));
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

        canvas.set_draw_color(Color::RGB(0, 0, 0));
        canvas.clear();
        canvas.present();

        Emulator {
            nes: Nes::new(cartridge),

            sdl_context,
            canvas,
            audio_frame_sink,
            time_source,
            start_time_ns: 0,

            emulated_cycles: 0,
            emulated_instructions: 0,

            debugging: false,
            debug_canvas,
        }
    }

    pub fn run(&mut self) {
        let mut pixel_buffer = vec![0; SCREEN_WIDTH * SCREEN_HEIGHT];
        let texture_creator = self.canvas.texture_creator();
        let mut texture = texture_creator
            .create_texture(
                Some(PixelFormatEnum::RGB888),
                sdl2::render::TextureAccess::Target,
                SCREEN_WIDTH as u32,
                SCREEN_HEIGHT as u32,
            )
            .unwrap();

        self.start_time_ns = self.time_source.time_ns();

        let mut event_pump = self.sdl_context.event_pump().unwrap();
        'running: loop {
            for event in event_pump.poll_iter() {
                match event {
                    Event::Quit { .. }
                    | Event::KeyDown {
                        keycode: Some(Keycode::Escape),
                        ..
                    } => break 'running,

                    Event::KeyDown {
                        keycode: Some(Keycode::F11),
                        ..
                    }
                    | Event::KeyDown {
                        keycode: Some(Keycode::F),
                        keymod: Mod::LCTRLMOD | Mod::RCTRLMOD,
                        ..
                    }
                    | Event::KeyDown {
                        keycode: Some(Keycode::Return),
                        keymod: Mod::LALTMOD | Mod::RALTMOD,
                        ..
                    } => {
                        self.toggle_fullscreen();
                    }
                    Event::KeyDown {
                        keycode: Some(Keycode::F5),
                        ..
                    } => {
                        self.toggle_debugging();
                    }
                    Event::Window { .. } => {
                        self.start_time_ns =
                            self.time_source.time_ns() - (self.emulated_cycles * CPU_CYCLE_TIME_NS);
                    }
                    _ => {}
                }
            }

            let mut video_frame_sink = Xrgb8888VideoSink::new(&mut pixel_buffer);

            let target_time_ns = self.time_source.time_ns() - self.start_time_ns;
            let target_cycles = target_time_ns / CPU_CYCLE_TIME_NS;
            while self.emulated_cycles < target_cycles {
                self.step(&mut video_frame_sink);
                self.emulated_instructions += 1;
            }

            if self.debugging {
                self.draw_debug_window();
            }

            if video_frame_sink.frame_written() {
                texture
                    .update(
                        None,
                        unsafe { std::mem::transmute(pixel_buffer.as_slice()) },
                        SCREEN_WIDTH * 4,
                    )
                    .unwrap();

                // Maintain aspect ratio when resizing window.
                let (target_width, target_height) = self.canvas.window().drawable_size();
                let target_ratio = target_width as f32 / target_height as f32;
                let dest_rect = if SCREEN_RATIO <= target_ratio {
                    let width =
                        (SCREEN_WIDTH as f32 * target_height as f32 / SCREEN_HEIGHT as f32) as u32;
                    Rect::new((target_width - width) as i32 / 2, 0, width, target_height)
                } else {
                    let height =
                        (SCREEN_HEIGHT as f32 * target_width as f32 / SCREEN_WIDTH as f32) as u32;
                    Rect::new(0, (target_height - height) as i32 / 2, target_width, height)
                };

                self.canvas.clear();
                self.canvas.copy(&texture, None, Some(dest_rect)).unwrap();
                self.canvas.present();

                self.update_gamepad(event_pump.keyboard_state());
            }

            thread::sleep(Duration::new(0, 1_000_000_000u32 / 60));
        }

        self.cleanup();
    }

    fn draw_debug_window(&mut self) {
        // DCBA98 76543210
        // ---------------
        // 0HRRRR CCCCPTTT
        // |||||| |||||+++- T: Fine Y offset, the row number within a tile
        // |||||| ||||+---- P: Bit plane (0: "lower"; 1: "upper")
        // |||||| ++++----- C: Tile column
        // ||++++---------- R: Tile row
        // |+-------------- H: Half of sprite table (0: "left"; 1: "right")
        // +--------------- 0: Pattern table is at $0000-$1FFF
        self.debug_canvas.clear();
        let mut mapper = self.nes.interconnect.mapper.borrow_mut();
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
                            let pixel = ((lower_byte & (1 << bit)) >> bit)
                                | ((upper_byte & (1 << bit)) >> (bit - 1));
                            let shade = match pixel {
                                3 => 255,
                                2 => 255 / 3 * 2,
                                1 => 255 / 3,
                                _ => 0,
                            };
                            self.debug_canvas
                                .set_draw_color(Color::RGB(shade, shade, shade));
                            self.debug_canvas
                                .draw_point((tile_x + (7 - bit) as i32, point_y))
                                .unwrap();
                        }
                    }
                }
            }
        }
        self.debug_canvas.present();
    }

    fn step<V: VideoSink>(&mut self, video_frame_sink: &mut V) -> (u32, bool) {
        let (cycles, trigger_watchpoint) =
            self.nes.step(video_frame_sink, &mut self.audio_frame_sink);

        self.emulated_cycles += cycles as u64;

        (cycles, trigger_watchpoint)
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

    fn set_fullscreen(&mut self, fullscreen: bool) {
        let state = if fullscreen {
            FullscreenType::Desktop
        } else {
            FullscreenType::Off
        };
        self.canvas
            .window_mut()
            .set_fullscreen(state)
            .unwrap_or_else(|e| {
                eprintln!("Unable to change fullscreen state: {:?}", e);
            });
    }

    fn toggle_fullscreen(&mut self) {
        self.set_fullscreen(matches!(
            self.canvas.window().fullscreen_state(),
            FullscreenType::Off
        ));
    }

    fn toggle_debugging(&mut self) {
        if self.debugging {
            self.debug_canvas.window_mut().show();
        } else {
            self.debug_canvas.window_mut().hide();
        }

        self.debugging = !self.debugging;
    }

    fn cleanup(&mut self) {
        self.set_fullscreen(false);
    }
}
