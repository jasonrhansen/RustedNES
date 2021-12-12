use rustednes_core::cartridge::Cartridge;
use rustednes_core::cpu::CPU_FREQUENCY;
use rustednes_core::input::Button;
use rustednes_core::memory::Memory;
use rustednes_core::nes::Nes;
use rustednes_core::ppu::{SCREEN_HEIGHT, SCREEN_WIDTH};
use rustednes_core::sink::*;
use rustednes_core::time_source::TimeSource;

use sdl2::event::{Event, WindowEvent};
use sdl2::keyboard::{KeyboardState, Keycode, Mod, Scancode};
use sdl2::pixels::{Color, PixelFormatEnum};
use sdl2::rect::Rect;
use sdl2::render::{Canvas, Texture};
use sdl2::video::{FullscreenType, Window};
use sdl2::{EventPump, Sdl};

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
    debug_palette_selector: usize,
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

        let debug_scale = 4;
        let debug_window = video_subsystem
            .window("Debug", 256 * debug_scale, 176 * debug_scale)
            .hidden()
            .build()
            .unwrap();
        let mut debug_canvas = debug_window.into_canvas().build().unwrap();
        debug_canvas
            .set_scale(debug_scale as f32, debug_scale as f32)
            .unwrap();
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
            debug_palette_selector: 0,
        }
    }

    pub fn run(&mut self) {
        // Create a texture we can use to render emulation frames.
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

        // Main event/emulation loop
        let mut event_pump = self.sdl_context.event_pump().unwrap();
        loop {
            if !self.handle_events(&mut event_pump) {
                break;
            }

            // Run enough emulator cycles to catch up with the time that has passed since the
            // previous loop iteration.
            let mut video_frame_sink = Xrgb8888VideoSink::new(&mut pixel_buffer);
            let target_time_ns = self.time_source.time_ns() - self.start_time_ns;
            let target_cycles = target_time_ns / CPU_CYCLE_TIME_NS;
            while self.emulated_cycles < target_cycles {
                self.step(&mut video_frame_sink);
                self.emulated_instructions += 1;
            }

            if self.debugging {
                self.render_debug_window();
            }

            if video_frame_sink.frame_written() {
                self.render_frame(pixel_buffer.as_ref(), &mut texture);
                self.update_gamepad(event_pump.keyboard_state());
            }

            thread::sleep(Duration::new(0, 1_000_000_000 / 60));
        }

        self.cleanup();
    }

    fn step<V: VideoSink>(&mut self, video_frame_sink: &mut V) -> (u32, bool) {
        let (cycles, trigger_watchpoint) =
            self.nes.step(video_frame_sink, &mut self.audio_frame_sink);

        self.emulated_cycles += cycles as u64;

        (cycles, trigger_watchpoint)
    }

    /// Returns false to signal to end emulation.
    fn handle_events(&mut self, events: &mut EventPump) -> bool {
        for event in events.poll_iter() {
            match event {
                Event::Quit { .. }
                | Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                } => return false,
                Event::Window {
                    window_id,
                    win_event: WindowEvent::Close,
                    ..
                } => {
                    if window_id == self.canvas.window().id() {
                        return false;
                    } else if window_id == self.debug_canvas.window().id() {
                        self.toggle_debugging();
                    }
                }
                Event::KeyDown {
                    window_id,
                    keycode: Some(Keycode::P),
                    ..
                } => {
                    if window_id == self.debug_canvas.window().id() {
                        self.cycle_debug_palette_selector();
                    }
                }
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
        return true;
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
            self.debug_canvas.window_mut().hide();
        } else {
            self.debug_canvas.window_mut().show();
        }

        self.debugging = !self.debugging;
    }

    /// Render a frame of emulation.
    fn render_frame(&mut self, pixel_buffer: &[u32], texture: &mut Texture) {
        texture
            .update(
                None,
                unsafe { std::mem::transmute(pixel_buffer) },
                SCREEN_WIDTH * 4,
            )
            .unwrap();

        // Maintain aspect ratio when resizing window.
        let (target_width, target_height) = self.canvas.window().drawable_size();
        let target_ratio = target_width as f32 / target_height as f32;
        let dest_rect = if SCREEN_RATIO <= target_ratio {
            let width = (SCREEN_WIDTH as f32 * target_height as f32 / SCREEN_HEIGHT as f32) as u32;
            Rect::new((target_width - width) as i32 / 2, 0, width, target_height)
        } else {
            let height = (SCREEN_HEIGHT as f32 * target_width as f32 / SCREEN_WIDTH as f32) as u32;
            Rect::new(0, (target_height - height) as i32 / 2, target_width, height)
        };

        self.canvas.clear();
        self.canvas.copy(&texture, None, Some(dest_rect)).unwrap();
        self.canvas.present();
    }

    /// Render debug info
    fn render_debug_window(&mut self) {
        self.debug_canvas.set_draw_color(Color::BLACK);
        self.debug_canvas.clear();

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

        let mut mapper = self.nes.interconnect.mapper.borrow_mut();

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
                            self.debug_canvas.set_draw_color(Color::from_u32(
                                &PixelFormatEnum::RGB888.try_into().unwrap(),
                                palette[self.debug_palette_selector * 4 + palette_index],
                            ));
                            self.debug_canvas
                                .draw_point((tile_x + (7 - bit) as i32, point_y))
                                .unwrap();
                        }
                    }
                }
            }
        }

        // Draw palettes
        for (i, &color) in palette.iter().enumerate() {
            self.debug_canvas.set_draw_color(Color::from_u32(
                &PixelFormatEnum::RGB888.try_into().unwrap(),
                color,
            ));

            self.debug_canvas
                .fill_rect(Rect::new(
                    i as i32 % 16 * 16,
                    144 + ((i as i32 / 16) * 16),
                    16,
                    16,
                ))
                .unwrap();
        }

        // Draw rectangle around selected palette
        self.debug_canvas.set_draw_color(Color::WHITE);
        self.debug_canvas
            .draw_rect(Rect::new(
                self.debug_palette_selector as i32 % 4 * 16 * 4,
                144 + ((self.debug_palette_selector as i32 / 4) * 16),
                16 * 4,
                16,
            ))
            .unwrap();

        self.debug_canvas.present();
    }

    fn cycle_debug_palette_selector(&mut self) {
        self.debug_palette_selector = (self.debug_palette_selector + 1) % 8;
    }

    fn cleanup(&mut self) {
        self.set_fullscreen(false);
    }
}
