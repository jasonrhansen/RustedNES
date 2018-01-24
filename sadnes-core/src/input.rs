use memory::Memory;

#[derive(Copy, Clone)]
pub enum Button {
    A,
    B,
    Select,
    Start,
    Up,
    Down,
    Left,
    Right,
}

pub struct GamePad {
    a_pressed: bool,
    b_pressed: bool,
    select_pressed: bool,
    start_pressed: bool,
    up_pressed: bool,
    down_pressed: bool,
    left_pressed: bool,
    right_pressed: bool,

    strobe_state: StrobeState,
}

impl GamePad {
    fn new() -> GamePad {
        GamePad {
            a_pressed: false,
            b_pressed: false,
            select_pressed: false,
            start_pressed: false,
            up_pressed: false,
            down_pressed: false,
            left_pressed: false,
            right_pressed: false,

            strobe_state: StrobeState { button: Button::A }
        }
    }

    pub fn set_button_pressed(&mut self, button: Button, pressed: bool) {
        match button {
            Button::A => self.a_pressed = pressed,
            Button::B => self.b_pressed = pressed,
            Button::Select => self.select_pressed = pressed,
            Button::Start => self.start_pressed = pressed,
            Button::Up => self.up_pressed = pressed,
            Button::Down => self.down_pressed = pressed,
            Button::Left => self.left_pressed = pressed,
            Button::Right => self.right_pressed = pressed,
        }
    }

    pub fn button_pressed(&self, button: Button) -> bool {
        match button {
            Button::A => self.a_pressed,
            Button::B => self.b_pressed,
            Button::Select => self.select_pressed,
            Button::Start => self.start_pressed,
            Button::Up => self.up_pressed,
            Button::Down => self.down_pressed,
            Button::Left => self.left_pressed,
            Button::Right => self.right_pressed,
        }
    }

    fn next_button_state(&mut self) -> bool {
        let state = self.button_pressed(self.strobe_state.button);
        self.strobe_state.next();
        state
    }
}

struct StrobeState {
    button: Button,
}

impl StrobeState {
    fn next(&mut self) {
        self.button = match self.button {
            Button::A => Button::B,
            Button::B => Button::Select,
            Button::Select => Button::Start,
            Button::Start => Button::Up,
            Button::Up => Button::Down,
            Button::Down => Button::Left,
            Button::Left => Button::Right,
            Button::Right => Button::A,
        };
    }

    fn reset(&mut self) {
        self.button = Button::A;
    }
}

pub struct Input {
    pub game_pad_1: GamePad,
    pub game_pad_2: GamePad,
}

impl Input {
    pub fn new() -> Input {
        Input {
            game_pad_1: GamePad::new(),
            game_pad_2: GamePad::new(),
        }
    }

    fn reset_strobe_states(&mut self) {
        self.game_pad_1.strobe_state.reset();
        self.game_pad_2.strobe_state.reset();
    }
}

impl Memory for Input {
    fn read_byte(&mut self, address: u16) -> u8 {
        if address == 0x4016 {
            self.game_pad_1.next_button_state() as u8
        } else if address == 0x4017 {
            self.game_pad_2.next_button_state() as u8
        } else {
            0
        }
    }

    fn write_byte(&mut self, address: u16, _value: u8) {
        if address == 0x4016 {
            self.reset_strobe_states();
        }
    }
}
