use crate::apu;
use crate::apu::Apu;
use crate::cpu::Cpu;
use crate::game_genie::Cheat;
use crate::input::{self, Input};
use crate::mapper::{self, Mapper, MapperEnum};
use crate::memory::{Memory, Ram};
use crate::ppu::{self, Ppu};
use crate::sink::*;

use serde_derive::{Deserialize, Serialize};

use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub struct Interconnect {
    pub ram: Ram,
    pub ppu: Ppu,
    pub apu: Apu,
    pub input: Input,
    pub mapper: Rc<RefCell<MapperEnum>>,

    cheats: HashMap<u16, Cheat>,
}

#[derive(Deserialize, Serialize)]
pub struct State {
    #[serde(with = "serde_bytes")]
    pub ram: Vec<u8>,
    pub ppu: ppu::State,
    pub apu: apu::State,
    pub input: input::State,
    pub mapper: mapper::State,
}

impl Interconnect {
    pub fn new(mapper: Rc<RefCell<MapperEnum>>) -> Self {
        Interconnect {
            ram: Ram::new(),
            ppu: Ppu::new(mapper.clone()),
            apu: Apu::new(mapper.clone()),
            input: Input::new(),
            mapper,
            cheats: HashMap::new(),
        }
    }

    pub fn get_state(&self) -> State {
        let mapper = self.mapper.borrow();
        State {
            ram: self.ram.to_vec(),
            ppu: self.ppu.get_state(),
            apu: self.apu.get_state(),
            input: self.input.get_state(),
            mapper: mapper.get_state(),
        }
    }

    pub fn apply_state(&mut self, state: &State) {
        self.ram.copy_from_slice(&state.ram);
        self.ppu.apply_state(&state.ppu);
        self.apu.apply_state(&state.apu);
        self.input.apply_state(&state.input);
        let mut mapper = self.mapper.borrow_mut();
        mapper.apply_state(&state.mapper);
    }
}

impl Memory for Interconnect {
    fn read_byte(&mut self, address: u16) -> u8 {
        let byte = if address < 0x2000 {
            self.ram.read_byte(address)
        } else if address < 0x4000 {
            self.ppu.read_byte(address & 0x2007)
        } else if address < 0x4016 {
            self.apu.read_byte(address)
        } else if address < 0x4018 {
            self.input.read_byte(address)
        } else {
            let mut mapper = self.mapper.borrow_mut();
            mapper.prg_read_byte(address)
        };

        if let Some(cheat) = self.cheats.get(&address) {
            let compare = cheat.compare();
            if compare.is_none() || compare.unwrap() == byte {
                return cheat.data();
            }
        }

        byte
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        if address < 0x2000 {
            self.ram.write_byte(address, value);
        } else if address < 0x4000 {
            self.ppu.write_byte(address, value);
        } else if address < 0x4016 || address == 0x4017 {
            self.apu.write_byte(address, value);
        } else if address < 0x4018 {
            self.input.write_byte(address, value);
        } else {
            let mut mapper = self.mapper.borrow_mut();
            mapper.prg_write_byte(address, value);
        }
    }
}

impl Interconnect {
    pub fn cycles<A: AudioSink, V: VideoSink>(
        &mut self,
        cpu: &mut Cpu,
        cycles: u32,
        video_frame_sink: &mut V,
        audio_frame_sink: &mut A,
    ) {
        for _ in 0..cycles {
            // 3 PPU cycles per CPU cycle
            for _ in 0..3 {
                self.ppu.step(cpu, video_frame_sink);
                let mut mapper = self.mapper.borrow_mut();
                mapper.step(cpu, &self.ppu);
            }

            self.apu.step(cpu, audio_frame_sink);
        }
    }

    pub fn reset(&mut self) {
        self.ram = Ram::default();
        self.ppu.reset();
        self.apu.reset();
        self.input = Input::default();
        let mut mapper = self.mapper.borrow_mut();
        mapper.reset();
    }

    pub fn add_cheat(&mut self, cheat: Cheat) {
        self.cheats.insert(cheat.address(), cheat);
    }

    pub fn remove_cheat(&mut self, cheat: Cheat) {
        self.cheats.remove(&cheat.address());
    }

    pub fn clear_cheats(&mut self) {
        self.cheats.clear();
    }
}
