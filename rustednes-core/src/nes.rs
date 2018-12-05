use crate::cartridge::Cartridge;
use crate::cpu;
use crate::cpu::Cpu;
use crate::game_genie::Cheat;
use crate::interconnect;
use crate::interconnect::Interconnect;
use crate::mapper;
use crate::sink::*;

use serde_derive::{Deserialize, Serialize};

use std::cell::RefCell;
use std::rc::Rc;

pub struct Nes {
    pub interconnect: Interconnect,
    pub cpu: Cpu,
}

#[derive(Deserialize, Serialize)]
pub struct State {
    pub interconnect: interconnect::State,
    pub cpu: cpu::State,
}

impl Nes {
    pub fn new(cartridge: Cartridge) -> Nes {
        let mapper = Rc::new(RefCell::new(mapper::create_mapper(cartridge)));
        let cpu = Cpu::new();
        let interconnect = Interconnect::new(mapper);
        let mut nes = Nes { interconnect, cpu };

        nes.reset();

        nes
    }

    pub fn get_state(&self) -> State {
        State {
            interconnect: self.interconnect.get_state(),
            cpu: self.cpu.get_state(),
        }
    }

    pub fn apply_state(&mut self, state: &State) {
        self.interconnect.apply_state(&state.interconnect);
        self.cpu.apply_state(&state.cpu);
    }

    pub fn reset(&mut self) {
        self.interconnect.reset();
        self.cpu.reset(&mut self.interconnect);
    }

    pub fn step(
        &mut self,
        video_frame_sink: &mut VideoSink,
        audio_frame_sink: &mut AudioSink,
    ) -> (u32, bool) {
        let (cpu_cycles, trigger_watchpoint) = self.cpu.step(&mut self.interconnect);

        self.interconnect.cycles(
            &mut self.cpu,
            cpu_cycles,
            video_frame_sink,
            audio_frame_sink,
        );

        (cpu_cycles, trigger_watchpoint)
    }

    pub fn add_cheat(&mut self, cheat: Cheat) {
        self.interconnect.add_cheat(cheat);
    }

    pub fn remove_cheat(&mut self, cheat: Cheat) {
        self.interconnect.remove_cheat(cheat);
    }

    pub fn clear_cheats(&mut self) {
        self.interconnect.clear_cheats();
    }
}
