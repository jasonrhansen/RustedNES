use std::cell::RefCell;
use std::rc::Rc;

use cartridge::Cartridge;
use cpu;
use cpu::Cpu;
use interconnect;
use interconnect::Interconnect;
use mapper;
use sink::*;
use game_genie::Cheat;

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

        let mut cpu = Cpu::new();

        let interconnect = Interconnect::new(mapper, &mut cpu as *mut Cpu);

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
        self.interconnect.cheats.insert(cheat.address(), cheat);
    }

    pub fn remove_cheat(&mut self, cheat: Cheat) {
        self.interconnect.cheats.remove(&cheat.address());
    }

    pub fn clear_cheats(&mut self) {
        self.interconnect.cheats.clear();
    }
}
