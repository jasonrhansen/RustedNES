use crate::apu::{self, Apu};
use crate::cartridge::Cartridge;
use crate::cpu::Cpu;
use crate::game_genie::Cheat;
use crate::input::{GamePad, Input};
use crate::mapper::{Mapper, MapperEnum};
use crate::memory::Ram;
use crate::ppu::Ppu;
use crate::ppu::{self, Vram};
use crate::sink::*;
use crate::system_bus::SystemBus;
use crate::{cpu, input, mapper};

use serde_derive::{Deserialize, Serialize};

use std::collections::HashMap;

pub struct Nes {
    ram: Ram,
    pub mapper: MapperEnum,
    pub cpu: Cpu,
    pub ppu: Ppu,
    pub apu: Apu,
    pub input: Input,
    cheats: HashMap<u16, Cheat>,
    cycles: usize,
}

#[derive(Deserialize, Serialize)]
pub struct State {
    #[serde(with = "serde_bytes")]
    pub ram: Vec<u8>,
    pub mapper: mapper::State,
    pub cpu: cpu::State,
    pub ppu: ppu::State,
    pub apu: apu::State,
    pub input: input::State,
    pub cycles: usize,
}

impl Nes {
    pub fn new(cartridge: Cartridge) -> Nes {
        let mut nes = Self {
            ram: Ram::new(),
            mapper: MapperEnum::from_cartridge(cartridge),
            cpu: Cpu::new(),
            ppu: Ppu::new(),
            apu: Apu::new(),
            input: Input::new(),
            cheats: HashMap::new(),
            cycles: 0,
        };

        nes.reset();

        nes
    }

    pub fn step<A: AudioSink, V: VideoSink + Sized>(
        &mut self,
        video_frame_sink: &mut V,
        audio_frame_sink: &mut A,
    ) -> (u32, bool) {
        let mut bus = SystemBus::new(
            &mut self.ram,
            &mut self.mapper,
            &mut self.ppu,
            &mut self.apu,
            &mut self.input,
            &self.cheats,
        );

        let (cycles, trigger_watchpoint) = self.cpu.step(&mut bus);

        for _ in 0..cycles {
            // There are 3 PPU cycles per CPU cycle.
            for _ in 0..3 {
                let request_nmi = self.ppu.tick(&mut self.mapper, video_frame_sink);
                if request_nmi {
                    self.cpu.request_nmi();
                }
            }

            // There is 1 APU cycle per CPU cycle.
            let (request_irq, cpu_stall_cycles) = self.apu.tick(&mut self.mapper, audio_frame_sink);
            if request_irq {
                self.cpu.request_irq();
            } else {
                self.cpu.reset_irq();
            }
            if cpu_stall_cycles > 0 {
                self.cpu.stall(cpu_stall_cycles);
            }
        }

        self.cycles += cycles as usize;

        (cycles, trigger_watchpoint)
    }

    pub fn get_state(&self) -> State {
        State {
            ram: self.ram.to_vec(),
            mapper: self.mapper.get_state(),
            cpu: self.cpu.get_state(),
            ppu: self.ppu.get_state(),
            apu: self.apu.get_state(),
            input: self.input.get_state(),
            cycles: self.cycles,
        }
    }

    pub fn apply_state(&mut self, state: &State) {
        self.ram.copy_from_slice(&state.ram);
        self.mapper.apply_state(&state.mapper);
        self.cpu.apply_state(&state.cpu);
        self.ppu.apply_state(&state.ppu);
        self.apu.apply_state(&state.apu);
        self.input.apply_state(&state.input);
        self.cycles = state.cycles;
    }

    pub fn system_bus(&'_ mut self) -> SystemBus<'_> {
        SystemBus::new(
            &mut self.ram,
            &mut self.mapper,
            &mut self.ppu,
            &mut self.apu,
            &mut self.input,
            &self.cheats,
        )
    }

    pub fn reset(&mut self) {
        let mut bus = SystemBus::new(
            &mut self.ram,
            &mut self.mapper,
            &mut self.ppu,
            &mut self.apu,
            &mut self.input,
            &self.cheats,
        );

        self.cpu.reset(&mut bus);
        self.ppu.reset();
        self.apu.reset();
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

    pub fn system_ram(&mut self) -> &mut Ram {
        &mut self.ram
    }

    pub fn video_ram(&mut self) -> &mut Vram {
        &mut self.ppu.mem.vram
    }

    pub fn save_ram(&mut self) -> *mut u8 {
        self.mapper.sram()
    }

    pub fn save_ram_size(&self) -> usize {
        self.mapper.sram_size()
    }

    pub fn game_pad_1(&mut self) -> &mut GamePad {
        &mut self.input.game_pad_1
    }

    pub fn game_pad_2(&mut self) -> &mut GamePad {
        &mut self.input.game_pad_1
    }
}
