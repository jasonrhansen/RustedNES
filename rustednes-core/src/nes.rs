use crate::apu::{self, Apu};
use crate::cartridge::Cartridge;
use crate::cpu::Cpu;
use crate::game_genie::Cheat;
use crate::input::{GamePad, Input};
use crate::mapper::{Mapper, MapperEnum};
use crate::memory::{Memory, Ram};
use crate::oam_dma::OamDma;
use crate::ppu::{self, Vram};
use crate::ppu::{OAMDATA_ADDRESS, Ppu};
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
    pub oam_dma: OamDma,
    pub input: Input,
    cheats: HashMap<u16, Cheat>,
    pub cycles: usize,

    // Debugging
    pub trace: bool,
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
            oam_dma: OamDma::new(),
            input: Input::new(),
            cheats: HashMap::new(),
            cycles: 0,
            trace: false,
        };

        nes.reset();

        nes
    }

    // Runs all hardware for a single CPU instruction and returns the number of cycles that
    // were run.
    pub fn step<A: AudioSink, V: VideoSink + Sized>(
        &mut self,
        video_frame_sink: &mut V,
        audio_frame_sink: &mut A,
    ) -> usize {
        let prev_cycles = self.cycles;

        if self.trace {
            let bus = SystemBus::new(
                &mut self.ram,
                &mut self.mapper,
                &mut self.ppu,
                &mut self.apu,
                &mut self.oam_dma,
                &mut self.input,
                &self.cheats,
            );
            println!("{}", self.cpu.trace(&bus));
        }

        // Run cycles until a CPU instruction completes.
        while !self.tick(video_frame_sink, audio_frame_sink) {}
        self.cycles - prev_cycles
    }

    // Run all hardware for the duration of a single CPU cycle. Returns whether a CPU instruction completed in this cycle.
    pub fn tick<A: AudioSink, V: VideoSink + Sized>(
        &mut self,
        video_frame_sink: &mut V,
        audio_frame_sink: &mut A,
    ) -> bool {
        // There is 1 APU cycle per CPU cycle.
        let cpu_stall_cycles = self.apu.tick(&mut self.mapper, audio_frame_sink);
        if cpu_stall_cycles > 0 {
            self.cpu.stall(cpu_stall_cycles);
        }

        // There are 3 PPU cycles per CPU cycle.
        for _ in 0..3 {
            self.ppu.tick(&mut self.mapper, video_frame_sink);
            self.cpu
                .set_nmi_line(self.ppu.nmi_output && self.ppu.nmi_occurred);
        }

        self.update_irq_line();

        let completed_instruction = if self.oam_dma.active {
            self.handle_oam_dma();
            false
        } else {
            let mut bus = SystemBus::new(
                &mut self.ram,
                &mut self.mapper,
                &mut self.ppu,
                &mut self.apu,
                &mut self.oam_dma,
                &mut self.input,
                &self.cheats,
            );
            self.cpu.tick(&mut bus)
        };

        self.update_irq_line();

        self.cycles += 1;

        completed_instruction
    }

    fn update_irq_line(&mut self) {
        // TODO: Check if mapper IRQ is active.
        self.cpu.set_irq_line_low(self.apu.irq_pending());
    }

    fn handle_oam_dma(&mut self) {
        if self.oam_dma.dummy_read {
            // Needs an extra cycle to align on odd cycles.
            if !self.cycles.is_multiple_of(2) {
                self.oam_dma.dummy_read = false;
            }
            return;
        }

        // ON even cycles read from CPU RAM.
        if self.cycles.is_multiple_of(2) {
            let addr = ((self.oam_dma.page as u16) << 8) | (self.oam_dma.count & 0xFF);
            self.oam_dma.data = self.ram.read_byte(addr);
        }
        // ON odd cycles write to PPU OAMDATA.
        else {
            self.ppu
                .write_byte(&mut self.mapper, OAMDATA_ADDRESS, self.oam_dma.data);
            self.oam_dma.count += 1;
            if self.oam_dma.count == 256 {
                self.oam_dma.active = false;
            }
        }
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
            &mut self.oam_dma,
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
            &mut self.oam_dma,
            &mut self.input,
            &self.cheats,
        );

        self.cpu.reset(&mut bus);
        self.ppu.reset();
        self.apu.reset();
    }

    pub fn initialize_nestest(&mut self) {
        self.cpu.initialize_nestest();
        self.ppu.initialize_nestest();
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
