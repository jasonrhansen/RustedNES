use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;

use apu;
use apu::Apu;
use cpu::Cpu;
use input;
use input::Input;
use mapper;
use mapper::Mapper;
use memory::{Memory, Ram};
use ppu;
use ppu::{Ppu, OAMDATA_ADDRESS};
use sink::*;
use game_genie::Cheat;

use serde_bytes;

pub const OAMDMA_ADDRESS: u16 = 0x4014;

pub struct Interconnect {
    pub ram: Ram,
    pub ppu: Ppu,
    pub apu: Apu,
    pub input: Input,
    pub mapper: Rc<RefCell<Box<Mapper>>>,
    dma_cycles: u32,

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
    pub dma_cycles: u32,
}

impl Interconnect {
    pub fn new(mapper: Rc<RefCell<Box<Mapper>>>) -> Self {
        Interconnect {
            ram: Ram::new(),
            ppu: Ppu::new(mapper.clone()),
            apu: Apu::new(mapper.clone()),
            input: Input::new(),
            mapper,
            cheats: HashMap::new(),
            dma_cycles: 0,
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
            dma_cycles: self.dma_cycles,
        }
    }

    pub fn apply_state(&mut self, state: &State) {
        self.ram.copy_from_slice(&state.ram);
        self.ppu.apply_state(&state.ppu);
        self.apu.apply_state(&state.apu);
        self.input.apply_state(&state.input);
        let mut mapper = self.mapper.borrow_mut();
        mapper.apply_state(&state.mapper);
        self.dma_cycles = state.dma_cycles;
    }

    fn handle_oam_dma(&mut self, addr_hi: u8) {
        let start = (addr_hi as u16) << 8;
        for i in 0..256 {
            let val = self.read_byte(start + i);
            self.write_byte(OAMDATA_ADDRESS, val);
        }

        // FIXME: An extra cycle should be added on an odd CPU cycle
        // http://wiki.nesdev.com/w/index.php/PPU_registers#OAMDMA
        // Currently we don't know what cycle we're on at this point
        // because the instruction cycles get added after this function
        // gets called.
        self.dma_cycles += 513;
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
        } else if address == OAMDMA_ADDRESS {
            self.handle_oam_dma(value);
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
    pub fn cycles(
        &mut self,
        cpu: &mut Cpu,
        cycles: u32,
        video_frame_sink: &mut VideoSink,
        audio_frame_sink: &mut AudioSink,
    ) {
        let cycles = cycles + self.dma_cycles;
        self.dma_cycles = 0;

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
        self.ram = Ram::new();
        self.ppu.reset();
        self.apu.reset();
        self.input = Input::new();
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
