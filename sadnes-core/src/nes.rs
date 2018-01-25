use std::cell::RefCell;
use std::rc::Rc;

use interconnect::Interconnect;
use mapper;
use cartridge::Cartridge;
use cpu::Cpu;
use sink::*;

pub struct Nes {
    pub interconnect: Interconnect,
    pub cpu: Cpu,
}

impl Nes {
    pub fn new(cartridge: Cartridge) -> Nes {
        let mapper = Rc::new(
            RefCell::new(
                mapper::create_mapper(Box::new(cartridge))
            )
        );

        let mut cpu = Cpu::new();

        let interconnect = Interconnect::new(mapper, &mut cpu as *mut Cpu);

        let mut nes = Nes {
            interconnect,
            cpu,
        };

        nes.reset();

        nes
    }

    pub fn reset(&mut self) {
        self.interconnect.reset();
        self.cpu.reset(&mut self.interconnect);
    }

    pub fn step(&mut self,
                video_frame_sink: &mut Sink<VideoFrame>,
                audio_frame_sink: &mut Sink<AudioFrame>) -> (u32, bool) {
        let (cpu_cycles, trigger_watchpoint) =
            self.cpu.step(&mut self.interconnect);

        self.interconnect.cycles(&mut self.cpu, cpu_cycles, video_frame_sink, audio_frame_sink);

        (cpu_cycles, trigger_watchpoint)
    }
}