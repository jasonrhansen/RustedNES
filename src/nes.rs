use std::cell::RefCell;
use std::rc::Rc;

use interconnect::Interconnect;
use mapper;
use cartridge::Cartridge;
use cpu::Cpu;

pub struct Nes {
    pub interconnect: Interconnect,
    pub cpu: Rc<RefCell<Box<Cpu>>>,
}

impl Nes {
    pub fn new(cartridge: Cartridge) -> Nes {
        let mapper = Rc::new(
            RefCell::new(
                mapper::create_mapper(Box::new(cartridge))
            )
        );

        let cpu = Rc::new(
            RefCell::new(Box::new(Cpu::new()))
        );

        let interconnect = Interconnect::new(mapper, cpu.clone());

        let mut nes = Nes {
            interconnect,
            cpu: cpu,
        };

        nes.reset();

        nes
    }

    fn reset(&mut self) {
        self.interconnect.reset();
        let mut cpu = self.cpu.borrow_mut();
        cpu.reset(&mut self.interconnect);
    }

    fn step(&mut self) -> u32 {
        let mut cpu = self.cpu.borrow_mut();
        let cpu_cycles = cpu.step(&mut self.interconnect);

        if let Some(interrupt) = self.interconnect.cycles(cpu_cycles) {
            cpu.request_interrupt(interrupt);
        }

        cpu_cycles
    }
}