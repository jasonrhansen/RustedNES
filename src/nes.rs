use cartridge::Cartridge;
use cpu::Cpu;
use memory::Interconnect;

struct Nes {
    interconnect: Interconnect,
    cpu: Cpu,
}

impl Nes {
    fn new(cartridge: Cartridge) -> Nes {
        Nes {
            interconnect: Interconnect::new(cartridge),
            cpu: Cpu::new(),
        }
    }

    fn run(&mut self, cartridge: Cartridge) {
        let mapper = Rc::new(
            RefCell::new(
                mapper::create_mapper(Box::new(rom))
            )
        );

        let interconnect = Interconnect::new(
            Ppu::new(mapper.clone()),
            Apu{},
            Input{}, mapper
        );

        let mut cpu = Cpu::new(interconnect);

        loop {
            let cpu_cycles = cpu.step();

            interconnect.cycles(cpu_cycles);
        }
    }
}