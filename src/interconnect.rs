use std::cell::RefCell;
use std::rc::Rc;

use apu::Apu;
use cpu::{Cpu, Interrupt};
use input::Input;
use mapper::Mapper;
use memory::{Memory, Ram};
use ppu::{Ppu, OAMDATA_ADDRESS};

const OAMDMA_ADDRESS: u16 = 0x4014;

pub struct Interconnect {
    pub ram: Ram,
    pub ppu: Ppu,
    pub apu: Apu,
    pub input: Input,
    pub mapper: Rc<RefCell<Box<Mapper>>>,
    pub cpu: Rc<RefCell<Cpu>>,
}

impl Interconnect {
    pub fn new(mapper: Rc<RefCell<Box<Mapper>>>, cpu: Rc<RefCell<Cpu>>) -> Self {
        Interconnect {
            ram: Ram::new(),
            ppu: Ppu::new(mapper.clone()),
            apu: Apu{},
            input: Input{},
            mapper,
            cpu,
        }
    }

    fn handle_oam_dma(&mut self, addr_hi: u8) {
        let start = (addr_hi as u16) << 8;
        for i in 0..256 {
            let val = self.read_byte(start + i);
            self.write_byte(OAMDATA_ADDRESS, val);
        }

        let mut cpu = self.cpu.borrow_mut();

        // FIXME: An extra cycle should be added on an odd CPU cycle
        // http://wiki.nesdev.com/w/index.php/PPU_registers#OAMDMA
        // Currently we don't know what cycle we're on at this point
        // because the instruction cycles get added after this function
        // gets called.
        cpu.cycles += 513;
    }
}

impl Memory for Interconnect {
    fn read_byte(&mut self, address: u16) -> u8 {
        if address < 0x2000 {
            self.ram.read_byte(address)
        } else if address < 0x4000 {
            self.ppu.read_byte(address)
        } else if address < 0x4015 {
            self.apu.read_byte(address)
        } else if address < 0x4018 {
            self.input.read_byte(address)
        } else {
            let mut mapper = self.mapper.borrow_mut();
            mapper.prg_read_byte(address)
        }
    }

    fn write_byte(&mut self, address: u16, value: u8) {
        if address < 0x2000 {
            self.ram.write_byte(address, value);
        } else if address < 0x4000 {
            self.ppu.write_byte(address, value);
        } else if address == OAMDMA_ADDRESS {
            self.handle_oam_dma(value);
        } else if address < 0x4015 {
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
    pub fn cycles(&mut self, cycles: u32) -> Option<Interrupt> {
        let interrupt = self.ppu.cycles(cycles);
        self.apu.cycles(cycles);

        interrupt
    }

    pub fn reset(&mut self) {
        self.ppu.reset();
    }
}
