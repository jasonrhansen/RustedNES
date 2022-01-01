use std::{
    fs::OpenOptions,
    io::{BufReader, BufWriter, Read, Write},
    path::PathBuf,
};

use rustednes_core::{nes::Nes, serialize};

pub struct StateManager {
    rom_path: PathBuf,
    slots: Vec<Option<String>>,
}

impl StateManager {
    pub fn new(rom_path: PathBuf, num_slots: usize) -> Self {
        assert!(num_slots > 0);

        Self {
            rom_path,
            slots: vec![None; num_slots],
        }
    }

    pub fn save_state(&mut self, nes: &Nes, slot: usize) {
        if slot >= self.slots.len() {
            eprintln!(
                "couldn't save state to slot {}, the maximum is: {}",
                slot,
                self.slots.len() - 1
            );
            return;
        }

        println!("Saving state to slot: {}", slot);
        self.slots[slot] = serde_json::to_string(&serialize::get_state(nes)).ok();
    }

    pub fn load_state(&mut self, nes: &mut Nes, slot: usize) {
        if slot >= self.slots.len() {
            eprintln!(
                "couldn't load state to slot {}, the maximum is: {}",
                slot,
                self.slots.len() - 1
            );
            return;
        }

        if self.slots[slot].is_none() {
            self.load_state_from_file(slot);
        }
        if let Some(ref s) = self.slots[slot] {
            println!("Loading state from slot: {}", slot);
            match serde_json::from_str(s) {
                Ok(state) => {
                    serialize::apply_state(nes, state);
                }
                Err(e) => {
                    eprintln!("error applying save state: {}", e);
                }
            }
        }
    }

    pub fn write_state_to_files(&mut self) {
        (0..self.slots.len()).for_each(|slot| self.write_state_to_file(slot));
    }

    fn write_state_to_file(&mut self, slot: usize) {
        assert!(slot < self.slots.len());

        if let Some(ref s) = self.slots[slot] {
            let path = self.save_state_file_path(slot);
            let save_file = OpenOptions::new()
                .write(true)
                .truncate(true)
                .create(true)
                .open(&path);
            match save_file {
                Ok(save_file) => {
                    println!("Writing save state file: {}", path.display());
                    let mut save_writer = BufWriter::new(save_file);
                    let _ = save_writer.write_all(s.as_bytes());
                }
                Err(e) => {
                    eprintln!("unable to open file to save state: {}", e);
                }
            }
        }
    }

    fn load_state_from_file(&mut self, slot: usize) {
        assert!(slot < self.slots.len());

        let path = self.save_state_file_path(slot);
        let save_file = OpenOptions::new()
            .read(true)
            .write(false)
            .create(false)
            .open(&path);
        if let Ok(save_file) = save_file {
            println!("Loading save state file: {}", path.display());
            let mut save_reader = BufReader::new(save_file);
            let mut serialized = String::new();
            let _ = save_reader.read_to_string(&mut serialized);
            self.slots[slot] = Some(serialized);
        }
    }

    fn save_state_file_path(&self, slot: usize) -> PathBuf {
        self.rom_path.with_extension(format!("sav{}", slot))
    }
}
