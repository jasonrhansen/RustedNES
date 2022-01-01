use std::{
    fs::OpenOptions,
    io::{BufReader, BufWriter, Read, Write},
    path::PathBuf,
};

use rustednes_core::{nes::Nes, serialize};

pub struct StateManager {
    rom_path: PathBuf,
    serialized: Option<String>,
}

impl StateManager {
    pub fn new(rom_path: PathBuf) -> Self {
        Self {
            rom_path,
            serialized: None,
        }
    }

    pub fn save_state(&mut self, nes: &Nes) {
        self.serialized = serde_json::to_string(&serialize::get_state(nes)).ok();
    }

    pub fn load_state(&mut self, nes: &mut Nes) {
        if self.serialized.is_none() {
            self.load_state_from_file();
        }
        if let Some(ref s) = self.serialized {
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

    pub fn save_state_to_file(&mut self) {
        if let Some(ref s) = self.serialized {
            let save_file = OpenOptions::new()
                .write(true)
                .truncate(true)
                .create(true)
                .open(&self.save_state_file_path());
            match save_file {
                Ok(save_file) => {
                    let mut save_writer = BufWriter::new(save_file);
                    let _ = save_writer.write_all(s.as_bytes());
                }
                Err(e) => {
                    eprintln!("unable to open file to save state: {}", e);
                }
            }
        }
    }

    fn load_state_from_file(&mut self) {
        let save_file = OpenOptions::new()
            .read(true)
            .write(false)
            .create(false)
            .open(&self.save_state_file_path());
        if let Ok(save_file) = save_file {
            println!("Loading save file");
            let mut save_reader = BufReader::new(save_file);
            let mut serialized = String::new();
            let _ = save_reader.read_to_string(&mut serialized);
            self.serialized = Some(serialized);
        }
    }

    fn save_state_file_path(&self) -> PathBuf {
        self.rom_path.with_extension("sav")
    }
}
