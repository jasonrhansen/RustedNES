use byteorder::{BigEndian, ReadBytesExt};
use serde_derive::{Deserialize, Serialize};
use thiserror::Error;

use std::cmp::max;
use std::fmt;
use std::fmt::{Debug, Formatter};
use std::io;
use std::io::Read;

// ROM must begin with this constant ("NES" followed by MS-DOS end-of-file)
const MAGIC_CONSTANT: u32 = 0x4e45_531a;

pub const PRG_ROM_BANK_SIZE: u16 = 16 * 1024;
pub const CHR_ROM_BANK_SIZE: u16 = 8 * 1024;
pub const PRG_RAM_BANK_SIZE: u16 = 8 * 1024;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Deserialize, Serialize)]
pub enum Mirroring {
    Horizontal,
    Vertical,
    OneScreenLower,
    OneScreenUpper,
    FourScreen,
}

impl Mirroring {
    pub fn mirror_address(self, address: u16) -> u16 {
        let address = address & 0x2FFF;
        match self {
            Mirroring::Horizontal => {
                let address = address & 0x2BFF;
                if address < 0x2800 {
                    address
                } else {
                    address - 0x0400
                }
            }
            Mirroring::Vertical => address & 0x27FF,
            Mirroring::OneScreenLower => address & 0x23FF,
            Mirroring::OneScreenUpper => (address & 0x27FF) | 0x0400,
            Mirroring::FourScreen => address,
        }
    }
}

#[derive(Error, Debug)]
pub enum LoadError {
    #[error("{0}")]
    FormatError(String),
    #[error("{0}")]
    IoError(#[from] io::Error),
}

pub struct Cartridge {
    pub mapper: u16,
    pub sub_mapper: u8,
    pub mirroring: Mirroring,
    pub default_mirroring: Mirroring,
    pub prg_rom_num_banks: u8,
    pub prg_rom: Vec<u8>,
    pub chr_num_banks: u8,
    pub chr: Vec<u8>,
    pub prg_ram: Vec<u8>,
    pub is_battery_backed: bool,
}

#[derive(Deserialize, Serialize)]
pub struct State {
    pub mirroring: Mirroring,
    #[serde(with = "serde_bytes")]
    pub chr: Vec<u8>,
    #[serde(with = "serde_bytes")]
    pub prg_ram: Vec<u8>,
}

impl Debug for Cartridge {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "mapper: {}", self.mapper)?;
        writeln!(f, "sub mapper: {}", self.sub_mapper)?;
        writeln!(f, "mirroring: {:?}", self.mirroring)?;
        writeln!(f, "PRG ROM size: {}", self.prg_rom.len())?;
        writeln!(f, "CHR ROM size: {}", self.chr.len())?;
        writeln!(f, "PRG RAM size: {}", self.prg_ram.len())?;
        writeln!(f, "battery backed: {}", self.is_battery_backed)
    }
}

impl Cartridge {
    pub fn load<R: Read>(r: &mut R) -> Result<Cartridge, LoadError> {
        let magic = r.read_u32::<BigEndian>()?;

        if magic != MAGIC_CONSTANT {
            return Err(LoadError::FormatError(
                "magic constant in header is incorrect".into(),
            ));
        }

        let prg_rom_num_banks = r.read_u8()?;
        let mut chr_num_banks = r.read_u8()?;

        let prg_rom_size = prg_rom_num_banks as usize * PRG_ROM_BANK_SIZE as usize;
        let chr_rom_size = chr_num_banks as usize * CHR_ROM_BANK_SIZE as usize;

        let flags6 = r.read_u8()?;
        let flags7 = r.read_u8()?;

        let prg_ram_size = max(1, r.read_u8()?) as usize * PRG_RAM_BANK_SIZE as usize;

        // Skip the rest of the header
        // TODO: Implement NEW 2.0
        for _ in 0..7 {
            r.read_u8()?;
        }

        let is_battery_backed = (flags6 & 0x02) != 0;

        let has_trainer = (flags6 & 0x04) != 0;
        if has_trainer {
            // Skip over trainer. We won't support it.
            for _ in 0..512 {
                r.read_u8()?;
            }
        }

        let mapper = ((flags7 & 0xf0) | (flags6 >> 4)) as u16;
        let sub_mapper = 0u8;

        let mirroring = if (flags6 & 0x08) != 0 {
            Mirroring::FourScreen
        } else if (flags6 & 0x01) == 1 {
            Mirroring::Vertical
        } else {
            Mirroring::Horizontal
        };

        let mut prg_rom = vec![0u8; prg_rom_size];
        r.read_exact(&mut prg_rom[..])?;

        let mut chr = vec![0u8; chr_rom_size];
        r.read_exact(&mut chr[..])?;

        // Add CHR bank if not in file
        if chr_num_banks == 0 {
            chr_num_banks = 1;
            chr = vec![0u8; CHR_ROM_BANK_SIZE as usize];
        }

        let prg_ram = vec![0u8; prg_ram_size];

        Ok(Cartridge {
            mapper,
            sub_mapper,
            mirroring,
            default_mirroring: mirroring,
            prg_rom_num_banks,
            prg_rom,
            chr_num_banks,
            chr,
            prg_ram,
            is_battery_backed,
        })
    }

    pub fn get_state(&self) -> State {
        State {
            mirroring: self.mirroring,
            chr: self.chr.clone(),
            prg_ram: self.prg_ram.clone(),
        }
    }

    pub fn apply_state(&mut self, state: &State) {
        self.mirroring = state.mirroring;
        self.chr = state.chr.clone();
        self.prg_ram = state.prg_ram.clone();
    }
}
