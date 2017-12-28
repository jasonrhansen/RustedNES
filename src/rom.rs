use std::cmp::max;
use std::error::Error;
use std::fmt;
use std::fmt::{Debug, Display, Formatter};
use std::io;
use std::io::{Read, Seek, SeekFrom};
use byteorder::{ReadBytesExt, BigEndian};

// ROM must begin with this constant ("NES" followed by MS-DOS end-of-file)
const MAGIC_CONSTANT: u32 = 0x4e45531a;

pub const PRG_ROM_BANK_SIZE: u16 = 16 * 1024;
const CHR_ROM_BANK_SIZE: u16 = 8 * 1024;

const PRG_RAM_BANK_SIZE: u16 = 8 * 1024;

#[derive(Clone, Copy, Debug)]
pub enum Mirroring {
    Horizontal,
    Vertical,
    FourScreen,
}

#[derive(Debug)]
pub enum LoadError {
    FormatError(String),
    IoError(io::Error),
}

impl LoadError {
    fn new<M: Into<String>>(msg: M) -> LoadError {
        LoadError::FormatError(msg.into())
    }
}

impl Display for LoadError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            LoadError::FormatError(ref m) => write!(f, "{}", m),
            LoadError::IoError(ref e) => write!(f, "{}", e),
        }
    }
}

impl Error for LoadError {
    fn description(&self) -> &str {
        match *self {
            LoadError::FormatError(ref m) => &m[..],
            LoadError::IoError(ref e) => e.description(),
        }
    }
}

impl From<io::Error> for LoadError {
    fn from(error: io::Error) -> Self {
        LoadError::IoError(error)
    }
}

pub struct NesRom {
    pub mapper: u16,
    pub sub_mapper: u8,
    pub mirroring: Mirroring,
    pub prg_rom: Vec<u8>,
    pub chr_rom: Vec<u8>,
    pub prg_ram_size: u16,
}

impl Debug for NesRom {
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        write!(f, "mapper: {}, sub_mapper: {}, mirroring: {:?}, PRG ROM size: {}, CHR ROM size: {}",
               self.mapper, self.sub_mapper, self.mirroring, self.prg_rom.len(), self.chr_rom.len())
    }
}

impl NesRom {
    pub fn load<R: Read + Seek>(r: &mut R) -> Result<NesRom, LoadError> {
        let magic = r.read_u32::<BigEndian>()?;

        if magic != MAGIC_CONSTANT {
            return Err(LoadError::new("magic constant in header is incorrect"));
        }

        let prg_rom_size = r.read_u8()? as u16 * PRG_RAM_BANK_SIZE;
        let chr_rom_size = r.read_u8()? as u16 * CHR_ROM_BANK_SIZE;

        let flags6 = r.read_u8()?;
        let flags7 = r.read_u8()?;

        let prg_ram_banks = max(1, r.read_u8()? as u16);

        // Skip the rest of the header
        // TODO: Implement NEW 2.0
        r.seek(SeekFrom::Current(17))?;

        let has_trainer = (flags6 & 0x04) != 0;
        if has_trainer {
            // Skip over trainer. We won't support it.
            r.seek(SeekFrom::Current(512))?;
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

        let mut prg_rom = vec![0u8; prg_rom_size as usize];
        r.read_exact(&mut prg_rom[..])?;

        let mut chr_rom = vec![0u8; chr_rom_size as usize];
        r.read_exact(&mut chr_rom[..])?;

        let prg_ram_size = prg_ram_banks * PRG_RAM_BANK_SIZE;

        Ok(NesRom {
            mapper,
            sub_mapper,
            mirroring,
            prg_rom,
            chr_rom,
            prg_ram_size,
        })
    }
}