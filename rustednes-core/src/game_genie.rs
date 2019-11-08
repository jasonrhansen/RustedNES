use std::fmt;

#[derive(PartialEq, Eq, Copy, Clone)]
pub struct Cheat {
    address: u16,
    data: u8,
    compare: Option<u8>,
}

impl Cheat {
    pub fn from_code(code: &[u8]) -> Result<Cheat, String> {
        decode(code)
    }

    pub fn address(self) -> u16 {
        self.address
    }

    pub fn data(self) -> u8 {
        self.data
    }

    pub fn compare(self) -> Option<u8> {
        self.compare
    }
}

impl fmt::Debug for Cheat {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Cheat {{ address: 0x{:04x}, data: 0x{:02x}, compare: {} }}",
            self.address,
            self.data,
            if let Some(c) = self.compare {
                format!("0x{:02x}", c)
            } else {
                "None".to_string()
            }
        )
    }
}

fn decode(code: &[u8]) -> Result<Cheat, String> {
    let len = code.len();
    if len != 6 && len != 8 {
        return Err("Code must be 6 or 8 characters".into());
    }

    let mut n = vec![0u8; len];
    for (i, b) in code.iter().enumerate() {
        n[i] = decode_byte(*b)?;
    }

    let n = n;

    let address = (0x8000 + ((n[3] as u16 & 7) << 12))
        | ((n[5] as u16 & 7) << 8)
        | ((n[4] as u16 & 8) << 8)
        | ((n[2] as u16 & 7) << 4)
        | ((n[1] as u16 & 8) << 4)
        | (n[4] as u16 & 7)
        | (n[3] as u16 & 8);

    let (data, compare) = if len == 8 {
        (
            ((n[1] & 7) << 4) | ((n[0] & 8) << 4) | (n[0] & 7) | (n[7] & 8),
            Some(((n[7] & 7) << 4) | ((n[6] & 8) << 4) | (n[6] & 7) | (n[5] & 8)),
        )
    } else {
        (
            ((n[1] & 7) << 4) | ((n[0] & 8) << 4) | (n[0] & 7) | (n[5] & 8),
            None,
        )
    };

    Ok(Cheat {
        address,
        data,
        compare,
    })
}

fn decode_byte(byte: u8) -> Result<u8, String> {
    match byte as char {
        'A' => Ok(0x0),
        'P' => Ok(0x1),
        'Z' => Ok(0x2),
        'L' => Ok(0x3),
        'G' => Ok(0x4),
        'I' => Ok(0x5),
        'T' => Ok(0x6),
        'Y' => Ok(0x7),
        'E' => Ok(0x8),
        'O' => Ok(0x9),
        'X' => Ok(0xA),
        'U' => Ok(0xB),
        'K' => Ok(0xC),
        'S' => Ok(0xD),
        'V' => Ok(0xE),
        'N' => Ok(0xF),
        c => Err(format!("Invalid code byte {}", c)),
    }
}

#[test]
fn test_valid_cheats() {
    let tests = [
        ("GOSSIP", 0xD1DD, 0x14, None),
        ("ZEXPYGLA", 0x94A7, 0x02, Some(0x03)),
        ("NTEINNYK", 0xDF07, 0xEF, Some(0x4F)),
        ("GXVUZGVG", 0xB4EA, 0x24, Some(0xC6)),
        ("GZNVILST", 0xE37D, 0x24, Some(0xE5)),
    ];

    for (code, address, data, compare) in tests.iter() {
        assert_eq!(
            Cheat::from_code(code.as_bytes()).unwrap(),
            Cheat {
                address: *address,
                data: *data,
                compare: *compare
            }
        );
    }
}

#[test]
fn test_invalid_cheats() {
    let tests = ["GOSSI", "GOSSIPP", "BBBBBB", "CCCCCCCC"];

    for code in tests.iter() {
        assert!(Cheat::from_code(code.as_bytes()).is_err())
    }
}
