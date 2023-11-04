/* SPDX-License-Identifier: MPL-2.0 */

use std::num::IntErrorKind;

use parse_display::Display;

pub mod rpn;
pub mod section;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ExportLevel {
    Local,
    Import,
    Export,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum RelocKind {
    /// 1-byte.
    Byte = 0,
    /// 2-byte.
    Word = 1,
    /// 4-byte.
    Long = 2,
    /// Signed 8-bit offset.
    Ofs8 = 3,
}

impl RelocKind {
    pub fn width(self) -> u8 {
        match self {
            Self::Byte => 1,
            Self::Word => 2,
            Self::Long => 4,
            Self::Ofs8 => 1,
        }
    }

    pub fn is_in_range(&self, value: i32) -> TruncationLevel {
        let nb_bits = match self {
            Self::Byte => 8,
            Self::Word => 16,
            Self::Long => return TruncationLevel::None, // This is obviously always in range.
            Self::Ofs8 => {
                return if !(-128..=127).contains(&value) {
                    TruncationLevel::Strict
                } else {
                    TruncationLevel::None
                }
            }
        };

        if value <= -(1 << nb_bits) || value >= 1 << nb_bits {
            // If the value is neither in unsigned range nor in the negative unsigned range, it's obviously wrong.
            TruncationLevel::Strict
        } else if value < -(1 << (nb_bits - 1)) {
            // If the value is outside of the signed range, it may or may not be wrong.
            TruncationLevel::Loose
        } else {
            TruncationLevel::None
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TruncationLevel {
    None,
    Loose,
    Strict,
}

// ---

#[derive(Debug, Clone, Display)]
pub enum ParseNumError<T> {
    #[display("cannot parse an integer from an empty string")]
    Empty,
    #[display("an invalid digit was found")]
    InvalidDigit,
    #[display("the number cannot be lower than {0}")]
    TooSmall(T),
    #[display("the number cannot be higher than {0}")]
    TooLarge(T),
}

pub fn parse_generic_u16(string: &str, min: u16, max: u16) -> Result<u16, ParseNumError<u16>> {
    let mut chars = string.chars();
    let parse_res =
        if chars.next() == Some('$') && chars.next().is_some_and(|c| c.is_ascii_hexdigit()) {
            u16::from_str_radix(&string[1..], 16)
        } else {
            string.parse()
        };

    match parse_res {
        Ok(num) => {
            if num < min {
                Err(ParseNumError::TooSmall(min))
            } else if num > max {
                Err(ParseNumError::TooLarge(max))
            } else {
                Ok(num)
            }
        }

        Err(err) => Err(match err.kind() {
            IntErrorKind::Empty => ParseNumError::Empty,
            IntErrorKind::InvalidDigit => ParseNumError::InvalidDigit,
            IntErrorKind::PosOverflow => ParseNumError::TooSmall(min),
            IntErrorKind::NegOverflow => ParseNumError::TooLarge(max),
            _ => panic!("Unknown parse error!?"),
        }),
    }
}
