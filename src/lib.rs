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

#[derive(Debug)]
pub enum TruncationLevel {
    None,
    Loose,
    Strict,
}
