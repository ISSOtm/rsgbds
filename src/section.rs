use std::ops::RangeInclusive;

use parse_display::Display;

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Display)]
#[display(style = "UPPERCASE")]
pub enum Kind {
    Wram0 = 0,
    Vram = 1,
    Romx = 2,
    Rom0 = 3,
    Hram = 4,
    Wramx = 5,
    Sram = 6,
    Oam = 7,
}

impl Kind {
    pub fn start_addr(&self) -> u16 {
        match self {
            Self::Wram0 => 0xC000,
            Self::Vram => 0x8000,
            Self::Romx => 0x4000,
            Self::Rom0 => 0x0000,
            Self::Hram => 0xFF80,
            Self::Wramx => 0xD000,
            Self::Sram => 0xA000,
            Self::Oam => 0xFE00,
        }
    }

    pub fn size(&self, large_rom0: bool, large_wram0: bool) -> u16 {
        match self {
            Self::Wram0 => {
                if large_wram0 {
                    0x2000
                } else {
                    0x1000
                }
            }
            Self::Vram => 0x2000,
            Self::Romx => 0x4000,
            Self::Rom0 => {
                if large_rom0 {
                    0x8000
                } else {
                    0x4000
                }
            }
            Self::Hram => 0x7F,
            Self::Wramx => 0x1000,
            Self::Sram => 0x2000,
            Self::Oam => 0xA0,
        }
    }

    pub fn banks(&self, banked_vram: bool) -> RangeInclusive<u32> {
        match self {
            Self::Wram0 => 0..=0,
            Self::Vram => 0..=if banked_vram { 1 } else { 0 },
            Self::Romx => 1..=u32::MAX,
            Self::Rom0 => 0..=0,
            Self::Hram => 0..=0,
            Self::Wramx => 1..=7,
            Self::Sram => 0..=u32::MAX,
            Self::Oam => 0..=0,
        }
    }

    pub fn has_data(&self) -> bool {
        matches!(self, Self::Rom0 | Self::Romx)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Display)]
pub enum Modifier {
    Normal,
    Union,
    Fragment,
}
