use std::fmt::Display;

use plumers::{color::Rgb32, prelude::Rgb16};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Rgb {
    pub red: u8,
    pub green: u8,
    pub blue: u8,
}

impl From<Rgba> for Rgb {
    fn from(value: Rgba) -> Self {
        Self {
            red: value.red,
            green: value.green,
            blue: value.blue,
        }
    }
}

/// Format the color using CSS notation (`#13579b`).
impl Display for Rgb {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{:x}{:x}{:x}", self.red, self.green, self.blue)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Rgba {
    pub red: u8,
    pub green: u8,
    pub blue: u8,
    pub alpha: u8,
}

/// Conversion from a Game Boy Color color (RGBA5551).
impl From<Rgb16> for Rgba {
    fn from(value: Rgb16) -> Self {
        let expand_channel = |rgb555| {
            let channel = rgb555 as u8 & 0b11111;
            channel << 3 | channel >> 2
        };
        Self {
            red: expand_channel(value.0),
            green: expand_channel(value.0 >> 5),
            blue: expand_channel(value.0 >> 10),
            alpha: if value.0 & 0x8000 != 0 { 0xFF } else { 0x00 },
        }
    }
}

impl From<Rgb32> for Rgba {
    fn from(value: Rgb32) -> Self {
        let [red, green, blue, alpha] = value.0.to_le_bytes();
        Self {
            red,
            green,
            blue,
            alpha,
        }
    }
}

impl Rgba {
    pub const TRANSPARENCY_THRESHOLD: u8 = 0x0F;
    pub const OPACITY_THRESHOLD: u8 = u8::MAX - Self::TRANSPARENCY_THRESHOLD;
    pub fn opacity(&self) -> Option<Opacity> {
        match self.alpha {
            ..=Self::TRANSPARENCY_THRESHOLD => Some(Opacity::Transparent),
            Self::OPACITY_THRESHOLD.. => Some(Opacity::Opaque),
            _ => None,
        }
    }

    pub const TRANSPARENT: Rgb16 = Rgb16(0x8000);
    /// # Panics
    ///
    /// This function panics if the color is neither opaque nor transparent (as defined by [`opacity()`][Self::opacity()]).
    pub fn cgb_color(&self, use_curve: bool) -> Rgb16 {
        /*
         * Based on inverting the "Modern - Accurate" formula used by SameBoy
         * since commit b5a611c5db46d6a0649d04d24d8d6339200f9ca1 (Dec 2020),
         * with gaps in the scale curve filled by polynomial interpolation.
         */
        const COLOR_CURVE: [u8; 256] = [
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, // These
            1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, // comments
            3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, // prevent
            5, 5, 5, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, // rustfmt
            7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 10, 10, 10, 10, // from
            10, 10, 11, 11, 11, 11, 11, 11, 12, 12, 12, 12, 12, 13, 13, 13, // reflowing
            13, 13, 14, 14, 14, 14, 14, 14, 15, 15, 15, 15, 15, 16, 16, 16, // these
            16, 16, 16, 17, 17, 17, 17, 17, 18, 18, 18, 18, 18, 18, 19, 19, // sixteen
            19, 19, 19, 20, 20, 20, 20, 20, 20, 21, 21, 21, 21, 21, 21, 22, // 16-item
            22, 22, 22, 22, 22, 22, 23, 23, 23, 23, 23, 23, 24, 24, 24, 24, // lines,
            24, 24, 24, 25, 25, 25, 25, 25, 25, 25, 25, 26, 26, 26, 26, 26, // which,
            26, 26, 26, 27, 27, 27, 27, 27, 27, 27, 27, 27, 28, 28, 28, 28, // in
            28, 28, 28, 28, 28, 28, 29, 29, 29, 29, 29, 29, 29, 29, 29, 29, // my
            29, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, 30, // opinion,
            31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, // help
            31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, 31, // readability!
        ];

        match self.opacity().unwrap() {
            Opacity::Opaque => {
                let (r, g, b) = if use_curve {
                    // TODO: I wonder if this can be done without float math
                    let green_linear = (self.green as f64 / 255.).powf(2.2);
                    let blue_linear = (self.blue as f64 / 255.).powf(2.2);
                    // Originally: ((green_linear * 4. - blue_linear) / 3.).clamp(0., 1.)
                    let green_adjusted =
                        (green_linear * (4. / 3.) - blue_linear * (1. / 3.)).clamp(0., 1.);

                    let green = (green_adjusted.powf(1. / 2.2) * 255.) as u8;
                    (
                        COLOR_CURVE[usize::from(self.red)],
                        COLOR_CURVE[usize::from(green)],
                        COLOR_CURVE[usize::from(self.blue)],
                    )
                } else {
                    (self.red >> 3, self.green >> 3, self.blue >> 3)
                };
                Rgb16(r as u16 | (g as u16) << 5 | (b as u16) << 10)
            }
            // TODO: is this ever reached?
            Opacity::Transparent => Self::TRANSPARENT,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Opacity {
    Opaque,
    Transparent,
}

/// Format the color using CSS notation (`#02468ace`).
impl std::fmt::Display for Rgba {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "#{:02x}{:02x}{:02x}{:02x}",
            self.red, self.green, self.blue, self.alpha
        )
    }
}
