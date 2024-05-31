/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

use crate::{palette::Palette, rgb::Rgba, Options};

use super::Tile;

#[derive(Debug, Clone)]
pub struct TileData {
    pub bytes: [u8; 16],
    /// FIXME: blegh. This duplicate of [`Options::allow_mirroring`] is only necessary for [`Self::eq`] to be self-contained.
    allow_mirroring: bool,
}

impl TileData {
    pub(super) fn row_bitplanes(
        tile: &Tile<'_, '_>,
        palette: &Palette,
        y: u8,
        options: &Options,
        has_transparency: bool,
    ) -> u16 {
        let mut row = 0;
        for x in 0..8 {
            row <<= 1;
            let color = Rgba::from(tile.pixel(x, y)).cgb_color(options.use_color_curve);
            let index = palette
                .index_of(color, has_transparency)
                .unwrap_or_else(|| {
                    panic!(
                        "The color ({color:?}) should be in the palette?!?
{palette:?}
has_transparency = {has_transparency}"
                    )
                });
            debug_assert!(index <= 3, "{index}");
            if (index & 1) != 0 {
                row |= 1;
            }
            if (index & 2) != 0 {
                row |= 0x100;
            }
        }
        row
    }

    pub(super) fn new(
        tile: &Tile<'_, '_>,
        palette: &Palette,
        options: &Options,
        has_transparency: bool,
    ) -> Self {
        let mut bytes = [0; 16];

        for (y, bitplanes) in bytes.chunks_exact_mut(2).enumerate() {
            let [bp0, bp1] = Self::row_bitplanes(tile, palette, y as u8, options, has_transparency)
                .to_le_bytes();
            bitplanes[0] = bp0;
            if options.bit_depth == 2 {
                bitplanes[1] = bp1;
            }
        }

        Self {
            bytes,
            allow_mirroring: options.allow_mirroring,
        }
    }

    pub fn try_matching(&self, other: &Self) -> Option<TileMatchKind> {
        // Check for strict equality first, as that can typically be optimized better by the compiler,
        // and it allows hoisting the `allow_mirroring` check out of any loop.
        if self.bytes == other.bytes {
            return Some(TileMatchKind {
                vflip: false,
                hflip: false,
            });
        } else if !self.allow_mirroring {
            return None;
        } else if std::iter::zip(&self.bytes, &other.bytes)
            .all(|(&our_bitplane, &their_bitplane)| our_bitplane == their_bitplane.reverse_bits())
        {
            return Some(TileMatchKind {
                vflip: false,
                hflip: true,
            });
        }

        let mut has_vflip = true;
        let mut has_vhflip = true;
        for (ours, theirs) in
            std::iter::zip(self.bytes.chunks_exact(2), other.bytes.rchunks_exact(2)).take(4)
        {
            let mut compare = |our_bitplane, their_bitplane: u8| {
                if our_bitplane != their_bitplane {
                    has_vflip = false;
                }
                if our_bitplane != their_bitplane.reverse_bits() {
                    has_vhflip = false;
                }
            };

            compare(ours[0], theirs[0]);
            compare(ours[1], theirs[1]);
        }

        // If we have both (i.e. the tile is horizontally symmetrical), default to vflip only.
        if has_vflip {
            Some(TileMatchKind {
                vflip: true,
                hflip: false,
            })
        } else if has_vhflip {
            Some(TileMatchKind {
                vflip: true,
                hflip: true,
            })
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TileMatchKind {
    pub vflip: bool,
    pub hflip: bool,
}

impl PartialEq for TileData {
    fn eq(&self, other: &Self) -> bool {
        self.try_matching(other).is_some()
    }
}

impl Eq for TileData {}

/// This hash implementation is specifically tailored to return the same hash for mirrored tiles too.
/// It was contributed by aaaaaa123456789 / ax6; thank you very much!
///
/// The way that this is done, is by hashing each row individually, and adding the hashes;
/// since addition is commutative, this takes care of vertical mirroring.
/// On its own, this would induce collisions with other permutations of the same pixel rows;
/// to mitigate this, each row's hash is multiplied by a factor, with mirror-able rows using the same factor.
///
/// Horizontal mirroring is taken care of by giving any row the same hash as its horizontal mirror;
/// a trivial implementation of this is to simply use the largest of these two.
///
/// Finally, notice that each bitplane is hashed independently into a [`u8`],
/// which are combined into the final [`u16`] hash.
/// This performs better than trying to combine the bitplanes in any way, since the bitplanes are essentially independent.
///
/// Since tile data has a fixed size, there is no need to write an extra byte to avoid prefix collisions.
impl std::hash::Hash for TileData {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        /// These factors are picked so that combinations of them (by addition) don't share common factors.
        /// Being mutually prime is a requirement, and being prime satisfies that with less work.
        /// (TODO: 37 + 23 being equal to 31 + 29 isn't ideal; consider picking other factors?)
        ///
        /// Ideally, we would like all combinations of the primes to have no common factors, but we also want them all to be odd.
        /// This is obviously possible, but the numbers would be extremely big.
        /// It's impossible for all combinations to be coprime, but we want the GCDs to be as small as possible,
        /// and the sums to have as few factors of 2 as possible.
        /// This is so that `hash([a, b, ...])` doesn't collide with `hash([b, a, ...])`,
        /// and `hash([a, a, ...])` doesn't guarantee zeroes at the end of the hash.
        ///
        /// In this case, since 23 + 37 = 29 + 31, (a, b, c, d) and (b, a, d, c) would have the same hash.
        const FACTORS: [u8; 8] = [23, 29, 31, 37, 37, 31, 29, 23];

        let mut low = 0;
        let mut high = 0;
        for (y, factor) in FACTORS.iter().enumerate() {
            let incorporate_row = |hash: &mut u8, bitplane: u8| {
                *hash = hash.wrapping_add(
                    factor.wrapping_mul(std::cmp::max(bitplane, bitplane.reverse_bits())),
                )
            };
            incorporate_row(&mut low, self.bytes[y * 2]);
            incorporate_row(&mut high, self.bytes[y * 2 + 1]);
        }
        state.write_u16(u16::from_le_bytes([low, high]));
    }
}
