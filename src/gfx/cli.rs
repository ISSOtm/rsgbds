/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

#![deny(missing_docs)]

use std::{fmt::Display, num::NonZeroU16, path::PathBuf, str::FromStr};

use arrayvec::ArrayVec;
use clap::{ColorChoice, Parser};

use super::*;
use crate::{rgb::Rgba, InputSlice};

/// The command-line interface.
#[derive(Debug, Parser)]
// We use placeholders in the CLI descriptions, and unfortunately the usual syntax for that looks like HTML tags.
// Since this is internal-only anyway, privilege the CLI UX.
#[allow(rustdoc::invalid_html_tags)]
#[clap(color = crate::common::cli::clap_color_choice())]
#[command(
    name = "rgbgfx",
    version = crate::common::build::PKG_VERSION,
    long_version = crate::common::build::CLAP_LONG_VERSION,
    about = "Game Boy graphics converter",
    long_about = "Converts images into data suitable for display on the Game Boy and Game Boy Color, or vice-versa.",
    after_help = "For comprehensive help, run `man rgbgfx`, or go to http://rgbds.gbdev.io/docs/",
    arg_required_else_help = true,
    help_expected = true
)]
pub(super) struct Cli {
    /// ID to assign to the first tile generated in each bank
    #[arg(short, long, default_value_t = [0, 0].into())]
    base_tiles: NumList<u8, 2>,
    /// Controls when to use color
    #[arg(long, default_value_t = ColorChoice::Auto)]
    color: ColorChoice,
    /// Attempt to use colors more adapted to real hardware
    #[arg(short = 'C', long)]
    color_curve: bool,
    /// Use the specified color palettes instead of automatically determining some
    #[arg(short, long)]
    colors: Option<PalSpec>,
    /// Set the bit depth of the tiles
    ///
    /// This affects both the generated tile data, and the maximum number of colors per palette.
    #[arg(short, long, default_value_t = 2, value_name = "bpp", value_parser = crate::common::cli::parse_number::<u8>)]
    depth: u8,
    /// Only process a portion of the image
    #[arg(short = 'L', long)]
    slice: Option<InputSlice>,
    /// Permit de-duplication of tiles that are a mirror of another one
    #[arg(short, long)]
    mirror_tiles: bool,
    /// Maximum number of tiles to generate for each bank
    // TODO: fail parsing if the value is > 256!
    #[arg(short = 'N', long)]
    nb_tiles: Option<NumList<u16, 2>>,
    /// Limit how many palettes can be generated
    // TODO: fail parsing if the value is > 256!
    #[arg(short, long, default_value_t = 8, value_name = "nb of palettes", value_parser = crate::common::cli::parse_number::<u16>)]
    nb_palettes: u16,
    /// Attempt to re-create a source image from binary data
    ///
    /// Note that this makes the input become an output, and outputs become inputs!
    // TODO: this ought to require a few args: input file, probably tile data?
    #[arg(
        short,
        long,
        value_name = "image width (in tiles)",
        requires = "output"
    )]
    reverse: Option<NonZeroU16>,
    /// Limit how many opaque colors each palette contains
    ///
    /// If applicable, this limit does not include the slot reserved for transparency!
    ///
    /// [default: <max size for bit depth, see --depth>]
    #[arg(short = 's', long, value_name = "nb of colors", value_parser = crate::common::cli::parse_number::<u8>)]
    palette_size: Option<u8>,
    // TODO: implement gbdev/rgbds#1005...
    //#[arg(short = 'U', long)]
    //unit_size: ???,
    /// Permit de-duplication of tiles
    #[arg(short, long)]
    unique_tiles: bool,
    /// Explain what is being done
    ///
    /// Specify multiple times (up to six) for increasing levels of detail.
    #[arg(short, long, action = clap::ArgAction::Count)]
    verbose: u8,
    /// Skip emitting the last <nb> tiles' data
    #[arg(short = 'x', long, default_value_t = 0, value_name = "nb of tiles", value_parser = crate::common::cli::parse_number::<usize>, hide_default_value = true)]
    trim_end: usize,
    /// Scan the image column by column, instead of row by row
    #[arg(short = 'Z', long)]
    columns: bool,

    /// Place files written through `--auto-*` options next to the tile data, instead of the image
    #[arg(help_heading = "Output files", short = 'O', long, requires = "output")]
    group_outputs: bool,
    /// Write a map of GBC tile attributes to <path>
    #[arg(help_heading = "Output files", short, long, value_name = "path")]
    attr_map: Option<PathBuf>,
    /// Write a map of GBC tile attributes to a pre-determined location
    #[arg(
        help_heading = "Output files",
        short = 'A',
        long,
        conflicts_with = "attr_map"
    )]
    auto_attr_map: bool,
    /// Write tile data to <path>
    #[arg(help_heading = "Output files", short, long, value_name = "path")]
    output: Option<PathBuf>,
    /// Write GBC palettes to <path>
    #[arg(help_heading = "Output files", short, long, value_name = "path")]
    palette: Option<PathBuf>,
    /// Write GBC palettes to a pre-determined location
    #[arg(
        help_heading = "Output files",
        short = 'P',
        long,
        conflicts_with = "palette"
    )]
    auto_palette: bool,
    /// Write a map of palette IDs to <path>
    #[arg(help_heading = "Output files", short = 'q', long, value_name = "path")]
    palette_map: Option<PathBuf>,
    /// Write a map of palette IDs to a pre-determined location
    #[arg(
        help_heading = "Output files",
        short = 'Q',
        long,
        conflicts_with = "palette_map"
    )]
    auto_palette_map: bool,
    /// Write a map of tile IDs to <path>
    #[arg(help_heading = "Output files", short, long, value_name = "path")]
    tilemap: Option<PathBuf>,
    /// Write a map of tile IDs to a pre-determined location
    #[arg(
        help_heading = "Output files",
        short = 'T',
        long,
        conflicts_with = "tilemap"
    )]
    auto_tilemap: bool,

    /// Path to the image to convert
    // TODO: should be required if any auto-path is requested, unless `-O` is set
    #[arg(value_name = "path to image", required_unless_present = "colors")]
    input: Option<PathBuf>,
}

#[derive(Debug, Clone)]
enum PalSpec {
    Inline(Vec<Vec<Option<Rgb>>>),
    Embedded,
    External { fmt: String, path: String },
}

#[derive(Debug, Clone)]
struct NumList<U: funty::Unsigned, const NB_MAX: usize>(ArrayVec<U, NB_MAX>);

impl<U: funty::Unsigned, T: Into<ArrayVec<U, NB_MAX>>, const NB_MAX: usize> From<T>
    for NumList<U, NB_MAX>
{
    fn from(value: T) -> Self {
        Self(value.into())
    }
}

impl<U: funty::Unsigned, const NB_MAX: usize> FromStr for NumList<U, NB_MAX> {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut numbers = ArrayVec::new();
        for (i, item) in s.split(',').enumerate() {
            let number = crate::common::cli::parse_number(item)
                .map_err(|err| format!("the {} number is invalid: {err}", Nth(i + 1)))?;
            numbers.try_push(number).map_err(|_err| {
                format!("too many numbers, expected {} at most", numbers.capacity())
            })?;
        }

        if numbers.is_empty() {
            Err("expected at least one number".to_string())
        } else {
            Ok(numbers.into())
        }
    }
}

impl<U: funty::Unsigned, const NB_MAX: usize> Display for NumList<U, NB_MAX> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut numbers = self.0.iter();
        write!(f, "{}", numbers.next().unwrap())?;
        for rest in numbers {
            write!(f, ",{rest}")?;
        }
        Ok(())
    }
}

impl<U: funty::Unsigned, const NB_MAX: usize> NumList<U, NB_MAX> {
    fn finish<F: FnMut(usize) -> U>(self, mut default: F) -> [U; NB_MAX] {
        std::array::from_fn(|i| self.0.get(i).copied().unwrap_or_else(|| default(i)))
    }
}

impl FromStr for InputSlice {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        fn parse_pair(s: &str, idx: usize) -> Result<(u16, u16), String> {
            let (left, right) = s.split_once(',').ok_or_else(|| {
                format!(
                    "{} the colon, expected two numbers, separated by a comma",
                    if idx == 1 { "before" } else { "after" }
                )
            })?;
            Ok((
                crate::common::cli::parse_number(left)
                    .map_err(|err| format!("the {} number is invalid: {err}", Nth(idx)))?,
                crate::common::cli::parse_number(right)
                    .map_err(|err| format!("the {} number is invalid: {err}", Nth(idx + 1)))?,
            ))
        }
        let (origin, size) = s
            .split_once(':')
            .ok_or("expected two pairs of numbers, separated by a colon")?;
        let (left, top) = parse_pair(origin, 1)?;
        let (width, height) = parse_pair(size, 3)?;
        Ok(InputSlice {
            left,
            top,
            width: NonZeroU16::new(width).ok_or("the width cannot be zero")?,
            height: NonZeroU16::new(height).ok_or("the height cannot be zero")?,
        })
    }
}

impl FromStr for PalSpec {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s == "embedded" {
            Ok(Self::Embedded)
        } else if s.starts_with('#') {
            let colors = s
                .trim_matches(|c: char| c.is_ascii_whitespace())
                .split_terminator([';', ':'])
                .map(|pal_str| {
                    pal_str
                        .trim_matches(|c: char| c.is_ascii_whitespace())
                        .split_terminator(',')
                        .map(|color| {
                            if color == "#none" {
                                None
                            } else {
                                Some(Rgb::from_str(color))
                            }
                            .transpose()
                        })
                        .collect()
                })
                .collect::<Result<_, _>>()?;
            Ok(Self::Inline(colors))
        } else {
            let (fmt, path) = s
                .split_once(':')
                .ok_or("an external palette spec must have the format `fmt:path`")?;
            Ok(Self::External {
                fmt: fmt.into(),
                path: path.into(),
            })
        }
    }
}

impl FromStr for Rgb {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let color_str = s
            .trim_matches(|c: char| c.is_ascii_whitespace())
            .strip_prefix('#')
            .ok_or("in an inline palette spec, colors must begin by a hash '#'")?;
        match color_str.len() {
            3 => {
                let mut chars = color_str.chars();
                let mut parse_channel = || -> Result<_, String> {
                    let digit =
                        chars.next().unwrap().to_digit(16).ok_or(
                            "in an inline palette spec, colors must be composed of hex digits",
                        )? as u8;
                    Ok(digit * 0x11)
                };
                Ok(Rgb {
                    red: parse_channel()?,
                    green: parse_channel()?,
                    blue: parse_channel()?,
                })
            }
            6 => {
                let parse_channel = |range| {
                    u8::from_str_radix(&color_str[range], 16).map_err(|_err| {
                        "in an inline palette spec, colors must be composed of hex digits"
                            .to_string()
                    })
                };
                Ok(Rgb {
                    red: parse_channel(0..2)?,
                    green: parse_channel(2..4)?,
                    blue: parse_channel(4..6)?,
                })
            }
            _ => Err(
                "in an inline palette spec, colors must be in `#rgb` or `#rrggbb` format".into(),
            ),
        }
    }
}

impl Cli {
    pub(super) fn finish(self) -> Result<(Options, Option<super::PalSpec>), ()> {
        crate::common::cli::apply_color_choice(self.color);

        let max_nb_colors_per_pal = 1 << self.depth;
        let nb_colors_per_pal = NonZeroU8::new(match self.palette_size {
            Some(size) => {
                if size > max_nb_colors_per_pal {
                    crate::build_error()
                        .with_message(format!(
                            "{}bpp palettes cannot contain {size} colors",
                            self.depth
                        ))
                        .with_note(format!("The maximum is {max_nb_colors_per_pal}"))
                        .finish()
                        .eprint_();
                    return Err(());
                }
                size
            }
            None => max_nb_colors_per_pal,
        })
        .ok_or_else(|| {
            crate::build_error()
                .with_message("Palettes cannot contain zero colors")
                .finish()
                .eprint_()
        })?;

        let pal_spec = self
            .colors
            .map(|pal_spec| {
                Ok(match pal_spec {
                    PalSpec::Inline(spec) => super::PalSpec::Explicit(
                        spec.into_iter()
                            .map(|pal| {
                                pal.into_iter()
                                    .map(|opt| {
                                        opt.map(|rgb| Rgba::from(rgb).cgb_color(self.color_curve))
                                    })
                                    .collect()
                            })
                            .collect(),
                    ),
                    PalSpec::Embedded => super::PalSpec::Embedded,
                    PalSpec::External { fmt, path } => {
                        let mut colors = pal_spec::parse_palette_file(
                            &fmt,
                            &path,
                            nb_colors_per_pal,
                            self.color_curve,
                        )?;
                        if colors.len() > self.nb_palettes.into() {
                            // TODO: warn about the truncation
                            colors.truncate(self.nb_palettes.into());
                        }
                        super::PalSpec::Explicit(colors)
                    }
                })
            })
            .transpose()?;

        fn auto_path(
            auto: bool,
            opt: Option<PathBuf>,
            extension: &str,
            group_outputs: bool,
            output: &Option<PathBuf>,
            input: &Option<PathBuf>,
        ) -> Option<PathBuf> {
            if auto {
                debug_assert_eq!(opt, None);
                let base = if group_outputs { output } else { input }.as_ref().unwrap();
                let mut target = base.clone();
                target.set_extension(extension);
                Some(target)
            } else {
                opt
            }
        }
        Ok((
            Options {
                reversed_width: self.reverse,
                verbosity: self.verbose,
                palettes_path: auto_path(
                    self.auto_palette,
                    self.palette,
                    "pal",
                    self.group_outputs,
                    &self.output,
                    &self.input,
                ),
                tilemap_path: auto_path(
                    self.auto_tilemap,
                    self.tilemap,
                    "tilemap",
                    self.group_outputs,
                    &self.output,
                    &self.input,
                ),
                attrmap_path: auto_path(
                    self.auto_attr_map,
                    self.attr_map,
                    "attrmap",
                    self.group_outputs,
                    &self.output,
                    &self.input,
                ),
                palmap_path: auto_path(
                    self.auto_palette_map,
                    self.palette_map,
                    "palmap",
                    self.group_outputs,
                    &self.output,
                    &self.input,
                ),
                input_path: self.input,
                output_path: self.output,
                use_color_curve: self.color_curve,
                allow_mirroring: self.mirror_tiles,
                allow_dedup: self.unique_tiles || self.mirror_tiles,
                column_major: self.columns,
                bit_depth: self.depth,
                input_slice: self.slice,
                nb_palettes: self.nb_palettes,
                nb_colors_per_pal,
                trim: self.trim_end,
                base_tile_ids: self.base_tiles.finish(|_i| 0),
                max_nb_tiles: self
                    .nb_tiles
                    .map(|max_nb_tiles| max_nb_tiles.finish(|_i| 0)),
            },
            pal_spec,
        ))
    }
}
