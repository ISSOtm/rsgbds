/* SPDX-License-Identifier: MPL-2.0 */

use clap::Parser;
use plumers::{
    color::Rgb32,
    image::{DynImage32, Frame},
};
use std::{fmt::Debug, num::NonZeroU16, path::PathBuf, process::ExitCode};

mod color_set;
mod error;
pub use error::Diagnostic;
use error::Reporter;
mod pal_packing;
mod palette;
mod process;
mod rgb;
use rgb::Rgb;

mod cli {
    #![deny(missing_docs)]

    use std::{fmt::Display, num::NonZeroU16, path::PathBuf, str::FromStr};

    use arrayvec::ArrayVec;
    use clap::Parser;

    use super::*;
    use crate::InputSlice;

    /// The command-line interface.
    #[derive(Debug, Parser)]
    #[clap(color = concolor_clap::color_choice())]
    #[command(
        name = "rgbgfx",
        version,
        about = "Game Boy graphics converter",
        long_about = "Converts images into data suitable for display on the Game Boy and Game Boy Color, or vice-versa.",
        after_help = "For comprehensive help, run `man rgbgfx`, or go to http://rgbds.gbdev.io/docs/"
    )]
    pub(super) struct Cli {
        /// Write a map of GBC tile attributes to <path>
        #[arg(short, long, value_name = "path")]
        pub(super) attr_map: Option<PathBuf>,
        /// Write a map of GBC tile attributes to a pre-determined location
        #[arg(short = 'A', long, conflicts_with = "attr_map")]
        pub(super) auto_attr_map: bool,
        /// ID to assign to the first tile generated in each bank
        #[arg(short, long, default_value_t = [0, 0].into())]
        pub(super) base_tiles: NumList<u8, 2>,
        #[command(flatten)]
        pub(super) color: concolor_clap::Color,
        /// Attempt to use colors more adapted to real hardware
        #[arg(short = 'C', long)]
        pub(super) color_curve: bool,
        /// Use the specified color palettes instead of automatically determining some
        #[arg(short, long)]
        pub(super) colors: Option<PalSpec>,
        /// Set the bit depth of the tiles
        ///
        /// This affects both the generated tile data, and the maximum number of colors per palette.
        #[arg(short, long, default_value_t = 2, value_name = "bpp", value_parser = parse_number::<u8>)]
        pub(super) depth: u8,
        /// Only process a portion of the image
        #[arg(short = 'L', long)]
        pub(super) slice: Option<InputSlice>,
        /// Permit de-duplication of tiles that are a mirror of another one
        #[arg(short, long)]
        pub(super) mirror_tiles: bool,
        /// Maximum number of tiles to generate for each bank
        #[arg(short = 'N', long)]
        pub(super) nb_tiles: Option<NumList<u8, 2>>,
        /// Limit how many palettes can be generated
        #[arg(short, long, default_value_t = 8, value_name = "nb of palettes", value_parser = parse_number::<u8>)]
        pub(super) nb_palettes: u8,
        /// Place files written through `--auto-*` options next to the tile data, instead of the image
        #[arg(short = 'O', long, requires = "output")]
        pub(super) group_outputs: bool,
        /// Write tile data to <path>
        #[arg(short, long, value_name = "path")]
        pub(super) output: Option<PathBuf>,
        /// Write GBC palettes to <path>
        #[arg(short, long, value_name = "path")]
        pub(super) palette: Option<PathBuf>,
        /// Write GBC palettes to a pre-determined location
        #[arg(short = 'P', long, conflicts_with = "palette")]
        pub(super) auto_palette: bool,
        /// Write a map of palette IDs to <path>
        #[arg(short = 'q', long, value_name = "path")]
        pub(super) palette_map: Option<PathBuf>,
        /// Write a map of palette IDs to a pre-determined location
        #[arg(short = 'Q', long, conflicts_with = "palette_map")]
        pub(super) auto_palette_map: bool,
        /// Attempt to re-create a source image from binary data
        ///
        /// Note that this makes the input become an output, and outputs become inputs!
        // TODO: width, or stride?
        // TODO: this ought to require a few args: input file, probably tile data?
        #[arg(short, long, value_name = "image width (in tiles)")]
        pub(super) reverse: Option<NonZeroU16>,
        /// Limit how many opaque colors each palette contains
        ///
        /// If applicable, this limit does not include the slot reserved for transparency!
        #[arg(short = 's', long, value_name = "nb of colors", value_parser = parse_number::<u8>)]
        pub(super) palette_size: Option<u8>,
        /// Write a map of tile IDs to <path>
        #[arg(short, long, value_name = "path")]
        pub(super) tilemap: Option<PathBuf>,
        /// Write a map of tile IDs to a pre-determined location
        #[arg(short = 'T', long, conflicts_with = "tilemap")]
        pub(super) auto_tilemap: bool,
        // TODO: implement gbdev/rgbds#1005...
        //#[arg(short = 'U', long)]
        //pub(super) unit_size: ???,
        /// Permit de-duplication of tiles
        #[arg(short, long)]
        pub(super) unique_tiles: bool,
        /// Explain what is being done
        ///
        /// Specify multiple times (up to six) for increasing levels of detail.
        #[arg(short, long, action = clap::ArgAction::Count)]
        pub(super) verbose: u8,
        /// Skip emitting the last <nb> tiles' data
        #[arg(short = 'x', long, default_value_t = 0, value_name = "nb of tiles", value_parser = parse_number::<usize>)]
        pub(super) trim_end: usize,
        /// Scan the image column by column, instead of row by row
        #[arg(short = 'Z', long)]
        pub(super) columns: bool,

        /// Path to the image to convert
        // TODO: should be required if any auto-path is requested, unless `-O` is set
        #[arg(value_name = "path to image")]
        pub(super) input: Option<PathBuf>,
    }

    #[derive(Debug, Clone)]
    pub(super) enum PalSpec {
        Inline(Vec<Vec<Rgb>>),
        Embedded,
        External { fmt: String, path: String },
    }

    fn parse_number<N: funty::Unsigned>(arg: &str) -> Result<N, std::num::ParseIntError> {
        if let Some(binary) = arg
            .strip_prefix('%')
            .or_else(|| arg.strip_prefix("0b"))
            .or_else(|| arg.strip_prefix("0B"))
        {
            N::from_str_radix(binary, 2)
        } else if let Some(hexadecimal) = arg
            .strip_prefix('$')
            .or_else(|| arg.strip_prefix("0x"))
            .or_else(|| arg.strip_prefix("0X"))
        {
            N::from_str_radix(hexadecimal, 16)
        } else {
            N::from_str_radix(arg, 10)
        }
    }

    #[derive(Debug, Clone)]
    pub(super) struct NumList<U: funty::Unsigned, const NB_MAX: usize>(ArrayVec<U, NB_MAX>);

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
                let number = parse_number(item)
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
                    parse_number(left)
                        .map_err(|err| format!("the {} number is invalid: {err}", Nth(idx)))?,
                    parse_number(right)
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
            } else if s.get(..1) == Some("#") {
                let colors = s
                    .trim_matches(|c: char| c.is_ascii_whitespace())
                    .split_terminator(&[';', ':'])
                    .map(|pal_str| {
                        pal_str
                            .trim_matches(|c: char| c.is_ascii_whitespace())
                            .split_terminator(',')
                            .map(Rgb::from_str)
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
                        let digit = chars.next().unwrap().to_digit(16).ok_or(
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
                    "in an inline palette spec, colors must be in `#rgb` or `#rrggbb` format"
                        .into(),
                ),
            }
        }
    }

    #[derive(Debug, Clone, Copy)]
    struct Nth(usize);
    impl Display for Nth {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            write!(
                f,
                "{}{}",
                self.0,
                match self.0 % 10 {
                    1 => "st",
                    2 => "nd",
                    3 => "rd",
                    _ => "th",
                }
            )
        }
    }

    impl Cli {
        pub(super) fn finish(self) -> Result<(Options, Option<super::PalSpec>), Diagnostic> {
            let pal_spec = self.colors.map(|pal_spec| match pal_spec {
                PalSpec::Inline(spec) => super::PalSpec::Explicit(spec),
                PalSpec::Embedded => super::PalSpec::Embedded,
                PalSpec::External { fmt, path } => todo!(),
            });

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
                    allow_dedup: self.unique_tiles,
                    column_major: self.columns,
                    bit_depth: self.depth,
                    input_slice: self.slice,
                    nb_palettes: self.nb_palettes,
                    // TODO: cap at bit depth
                    nb_colors_per_pal: self.palette_size.unwrap_or(1 << self.depth),
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
}
use cli::*;

fn main() -> ExitCode {
    // TODO: not convinced that `argfile` handles at-files the way we document...
    let args = argfile::expand_args_from(wild::args_os(), argfile::parse_fromfile, argfile::PREFIX)
        .expect("Failed to expand command-line args");
    let cli = Cli::parse_from(args);
    let mut reporter = Reporter::new(match &cli.color.color {
        concolor_clap::ColorChoice::Auto => codespan_reporting::term::termcolor::ColorChoice::Auto,
        concolor_clap::ColorChoice::Always => {
            codespan_reporting::term::termcolor::ColorChoice::Always
        }
        concolor_clap::ColorChoice::Never => {
            codespan_reporting::term::termcolor::ColorChoice::Never
        }
    });
    let (options, pal_spec) = match cli.finish() {
        Ok((opt, spec)) => (opt, spec),
        Err(diag) => {
            reporter.report(&diag);
            return ExitCode::FAILURE;
        }
    };

    // TODO: verbosity easter egg

    if let Err(diag) = run(options, pal_spec, &mut reporter) {
        reporter.report(&diag);
        return ExitCode::FAILURE;
    }

    ExitCode::SUCCESS
}

fn run(
    options: Options,
    pal_spec: Option<PalSpec>,
    reporter: &mut Reporter,
) -> Result<(), Diagnostic> {
    if let Some(width) = &options.reversed_width {
        todo!();
    } else if let Some(input_path) = &options.input_path {
        process::process(input_path, &options, pal_spec, reporter)
    } else if let (Some(palette_path), Some(pal_spec)) = (&options.palettes_path, pal_spec) {
        match pal_spec {
            PalSpec::Embedded => Err(todo!()),
            PalSpec::Explicit(pal_specs) => {
                process::process_palettes_only(&pal_specs, palette_path, &options)
            }
        }
    } else {
        Err(todo!())
    }
}

#[derive(Debug, Clone)]
struct Options {
    reversed_width: Option<NonZeroU16>,
    verbosity: u8,

    input_path: Option<PathBuf>,
    output_path: Option<PathBuf>,
    palettes_path: Option<PathBuf>,
    tilemap_path: Option<PathBuf>,
    attrmap_path: Option<PathBuf>,
    palmap_path: Option<PathBuf>,

    use_color_curve: bool,
    allow_mirroring: bool,
    allow_dedup: bool,
    column_major: bool,
    bit_depth: u8,
    input_slice: Option<InputSlice>,
    nb_palettes: u8,
    nb_colors_per_pal: u8,
    trim: usize,

    base_tile_ids: [u8; 2],
    max_nb_tiles: Option<[u8; 2]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum PalSpec {
    Embedded,
    Explicit(Vec<Vec<Rgb>>),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
struct InputSlice {
    left: u16,
    top: u16,
    /// In tiles.
    width: NonZeroU16,
    /// In tiles.
    height: NonZeroU16,
}

impl Options {
    pub fn colors_per_palette(&self, has_transparency: bool) -> u8 {
        if has_transparency {
            self.nb_colors_per_pal - 1
        } else {
            self.nb_colors_per_pal
        }
    }
}

impl InputSlice {
    fn iter_tiles<'frame, 'img: 'frame>(
        &self,
        frame: &'frame Frame<'img, Rgb32, DynImage32>,
        colum_major: bool,
    ) -> TileIter<'frame, 'img, '_> {
        TileIter::new(frame, self, colum_major)
    }
}

#[derive(Debug, Clone)]
struct Tile<'frame, 'img: 'frame> {
    frame: &'frame Frame<'img, Rgb32, DynImage32>,
    x: u16,
    y: u16,
}

#[derive(Debug, Clone)]
struct TileIter<'frame, 'img: 'frame, 'slice> {
    frame: &'frame Frame<'img, Rgb32, DynImage32>,
    slice: &'slice InputSlice,
    dx: u16,
    dy: u16,
    column_major: bool,
}

impl<'img> Tile<'_, 'img> {
    fn pixel(&self, x: u8, y: u8) -> Rgb32 {
        self.frame.pixel(x.into(), y.into())
    }
}

impl<'frame, 'img: 'frame, 'slice> TileIter<'frame, 'img, 'slice> {
    fn new(
        frame: &'frame Frame<'img, Rgb32, DynImage32>,
        slice: &'slice InputSlice,
        column_major: bool,
    ) -> Self {
        Self {
            frame,
            slice,
            dx: 0,
            dy: 0,
            column_major,
        }
    }
}

impl<'frame, 'img: 'frame> Iterator for TileIter<'frame, 'img, '_> {
    type Item = Tile<'frame, 'img>;

    fn next(&mut self) -> Option<Self::Item> {
        let (width, height) = (self.slice.width.get(), self.slice.height.get());
        let tile = Tile {
            frame: self.frame,
            x: self.dx * 8,
            y: self.dy * 8,
        };

        let coords = if self.column_major {
            (&mut self.dx, width, &mut self.dy, height)
        } else {
            (&mut self.dy, height, &mut self.dx, width)
        };

        if *coords.2 == coords.3 {
            return None;
        }

        *coords.0 += 1;
        if *coords.0 == coords.1 {
            *coords.0 = 0;
            *coords.2 += 1;
        }
        Some(tile)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn verify_cli() {
        use clap::CommandFactory;
        Cli::command().debug_assert()
    }
}
