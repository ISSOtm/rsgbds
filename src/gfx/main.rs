/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

#![deny(
    clippy::undocumented_unsafe_blocks,
    unsafe_op_in_unsafe_fn,
    unused_unsafe
)]
#![debugger_visualizer(gdb_script_file = "../../maintainer/gdb_pretty_printers.py")]

use clap::Parser;
use plumers::prelude::*;

use std::{
    fmt::Display,
    io::Read,
    num::{NonZeroU16, NonZeroU8},
    path::PathBuf,
    process::ExitCode,
};

pub use crate::common::diagnostics::{
    build_error, build_warning, ContentlessReport, Report, ReportBuilder,
};

fn main() -> ExitCode {
    crate::common::cli::setup_panic_handler();
    crate::common::cli::detect_default_color_choice();

    let args = crate::common::argfile::collect_expanded_args();
    let cli = Cli::parse_from(args); // This also calls `crate::common::cli::apply_color_choice`.
    let (options, pal_spec) = match cli.finish() {
        Ok((opt, spec)) => (opt, spec),
        Err(()) => {
            return ExitCode::FAILURE;
        }
    };

    // TODO: verbosity easter egg

    match run(options, pal_spec) {
        Ok(()) => ExitCode::SUCCESS,
        Err(()) => ExitCode::FAILURE,
    }
}

mod cli;
use cli::*;
mod color_set;
#[path = "../common/mod.rs"]
mod common;
mod pal_packing;
mod pal_spec;
mod palette;
mod process;
mod reverse;
mod rgb;
use rgb::Rgb;

fn run(options: Options, pal_spec: Option<PalSpec>) -> Result<(), ()> {
    if let Some(&width) = &options.reversed_width.as_ref() {
        reverse::reverse(width, &options, pal_spec)
    } else if let Some(input_path) = &options.input_path {
        process::process(input_path, &options, pal_spec)
    } else if let Some(palettes_path) = &options.palettes_path {
        match pal_spec.expect("The CLI should enforce either an input path or a pal spec!") {
            PalSpec::Embedded => {
                build_error()
                    .with_message("An embedded color spec cannot be used without an input image")
                    .finish()
                    .eprint_();
                Err(())
            }
            PalSpec::Explicit(pal_specs) => {
                process::process_palettes_only(&pal_specs, palettes_path, &options)
            }
        }
    } else {
        let mut builder = build_error().with_message("No operation was specified");
        if matches!(pal_spec, Some(PalSpec::Explicit(_))) {
            builder.set_help(
                "To dump the color spec to a file, please specify that file's path with `-p`",
            );
        }
        builder.finish().eprint_();
        Err(())
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
    nb_palettes: u16,
    nb_colors_per_pal: NonZeroU8,
    trim: usize,

    base_tile_ids: [u8; 2],
    max_nb_tiles: Option<[u16; 2]>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum PalSpec {
    Embedded,
    Explicit(Vec<Vec<Option<Rgb16>>>),
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
            self.nb_colors_per_pal.get() - 1
        } else {
            self.nb_colors_per_pal.get()
        }
    }
}

// Little convenience utilities.

fn try_reading<R: Read>(mut buf: &mut [u8], mut from: R) -> std::io::Result<Option<()>> {
    use std::io::ErrorKind;

    let mut partial_read = false;
    while !buf.is_empty() {
        match from.read(buf) {
            // Since the buffer is not empty, this can only signify EOF.
            Ok(0) => {
                if partial_read {
                    return Err(ErrorKind::UnexpectedEof.into());
                } else {
                    return Ok(None);
                }
            }
            Ok(n) => {
                buf = &mut buf[n..];
                partial_read = true;
            }
            Err(error) => {
                if !matches!(error.kind(), ErrorKind::Interrupted) {
                    return Err(error);
                }
            }
        }
    }

    Ok(Some(()))
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn verify_cli() {
        use clap::CommandFactory;
        Cli::command().debug_assert()
    }
}
