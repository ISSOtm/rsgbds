/* SPDX-License-Identifier: MPL-2.0 */

#![deny(
    clippy::undocumented_unsafe_blocks,
    unsafe_op_in_unsafe_fn,
    unused_unsafe
)]
#![debugger_visualizer(gdb_script_file = "../../maintainer/gdb_pretty_printers.py")]

use clap::Parser;
use plumers::prelude::*;

use std::{
    fmt::{Debug, Display},
    io::Read,
    num::{NonZeroU16, NonZeroU8},
    path::{Path, PathBuf},
    process::ExitCode,
};

fn main() -> ExitCode {
    let args = rgbds::common::argfile::collect_expanded_args();
    let cli = Cli::parse_from(args);
    let mut reporter = Reporter::new(if concolor::get(concolor::Stream::Stderr).color() {
        codespan_reporting::term::termcolor::ColorChoice::Always
    } else {
        codespan_reporting::term::termcolor::ColorChoice::Never
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

mod color_set;
mod error;
pub use error::Diagnostic;
use error::Reporter;
mod pal_packing;
mod pal_spec;
mod palette;
mod process;
mod reverse;
mod rgb;
use rgb::Rgb;

fn run(
    options: Options,
    pal_spec: Option<PalSpec>,
    reporter: &mut Reporter,
) -> Result<(), Diagnostic> {
    if let Some(&width) = &options.reversed_width.as_ref() {
        reverse::reverse(width, &options, pal_spec, reporter)
    } else if let Some(input_path) = &options.input_path {
        process::process(input_path, &options, pal_spec, reporter)
    } else if let Some(palettes_path) = &options.palettes_path {
        match pal_spec.expect("The CLI should enforce either an input path or a pal spec!") {
            PalSpec::Embedded => Err(Diagnostic::error()
                .with_message("An embedded color spec cannot be used without an input image")),
            PalSpec::Explicit(pal_specs) => {
                process::process_palettes_only(&pal_specs, palettes_path, &options)
            }
        }
    } else {
        Err(Diagnostic::error().with_message(
            "To dump the color spec to a file, please specify that file's path with `-p`",
        ))
    }
}

mod cli;
use cli::*;

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

fn input_error<S: Into<String>, P: AsRef<Path>>(err_msg: S, path: P) -> Diagnostic {
    let path = path.as_ref();
    Diagnostic::error()
        .with_message(err_msg)
        .with_notes(vec![if path == Path::new("-") {
            "Reading from standard input".into()
        } else {
            format!("File path: {}", path.display())
        }])
}

fn output_error<S: Into<String>, P: AsRef<Path>>(err_msg: S, path: P) -> Diagnostic {
    let path = path.as_ref();
    Diagnostic::error()
        .with_message(err_msg)
        .with_notes(vec![if path == Path::new("-") {
            "Writing to standard output".into()
        } else {
            format!("File path: {}", path.display())
        }])
}

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
