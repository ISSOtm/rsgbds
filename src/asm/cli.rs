/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

#![deny(missing_docs)]

use std::{fmt::Display, path::PathBuf, str::FromStr};

use clap::{ColorChoice, Parser};

use super::*;

/// The command-line interface.
#[derive(Debug, Parser)]
#[clap(color = crate::common::cli::clap_color_choice())]
#[command(
    name = "rgbasm",
    version = crate::common::build::PKG_VERSION,
    long_version = crate::common::build::CLAP_LONG_VERSION,
    about = "Game Boy assembler",
    long_about = "Assembles some Game Boy assembly into an object file, which can then be linked into a ROM.",
    after_help = "For comprehensive help, run `man rgbasm`, or go to http://rgbds.gbdev.io/docs/",
    arg_required_else_help = true,
    help_expected = true
)]
pub(super) struct Cli {
    /// The two characters to use for binary constants
    #[arg(short, long, default_value_t = Chars(['0', '1']), value_name = "chars")]
    binary_digits: Chars<2>,
    /// Controls when to use color
    #[arg(long, default_value_t = ColorChoice::Auto)]
    color: ColorChoice,
    /// Define a string symbol before assembling the source code
    #[arg(short = 'D', long, value_name = "definition")]
    defines: Vec<String>,
    /// Export all labels, even unreferenced and local ones
    #[arg(short, long)]
    export_all: bool,
    /// The four characters to use for character constants
    #[arg(short, long, default_value_t = Chars(['0', '1', '2', '3']), value_name = "chars")]
    gfx_chars: Chars<4>,
    /// Add a new include path
    #[arg(short = 'I', long, value_name = "path")]
    inc_paths: Vec<PathBuf>,
    /// Print Make-style dependencies to this file
    #[arg(short = 'M', long, value_name = "path")]
    dependfile: Option<PathBuf>,
    // TODO: `MG`, `MP`, `MT`, `MQ`
    /// Write an object file to this path
    #[arg(short, long, value_name = "path")]
    output: Option<PathBuf>,
    /// Include this file before starting to read the input
    #[arg(short = 'P', long, value_name = "path")]
    preinclude: Option<PathBuf>,
    /// Use this as the default byte for `ds`
    #[arg(short, long, default_value_t = 0, value_name = "byte", value_parser = crate::common::cli::parse_number::<u8>)]
    pad_value: u8,
    /// Use this as the default precision of fixed-point numbers
    #[arg(short = 'Q', long, default_value_t = 16, value_name = "precision", value_parser = parse_precision)]
    q_precision: u8,
    /// Recursion depth past which rgbasm will assume being in an infinite loop
    #[arg(short, long, default_value_t = 64, value_name = "max depth")]
    recursion_depth: usize,
    /// Enable or disable a warning
    #[arg(short = 'W', long, value_name = "flag")]
    warning: Vec<String>,
    /// Inhibit all warnings, even when turned into errors
    #[arg(short = 'w')]
    inhibit_warnings: bool,
    /// Abort if more than this many errors are generated
    #[arg(short = 'X', long, default_value_t = 64, value_name = "max")]
    max_errors: usize,

    /// Path to the file to assemble
    input: PathBuf,
}

// TODO: validate the range
fn parse_precision(arg: &str) -> Result<u8, std::num::ParseIntError> {
    arg.strip_prefix('.').unwrap_or(arg).parse()
}

#[derive(Debug, Clone)]
struct Chars<const N: usize>([char; N]);

impl<const N: usize> FromStr for Chars<N> {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        (|| {
            let mut c = s.chars();
            let mut chars = [char::default(); N];
            for (i, slot) in chars.iter_mut().enumerate() {
                *slot = c.next().ok_or(i)?;
            }

            let rest = c.count();
            if rest == 0 {
                Ok(Self(chars))
            } else {
                Err(N + rest)
            }
        })()
        .map_err(|got| format!("expected {N} characters, got {got}"))
    }
}

impl<const N: usize> Display for Chars<N> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for c in &self.0 {
            write!(f, "{c}")?;
        }
        Ok(())
    }
}

impl Cli {
    pub(super) fn finish(self) -> Result<(Options, PathBuf, Vec<String>, Vec<String>), ()> {
        crate::common::cli::apply_color_choice(self.color);

        let mut warnings = [WarningLevel::Default; NB_WARNINGS];
        // TODO: parse warning flags

        Ok((
            Options {
                binary_digits: self.binary_digits.0,
                export_all: self.export_all,
                gfx_chars: self.gfx_chars.0,
                inc_paths: self.inc_paths,
                dependfile: self.dependfile,
                output: self.output,
                preinclude: self.preinclude,
                pad_value: self.pad_value,
                q_precision: self.q_precision,
                recursion_depth: self.recursion_depth,
                inhibit_warnings: self.inhibit_warnings,
                warnings,
                max_errors: self.max_errors,
            },
            self.input,
            self.defines,
            self.warning,
        ))
    }
}
