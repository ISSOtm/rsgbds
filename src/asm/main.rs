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

use std::{
    cell::Cell,
    fs::File,
    path::{Path, PathBuf},
};

use compact_str::CompactString;
use rustc_hash::FxBuildHasher;
use string_interner::StringInterner;
use sysexits::ExitCode;

fn main() -> ExitCode {
    // `Cli::finish()` also calls `crate::common::cli::apply_color_choice`.
    let (options, input_path, defines, warnings) =
        match crate::common::cli::setup_and_parse_args().and_then(Cli::finish) {
            Ok(cli) => cli,
            Err(()) => return ExitCode::Usage,
        };

    run(options, input_path, defines)
        .err()
        .unwrap_or(ExitCode::Ok)
}

mod cli;
use cli::*;
#[path = "../common/mod.rs"]
mod common;
mod context_stack;
use context_stack::ContextStack;
mod diagnostics;
use diagnostics::{WarningLevel, NB_WARNINGS};
mod format;
mod lexer;
mod macro_args;
mod parser;
mod source_store;
use source_store::{SourceHandle, SourceStore};
mod symbols;
use symbols::Symbols;

use crate::context_stack::Span;
mod tokens;

#[derive(Debug, Clone)]
pub struct Options {
    binary_digits: [char; 2],
    export_all: bool,
    gfx_chars: [char; 4],
    inc_paths: Vec<PathBuf>,
    dependfile: Option<PathBuf>,
    // TODO: `MG`, `MP`, `MT`, `MQ`
    output: Option<PathBuf>,
    preinclude: Option<PathBuf>,
    pad_value: u8,
    q_precision: u8,
    recursion_depth: usize,
    inhibit_warnings: bool,
    // TODO: use some bitfield(s) instead?
    warnings: [WarningLevel; NB_WARNINGS],
    max_errors: usize,
}

fn run(options: Options, input_path: PathBuf, defines: Vec<String>) -> Result<(), ExitCode> {
    let mut sources = SourceStore::new();
    let mut context_stack = ContextStack::new();
    let remaining_errors = Cell::new(options.max_errors);

    let mut symbols = Symbols::new();
    for mut define in defines {
        let (name, value): (_, CompactString) = match define.split_once('=') {
            Some((name, _value)) => (symbols.intern_name(name), {
                // Reuse the string's buffer for the `CompactString`.
                define.drain(..=name.len()); // The extra char is the `=` sign.
                define.into()
            }),
            None => (symbols.intern_name(define), CompactString::const_new("1")),
        };
        symbols.define_string_interned(name, Span::COMMAND_LINE, value);
    }

    if let Some(preinc_path) = &options.preinclude {
        match options.load_file(preinc_path, &mut sources) {
            Err(err) => diagnostics::error(
                Span::COMMAND_LINE,
                |report| report.with_message(format!("Failed to open preinclude file: {err}")),
                &sources,
                &remaining_errors,
                &options,
            ), // Try to keep going even after this failure.
            Ok(handle) => {
                parser::parse_file(
                    &mut context_stack,
                    &sources,
                    handle,
                    &mut symbols,
                    &remaining_errors,
                    &options,
                );
            }
        }
    }

    let res = if input_path == AsRef::<Path>::as_ref("-") {
        sources.load_stdin()
    } else {
        sources.load_file(&input_path)
    };
    let handle = match res {
        Ok(handle) => handle,
        Err(err) => {
            diagnostics::error(
                Span::COMMAND_LINE,
                |report| report.with_message(format!("Failed to open input file: {err}")),
                &sources,
                &remaining_errors,
                &options,
            );
            return Err(ExitCode::NoInput);
        }
    };
    parser::parse_file(
        &mut context_stack,
        &sources,
        handle,
        &mut symbols,
        &remaining_errors,
        &options,
    );

    if remaining_errors.get() == options.max_errors {
        todo!();
    } else {
        Err(ExitCode::DataErr)
    }
}

impl Options {
    /// Looks up a file along all defined search paths ("include paths"), and loads it into the
    /// [`SourceStore`].
    ///
    /// # Caveats
    ///
    /// Unfortunately, this is vulnerable to a TOCTTOU condition, since the file is opened *after*
    /// this checking is done, and thus the target could be changed in the meantime.
    ///
    /// Ideally, we'd avoid that by attempting to use the `File` and checking that
    /// `matches!(err.kind(), NotFound | IsADirectory)`, *but* the latter is not stable as of our MSRV.
    fn load_file(
        &self,
        path: &Path,
        sources: &mut SourceStore,
    ) -> Result<SourceHandle, std::io::Error> {
        if path.is_file() {
            return sources.load_file(path);
        }

        let mut buf = PathBuf::new();
        for inc_dir in &self.inc_paths {
            buf.clear();
            buf.push(inc_dir);
            buf.push(path);

            if buf.is_file() {
                return sources.load_file(&buf);
            }
        }

        Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            "File not found under any include path",
        ))
    }
}
