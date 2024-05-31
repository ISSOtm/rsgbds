/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

use std::{
    fs::File,
    io::{BufRead, BufReader, Read, StdinLock, Write},
    path::Path,
};

use ariadne::{Cache, Label, Report, ReportBuilder, Source, Span};

use super::diagnostics::{ContentlessReport, ERROR_KIND};

pub enum Input<'path, 'stdin> {
    File(BufReader<File>, &'path Path),
    Stdin(StdinLock<'stdin>),
}

pub enum Output<'path> {
    File(File, &'path Path),
    Stdout,
}

pub fn read<P: AsRef<Path>>(path: P) -> std::io::Result<Vec<u8>> {
    let mut buf = Vec::new();
    Input::new(path.as_ref())?.read_to_end(&mut buf)?;
    Ok(buf)
}

impl<'path> Input<'path, '_> {
    pub fn new(path: &'path Path) -> std::io::Result<Self> {
        Ok(if path == Path::new("-") {
            Self::Stdin(std::io::stdin().lock())
        } else {
            Self::File(BufReader::new(File::open(path)?), path)
        })
    }

    pub fn error_in<M: ToString>(&self, err_msg: M) -> ReportBuilder<BinFileDiag> {
        Report::build(ERROR_KIND, "", 0)
            .with_message(err_msg)
            .with_label(Label::new(BinFileDiag(match self {
                Input::File(_, path) => path.display().to_string(),
                Input::Stdin(_) => "<standard input>".into(),
            })))
    }

    pub fn error<M: ToString>(path: &Path, err_msg: M) -> ReportBuilder<BinFileDiag> {
        Report::build(ERROR_KIND, "", 0)
            .with_message(err_msg)
            .with_label(Label::new(BinFileDiag(if path == Path::new("-") {
                "<standard input>".into()
            } else {
                path.display().to_string()
            })))
    }
}

impl<'path> Output<'path> {
    pub fn new(path: &'path Path) -> std::io::Result<Self> {
        Ok(if path == Path::new("-") {
            Self::Stdout
        } else {
            Self::File(File::create(path)?, path)
        })
    }

    pub fn error_in<M: ToString>(&self, err_msg: M) -> ReportBuilder<BinFileDiag> {
        Report::build(ERROR_KIND, "", 0)
            .with_message(err_msg)
            .with_label(Label::new(BinFileDiag(match self {
                Output::File(_, path) => path.display().to_string(),
                Output::Stdout => "<standard output>".into(),
            })))
    }

    pub fn error<M: ToString>(path: &Path, err_msg: M) -> ReportBuilder<BinFileDiag> {
        Report::build(ERROR_KIND, "", 0)
            .with_message(err_msg)
            .with_label(Label::new(BinFileDiag(if path == Path::new("-") {
                "<standard output>".into()
            } else {
                path.display().to_string()
            })))
    }
}

#[derive(Debug, Clone)]
pub struct BinFileDiag(String);

impl Span for BinFileDiag {
    type SourceId = String;

    fn source(&self) -> &Self::SourceId {
        &self.0
    }

    fn start(&self) -> usize {
        0
    }

    fn end(&self) -> usize {
        0
    }
}

#[derive(Debug, Clone)]
struct BinFileName(Source<&'static str>);

impl Cache<String> for BinFileName {
    type Storage = &'static str;

    fn fetch(
        &mut self,
        _id: &String,
    ) -> Result<&Source<Self::Storage>, Box<dyn std::fmt::Debug + '_>> {
        Ok(&self.0)
    }

    fn display<'a>(&self, id: &'a String) -> Option<Box<dyn std::fmt::Display + 'a>> {
        Some(Box::new(id))
    }
}

impl ContentlessReport for Report<'_, BinFileDiag> {
    fn eprint_(&self) {
        // An empty source elides all labels and note+help messages :/
        self.eprint(BinFileName(Source::from(" ")))
            .expect("Failed to print diagnostic")
    }
}

macro_rules! forward {
    ($Stdio:pat => $io:expr, $(fn $name:ident $params:tt $( -> $ret:ty )?;)*) => {$(
        forward_one!($Stdio, $io, $name, $params, $($ret)?);
    )*};
}

macro_rules! forward_one {
    ($Stdio:pat, $io:expr, $name:ident, (&self $(
        , $param_name:ident: $param_ty:ty
    )*), $($ret:ty)?) => {
        fn $name(&self $(, $param_name: $param_ty)*) $( -> $ret )? {
            match self {
                $Stdio => $io.$name($($param_name),*),
                Self::File(file, _) => file.$name($($param_name),*),
            }
        }
    };
    ($Stdio:pat, $io:expr, $name:ident, (&mut self $(
        , $param_name:ident: $param_ty:ty
    )*), $($ret:ty)?) => {
        fn $name(&mut self $(, $param_name: $param_ty)*) $( -> $ret )? {
            match self {
                $Stdio => $io.$name($($param_name),*),
                Self::File(file, _) => file.$name($($param_name),*),
            }
        }
    };
}

impl Read for Input<'_, '_> {
    forward! { Self::Stdin(lock) => lock,
        fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize>;
        fn read_vectored(&mut self, bufs: &mut [std::io::IoSliceMut<'_>]) -> std::io::Result<usize>;
        fn read_to_end(&mut self, buf: &mut Vec<u8>) -> std::io::Result<usize>;
        fn read_to_string(&mut self, buf: &mut String) -> std::io::Result<usize>;
        fn read_exact(&mut self, buf: &mut [u8]) -> std::io::Result<()>;
    }
}

impl BufRead for Input<'_, '_> {
    forward! { Self::Stdin(lock) => lock,
        fn fill_buf(&mut self) -> std::io::Result<&[u8]>;
        fn consume(&mut self, amt: usize);
        fn read_until(&mut self, byte: u8, buf: &mut Vec<u8>) -> std::io::Result<usize>;
        fn read_line(&mut self, buf: &mut String) -> std::io::Result<usize>;
    }
}

impl Write for Output<'_> {
    forward! { Self::Stdout => std::io::stdout(),
        fn write(&mut self, buf: &[u8]) -> std::io::Result<usize>;
        fn flush(&mut self) -> std::io::Result<()>;
        fn write_all(&mut self, buf: &[u8]) -> std::io::Result<()>;
        fn write_fmt(&mut self, fmt: std::fmt::Arguments<'_>) -> std::io::Result<()>;
    }
}
