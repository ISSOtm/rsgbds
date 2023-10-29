/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

use std::{
    borrow::Cow,
    cell::Cell,
    fmt::Debug,
    fs::File,
    io::{self, Read},
    ops::{Deref, Range},
};

use memmap2::{Mmap, MmapOptions};

pub struct Storage {
    name: String,
    bytes: StorageKind,
    /// Cache for error reporting.
    /// Indices of the lines' starts within the error-reporting string.
    /// If this is `None`, the cache hasn't been initialised yet.
    line_starts: Cell<Option<Vec<usize>>>,
}

#[derive(Debug)]
pub enum StorageKind {
    /// The file is mapped straight into memory, to avoid copying.
    Mapped(Mmap),
    /// The file is read into memory, usually because mapping failed.
    Read(String),
}

impl Storage {
    fn new(name: String, bytes: StorageKind) -> Self {
        Self {
            bytes,
            name,
            line_starts: Cell::new(None),
        }
    }

    pub fn from_file(name: String, file: &File) -> io::Result<Self> {
        // FIXME: we will get a lot of UB if the file changes under us and yields some invalid UTF-8...
        // TODO: compare the performance with and without
        match unsafe { MmapOptions::new().populate().map(file) } {
            Ok(mapping) => Ok(match String::from_utf8_lossy(&mapping) {
                Cow::Borrowed(_) => Self::new(name, StorageKind::Mapped(mapping)),
                Cow::Owned(string) => Self::new(name, StorageKind::Read(string)),
            }),
            Err(_) => Self::from_readable(name, file),
        }
    }

    pub fn from_readable<R: Read>(name: String, mut src: R) -> io::Result<Self> {
        let mut data = Vec::new();
        src.read_to_end(&mut data)?;
        Ok(Self::new(
            name,
            StorageKind::Read(match String::from_utf8_lossy(&data) {
                Cow::Borrowed(_) => unsafe { String::from_utf8_unchecked(data) },
                Cow::Owned(string) => string,
            }),
        ))
    }

    fn with_line_starts<T, F: FnOnce(&Vec<usize>) -> T>(&self, f: F) -> T {
        let line_starts = self.line_starts.take().unwrap_or_else(|| {
            // There is no cache, so we must initialise it.
            codespan_reporting::files::line_starts(self.as_ref()).collect()
        });
        let ret = f(&line_starts);
        self.line_starts.set(Some(line_starts));
        ret
    }

    pub fn name(&self) -> &str {
        &self.name
    }

    pub(crate) fn line_start(&self, byte_index: usize) -> Result<usize, usize> {
        if byte_index < self.bytes.len() {
            Ok(
                self.with_line_starts(|line_starts| match line_starts.binary_search(&byte_index) {
                    Ok(line) => line,
                    Err(next_line) => next_line - 1,
                }),
            )
        } else {
            Err(self.bytes.len())
        }
    }

    pub(crate) fn line_range(&self, line_index: usize) -> Result<Range<usize>, usize> {
        self.with_line_starts(|line_starts| {
            line_starts
                .get(line_index)
                .cloned()
                .ok_or(line_starts.len())
                .map(|start| {
                    start
                        ..line_starts
                            .get(line_index + 1)
                            .cloned()
                            .unwrap_or(self.bytes.len())
                })
        })
    }
}

impl Debug for Storage {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let line_starts = self.line_starts.take();
        let res = write!(
            f,
            "Storage {{ name: {:?}, bytes: {:?}, line_starts: {:?} }}",
            &self.name, &self.bytes, &line_starts
        );
        self.line_starts.set(line_starts);
        res
    }
}

impl AsRef<str> for Storage {
    fn as_ref(&self) -> &str {
        self.bytes.deref()
    }
}

impl Deref for StorageKind {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        match self {
            Self::Mapped(mapping) => unsafe { std::str::from_utf8_unchecked(mapping) },
            Self::Read(string) => string.deref(),
        }
    }
}
