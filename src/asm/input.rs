use std::{
    borrow::Cow,
    cell::Cell,
    fmt::{Debug, Display},
    fs::File,
    io::{self, Read},
    ops::{Deref, Range},
    rc::Rc,
};

use memmap2::{Mmap, MmapOptions};

pub struct Storage {
    name: SourceString,
    bytes: Rc<StorageKind>,
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
    fn new(name: SourceString, kind: StorageKind) -> Self {
        Self {
            bytes: Rc::new(kind),
            name,
            line_starts: Cell::new(None),
        }
    }

    pub fn from_file(name: SourceString, file: &File) -> io::Result<Self> {
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

    pub fn from_readable<R: Read>(name: SourceString, mut src: R) -> io::Result<Self> {
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
            codespan_reporting::files::line_starts(self.deref()).collect()
        });
        let ret = f(&line_starts);
        self.line_starts.set(Some(line_starts));
        ret
    }

    pub fn name(&self) -> &SourceString {
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

    pub fn slice(&self, range: Range<usize>) -> SourceString {
        SourceString::from_storage(Rc::clone(&self.bytes), range)
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

impl Deref for Storage {
    type Target = <StorageKind as Deref>::Target;

    fn deref(&self) -> &Self::Target {
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

// We really don't want
#[derive(Debug, Clone)]
pub struct SourceString(SourceStringImpl);

#[derive(Debug, Clone)]
enum SourceStringImpl {
    Borrowed {
        storage: Rc<StorageKind>,
        range: Range<usize>,
    },
    Owned(String),
}

impl SourceString {
    pub fn from_storage(storage: Rc<StorageKind>, range: Range<usize>) -> Self {
        Self(SourceStringImpl::Borrowed { storage, range })
    }

    pub fn clone_empty(other: &Self) -> Self {
        Self(match &other.0 {
            SourceStringImpl::Borrowed { storage, range } => SourceStringImpl::Borrowed {
                storage: Rc::clone(storage),
                range: range.start..range.start,
            },
            SourceStringImpl::Owned(_) => SourceStringImpl::Owned(String::new()),
        })
    }

    pub fn push(this: &mut Self, ch: char) {
        match &mut this.0 {
            SourceStringImpl::Borrowed { storage, range } => {
                debug_assert_eq!(storage[range.end..].chars().next(), Some(ch));
                range.end += ch.len_utf8();
            }
            SourceStringImpl::Owned(string) => string.push(ch),
        }
    }

    pub fn make_owned(this: &mut Self) {
        take_mut::take(&mut this.0, |imp| match imp {
            SourceStringImpl::Borrowed { storage, range } => {
                SourceStringImpl::Owned(storage[range].to_owned())
            }
            SourceStringImpl::Owned(string) => SourceStringImpl::Owned(string),
        });
    }

    pub fn is_owned(this: &mut Self) -> bool {
        matches!(this.0, SourceStringImpl::Owned(_))
    }
}

impl From<String> for SourceString {
    fn from(string: String) -> Self {
        Self(SourceStringImpl::Owned(string))
    }
}

impl Deref for SourceString {
    type Target = str;

    fn deref(&self) -> &str {
        match &self.0 {
            SourceStringImpl::Borrowed { storage, range } => &storage[range.clone()],
            SourceStringImpl::Owned(string) => string.as_str(),
        }
    }
}

impl AsRef<str> for SourceString {
    fn as_ref(&self) -> &str {
        self.deref()
    }
}

impl Display for SourceString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.deref(), f)
    }
}
