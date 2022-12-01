use std::{
    borrow::Cow,
    cell::Cell,
    debug_assert,
    fmt::{Debug, Display},
    fs::File,
    io::{self, Read},
    ops::{Deref, Range},
    rc::Rc,
};

use memmap2::{Mmap, MmapOptions};

pub struct Storage {
    name: SourceString,
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
    fn new(name: SourceString, bytes: StorageKind) -> Self {
        Self {
            bytes,
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
            codespan_reporting::files::line_starts(self.as_ref()).collect()
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

// We really don't want other pieces of code to have *any* visibility to the internals (and `enum`s
/// cannot have their variants' visibility overridden).
#[derive(Debug, Clone)]
pub struct SourceString(SourceStringImpl);

#[derive(Debug, Clone)]
enum SourceStringImpl {
    Borrowed {
        storage: Rc<Storage>,
        range: Range<usize>,
    },
    Owned(String),
}

impl SourceString {
    /// Creates a new, empty `SourceString`.
    /// This does not allocate.
    pub fn new() -> Self {
        Self(SourceStringImpl::Owned(String::new())) // An empty `String` does not allocate.
    }

    /// Creates a new `SourceString` covering a certain range of source code.
    pub fn from_storage(storage: Rc<Storage>, range: Range<usize>) -> Self {
        Self(SourceStringImpl::Borrowed { storage, range })
    }

    /// Creates a new empty `SourceString`, but starting from the same source location as `other`
    /// if applicable.
    pub fn clone_empty(other: &Self, offset: usize) -> Self {
        Self(match &other.0 {
            SourceStringImpl::Borrowed { storage, range } => {
                let start = range.start + offset;
                SourceStringImpl::Borrowed {
                    storage: Rc::clone(storage),
                    range: start..start,
                }
            }
            SourceStringImpl::Owned(_) => SourceStringImpl::Owned(String::new()),
        })
    }

    /// Creates a new `SourceString` containing a slice of the original.
    pub fn new_sliced(orig: &Self, mut sub_range: Range<usize>) -> Self {
        match &orig.0 {
            SourceStringImpl::Borrowed { storage, range } => {
                debug_assert!(sub_range.len() <= range.len());
                sub_range.start += range.start;
                sub_range.end += range.start;
                Self(SourceStringImpl::Borrowed {
                    storage: Rc::clone(storage),
                    range: sub_range,
                })
            }
            SourceStringImpl::Owned(string) => {
                Self(SourceStringImpl::Owned(string[sub_range].into()))
            }
        }
    }

    /// Appends a character to the string; if it is directly referencing the source code, `ch` must
    /// match the next character in the source code.
    pub fn push(this: &mut Self, ch: char) {
        match &mut this.0 {
            SourceStringImpl::Borrowed { storage, range } => {
                debug_assert_eq!(storage.bytes[range.end..].chars().next(), Some(ch));
                range.end += ch.len_utf8();
            }
            SourceStringImpl::Owned(string) => string.push(ch),
        }
    }

    /// Appends a `SourceString`'s contents to this one.
    /// This string will be made "owned", as it is assumed `other` isn't contiguous.
    pub fn concat(this: &mut Self, other: &Self) {
        Self::make_owned(this).push_str(other);
    }

    /// "Unlink" the string from the source code.
    /// In particular, this allows [`push()`][Self::push()] to push characters that don't match the
    /// source code.
    pub fn make_owned(this: &mut Self) -> &mut String {
        take_mut::take(&mut this.0, |imp| match imp {
            SourceStringImpl::Borrowed { storage, range } => {
                SourceStringImpl::Owned(storage.bytes[range].to_owned())
            }
            inner @ SourceStringImpl::Owned(_) => inner,
        });
        let SourceStringImpl::Owned(string) = &mut this.0 else { unreachable!(); };
        string
    }

    pub fn is_owned(this: &mut Self) -> bool {
        matches!(this.0, SourceStringImpl::Owned(_))
    }

    pub fn trim_end(this: &mut Self, how_much: usize) {
        match &mut this.0 {
            SourceStringImpl::Borrowed { range, .. } => range.end -= how_much,
            SourceStringImpl::Owned(string) => string.truncate(string.len() - how_much),
        }
    }

    pub fn truncate(this: &mut Self, new_len: usize) {
        match &mut this.0 {
            SourceStringImpl::Borrowed { range, .. } => range.end = range.start + new_len,
            SourceStringImpl::Owned(string) => string.truncate(new_len),
        }
    }

    pub fn is_empty(this: &mut Self) -> bool {
        match &this.0 {
            SourceStringImpl::Borrowed { range, .. } => range.is_empty(),
            SourceStringImpl::Owned(string) => string.is_empty(),
        }
    }

    pub fn storage(this: &Self) -> Option<&Rc<Storage>> {
        match &this.0 {
            SourceStringImpl::Borrowed { storage, .. } => Some(storage),
            SourceStringImpl::Owned(..) => None,
        }
    }

    pub fn storage_base_ofs(this: &Self) -> Option<usize> {
        match &this.0 {
            SourceStringImpl::Borrowed { range, .. } => Some(range.start),
            SourceStringImpl::Owned(..) => None,
        }
    }
}

impl<T: Into<String>> From<T> for SourceString {
    fn from(into_string: T) -> Self {
        Self(SourceStringImpl::Owned(into_string.into()))
    }
}

impl Deref for SourceString {
    type Target = str;

    fn deref(&self) -> &str {
        match &self.0 {
            SourceStringImpl::Borrowed { storage, range } => &storage.bytes[range.clone()],
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
