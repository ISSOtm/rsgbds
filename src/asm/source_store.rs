use std::{
    fs::File,
    io::Read,
    ops::{Index, Range},
    path::Path,
    sync::OnceLock,
};

use compact_str::{CompactString, ToCompactString};

pub type Report = ariadne::Report<'static, RawSpan>;
pub type ReportBuilder = ariadne::ReportBuilder<'static, RawSpan>;

/// Sources are *not* reference-counted.
/// This is because it is rare for files not to define anything, and thus be suitable for unloading.
#[derive(Debug)]
pub struct SourceStore(Vec<Source>);

#[derive(Debug)]
pub struct Source {
    name: CompactString,
    // TODO: maybe we shouldn't compute this eagerly?
    // TODO: consider using `mmap` when possible... at the cost of UB if the file is modified "beneath our feet", but maybe that's acceptable
    contents: ariadne::Source<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceHandle(usize);

#[derive(Debug, Clone)]
pub struct SourceSlice {
    handle: SourceHandle,
    bytes: Range<usize>,
}

impl SourceStore {
    pub fn new() -> Self {
        Self(Vec::new())
    }

    fn load<R: Read>(
        &mut self,
        mut input: R,
        name: CompactString,
    ) -> Result<SourceHandle, std::io::Error> {
        let handle = SourceHandle(self.0.len());

        let mut buf = String::new();
        input.read_to_string(&mut buf)?; // TODO: if performance suffers here, consider using the `Simdutf8` crate.
        self.0.push(Source {
            name,
            contents: buf.into(),
        });

        Ok(handle)
    }

    pub fn load_file(&mut self, path: &Path) -> Result<SourceHandle, std::io::Error> {
        let file = File::open(path)?;
        self.load(file, path.display().to_compact_string())
    }

    pub fn load_stdin(&mut self) -> Result<SourceHandle, std::io::Error> {
        self.load(
            std::io::stdin().lock(),
            "<standard input>".to_compact_string(),
        )
    }
}

impl Index<SourceHandle> for SourceStore {
    type Output = Source;

    fn index(&self, index: SourceHandle) -> &Self::Output {
        &self.0[index.0]
    }
}

impl Index<&SourceHandle> for SourceStore {
    type Output = Source;

    fn index(&self, index: &SourceHandle) -> &Self::Output {
        &self[*index]
    }
}

impl Source {
    pub fn name(&self) -> &CompactString {
        &self.name
    }
}

impl AsRef<str> for Source {
    fn as_ref(&self) -> &str {
        self.contents.text()
    }
}

impl SourceSlice {
    pub fn as_raw(&self) -> (&SourceHandle, &Range<usize>) {
        (&self.handle, &self.bytes)
    }
}

// This `Span` should not be used outside of diagnostic management.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct RawSpan {
    source: SourceHandle,
    byte_range: Range<usize>,
}

impl SourceHandle {
    // Some special definition sites.
    // We'll panic from the `Vec` growing larger than `isize::MAX` before either of these IDs is reused.
    pub const COMMAND_LINE: SourceHandle = SourceHandle(usize::MAX);
    pub const BUILTIN: SourceHandle = SourceHandle(usize::MAX - 1);
}

impl From<(SourceHandle, Range<usize>)> for RawSpan {
    fn from((source, byte_range): (SourceHandle, Range<usize>)) -> Self {
        Self { source, byte_range }
    }
}

// Let `SourceStore` be used for diagnostic printing.
impl ariadne::Cache<SourceHandle> for &SourceStore {
    type Storage = String;

    fn fetch(
        &mut self,
        id: &SourceHandle,
    ) -> Result<&ariadne::Source<Self::Storage>, Box<dyn std::fmt::Debug + '_>> {
        if *id == SourceHandle::BUILTIN || *id == SourceHandle::COMMAND_LINE {
            static EMPTY_SOURCE: OnceLock<ariadne::Source<String>> = OnceLock::new();

            Ok(EMPTY_SOURCE.get_or_init(|| ariadne::Source::from(String::new())))
        } else {
            // This should never fail. We (should) never hand out bad handles.
            Ok(&self[id].contents)
        }
    }

    fn display<'a>(&self, id: &'a SourceHandle) -> Option<Box<dyn std::fmt::Display + 'a>> {
        use ariadne::Fmt;

        if *id == SourceHandle::BUILTIN {
            Some(Box::new("<built-in>".fg(ariadne::Color::BrightBlack)))
        } else if *id == SourceHandle::COMMAND_LINE {
            Some(Box::new("<command line>".fg(ariadne::Color::BrightBlack)))
        } else {
            let source = &self[id];
            // FIXME: can't return data borrowing from `self` due to method signature, this sucks :/
            Some(Box::new(source.name.clone()))
        }
    }
}

// Let `Span` be used for diagnostic printing.
impl ariadne::Span for RawSpan {
    type SourceId = SourceHandle;

    fn source(&self) -> &Self::SourceId {
        &self.source
    }

    fn start(&self) -> usize {
        self.byte_range.start
    }

    fn end(&self) -> usize {
        self.byte_range.end
    }
}
