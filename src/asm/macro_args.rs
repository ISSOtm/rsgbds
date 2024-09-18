use std::cell::Cell;

use compact_str::CompactString;

#[derive(Debug)]
pub struct MacroArgs {
    args: Vec<CompactString>,
    shift: Cell<usize>,
}

impl AsRef<[CompactString]> for MacroArgs {
    fn as_ref(&self) -> &[CompactString] {
        &self.args[self.shift.get()..]
    }
}
