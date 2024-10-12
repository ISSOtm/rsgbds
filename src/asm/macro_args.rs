use std::cell::Cell;

use compact_str::CompactString;

#[derive(Debug)]
pub struct MacroArgs {
    args: Vec<CompactString>,
    shift: Cell<usize>,
}

impl MacroArgs {
    pub fn new(args: Vec<CompactString>) -> Self {
        Self {
            args,
            shift: Cell::new(0),
        }
    }
}

impl AsRef<[CompactString]> for MacroArgs {
    fn as_ref(&self) -> &[CompactString] {
        &self.args[self.shift.get()..]
    }
}
