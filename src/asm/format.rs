use compact_str::CompactString;

#[derive(Debug)]
pub struct FormatSpec;

impl FormatSpec {
    pub fn new() -> Self {
        Self // TODO
    }

    pub fn is_finished(&self) -> bool {
        todo!()
    }

    pub fn write_str(&self, s: &str, buf: &mut CompactString) {
        // TODO: format flag checks
        buf.push_str(s);
    }
}
