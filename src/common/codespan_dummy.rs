struct Dummy;

impl codespan_reporting::files::Files<'static> for Dummy {
    type FileId = ();
    type Name = &'static str;
    type Source = &'static str;

    fn name(
        &'static self,
        _id: Self::FileId,
    ) -> Result<Self::Name, codespan_reporting::files::Error> {
        Ok("")
    }

    fn source(
        &'static self,
        _id: Self::FileId,
    ) -> Result<Self::Name, codespan_reporting::files::Error> {
        Ok("")
    }

    fn line_index(
        &'static self,
        _id: Self::FileId,
        _byte_index: usize,
    ) -> Result<usize, codespan_reporting::files::Error> {
        Ok(0)
    }

    fn line_range(
        &'static self,
        _id: Self::FileId,
        _line_index: usize,
    ) -> Result<std::ops::Range<usize>, codespan_reporting::files::Error> {
        Ok(0..0)
    }
}
