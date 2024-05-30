use ariadne::{ReportKind, Source, Span};
use yansi::Color;

pub struct FilelessReports;
pub type Report = ariadne::Report<'static, FilelessReports>;
pub type ReportBuilder = ariadne::ReportBuilder<'static, FilelessReports>;

pub fn new_report(kind: ReportKind) -> ariadne::ReportBuilder<'_, FilelessReports> {
    Report::build(kind, (), 0)
}

pub const ERROR_KIND: ReportKind = ReportKind::Custom("error", Color::Red);

pub fn build_error() -> ReportBuilder {
    new_report(ERROR_KIND)
}

pub const WARNING_KIND: ReportKind = ReportKind::Custom("warning", Color::Yellow);

pub fn build_warning() -> ReportBuilder {
    new_report(WARNING_KIND)
}

pub trait ContentlessReport {
    fn eprint_(&self);
}

impl ContentlessReport for Report {
    fn eprint_(&self) {
        self.eprint(Source::from(""))
            .expect("Failed to print diagnostic")
    }
}

impl Span for FilelessReports {
    type SourceId = ();

    fn source(&self) -> &Self::SourceId {
        &()
    }

    fn start(&self) -> usize {
        0
    }

    fn end(&self) -> usize {
        0
    }
}
