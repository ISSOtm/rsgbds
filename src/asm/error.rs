use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    term::{
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};

use crate::{
    fstack::Fstack,
    language::{AsmError, AsmErrorKind, ParseError},
};

pub struct Reporter {
    writer: StandardStream,
    config: Config,
}

impl Reporter {
    pub fn new(color_choice: ColorChoice) -> Self {
        Self {
            writer: StandardStream::stderr(color_choice),
            config: Default::default(),
        }
    }

    fn report(&mut self, fstack: &Fstack, diagnostic: &Diagnostic<usize>) {
        if let Err(error) = codespan_reporting::term::emit(
            &mut self.writer,
            &self.config,
            &fstack.get_files(),
            diagnostic,
        ) {
            panic!("Error writing diagnostic: {}", error);
        }
    }

    pub fn report_error(&mut self, fstack: &Fstack, error: ParseError) {
        let (begin, end, kind) = match error {
            lalrpop_util::ParseError::InvalidToken { location } => {
                (location, None, AsmErrorKind::InvalidToken)
            }
            lalrpop_util::ParseError::UnrecognizedEOF { location, expected } => {
                (location, None, AsmErrorKind::UnrecognizedEOF(expected))
            }
            lalrpop_util::ParseError::UnrecognizedToken { token, expected } => (
                token.0,
                Some(token.2),
                AsmErrorKind::UnrecognizedToken(token.1, expected),
            ),
            lalrpop_util::ParseError::ExtraToken { token } => {
                (token.0, Some(token.2), AsmErrorKind::ExtraToken(token.1))
            }
            lalrpop_util::ParseError::User {
                error: AsmError { begin, end, kind },
            } => (begin, Some(end), kind),
        };
        let (file_id, range) = Fstack::make_diag_info(&begin, end.as_ref());
        let mut labels = vec![Label::primary(file_id, range)];
        kind.labels(&mut labels);
        let diagnostic = Diagnostic::error()
            .with_labels(labels)
            .with_message(kind.to_string()) // TODO: ew!
            .with_notes(kind.notes());
        self.report(fstack, &diagnostic);
    }
}

#[derive(Debug)]
pub(super) enum WarningType {
    /// Assertions
    Assert,
    /// `for` loop with backwards range
    BackwardsFor,
    /// Invalid args to builtins
    BuiltinArg,
    /// Charmap entry re-definition
    CharmapRedef,
    /// Division undefined behavior
    Div,
    /// `db`, `dw` or `dl` directive without data in ROM
    EmptyDataDirective,
    /// Empty macro argument
    EmptyMacroArg,
    /// Empty second argument in `STRRPL`
    EmptyStrrpl,
    /// Constants too large
    LargeConstant,
    /// String too long for internal buffers
    LongStr,
    /// Shift past available arguments in macro
    MacroShift,
    /// Comment-start delimiter in a block comment
    NestedComment,
    /// Obsolete things
    Obsolete,
    /// Shifting undefined behavior
    Shift,
    /// Strange shift amount
    ShiftAmount,
    /// User warnings
    User,
}
