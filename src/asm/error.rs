use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    term::{
        termcolor::{ColorChoice, StandardStream},
        Config,
    },
};

use crate::{
    fstack::Fstack,
    language::{AsmError, AsmErrorKind, Location, ParseError, Warning, WarningId, WarningKind},
};

#[derive(Debug, Clone, Copy)]
pub enum WarningState {
    Default,
    Disabled,
    Enabled,
    Error,
}

#[derive(Debug)]
pub struct Reporter {
    writer: StandardStream,
    config: Config,

    warning_levels: [WarningState; WarningId::NB_WARNINGS],
    warnings_are_errors: bool,
}

impl Reporter {
    pub fn new(color_choice: ColorChoice) -> Self {
        let mut config = Config::default();
        // The defaults have poor contrast.
        config.styles.primary_label_bug.set_intense(true);
        config.styles.primary_label_error.set_intense(true);
        config.styles.primary_label_warning.set_intense(true);
        config.styles.primary_label_note.set_intense(true);
        config.styles.primary_label_help.set_intense(true);
        config.styles.secondary_label.set_intense(true);
        config.styles.line_number.set_intense(true);
        Self {
            writer: StandardStream::stderr(color_choice),
            config,
            warning_levels: [WarningState::Default; WarningId::NB_WARNINGS],
            warnings_are_errors: false,
        }
    }

    fn extract_error_info(error: ParseError) -> (Location, Option<Location>, AsmErrorKind) {
        match error {
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
        }
    }

    fn make_primary_label(begin: &Location, end: Option<&Location>) -> Vec<Label<usize>> {
        Fstack::make_diag_info(begin, end)
            .map(|(file_id, range)| Label::primary(file_id, range))
            .into_iter()
            .collect()
    }

    fn make_warning_labels(
        begin: &Location,
        end: Option<&Location>,
        kind: &WarningKind,
    ) -> Vec<Label<usize>> {
        let mut labels = Self::make_primary_label(begin, end);
        kind.labels(&mut labels);
        labels
    }

    fn make_error_labels(
        begin: &Location,
        end: Option<&Location>,
        kind: &AsmErrorKind,
    ) -> Vec<Label<usize>> {
        let mut labels = Self::make_primary_label(begin, end);
        kind.labels(&mut labels);
        labels
    }

    fn report(&mut self, fstack: &Fstack, diagnostic: &Diagnostic<usize>) {
        if let Err(err) = codespan_reporting::term::emit(
            &mut self.writer,
            &self.config,
            &fstack.get_files(),
            diagnostic,
        ) {
            eprintln!("Internal error when writing diagnostic: {err}");
        }
    }

    pub fn warn(&mut self, fstack: &Fstack, warning: Warning) {
        let id = WarningId::from(&warning.kind);

        // Determine what to do based on configured warning levels.
        let is_error = match self.warning_levels[id as usize] {
            WarningState::Disabled => return,
            WarningState::Default if !WarningId::DEFAULTS[id as usize] => return,

            WarningState::Error => true,

            // `Default` only reaches here if the default state is "enabled".
            WarningState::Enabled | WarningState::Default => self.warnings_are_errors,
        };

        let diagnostic = if is_error {
            Diagnostic::error().with_code(format!("-Werror={id}"))
        } else {
            Diagnostic::warning().with_code(format!("-W{id}"))
        }
        .with_labels(Self::make_warning_labels(
            &warning.begin,
            Some(&warning.end),
            &warning.kind,
        ))
        .with_message(warning.kind.to_string())
        .with_notes(warning.kind.notes());
        self.report(fstack, &diagnostic);

        // TODO: print help
    }

    pub fn report_error(&mut self, fstack: &Fstack, error: ParseError) {
        let (begin, end, kind) = Self::extract_error_info(error);

        let diagnostic = Diagnostic::error()
            .with_labels(Self::make_error_labels(&begin, end.as_ref(), &kind))
            .with_message(kind.to_string()) // TODO: ew!
            .with_notes(kind.notes());
        self.report(fstack, &diagnostic);

        kind.report_help(|diag| self.report(fstack, diag));
    }

    pub fn report_fatal_error(&mut self, fstack: &Fstack, error: ParseError) {
        let (begin, end, kind) = Self::extract_error_info(error);

        let mut notes = kind.notes();
        notes.push("Aborted assembling due to this error being fatal".into());
        let diagnostic = Diagnostic::error()
            .with_labels(Self::make_error_labels(&begin, end.as_ref(), &kind))
            .with_message(kind.to_string()) // TODO: ew!
            .with_notes(notes);
        self.report(fstack, &diagnostic);
    }
}
