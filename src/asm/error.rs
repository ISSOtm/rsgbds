/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    term::{
        termcolor::{ColorChoice, StandardStream},
        Config as OutputConfig,
    },
};

use crate::{
    fstack::Fstack,
    language::{AsmError, AsmErrorKind, Location, ParseError, Warning, WarningId, WarningKind},
};

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Hash)]
pub enum WarningState {
    #[default]
    Default,
    Disabled,
    Enabled,
    Error,
}

#[derive(Debug)]
pub struct Reporter {
    writer: StandardStream,
    config: OutputConfig,
}

impl Reporter {
    pub fn new(color_choice: ColorChoice) -> Self {
        let mut config = OutputConfig::default();
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

    pub fn warn(&mut self, fstack: &Fstack, warning: Warning, settings: &WarningSettings) {
        let id = WarningId::from(&warning.kind);

        // Determine what to do based on configured warning levels.
        let is_error = match settings.level(id) {
            WarningState::Disabled => return,
            WarningState::Default if !WarningSettings::default_enabled(id) => return,

            WarningState::Error => true,

            // `Default` only reaches here if the default state is "enabled".
            WarningState::Enabled | WarningState::Default => settings.warnings_are_errors,
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

#[derive(Debug, Clone, Default)]
pub struct WarningSettings {
    levels: [WarningState; WarningId::NB_WARNINGS],
    warnings_are_errors: bool,
}

impl WarningSettings {
    fn level(&self, id: WarningId) -> WarningState {
        self.levels[id as usize]
    }

    fn default_enabled(id: WarningId) -> bool {
        WarningId::DEFAULTS[id as usize]
    }

    pub fn process_flag(&mut self, flag: &str) -> Result<(), AsmErrorKind> {
        let (id_name, new_state) = if let Some(rest) = flag.strip_prefix("no-") {
            (rest, WarningState::Disabled)
        } else if let Some(rest) = flag.strip_prefix("error=") {
            (rest, WarningState::Error)
        } else if flag == "error" {
            self.warnings_are_errors = true;
            return Ok(());
        } else {
            (flag, WarningState::Enabled)
        };

        WarningKind::handle_flag(id_name, &mut self.levels, new_state)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn everything_enables_everything() {
        let mut flags = WarningSettings::default();
        flags.process_flag("everything").unwrap();
        for (i, state) in flags.levels.iter().copied().enumerate() {
            assert_eq!(state, WarningState::Enabled, "{i}");
        }
    }

    #[test]
    fn parametric_levels() {
        let mut flags = WarningSettings::default();

        flags.process_flag("unmapped-char=0").unwrap();
        assert_eq!(
            flags.level(WarningId::UnmappedChar1),
            WarningState::Disabled
        );
        assert_eq!(
            flags.level(WarningId::UnmappedChar2),
            WarningState::Disabled
        );

        flags.process_flag("unmapped-char=1").unwrap();
        assert_eq!(flags.level(WarningId::UnmappedChar1), WarningState::Enabled);
        assert_eq!(
            flags.level(WarningId::UnmappedChar2),
            WarningState::Disabled
        );

        flags.process_flag("unmapped-char=2").unwrap();
        assert_eq!(flags.level(WarningId::UnmappedChar1), WarningState::Enabled);
        assert_eq!(flags.level(WarningId::UnmappedChar2), WarningState::Enabled);

        flags.process_flag("error=unmapped-char=1").unwrap();
        assert_eq!(flags.level(WarningId::UnmappedChar1), WarningState::Error);
        assert_eq!(
            flags.level(WarningId::UnmappedChar2),
            WarningState::Disabled
        );

        flags.process_flag("no-unmapped-char").unwrap(); // Equivalent to "0".
        assert_eq!(
            flags.level(WarningId::UnmappedChar1),
            WarningState::Disabled
        );
        assert_eq!(
            flags.level(WarningId::UnmappedChar2),
            WarningState::Disabled
        );

        let err = flags.process_flag("no-unmapped-char=1").unwrap_err();
        assert!(
            matches!(err, AsmErrorKind::NegatedParametricWarning("unmapped-char")),
            "Got {err:?}",
        );
    }
}
