/* SPDX-License-Identifier: MPL-2.0 */

use codespan_reporting::term::{
    termcolor::{ColorChoice, StandardStream},
    Config as OutputConfig,
};

pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<()>;

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

    pub fn report(&mut self, diagnostic: &Diagnostic) {
        if let Err(err) = codespan_reporting::term::emit(
            &mut self.writer,
            &self.config,
            &rgbds::common::codespan_dummy::Dummy,
            diagnostic,
        ) {
            eprintln!("Internal error when writing diagnostic: {err}");
        }
    }
}
