/* SPDX-License-Identifier: MPL-2.0 */

use super::*;

impl Tokenizer<'_, '_, '_, '_, '_, '_> {
    // TODO: it'd be nicer to determine `end_len` automatically...
    pub(super) fn next_capture_body(
        &mut self,
        end_keyword: Keyword,
        end_tok_len: usize,
    ) -> Option<(Result<Token, AsmErrorKind>, usize)> {
        // Expansions will be processed when actually reading the captured body.
        debug_assert!(
            self.expand_macro_args,
            "Macro arg expansion disabled before capturing body!?"
        );
        debug_assert!(
            self.enable_interpolation,
            "Interpolation disabled before capturing body!?"
        );
        self.expand_macro_args = false;
        self.enable_interpolation = false;

        self.start_capture();
        let res = loop {
            // We are at the beginning of a line, so attempt to match an `ENDM` token.
            match self.peek() {
                None => break Err(AsmErrorKind::UnterminatedMacro), // TODO: this would be wrong for REPTs!
                Some(c) => {
                    self.bump_capture(true);
                    if can_start_ident(c) && self.read_specific_keyword(c, end_keyword) {
                        break Ok(());
                    }
                }
            }
        };
        let mut body = self.end_capture();
        let read_len = body.len();
        body.truncate(read_len - end_tok_len);

        self.expand_macro_args = true;
        self.enable_interpolation = true;
        self.lexer.borrow_mut().mode = Mode::Normal; // Automatically revert back to normal mode.
        Some((res.map(|()| Token::String(body)), read_len))
    }
}
