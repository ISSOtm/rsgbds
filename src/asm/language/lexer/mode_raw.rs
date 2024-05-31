/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

use super::*;

impl Tokenizer<'_, '_, '_, '_, '_, '_> {
    pub(super) fn next_raw(&mut self) -> Option<(Result<Token, AsmErrorKind>, usize)> {
        // First, skip any whitespace.
        loop {
            match self.peek() {
                Some(c) if is_whitespace(c) => {
                    self.bump();
                }
                Some('\\') => todo!(), // Check if it's a line cont.
                _ => break,
            }
        }

        let start_offset = self.cur_root_offset();
        let mut paren_depth = 0u32;
        self.start_capture();
        let (mut string, last_char) = loop {
            let begin = self.cur_loc();
            match self.peek() {
                Some('"') => {
                    self.bump_capture(true);
                    // TODO: this is duplicated with the entry point to `read_string`, which is not ideal;
                    //       but, this is because that code uses `bump()` instead.
                    let multiline = if self.peek() == Some('"') {
                        self.bump_capture(true);
                        if self.peek() == Some('"') {
                            self.bump_capture(true);
                            true
                        } else {
                            continue; // Empty string.
                        }
                    } else {
                        false
                    };
                    let res = self.read_string_body(multiline);
                    self.bump_capture(true); // The closing quote.

                    if let Err(()) = res {
                        let end = self.cur_loc();
                        self.reporter.borrow_mut().report_error(
                            self.fstack,
                            AsmError::new(begin, end, AsmErrorKind::UnterminatedString).into(),
                        );
                    }
                }

                ch @ (Some('\r' | '\n') | None) => {
                    // Do not shift the EOL char right now (if at all):
                    // the newline token is not to be returned if we already read a macro arg.
                    let string = self.end_capture();
                    break (string, ch);
                }

                Some(';') => {
                    let string = self.end_capture();
                    self.bump();
                    self.discard_comment();
                    break (string, Some(';'));
                }

                // Block comment inside macro arg?
                Some('/') => {
                    self.bump_capture(true);
                    if self.peek() == Some('*') {
                        self.bump_capture(false);
                        // Pop off the `/`.
                        self.capture.as_mut().unwrap().pop();
                        todo!(); // Either duplicate `discard_block_comment` (meh), or temporarily disable the capture somehow.
                    }
                }

                Some(',') if paren_depth == 0 => {
                    let string = self.end_capture();
                    self.bump();
                    break (string, Some(','));
                }

                Some('\\') => {
                    self.bump_capture(false);
                    match self.peek() {
                        None => todo!(), // Report unterminated char escape in macro arg
                        Some(line_cont_start!()) => {
                            if let Err(kind) = self.discard_line_cont() {
                                return Some((Err(kind), start_offset));
                            }
                        }
                        Some(c) => {
                            self.bump_capture(false);
                            let to_push = if let Some(escape) = escape_char(c, true) {
                                escape
                            } else {
                                todo!(); // Report invalid char escape in macro
                            };
                            self.capture.as_mut().unwrap().push(to_push);
                        }
                    }
                }

                Some(c) => {
                    if c == '(' {
                        paren_depth += 1;
                    } else if c == ')' && paren_depth != 0 {
                        paren_depth -= 1;
                    }
                    dbg!((c, paren_depth));

                    self.bump_capture(true);
                }
            }
        };

        // (Maybe) trim trailing whitespace.
        let trimmed_len = string.trim_end_matches(is_whitespace).len();
        // Commas permit empty arguments (i.e. two commas separated by whitespace only).
        if trimmed_len != 0 || last_char == Some(',') {
            String::truncate(&mut string, trimmed_len);
            Some(Token::String(string))
        } else {
            // This is a token that ends the line, and "raw mode" only lasts until the end of its line.
            // (Doing this saves having to inject "lookahead hack" tokens.)
            self.lexer.borrow_mut().mode = Mode::Normal;

            if let Some(eol_char) = last_char {
                self.bump(); // Read `last_char`.
                self.handle_crlf(eol_char);
                Some(Token::Newline)
            } else {
                None
            }
        }
        .map(|token| (Ok(token), start_offset))
    }
}
