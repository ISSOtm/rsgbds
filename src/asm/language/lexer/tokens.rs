/* SPDX-License-Identifier: MPL-2.0 */

use super::*;

/// Lexing sub-functions.
impl Tokenizer<'_, '_, '_, '_, '_, '_> {
    pub(super) fn handle_crlf(&mut self, ch: char) {
        if ch == '\r' && self.peek() == Some('\n') {
            self.bump();
        }
    }

    pub(super) fn discard_comment(&mut self) {
        // TODO: batch this, by instead manipulating the `offset`s directly
        //       (would avoid iterating through the expansions repeatedly, for instance)
        debug_assert!(
            self.expand_macro_args,
            "Macro arg expansion disabled before comment!?"
        );
        debug_assert!(
            self.enable_interpolation,
            "Interpolation disabled before comment!?"
        );
        self.expand_macro_args = false;
        self.enable_interpolation = false;

        while !matches!(self.peek(), None | Some('\r' | '\n')) {
            self.bump();
        }

        self.expand_macro_args = true;
        self.enable_interpolation = true;
    }

    pub(super) fn discard_block_comment(&mut self) -> Result<(), AsmErrorKind> {
        debug_assert!(
            self.expand_macro_args,
            "Macro arg expansion disabled before block comment!?"
        );
        debug_assert!(
            self.enable_interpolation,
            "Interpolation disabled before block comment!?"
        );
        self.expand_macro_args = false;
        self.enable_interpolation = false;

        // TODO: batch this, by instead manipulating the `offset`s directly
        //       (would avoid iterating through the expansions repeatedly, for instance)
        let res = loop {
            let Some(ch) = self.peek() else {
                break Err(AsmErrorKind::UnterminatedBlockComment);
            };
            let begin = self.cur_loc();
            self.bump();

            if ch == '/' && self.peek() == Some('*') {
                let mut end = self.cur_loc();
                end.offset += 1; // Act as if the `*` had been bumped, but don't bump it, otherwise `/*/` doesn't terminate the comment.
                self.reporter.borrow_mut().warn(
                    self.fstack,
                    Warning {
                        begin,
                        end,
                        kind: crate::language::WarningKind::NestedBlockComment,
                    },
                    &self.runtime_opts.borrow().cur_options().warn_settings,
                );
            } else if ch == '*' && self.peek() == Some('/') {
                self.bump();
                break Ok(());
            }
        };

        self.expand_macro_args = true;
        self.enable_interpolation = true;

        res
    }

    pub(super) fn discard_line_cont(&mut self) -> Result<(), AsmErrorKind> {
        todo!(); // I'm thinking, grab the current source, and try to read from it to the end. This bypasses both expansion kinds, and ensures that an `equs` or macro arg cannot begin a line continuation.
    }

    pub(super) fn read_anon_label_ref(&mut self, first_char: char) -> u32 {
        let mut n = 1;

        while self.peek() == Some(first_char) {
            self.bump();
            n += 1;
        }

        n
    }

    pub(super) fn read_number(&mut self, base_value: u32, radix: u32) -> u32 {
        let mut value = base_value;
        while let Some(ch) = self.peek() {
            if ch == '_' {
                // Separator character, ignore.
            } else if let Some(digit) = ch.to_digit(radix) {
                value = value.wrapping_mul(radix).wrapping_add(digit);
            } else {
                break;
            }
            self.bump();
        }
        value
    }

    pub(super) fn read_bin_number(&mut self, first_char: char) -> u32 {
        let opt_stack = self.runtime_opts.borrow();
        let opts = opt_stack.cur_options();

        let digit = |ch| {
            if ch == opts.binary_digits[0] {
                Some(0)
            } else if ch == opts.binary_digits[1] {
                Some(1)
            } else {
                None
            }
        };

        let mut value = digit(first_char).unwrap();
        while let Some(ch) = self.peek() {
            if ch == '_' {
                // Separator character, ignore.
            } else if let Some(digit) = digit(ch) {
                value = value * 2 + digit;
            } else {
                break;
            }
            self.bump();
        }

        value
    }

    pub(super) fn read_gfx_constant(&mut self) -> Result<u32, AsmErrorKind> {
        let opt_stack = self.runtime_opts.borrow();
        let opts = opt_stack.cur_options();

        let digit = |ch| match ch {
            ch if ch == opts.gfx_digits[0] => Some(0),
            ch if ch == opts.gfx_digits[1] => Some(1),
            ch if ch == opts.gfx_digits[2] => Some(2),
            ch if ch == opts.gfx_digits[3] => Some(3),
            _ => None,
        };

        let mut msb = 0u8;
        let mut lsb = 0u8;
        let mut width = 0;
        while let Some(ch) = self.peek() {
            if ch == '_' {
                if width == 0 {
                    break; // This is not allowed as the first char.
                }
                // Separator character, ignore it.
            } else if let Some(digit) = digit(ch) {
                if width < 8 {
                    msb = msb << 1 | digit & 1;
                    lsb = lsb << 1 | digit >> 1;
                }
                if width < 9 {
                    width += 1;
                }
            } else {
                break;
            }
            self.bump();
        }

        if width == 0 {
            return Err(AsmErrorKind::NoGfxChars(opts.gfx_digits));
        }
        if width > 8 {
            // TODO: warning
        }
        Ok(u32::from(msb) << 8 | u32::from(lsb))
    }

    pub(super) fn read_string_body(&mut self, multiline: bool) -> Result<(), ()> {
        loop {
            macro_rules! append {
                ($ch:expr) => {
                    self.capture.as_mut().unwrap().push($ch);
                };
            }

            match self.peek() {
                Some('"') => {
                    if multiline {
                        // A single quote is not enough, we need three in a row.
                        self.bump_capture(false);
                        let Some('"') = self.peek() else {
                            append!('"');
                            continue;
                        };
                        self.bump_capture(false);
                        let Some('"') = self.peek() else {
                            append!('"');
                            append!('"');
                            continue;
                        };
                    }
                    break Ok(());
                }

                None => break Err(()),
                Some('\n') if !multiline => break Err(()),

                // Special characters.
                Some('\\') => {
                    // Do not bump the backslash yet, as that might change the active expansion.
                    let mut lexer = self.lexer.borrow_mut();
                    let cur_state = lexer.cur_state_mut();
                    let cur_node = self.cur_node_handle();
                    cur_node.with_node(|node| {
                        let (source, cur_ofs) = Self::get_state_source(cur_state, node);

                        // Since the backslash hasn't been bumped, `source` points to it.
                        debug_assert_eq!(source.chars().next(), Some('\\'));
                        if let Some((result, trigger_len)) =
                            self.read_putative_backslash_expansion(source[1..].chars())
                        {
                            *cur_ofs += trigger_len;
                            match result {
                                Ok(expansion) => {
                                    let string = &mut self.capture.as_mut().unwrap();
                                    string.push_str(&expansion);
                                }
                                Err(kind) => {
                                    let begin = self.cur_loc();
                                    let end = Location {
                                        storage: begin.storage.clone(),
                                        offset: begin.offset + trigger_len,
                                    };
                                    self.reporter.borrow_mut().report_error(
                                        self.fstack,
                                        AsmError::new(begin, end, kind).into(),
                                    );
                                }
                            }
                        } else {
                            // Regular ol' handling.
                            let begin = self.cur_loc();
                            self.bump_capture(false);

                            match self.peek() {
                                Some('\\' | '"' | '{' | '}') => self.bump_capture(true),
                                Some('n') => {
                                    append!('\n');
                                    self.bump_capture(false);
                                }
                                Some('r') => {
                                    append!('\r');
                                    self.bump_capture(false);
                                }
                                Some('t') => {
                                    append!('\t');
                                    self.bump_capture(false);
                                }
                                Some(line_cont_start!()) => {
                                    if let Err(kind) = self.discard_line_cont() {
                                        let end = Location {
                                            storage: begin.storage.clone(),
                                            offset: self.cur_root_offset(),
                                        };
                                        self.reporter.borrow_mut().report_error(
                                            self.fstack,
                                            AsmError::new(begin, end, kind).into(),
                                        );
                                    }
                                    todo!("Mark capture as disrupted");
                                }

                                // Do not bump the character, it will then be read normally on the next iteration.
                                c => self.reporter.borrow_mut().report_error(
                                    self.fstack,
                                    AsmError::new(
                                        begin,
                                        self.cur_loc(),
                                        c.map_or(
                                            AsmErrorKind::IllegalEscapeEof,
                                            AsmErrorKind::IllegalEscape,
                                        ),
                                    )
                                    .into(),
                                ),
                            }
                        }
                    });
                }

                // Other characters get appended normally.
                Some(_) => self.bump_capture(true),
            }
        }
    }

    // This is mostly a smaller subset of the function below; importantly, this one doesn't start a
    // capture, which allows it to be used from within `next_capture_body`.
    // This also makes it more performant for the purpose of only looking for a keyword.
    pub(super) fn read_specific_keyword(&mut self, first_char: char, expected: Keyword) -> bool {
        let mut state = TrieIndex::try_from(first_char)
            .ok()
            .and_then(|idx| TrieIter::new(&KEYWORD_TRIE).next(idx));
        // Note: the caller has already `bump()`ed the first char.

        while let Some(ch) = self.peek() {
            if let Ok(idx) = TrieIndex::try_from(ch) {
                state = state.and_then(|state| state.next(idx));
            } else {
                break; // Not a valid identifier character.
            }
            self.bump_capture(true);
        }

        state.map_or(false, |state| state.done() == Some(expected))
    }
    pub(super) fn read_identifier(&mut self, first_char: char) -> Token {
        self.start_capture();
        let mut is_local = first_char == '.';
        let mut state = match TrieIndex::try_from(first_char) {
            Ok(idx) => TrieIter::new(&KEYWORD_TRIE).next(idx),
            Err(_) => None,
        };
        self.bump_capture(true);

        while let Some(ch) = self.peek() {
            if ch == '.' {
                is_local = true;
                state = None;
            } else if let Ok(idx) = TrieIndex::try_from(ch) {
                state = state.and_then(|state| state.next(idx));
            } else {
                break;
            }
            self.bump_capture(true);
        }

        let ident_name = self.end_capture();
        state
            .and_then(|state| state.done())
            .map(Token::from)
            .unwrap_or_else(|| {
                if !is_local {
                    Token::Identifier(ident_name)
                } else if ident_name.len() != 1 {
                    debug_assert_ne!(ident_name.deref(), ".");
                    Token::LocalIdent(ident_name)
                } else {
                    Token::Period
                }
            })
    }
}
