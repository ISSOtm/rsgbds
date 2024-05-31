/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

use super::*;

impl Tokenizer<'_, '_, '_, '_, '_, '_> {
    pub(super) fn next_normal(&mut self) -> Option<(Result<Token, AsmErrorKind>, usize)> {
        macro_rules! try_chars {
            ($default:expr $(, $ch:pat => $result:expr)+ $(,)?) => {
                match self.peek() {
                    $( Some($ch) => { self.bump(); $result } )+
                    _ => $default,
                }
            };
        }

        let mut start_offset;
        Some((
            loop {
                let c = self.peek()?;
                start_offset = self.cur_root_offset();

                break match c {
                    // Ignore whitespace and comments.
                    ';' => {
                        self.bump(); // ';'
                        self.discard_comment();
                        continue;
                    }
                    c if is_whitespace(c) => {
                        self.bump();
                        continue;
                    }

                    // Line continuations.
                    '\\' => {
                        let begin = self.cur_loc();
                        self.bump();
                        // Macro args are handled by `peek`, and character escapes do not exist outside of string literals, so this must be a line continuation, or a syntax error.
                        if let Err(kind) = self.discard_line_cont() {
                            let end = self.cur_loc();
                            self.reporter
                                .borrow_mut()
                                .report_error(self.fstack, AsmError::new(begin, end, kind).into());
                        }
                        continue;
                    }

                    // Unambiguous one-char tokens.
                    '~' => {
                        self.bump();
                        Ok(Token::Complement)
                    }

                    '[' => {
                        self.bump();
                        Ok(Token::LeftBracket)
                    }
                    ']' => {
                        self.bump();
                        Ok(Token::RightBracket)
                    }
                    '(' => {
                        self.bump();
                        Ok(Token::LeftParens)
                    }
                    ')' => {
                        self.bump();
                        Ok(Token::RightParens)
                    }

                    ',' => {
                        self.bump();
                        Ok(Token::Comma)
                    }

                    // One- or two-char tokens.
                    '+' => {
                        self.bump();
                        Ok(try_chars!(Token::Plus, '=' => Token::AddEq))
                    }
                    '-' => {
                        self.bump();
                        Ok(try_chars!(Token::Minus, '=' => Token::SubEq))
                    }
                    '*' => {
                        self.bump();
                        Ok(try_chars!(Token::Mul, '=' => Token::MulEq, '*' => Token::Exponent))
                    }
                    '/' => {
                        self.bump();
                        Ok(try_chars!(Token::Div, '=' => Token::DivEq, '*' => {
                            match self.discard_block_comment() {
                                Ok(()) => continue,
                                Err(kind) => break Err(kind),
                            }
                        }))
                    }
                    '|' => {
                        self.bump();
                        Ok(try_chars!(Token::BitOr, '=' => Token::OrEq, '|' => Token::LogicOr))
                    }
                    '^' => {
                        self.bump();
                        Ok(try_chars!(Token::BitXor, '=' => Token::XorEq))
                    }
                    '=' => {
                        self.bump();
                        Ok(try_chars!(Token::Eq, '=' => Token::LogicEq))
                    }
                    '!' => {
                        self.bump();
                        Ok(try_chars!(Token::LogicNot, '=' => Token::LogicNe))
                    }

                    // One-, two-, or three-char tokens.
                    '<' => {
                        self.bump();
                        Ok(try_chars!(Token::Lt, '=' => Token::Lte, '<' => {
                            try_chars!(Token::Shl, '=' => Token::ShlEq)
                        }))
                    }

                    // One-, two-, three-, or four-char tokens.
                    '>' => {
                        self.bump();
                        Ok(try_chars!(Token::Gt, '=' => Token::Gte, '>' => {
                            try_chars!(Token::Shr, '=' => Token::ShrEq, '>' => {
                                try_chars!(Token::UShr, '=' => Token::UShrEq)
                            })
                        }))
                    }

                    // The colon may be just that, a double-colon, or an anonymous label ref.
                    ':' => {
                        self.bump();
                        Ok(match self.peek() {
                            Some(':') => {
                                self.bump();
                                Token::DoubleColon
                            }
                            Some(c @ ('+' | '-')) => {
                                self.bump();
                                Token::AnonLabelRef(self.read_anon_label_ref(c), c == '-')
                            }
                            _ => Token::Colon,
                        })
                    }

                    // Numbers.
                    c @ '0'..='9' => {
                        self.bump();
                        let int_part = self.read_number(c.to_digit(10).unwrap(), 10);
                        Ok(if self.peek() == Some('.') {
                            todo!();
                        } else {
                            Token::Number(int_part)
                        })
                    }
                    '&' => {
                        self.bump();
                        Ok(match self.peek() {
                            Some('=') => {
                                self.bump();
                                Token::AndEq
                            }
                            Some('&') => {
                                self.bump();
                                Token::LogicAnd
                            }
                            Some('0'..='7') => {
                                self.bump();
                                Token::Number(self.read_number(c.to_digit(8).unwrap(), 8))
                            }
                            _ => Token::BitAnd,
                        })
                    }
                    '%' => {
                        self.bump();
                        Ok(match self.peek() {
                            Some('=') => {
                                self.bump();
                                Token::ModEq
                            }
                            Some(ch)
                                if self
                                    .runtime_opts
                                    .borrow()
                                    .cur_options()
                                    .binary_digits
                                    .iter()
                                    .any(|c| *c == ch) =>
                            {
                                self.bump();
                                Token::Number(self.read_bin_number(ch))
                            }
                            _ => Token::Mod,
                        })
                    }
                    '$' => {
                        self.bump();
                        if let Some(base_value) = self.peek().and_then(|ch| ch.to_digit(16)) {
                            Ok(Token::Number(self.read_number(base_value, 16)))
                        } else {
                            Err(AsmErrorKind::NoHexDigits)
                        }
                    }
                    '`' => {
                        self.bump();
                        self.read_gfx_constant().map(Token::Number)
                    }

                    // Strings.
                    '"' => 'string: {
                        debug_assert!(
                            self.expand_macro_args,
                            "Macro arg expansion disabled before string literal!?"
                        );
                        debug_assert!(
                            self.enable_interpolation,
                            "Interpolation disabled before string literal!?"
                        );
                        self.expand_macro_args = false;
                        self.enable_interpolation = false;

                        self.bump();
                        let multiline = if self.peek() == Some('"') {
                            self.bump();
                            if self.peek() == Some('"') {
                                self.bump();
                                true
                            } else {
                                // An empty string.
                                break 'string Ok(Token::String(String::new()));
                            }
                        } else {
                            false
                        };

                        self.start_capture();
                        if let Err(()) = self.read_string_body(multiline) {
                            todo!(); // Report an error
                        }
                        self.expand_macro_args = true;
                        self.enable_interpolation = true;

                        let string = self.end_capture();
                        self.bump(); // The closing quote.
                        Ok(Token::String(string))
                    }

                    // Newline.
                    '\r' => {
                        self.bump();
                        todo!();
                    }
                    '\n' => {
                        self.bump();
                        Ok(Token::Newline)
                    }

                    // Identifiers.
                    '@' => {
                        self.start_capture();
                        self.bump_capture(true);
                        self.inject_lookahead_hack = true; // For consistency with the normal `Identifier` path.
                        Ok(Token::Identifier(self.end_capture()))
                    }
                    c if can_start_ident(c) || c == '.' => {
                        let token = self.read_identifier(c);

                        // TODO: make ELIF after evaluated IF skip the condition

                        Ok(match token {
                            Token::Identifier(name) => {
                                if self.lexer.borrow().expand_equs {
                                    // The symbol is not REQUIRED to exist or be an `equs`, so errors
                                    // can and should be swallowed.
                                    if let Ok(equs) = self.symbols.borrow().get_string(&name) {
                                        // TODO: check for recursion depth

                                        // No point in doing all of the work if the expansion is empty.
                                        if !equs.is_empty() {
                                            Self::begin_expansion(
                                                &mut self.lexer.borrow_mut(),
                                                Rc::clone(equs),
                                            );
                                        }
                                        continue;
                                    }
                                }

                                if self.peek() == Some(':') {
                                    Token::Label(name)
                                } else {
                                    // "Protected" identifiers need an action to happen right after the identifier.
                                    self.inject_lookahead_hack = true;
                                    Token::Identifier(name)
                                }
                            }
                            Token::Def | Token::Redef | Token::Macro | Token::Purge => {
                                // After a `def` or a `redef`, EQUS must not be expanded.
                                // This gets set back to `true` after reading the identifier,
                                // or after recovering from a syntax error.
                                self.lexer.borrow_mut().expand_equs = false;
                                token
                            }
                            Token::Opt => {
                                // `opt` has unique parsing, so it needs to switch into raw mode immediately.
                                self.lexer.borrow_mut().mode = Mode::Raw;
                                token
                            }
                            tok => tok,
                        })
                    }

                    // Garbage characters.
                    c => Err(AsmErrorKind::BadChar(c)),
                };
            },
            start_offset,
        ))
    }
}
