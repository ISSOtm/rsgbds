/* SPDX-License-Identifier: MPL-2.0 */

//! The "character stream" functions.
use super::*;

impl<'fstack> Tokenizer<'_, 'fstack, '_, '_, '_, '_> {
    // Retrieves the "source" string underlying the provided state; if an expansion is active, its
    // contents are returned, otherwise the state's "root" node is used.
    // Note that the returned `&str` has already been offset, the `&mut usize` should only be
    // increased by however many **bytes** end up being consumed.
    //
    // Avoid calling this function directly, prefer using [`peek`][Self::peek()].
    pub(super) fn get_state_source<'ret, 'state: 'ret, 'node: 'ret>(
        cur_state: &'state mut State,
        node: &'node Node,
    ) -> (&'ret str, &'state mut usize) {
        let (source, cur_offset): (&str, _) = 'outer: {
            for expansion in cur_state.expansions.iter_mut().rev() {
                if !expansion.has_ended() {
                    break 'outer (&expansion.source, &mut expansion.offset);
                }
            }
            (node.as_ref(), &mut cur_state.offset)
        };
        (&source[*cur_offset..], cur_offset)
    }

    fn with_cur_source<T, F: FnOnce(&str, &mut usize) -> T>(&self, f: F) -> T {
        debug_assert!(
            !self.expand_macro_args,
            "Accessing cur source raw while macro args are active!?"
        );
        debug_assert!(
            !self.enable_interpolation,
            "Accessing cur source raw while interpolation is enabled!?"
        );

        let mut lexer = self.lexer.borrow_mut();
        let cur_node = self.cur_node_handle();
        cur_node.with_node(|node| {
            let (source, cur_offset) = Self::get_state_source(lexer.cur_state_mut(), node);
            f(source, cur_offset)
        })
    }

    pub(super) fn peek(&mut self) -> Option<char> {
        let mut lexer = self.lexer.borrow_mut();
        let cur_node = self.cur_node_handle();
        cur_node.with_node(|node| {
            loop {
                let (source, cur_offset) = Self::get_state_source(lexer.cur_state_mut(), node);
                let mut chars = source.chars();

                // This prevents re-expanding characters that have already been scanned by a previous `peek` without `bump`.
                if self.macro_arg_scan_distance > 0 {
                    break chars.next();
                }

                let c = match chars.next() {
                    Some('\\') if self.expand_macro_args => {
                        let let_expansions_expire = |lexer: &mut Lexer| {
                            let expansions = &mut lexer.cur_state_mut().expansions;
                            while let Some(expansion) = expansions.last() {
                                if !expansion.has_ended() {
                                    break;
                                }
                                expansions.pop();
                            }
                        };

                        match self.read_putative_backslash_expansion(chars) {
                            Some((Ok(expansion), trigger_len)) => {
                                // Skip over the trigger text.
                                // It's fine to do this "raw", because expansion triggers are always
                                // contained within a single expansion level anyway.
                                // (The fact that we're using a single [`str::chars()`] iterator to
                                // read the trigger should be proof enough.)
                                *cur_offset += trigger_len;
                                let_expansions_expire(&mut lexer);

                                // Don't bother doing the expensive work for empty expansions.
                                if !expansion.is_empty() {
                                    // Macro args aren't recursive.
                                    self.macro_arg_scan_distance += expansion.len();
                                    Self::begin_expansion(&mut lexer, expansion);
                                }
                                continue;
                            }

                            Some((Err(kind), trigger_len)) => {
                                let cur_node = self.fstack.cur_node_handle();
                                let begin = Location {
                                    storage: cur_node.clone(),
                                    offset: *cur_offset,
                                };
                                let end = Location {
                                    storage: cur_node,
                                    offset: *cur_offset + trigger_len,
                                };
                                self.reporter.borrow_mut().report_error(
                                    self.fstack,
                                    AsmError::new(begin, end, kind).into(),
                                );

                                // Skip the bad expansion trigger.
                                *cur_offset += trigger_len;
                                let_expansions_expire(&mut lexer);
                                continue;
                            }

                            None => Some('\\'), // If it doesn't introduce a macro arg, then just return it.
                        }
                    }
                    Some('{') if self.enable_interpolation => {
                        todo!();
                    }
                    Some(c) => Some(c),
                    None => None,
                };

                if c.is_some() {
                    self.macro_arg_scan_distance += 1; // Do not consider this character again.
                }
                break c;
            }
        })
    }

    /// Do not call this directly, use either [`bump()`][Self::bump()] or [`bump_capture()`][Self::bump_capture()].
    fn bump_internal(&mut self) -> char {
        let mut lexer = self.lexer.borrow_mut();

        let (cur_ofs, bumped_char) = loop {
            if let Some(expansion) = lexer.cur_state_mut().expansions.last_mut() {
                if expansion.has_ended() {
                    lexer.cur_state_mut().expansions.pop();
                    continue; // Retry with the parent.
                }
                let offset = expansion.offset;
                break (
                    &mut expansion.offset,
                    expansion.source[offset..].chars().next(),
                );
            } else {
                let offset = &mut lexer.cur_state_mut().offset;
                let cur_offset = *offset;
                break self
                    .cur_node_handle()
                    .with_node(|node| (offset, node.as_ref()[cur_offset..].chars().next()));
            };
        };

        self.macro_arg_scan_distance -= 1;

        let c = bumped_char.expect("Cannot shift at EOF!?");
        *cur_ofs += c.len_utf8();
        c
    }

    /// Use to "accept" a character while capture is not active.
    pub(super) fn bump(&mut self) {
        self.bump_internal();

        #[cfg(debug_assertions)]
        if let Some(capture) = &self.capture {
            panic!("Unexpectedly capturing!? (capture = {capture:?})");
        }
    }

    /// Use to "accept" a character while capture is active.
    /// `should_capture` specifies whether the shifted character must be appended to the capture.
    pub(super) fn bump_capture(&mut self, should_capture: bool) {
        let c = self.bump_internal();

        let capture = self.capture.as_mut().expect("Unexpectedly not capturing!?");
        if should_capture {
            capture.push(c);
        }
    }

    pub(super) fn start_capture(&mut self) {
        debug_assert!(
            self.capture.is_none(),
            "Lexer beginning capture without ending previous!?"
        );

        let lexer = self.lexer.borrow();
        let cur_state = lexer.cur_state();
        let start_ofs = cur_state.offset;
        self.capture = Some(match cur_state.expansions.last() {
            Some(expansion) => String::new(),
            None => self
                .cur_node_handle()
                .with_node(|node| node.slice(start_ofs..start_ofs)),
        });
    }

    pub(super) fn end_capture(&mut self) -> String {
        self.capture
            .take()
            .expect("Lexer ending capture without starting it!?")
    }

    pub(super) fn location(
        storage: Option<NodeHandle<'fstack>>,
        mut offset: usize,
    ) -> Location<'fstack> {
        if let Some(node) = &storage {
            offset += node.with_node(Node::storage_base_ofs);
        }
        Location { storage, offset }
    }

    pub(super) fn cur_loc(&self) -> Location<'fstack> {
        Self::location(self.fstack.cur_node_handle(), self.cur_root_offset())
    }
}
