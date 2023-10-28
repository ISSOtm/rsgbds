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
        let mut skip = 0;
        let (source, cur_offset): (&str, _) = 'outer: {
            for expansion in cur_state.expansions.iter_mut().rev() {
                if !expansion.has_ended() {
                    break 'outer (&expansion.source, &mut expansion.offset);
                }
                skip = expansion.parent_skip;
            }
            (node.as_ref(), &mut cur_state.offset)
        };
        (&source[*cur_offset + skip..], cur_offset)
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

                let c = match chars.next() {
                    // This prevents re-expanding characters that have already been scanned by a previous `peek` without `bump`.
                    Some(c) if self.macro_arg_scan_distance != 0 => Some(c),

                    Some('\\') if self.expand_macro_args => {
                        match self.read_putative_backslash_expansion(chars) {
                            Some((Ok(expansion), trigger_len)) => {
                                self.macro_arg_scan_distance += trigger_len; // Macro args aren't recursive.

                                // Don't bother doing the expensive work for empty expansions.
                                if !expansion.is_empty() {
                                    self.macro_arg_scan_distance += expansion.len();
                                    Self::begin_expansion(&mut lexer, expansion, trigger_len);
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
                            }

                            None => {}
                        }
                        Some('\\') // If it doesn't introduce a macro arg, then just return it.
                    }
                    Some('{') if self.enable_interpolation => {
                        todo!();
                    }
                    Some(c) => Some(c),
                    None => None,
                };
                break c;
            }
        })
    }

    /// Do not call this directly, use either [`bump()`][Self::bump()] or [`bump_capture()`][Self::bump_capture()].
    fn bump_internal(&mut self) -> char {
        let mut lexer = self.lexer.borrow_mut();

        let mut skip = 0;
        let (cur_ofs, bumped_char) = loop {
            if let Some(expansion) = lexer.cur_state_mut().expansions.last_mut() {
                if expansion.has_ended() {
                    debug_assert_eq!(skip, 0, "Cannot apply offset to ended expansion!?");

                    skip = expansion.parent_skip;
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
                let ofs = *offset + skip; // Account for the "trigger" of any expansion that may have just ended.
                break self
                    .cur_node_handle()
                    .with_node(|node| (offset, node.as_ref()[ofs..].chars().next()));
            };
        };

        let c = bumped_char.expect("Cannot shift at EOF!?");
        *cur_ofs += skip + c.len_utf8();
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
