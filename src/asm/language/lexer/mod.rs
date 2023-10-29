/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

use std::{cell::RefCell, dbg, debug_assert, debug_assert_eq, ops::Deref, rc::Rc};

use crate::{
    error::Reporter,
    fstack::{Fstack, Node, NodeHandle},
    language::{tokens::can_start_ident, Warning},
    macro_args::MacroArgs,
    opt::RuntimeOptStack,
    symbols::Symbols,
};

use super::{
    tokens::{Keyword, Token, TrieIndex, TrieIter, KEYWORD_TRIE},
    AsmError, AsmErrorKind,
};

/// Unlike state in the [`Fstack`], state in the `Lexer` does not persist once the context is exited.
#[derive(Debug)]
pub struct Lexer {
    states: Vec<State>,

    pub expand_equs: bool,
    pub mode: Mode,
}

#[derive(Debug)]
struct State {
    /// Offset within the buffer.
    offset: usize,
    expansions: Vec<Expansion>,
}

#[derive(Debug)]
struct Expansion {
    source: Rc<String>,
    offset: usize,
}

#[derive(Debug, Clone, Copy)]
pub enum Mode {
    Normal,
    Raw,
    CaptureMacroBody,
    CaptureLoopBody,
}

impl Lexer {
    pub fn new() -> Self {
        Self {
            states: vec![State::new()],
            expand_equs: true, // Enabled by default.
            mode: Mode::Normal,
        }
    }

    fn cur_state(&self) -> &State {
        self.states
            .last()
            .expect("There should always be at least one lexer state")
    }

    fn cur_state_mut(&mut self) -> &mut State {
        self.states
            .last_mut()
            .expect("There should always be at least one lexer state")
    }

    pub fn push_new_state(&mut self) {
        self.states.push(State::new());
    }

    pub fn pop_state(&mut self) {
        self.states.pop();
    }

    pub fn cur_ofs(&self) -> usize {
        self.cur_state().offset
    }
}

impl State {
    fn new() -> Self {
        Self {
            offset: 0,
            expansions: Vec::new(), // This doesn't allocate.
        }
    }

    pub fn reset(&mut self) {
        // TODO: what if the buffer *ends* with an expansion? Is it still on the stack, but "inactive"?
        debug_assert_eq!(self.expansions.len(), 0); // No expansion should be active when resetting a state.
        self.offset = 0;
    }
}

impl Expansion {
    fn has_ended(&self) -> bool {
        debug_assert!(self.offset <= self.source.len());
        self.offset == self.source.len()
    }
}

#[derive(Debug, Clone, Default, PartialEq)]
pub struct Location<'fstack> {
    storage: Option<NodeHandle<'fstack>>,
    offset: usize,
}

impl Location<'_> {
    pub(crate) const fn builtin() -> Location<'static> {
        Location {
            storage: None,
            offset: 0,
        }
    }

    pub(crate) fn handle(&self) -> Option<&NodeHandle<'_>> {
        self.storage.as_ref()
    }

    pub(crate) fn offset(&self) -> usize {
        self.offset
    }
}

impl PartialOrd for Location<'_> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.storage != other.storage {
            None
        } else {
            Some(self.offset.cmp(&other.offset))
        }
    }
}

#[derive(Debug)]
pub struct Tokenizer<'opts, 'fstack, 'lexer, 'macro_args, 'reporter, 'syms> {
    runtime_opts: &'opts RefCell<RuntimeOptStack>,
    fstack: &'fstack Fstack,
    lexer: &'lexer RefCell<Lexer>,
    macro_args: &'macro_args RefCell<Vec<MacroArgs>>,
    reporter: &'reporter RefCell<Reporter>,
    symbols: &'syms RefCell<Symbols<'fstack>>,

    // These are fine here because they are always both false when a new state is pushed.
    // TODO: are they really necessary? Don't we always know their state? If so, why not simply pass them as args to `peek()`?
    expand_macro_args: bool,
    enable_interpolation: bool,

    /// How many characters have already been scanned for macro args.
    macro_arg_scan_distance: usize,

    /// If `Some()`, shifted characters are "captured" into this string.
    capture: Option<String>, // Capture shouldn't be active when changing states.

    /// Is the lexer at the beginning of the line?
    /// (If `true`, the lexer will next generate a [`LookaheadHack`][Token::LookaheadHack] token.)
    inject_lookahead_hack: bool,
}

// A couple of properties.

fn is_whitespace(c: char) -> bool {
    matches!(c, ' ' | '\t')
}

fn escape_char(c: char, is_macro_arg: bool) -> Option<char> {
    match c {
        ',' | '(' | ')' if is_macro_arg => Some(c),
        '\\' | '"' | '{' | '}' => Some(c),
        'n' => Some('\n'),
        'r' => Some('\r'),
        't' => Some('\t'),
        _ => None,
    }
}

macro_rules! line_cont_start {
    () => {
        ' ' | '\r' | '\n'
    };
}

impl<'opts, 'fstack, 'lexer, 'macro_args, 'reporter, 'syms>
    Tokenizer<'opts, 'fstack, 'lexer, 'macro_args, 'reporter, 'syms>
{
    pub fn new(
        runtime_opts: &'opts RefCell<RuntimeOptStack>,
        fstack: &'fstack Fstack,
        lexer: &'lexer RefCell<Lexer>,
        macro_args: &'macro_args RefCell<Vec<MacroArgs>>,
        reporter: &'reporter RefCell<Reporter>,
        symbols: &'syms RefCell<Symbols<'fstack>>,
    ) -> Self {
        Self {
            runtime_opts,
            fstack,
            lexer,
            macro_args,
            reporter,
            symbols,

            expand_macro_args: true,    // Enabled by default.
            enable_interpolation: true, // Enabled by default.

            macro_arg_scan_distance: 0,

            capture: None, // Disabled by default.

            inject_lookahead_hack: false,
        }
    }
}

/// Helper functions.
impl<'fstack> Tokenizer<'_, 'fstack, '_, '_, '_, '_> {
    fn cur_root_offset(&self) -> usize {
        self.lexer.borrow().cur_state().offset
    }

    fn cur_node_handle(&self) -> NodeHandle<'fstack> {
        self.fstack
            .cur_node_handle()
            .expect("Should have an active fstack node")
    }

    fn with_active_macro_args<T, F: FnMut(&MacroArgs) -> Result<T, AsmErrorKind>>(
        &self,
        mut f: F,
    ) -> Result<T, AsmErrorKind> {
        match self.macro_args.borrow().last() {
            None => Err(AsmErrorKind::NoActiveMacro),
            Some(args) => f(args),
        }
    }

    fn try_get_macro_arg(&self, idx: u32) -> Result<Rc<String>, AsmErrorKind> {
        if idx == 0 {
            Err(AsmErrorKind::NoMacroArg0)
        } else {
            self.with_active_macro_args(|args| match args.get(idx.try_into().unwrap()) {
                None => Err(AsmErrorKind::NoMacroArg(idx)),
                Some(arg) => Ok(Rc::clone(arg)),
            })
        }
    }

    fn read_putative_backslash_expansion<It: Iterator<Item = char>>(
        &self,
        mut iter: It,
    ) -> Option<(Result<Rc<String>, AsmErrorKind>, usize)> {
        Some(match iter.next()? {
            c @ '0'..='9' => {
                let idx = c as u32 - '0' as u32; // Because `to_digit` is inconvenient here.
                (self.try_get_macro_arg(idx), 2)
            }
            '#' => (
                self.with_active_macro_args(|args| Ok(args.make_concat())),
                2,
            ),
            '<' => {
                todo!();
            }

            '@' => (todo!(), 2),

            _ => return None,
        })
    }

    fn begin_expansion(lexer: &mut Lexer, source: Rc<String>) {
        lexer
            .cur_state_mut()
            .expansions
            .push(Expansion { source, offset: 0 })
    }
}

mod char_stream;
mod tokens;
// These are the various "modes" the function below dispatches to.
mod mode_capture_body;
mod mode_normal;
mod mode_raw;

/// The interface used by the parser.
impl<'fstack> Iterator for Tokenizer<'_, 'fstack, '_, '_, '_, '_> {
    type Item = Result<(Location<'fstack>, Token, Location<'fstack>), AsmError<'fstack>>;

    fn next(&mut self) -> Option<Self::Item> {
        let cur_node = self.fstack.cur_node_handle()?; // No active node means we reached the end of input.
        let storage_base_ofs = cur_node.with_node(Node::storage_base_ofs);

        let mode = self.lexer.borrow().mode;
        let (res, start_offset) = if std::mem::replace(&mut self.inject_lookahead_hack, false) {
            (Ok(Token::LookaheadHack), self.cur_root_offset())
        } else {
            let token = match mode {
                Mode::Normal => self.next_normal(),
                Mode::Raw => self.next_raw(),
                Mode::CaptureMacroBody => self.next_capture_body(Keyword::Endm, 4),
                Mode::CaptureLoopBody => self.next_capture_body(Keyword::Endr, 4),
            };
            match token {
                None => {
                    // We have no more tokens in the active "node", but we want to ensure that all
                    // directives end with a newline.
                    let loc = Self::location(Some(cur_node.clone()), self.cur_root_offset());
                    // FIXME: if you have a `INCLUDE` at EOL without a newline, this will pop off its parent node *before* excuting the `INCLUDE`!!
                    //        This can be fixed by controlling that the INCLUDE is executed before the newline, but that would require either a "lexer hack" injection (likely right after parsing the `INCLUDE`), or a hand-written parser.
                    self.fstack.handle_end_of_node(
                        &mut self.lexer.borrow_mut(),
                        &mut self.macro_args.borrow_mut(),
                    );
                    return Some(Ok((loc.clone(), Token::Newline, loc)));
                }
                Some(token) => token,
            }
        };
        let start_location = Self::location(Some(cur_node.clone()), start_offset);
        // FIXME: this is wrong for macro args followed by a comma and/or right-trimmed.
        let end_location = Self::location(Some(cur_node.clone()), self.cur_root_offset());
        Some(match res {
            Ok(token) => Ok((start_location, token, end_location)),
            Err(kind) => Err(AsmError::new(start_location, end_location, kind)),
        })
    }
}
