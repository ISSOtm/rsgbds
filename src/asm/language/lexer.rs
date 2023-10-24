use std::{cell::RefCell, dbg, debug_assert, debug_assert_eq, ops::Deref, rc::Rc};

use crate::{
    error::Reporter,
    fstack::{Fstack, Node, NodeHandle},
    language::{tokens::can_start_ident, Warning},
    macro_args::MacroArgs,
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

    bin_digits: [char; 2],
    gfx_digits: [char; 4],
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
    /// How many bytes long the text that triggered this expansion is.
    parent_skip: usize,
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
            bin_digits: ['0', '1'],
            gfx_digits: ['0', '1', '2', '3'],
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
pub struct Tokenizer<'fstack, 'lexer, 'macro_args, 'reporter, 'syms> {
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

impl<'fstack, 'lexer, 'macro_args, 'reporter, 'syms>
    Tokenizer<'fstack, 'lexer, 'macro_args, 'reporter, 'syms>
{
    pub fn new(
        fstack: &'fstack Fstack,
        lexer: &'lexer RefCell<Lexer>,
        macro_args: &'macro_args RefCell<Vec<MacroArgs>>,
        reporter: &'reporter RefCell<Reporter>,
        symbols: &'syms RefCell<Symbols<'fstack>>,
    ) -> Self {
        Self {
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
impl<'fstack> Tokenizer<'fstack, '_, '_, '_, '_> {
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

    fn begin_expansion(lexer: &mut Lexer, source: Rc<String>, trigger_len: usize) {
        lexer.cur_state_mut().expansions.push(Expansion {
            source,
            offset: 0,
            parent_skip: trigger_len,
        })
    }
}

/// The "character stream" functions.
impl<'fstack> Tokenizer<'fstack, '_, '_, '_, '_> {
    // Retrieves the "source" string underlying the provided state; if an expansion is active, its
    // contents are returned, otherwise the state's "root" node is used.
    // Note that the returned `&str` has already been offset, the `&mut usize` should only be
    // increased by however many **bytes** end up being consumed.
    //
    // Do not call this function directly, it is only factored out for code reuse.
    fn get_state_source<'ret, 'state: 'ret, 'node: 'ret>(
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

    fn peek(&mut self) -> Option<char> {
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
    fn bump(&mut self) {
        self.bump_internal();

        #[cfg(debug_assertions)]
        if let Some(capture) = &self.capture {
            panic!("Unexpectedly capturing!? (capture = {capture:?})");
        }
    }

    /// Use to "accept" a character while capture is active.
    /// `should_capture` specifies whether the shifted character must be appended to the capture.
    fn bump_capture(&mut self, should_capture: bool) {
        let c = self.bump_internal();

        let capture = self.capture.as_mut().expect("Unexpectedly not capturing!?");
        if should_capture {
            capture.push(c);
        }
    }

    fn start_capture(&mut self) {
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

    fn end_capture(&mut self) -> String {
        self.capture
            .take()
            .expect("Lexer ending capture without starting it!?")
    }

    fn location(storage: Option<NodeHandle<'fstack>>, mut offset: usize) -> Location<'fstack> {
        if let Some(node) = &storage {
            offset += node.with_node(Node::storage_base_ofs);
        }
        Location { storage, offset }
    }

    fn cur_loc(&self) -> Location<'fstack> {
        Self::location(self.fstack.cur_node_handle(), self.cur_root_offset())
    }
}

/// Lexing sub-functions.
impl Tokenizer<'_, '_, '_, '_, '_> {
    fn handle_crlf(&mut self, ch: char) {
        if ch == '\r' && self.peek() == Some('\n') {
            self.bump();
        }
    }

    fn discard_comment(&mut self) {
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

    fn discard_block_comment(&mut self) -> Result<(), AsmErrorKind> {
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

    fn discard_line_cont(&mut self) -> Result<(), AsmErrorKind> {
        todo!(); // I'm thinking, grab the current source, and try to read from it to the end. This bypasses both expansion kinds, and ensures that an `equs` or macro arg cannot begin a line continuation.
    }

    fn read_anon_label_ref(&mut self, first_char: char) -> u32 {
        let mut n = 1;

        while self.peek() == Some(first_char) {
            self.bump();
            n += 1;
        }

        n
    }

    fn read_number(&mut self, base_value: u32, radix: u32) -> u32 {
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

    fn read_bin_number(&mut self, first_char: char) -> u32 {
        let lexer = self.lexer.borrow();
        let digit = |ch| {
            if ch == lexer.bin_digits[0] {
                Some(0)
            } else if ch == lexer.bin_digits[1] {
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

    fn read_gfx_constant(&mut self) -> Result<u32, AsmErrorKind> {
        let lexer = self.lexer.borrow();
        let digit = |ch| match ch {
            ch if ch == lexer.gfx_digits[0] => Some(0),
            ch if ch == lexer.gfx_digits[1] => Some(1),
            ch if ch == lexer.gfx_digits[2] => Some(2),
            ch if ch == lexer.gfx_digits[3] => Some(3),
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
            return Err(AsmErrorKind::NoGfxChars(lexer.gfx_digits));
        }
        if width > 8 {
            // TODO: warning
        }
        Ok(u32::from(msb) << 8 | u32::from(lsb))
    }

    fn read_string_body(&mut self, multiline: bool) -> Result<(), ()> {
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
    fn read_specific_keyword(&mut self, first_char: char, expected: Keyword) -> bool {
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
    fn read_identifier(&mut self, first_char: char) -> Token {
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

/// The "main" lexing functions.
impl Tokenizer<'_, '_, '_, '_, '_> {
    fn next_normal(&mut self) -> Option<(Result<Token, AsmErrorKind>, usize)> {
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
                            Some(ch) if self.lexer.borrow().bin_digits.iter().any(|c| *c == ch) => {
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
                                                0,
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

    fn next_raw(&mut self) -> Option<(Result<Token, AsmErrorKind>, usize)> {
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

    // TODO: it'd be nicer to determine `end_len` automatically...
    fn next_capture_body(
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

/// The interface used by the parser.
impl<'fstack> Iterator for Tokenizer<'fstack, '_, '_, '_, '_> {
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
                    self.fstack.handle_end_of_node(&mut self.lexer.borrow_mut());
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
