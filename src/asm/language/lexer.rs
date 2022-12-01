use std::{cell::RefCell, ops::Deref, rc::Rc};

use crate::{
    fstack::{Fstack, Node, NodeHandle},
    input::SourceString,
    language::tokens::can_start_ident,
    symbols::{SymbolData, SymbolKind, Symbols},
};

use super::{
    tokens::{Token, TrieIndex, TrieIter, KEYWORD_TRIE},
    AsmError, AsmErrorKind,
};

/// Unlike state in the [`Fstack`], state in the `Lexer` does not persist once the context is exited.
#[derive(Debug)]
pub struct Lexer {
    states: Vec<State>,

    bin_digits: [char; 2],
    gfx_digits: [char; 4],
    pub expand_strings: bool,
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
    source: Rc<SourceString>,
    offset: usize,
    /// How many bytes long the text that triggered this expansion is.
    parent_skip: usize,
}

#[derive(Debug, Clone, Copy)]
pub enum Mode {
    Normal,
    Raw,
}

impl Lexer {
    pub fn new() -> Self {
        Self {
            states: vec![State::new()],
            bin_digits: ['0', '1'],
            gfx_digits: ['0', '1', '2', '3'],
            expand_strings: true, // Enabled by default.
            mode: Mode::Normal,
        }
    }

    fn cur_state(&self) -> &State {
        self.states
            .last()
            .expect("Lexer states should never be empty")
    }

    fn cur_state_mut(&mut self) -> &mut State {
        self.states
            .last_mut()
            .expect("Lexer states should never be empty")
    }
}

impl State {
    fn new() -> Self {
        Self {
            offset: 0,
            expansions: Vec::new(), // This doesn't allocate.
        }
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
pub struct Tokenizer<'fstack, 'lexer, 'syms> {
    fstack: &'fstack Fstack,
    lexer: &'lexer RefCell<Lexer>,
    symbols: &'syms RefCell<Symbols<'fstack>>,

    // These are fine here because they are always both false when a new state is pushed.
    expand_macro_args: bool,
    enable_interpolation: bool,

    /// If `Some()`, shifted characters are "captured" into this string.
    capture: Option<SourceString>, // Capture shouldn't be active when changing states.
    /// Set to `false` when capture begins, and to `true` every time the "expansion level" changes.
    ///
    /// If `true` and a character must be captured, then the capture is made "owned" instead of zero-copy.
    capture_disrupted: bool,

    /// Is the lexer at the beginning of the line?
    /// (If `true`, the lexer will next generate a "BOL" token.)
    inject_lookahead_hack: bool,
}

// A couple of properties.

fn is_whitespace(c: char) -> bool {
    matches!(c, ' ' | '\t')
}

impl<'fstack, 'lexer, 'syms> Tokenizer<'fstack, 'lexer, 'syms> {
    pub fn new(
        fstack: &'fstack Fstack,
        lexer: &'lexer RefCell<Lexer>,
        symbols: &'syms RefCell<Symbols<'fstack>>,
    ) -> Self {
        Self {
            fstack,
            lexer,
            symbols,

            expand_macro_args: true,    // Enabled by default.
            enable_interpolation: true, // Enabled by default.

            capture: None,            // Disabled by default.
            capture_disrupted: false, // Does not really matter.

            inject_lookahead_hack: false,
        }
    }
}

/// Helper functions.
impl Tokenizer<'_, '_, '_> {
    fn cur_root_offset(&self) -> usize {
        self.lexer.borrow().cur_state().offset
    }

    fn with_cur_node<T, F: FnOnce(&Node) -> T>(&self, f: F) -> T {
        self.fstack
            .cur_node()
            .expect("Should have an active fstack node")
            .with_node(f)
    }

    fn try_read_backslash_expansion<It: Iterator<Item = char>>(
        mut iter: It,
    ) -> Option<(Rc<SourceString>, usize)> {
        match iter.next()? {
            '0'..='9' => Some(todo!()),
            '@' => Some(todo!()),
            '#' => Some(todo!()),
            '<' => {
                todo!();
            }
            _ => None,
        }
    }

    fn begin_expansion(lexer: &mut Lexer, source: Rc<SourceString>, trigger_len: usize) {
        lexer.cur_state_mut().expansions.push(Expansion {
            source,
            offset: 0,
            parent_skip: trigger_len,
        })
    }
}

/// The "character stream" functions.
impl Tokenizer<'_, '_, '_> {
    fn peek(&mut self) -> Option<char> {
        self.with_cur_node(|node| {
            Some(loop {
                let mut lexer = self.lexer.borrow_mut();
                let cur_state = lexer.cur_state();
                let source = match cur_state
                    .expansions
                    .iter()
                    .rfind(|expansion| !expansion.has_ended())
                {
                    Some(expansion) => &expansion.source[expansion.offset..],
                    None => &node.as_ref()[cur_state.offset..],
                };
                let mut chars = source.chars();

                let c = match chars.next()? {
                    '\\' if self.expand_macro_args => {
                        if let Some((source, trigger_len)) =
                            Self::try_read_backslash_expansion(chars)
                        {
                            // Don't bother doing the expensive work for empty expansions.
                            if !source.is_empty() {
                                Self::begin_expansion(&mut lexer, source, trigger_len);
                            }
                            continue;
                        }
                        '\\' // If it doesn't introduce a macro arg, then just return it.
                    }
                    '{' if self.enable_interpolation => {
                        todo!();
                    }
                    c => c,
                };
                break c;
            })
        })
    }

    /// Do not call this directly, use either [`shift()`][Self::shift()] or [`shift_capture()`][Self::shift_capture()].
    fn shift_internal(&mut self) -> char {
        let mut lexer = self.lexer.borrow_mut();

        let mut skip = 0;
        let (cur_ofs, c) = loop {
            break if let Some(expansion) = lexer.cur_state_mut().expansions.last_mut() {
                if expansion.has_ended() {
                    debug_assert_eq!(skip, 0, "Cannot apply offset to ended expansion!?");

                    skip = expansion.parent_skip;
                    lexer.cur_state_mut().expansions.pop();
                    self.capture_disrupted = true;
                    continue; // Retry with the parent.
                }
                let offset = expansion.offset;
                (
                    &mut expansion.offset,
                    expansion.source[offset..].chars().next(),
                )
            } else {
                let offset = &mut lexer.cur_state_mut().offset;
                let ofs = *offset;
                self.with_cur_node(|node| (offset, node.as_ref()[ofs..].chars().next()))
            };
        };

        let c = c.expect("Cannot shift at EOF!?");
        *cur_ofs += skip + c.len_utf8();
        c
    }

    /// Use to "accept" a character while capture is not active.
    fn shift(&mut self) {
        self.shift_internal();

        #[cfg(debug_assertions)]
        if let Some(capture) = &self.capture {
            panic!("Unexpectedly capturing!? (capture = {capture:?})");
        }
    }

    /// Use to "accept" a character while capture is active.
    /// `should_capture` specifies whether the shifted character must be appended to the capture.
    fn shift_capture(&mut self, should_capture: bool) {
        let c = self.shift_internal();

        let capture = self.capture.as_mut().expect("Unexpectedly not capturing!?");
        if should_capture {
            if self.capture_disrupted && !SourceString::is_owned(capture) {
                SourceString::make_owned(capture);
            }
            SourceString::push(capture, c);
        } else {
            self.capture_disrupted = true;
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
            Some(expansion) => SourceString::clone_empty(&expansion.source),
            None => self.with_cur_node(|node| node.slice(start_ofs..start_ofs)),
        });
        self.capture_disrupted = false;
    }

    fn end_capture(&mut self) -> SourceString {
        self.capture
            .take()
            .expect("Lexer ending capture without starting it!?")
    }
}

/// Lexing sub-functions.
impl Tokenizer<'_, '_, '_> {
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
            self.shift_capture(true);
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
            self.shift();

            if ch == '/' && self.peek() == Some('*') {
                todo!();
            } else if ch == '*' && self.peek() == Some('/') {
                self.shift();
                break Ok(());
            }
        };

        self.expand_macro_args = true;
        self.enable_interpolation = true;

        res
    }

    fn read_anon_label_ref(&mut self, first_char: char) -> u32 {
        let mut n = 1;

        while self.peek() == Some(first_char) {
            self.shift();
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
                value = value * radix + digit;
            } else {
                break;
            }
            self.shift();
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
            self.shift();
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
            self.shift();
        }

        if width == 0 {
            return Err(AsmErrorKind::NoGfxChars(lexer.gfx_digits));
        }
        if width > 8 {
            // TODO: warning
        }
        Ok(u32::from(msb) << 8 | u32::from(lsb))
    }

    fn read_string(&mut self, multiline: bool) -> SourceString {
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

        self.start_capture();
        let unterminated = loop {
            macro_rules! append {
                ($ch:expr) => {
                    SourceString::push(self.capture.as_mut().unwrap(), $ch);
                };
            }

            match self.peek() {
                Some('"') => {
                    if multiline {
                        // A single quote is not enough, we need three in a row.
                        self.shift_capture(false);
                        let Some('"') = self.peek() else {
                            append!('"');
                            continue;
                        };
                        self.shift_capture(false);
                        let Some('"') = self.peek() else {
                            append!('"');
                            append!('"');
                            continue;
                        };
                    }
                    break false;
                }

                None => break true,
                Some('\n') if !multiline => break true,

                // Special characters.
                Some('\\') => {
                    self.shift_capture(false);
                    match self.peek() {
                        Some('\\' | '"' | '{' | '}') => self.shift_capture(true),
                        Some('n') => {
                            append!('\n');
                            self.shift_capture(false);
                        }
                        Some('r') => {
                            append!('\r');
                            self.shift_capture(false);
                        }
                        Some('t') => {
                            append!('\t');
                            self.shift_capture(false);
                        }
                        _ => todo!(),
                    }
                }

                // Other characters get appended normally.
                Some(_) => self.shift_capture(true),
            }
        };

        if unterminated {
            todo!(); // Report an error
        }
        self.expand_macro_args = true;
        self.enable_interpolation = true;

        let string = self.end_capture();
        self.shift(); // The closing quote.
        if multiline {
            // The last two characters are quotes, so we need to trim them off.
            todo!();
        }
        string
    }

    fn read_identifier(&mut self, first_char: char) -> Token {
        self.start_capture();
        let (mut is_local, mut state) = match TrieIndex::try_from(first_char) {
            Ok(idx) => (false, TrieIter::new(&KEYWORD_TRIE).next(idx)),
            Err(_) => (true, None),
        };
        debug_assert_eq!(is_local, first_char == '.');
        self.shift_capture(true);

        while let Some(ch) = self.peek() {
            if ch == '.' {
                is_local = true;
                state = None;
            } else if let Ok(idx) = TrieIndex::try_from(ch) {
                state = state.and_then(|state| state.next(idx));
            } else {
                break;
            }
            self.shift_capture(true);
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
impl Tokenizer<'_, '_, '_> {
    fn next_normal(&mut self) -> Option<(Result<Token, AsmErrorKind>, usize)> {
        macro_rules! try_chars {
            ($default:expr $(, $ch:pat => $result:expr)+ $(,)?) => {
                match self.peek() {
                    $( Some($ch) => { self.shift(); $result } )+
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
                        self.discard_comment();
                        continue;
                    }
                    c if is_whitespace(c) => {
                        self.shift();
                        continue;
                    }

                    // Unambiguous one-char tokens.
                    '~' => {
                        self.shift();
                        Ok(Token::Complement)
                    }

                    '[' => {
                        self.shift();
                        Ok(Token::LeftBracket)
                    }
                    ']' => {
                        self.shift();
                        Ok(Token::RightBracket)
                    }
                    '(' => {
                        self.shift();
                        Ok(Token::LeftParens)
                    }
                    ')' => {
                        self.shift();
                        Ok(Token::RightParens)
                    }

                    ',' => {
                        self.shift();
                        Ok(Token::Comma)
                    }

                    // One- or two-char tokens.
                    '+' => {
                        self.shift();
                        Ok(try_chars!(Token::Plus, '=' => Token::AddEq))
                    }
                    '-' => {
                        self.shift();
                        Ok(try_chars!(Token::Minus, '=' => Token::SubEq))
                    }
                    '*' => {
                        self.shift();
                        Ok(try_chars!(Token::Mul, '=' => Token::MulEq, '*' => Token::Exponent))
                    }
                    '/' => {
                        self.shift();
                        Ok(try_chars!(Token::Div, '=' => Token::DivEq, '*' => {
                            match self.discard_block_comment() {
                                Ok(()) => continue,
                                Err(kind) => break Err(kind),
                            }
                        }))
                    }
                    '|' => {
                        self.shift();
                        Ok(try_chars!(Token::BitOr, '=' => Token::OrEq, '|' => Token::LogicOr))
                    }
                    '^' => {
                        self.shift();
                        Ok(try_chars!(Token::BitXor, '=' => Token::XorEq))
                    }
                    '=' => {
                        self.shift();
                        Ok(try_chars!(Token::Eq, '=' => Token::LogicEq))
                    }
                    '!' => {
                        self.shift();
                        Ok(try_chars!(Token::LogicNot, '=' => Token::LogicNe))
                    }

                    // One-, two-, or three-char tokens.
                    '<' => {
                        self.shift();
                        Ok(try_chars!(Token::Lt, '=' => Token::Lte, '<' => {
                            try_chars!(Token::Shl, '=' => Token::ShlEq)
                        }))
                    }

                    // One-, two-, three-, or four-char tokens.
                    '>' => {
                        self.shift();
                        Ok(try_chars!(Token::Gt, '=' => Token::Gte, '>' => {
                            try_chars!(Token::Shr, '=' => Token::ShrEq, '>' => {
                                try_chars!(Token::UShr, '=' => Token::UShrEq)
                            })
                        }))
                    }

                    // The colon may be just that, a double-colon, or an anonymous label ref.
                    ':' => {
                        self.shift();
                        Ok(match self.peek() {
                            Some(':') => {
                                self.shift();
                                Token::DoubleColon
                            }
                            Some(c @ ('+' | '-')) => {
                                self.shift();
                                Token::AnonLabelRef(self.read_anon_label_ref(c), c == '-')
                            }
                            _ => Token::Colon,
                        })
                    }

                    // Numbers.
                    c @ '0'..='9' => {
                        self.shift();
                        let int_part = self.read_number(c.to_digit(10).unwrap(), 10);
                        Ok(if self.peek() == Some('.') {
                            todo!();
                        } else {
                            Token::Number(int_part)
                        })
                    }
                    '&' => {
                        self.shift();
                        Ok(match self.peek() {
                            Some('=') => {
                                self.shift();
                                Token::AndEq
                            }
                            Some('&') => {
                                self.shift();
                                Token::LogicAnd
                            }
                            Some('0'..='7') => {
                                self.shift();
                                Token::Number(self.read_number(c.to_digit(8).unwrap(), 8))
                            }
                            _ => Token::BitAnd,
                        })
                    }
                    '%' => {
                        self.shift();
                        Ok(match self.peek() {
                            Some('=') => {
                                self.shift();
                                Token::ModEq
                            }
                            Some(ch)
                                if self
                                    .lexer
                                    .borrow()
                                    .bin_digits
                                    .iter()
                                    .find(|c| **c == ch)
                                    .is_some() =>
                            {
                                self.shift();
                                Token::Number(self.read_bin_number(ch))
                            }
                            _ => Token::Mod,
                        })
                    }
                    '$' => {
                        self.shift();
                        if let Some(base_value) = self.peek().and_then(|ch| ch.to_digit(16)) {
                            Ok(Token::Number(self.read_number(base_value, 16)))
                        } else {
                            Err(AsmErrorKind::NoHexDigits)
                        }
                    }
                    '`' => {
                        self.shift();
                        self.read_gfx_constant().map(Token::Number)
                    }

                    // Strings.
                    '"' => {
                        self.shift();
                        let multiline = if self.peek() == Some('"') {
                            self.shift();
                            if self.peek() == Some('"') {
                                self.shift();
                                true
                            } else {
                                todo!(); // Generate an empty string.
                            }
                        } else {
                            false
                        };
                        Ok(Token::String(self.read_string(multiline)))
                    }

                    // Newline.
                    '\r' => {
                        self.shift();
                        todo!();
                    }
                    '\n' => {
                        self.shift();
                        Ok(Token::Newline)
                    }

                    // Line continuations.
                    '\\' => {
                        // Macro args are handled by `peek`, and character escapes do not exist outside of string literals, so this must be a line continuation.
                        self.shift();
                        todo!();
                    }

                    // Identifiers.
                    '@' => {
                        self.start_capture();
                        self.shift();
                        Ok(Token::Identifier(self.end_capture()))
                    }
                    c if can_start_ident(c) || c == '.' => {
                        let token = self.read_identifier(c);

                        // TODO: make ELIF after evaluated IF skip the condition

                        Ok(match token {
                            Token::Identifier(name) => {
                                if self.lexer.borrow().expand_strings {
                                    if let Some(equs) = self.symbols.borrow().get_string(&name) {
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

                                // TODO: maybe return T_LABEL (BOL, or peek() == ':')

                                // Macros and "protected" identifiers need an action to happen right after the identifier.
                                self.inject_lookahead_hack = true;
                                Token::Identifier(name)
                            }
                            Token::Def => {
                                self.inject_lookahead_hack = true; // Macros need this.
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

impl<'fstack> Iterator for Tokenizer<'fstack, '_, '_> {
    type Item = Result<(Location<'fstack>, Token, Location<'fstack>), AsmError<'fstack>>;

    fn next(&mut self) -> Option<Self::Item> {
        let cur_node = self.fstack.cur_node()?;

        let mode = self.lexer.borrow().mode;
        let (res, start_offset) = if std::mem::replace(&mut self.inject_lookahead_hack, false) {
            (Ok(Token::LookaheadHack), self.cur_root_offset())
        } else {
            match mode {
                Mode::Normal => self.next_normal(),
                Mode::Raw => todo!(),
            }?
        };
        let start_location = Location {
            storage: Some(cur_node.clone()),
            offset: start_offset,
        };
        let end_location = Location {
            storage: Some(cur_node.clone()),
            offset: self.cur_root_offset(),
        };
        Some(match res {
            Ok(token) => Ok((start_location, token, end_location)),
            Err(kind) => Err(AsmError::new(start_location, end_location, kind)),
        })
    }
}
