/*! The innermost part of the language's processing.

`rgbasm`'s lexer is unusual because of the language's design.
In particular, macro arguments (`\1` etc.) and interpolation (`{DUCK}`) work at a textual level; this is enforced by their semantics.

The first (known) design of the lexer handled them in the various token functions, but this meant that there was a lot of duplicated handling, and this caused a lot of bugs ([#63], [#362], [#531]...)
Instead, we handle them at the lowest level, thus handling them in a manner transparent to the rest of the lexer.

TODO: describe the "char stream" / remainder split more

[#63]: https://github.com/gbdev/rgbds/issues/63
[#362]: https://github.com/gbdev/rgbds/issues/362
[#531]: https://github.com/gbdev/rgbds/issues/531
 */

use std::{cell::Cell, num::NonZeroUsize, ops::Range};

use ariadne::Label;
use compact_str::CompactString;
use uncased::UncasedStr;

use crate::{
    context_stack::{ContextStack, SourceContext, SourceNode, SourceRef, SourcesMut, Span},
    diagnostics,
    format::FormatSpec,
    source_store::{ReportBuilder, SourceHandle, SourceStore},
    symbols::Symbols,
    tokens::{tok, Token, TokenPayload, KEYWORDS},
    Options,
};

#[derive(Debug)]
pub struct LexerState {
    expansions: Vec<Expansion>,
    /// How many of the chars yet to be consumed have been scanned for expansions.
    /// This is similar to the colloquial "blue paint" of the C preprocessor.
    ///
    /// Scanning for expansions is idempotent, so in theory we do not need to store this; however, macro args present a special case.
    /// The second backslash out of two (`\\`) should not trigger macro args; for example, `\\1` should *not* expand,
    /// even though after consuming the first backslash, we're left with `\1`, which *does* look like a macro arg.
    ///
    /// Likewise, macro args cannot contain expansions (this is by design), so they are immediately marked as "scanned" when expanded.
    nb_scanned_for_expansion: usize,

    cursor: usize,
}

#[derive(Debug)]
struct Expansion {
    name: Option<CompactString>,
    contents: CompactString,
    cursor: usize,
}

impl LexerState {
    pub fn new() -> Self {
        Self {
            expansions: Vec::new(),
            nb_scanned_for_expansion: 0,
            cursor: 0,
        }
    }

    fn active_expansion(&self) -> Option<&str> {
        self.expansions
            .iter()
            .map(|expansion| &expansion.contents[expansion.cursor..])
            .find(|buf| !buf.is_empty())
    }

    fn active_buffer<'a>(&'a self, source: &'a str) -> &'a str {
        self.active_expansion().unwrap_or(&source[self.cursor..])
    }

    #[must_use = "Do not increase the recursion depth if this function returns `true`!"]
    fn check_recursion_depth(&self, options: &Options) -> bool {
        self.expansions.len() >= options.recursion_depth
    }

    fn begin_expansion(
        &mut self,
        name: Option<CompactString>,
        contents: CompactString,
        options: &Options,
    ) {
        if name.is_some() && self.check_recursion_depth(options) {
            return;
        }

        // Do not expand empty strings (we'd just skip over that buffer anyway).
        if contents.is_empty() {
            return;
        }
        self.expansions.push(Expansion {
            name,
            contents,
            cursor: 0,
        });
    }
}

macro_rules! chars {
    (newline) => {'\r' | '\n'};
    (whitespace) => {' ' | '\t'};
    (starts_ident) => {'A'..='Z' | 'a'..='z' | '.' | '_'};
    (ident) => {chars!(starts_ident) | '0'..='9' | '#' | '$' | '@'};
    (starts_macro_arg) => {'@' | '#' | '<' | '1'..='9'};
}

/// Parameters that remain constant throughout a call to `next_token`.
/// This is used to pass all of them at once, for simplicity.
struct LexParams<'ctx_stack, 'src_store, 'syms, 'sym_ctx_stack, 'errs_rem, 'options> {
    ctx_stack: &'ctx_stack ContextStack,
    src_ctx: &'ctx_stack mut SourceContext,
    source_store: &'src_store SourceStore,
    node: &'ctx_stack SourceNode,
    source: &'src_store str,
    symbols: &'syms mut Symbols<'sym_ctx_stack>,
    nb_errors_remaining: &'errs_rem Cell<usize>,
    options: &'options Options,
}

pub fn next_token<'ctx_stack>(
    ctx_stack: &'ctx_stack mut ContextStack,
    source_store: &SourceStore,
    symbols: &mut Symbols, // We need to be able to intern names, and to resolve them for expansion.
    nb_errors_remaining: &Cell<usize>,
    options: &Options,
) -> Option<Token<'ctx_stack>> {
    let mut sources = ctx_stack.sources_mut();
    let (src_ctx, nodes) = sources.active_context_mut()?; // If there are no more contexts, no more tokens can be produced.
    let node = src_ctx.node(nodes);
    let mut parameters = LexParams {
        ctx_stack,
        src_ctx,
        source_store,
        node,
        source: node.source(source_store),
        symbols,
        nb_errors_remaining,
        options,
    };
    let params = &mut parameters;

    Some(loop {
        let ch = peek(params, true, true)?;
        let start = loc(params.src_ctx); // All tokens start *now*; even if `make!` is recursively invoked.

        macro_rules! make {
            ($kind:expr) => {{
                consume_char(params.src_ctx, ch);
                // Since there is no need to dispatch, don't bother `peek`ing.
                Token {
                    payload: $kind,
                    span: span(start, loc(params.src_ctx), ctx_stack, &mut sources),
                }
            }};
            ($default:expr $(, $extra:pat => $with_extra:expr)+ $(,)?) => {{
                consume_char(params.src_ctx, ch);
                match peek(params, true, true) {
                    $(Some(extra_ @ $extra) => {
                        consume_char(params.src_ctx, extra_);
                        #[allow(unreachable_code)] // Sometimes, `payload` is a diverging expression. But that's intended!
                        Token {
                            payload: $with_extra,
                            span: span(start, loc(params.src_ctx), ctx_stack, &mut sources),
                        }
                    })+
                    _ => Token {
                        payload: $default,
                        span: span(start, loc(params.src_ctx), ctx_stack, &mut sources),
                    }
                }
            }};
        }

        match ch {
            ';' => {
                discard_comment(params);
                continue;
            }
            // TODO: try to discard whitespace in bulk.
            //       We could advance through the buffers/expansions, ignoring leading whitespace.
            //       Such a function would also be useful in other places, I think?
            chars!(whitespace) => {
                consume_char(params.src_ctx, ch);
                continue;
            } // Just ignore whitespace.

            // Newline.
            '\r' => {
                handle_crlf(params);

                break Token {
                    span: span(start, loc(params.src_ctx), ctx_stack, &mut sources),
                    payload: tok!("end of line"),
                };
            }
            '\n' => break make!(tok!("end of line")),

            // Unambiguous single-char tokens.
            '~' => break make!(tok!("~")),
            '@' => break make!(tok!("identifier")(params.symbols.intern_name("@"))),
            '[' => break make!(tok!("[")),
            ']' => break make!(tok!("]")),
            '(' => break make!(tok!("(")),
            ')' => break make!(tok!(")")),
            ',' => break make!(tok!(",")),

            // 1- or 2-char tokens.
            '+' => break make!(tok!("+"), '=' => tok!("+=")),
            '-' => break make!(tok!("-"), '=' => tok!("-=")),
            '*' => {
                break make!(tok!("*"), '=' => tok!("*="), '*' => tok!("**"), '/' => { error_block_comment_term(params, start); continue; });
            }
            '/' => {
                break make!(tok!("/"), '=' => tok!("/="), '*' => { discard_block_comment(params, start); continue; });
            }
            '|' => break make!(tok!("|"), '=' => tok!("|="), '|' => tok!("||")),
            '^' => break make!(tok!("^"), '=' => tok!("^=")),
            '=' => break make!(tok!("="), '=' => tok!("==")),
            '!' => break make!(tok!("!"), '=' => tok!("!=")),

            '<' => {
                break make!(tok!("<"), '=' => tok!("<="),
                    '<' => break make!(tok!("<<"), '=' => tok!("<<=")),
                )
            }
            '>' => {
                break make!(tok!(">"), '=' => tok!(">="),
                    '>' => break make!(tok!(">>"), '=' => tok!(">>="),
                        '>' => break make!(tok!(">>>"), '=' => tok!(">>>=")),
                    )
                )
            }
            ':' => break make!(tok!(":"), ':' => tok!("::"), '-' | '+' => todo!()),

            '0'..='9' => {
                consume_char(params.src_ctx, ch);

                let number = read_number(params, ch, 10, start);
                if peek(params, true, true) == Some('.') {
                    todo!();
                }
                break make!(tok!("number")(number));
            }

            '&' => {
                break make!(tok!("&"), '=' => tok!("&="), '&' => tok!("&&"), ch @ '0'..='7' => tok!("number")(read_number(params, ch, 8, start)));
            }
            '%' => {
                consume_char(params.src_ctx, '%');

                break Token {
                    payload: match peek(params, true, true) {
                        Some('=') => tok!("%="),
                        Some(c) if params.options.binary_digits.contains(&c) => tok!("number")(
                            read_dyn_number(params, c, &params.options.binary_digits, start),
                        ),
                        _ => tok!("%"),
                    },
                    span: span(start, loc(params.src_ctx), ctx_stack, &mut sources),
                };
            }
            '$' => {
                consume_char(params.src_ctx, ch);

                if let Some(first_char) = peek(params, true, true) {
                    if first_char.is_ascii_hexdigit() {
                        break make!(tok!("number")(read_number(params, first_char, 16, start)));
                    }
                }
            }
            '`' => {
                consume_char(params.src_ctx, ch);

                if let Some(first_char) = peek(params, true, true) {
                    if params.options.gfx_chars.contains(&first_char) {
                        break make!(tok!("number")(read_dyn_number(
                            params,
                            first_char,
                            &params.options.gfx_chars,
                            start
                        )));
                    }
                }
            }

            '"' => break make!(read_string(params, start, false)),

            // Macro args were dealt with by `peek`, so if we still see one at this stage,
            // the only valid possibility is a line continuation.
            '\\' => todo!(),

            '#' => {
                consume_char(params.src_ctx, '#');

                if let Some(payload) = match peek(params, true, true) {
                    Some('"') => Some(read_string(params, start, true)),
                    Some(chars!(starts_ident)) => Some(read_ident(params, true, true)),
                    _ => None,
                } {
                    break Token {
                        payload,
                        span: span(start, loc(params.src_ctx), ctx_stack, &mut sources),
                    };
                }
            }
            chars!(starts_ident) => {
                break Token {
                    payload: read_ident(params, false, true),
                    span: span(start, loc(params.src_ctx), ctx_stack, &mut sources),
                };
            }

            _ => consume_char(params.src_ctx, ch),
        }

        // This point is only reached by invalid characters.
        // TODO: how to recover from these?
        //       Discard until the next stand-alone punctuation (e.g. comma) or whitespace?
        //       Currently, each bad character generates an individual diagnostic, which gets floody quickly.
        params.error(start, |error, span| {
            error.with_message("Unexpected character").with_label(
                diagnostics::error_label(span).with_message(
                    "This does not belong to any of the tokens expected at this point",
                ),
            )
        });
    })
}

/// This assumes that the leading semicolon has *not* been shifted yet.
fn discard_comment(params: &mut LexParams<'_, '_, '_, '_, '_, '_>) {
    debug_assert_eq!(peek(params, false, false), Some(';'));
    let mut ch = ';';

    loop {
        consume_char(params.src_ctx, ch);
        match peek(params, false, false) {
            None | Some(chars!(newline)) => break,
            Some(c) => ch = c,
        }
    }
}

fn discard_block_comment(
    params: &mut LexParams<'_, '_, '_, '_, '_, '_>,
    start: (NonZeroUsize, usize),
) {
    let start_marker_end = params.src_ctx.lexer_state().cursor;
    loop {
        match peek(params, false, false) {
            None => {
                params.error_at(start.1..start_marker_end, |error, span| {
                    error.with_message("Unterminated block comment").with_label(
                        diagnostics::error_label(span).with_message("The comment starts here"),
                    )
                });
                break;
            }
            Some('*') => {
                consume_char(params.src_ctx, '*');

                if let Some(ch) = peek(params, false, false) {
                    consume_char(params.src_ctx, ch);

                    if ch == '/' {
                        break; // Matched `*/`!
                    }
                }
            }
            Some('/') => {
                let marker_start = params.src_ctx.lexer_state().cursor;
                consume_char(params.src_ctx, '/');

                if let Some(ch) = peek(params, false, false) {
                    consume_char(params.src_ctx, ch);

                    if ch == '*' {
                        params.warning_at(
                            marker_start..params.src_ctx.lexer_state().cursor,
                            |warning, span| {
                                let source_handle = span.0;
                                warning
                                    .with_message("Block comment opening marker found inside of a block comment")
                                    .with_labels([
                                        diagnostics::warning_label(span)
                                            .with_message("This opening marker..."),
                                        diagnostics::warning_label((source_handle, start.1..start_marker_end))
                                            .with_message("...is inside of the block comment opened here"),
                                    ])
                            },
                        );
                    }
                }
            }
            Some(ch) => consume_char(params.src_ctx, ch),
        }
    }
}

fn error_block_comment_term(
    params: &mut LexParams<'_, '_, '_, '_, '_, '_>,
    start: (NonZeroUsize, usize),
) {
    params.error(start, |error, span| {
        error
            .with_message("Found a block comment's end marker outside of a block comment")
            .with_label(diagnostics::error_label(span))
    });
}

fn read_number(
    params: &mut LexParams<'_, '_, '_, '_, '_, '_>,
    first_char: char,
    radix: u32,
    start: (NonZeroUsize, usize),
) -> i32 {
    let mut number = Some(first_char.to_digit(radix).unwrap()); // Should be checked by the caller.
    while let Some(mut ch) = peek(params, true, true) {
        if ch == '_' {
            let underscore_start = loc(params.src_ctx);
            consume_char(params.src_ctx, ch);
            match peek(params, true, true) {
                Some(next) if next.is_digit(radix) => ch = next,
                _ => {
                    params.error(underscore_start, |error, span| {
                        error
                            .with_message("Trailing underscore in number literal")
                            .with_label(
                                diagnostics::error_label(span)
                                    .with_message("Expected a digit after this `_`"),
                            )
                    });
                    break;
                }
            }
        }

        if let Some(digit) = ch.to_digit(radix) {
            consume_char(params.src_ctx, ch);
            number = number
                .and_then(|num| num.checked_mul(radix))
                .and_then(|num| num.checked_add(digit));
        } else {
            break;
        }
    }
    make_number_token(params, number, start)
}

fn read_dyn_number(
    params: &mut LexParams<'_, '_, '_, '_, '_, '_>,
    first_char: char,
    digits: &[char],
    start: (NonZeroUsize, usize),
) -> i32 {
    let to_digit = |ch| digits.iter().position(|&c| c == ch).map(|idx| idx as u32);

    let mut number = Some(to_digit(first_char).unwrap()); // Should be checked by the caller.
    while let Some(mut ch) = peek(params, true, true) {
        if ch == '_' {
            let underscore_start = loc(params.src_ctx);
            consume_char(params.src_ctx, ch);
            match peek(params, true, true) {
                Some(next) if to_digit(next).is_some() => ch = next,
                _ => {
                    params.error(underscore_start, |error, span| {
                        error
                            .with_message("Trailing underscore in number literal")
                            .with_label(
                                diagnostics::error_label(span)
                                    .with_message("Expected a digit after this `_`"),
                            )
                    });
                    break;
                }
            }
        }

        if let Some(digit) = to_digit(ch) {
            consume_char(params.src_ctx, ch);
            number = number
                .and_then(|num| num.checked_mul(digits.len() as u32))
                .and_then(|num| num.checked_add(digit));
        } else {
            break;
        }
    }
    make_number_token(params, number, start)
}

fn make_number_token(
    params: &mut LexParams<'_, '_, '_, '_, '_, '_>,
    number: Option<u32>,
    start: (NonZeroUsize, usize),
) -> i32 {
    number.unwrap_or_else(|| {
        params.error(start, |error, span| {
            error
                .with_message("Number literals are only supported up to 4_294_967_295")
                .with_label(
                    diagnostics::error_label(span).with_message("This number literal is too large"),
                )
        });
        u32::MAX // Saturate.
    }) as i32
}

fn read_string(
    params: &mut LexParams<'_, '_, '_, '_, '_, '_>,
    start: (NonZeroUsize, usize),
    is_raw: bool,
) -> TokenPayload {
    // We reach this function after reading a single double-quote, but possibly there might be two more,
    // which would start a multiline string.
    let multiline = if let Some('"') = peek(params, false, false) {
        consume_char(params.src_ctx, '"');
        if let Some('"') = peek(params, false, false) {
            consume_char(params.src_ctx, '"');
            true
        } else {
            // Two consecutive quotes just mean an empty string.
            return tok!("string")(CompactString::default());
        }
    } else {
        false
    };
    let open_marker_end = params.src_ctx.lexer_state().cursor;

    let mut string = CompactString::default();
    loop {
        let Some(ch) = peek(params, false, false).and_then(|ch| {
            if !multiline && matches!(ch, chars!(newline)) {
                None
            } else {
                Some(ch)
            }
        }) else {
            params.error_at(start.1..open_marker_end, |error, span| {
                error.with_message("Unterminated string").with_label(
                    diagnostics::error_label(span).with_message("The string starts here"),
                )
            });
            break;
        };

        match ch {
            '\r' => {
                // Note: this is only possible for multiline strings.
                consume_char(params.src_ctx, '\r');
                handle_crlf(params);
                string.push('\n'); // For platform independency, only push LF even if CRLF.
            }
            '"' => {
                consume_char(params.src_ctx, '"');

                if !multiline {
                    break;
                }
                // Multi-line strings need three quotes to be terminated.
                if peek(params, false, false) == Some('"') {
                    // Two quotes...
                    consume_char(params.src_ctx, '"');
                    if peek(params, false, false) == Some('"') {
                        // Three!
                        consume_char(params.src_ctx, '"');
                        break;
                    }
                    string.push('"');
                }
                string.push('"');
            }
            '\\' if !is_raw => {
                // Escape character, or macro argument.
                todo!()
            }
            '{' if !is_raw => {
                // Interpolation.
                todo!()
            }
            ch => {
                consume_char(params.src_ctx, ch);
                string.push(ch);
            }
        }
    }

    tok!("string")(string)
}

fn read_ident(
    params: &mut LexParams<'_, '_, '_, '_, '_, '_>,
    is_raw: bool,
    with_expansions: bool,
) -> TokenPayload {
    let mut name = CompactString::default();
    while let Some(ch) = peek(params, with_expansions, with_expansions) {
        consume_char(params.src_ctx, ch);

        let chars!(ident) = ch else {
            break;
        };
        name.push(ch);
    }

    if !is_raw {
        if let Some(keyword) = KEYWORDS.get(name.as_str().into()) {
            return keyword.clone();
        }
    }
    tok!("identifier")(params.symbols.intern_name(name))
}

/// Assuming that a `\r` has just been consumed, consumes a `\n` if it is the next char.
fn handle_crlf(params: &mut LexParams<'_, '_, '_, '_, '_, '_>) {
    // We explicitly don't use `peek`, because we only care about CRLF within a single buffer.
    if params
        .src_ctx
        .lexer_state()
        .active_buffer(params.source)
        .starts_with('\n')
    {
        consume_char(params.src_ctx, '\n');
    }
}

fn read_interpolation(
    params: &mut LexParams<'_, '_, '_, '_, '_, '_>,
) -> Option<(CompactString, CompactString)> {
    fn inner(
        params: &mut LexParams<'_, '_, '_, '_, '_, '_>,
        remaining_depth: usize,
    ) -> Option<(CompactString, CompactString)> {
        // The opening brace that got us here should not have been consumed yet.
        debug_assert_eq!(peek(params, false, false), Some('{'));
        let start = loc(params.src_ctx);
        consume_char(params.src_ctx, '{');

        if remaining_depth == 0 {
            let mut depth = 1; // From the opening brace that got us here.
            while depth != 0 {
                let Some(ch) = peek(params, false, false) else {
                    break;
                };
                match ch {
                    '{' => depth += 1,
                    '}' => depth -= 1,
                    _ => {} // Just consume other chars.
                };
                consume_char(params.src_ctx, ch);
            }
            params.error(start, |error, span| {
                error
                    .with_message(format!(
                        "Recursion limit ({}) exceeded",
                        params.options.recursion_depth
                    ))
                    .with_label(
                        diagnostics::error_label(span)
                            .with_message("Trying to expand this interpolation"),
                    )
            });
            return None;
        }

        let mut fmt = FormatSpec::new();
        let mut buf = CompactString::default();
        loop {
            // We will expand interpolations manually, since otherwise `peek` calls this function
            // recursively without checking for the depth.
            match peek(params, true, false) {
                Some('{') => {
                    // Nested expansion.
                    if let Some((name, contents)) = inner(params, remaining_depth - 1) {
                        params.src_ctx.lexer_state_mut().begin_expansion(
                            Some(name),
                            contents,
                            params.options,
                        );
                    }
                }
                None | Some('\r' | '\n' | '"') => {
                    params.error(start, |error, (src_id, byte_range)| {
                        error.with_message("Unterminated interpolation").with_label(
                            diagnostics::error_label((
                                src_id,
                                byte_range.start..byte_range.start + 1, // Braces are 1-byte.
                            ))
                            .with_message("This brace is never closed"),
                        )
                    });
                    return None;
                }
                Some('}') => {
                    consume_char(params.src_ctx, '}');
                    break;
                }
                Some(':') => {
                    let start = loc(params.src_ctx);
                    consume_char(params.src_ctx, ':');
                    if fmt.is_finished() {
                        params.error(start, |error, (src_id, byte_range)| {
                            // TODO: would be nice to highlight the other colon
                            error
                                .with_message("A format spec has already been provided")
                                .with_label(
                                    diagnostics::error_label((
                                        src_id,
                                        byte_range.end - 1..byte_range.end,
                                    ))
                                    .with_message("Second colon here"),
                                )
                        });
                    } else {
                        todo!();
                    }
                }
                Some(c) => {
                    consume_char(params.src_ctx, c);
                    buf.push(c);
                }
            }
        }

        let (name, is_raw) = match buf.strip_prefix('#') {
            Some(rest) => (rest, true),
            None => (buf.as_str(), false),
        };
        // PC doesn't follow the usual naming rules, but is nonetheless a valid symbol.
        if name != "@" {
            let mut chars = name.chars();
            match chars.next() {
                None => {
                    params.error(start, |error, (src_id, byte_range)| {
                        error
                            .with_message("Missing symbol name in this interpolation")
                            .with_label(
                                diagnostics::error_label((
                                    src_id,
                                    byte_range.end - 1..byte_range.end,
                                ))
                                .with_message("Missing before this brace"),
                            )
                    });
                    return None;
                }
                Some(chars!(starts_ident)) => {} // OK!
                Some(c) => {
                    params.error(start, |error, span| {
                        error
                            .with_message("Attempting to interpolate an invalid symbol name")
                            .with_label(diagnostics::error_label(span).with_message(format!(
                                "The character `{c}` cannot begin a symbol name"
                            )))
                    });
                    return None;
                }
            }
            if let Some(c) = chars.find(|c| !matches!(c, chars!(ident))) {
                params.error(start, |error, span| {
                    error
                        .with_message("Attempting to interpolate an invalid symbol name")
                        .with_label(diagnostics::error_label(span).with_message(format!(
                            "The character `{c}` is not allowed inside of a symbol name"
                        )))
                });
                return None;
            }

            if !is_raw && KEYWORDS.contains_key(UncasedStr::new(name)) {
                params.error(start, |error, span| {
                    error
                        .with_message(format!("The name `{name}` is a reserved keyword"))
                        .with_help("Add a `#` prefix to use it as a raw symbol name instead")
                });
                return None;
            }
        }

        // TODO: would be nice to use `with_capacity`, but that depends on the contents of the symbol u_u
        let mut contents = CompactString::default();
        // Interning must be done separately, as otherwise `params` remains *mutably* borrowed inside of the `match`.
        let interned_name = params.symbols.get_interned_name(name);
        match params.symbols.format_as(
            interned_name,
            &fmt,
            &mut contents,
            params.src_ctx.macro_args(),
        ) {
            Ok(()) => Some((buf, contents)),
            Err(err) => {
                params.error(start, |error, span| {
                    use crate::symbols::FormatError;

                    let mut error = error.with_message(format!(
                        "Cannot interpolate symbol `{name}`: {}",
                        match &err {
                            FormatError::NotFound | FormatError::Deleted(..) => "it does not exist",
                            FormatError::BadKind =>
                                "only numeric and string symbols can be interpolated",
                        },
                    ));
                    error.add_label(diagnostics::error_label(span));
                    if let FormatError::Deleted(location) = err {
                        // TODO: would be nice to point out *where* using a label.
                        //       That would require cloning `location` through our borrowed `ctx_stack`, though!
                        error.set_note("The symbol was deleted earlier");
                    }
                    error
                });
                None
            }
        }
    }
    // TODO: would be nice if the depth was cumulated with the `context`'s own depth?
    inner(params, params.options.recursion_depth)
}

fn peek(
    params: &mut LexParams<'_, '_, '_, '_, '_, '_>,
    with_macro_args: bool,
    with_interpolation: bool,
) -> Option<char> {
    loop {
        let state = params.src_ctx.lexer_state_mut();
        let buffer = state.active_buffer(params.source);
        let mut chars = buffer.chars();

        let ch = chars.next();
        // Do not try to perform expansions if the character has been already scanned for them.
        // This is especially important because expansions are *not* idempotent.
        if state.nb_scanned_for_expansion > 0 {
            break ch;
        }
        // We are about to scan that character for expansions, so don't do it again.
        state.nb_scanned_for_expansion += 1;

        match ch {
            Some('\\') if with_macro_args => {
                todo!()
            }
            Some('{') if with_interpolation => {
                if let Some((name, contents)) = read_interpolation(params) {
                    // The lexer state should be unchanged, but we cannot carry the `state` borrow
                    // from `params` across the call to `read_interpolation`.
                    params.src_ctx.lexer_state_mut().begin_expansion(
                        Some(name),
                        contents,
                        params.options,
                    );
                }
                continue;
            }
            ch => break ch,
        }
    }
}

fn consume_char(context: &mut SourceContext, ch: char) {
    let state = context.lexer_state_mut();

    state.nb_scanned_for_expansion -= 1;

    while let Some(expansion) = state.expansions.last_mut() {
        debug_assert!(expansion.cursor <= expansion.contents.len());

        // If we had reached the end of that expansion, remove it from the list (and keep looping).
        // It may seems bizarre not to perform this check *after* incrementing `cursor`, but there is a reason.
        // If we did that, then an expansion that expands to exactly itself (e.g. `DEF recurse EQUS "recurse"`)
        // would always end *right before* the new expansion is pushed onto the stack; this would cause
        // an infinite loop at a constant stack depth; that would be bad.
        // So we keep expansions in the list for one extra character.
        // TODO: instead of this, just clear empty expansions at the beginning of `next_token`? This may also improve error reporting.
        if expansion.cursor == expansion.contents.len() {
            state.expansions.pop();
        } else {
            expansion.cursor += ch.len_utf8();
            return;
        }
    }

    // Advance within the buffer's contents.
    state.cursor += ch.len_utf8();
}

fn loc(context: &SourceContext) -> (NonZeroUsize, usize) {
    (context.source_id(), context.lexer_state().cursor)
}
fn span<'ctx_stack>(
    start: (NonZeroUsize, usize),
    end: (NonZeroUsize, usize),
    ctx_stack: &'ctx_stack ContextStack,
    sources: &mut SourcesMut<'_>,
) -> Span<'ctx_stack> {
    debug_assert_eq!(start.0, end.0);
    Span {
        src: Some(SourceRef::new_via(ctx_stack, start.0, sources)),
        byte_span: start.1..end.1,
    }
}
impl LexParams<'_, '_, '_, '_, '_, '_> {
    fn error_at<F: FnOnce(ReportBuilder, (SourceHandle, Range<usize>)) -> ReportBuilder>(
        &self,
        byte_range: Range<usize>,
        build: F,
    ) {
        diagnostics::lex_error(
            self.node,
            &byte_range,
            build,
            self.source_store,
            self.nb_errors_remaining,
            self.options,
        );
    }

    fn error<F: FnOnce(ReportBuilder, (SourceHandle, Range<usize>)) -> ReportBuilder>(
        &self,
        start: (NonZeroUsize, usize),
        build: F,
    ) {
        let end = self.src_ctx.lexer_state().cursor;
        self.error_at(start.1..end, build);
    }

    fn warning_at<F: FnOnce(ReportBuilder, (SourceHandle, Range<usize>)) -> ReportBuilder>(
        &self,
        byte_range: Range<usize>,
        build: F,
    ) {
        todo!();
    }

    fn warning<F: FnOnce(ReportBuilder, (SourceHandle, Range<usize>)) -> ReportBuilder>(
        &self,
        start: (NonZeroUsize, usize),
        build: F,
    ) {
        let end = self.src_ctx.lexer_state().cursor;
        self.warning_at(start.1..end, build);
    }
}
