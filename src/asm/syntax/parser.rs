/*! A manually-implemented rgbasm language parser.

Historically, we have used a parser generator ([Bison]), and written rgbasm's language as a LR(1) grammar: [\[1\]].
*Unfortunately*, rgbasm's grammar is not context-free, so this actually requires several lexer hacks (where the parser communicates information back to the lexer).

We originally tried [using a LR(1) parser generator][rgbasm-lalrpop], but this turned to require *heavily* working around its lookahead behaviour, and extensive use of [`RefCell`][std::cell::RefCell], to the point of unmaintainability.
So instead, this takes the approach of a manually-written parser, which can handle all of those edge cases much more sanely.

The extra boilerplate is counter-balanced by how much the aforementioned workarounds required, and the input syntax was fairly well-understood before we switched to a manual parser, so we can expect that there shouldn't be any grammar ambiguities.

[\[1\]]: https://github.com/gbdev/rgbds/blob/15919e550ffe4461e3c7d908897db324d48500a6/src/asm/parser.y
[Bison]: https://www.gnu.org/software/bison/
[rgbasm-lalrpop]: https://github.com/ISSOtm/rsgbds/blob/4cd81e2920b71b335f4be744adcc3f307bdd5fd7/src/asm/language/parser.lalrpop
*/

use std::{cell::Cell, rc::Rc};

use crate::{
    context_stack::{ContextStack, Span},
    diagnostics::{self, warning},
    macro_args::MacroArgs,
    source_store::{SourceHandle, SourceStore},
    symbols::Symbols,
    syntax::{
        lexer,
        tokens::{tok, Token, TokenPayload},
    },
    Options,
};

macro_rules! expect_one_of {
    ($payload:expr => {
        $( None => $if_none:expr, )?
        $( $($token:ident is)? $($kind:tt)|+ => $then:expr, )+
    } $( else { $default_ident:ident => $default:expr } )?) => {match $payload {
        $( None => $if_none, )?
        $( Some($($token @)? Token {
            payload: $(tok!($kind))|+,
            ..
        }) => $then, )+
        $(
            $default_ident => $default,
            #[allow(unreachable_patterns)] // Since the above is a catch-all, the next pattern will be unreachable.
        )?
        _ => eprintln!("Syntax error"),
    }}
}

pub fn parse_file(
    ctx_stack: &ContextStack,
    sources: &SourceStore,
    source: SourceHandle,
    symbols: &mut Symbols,
    nb_errors_remaining: &Cell<usize>,
    options: &Options,
) {
    ctx_stack.sources_mut().push_file_context(source);
    while ctx_stack.sources_mut().active_context().is_some() {
        while let Some(mut first_token) =
            lexer::next_token(ctx_stack, sources, symbols, nb_errors_remaining, options)
        {
            // TODO: handle directives that cannot be preceded by a label def.
            // TODO: handle leading diff marks
            // TODO: handle Git conflict markers
            if let TokenPayload::Identifier(ident) = first_token.payload {
                // Identifiers at the beginning of the line can be two things.
                // Either a label name, if it's *directly* followed by a colon; or the name of a macro.
                if lexer::is_next_char_a_colon(
                    ctx_stack,
                    sources,
                    symbols,
                    nb_errors_remaining,
                    options,
                ) {
                    expect_one_of!(lexer::next_token(
                        ctx_stack,
                        sources,
                        symbols,
                        nb_errors_remaining,
                        options
                    ) => {
                        ":" => {
                            // TODO
                            eprintln!("Defining label {}", symbols.resolve(ident));
                            match lexer::next_token(ctx_stack, sources, symbols, nb_errors_remaining, options) {
                                Some(token) => first_token = token,
                                None => break,
                            };
                        },
                        "::" => {
                            // TODO
                            eprintln!("Defining exported label {}", symbols.resolve(ident));
                            match lexer::next_token(ctx_stack, sources, symbols, nb_errors_remaining, options) {
                                Some(token) => first_token = token,
                                None => break,
                            };
                        },
                    } else {
                        // Try continuing the parse with this token as the first in the line.
                        token => first_token = token.unwrap()
                    });
                }
            }

            match first_token.payload {
                TokenPayload::Newline => {} // Empty line.

                TokenPayload::Identifier(ident) => {
                    // Macro call.
                    // Get the macro's arguments.
                    let args = Rc::new(MacroArgs::new(
                        std::iter::from_fn(|| {
                            lexer::next_raw(
                                ctx_stack,
                                sources,
                                symbols,
                                nb_errors_remaining,
                                options,
                            )
                        })
                        .collect(),
                    ));

                    // TODO: consume the newline, if `next_raw` doesn't already?

                    let name = symbols.resolve(ident);
                    match symbols.find_macro_interned(&ident) {
                        None => diagnostics::error(
                            &first_token.span,
                            |error| {
                                error
                                    .with_message(format!("Macro `{name}` does not exist"))
                                    .with_label(
                                        diagnostics::error_label(first_token.span.resolve())
                                            .with_message("Attempting to call the macro here"),
                                    )
                            },
                            sources,
                            nb_errors_remaining,
                            options,
                        ),
                        Some(Err(other)) => diagnostics::error(
                            &first_token.span,
                            |error| {
                                error
                                    .with_message(format!("`{name}` is not a macro"))
                                    .with_label(
                                        diagnostics::error_label(first_token.span.resolve())
                                            .with_message("Attempting to call the macro here"),
                                    )
                            },
                            sources,
                            nb_errors_remaining,
                            options,
                        ),
                        Some(Ok(slice)) => {
                            ctx_stack
                                .sources_mut()
                                .push_macro_context(name.into(), slice, args)
                        }
                    }
                }

                // These are not valid after a label.
                TokenPayload::Macro => todo!(),
                TokenPayload::Endm => todo!(),
                TokenPayload::Rept => todo!(),
                TokenPayload::For => todo!(),
                TokenPayload::Endr => todo!(),
                TokenPayload::If => todo!(),
                TokenPayload::Elif => todo!(),
                TokenPayload::Else => todo!(),
                TokenPayload::Endc => todo!(),

                TokenPayload::Adc => todo!(),
                TokenPayload::Add => todo!(),
                TokenPayload::And => todo!(),
                TokenPayload::Bit => todo!(),
                TokenPayload::Call => todo!(),
                TokenPayload::Ccf => todo!(),
                TokenPayload::Cp => todo!(),
                TokenPayload::Cpl => todo!(),
                TokenPayload::Daa => todo!(),
                TokenPayload::Dec => todo!(),
                TokenPayload::Di => todo!(),
                TokenPayload::Ei => todo!(),
                TokenPayload::Halt => todo!(),
                TokenPayload::Inc => todo!(),
                TokenPayload::Jp => todo!(),
                TokenPayload::Jr => todo!(),
                TokenPayload::Ldd => todo!(),
                TokenPayload::Ldh => todo!(),
                TokenPayload::Ldi => todo!(),
                TokenPayload::Ld => todo!(),
                TokenPayload::Nop => todo!(),
                TokenPayload::Or => todo!(),
                TokenPayload::Pop => todo!(),
                TokenPayload::Push => todo!(),
                TokenPayload::Res => todo!(),
                TokenPayload::Reti => todo!(),
                TokenPayload::Ret => todo!(),
                TokenPayload::Rla => todo!(),
                TokenPayload::Rlca => todo!(),
                TokenPayload::Rlc => todo!(),
                TokenPayload::Rl => todo!(),
                TokenPayload::Rra => todo!(),
                TokenPayload::Rrca => todo!(),
                TokenPayload::Rrc => todo!(),
                TokenPayload::Rr => todo!(),
                TokenPayload::Rst => todo!(),
                TokenPayload::Sbc => todo!(),
                TokenPayload::Scf => todo!(),
                TokenPayload::Set => todo!(),
                TokenPayload::Sla => todo!(),
                TokenPayload::Sra => todo!(),
                TokenPayload::Srl => todo!(),
                TokenPayload::Stop => todo!(),
                TokenPayload::Sub => todo!(),
                TokenPayload::Swap => todo!(),
                TokenPayload::Xor => todo!(),

                TokenPayload::Align => todo!(),
                TokenPayload::Assert => todo!(),
                TokenPayload::Break => todo!(),
                TokenPayload::Charmap => todo!(),
                TokenPayload::Db => todo!(),
                TokenPayload::Dl => todo!(),
                TokenPayload::Ds => todo!(),
                TokenPayload::Dw => todo!(),
                TokenPayload::Endsection => todo!(),
                TokenPayload::Endl => todo!(),
                TokenPayload::Endu => todo!(),
                TokenPayload::Export => todo!(),
                TokenPayload::Fail => todo!(),
                TokenPayload::Fatal => todo!(),
                TokenPayload::Incbin => todo!(),
                TokenPayload::Include => todo!(),
                TokenPayload::Load => todo!(),
                TokenPayload::Newcharmap => todo!(),
                TokenPayload::Nextu => todo!(),
                TokenPayload::Opt => todo!(),
                TokenPayload::Popc => todo!(),
                TokenPayload::Popo => todo!(),
                TokenPayload::Pops => todo!(),
                TokenPayload::Println => todo!(),
                TokenPayload::Print => todo!(),
                TokenPayload::Purge => todo!(),
                TokenPayload::Pushc => todo!(),
                TokenPayload::Pusho => todo!(),
                TokenPayload::Pushs => todo!(),
                TokenPayload::Rb => todo!(),
                TokenPayload::Rw => todo!(),
                TokenPayload::Redef => todo!(),
                TokenPayload::Rsreset => todo!(),
                TokenPayload::Rsset => todo!(),
                TokenPayload::Section => todo!(),
                TokenPayload::Setcharmap => todo!(),
                TokenPayload::Shift => todo!(),
                TokenPayload::StaticAssert => todo!(),
                TokenPayload::Union => todo!(),
                TokenPayload::Warn => todo!(),

                _ => diagnostics::error(
                    &first_token.span,
                    |error| {
                        error
                            .with_message(format!(
                                "Syntax error: unexpected {}",
                                &first_token.payload
                            ))
                            .with_label(
                                diagnostics::error_label(first_token.span.resolve())
                                    .with_message("Expected an instruction or a directive here"),
                            )
                    },
                    sources,
                    nb_errors_remaining,
                    options,
                ),
            }
        }

        // We're done parsing from this context, so end it.
        // (This will make REPT/FOR loop if possible, and pop everything else.)
        ctx_stack.sources_mut().end_current_context();
    }
}
