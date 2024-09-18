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

use std::cell::Cell;

use crate::{
    context_stack::ContextStack,
    lexer,
    source_store::{SourceHandle, SourceStore},
    symbols::Symbols,
    Options,
};

pub fn parse_file(
    ctx_stack: &mut ContextStack,
    sources: &SourceStore,
    source: SourceHandle,
    symbols: &mut Symbols,
    nb_errors_remaining: &Cell<usize>,
    options: &Options,
) {
    ctx_stack.sources_mut().push_file_context(source);
    while ctx_stack.sources_mut().active_context().is_some() {
        while let Some(token) =
            lexer::next_token(ctx_stack, sources, symbols, nb_errors_remaining, options)
        {
            dbg!(token.payload);
        }

        // We're done parsing from this context, so end it.
        // (This will make REPT/FOR loop if possible, and pop everything else.)
        ctx_stack.sources_mut().end_current_context();
    }
}
