use std::{fmt::Display, ops::Range};

use codespan_reporting::diagnostic::{Label, LabelStyle};
use lalrpop_util::lalrpop_mod;
use parse_display::Display;

mod lexer;
pub use lexer::{Lexer, Location, Tokenizer};
lalrpop_mod!(parser, "/asm/language/parser.rs");
pub use parser::TranslationUnitParser as Parser;
mod tokens;
use tokens::Token;

use crate::{expr::EvalError, input::SourceString};

pub type ParseError<'fstack> =
    lalrpop_util::ParseError<Location<'fstack>, Token, AsmError<'fstack>>;

#[derive(Debug)]
pub struct AsmError<'fstack> {
    pub begin: Location<'fstack>,
    pub end: Location<'fstack>,
    pub kind: AsmErrorKind,
}

impl<'fstack> AsmError<'fstack> {
    fn new(begin: Location<'fstack>, end: Location<'fstack>, kind: AsmErrorKind) -> Self {
        Self { begin, end, kind }
    }
}

#[derive(Debug, Display)]
pub enum AsmErrorKind {
    // Lexer errors.
    #[display("Syntax error: unexpected '{0}'")]
    BadChar(char),

    // Syntax errors.
    #[display("Syntax error: unexpected '{0}' at the beginning of the line")]
    DiffMark(char),
    #[display("Syntax error: invalid token")]
    InvalidToken,
    #[display("Syntax error: unexpected end-of-file")]
    UnrecognizedEOF(Vec<String>),
    #[display("Syntax error: unexpected {0}")]
    UnrecognizedToken(Token, Vec<String>),
    #[display("Syntax error: unexpected {0}")]
    ExtraToken(Token),
    #[display("Syntax error: unterminated block comment")]
    UnterminatedBlockComment,
    #[display("Syntax error: no hexadecimal digits found after '$'")]
    NoHexDigits,
    #[display("Syntax error: no graphics \"digits\" found after '`'")]
    NoGfxChars([char; 4]),

    // Semantic errors.
    #[display("{0} is already defined")]
    SymAlreadyDefined(SourceString, usize, Range<usize>),
    #[display("{0}")]
    EvalError(EvalError),
}

impl AsmErrorKind {
    pub fn notes(&self) -> Vec<String> {
        // TODO: ew, `String`s here instead of `Display`?
        match self {
            Self::BadChar(_) => vec![],
            Self::DiffMark(_) => vec!["Is it a leftover diff mark?".to_string()],
            Self::InvalidToken => vec![],
            Self::UnrecognizedEOF(expected) => {
                vec![format!("Expected {}", ExpectedTokens(expected))]
            }
            Self::UnrecognizedToken(_, expected) => {
                vec![format!("Expected {}", ExpectedTokens(expected))]
            }
            Self::ExtraToken(_) => vec![],
            Self::UnterminatedBlockComment => vec![],
            Self::NoHexDigits => vec![],
            Self::NoGfxChars([a, b, c, d]) => vec![format!(
                "The digits were {}, {}, {}, and {}",
                a.escape_debug(),
                b.escape_debug(),
                c.escape_debug(),
                d.escape_debug()
            )],
            Self::SymAlreadyDefined(_, _, _) => vec![],
            Self::EvalError(_) => vec![],
        }
    }

    pub fn labels(&self, labels: &mut Vec<Label<usize>>) {
        match self {
            Self::BadChar(_) => {}
            Self::DiffMark(_) => {}
            Self::InvalidToken => {}
            Self::UnrecognizedEOF(_) => {}
            Self::UnrecognizedToken(_, _) => {}
            Self::ExtraToken(_) => {}
            Self::UnterminatedBlockComment => {}
            Self::NoHexDigits => {}
            Self::NoGfxChars(_) => {}
            Self::SymAlreadyDefined(_, file_id, range) => {
                labels.extend_from_slice(&[Label {
                    style: LabelStyle::Secondary,
                    file_id: *file_id,
                    range: range.clone(),
                    message: "Previously defined here".into(),
                }]);
            }
            Self::EvalError(_) => {}
        }
    }
}

struct ExpectedTokens<'a>(&'a Vec<String>);

impl Display for ExpectedTokens<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0.last() {
            None => write!(f, "nothing"),
            Some(last) if self.0.len() == 1 => write!(f, "a {}", last),
            Some(last) => {
                for token in &self.0[..self.0.len() - 1] {
                    write!(f, "a {}, ", token)?;
                }
                write!(f, "or a {}", last)
            }
        }
    }
}
