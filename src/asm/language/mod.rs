use std::{
    fmt::{Display, Write},
    ops::Range,
    write,
};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use lalrpop_util::lalrpop_mod;
use parse_display::Display;
use rgbds::{rpn::EvalError, section::Kind as SectionKind};

mod lexer;
pub use lexer::{Lexer, Location, Tokenizer};
lalrpop_mod!(parser, "/asm/language/parser.rs");
pub use parser::TranslationUnitParser as Parser;
mod tokens;
use tokens::Token;
use warnings_gen::Warnings;

use crate::{input::SourceString, instructions::BadInstructionKind};

pub type ParseError<'fstack> =
    lalrpop_util::ParseError<Location<'fstack>, Token, AsmError<'fstack>>;

#[derive(Debug, Warnings)]
#[warning(id_enum = "WarningId")]
pub enum WarningKind {
    /// Assertions
    #[warning(default = true)]
    Assert,
    /// `for` loop with backwards range
    #[warning(default = false)]
    BackwardsFor,
    /// Invalid args to builtins
    #[warning(default = false)]
    BuiltinArg,
    /// Charmap entry re-definition
    #[warning(default = false)]
    CharmapRedef,
    /// Division undefined behavior
    #[warning(default = false)]
    Div,
    /// `db`, `dw` or `dl` directive without data in ROM
    #[warning(default = false)]
    EmptyDataDirective,
    /// Empty macro argument
    #[warning(default = false)]
    EmptyMacroArg,
    /// Empty second argument in `STRRPL`
    #[warning(default = false)]
    EmptyStrrpl,
    /// Constants too large
    #[warning(default = false)]
    LargeConstant,
    /// Shift past available arguments in macro
    #[warning(default = false)]
    MacroShift,
    // TODO: ideally, this would also report where the block comment started
    /// Block-comment-start delimiter in a block comment
    #[warning(default = true)]
    NestedBlockComment,
    /// Treating string as number may lose some bits
    #[warning(default = 1, max = 2)]
    NumericString { level: u8, len: usize },
    /// Obsolete things
    #[warning(default = true)]
    Obsolete(ObsoleteKind),
    /// Shifting undefined behavior
    #[warning(default = false)]
    Shift,
    /// Strange shift amount
    #[warning(default = false)]
    ShiftAmount,
    /// Implicit truncation loses some bits
    #[warning(default = 1, max = 2)]
    Truncation { level: u8, width: u8 },
    /// Character without charmap entry
    #[warning(default = 1, max = 2)]
    UnmappedChar { level: u8, ch: char },
    /// User warnings
    #[warning(default = true)]
    User(SourceString),

    #[warning(meta(
        BackwardsFor,
        BuiltinArg,
        CharmapRedef,
        EmptyDataDirective,
        EmptyStrrpl,
        LargeConstant,
        LongStr,
        NestedComment,
        Obsolete,
        NumericString1,
        UnmappedChar1
    ))]
    All,
    #[warning(meta(
        EmptyMacroArg,
        MacroShift,
        NumericString2,
        Truncation1,
        Truncation2,
        UnmappedChar2
    ))]
    Extra,
    #[warning(meta(Everything))]
    Everything,
}

impl Display for WarningKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Assert => todo!(),
            Self::BackwardsFor => todo!(),
            Self::BuiltinArg => todo!(),
            Self::CharmapRedef => todo!(),
            Self::Div => todo!(),
            Self::EmptyDataDirective => todo!(),
            Self::EmptyMacroArg => write!(f, "Empty macro argument"),
            Self::EmptyStrrpl => todo!(),
            Self::LargeConstant => todo!(),
            Self::MacroShift => todo!(),
            Self::NestedBlockComment => write!(f, "\"/*\" within block comment"),
            Self::NumericString { level, len } => match level {
                1 => write!(
                    f,
                    "Charmap conversion of this string to a number ignores the first {} character{}",
                    len - 4,
                    Plural(len - 4)
                ),
                2 => write!(f, "This {len}-character string is charmap-converted to a number"),
                _ => unreachable!(),
            },
            Self::Obsolete(kind) => kind.fmt(f),
            Self::Shift => todo!(),
            Self::ShiftAmount => todo!(),
            Self::Truncation { level: _, width } => write!(f, "This expression does not fit in {width} bits"),
            Self::UnmappedChar { level, ch } => match level {
                1 => write!(f, "Character '{}' is not in charmap", ch.escape_default()),
                2 => write!(f, "Character '{}' is not in charmap {}", ch.escape_default(), todo!()),
                _ => unreachable!(),
            },
            Self::User(msg) => write!(f, "{msg}"),

            Self::All | Self::Extra | Self::Everything => unreachable!(),
        }
    }
}

#[derive(Debug, Display)]
pub enum ObsoleteKind {
    #[display("`ld [c], a` is deprecated and will be removed in an upcoming version")]
    LdCA,
    #[display("`ld a, [c]` is deprecated and will be removed in an upcoming version")]
    LdAC,
}

#[derive(Debug, Display)]
pub enum SymEvalErrKind {
    #[display("Symbol \"{0}\" does not exist")]
    NoSuchSymbol(SourceString),
    #[display("RGBASM cannot compute the value of \"{0}\" at this point")]
    NonConst(SourceString), // TODO: say where it was defined?
    #[display("\"{0}\" is not a numeric symbol")]
    NotNumeric(SourceString),
    #[display("\"_NARG\" is only defined inside of macros")]
    NargOutsideMacro,
    #[display("\"@\" is only defined within a section")]
    PcOutsideSection,
}

#[derive(Debug, Display)]
pub enum AsmErrorKind {
    // Lexer errors.
    #[display("Syntax error: unexpected '{0}'")]
    BadChar(char),
    #[display("Syntax error: macro argument is being used outside of a macro")]
    NoActiveMacro,
    #[display("Syntax error: macro argument '\\0' does not exist")]
    NoMacroArg0,
    #[display("Syntax error: macro argument '\\{0}' is not defined")]
    NoMacroArg(u32),

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
    #[display("Syntax error: unterminated macro")]
    UnterminatedMacro,
    #[display("Syntax error: unterminated string literal")]
    UnterminatedString,
    #[display("Syntax error: no hexadecimal digits found after '$'")]
    NoHexDigits,
    #[display("Syntax error: no graphics \"digits\" found after '`'")]
    NoGfxChars([char; 4]),
    #[display("Cannot escape '{0}'")]
    IllegalEscape(char),
    #[display("Character being escaped is missing")]
    IllegalEscapeEof,
    #[display("Invalid instruction: {0}")]
    BadInstruction(BadInstructionKind),

    // Semantic errors.
    #[display("{0} is already defined")]
    SectAlreadyDefined(SourceString, Option<(usize, Range<usize>)>),
    #[display("{0} is already defined")]
    SymAlreadyDefined(SourceString, Option<(usize, Range<usize>)>),
    #[display("Only labels can be local")]
    IllegalLocal,
    #[display("Symbol \"{0}\" does not exist")]
    NoSuchSymbol(SourceString),
    // TODO: report the actual kind as "help"
    #[display("Symbol \"{0}\" is not a string constant")]
    SymNotEqus(SourceString),
    #[display("Symbol \"{0}\" is not a macro")]
    SymNotMacro(SourceString),
    #[display("Built-in symbol \"{0}\" cannot be purged")]
    PurgingBuiltin(SourceString),
    #[display("Symbol \"{0}\" is referenced and thus cannot be purged")]
    PurgingReferenced(SourceString),
    #[display("{0}")]
    EvalError(EvalError<SymEvalErrKind>),

    // Section specification errors.
    #[display("An address must be in 16-bit range, not ${0:04x}")]
    AddrOutOfRange(i32),
    #[display("Alignment must be between 0 and 16 (inclusive), not {0}")]
    AlignOutOfRange(i32),
    #[display(
        "Alignment offset (${0:02x}) must not be greater than the alignment boundary (${1:02x})"
    )]
    AlignOfsOutOfRange(i32, i32),
    #[display("BANK[...] is not allowed for {0} sections")]
    Unbanked(SectionKind),
    #[display("Bank number (${0:04x}) must be between ${1:02x} and ${2:02x}")]
    BankOutOfRange(u32, u32, u32),
    #[display("Address ${0:04x} must be between ${1:04x} and ${2:04x} inclusive")]
    AddrOutOfBounds(u16, u16, u16),
    #[display("Address ${0:04x} is incompatible with ALIGN[{1}, ${2:02x}]")]
    AlignMismatch(u16, u8, u16),
    #[display("{0}-bit alignment is impossible for {1} sections")]
    OverAligned(u8, SectionKind),

    // Data output errors.
    #[display("Data found outside of any section")]
    DataOutsideSection, // TODO: show the `PUSHS` that reset the section scope, or print help text warning that no section was ever started (suggest starting one either way)
    #[display("Instruction found outside of any section")]
    InstrOutsideSection,
    #[display("Only ROM0 and ROMX sections can contain data, not {0}")]
    NotCodeSection(SectionKind),
}

impl WarningKind {
    pub fn notes(&self) -> Vec<String> {
        // TODO: ew, `String`s here instead of `Display`?
        match self {
            Self::Obsolete(kind) => kind.notes(),

            Self::All | Self::Extra | Self::Everything => unreachable!(),
            _ => vec![],
        }
    }

    pub fn labels(&self, labels: &mut Vec<Label<usize>>) {
        match self {
            _ => {}
        }
    }
}

impl From<ObsoleteKind> for WarningKind {
    fn from(value: ObsoleteKind) -> Self {
        Self::Obsolete(value)
    }
}

impl ObsoleteKind {
    fn notes(&self) -> Vec<String> {
        match self {
            Self::LdCA => vec![format!("Please use `ldh [c], a` instead")],
            Self::LdAC => vec![format!("Please use `ldh a, [c]` instead")],
        }
    }
}

impl AsmErrorKind {
    pub fn notes(&self) -> Vec<String> {
        // TODO: ew, `String`s here instead of `Display`?
        match self {
            Self::DiffMark(_) => vec!["Is it a leftover diff mark?".to_string()],
            Self::UnrecognizedEOF(expected) => {
                vec![format!("Expected {}", ExpectedTokens(expected))]
            }
            Self::UnrecognizedToken(_, expected) => {
                vec![format!("Expected {}", ExpectedTokens(expected))]
            }
            Self::NoGfxChars([a, b, c, d]) => vec![format!(
                "The digits were {}, {}, {}, and {}",
                a.escape_debug(),
                b.escape_debug(),
                c.escape_debug(),
                d.escape_debug()
            )],
            Self::BadInstruction(kind) => kind.notes(),
            Self::Unbanked(..) => vec![
                "BANK[...] is only allowed for ROMX, VRAM, SRAM, and WRAMX sections".to_string(),
            ],
            Self::AlignMismatch(addr, align, _) => vec![format!(
                "ALIGN[{align}, {}] would work",
                addr & ((1 << align) - 1)
            )],

            _ => vec![],
        }
    }

    pub fn labels(&self, labels: &mut Vec<Label<usize>>) {
        match self {
            Self::SectAlreadyDefined(_, prev_def_info) => {
                if let Some((file_id, range)) = prev_def_info {
                    labels.push(
                        Label::secondary(*file_id, range.clone())
                            .with_message("Previously defined here"),
                    );
                }
            }
            Self::SymAlreadyDefined(_, prev_def_info) => {
                if let Some((file_id, range)) = prev_def_info {
                    labels.extend_from_slice(&[Label::secondary(*file_id, range.clone())
                        .with_message("Previously defined here")]);
                }
            }

            _ => {}
        }
    }

    pub fn report_help<F: FnOnce(&Diagnostic<usize>)>(&self, report: F) {
        match self {
            Self::BadInstruction(kind) => kind.report_help(report),

            _ => {}
        }
    }
}

impl From<EvalError<SymEvalErrKind>> for AsmErrorKind {
    fn from(value: EvalError<SymEvalErrKind>) -> Self {
        Self::EvalError(value)
    }
}

impl From<BadInstructionKind> for AsmErrorKind {
    fn from(value: BadInstructionKind) -> Self {
        Self::BadInstruction(value)
    }
}

#[derive(Debug)]
pub struct Warning<'fstack> {
    pub begin: Location<'fstack>,
    pub end: Location<'fstack>,
    pub kind: WarningKind,
}

#[derive(Debug)]
pub struct AsmError<'fstack> {
    pub begin: Location<'fstack>,
    pub end: Location<'fstack>,
    pub kind: AsmErrorKind,
}

impl<'fstack> AsmError<'fstack> {
    pub fn new(begin: Location<'fstack>, end: Location<'fstack>, kind: AsmErrorKind) -> Self {
        Self { begin, end, kind }
    }
}

struct ExpectedTokens<'a>(&'a Vec<String>);

impl Display for ExpectedTokens<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0.last() {
            None => write!(f, "nothing"),
            Some(last) if self.0.len() == 1 => write!(f, "{}", ExpectedToken(last)),
            Some(last) => {
                for token in &self.0[..self.0.len() - 1] {
                    write!(f, "{}, ", ExpectedToken(token))?;
                }
                write!(f, "or {}", ExpectedToken(last))
            }
        }
    }
}

struct ExpectedToken<'a>(&'a String);

// TODO: it'd be nicer if we could format the token names ourselves. Oh well.
impl Display for ExpectedToken<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0.chars().next().expect("Empty token name!?") {
            '"' => write!(f, "{}", self.0),
            'a' | 'i' => write!(f, "an {}", self.0),
            _ => write!(f, "a {}", self.0),
        }
    }
}

struct Plural<T>(T);

impl<T: From<u8> + PartialEq<T>> Display for Plural<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.0 != T::from(1) {
            f.write_char('s')
        } else {
            Ok(())
        }
    }
}
