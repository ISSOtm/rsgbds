use std::{fmt::Display, ops::Not};

use codespan_reporting::diagnostic::Diagnostic;
use parse_display::Display;

use crate::expr::Expression;

mod encoding;
pub use encoding::Encoder;

#[derive(Debug)]
pub enum Instruction<'fstack> {
    // Listed roughly in the order in https://gbdev.io/gb-opcodes/optables/darkoctal

    // 0x–7x
    // x0
    Nop,
    LdAddr16Sp(Expression<'fstack>),
    Stop(Expression<'fstack>),
    Jr(Expression<'fstack>),
    JrCond(Condition, Expression<'fstack>),
    // x1
    LdImm16(Reg16, Expression<'fstack>),
    AddHl(Reg16),
    // x2
    LdReg16IndA(Reg16Ind),
    LdAReg16Ind(Reg16Ind),
    // x3
    IncReg16(Reg16),
    DecReg16(Reg16),
    // x4
    IncReg8(Reg8),
    // x5
    DecReg8(Reg8),
    // x6
    LdImm8(Reg8, Expression<'fstack>),
    // x7
    Rlca,
    Rrca,
    Rla,
    Rra,
    Daa,
    Cpl,
    Scf,
    Ccf,

    // 10x–17x
    LdReg8Reg8(Reg8, Reg8),
    Halt,

    // 20x
    Add(Reg8),
    // 21x
    Adc(Reg8),
    // 22x
    Sub(Reg8),
    // 23x
    Sbc(Reg8),
    // 24x
    And(Reg8),
    // 25x
    Xor(Reg8),
    // 26x
    Or(Reg8),
    // 27x
    Cp(Reg8),

    // 30x–37x
    // x0
    RetCond(Condition),
    LdhAddr8A(Expression<'fstack>),
    AddSpRel8(Expression<'fstack>),
    LdhAAddr8(Expression<'fstack>),
    LdHlSpRel8(Expression<'fstack>),
    // x1
    Pop(Reg16Stack),
    Ret,
    Reti,
    JpHl,
    LdSpHl,
    // x2
    JpCond(Condition, Expression<'fstack>),
    LdhCA,
    LdAddr16A(Expression<'fstack>),
    LdhAC,
    LdAAddr16(Expression<'fstack>),
    // x3
    Jp(Expression<'fstack>),
    Di,
    Ei,
    // x4
    CallCond(Condition, Expression<'fstack>),
    // x5
    Push(Reg16Stack),
    Call(Expression<'fstack>),
    // x6
    AddImm8(Expression<'fstack>),
    AdcImm8(Expression<'fstack>),
    SubImm8(Expression<'fstack>),
    SbcImm8(Expression<'fstack>),
    AndImm8(Expression<'fstack>),
    XorImm8(Expression<'fstack>),
    OrImm8(Expression<'fstack>),
    CpImm8(Expression<'fstack>),
    // x7
    Rst(Expression<'fstack>),

    Prefixed(PrefixKind, Reg8),
}

#[derive(Debug)]
pub enum PrefixKind {
    Rlc,
    Rrc,
    Rl,
    Rr,
    Sla,
    Sra,
    Swap,
    Srl,
    Bit(u8),
    Res(u8),
    Set(u8),
}

type InstrResult<T> = Result<T, BadInstructionKind>;

macro_rules! alu {
    ($fn_name:ident => $variant:ident) => {
        pub fn $fn_name(dest: Option<Reg8>, src: Reg8) -> InstrResult<Self> {
            match dest {
                None | Some(Reg8::A) => Ok(Self::$variant(src)),
                Some(reg) => Err(BadInstructionKind::Alu(stringify!($fn_name), reg, src)),
            }
        }
    };
}
macro_rules! alu_imm {
    ($fn_name:ident => $variant:ident) => {
        pub fn $fn_name(dest: Option<Reg8>, src: Expression<'fstack>) -> InstrResult<Self> {
            match dest {
                None | Some(Reg8::A) => Ok(Self::$variant(src)),
                Some(reg) => Err(BadInstructionKind::AluImm(stringify!($fn_name), reg)),
            }
        }
    };
}
impl<'fstack> Instruction<'fstack> {
    pub fn ld_addr16_sp(addr: Expression<'fstack>, reg: Reg16) -> InstrResult<Self> {
        match reg {
            Reg16::Sp => Ok(Instruction::LdAddr16Sp(addr)),
            _ => Err(BadInstructionKind::ToAddr16),
        }
    }

    pub fn add16(lhs: Reg16Stack, rhs: Reg16) -> InstrResult<Self> {
        match lhs {
            Reg16Stack::Hl => Ok(Self::AddHl(rhs)),
            _ => Err(BadInstructionKind::Add16(lhs, rhs)),
        }
    }

    pub fn ld_reg16_ind_a(dest: Reg16Ind, src: Reg8) -> InstrResult<Self> {
        match src {
            Reg8::A => Ok(Self::LdReg16IndA(dest)),
            _ => Err(BadInstructionKind::ToReg16Ind(dest, Some(src))),
        }
    }

    pub fn ld_a_reg16_ind(dest: Reg8, src: Reg16Ind) -> InstrResult<Self> {
        match dest {
            Reg8::A => Ok(Self::LdAReg16Ind(src)),
            _ => Err(BadInstructionKind::FromReg16Ind(dest, src)),
        }
    }

    pub fn cpl(reg: Option<Reg8>) -> InstrResult<Self> {
        match reg {
            None | Some(Reg8::A) => Ok(Self::Cpl),
            Some(reg) => Err(BadInstructionKind::Cpl(reg)),
        }
    }

    pub fn ld_reg8_reg8(dest: Reg8, src: Reg8) -> InstrResult<Self> {
        match (dest, src) {
            (Reg8::HlInd, Reg8::HlInd) => Err(BadInstructionKind::LdHlHl),
            _ => Ok(Self::LdReg8Reg8(dest, src)),
        }
    }

    alu!(add => Add);
    alu!(adc => Adc);
    alu!(sub => Sub);
    alu!(sbc => Sbc);
    alu!(and => And);
    alu!(xor => Xor);
    alu!(or => Or);
    alu!(cp => Cp);

    pub fn ld_hram_a(dest: Expression<'fstack>, src: Reg8) -> InstrResult<Self> {
        match src {
            Reg8::A => Ok(Self::LdhAddr8A(dest)),
            _ => Err(BadInstructionKind::ToHram(Some(src))),
        }
    }

    pub fn ld_a_hram(dest: Reg8, src: Expression<'fstack>) -> InstrResult<Self> {
        match dest {
            Reg8::A => Ok(Self::LdhAAddr8(src)),
            _ => Err(BadInstructionKind::FromHram(dest)),
        }
    }

    pub fn jp(dest: Reg16) -> InstrResult<Self> {
        match dest {
            Reg16::Hl => Ok(Self::JpHl),
            _ => Err(BadInstructionKind::JpInd(dest)),
        }
    }

    pub fn ld_sp_reg16(src: Reg16Stack) -> InstrResult<Self> {
        match src {
            Reg16Stack::Hl => Ok(Self::LdSpHl),
            _ => Err(BadInstructionKind::LdSpReg16(src)),
        }
    }

    pub fn ldh_c_a(dest: Reg8, src: Reg8) -> InstrResult<Self> {
        match (dest, src) {
            (Reg8::C, Reg8::A) => Ok(Instruction::LdhCA),
            (Reg8::C, src) => Err(BadInstructionKind::LdhToC(src)),
            _ => todo!(),
        }
    }

    pub fn ld_addr16_a(dest: Expression<'fstack>, src: Reg8) -> InstrResult<Self> {
        match src {
            Reg8::A => Ok(Instruction::LdAddr16A(dest)),
            _ => Err(BadInstructionKind::ToAddr16),
        }
    }

    pub fn ldh_a_c(dest: Reg8, src: Reg8) -> InstrResult<Self> {
        match (dest, src) {
            (Reg8::A, Reg8::C) => Ok(Instruction::LdhAC),
            (dest, Reg8::C) => Err(BadInstructionKind::LdhFromC(dest)),
            _ => todo!(),
        }
    }

    pub fn ld_a_addr16(dest: Reg8, src: Expression<'fstack>) -> InstrResult<Self> {
        match dest {
            Reg8::A => Ok(Instruction::LdAAddr16(src)),
            _ => Err(BadInstructionKind::FromAddr16),
        }
    }

    alu_imm!(add_imm8 => AddImm8);
    alu_imm!(adc_imm8 => AdcImm8);
    alu_imm!(sub_imm8 => SubImm8);
    alu_imm!(sbc_imm8 => SbcImm8);
    alu_imm!(and_imm8 => AndImm8);
    alu_imm!(xor_imm8 => XorImm8);
    alu_imm!(or_imm8 => OrImm8);
    alu_imm!(cp_imm8 => CpImm8);
}

#[derive(Debug, Display)]
pub enum BadInstructionKind {
    #[display("only `a` and `sp` can be directly written to a fixed location")]
    ToAddr16,
    #[display("the left-hand register of a 16-bit `add` must be `hl`")]
    Add16(Reg16Stack, Reg16),
    #[display("only `a` can be written to `[{0}]`")]
    ToReg16Ind(Reg16Ind, Option<Reg8>),
    #[display("`[{1}]` can only be written to `a`")]
    FromReg16Ind(Reg8, Reg16Ind),
    #[display("`daa` already implies `a`")]
    Daa,
    #[display("`cpl` only supports `a`")]
    Cpl(Reg8),
    #[display("`ld [hl], [hl]` does not exist")]
    LdHlHl,
    #[display("the left-hand register of `{0}` must be `a`")]
    Alu(&'static str, Reg8, Reg8),
    #[display("`ldh` only supports `a`")]
    ToHram(Option<Reg8>),
    #[display("`ldh` only supports `a`")]
    FromHram(Reg8),
    #[display("`jp` only supports `hl` as its destination, not `{0}`")]
    JpInd(Reg16),
    #[display("`jr` does not support a register as its destination")]
    JrInd(Reg16),
    #[display("`sp` can only be copied from `hl` or from a constant")]
    LdSpReg16(Reg16Stack),
    #[display("conditional `jp` does not support a register as its destination")]
    JpCond(Condition, Reg16),
    #[display("`jr` does not support a register as its destination")]
    JrCond(Condition, Reg16),
    #[display("the contents of a fixed location can only be copied to `a`")]
    FromAddr16,
    #[display("`call` does not support a register as its destination")]
    CallInd(Reg16),
    #[display("conditional `call` does not support a register as its destination")]
    CallCond(Reg16),
    #[display("only `ldh [c], a` exists")]
    LdhToC(Reg8),
    #[display("only `ldh a, [c]` exists")]
    LdhFromC(Reg8),
    #[display("the left-hand register of `{0}` must be `a`")]
    AluImm(&'static str, Reg8),

    #[display("cannot copy `{src}` to `{dest}` directly")]
    LdReg16Reg16 { dest: Reg16Stack, src: Reg16Stack },
    #[display("cannot copy `sp` to `{0}`")]
    LdReg16Sp(Reg16Stack),
    #[display("cannot copy `sp` to itself")]
    LdSpSp,
}

impl BadInstructionKind {
    pub fn notes(&self) -> Vec<String> {
        match self {
            _ => vec![],
        }
    }

    pub fn report_help<F: FnOnce(&Diagnostic<usize>)>(&self, report: F) {
        match self {
            Self::Add16(lhs, rhs) => {
                if matches!(rhs, Reg16::Hl) {
                    report(&Diagnostic::help().with_message(format!(
                        "If you meant to add `{lhs}` to `hl`, use `add hl, {lhs}` instead"
                    )));
                }
            }
            Self::LdReg16Reg16 { dest, src } => {
                if let (Some(dest_low), Some(src_low)) = (dest.low(), src.low()) {
                    report(&Diagnostic::help().with_message(format!(
                        "Consider using `ld {}, {}` and `ld {}, {}` instead",
                        dest_low,
                        src_low,
                        dest.high(),
                        src.high()
                    )));
                }
            }
            Self::ToReg16Ind(dest, src) => {
                let src_display = Reg8OrExpr(src);
                report(
                    &Diagnostic::help()
                        .with_message(format!("Consider using `ld [hl], {src_display}`, or `ld a, {src_display}` then `ld [{dest}], a`")),
                )
            }
            Self::FromReg16Ind(dest, src) => report(&Diagnostic::help().with_message(format!(
                "Consider using `ld {dest}, [hl]`, or `ld a, [{src}]` then `ld {dest}, a`"
            ))),
            Self::Daa => report(&Diagnostic::help().with_message("Please write `daa` instead")),
            Self::Cpl(reg) => report(&Diagnostic::help().with_message(format!(
                "Consider using `ld a, {reg}`, then `cpl`, and `ld {reg}, a`"
            ))),
            Self::Alu(op, lhs, rhs) => {
                if matches!(rhs, Reg8::A) {
                    report(&Diagnostic::help().with_message(format!(
                        "If you meant to add `{lhs}` to `a`, use `{op} a, {lhs}` instead"
                    )));
                }
            }
            Self::ToHram(src) => {
                let src_display = Reg8OrExpr(src);
                report(
                    &Diagnostic::help()
                        .with_message(format!("Consider using `ld [hl], {src_display}`, or `ld a, {src_display}` then `ldh [<address>], a`")),
                )
            }
            Self::FromHram(dest) => report(&Diagnostic::help().with_message(format!(
                "Consider using `ld {dest}, [hl]`, or `ldh a, [<address>]` then `ld {dest}, a`"
            ))),
            Self::JpInd(dest) => report(&Diagnostic::help().with_message(format!(
                "Consider copying `{dest}` to `hl` then using `jp hl`, or using `push {dest}` then `ret`"
            ))),
            Self::JrInd(dest) => report(&Diagnostic::help().with_message(format!("Consider trying `jp {dest}` instead"))),
            Self::LdSpReg16(src) => report(&Diagnostic::help().with_message(format!("Condifer copying `{src}` to `hl`, then `ld sp, hl`"))),
            Self::JpCond(cond, dest) | Self::JrCond(cond, dest) => report(&Diagnostic::help().with_message(
                format!("Consider using a `jr {}, ...` to jump over a `jp {dest}` instead", !cond)
            )),
            Self::CallInd(dest) => report(&Diagnostic::help().with_message(format!(
                "Consider making a function that jumps to `{dest}`, and `call`ing that"
            ))),
            Self::CallCond(dest) => report(&Diagnostic::help().with_message(format!(
                "Consider making a function that jumps to `{dest}`, and `call`ing that conditionally"
            ))),
            Self::LdhToC(src) => report(&Diagnostic::help().with_message(format!("Consider using `ld a, {src}` then `ldh [c], a`"))),
            Self::LdhFromC(dest) => report(&Diagnostic::help().with_message(format!("Consider using `ldh a, [c]` then `ld {dest}, a`"))),

            Self::LdReg16Sp(dest) => {
                if matches!(dest, Reg16Stack::Hl) {
                    report(&Diagnostic::help().with_message("Did you mean to write `ld hl, sp + 0`?"));
                }
            }
            Self::LdSpSp => report(&Diagnostic::help().with_message("If you only want to waste some cycles, consider using `nop`s")),

            _ => {}
        }
    }
}

struct Reg8OrExpr<'a>(&'a Option<Reg8>);

impl Display for Reg8OrExpr<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.0 {
            Some(reg) => reg.fmt(f),
            None => f.write_str("..."),
        }
    }
}

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, Hash)]
#[display(style = "lowercase")]
pub enum Reg8 {
    B = 0,
    C = 1,
    D = 2,
    E = 3,
    H = 4,
    L = 5,
    #[display("[hl]")]
    HlInd = 6,
    A = 7,
}

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, Hash)]
#[display(style = "lowercase")]
pub enum Reg16 {
    Bc = 0x00,
    De = 0x10,
    Hl = 0x20,
    Sp = 0x30,
}

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, Hash)]
#[display(style = "lowercase")]
pub enum Reg16Ind {
    Bc = 0x00,
    De = 0x10,
    Hli = 0x20,
    Hld = 0x30,
}

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, Hash)]
#[display(style = "lowercase")]
pub enum Reg16Stack {
    Bc = 0x00,
    De = 0x10,
    Hl = 0x20,
    Af = 0x30,
}

#[derive(Debug, Display, Clone, Copy, PartialEq, Eq, Hash)]
#[display(style = "lowercase")]
pub enum Condition {
    Nz = 0x00,
    Z = 0x08,
    Nc = 0x10,
    C = 0x18,
}

impl Reg16Stack {
    pub fn low(&self) -> Option<Reg8> {
        match self {
            Self::Bc => Some(Reg8::C),
            Self::De => Some(Reg8::E),
            Self::Hl => Some(Reg8::L),
            Self::Af => None,
        }
    }

    pub fn high(&self) -> Reg8 {
        match self {
            Self::Bc => Reg8::B,
            Self::De => Reg8::D,
            Self::Hl => Reg8::H,
            Self::Af => Reg8::A,
        }
    }
}

impl Not for Condition {
    type Output = Condition;

    fn not(self) -> Self::Output {
        match self {
            Condition::Nz => Condition::Z,
            Condition::Z => Condition::Nz,
            Condition::Nc => Condition::C,
            Condition::C => Condition::Nc,
        }
    }
}

impl Not for &Condition {
    type Output = Condition;

    fn not(self) -> Self::Output {
        match *self {
            Condition::Nz => Condition::Z,
            Condition::Z => Condition::Nz,
            Condition::Nc => Condition::C,
            Condition::C => Condition::Nc,
        }
    }
}
