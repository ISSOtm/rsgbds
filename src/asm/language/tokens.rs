/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

use keyword_trie_gen::keyword_trie;
use parse_display::Display;

pub fn can_start_ident(ch: char) -> bool {
    matches!(ch, 'A'..='Z' | 'a'..='z' | '_')
}

#[derive(Debug, Clone, Display)]
#[display(style = "title case")]
#[display("\"{}\"")]
#[keyword_trie(
    trie_name = "KEYWORD_TRIE",
    ty_name = "Keyword",
    chars = "#@0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_"
)]
pub enum Token {
    #[display("number")]
    Number(u32),
    #[display("string")]
    String(String),

    // Punctuation.
    #[display("\".\"")]
    Period,
    #[display("\",\"")]
    Comma,
    #[display("\":\"")]
    Colon,
    #[display("\"::\"")] // T_PAAMAYIM_NEKUDOTAYIM
    DoubleColon,
    #[display("\"[\"")]
    LeftBracket,
    #[display("\"]\"")]
    RightBracket,
    #[display("\"(\"")]
    LeftParens,
    #[display("\")\"")]
    RightParens,
    #[display("newline")]
    Newline,

    // Operators.
    #[display("\"!\"")]
    LogicNot,
    #[display("\"&&\"")]
    LogicAnd,
    #[display("\"||\"")]
    LogicOr,
    #[display("\">\"")]
    Gt,
    #[display("\"<\"")]
    Lt,
    #[display("\">=\"")]
    Gte,
    #[display("\"<=\"")]
    Lte,
    #[display("\"!=\"")]
    LogicNe,
    #[display("\"==\"")]
    LogicEq,
    #[display("\"+\"")]
    Plus,
    #[display("\"-\"")]
    Minus,
    #[display("\"*\"")]
    Mul,
    #[display("\"/\"")]
    Div,
    #[display("\"%\"")]
    Mod,
    #[display("\"**\"")]
    Exponent,
    #[display("\"<<\"")]
    Shl,
    #[display("\">>\"")]
    Shr,
    #[display("\">>>\"")]
    UShr,
    #[display("\"|\"")]
    BitOr,
    #[display("\"^\"")]
    BitXor,
    #[display("\"&\"")]
    BitAnd,
    #[display("\"~\"")]
    Complement,

    // Built-in label/section functions.
    Def,
    Bank,
    Align,
    Sizeof,
    Startof,
    Opcode,

    // Built-in fixed-point functions.
    Sin,
    Cos,
    Tan,
    Asin,
    Acos,
    Atan,
    Atan2,
    FMul,
    FDiv,
    FMod, // No, we are still not doing audio processing.
    Pow,
    Log,
    Round,
    Ceil,
    Floor,

    // Built-in expression functions.
    High,
    Low,
    IsConst,

    // Built-in string functions.
    Strcmp,
    Strin,
    Strrin,
    Strsub,
    Strlen,
    Strcat,
    Strupr,
    Strlwr,
    Strrpl,
    Strfmt,

    // Built-in charmap functions.
    Charlen,
    Charsub,

    // Identifiers.
    #[display("label")]
    Label(String),
    #[display("identifier")]
    Identifier(String),
    #[display("local identifier")]
    LocalIdent(String),
    #[display("anonymous label reference")]
    AnonLabelRef(u32, bool),

    // Assignment operators.
    Equ,
    #[display("=")]
    Eq,
    Rb,
    Rw,
    // No `Rl` here because it's also an instruction.
    Equs,

    // Compound assignment operators.
    #[display("\"+=\"")]
    AddEq,
    #[display("\"-=\"")]
    SubEq,
    #[display("\"*=\"")]
    MulEq,
    #[display("\"/=\"")]
    DivEq,
    #[display("\"%=\"")]
    ModEq,
    #[display("\"<<=\"")]
    ShlEq,
    #[display("\">>=\"")]
    ShrEq,
    #[display("\">>>=\"")]
    UShrEq,
    #[display("\"|=\"")]
    OrEq,
    #[display("\"^=\"")]
    XorEq,
    #[display("\"&=\"")]
    AndEq,

    // Directives.
    Rsreset,
    Rsset,
    Redef,
    Export,
    Purge,
    Print,
    Println,
    If,
    Elif,
    Else,
    Endc,
    Macro,
    Shift,
    Endm,
    Rept,
    For,
    Break,
    Endr,
    Section,
    Fragment,
    Load,
    Endl,
    Pushs,
    Pops,
    Union,
    Nextu,
    Endu,
    Ds,
    Db,
    Dw,
    Dl,
    Include,
    Incbin,
    Charmap,
    Newcharmap,
    Setcharmap,
    Pushc,
    Popc,
    Fail,
    Warn,
    Fatal,
    Assert,
    StaticAssert,
    Opt,
    Pusho,
    Popo,

    // Memory types.
    Rom0,
    Romx,
    Vram,
    Sram,
    Wram0,
    Wramx,
    Oam,
    Hram,

    // Instructions.
    Adc,
    Add,
    And,
    Bit,
    Call,
    Ccf,
    Cp,
    Cpl,
    Daa,
    Dec,
    Di,
    Ei,
    Halt,
    Inc,
    Jp,
    Jr,
    Ld,
    Ldi,
    Ldd,
    Ldh,
    Nop,
    Or,
    Pop,
    Push,
    Res,
    Ret,
    Reti,
    Rst,
    Rl,
    Rla,
    Rlc,
    Rlca,
    Rr,
    Rra,
    Rrc,
    Rrca,
    Sbc,
    Scf,
    Set,
    Stop,
    Sla,
    Sra,
    Srl,
    Sub,
    Swap,
    Xor,

    // Registers.
    A,
    B,
    C,
    D,
    E,
    H,
    L,
    Af,
    Bc,
    De,
    Hl,
    Sp,
    Hld,
    Hli,

    // Conditions.
    Nz,
    Z,
    Nc, // No `C` here because it's already a register.

    // Hacks.
    /// This dummy token is used to "stuff" LALRPOP's lookahead token, to ensure that the lexer can be configured at precise parsing points.
    /// The intended usage is that you place the "fragile" action code BEFORE this token.
    // I will probably go to hell for this.
    LookaheadHack,
}
