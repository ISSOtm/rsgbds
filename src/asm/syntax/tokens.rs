use compact_str::CompactString;
use phf::phf_map;
use uncased::UncasedStr;

use crate::{context_stack::Span, symbols::SymName};

#[derive(Debug)]
pub struct Token<'ctx_stack> {
    pub payload: TokenPayload,
    pub span: Span<'ctx_stack>,
}

macro_rules! define_tokens {
    (
        $(#[$enum_attr:meta])*
        pub enum $enum_name:ident {$(
            #[name = $descr:tt $(, keyword $kw:tt)?]
            $(#[$attr:meta])*
            $token:ident $(($($fields:ty),* $(,)?))? ,
        )*}
    ) => {
        $(#[$enum_attr])*
        #[derive(strum_macros::Display)]
        pub enum $enum_name {$(
            #[doc = $descr]
            #[strum(to_string = $descr)]
            $(#{$attr})*
            $token $(($($fields,)*))? ,
        )*}

        macro_rules! tok {$(
            ($descr) => {$enum_name::$token};
        )*}
        // Workaround for rust-lang/rust#52234, from https://github.com/rust-lang/rust/pull/52234#issuecomment-976702997
        pub(crate) use tok;

        pub const KEYWORDS: phf::Map<&UncasedStr, $enum_name> = phf_map! {$($(
            // `let _ = $kw;` is useless, but it's necessary to make that metavariable intervene
            // and thus "filter" the tokens to only keep the keywords.
            UncasedStr::new($descr) => { let _ = $kw; $enum_name::$token },
        )?)*};
    }
}

define_tokens! {
    #[derive(Debug, Clone)]
    pub enum TokenPayload {
        #[name = "number"]
        Number(i32),
        #[name = "string"]
        String(CompactString),
        #[name = "identifier"]
        Identifier(SymName),
        #[name = "anonymous label reference"]
        AnonLabelRef(i32),
        #[name = "end of line"]
        Newline,

        // General punctuation.
        #[name = ","]
        Comma,
        #[name = ":"]
        Colon,
        #[name = "::"]
        DblColon,
        #[name = "["]
        LBrack,
        #[name = "]"]
        RBrack,
        #[name = "("]
        LParen,
        #[name = ")"]
        RParen,

        // Arithmetic operators.
        #[name = "+"]
        Plus,
        #[name = "-"]
        Minus,
        #[name = "*"]
        Asterisk,
        #[name = "/"]
        Slash,
        #[name = "%"]
        Percent,
        #[name = "**"]
        DblAsterisk,

        // Comparison operators.
        #[name = "=="]
        DblEqual,
        #[name = "!="]
        NotEqu,
        #[name = "<"]
        Lt,
        #[name = ">"]
        Gt,
        #[name = "<="]
        Lte,
        #[name = ">="]
        Gte,

        // Logical operators.
        #[name = "&&"]
        DblAmpersand,
        #[name = "||"]
        DlbPipe,
        #[name = "!"]
        Bang,

        // Bit-level operators.
        #[name = "&"]
        Ampersand,
        #[name = "|"]
        Pipe,
        #[name = "^"]
        Caret,
        #[name = "<<"]
        Shl,
        #[name = ">>"]
        Shr,
        #[name = ">>>"]
        UShr,
        #[name = "~"]
        Complement,

        // Assignment operators.
        #[name = "="]
        Equal,
        #[name = "+="]
        AddEq,
        #[name = "-="]
        SubEq,
        #[name = "*="]
        MulEq,
        #[name = "/="]
        DivEq,
        #[name = "%="]
        ModEq,
        // No PowEq.
        #[name = "&="]
        BitAndEq,
        #[name = "|="]
        BitOrEq,
        #[name = "^="]
        BitXorEq,
        #[name = "<<="]
        ShlEq,
        #[name = ">>="]
        ShrEq,
        #[name = ">>>="]
        UShrEq,

        // Registers.
        #[name = "b", keyword()]
        B,
        #[name = "c", keyword()]
        C,
        #[name = "d", keyword()]
        D,
        #[name = "e", keyword()]
        E,
        #[name = "h", keyword()]
        H,
        #[name = "l", keyword()]
        L,
        #[name = "a", keyword()]
        A,
        #[name = "af", keyword()]
        Af,
        #[name = "bc", keyword()]
        Bc,
        #[name = "de", keyword()]
        De,
        #[name = "hl", keyword()]
        Hl,
        #[name = "sp", keyword()]
        Sp,
        #[name = "hli", keyword()]
        HlInc,
        #[name = "hld", keyword()]
        HlDec,

        // Condition codes.
        #[name = "z", keyword()]
        Z,
        #[name = "nz", keyword()]
        Nz,
        // C is the same as the register, further above.
        #[name = "nc", keyword()]
        Nc,

        // Instructions.
        #[name = "adc", keyword()]
        Adc,
        #[name = "add", keyword()]
        Add,
        #[name = "and", keyword()]
        And,
        #[name = "bit", keyword()]
        Bit,
        #[name = "call", keyword()]
        Call,
        #[name = "ccf", keyword()]
        Ccf,
        #[name = "cp", keyword()]
        Cp,
        #[name = "cpl", keyword()]
        Cpl,
        #[name = "daa", keyword()]
        Daa,
        #[name = "dec", keyword()]
        Dec,
        #[name = "di", keyword()]
        Di,
        #[name = "ei", keyword()]
        Ei,
        #[name = "halt", keyword()]
        Halt,
        #[name = "inc", keyword()]
        Inc,
        #[name = "jp", keyword()]
        Jp,
        #[name = "jr", keyword()]
        Jr,
        #[name = "ldd", keyword()]
        Ldd,
        #[name = "ldh", keyword()]
        Ldh,
        #[name = "ldi", keyword()]
        Ldi,
        #[name = "ld", keyword()]
        Ld,
        #[name = "nop", keyword()]
        Nop,
        #[name = "or", keyword()]
        Or,
        #[name = "pop", keyword()]
        Pop,
        #[name = "push", keyword()]
        Push,
        #[name = "res", keyword()]
        Res,
        #[name = "reti", keyword()]
        Reti,
        #[name = "ret", keyword()]
        Ret,
        #[name = "rla", keyword()]
        Rla,
        #[name = "rlca", keyword()]
        Rlca,
        #[name = "rlc", keyword()]
        Rlc,
        #[name = "rl", keyword()]
        Rl,
        #[name = "rra", keyword()]
        Rra,
        #[name = "rrca", keyword()]
        Rrca,
        #[name = "rrc", keyword()]
        Rrc,
        #[name = "rr", keyword()]
        Rr,
        #[name = "rst", keyword()]
        Rst,
        #[name = "sbc", keyword()]
        Sbc,
        #[name = "scf", keyword()]
        Scf,
        #[name = "set", keyword()]
        Set,
        #[name = "sla", keyword()]
        Sla,
        #[name = "sra", keyword()]
        Sra,
        #[name = "srl", keyword()]
        Srl,
        #[name = "stop", keyword()]
        Stop,
        #[name = "sub", keyword()]
        Sub,
        #[name = "swap", keyword()]
        Swap,
        #[name = "xor", keyword()]
        Xor,

        // Directive keywords.
        #[name = "align", keyword()]
        Align,
        #[name = "assert", keyword()]
        Assert,
        #[name = "break", keyword()]
        Break,
        #[name = "charmap", keyword()]
        Charmap,
        #[name = "db", keyword()]
        Db,
        #[name = "dl", keyword()]
        Dl,
        #[name = "ds", keyword()]
        Ds,
        #[name = "dw", keyword()]
        Dw,
        #[name = "elif", keyword()]
        Elif,
        #[name = "else", keyword()]
        Else,
        #[name = "endc", keyword()]
        Endc,
        #[name = "endl", keyword()]
        Endl,
        #[name = "endm", keyword()]
        Endm,
        #[name = "endr", keyword()]
        Endr,
        #[name = "endsection", keyword()]
        Endsection,
        #[name = "endu", keyword()]
        Endu,
        #[name = "equ", keyword()]
        Equ,
        #[name = "equs", keyword()]
        Equs,
        #[name = "export", keyword()]
        Export,
        #[name = "fail", keyword()]
        Fail,
        #[name = "fatal", keyword()]
        Fatal,
        #[name = "for", keyword()]
        For,
        #[name = "fragment", keyword()]
        Fragment,
        #[name = "if", keyword()]
        If,
        #[name = "incbin", keyword()]
        Incbin,
        #[name = "include", keyword()]
        Include,
        #[name = "load", keyword()]
        Load,
        #[name = "macro", keyword()]
        Macro,
        #[name = "newcharmap", keyword()]
        Newcharmap,
        #[name = "nextu", keyword()]
        Nextu,
        #[name = "opt", keyword()]
        Opt,
        #[name = "popc", keyword()]
        Popc,
        #[name = "popo", keyword()]
        Popo,
        #[name = "pops", keyword()]
        Pops,
        #[name = "println", keyword()]
        Println,
        #[name = "print", keyword()]
        Print,
        #[name = "purge", keyword()]
        Purge,
        #[name = "pushc", keyword()]
        Pushc,
        #[name = "pusho", keyword()]
        Pusho,
        #[name = "pushs", keyword()]
        Pushs,
        #[name = "rb", keyword()]
        Rb,
        #[name = "redef", keyword()]
        Redef,
        #[name = "rept", keyword()]
        Rept,
        // No Rl, as it's covered by an instruction, further above.
        #[name = "rsreset", keyword()]
        Rsreset,
        #[name = "rsset", keyword()]
        Rsset,
        #[name = "rw", keyword()]
        Rw,
        #[name = "section", keyword()]
        Section,
        #[name = "setcharmap", keyword()]
        Setcharmap,
        #[name = "shift", keyword()]
        Shift,
        #[name = "static_assert", keyword()]
        StaticAssert,
        #[name = "union", keyword()]
        Union,
        #[name = "warn", keyword()]
        Warn,

        // Function keywords.
        #[name = "acos", keyword()]
        Acos,
        #[name = "asin", keyword()]
        Asin,
        #[name = "atan", keyword()]
        Atan,
        #[name = "atan2", keyword()]
        Atan2,
        #[name = "bank", keyword()]
        Bank,
        #[name = "bitwidth", keyword()]
        Bitwidth,
        #[name = "ceil", keyword()]
        Ceil,
        #[name = "charlen", keyword()]
        Charlen,
        #[name = "charsub", keyword()]
        Charsub,
        #[name = "cos", keyword()]
        Cos,
        #[name = "def", keyword()]
        Def,
        #[name = "fdiv", keyword()]
        Fdiv,
        #[name = "floor", keyword()]
        Floor,
        #[name = "fmod", keyword()]
        Fmod,
        #[name = "fmul", keyword()]
        Fmul,
        #[name = "high", keyword()]
        High,
        #[name = "incharmap", keyword()]
        Incharmap,
        #[name = "isconst", keyword()]
        Isconst,
        #[name = "log", keyword()]
        Log,
        #[name = "low", keyword()]
        Low,
        #[name = "pow", keyword()]
        Pow,
        #[name = "round", keyword()]
        Round,
        #[name = "sin", keyword()]
        Sin,
        #[name = "sizeof", keyword()]
        Sizeof,
        #[name = "startof", keyword()]
        Startof,
        #[name = "strcat", keyword()]
        Strcat,
        #[name = "strcmp", keyword()]
        Strcmp,
        #[name = "strfmt", keyword()]
        Strfmt,
        #[name = "strin", keyword()]
        Strin,
        #[name = "strlen", keyword()]
        Strlen,
        #[name = "strlwr", keyword()]
        Strlwr,
        #[name = "strrin", keyword()]
        Strrin,
        #[name = "strrpl", keyword()]
        Strrpl,
        #[name = "strsub", keyword()]
        Strsub,
        #[name = "strupr", keyword()]
        Strupr,
        #[name = "tan", keyword()]
        Tan,
        #[name = "tzcount", keyword()]
        Tzcount,

        // Section kinds.
        #[name = "hram", keyword()]
        Hram,
        #[name = "oam", keyword()]
        Oam,
        #[name = "rom0", keyword()]
        Rom0,
        #[name = "romx", keyword()]
        Romx,
        #[name = "sram", keyword()]
        Sram,
        #[name = "vram", keyword()]
        Vram,
        #[name = "wram0", keyword()]
        Wram0,
        #[name = "wramx", keyword()]
        Wramx,
    }
}
