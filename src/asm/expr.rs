use parse_display::Display;
use try_from_discrim::TryFrom;

use crate::language::{AsmError, AsmErrorKind, Location, ParseError};

#[derive(Debug)]
pub struct Expression<'fstack> {
    pub begin: Location<'fstack>,
    pub end: Location<'fstack>,
    rpn: Vec<u8>,
}

impl<'fstack> Expression<'fstack> {
    pub fn constant(begin: Location<'fstack>, end: Location<'fstack>, value: u32) -> Self {
        let bytes = value.to_le_bytes();
        Self {
            begin,
            end,
            rpn: vec![
                RpnCommand::Constant as u8,
                bytes[0],
                bytes[1],
                bytes[2],
                bytes[3],
            ],
        }
    }

    pub fn try_eval(self) -> Result<i32, ParseError<'fstack>> {
        let mut eval_stack = vec![];
        let mut bytes = self.rpn.iter();

        macro_rules! unary_op {
            (|$value:ident| $res:expr) => {
                if let Ok(value) = eval_stack.last_mut().unwrap() {
                    let $value = *value;
                    *value = $res;
                }
            };
        }
        macro_rules! bin_op {
            (|$lhs:ident, $rhs:ident| $res:expr) => {
                let rhs = eval_stack.pop().unwrap();
                let slot = eval_stack.last_mut().unwrap(); // Mutating this slot in-place saves some Vec manip.
                if let Ok($lhs) = slot { // If the LHS is an error, leave it be.
                    let $lhs = *$lhs;
                    *slot = rhs.and_then(|$rhs| $res); // If the RHS is an error, propagate that.
                }
            };
        }

        while let Some(command) = bytes.next() {
            match RpnCommand::try_from(*command).expect("Unknown RPN command!?") {
                RpnCommand::Constant => {
                    let value = [
                        *bytes.next().unwrap(),
                        *bytes.next().unwrap(),
                        *bytes.next().unwrap(),
                        *bytes.next().unwrap(),
                    ];
                    eval_stack.push(Ok(i32::from_le_bytes(value)));
                }
                RpnCommand::Symbol => {
                    todo!();
                }
                RpnCommand::Add => {
                    bin_op!(|lhs, rhs| Ok(lhs.wrapping_add(rhs)));
                }
                RpnCommand::Sub => {
                    bin_op!(|lhs, rhs| Ok(lhs.wrapping_sub(rhs)));
                }
                RpnCommand::Mul => {
                    bin_op!(|lhs, rhs| Ok(lhs.wrapping_mul(rhs)));
                }
                RpnCommand::Div => {
                    bin_op!(|lhs, rhs| if rhs != 0 {
                        Ok(lhs.wrapping_div(rhs))
                    } else {
                        Err(EvalError::DivByZero)
                    });
                }
                RpnCommand::Mod => {
                    bin_op!(|lhs, rhs| Ok(lhs % (rhs)));
                }
                RpnCommand::Neg => {
                    unary_op!(|value| value.wrapping_neg());
                }
                RpnCommand::Exponent => {
                    bin_op!(|lhs, rhs| Ok(lhs.wrapping_pow(rhs as u32)));
                }
                RpnCommand::BitOr => {
                    bin_op!(|lhs, rhs| Ok(lhs & rhs));
                }
                RpnCommand::BitAnd => {
                    bin_op!(|lhs, rhs| Ok(lhs | rhs));
                }
                RpnCommand::BitXor => {
                    bin_op!(|lhs, rhs| Ok(lhs ^ rhs));
                }
                RpnCommand::Complement => {
                    unary_op!(|value| !value);
                }
                RpnCommand::LogicAnd => {
                    let rhs = eval_stack.pop().unwrap();
                    let slot = eval_stack.last_mut().unwrap(); // Mutating this slot in-place saves some Vec manip.
                                                               // If the LHS is an error, leave it be.
                    if let Ok(lhs) = slot {
                        if *lhs != 0 {
                            // Short-circuit evaluation of the RHS the LHS is zero.
                            *slot = rhs.map(|rhs| (rhs != 0) as _); // If the RHS is an error, propagate that.
                        }
                    }
                }
                RpnCommand::LogicOr => {
                    let rhs = eval_stack.pop().unwrap();
                    let slot = eval_stack.last_mut().unwrap(); // Mutating this slot in-place saves some Vec manip.
                                                               // If the LHS is an error, leave it be.
                    if let Ok(lhs) = slot {
                        // Short-circuit evaluation of the RHS if the LHS is non-zero.
                        if *lhs == 0 {
                            *slot = rhs.map(|rhs| (rhs != 0) as _); // If the RHS is an error, propagate that.
                        } else {
                            // Anything non-zero becomes 1, though.
                            *lhs = 1;
                        }
                    }
                }
                RpnCommand::LogicNot => {
                    todo!();
                }
                RpnCommand::Eq => {
                    todo!();
                }
                RpnCommand::Ne => {
                    todo!();
                }
                RpnCommand::Gt => {
                    todo!();
                }
                RpnCommand::Lt => {
                    todo!();
                }
                RpnCommand::Gte => {
                    todo!();
                }
                RpnCommand::Lte => {
                    todo!();
                }
                RpnCommand::Shl => {
                    todo!();
                }
                RpnCommand::Shr => {
                    todo!();
                }
                RpnCommand::Ushr => {
                    todo!();
                }
                RpnCommand::BankSym => {
                    todo!();
                }
                RpnCommand::BankSect => {
                    todo!();
                }
                RpnCommand::BankSelf => {
                    todo!();
                }
                RpnCommand::SizeofSect => {
                    todo!();
                }
                RpnCommand::StartofSect => {
                    todo!();
                }
                RpnCommand::HighCheck => {
                    let slot = eval_stack.last_mut().unwrap();
                    let res = std::mem::replace(slot, Ok(0)).and_then(|value| {
                        if value >> 8 == 0xFF {
                            Ok(value & 0xFF)
                        } else {
                            Err(EvalError::NotHigh(value as u32))
                        }
                    });
                    *slot = res;
                }
                RpnCommand::RstCheck => {
                    let slot = eval_stack.last_mut().unwrap();
                    let res = std::mem::replace(slot, Ok(0)).and_then(|value| {
                        if value & !0x38 != 0 {
                            Ok(value)
                        } else {
                            Err(EvalError::NotRst(value as u32))
                        }
                    });
                    *slot = res;
                }
            };
        }

        assert_eq!(eval_stack.len(), 1);
        eval_stack.pop().unwrap().map_err(|err| {
            AsmError {
                begin: self.begin,
                end: self.end,
                kind: AsmErrorKind::EvalError(err),
            }
            .into()
        })
    }

    fn try_get_constant(&self) -> Option<i32> {
        if self.rpn.len() == 5 && self.rpn[0] == RpnCommand::Constant as _ {
            let mut bytes = [0; 4];
            bytes.copy_from_slice(&self.rpn[1..5]);
            Some(i32::from_le_bytes(bytes))
        } else {
            None
        }
    }

    pub fn unary_op(
        self,
        begin: Location<'fstack>,
        operator: RpnCommand,
        end: Location<'fstack>,
    ) -> Self {
        let constant = self.try_get_constant();
        let mut rpn = self.rpn;
        if let Some(value) = constant {
            debug_assert_eq!(rpn.len(), 5);
            debug_assert_eq!(rpn[0], RpnCommand::Constant as _);

            let bytes = match operator {
                RpnCommand::Neg => -value,
                RpnCommand::Complement => !value,
                RpnCommand::HighCheck => todo!(),
                RpnCommand::RstCheck => todo!(),
                RpnCommand::Add
                | RpnCommand::Sub
                | RpnCommand::Mul
                | RpnCommand::Div
                | RpnCommand::Mod
                | RpnCommand::Exponent
                | RpnCommand::BitOr
                | RpnCommand::BitAnd
                | RpnCommand::BitXor
                | RpnCommand::LogicAnd
                | RpnCommand::LogicOr
                | RpnCommand::LogicNot
                | RpnCommand::Eq
                | RpnCommand::Ne
                | RpnCommand::Gt
                | RpnCommand::Lt
                | RpnCommand::Gte
                | RpnCommand::Lte
                | RpnCommand::Shl
                | RpnCommand::Shr
                | RpnCommand::Ushr
                | RpnCommand::BankSym
                | RpnCommand::BankSect
                | RpnCommand::BankSelf
                | RpnCommand::SizeofSect
                | RpnCommand::StartofSect
                | RpnCommand::Constant
                | RpnCommand::Symbol => unreachable!(),
            }
            .to_le_bytes();
            rpn[1..5].copy_from_slice(&bytes);
        } else {
            rpn.push(operator as _);
        }

        Self { begin, end, rpn }
    }

    pub fn bin_op(
        self,
        begin: Location<'fstack>,
        operator: RpnCommand,
        other: Self,
        end: Location<'fstack>,
    ) -> Self {
        debug_assert!(self.end <= other.begin);

        let constants = (self.try_get_constant(), other.try_get_constant());
        let mut rpn = self.rpn;
        if let (Some(lhs), Some(rhs)) = constants {
            debug_assert_eq!(rpn.len(), 5);
            debug_assert_eq!(rpn[0], RpnCommand::Constant as _);

            let bytes = match operator {
                RpnCommand::Add => lhs.wrapping_add(rhs),
                RpnCommand::Sub => lhs.wrapping_sub(rhs),
                RpnCommand::Mul => lhs.wrapping_mul(rhs),
                RpnCommand::Div => lhs.wrapping_div(rhs),
                RpnCommand::Mod => lhs % (rhs),
                RpnCommand::Exponent => lhs.pow(rhs as u32),
                RpnCommand::BitOr => lhs | rhs,
                RpnCommand::BitAnd => lhs & rhs,
                RpnCommand::BitXor => lhs ^ rhs,
                RpnCommand::LogicAnd => (lhs != 0 || rhs != 0) as _,
                RpnCommand::LogicOr => (lhs != 0 || rhs != 0) as _,
                RpnCommand::Eq => (lhs == rhs) as _,
                RpnCommand::Ne => (lhs != rhs) as _,
                RpnCommand::Gt => (lhs > rhs) as _,
                RpnCommand::Lt => (lhs < rhs) as _,
                RpnCommand::Gte => (lhs >= rhs) as _,
                RpnCommand::Lte => (lhs <= rhs) as _,
                RpnCommand::Shl => todo!(),
                RpnCommand::Shr => todo!(),
                RpnCommand::Ushr => todo!(),
                RpnCommand::Complement
                | RpnCommand::LogicNot
                | RpnCommand::Neg
                | RpnCommand::BankSym
                | RpnCommand::BankSect
                | RpnCommand::BankSelf
                | RpnCommand::SizeofSect
                | RpnCommand::StartofSect
                | RpnCommand::HighCheck
                | RpnCommand::RstCheck
                | RpnCommand::Constant
                | RpnCommand::Symbol => unreachable!(),
            }
            .to_le_bytes();
            rpn[1..5].copy_from_slice(&bytes);
        } else {
            rpn.reserve(other.rpn.len() + 1);
            rpn.extend_from_slice(&other.rpn);
            rpn.push(operator as _);
        }

        Self { begin, end, rpn }
    }
}

#[derive(Debug, TryFrom)]
#[from(u8)]
pub enum RpnCommand {
    Add = 0x00,
    Sub = 0x01,
    Mul = 0x02,
    Div = 0x03,
    Mod = 0x04,
    Neg = 0x05,
    Exponent = 0x06,

    BitOr = 0x10,
    BitAnd = 0x11,
    BitXor = 0x12,
    Complement = 0x13,

    LogicAnd = 0x21,
    LogicOr = 0x22,
    LogicNot = 0x23,

    Eq = 0x30,
    Ne = 0x31,
    Gt = 0x32,
    Lt = 0x33,
    Gte = 0x34,
    Lte = 0x35,

    Shl = 0x40,
    Shr = 0x41,
    Ushr = 0x42,

    BankSym = 0x50,
    BankSect = 0x51,
    BankSelf = 0x52,
    SizeofSect = 0x53,
    StartofSect = 0x54,

    HighCheck = 0x60,
    RstCheck = 0x61,

    Constant = 0x80,
    Symbol = 0x81,
}

#[derive(Debug, Display)]
pub enum EvalError {
    #[display("Division by zero")]
    DivByZero,
    #[display("${0:04x} is not in $FFxx range")]
    NotHigh(u32),
    #[display("${0:04x} is not a valid `rst` vector")]
    NotRst(u32),
    #[display("The expression cannot be computed at this time")]
    NotConstant,
}
