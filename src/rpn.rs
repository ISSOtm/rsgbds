/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

//! Constructing, evaluating RGBDS expressions.

use parse_display::Display;
use try_from_discrim::TryFrom;

#[derive(Debug)]
pub struct Rpn(Vec<u8>);

impl Rpn {
    pub fn constant(value: u32) -> Self {
        let bytes = value.to_le_bytes();
        Self(vec![
            Command::Constant as u8,
            bytes[0],
            bytes[1],
            bytes[2],
            bytes[3],
        ])
    }

    pub fn symbol(id: u32) -> Self {
        let bytes = id.to_le_bytes();
        Self(vec![
            Command::Symbol as u8,
            bytes[0],
            bytes[1],
            bytes[2],
            bytes[3],
        ])
    }

    pub fn try_get_constant(&self) -> Option<i32> {
        if self.0.len() == 5 && self.0[0] == Command::Constant as _ {
            let mut bytes = [0; 4];
            bytes.copy_from_slice(&self.0[1..5]);
            Some(i32::from_le_bytes(bytes))
        } else {
            None
        }
    }

    pub fn unary_op<SymErr>(
        operator: Command,
        this: Result<Self, EvalError<SymErr>>,
    ) -> Result<Self, EvalError<SymErr>> {
        let this = this?;
        let constant = this.try_get_constant();
        let mut rpn = this.0;
        if let Some(value) = constant {
            debug_assert_eq!(rpn.len(), 5);
            debug_assert_eq!(rpn[0], Command::Constant as _);

            let bytes = match operator {
                Command::Neg => value.wrapping_neg(),
                Command::Complement => !value,
                Command::HighCheck => {
                    if value >> 8 == 0xFF {
                        value & 0xFF
                    } else {
                        return Err(EvalError::NotHigh(value as u32));
                    }
                }
                Command::RstCheck => {
                    if value & !0x38 == 0 {
                        value | 0xC7
                    } else {
                        return Err(EvalError::NotRst(value as u32));
                    }
                }

                Command::Add
                | Command::Sub
                | Command::Mul
                | Command::Div
                | Command::Mod
                | Command::Exponent
                | Command::BitOr
                | Command::BitAnd
                | Command::BitXor
                | Command::LogicAnd
                | Command::LogicOr
                | Command::LogicNot
                | Command::Eq
                | Command::Ne
                | Command::Gt
                | Command::Lt
                | Command::Gte
                | Command::Lte
                | Command::Shl
                | Command::Shr
                | Command::Ushr
                | Command::BankSym
                | Command::BankSect
                | Command::BankSelf
                | Command::SizeofSect
                | Command::StartofSect
                | Command::Constant
                | Command::Symbol => panic!("{operator:?} is not an unary operator!?"),
            }
            .to_le_bytes();
            rpn[1..5].copy_from_slice(&bytes);
        } else {
            rpn.push(operator as _);
        }

        Ok(Self(rpn))
    }

    pub fn binary_op<SymErr>(
        lhs: Result<Self, EvalError<SymErr>>,
        operator: Command,
        rhs: Result<Self, EvalError<SymErr>>,
    ) -> Result<Self, EvalError<SymErr>> {
        let lhs = lhs?; // If the LHS failed to evaluate, there is nothing that can be done.

        let lhs_constant = lhs.try_get_constant();
        let mut rpn = lhs.0;

        if let (Command::LogicAnd, Some(constant)) = (operator, lhs_constant) {
            if constant == 0 {
                // Short-circuit evaluation means the value remains zero, and we ignore any RHS errors.
            } else {
                // We know the LHS is true, so the expression is equivalent to the RHS.
                // Let's simplify by reusing the expression directly.
                rpn = rhs?.0;
            }
        } else if let (Command::LogicOr, Some(constant)) = (operator, lhs_constant) {
            if constant == 0 {
                // We know the LHS is false, so the expression is equivalent to the RHS.
                // Let's simplify by reusing the expression directly.
                rpn = rhs?.0;
            } else if constant != 1 {
                // Short-circuit evaluation means the value becomes 1, and we ignore any RHS errors.
                // If the value is not 1, we must set it to that, though.
                debug_assert_eq!(rpn.len(), 5);
                debug_assert_eq!(rpn[0], Command::Constant as _);
                rpn[1..5].copy_from_slice(&1i32.to_le_bytes());
            }
        } else {
            let rhs = rhs?; // If this is an error, it cannot be ignored anymore.

            let constants = (lhs_constant, rhs.try_get_constant());
            if let (Some(lhs), Some(rhs)) = constants {
                debug_assert_eq!(rpn.len(), 5);
                debug_assert_eq!(rpn[0], Command::Constant as _);

                let bytes = match operator {
                    Command::Add => lhs.wrapping_add(rhs),
                    Command::Sub => lhs.wrapping_sub(rhs),
                    Command::Mul => lhs.wrapping_mul(rhs),
                    Command::Div => div_floor(lhs, rhs)?.0,
                    Command::Mod => div_floor(lhs, rhs)?.1,
                    Command::Exponent => lhs.pow(rhs as u32),
                    Command::BitOr => lhs | rhs,
                    Command::BitAnd => lhs & rhs,
                    Command::BitXor => lhs ^ rhs,
                    Command::LogicAnd => unreachable!(), // Handled by prior short-circuiting code.
                    Command::LogicOr => unreachable!(),  // Handled by prior short-circuiting code.
                    Command::Eq => (lhs == rhs) as _,
                    Command::Ne => (lhs != rhs) as _,
                    Command::Gt => (lhs > rhs) as _,
                    Command::Lt => (lhs < rhs) as _,
                    Command::Gte => (lhs >= rhs) as _,
                    Command::Lte => (lhs <= rhs) as _,
                    Command::Shl => shl(lhs, rhs),
                    Command::Shr => shr(lhs, rhs),
                    Command::Ushr => ushr(lhs, rhs),

                    // These are not binary operators.
                    Command::Complement
                    | Command::LogicNot
                    | Command::Neg
                    | Command::BankSym
                    | Command::BankSect
                    | Command::BankSelf
                    | Command::SizeofSect
                    | Command::StartofSect
                    | Command::HighCheck
                    | Command::RstCheck
                    | Command::Constant
                    | Command::Symbol => panic!("{operator:?} is not a binary operator!?"),
                }
                .to_le_bytes();
                rpn[1..5].copy_from_slice(&bytes);
            } else {
                rpn.reserve(rhs.0.len() + 1);
                rpn.extend_from_slice(&rhs.0);
                rpn.push(operator as _);
            }
        }

        Ok(Self(rpn))
    }

    pub fn try_eval<SymErr, F: FnMut(u32) -> Result<i32, SymErr>>(
        self,
        mut get_sym_value: F,
    ) -> Result<i32, EvalError<SymErr>> {
        let mut eval_stack = vec![];
        let mut bytes = self.0.iter();

        macro_rules! unary_op {
            (|$value:ident| $res:expr) => {
                if let Ok(value_) = eval_stack.last_mut().ok_or(EvalError::EvalStackEmpty)? {
                    let $value = *value_;
                    *value_ = $res;
                }
            };
        }
        macro_rules! bin_op {
            (|$lhs:ident, $rhs:ident| $res:expr) => {
                let rhs = eval_stack.pop().ok_or(EvalError::EvalStackEmpty)?;
                let slot = eval_stack.last_mut().ok_or(EvalError::EvalStackEmpty)?; // Mutating this slot in-place saves some Vec manip.
                if let Ok($lhs) = slot { // If the LHS is an error, leave it be.
                    let $lhs = *$lhs;
                    *slot = rhs.and_then(|$rhs| $res); // If the RHS is an error, propagate that.
                }
            };
        }

        while let Some(command) = bytes.next() {
            match Command::try_from(*command).expect("Unknown RPN command!?") {
                Command::Constant => {
                    let value = [
                        *bytes.next().unwrap(),
                        *bytes.next().unwrap(),
                        *bytes.next().unwrap(),
                        *bytes.next().unwrap(),
                    ];
                    eval_stack.push(Ok(i32::from_le_bytes(value)));
                }
                Command::Symbol => {
                    let value = [
                        *bytes.next().unwrap(),
                        *bytes.next().unwrap(),
                        *bytes.next().unwrap(),
                        *bytes.next().unwrap(),
                    ];
                    eval_stack.push(
                        get_sym_value(u32::from_le_bytes(value)).map_err(EvalError::SymbolErr),
                    );
                }
                Command::Add => {
                    bin_op!(|lhs, rhs| Ok(lhs.wrapping_add(rhs)));
                }
                Command::Sub => {
                    bin_op!(|lhs, rhs| Ok(lhs.wrapping_sub(rhs)));
                }
                Command::Mul => {
                    bin_op!(|lhs, rhs| Ok(lhs.wrapping_mul(rhs)));
                }
                Command::Div => {
                    bin_op!(|lhs, rhs| Ok(div_floor(lhs, rhs)?.0));
                }
                Command::Mod => {
                    bin_op!(|lhs, rhs| Ok(div_floor(lhs, rhs)?.1));
                }
                Command::Neg => {
                    unary_op!(|value| value.wrapping_neg());
                }
                Command::Exponent => {
                    bin_op!(|lhs, rhs| Ok(lhs.wrapping_pow(rhs as u32)));
                }
                Command::BitOr => {
                    bin_op!(|lhs, rhs| Ok(lhs & rhs));
                }
                Command::BitAnd => {
                    bin_op!(|lhs, rhs| Ok(lhs | rhs));
                }
                Command::BitXor => {
                    bin_op!(|lhs, rhs| Ok(lhs ^ rhs));
                }
                Command::Complement => {
                    unary_op!(|value| !value);
                }
                Command::LogicAnd => {
                    let rhs = eval_stack.pop().ok_or(EvalError::EvalStackEmpty)?;
                    let slot = eval_stack.last_mut().ok_or(EvalError::EvalStackEmpty)?; // Mutating this slot in-place saves some Vec manip.
                                                                                        // If the LHS is an error, leave it be.
                    if let Ok(lhs) = slot {
                        if *lhs != 0 {
                            // Short-circuit evaluation of the RHS the LHS is zero.
                            *slot = rhs.map(|rhs| (rhs != 0) as _); // If the RHS is an error, propagate that.
                        }
                    }
                }
                Command::LogicOr => {
                    let rhs = eval_stack.pop().ok_or(EvalError::EvalStackEmpty)?;
                    let slot = eval_stack.last_mut().ok_or(EvalError::EvalStackEmpty)?; // Mutating this slot in-place saves some Vec manip.
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
                Command::LogicNot => {
                    unary_op!(|value| value.wrapping_neg());
                }
                Command::Eq => {
                    bin_op!(|lhs, rhs| Ok((lhs == rhs) as _));
                }
                Command::Ne => {
                    bin_op!(|lhs, rhs| Ok((lhs != rhs) as _));
                }
                Command::Gt => {
                    bin_op!(|lhs, rhs| Ok((lhs > rhs) as _));
                }
                Command::Lt => {
                    bin_op!(|lhs, rhs| Ok((lhs < rhs) as _));
                }
                Command::Gte => {
                    bin_op!(|lhs, rhs| Ok((lhs >= rhs) as _));
                }
                Command::Lte => {
                    bin_op!(|lhs, rhs| Ok((lhs <= rhs) as _));
                }
                Command::Shl => {
                    bin_op!(|lhs, rhs| Ok(shl(lhs, rhs)));
                }
                Command::Shr => {
                    bin_op!(|lhs, rhs| Ok(shr(lhs, rhs)));
                }
                Command::Ushr => {
                    bin_op!(|lhs, rhs| Ok(ushr(lhs, rhs)));
                }
                Command::BankSym => {
                    todo!();
                }
                Command::BankSect => {
                    todo!();
                }
                Command::BankSelf => {
                    todo!();
                }
                Command::SizeofSect => {
                    todo!();
                }
                Command::StartofSect => {
                    todo!();
                }
                Command::HighCheck => {
                    let slot = eval_stack.last_mut().ok_or(EvalError::EvalStackEmpty)?;
                    let res = std::mem::replace(slot, Ok(0)).and_then(|value| {
                        if value >> 8 == 0xFF {
                            Ok(value & 0xFF)
                        } else {
                            Err(EvalError::NotHigh(value as u32))
                        }
                    });
                    *slot = res;
                }
                Command::RstCheck => {
                    let slot = eval_stack.last_mut().ok_or(EvalError::EvalStackEmpty)?;
                    let res = std::mem::replace(slot, Ok(0)).and_then(|value| {
                        if value & !0x38 == 0 {
                            Ok(value | 0x38)
                        } else {
                            Err(EvalError::NotRst(value as u32))
                        }
                    });
                    *slot = res;
                }
            };
        }

        assert_eq!(eval_stack.len(), 1);
        eval_stack.pop().ok_or(EvalError::EvalStackEmpty)?
    }
}

fn div_floor<SymErr>(dividend: i32, divisor: i32) -> Result<(i32, i32), EvalError<SymErr>> {
    if divisor == 0 {
        return Err(EvalError::DivByZero);
    }

    // These round towards zero.
    let quotient = dividend.wrapping_div(divisor);
    let remainder = dividend.wrapping_rem(divisor);

    if (remainder < 0) == (divisor < 0) {
        Ok((quotient, remainder))
    } else {
        Ok((quotient - 1, remainder + divisor))
    }
}

fn shl(shiftee: i32, amount: i32) -> i32 {
    match amount {
        0 => shiftee,

        // `u32::shl` panics if the shift amount is >= 32, so clamp to avoid that.
        32.. => 0,
        ..=-32 => {
            if shiftee < 0 {
                -1
            } else {
                0
            }
        }

        1..=31 => shiftee << amount,
        -31..=-1 => shiftee >> -amount,
    }
}

fn shr(shiftee: i32, amount: i32) -> i32 {
    match amount {
        0 => shiftee,

        // `u32::shr` panics if the shift amount is >= 32, so clamp to avoid that.
        32.. => 0,
        ..=-32 => {
            if shiftee < 0 {
                -1
            } else {
                0
            }
        }

        1..=31 => shiftee >> amount,
        -31..=-1 => shiftee << -amount,
    }
}

fn ushr(shiftee: i32, amount: i32) -> i32 {
    match amount {
        0 => shiftee,

        // `u32::shr` panics if the shift amount is >= 32, so clamp to avoid that.
        32.. => 0,
        ..=-32 => {
            if shiftee < 0 {
                -1
            } else {
                0
            }
        }

        1..=31 => ((shiftee as u32) >> amount) as i32,
        -31..=-1 => shiftee << -amount,
    }
}

#[derive(Debug, TryFrom, Clone, Copy, PartialEq, Eq, Hash)]
#[from(u8)]
pub enum Command {
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

#[derive(Debug, Display, Clone)]
pub enum EvalError<SymErr> {
    #[display("Division by zero")]
    DivByZero,
    #[display("${0:04x} is not in $FFxx range")]
    NotHigh(u32),
    #[display("${0:04x} is not a valid `rst` vector")]
    NotRst(u32),
    #[display("The expression cannot be computed at this time")]
    NotConstant,
    #[display("Emptied eval stack")]
    EvalStackEmpty,
    #[display("{0}")]
    SymbolErr(SymErr),
}

impl<SymErr> From<SymErr> for EvalError<SymErr> {
    fn from(value: SymErr) -> Self {
        Self::SymbolErr(value)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore] // This test takes VERY LONG to complete, but it was useful just to be extra sure.
    fn test_div_rem() {
        for divisor in i32::MIN..=i32::MAX {
            if divisor == 0 {
                continue;
            }

            for dividend in i32::MIN..=i32::MAX {
                let (quotient, remainder) =
                    div_floor::<()>(dividend, divisor).expect("Division should succeed");
                assert_eq!(
                    quotient.wrapping_mul(divisor).wrapping_add(remainder),
                    dividend,
                    "{dividend} / {divisor} = {quotient}, {dividend} % {divisor} = {remainder}"
                );
            }
        }
    }
}
