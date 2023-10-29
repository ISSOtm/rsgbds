/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

use rgbds::{
    rpn::{Command, EvalError, Rpn},
    RelocKind,
};

use crate::{
    language::{AsmError, Location, SymEvalErrKind},
    macro_args::MacroArgs,
    sections::Sections,
    symbols::{SymbolId, Symbols},
};

/// Importantly, the two locations `begin` and `end` do not necessarily represent the full expression, but e.g. the location of the sub-expression that generated the current error.
#[derive(Debug)]
pub struct Expression<'fstack> {
    pub begin: Location<'fstack>,
    pub end: Location<'fstack>,
    rpn: Result<Rpn, EvalError<SymEvalErrKind>>,
}

impl<'fstack> Expression<'fstack> {
    pub fn constant(begin: Location<'fstack>, end: Location<'fstack>, value: u32) -> Self {
        Self {
            begin,
            end,
            rpn: Ok(Rpn::constant(value)),
        }
    }

    pub fn symbol(
        begin: Location<'fstack>,
        end: Location<'fstack>,
        sym_id: Result<SymbolId, SymEvalErrKind>,
    ) -> Self {
        Self {
            begin,
            end,
            rpn: match sym_id {
                Ok(id) => Ok(Rpn::symbol(id.0)),
                Err(err) => Err(err.into()),
            },
        }
    }

    // These are separate from `binary_op` because we don't have a full expression,
    // we'd like to avoid constructing bogus locations when we can just spawn a tiny RPN expression.
    // Plus it avoids potentially mucking with any internal assumptions in `binary_op`.
    pub fn low(self, begin: Location<'fstack>, end: Location<'fstack>) -> Self {
        match Rpn::binary_op(self.rpn, Command::BitAnd, Ok(Rpn::constant(0xFF))) {
            Ok(rpn) => Self {
                begin,
                end,
                rpn: Ok(rpn),
            },
            Err(rpn) => Self {
                begin: self.begin,
                end: self.end,
                rpn: Err(rpn),
            },
        }
    }

    pub fn unary_op(
        self,
        begin: Location<'fstack>,
        operator: Command,
        end: Location<'fstack>,
    ) -> Self {
        match Rpn::unary_op(operator, self.rpn) {
            Ok(rpn) => Self {
                begin,
                end,
                rpn: Ok(rpn),
            },
            Err(rpn) => Self {
                begin: self.begin,
                end: self.end,
                rpn: Err(rpn),
            },
        }
    }

    pub fn binary_op(
        self,
        begin: Location<'fstack>,
        operator: Command,
        rhs: Self,
        end: Location<'fstack>,
    ) -> Self {
        debug_assert!(self.end <= rhs.begin);

        let is_err = (self.rpn.is_err(), rhs.rpn.is_err());

        match Rpn::binary_op(self.rpn, operator, rhs.rpn) {
            Ok(rpn) => Self {
                begin,
                end,
                rpn: Ok(rpn),
            },
            Err(err) => {
                // This is arguably a bit of a heuristic, that only works because short-circuiting is left-associative only.
                let (begin, end) = match is_err {
                    (false, false) => (begin, end), // The error stemmed from this operation.
                    (true, _) => (self.begin, self.end),
                    (false, true) => (rhs.begin, rhs.end),
                };
                Self {
                    begin,
                    end,
                    rpn: Err(err),
                }
            }
        }
    }

    pub fn try_eval(
        self,
        symbols: &Symbols,
        macro_args: Option<&MacroArgs>,
        sections: &Sections,
    ) -> Result<(i32, Location<'fstack>, Location<'fstack>), AsmError<'fstack>> {
        let get_sym_value = |id| symbols.get_number_from_id(SymbolId(id), macro_args, sections);

        match self.rpn.and_then(|rpn| rpn.try_eval(get_sym_value)) {
            Ok(value) => Ok((value, self.begin, self.end)),
            Err(err) => Err(AsmError::new(self.begin, self.end, err.into())),
        }
    }

    pub fn into_raw_parts(
        self,
    ) -> Result<(Location<'fstack>, Location<'fstack>, Rpn), AsmError<'fstack>> {
        match self.rpn {
            Ok(rpn) => Ok((self.begin, self.end, rpn)),
            Err(err) => Err(AsmError::new(self.begin, self.end, err.into())),
        }
    }
}

#[derive(Debug)]
pub enum ByteOrExpr<'fstack> {
    Byte(u8),
    Expr(Location<'fstack>, Location<'fstack>, Rpn, RelocKind),
}

impl<'fstack> ByteOrExpr<'fstack> {
    pub fn try_from_expr(
        expr: Expression<'fstack>,
        kind: RelocKind,
    ) -> Result<Self, AsmError<'fstack>> {
        let (begin, end, rpn) = expr.into_raw_parts()?;
        Ok(Self::Expr(begin, end, rpn, kind))
    }
}
