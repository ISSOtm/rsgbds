/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

use std::rc::Rc;

use crate::language::AsmErrorKind;

#[derive(Debug, Clone)]
pub struct MacroArgs {
    args: Vec<Rc<String>>,
    shift_amount: usize,
}

impl MacroArgs {
    pub fn new(args: Vec<Rc<String>>) -> Self {
        Self {
            args,
            shift_amount: 0,
        }
    }

    pub fn nb_args(&self) -> usize {
        self.args.len() - self.shift_amount
    }

    pub fn get(&self, idx: usize) -> Option<&Rc<String>> {
        self.args.get(idx + self.shift_amount - 1)
    }

    pub fn shift(&mut self, amount: isize) -> Result<(), AsmErrorKind> {
        if let Some(new_shift) = self.shift_amount.checked_add_signed(amount) {
            if new_shift <= self.args.len() {
                self.shift_amount = new_shift;
                Ok(())
            } else {
                Err(AsmErrorKind::Overshift(new_shift, self.args.len()))
            }
        } else {
            Err(AsmErrorKind::NegativeShift(self.shift_amount, amount))
        }
    }

    pub fn make_concat(&self) -> Rc<String> {
        let mut args = self.args.iter();
        match args.next() {
            None => Rc::new(String::new()),

            Some(first_arg) => {
                if self.args.len() == 1 {
                    Rc::clone(first_arg)
                } else {
                    let mut string = String::clone(first_arg);
                    for arg in args {
                        string.push(',');
                        string.push_str(arg);
                    }
                    Rc::new(string)
                }
            }
        }
    }
}
