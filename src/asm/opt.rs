/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

use crate::{error::WarningSettings, language::AsmErrorKind};

/// Assembler options that can be changed at runtime (with the `OPT` directive).
#[derive(Debug, Clone)]
pub struct RuntimeOptions {
    pub binary_digits: [char; 2],
    pub gfx_digits: [char; 4],

    pub fill_byte: u8,

    pub fixed_point_precision: u8,

    pub max_recursion_depth: u16,

    pub nop_after_halt: bool,
    pub optimise_to_ldh: bool,

    pub warn_settings: WarningSettings,
}

impl RuntimeOptions {
    pub fn parse_option(&mut self, option: char, arg: &str) -> Result<(), AsmErrorKind> {
        let mut chars = arg.chars();
        match option {
            'b' => match arg.len() {
                2 => {
                    self.binary_digits = [chars.next().unwrap(), chars.next().unwrap()];
                    Ok(())
                }
                len => Err(AsmErrorKind::BadOptBLen(len)),
            },

            'g' => match arg.len() {
                4 => {
                    self.gfx_digits = [
                        chars.next().unwrap(),
                        chars.next().unwrap(),
                        chars.next().unwrap(),
                        chars.next().unwrap(),
                    ];
                    Ok(())
                }
                len => Err(AsmErrorKind::BadOptGLen(len)),
            },

            'p' => {
                // The parse function's range check ensures the truncation is fine.
                self.fill_byte = rgbds::parse_generic_u16(arg, 0, 0xFF)
                    .map_err(|err| AsmErrorKind::BadOptArg('p', err))?
                    as u8;
                Ok(())
            }

            'Q' => {
                let arg = arg.strip_prefix('.').unwrap_or(arg);
                self.fixed_point_precision = rgbds::parse_generic_u16(arg, 1, 31)
                    .map_err(|err| AsmErrorKind::BadOptArg('Q', err))?
                    as u8;
                Ok(())
            }

            'r' => {
                self.max_recursion_depth = rgbds::parse_generic_u16(arg, 0, u16::MAX)
                    .map_err(|err| AsmErrorKind::BadOptArg('r', err))?;
                Ok(())
            }

            'W' => self.warn_settings.process_flag(arg),

            mut c => {
                let bool_val = if c == '!' {
                    c = chars.next().ok_or(AsmErrorKind::NoOptToNegate)?;
                    false
                } else {
                    true
                };

                match c {
                    'H' => self.nop_after_halt = bool_val,
                    'L' => self.optimise_to_ldh = bool_val,

                    // Kept for backwards compat, but do-nothing.
                    c @ ('h' | 'l') => {
                        if !bool_val {
                            return Err(AsmErrorKind::CannotDisableOpt(c));
                        }
                    }

                    _ => return Err(AsmErrorKind::UnknownOption(c)),
                }

                if chars.next().is_none() {
                    Ok(())
                } else {
                    Err(AsmErrorKind::UnexpectedOptArg(c))
                }
            }
        }
    }
}

impl Default for RuntimeOptions {
    fn default() -> Self {
        Self {
            binary_digits: ['0', '1'],
            gfx_digits: ['0', '1', '2', '3'],
            fixed_point_precision: 16,
            fill_byte: 0x00,
            nop_after_halt: false,
            optimise_to_ldh: false,
            max_recursion_depth: 64,
            warn_settings: WarningSettings::default(),
        }
    }
}

#[derive(Debug)]
pub struct RuntimeOptStack(Vec<RuntimeOptions>);

impl RuntimeOptStack {
    pub fn new() -> Self {
        Self(vec![Default::default()])
    }

    pub fn cur_options(&self) -> &RuntimeOptions {
        self.0.last().unwrap() // The vector is never emptied, `pop` ensures that much.
    }
    pub fn cur_options_mut(&mut self) -> &mut RuntimeOptions {
        self.0.last_mut().unwrap() // Same.
    }

    pub fn push(&mut self) {
        self.0.push(self.cur_options().clone())
    }

    pub fn pop(&mut self) -> Result<(), AsmErrorKind> {
        if self.0.len() == 1 {
            Err(AsmErrorKind::EmptyOptionStack)
        } else {
            self.0.pop();
            Ok(())
        }
    }
}
