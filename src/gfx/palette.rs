/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

use std::fmt::Display;

use arrayvec::ArrayVec;
use plumers::color::Rgb16;

use crate::rgb::Rgba;

#[derive(Debug, Clone)]
pub struct Palette {
    pub(crate) colors: ArrayVec<Rgb16, 4>,
}

impl Palette {
    pub fn new(has_transparency: bool) -> Self {
        let mut this = Self {
            colors: Default::default(),
        };
        if has_transparency {
            this.colors.push(Rgba::TRANSPARENT);
        }
        this
    }

    pub fn add_color(&mut self, color: Rgb16) {
        if self.colors.iter().all(|&already_in| already_in != color) {
            self.colors.push(color);
        }
    }

    pub fn index_of(&self, color: Rgb16, has_transparency: bool) -> Option<u8> {
        if color == Rgba::TRANSPARENT {
            debug_assert!(has_transparency);
            debug_assert_eq!(self.colors[0], Rgba::TRANSPARENT);
        }
        self.colors
            .iter()
            .position(|&candidate| candidate == color)
            .map(|idx| idx as u8)
    }
}

impl Display for Palette {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        crate::color_set::format_color_set(&self.colors, f)
    }
}
