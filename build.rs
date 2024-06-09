/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

fn main() {
    #[cfg(any(feature = "rgbasm", feature = "rgblink"))]
    lalrpop::process_root().expect("Parser generation failed");

    #[cfg(any(
        feature = "rgbasm",
        feature = "rgblink",
        feature = "rgbfix",
        feature = "rgbgfx"
    ))]
    shadow_rs::new().expect("Shadow info generation failed");
}
