/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

//! Convenience functionality shared between the RGBDS executables.
//!
//! This is not intended for downstream consumers of RGBDS' API,
//! so none of this is covered by SemVer.

pub mod argfile;
shadow_rs::shadow!(build);
pub mod cli;
pub mod dash_stdio;
pub mod diagnostics;
