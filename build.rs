/* SPDX-License-Identifier: MPL-2.0 */

fn main() {
    #[cfg(feature = "rgbasm")]
    lalrpop::process_root().expect("Parser generation failed");
}
