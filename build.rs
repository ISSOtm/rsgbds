/* SPDX-License-Identifier: MPL-2.0 */

fn main() {
    #[cfg(any(feature = "rgbasm", feature = "rgblink"))]
    lalrpop::process_root().expect("Parser generation failed");
}
