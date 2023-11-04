/* SPDX-License-Identifier: MPL-2.0 */

fn main() {
    lalrpop::process_root().expect("Parser generation failed");
}
