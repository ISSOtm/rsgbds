/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

use std::{
    fmt::Display,
    fs::File,
    io::Write,
    panic::PanicInfo,
    path::PathBuf,
    time::{Duration, SystemTime},
};

use clap::{ColorChoice, Parser};
use sysexits::ExitCode;
use yansi::{Condition, Paint};

const CRASH_URL: &str = "http://rgbds.gbdev.io/report-crash";

pub fn setup_and_parse_args<Cli: Parser>() -> Result<Cli, ()> {
    setup_panic_handler();
    detect_default_color_choice();

    let args = super::argfile::collect_expanded_args();
    Cli::try_parse_from(args).map_err(|err| err.print().expect("Failed to print CLI error"))
}

pub fn parse_number<N: funty::Unsigned>(arg: &str) -> Result<N, std::num::ParseIntError> {
    if let Some(binary) = arg
        .strip_prefix('%')
        .or_else(|| arg.strip_prefix("0b"))
        .or_else(|| arg.strip_prefix("0B"))
    {
        N::from_str_radix(binary, 2)
    } else if let Some(hexadecimal) = arg
        .strip_prefix('$')
        .or_else(|| arg.strip_prefix("0x"))
        .or_else(|| arg.strip_prefix("0X"))
    {
        N::from_str_radix(hexadecimal, 16)
    } else {
        N::from_str_radix(arg, 10)
    }
}

fn setup_panic_handler() {
    let should_override = !cfg!(debug_assertions) && std::env::var_os("RUST_BACKTRACE").is_none();
    if should_override {
        std::panic::set_hook(Box::new(|info| {
            // Print one line as early as possible, so that a crash at a later point still (hopefully) emits something.
            eprintln!(
                std::concat!(
                    "Sorry! ",
                    env!("CARGO_BIN_NAME"),
                    " had a problem and {}. :("
                ),
                "crashed".red().bold()
            );

            let file_path = generate_crash_report(info);

            eprintln!(
                "  A crash report has been generated at {}
  To send this report to the developers, please visit:
    {CRASH_URL}
{}",
                file_path.display().bold(),
                "  We take privacy seriously, and do not perform any automated data collection.
  In order to improve RGBDS, we rely on people to submit reports.
  Thank you kindly!"
                    .italic()
            );
        }));
    }
}

fn detect_default_color_choice() {
    yansi::whenever(Condition::cached(
        Condition::os_support()
            && Condition::stderr_is_tty_live()
            && Condition::clicolor_live()
            && Condition::no_color_live(),
    ));
}

pub fn clap_color_choice() -> ColorChoice {
    // Always align `clap` on `yansi`, to avoid any discrepancies in how detection might be performed.
    if yansi::is_enabled() {
        ColorChoice::Always
    } else {
        ColorChoice::Never
    }
}

pub fn apply_color_choice(user_choice: ColorChoice) {
    match user_choice {
        ColorChoice::Auto => {} // Continue with the previous auto-detected result.
        ColorChoice::Always => yansi::enable(),
        ColorChoice::Never => yansi::disable(),
    }
}

fn generate_crash_report(info: &PanicInfo<'_>) -> PathBuf {
    let now = SystemTime::now().duration_since(SystemTime::UNIX_EPOCH);

    let mut path = std::env::temp_dir();

    struct Stamp<'a, E>(&'a Result<Duration, E>);
    impl<E> Display for Stamp<'_, E> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
            match self.0 {
                Ok(timestamp) => write!(f, "{:06}", timestamp.as_secs() % 1_000_000),
                Err(_) => f.write_str("XXXXXX"),
            }
        }
    }
    path.push(format!(
        std::concat!(env!("CARGO_BIN_NAME"), "-crash-{}.toml",),
        Stamp(&now)
    ));

    let mut file = File::options()
        .write(true)
        .create_new(true)
        .open(&path)
        .unwrap_or_else(|err| {
            eprintln!(
                "Failed to create crash report at \"{}\": {err}",
                path.display()
            );
            std::process::exit(ExitCode::CantCreat as _);
        });

    // Attempt to write to the file, but panicking at this point is rather useless.
    // (Better a truncated report than none at all.)
    macro_rules! report {
        ( $( $fmt:tt )+ ) => {
            let _ = writeln!(&mut file, $($fmt)+);
        };
    }

    report!("# Please submit this crash report; see {CRASH_URL} for instructions");

    // Pray that the OS string doesn't contain three consecutive quotes. That's unlikely enough.
    report!(
        r#"
[env]
os = '''
{}
'''
cli = ["#,
        os_info::get()
    );
    for arg in std::env::args_os() {
        report!("\t\"{}\",", arg.to_string_lossy());
    }
    report!("]");

    report!(
        "
[panic]"
    );
    match info.location() {
        Some(location) => {
            report!(
                "file = \"{}\"
line = {}",
                TomlString(location.file()),
                location.line()
            );
        }
        None => {
            report!("# Unknown panic location");
        }
    }
    if let Some(msg) = info.payload().downcast_ref::<&str>() {
        report!("message = \"{}\"", TomlString(msg));
    } else if let Some(msg) = info.payload().downcast_ref::<String>() {
        report!("message = \"{}\"", TomlString(msg));
    } else {
        report!("# message = <Unknown panic payload>");
    }
    report!(
        "backtrace = '''
{}
'''",
        std::backtrace::Backtrace::force_capture(), // This is an expensive operation, but we are about to give up anyway.
    );

    // Spice it up a little bit with a random funny quip.
    let quote_id = match now {
        Ok(duration) => duration.as_secs(),
        Err(_) => 0,
    } as usize
        % CRASH_QUOTES.len();
    report!(
        "
# {}",
        CRASH_QUOTES[quote_id]
    );

    path
}

struct TomlString<'a>(&'a str);
impl Display for TomlString<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // > Any Unicode character may be used except those that must be escaped:
        // > quotation mark, backslash, and the control characters other than tab (U+0000 to U+0008, U+000A to U+001F, U+007F).
        const MUST_BE_ESCAPED: [char; 32] = [
            '\u{0000}', '\u{0001}', '\u{0002}', '\u{0003}', '\u{0004}', '\u{0005}', '\u{0006}',
            '\u{0007}', '\u{0008}', '\u{000A}', '\u{000B}', '\u{000C}', '\u{000D}', '\u{000E}',
            '\u{000F}', '\u{0010}', '\u{0011}', '\u{0012}', '\u{0013}', '\u{0014}', '\u{0015}',
            '\u{0016}', '\u{0017}', '\u{0018}', '\u{0019}', '\u{001A}', '\u{001B}', '\u{001C}',
            '\u{001D}', '\u{001E}', '\u{001F}', '\u{007F}',
        ];
        let mut s = self.0;
        while let Some(idx) = s.find(MUST_BE_ESCAPED) {
            let c = s[idx..].chars().next().unwrap();
            write!(f, "{}\\U{:08X}", &s[..idx], c as u32)?;
            s = &s[idx + c.len_utf8()..];
        }
        f.write_str(s)
    }
}

const CRASH_QUOTES: &[&str] = &[
    "Slightly more expressive than a bunch of vertical lines on the screen.",
    "I warned you about assembly! I told you dog!",
    "Someone got a broom and dustpan?",
    "OOPSIE WOOPSIE!! Uwu We made a fucky wucky!! A wittle fucko boingo! The code monkeys at our headquarters are working VEWY HAWD to fix this!",
    "Hold on! We're sending backup!",
    "So said EVELYN the modified DOG:",
    "There was a crash, but don't fret. Free ice cream awaits a short walk away!",
    "Oh my god!! You tripped! Are you injured?",
    "Internal Compiler Error! Don't fret; we have free ICE cream",
    "Drove that approach straight into the ground >_<",
    "Pro gamer tip: you win the game by playing",
    "You've experienced a crash. Now before a bandicoot can do any more damage...",
    "ALL YOUR panic!() ARE BELONG TO US",
    "This is a feature, I SWEAR!",
    "That's a 1 in 256 miss!",
    "Wild MISSINGNO. appeared!",
    "*confused silicon noises*",
];
