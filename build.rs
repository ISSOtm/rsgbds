/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

use std::{
    error::Error,
    fs::File,
    io::{BufRead, BufReader, Write},
    path::{Path, PathBuf},
    str::FromStr,
};

fn main() -> Result<(), Box<dyn Error>> {
    #[cfg(any(
        feature = "rgbasm",
        feature = "rgblink",
        feature = "rgbfix",
        feature = "rgbgfx"
    ))]
    shadow_rs::new()?;

    #[cfg(feature = "rgbasm")]
    generate_warnings_mod();

    Ok(())
}

#[derive(Debug)]
struct Warning {
    name: String,
    kind: WarningKind,
    meta: MetaWarnings,
}

#[derive(Debug)]
enum WarningKind {
    Boolean { default: bool },
    Parametric { nb_levels: u8, default_level: u8 },
}

#[derive(Debug, Default)]
struct MetaWarnings {
    all: bool,
    extra: bool,
}

fn generate_warnings_mod() {
    let warnings = parse_all_warnings();

    let mut out_path = PathBuf::from(std::env::var("OUT_DIR").unwrap());
    out_path.push("warnings.rs");
    let mut file = File::create(out_path).expect("Failed to create `warnings.rs`");

    writeln!(
        &mut file,
        "#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct WarningKind(#[doc(hidden)] pub usize);
pub const NB_WARNINGS: usize = {};

const DEFAULT_WARNINGS: [bool; NB_WARNINGS] = [",
        warnings.iter().fold(0, |acc, warning| acc
            + match warning.kind {
                WarningKind::Boolean { .. } => 1,
                WarningKind::Parametric { nb_levels, .. } => nb_levels,
            }),
    )
    .unwrap();
    for warning in &warnings {
        match warning.kind {
            WarningKind::Boolean { default } => {
                writeln!(&mut file, "    {default}, // {}", warning.name).unwrap();
            }
            WarningKind::Parametric {
                nb_levels,
                default_level,
            } => {
                for level in 1..=nb_levels {
                    writeln!(
                        &mut file,
                        "    {}, // {}={level}",
                        level <= default_level,
                        warning.name
                    )
                    .unwrap();
                }
            }
        }
    }
    writeln!(
        &mut file,
        "];

macro_rules! warning {{"
    )
    .unwrap();
    let mut i = 0;
    for warning in &warnings {
        match warning.kind {
            WarningKind::Boolean { .. } => {
                writeln!(
                    &mut file,
                    "    (\"{}\") => {{ $crate::diagnostics::WarningKind({i}) }};",
                    &warning.name
                )
                .unwrap();
                i += 1;
            }
            WarningKind::Parametric { nb_levels, .. } => {
                for level in 1..=nb_levels {
                    writeln!(
                        &mut file,
                        "    (\"{}={level}\") => {{ $crate::diagnostics::WarningKind({i}) }};",
                        &warning.name
                    )
                    .unwrap();
                    i += 1;
                }
            }
        }
    }
    writeln!(
        &mut file,
        "}}
pub(crate) use warning;

impl std::fmt::Display for WarningKind {{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {{
        f.write_str(match *self {{"
    )
    .unwrap();
    let mut i = 0;
    for warning in &warnings {
        match warning.kind {
            WarningKind::Boolean { .. } => {
                writeln!(
                    &mut file,
                    "            WarningKind({i}) => \"-W{}\",",
                    &warning.name
                )
                .unwrap();
                i += 1;
            }
            WarningKind::Parametric { nb_levels, .. } => {
                for level in 1..=nb_levels {
                    writeln!(
                        &mut file,
                        "            WarningKind({i}) => \"-W{}={level}\",",
                        &warning.name
                    )
                    .unwrap();
                    i += 1;
                }
            }
        }
    }
    writeln!(
        &mut file,
        "            WarningKind(_) => unreachable!(),
        }})
    }}
}}"
    )
    .unwrap();
}

fn parse_all_warnings() -> Vec<Warning> {
    let mut warnings = Vec::new();

    let mut line_buf = String::new();
    fn read_first_line<'line>(path: &Path, buf: &'line mut String) -> &'line str {
        buf.clear();
        BufReader::new(
            File::open(path)
                .unwrap_or_else(|err| panic!("Failed to open {}: {err}", path.display())),
        )
        .read_line(buf)
        .unwrap_or_else(|err| panic!("Failed to read the first line of {}: {err}", path.display()));
        buf.strip_prefix(".\\\"").unwrap_or_else(|| {
            panic!(
                "`{}` must start with a mdoc comment line (`.\\\"`)",
                path.display()
            )
        })
    }

    fn parse_directive(directive: &str) -> (&str, &str) {
        let (name, value) = directive.split_once('=').unwrap_or_else(|| {
            panic!("Directive `{directive}` should be formatted as `<name> = <value>`")
        });
        (
            name.trim_matches(|c: char| c.is_ascii_whitespace()),
            value.trim_matches(|c: char| c.is_ascii_whitespace()),
        )
    }

    println!("cargo:rerun-if-changed=src/asm/warnings");
    for warning in std::fs::read_dir("src/asm/warnings").expect("Failed to list `src/asm/warnings`")
    {
        let warning = warning.expect("Error while listing `src/asm/warnings`");
        let path = warning.path();

        let (name, kind, meta) = if warning
            .file_type()
            .expect("Failed to get file type")
            .is_dir()
        {
            // This is a parametric warning.
            let mut nb_levels = 0u8;
            let mut default_level = None;
            let mut meta = None;

            for f in std::fs::read_dir(&path).expect("Failed to list parametric warning directory")
            {
                let f = f.expect("Error while listing parametric warning directory");
                let name = f.file_name();

                if name == "descr.mdoc" {
                    let line = read_first_line(&f.path(), &mut line_buf);

                    for directive in line.split(';') {
                        let (name, value) = parse_directive(directive);
                        match name {
                            "default" => {
                                if default_level
                                    .replace(value.parse().unwrap_or_else(|err| {
                                        panic!("Bad value for `default` directive: {err}")
                                    }))
                                    .is_some()
                                {
                                    panic!("`default` directive specified twice");
                                }
                            }
                            "meta" => {
                                if meta
                                    .replace(value.parse().unwrap_or_else(|err| {
                                        panic!("Bad value for `meta` directive: {err}")
                                    }))
                                    .is_some()
                                {
                                    panic!("`meta` directive specified twice");
                                }
                            }
                            _ => panic!("Unknown directive `{name}`"),
                        }
                    }
                } else {
                    // Parse the file name; it should be `<level>.mdoc`.
                    let level = name
                        .to_str()
                        .and_then(|s| s.strip_suffix(".mdoc"))
                        .unwrap_or_else(|| panic!(
                            "Files in parametric warning directories should be named e.g. `1.mdoc`; `{}` is not",
                            f.path().display(),
                        ))
                        .parse()
                        .unwrap_or_else(|err| panic!("Invalid file name for {}: {err}", path.display()));
                    if level == 0 {
                        panic!("Files in parametric warning directories cannot be named `0.mdoc`",);
                    } else if level > nb_levels {
                        nb_levels = level;
                    }
                }
            }

            if nb_levels < 2 {
                panic!(
                    "Please create at least `1.mdoc` and `2.mdoc` in `{}`",
                    path.display()
                );
            }

            let default_level = default_level.expect("Missing `default` directive");
            (
                warning.file_name().to_string_lossy().into(),
                WarningKind::Parametric {
                    nb_levels,
                    default_level,
                },
                meta,
            )
        } else {
            // This is a simple warning.
            let name = warning
                .file_name()
                .to_string_lossy()
                .strip_suffix(".mdoc")
                .expect("Warning files must have the `.mdoc` extension")
                .into();

            let line = read_first_line(&path, &mut line_buf);
            let mut default = None;
            let mut meta = None;

            for directive in line.split(';') {
                let (name, value) = parse_directive(directive);
                match name {
                    "default" => {
                        if default
                            .replace(value.parse().unwrap_or_else(|err| {
                                panic!("Bad value for `default` directive: {err}")
                            }))
                            .is_some()
                        {
                            panic!("`default` directive specified twice");
                        }
                    }
                    "meta" => {
                        if meta
                            .replace(value.parse().unwrap_or_else(|err| {
                                panic!("Bad value for `meta` directive: {err}")
                            }))
                            .is_some()
                        {
                            panic!("`meta` directive specified twice");
                        }
                    }
                    _ => panic!("Unknown directive `{name}`"),
                }
            }

            let default = default.expect("Missing `default` directive");
            (name, WarningKind::Boolean { default }, meta)
        };

        let meta = meta.unwrap_or(Default::default());
        warnings.push(Warning { name, kind, meta });
    }

    warnings
}

impl FromStr for MetaWarnings {
    type Err = Box<dyn Error>;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut this = Self {
            all: false,
            extra: false,
        };

        for name in s.split(',') {
            match name.trim_matches(|c: char| c.is_ascii_whitespace()) {
                "all" => {
                    if std::mem::replace(&mut this.all, true) {
                        return Err("`all` meta warning specified twice".into());
                    }
                }
                "extra" => {
                    if std::mem::replace(&mut this.extra, true) {
                        return Err("`extra` meta warning specified twice".into());
                    }
                }
                warning => return Err(format!("Unknown meta warning `{warning}`").into()),
            }
        }

        Ok(this)
    }
}
