use cfg_if::cfg_if;

use std::{
    ffi::{OsStr, OsString},
    fs::File,
    io::{BufRead, BufReader},
    path::Path,
};

use rgbds::WHITESPACE_CHARS;

use super::diagnostics::{build_error, ContentlessReport};

pub fn collect_expanded_args() -> Vec<OsString> {
    let args = std::env::args_os();
    let mut expanded = Vec::new();
    for arg in args {
        process_arg(arg, &mut expanded);
    }
    expanded
}

fn process_arg(arg: OsString, expanded: &mut Vec<OsString>) {
    match try_extract_argfile_path(&arg) {
        None => expanded.push(arg),
        Some(path) => {
            let mut file = BufReader::new(File::open(path).unwrap_or_else(|err| {
                build_error()
                    .with_message(format!(
                        "Failed to open arg-file \"{}\": {err}",
                        path.display()
                    ))
                    .finish()
                    .eprint_();
                std::process::exit(2);
            }));

            let mut line_buf = String::new();
            loop {
                line_buf.clear();
                let line_len = file.read_line(&mut line_buf).unwrap_or_else(|err| {
                    build_error()
                        .with_message(format!(
                            "Failed to read arg-file \"{}\": {err}",
                            path.display()
                        ))
                        .finish()
                        .eprint_();
                    std::process::exit(2);
                });
                if line_len == 0 {
                    break; // EOF.
                }

                let line = line_buf
                    .trim_end_matches('\n')
                    .trim_start_matches(WHITESPACE_CHARS);
                // Ignore comments.
                if line.starts_with('#') {
                    continue;
                }

                for arg in line.split_terminator(WHITESPACE_CHARS) {
                    process_arg(arg.to_owned().into(), expanded);
                }
            }
        }
    }
}

fn try_extract_argfile_path(arg: &OsStr) -> Option<&Path> {
    cfg_if! {
        if #[cfg(unix)] {
            use std::os::unix::ffi::OsStrExt;
            arg.as_bytes()
                .strip_prefix(b"@")
                .map(|path| OsStr::from_bytes(path).as_ref())
        }
        // On other platforms, fallback to the method provided by Rust 1.74.
        // This indeed raises the MSRV to 1.74 and makes the `Cargo.toml` lie,
        // but it lets non-Unix platforms compile without patches on Rust 1.74+.
        // Rust 1.73 is only used for macOS 10.9 compat, after all.
        else {
            arg.as_encoded_bytes().strip_prefix(b"@").map(|path| {
                // SAFETY: we merely stripped an ASCII byte off the beginning of a (superset of) UTF-8.
                unsafe { OsStr::from_encoded_bytes_unchecked(path) }.as_ref()
            })
        }
    }
}
