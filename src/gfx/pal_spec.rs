/*
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at https://mozilla.org/MPL/2.0/.
 *
 * SPDX-License-Identifier: MPL-2.0
 */

use std::{
    io::{BufRead, Read},
    num::NonZeroU8,
};

use plumers::prelude::*;
use rgbds::WHITESPACE_CHARS;

use crate::{
    common::{dash_stdio::Input, diagnostics::ContentlessReport},
    rgb::{Rgb, Rgba},
};

pub fn parse_palette_file(
    format: &str,
    path: &str,
    nb_colors_per_pal: NonZeroU8,
    use_curve: bool,
) -> Result<Vec<Vec<Option<Rgb16>>>, ()> {
    let file = Input::new(path.as_ref()).map_err(|err| {
        Input::error(
            path.as_ref(),
            format!("Failed to open external palette file: {err}"),
        )
        .finish()
        .eprint_();
    })?;

    if format.eq_ignore_ascii_case("PSP") {
        parse_psp_file(file, path, nb_colors_per_pal, use_curve)
    } else if format.eq_ignore_ascii_case("GPL") {
        parse_gpl_file(file, path, nb_colors_per_pal, use_curve)
    } else if format.eq_ignore_ascii_case("HEX") {
        parse_hex_file(file, path, nb_colors_per_pal, use_curve)
    } else if format.eq_ignore_ascii_case("ACT") {
        parse_act_file(file, path, nb_colors_per_pal, use_curve)
    } else if format.eq_ignore_ascii_case("ACO") {
        parse_aco_file(file, path, nb_colors_per_pal, use_curve)
    } else if format.eq_ignore_ascii_case("GBC") {
        parse_gbc_file(file, path, nb_colors_per_pal, use_curve)
    } else {
        crate::build_error()
            .with_message(format!("Unknown external palette spec format \"{format}\""))
            .with_help("Supported formats are PSP, GPL, HEX, ACT, ACO, and GBC")
            .finish()
            .eprint_();
        Err(())
    }
}

/// <https://www.selapa.net/swatches/colors/fileformats.php#psp_pal>
pub fn parse_psp_file(
    file: Input,
    path: &str,
    nb_colors_per_pal: NonZeroU8,
    use_curve: bool,
) -> Result<Vec<Vec<Option<Rgb16>>>, ()> {
    let mut lines = Lines::new(file);
    macro_rules! next_line {
        () => {
            lines.next_line().map_err(|err| {
                Input::error(path.as_ref(), format!("Failed to read PSP file: {err}"))
                    .finish()
                    .eprint_();
            })
        };
        (require) => {
            next_line!().and_then(|opt| {
                opt.ok_or_else(|| {
                    Input::error(
                        path.as_ref(),
                        format!("Failed to read PSP file: it appears to be truncated"),
                    )
                    .finish()
                    .eprint_();
                })
            })
        };
    }

    if next_line!(require)? != "JASC-PAL" {
        Input::error(
            path.as_ref(),
            "Palette file does not appear to be a PSP palette file",
        )
        .finish()
        .eprint_();
        return Err(());
    }
    match next_line!(require)? {
        "0100" => {}
        version => {
            Input::error(
                path.as_ref(),
                format!("PSP palette file version {version} is not supported"),
            )
            .finish()
            .eprint_();
            return Err(());
        }
    }

    let nb_colors: u16 = next_line!(require)?.parse().map_err(|err| {
        Input::error(
            path.as_ref(),
            format!("Invalid number of colors in PSP file: {err}"),
        )
        .finish()
        .eprint_();
    })?;
    match nb_colors % u16::from(nb_colors_per_pal.get()) {
        0 => {} // OK!
        leftover => crate::build_warning()
            .with_message(format!(
                "PSP file contains {nb_colors}, not a multiple of {nb_colors_per_pal}"
            ))
            .with_note(format!("The last {leftover} colors will be ignored"))
            .finish()
            .eprint_(),
    }
    match nb_colors % u16::from(nb_colors_per_pal.get()) {
        0 => {} // OK!
        leftover => crate::build_warning()
            .with_message(format!(
                "PSP file contains {nb_colors}, not a multiple of {nb_colors_per_pal}"
            ))
            .with_note(format!("The last {leftover} colors will be ignored"))
            .finish()
            .eprint_(),
    }

    let nb_palettes = nb_colors / u16::from(nb_colors_per_pal.get());

    let mut palettes = Vec::with_capacity(nb_palettes.into());
    for _ in 0..nb_palettes {
        let mut palette = Vec::with_capacity(nb_colors.into());
        for _ in 0..nb_colors_per_pal.get() {
            palette.push(Some(
                Rgba::from(parse_color(
                    next_line!(require)?,
                    path,
                    "Failed to parse PSP file",
                )?)
                .cgb_color(use_curve),
            ));
        }
        palettes.push(palette);
    }

    Ok(palettes)
}

/// <https://gitlab.gnome.org/GNOME/gimp/-/blob/gimp-2-10/app/core/gimppalette-load.c#L39>
pub fn parse_gpl_file(
    file: Input,
    path: &str,
    nb_colors_per_pal: NonZeroU8,
    use_curve: bool,
) -> Result<Vec<Vec<Option<Rgb16>>>, ()> {
    let mut lines = Lines::new(file);
    macro_rules! next_line {
        () => {
            lines.next_line().map_err(|err| {
                Input::error(path.as_ref(), format!("Failed to read GPL file: {err}"))
                    .finish()
                    .eprint_();
            })
        };
        (require) => {
            next_line!().and_then(|opt| {
                opt.ok_or_else(|| {
                    Input::error(
                        path.as_ref(),
                        format!("Failed to read GPL file: it appears to be truncated"),
                    )
                    .finish()
                    .eprint_();
                })
            })
        };
    }

    if !next_line!(require)?.starts_with("GIMP Palette") {
        Input::error(
            path.as_ref(),
            "Palette file does not appear to be a GPL palette file",
        )
        .finish()
        .eprint_();
        return Err(());
    }

    let mut palettes = Vec::with_capacity(8);
    'next_pal: loop {
        palettes.push(Vec::with_capacity(nb_colors_per_pal.get().into()));
        let palette = palettes.last_mut().unwrap();

        while let Some(line) = next_line!()? {
            if line.starts_with('#') || line.starts_with("Name:") || line.starts_with("Column:") {
                continue;
            }

            let color = parse_color(line, path, "Failed to parse GPL file")?;
            palette.push(Some(Rgba::from(color).cgb_color(use_curve)));
            if palette.len() == nb_colors_per_pal.get().into() {
                continue 'next_pal;
            }
        }

        // Remove the last palette, in case it turns out to be empty.
        if palette.is_empty() {
            palettes.pop();
        } else if palette.len() != usize::from(nb_colors_per_pal.get()) {
            crate::build_warning()
                .with_message(format!(
                    "The last {} colors in the GPL file will be ignored",
                    palette.len()
                ))
                .finish()
                .eprint_();
            palettes.pop();
        }
        break;
    }

    Ok(palettes)
}

/// <https://lospec.com/palette-list/tag/gbc>
pub fn parse_hex_file(
    file: Input,
    path: &str,
    nb_colors_per_pal: NonZeroU8,
    use_curve: bool,
) -> Result<Vec<Vec<Option<Rgb16>>>, ()> {
    let mut lines = Lines::new(file);
    let mut palettes = Vec::with_capacity(8);
    'next_pal: loop {
        palettes.push(Vec::with_capacity(nb_colors_per_pal.get().into()));
        let palette = palettes.last_mut().unwrap();

        while let Some(line) = lines.next_line().map_err(|err| {
            Input::error(path.as_ref(), format!("Failed to read HEX file: {err}"))
                .finish()
                .eprint_();
        })? {
            fn parse_line(line: &str) -> Option<Rgb> {
                let mut chars = line.chars();
                let mut digit = || Some(chars.next()?.to_digit(16)? as u8);
                let mut channel = || Some(digit()? * 16 + digit()?);

                let red = channel()?;
                let green = channel()?;
                let blue = channel()?;
                Some(Rgb { red, green, blue })
            }
            let color = parse_line(line).ok_or_else(|| {
                Input::error(
                    path.as_ref(),
                    format!("Failed to read HEX file: failed to parse color out of \"{line}\""),
                )
                .finish()
                .eprint_();
            })?;

            palette.push(Some(Rgba::from(color).cgb_color(use_curve)));
            if palette.len() == nb_colors_per_pal.get().into() {
                continue 'next_pal;
            }
        }

        // Remove the last palette, in case it turns out to be empty.
        if palette.is_empty() {
            palettes.pop();
        } else if palette.len() != usize::from(nb_colors_per_pal.get()) {
            crate::build_warning()
                .with_message(format!(
                    "The last {} colors in the HEX file will be ignored",
                    palette.len()
                ))
                .finish()
                .eprint_();
            palettes.pop();
        }
        break;
    }

    Ok(palettes)
}

/// <https://www.adobe.com/devnet-apps/photoshop/fileformatashtml/#50577411_pgfId-1070626>
pub fn parse_act_file(
    mut file: Input,
    path: &str,
    nb_colors_per_pal: NonZeroU8,
    use_curve: bool,
) -> Result<Vec<Vec<Option<Rgb16>>>, ()> {
    let mut buf = [0; 768 + 4];
    let nb_colors = match file
        .read_exact(&mut buf[..768])
        .and_then(|()| try_fill_buf(file, &mut buf[769..]))
        .map_err(|err| {
            Input::error(path.as_ref(), format!("Failed to read ACT file: {err}"))
                .finish()
                .eprint_();
        })? {
        Ok(()) => u16::from_be_bytes([buf[768], buf[769]]),
        Err(0) => 256,
        Err(len) => {
            Input::error(
                path.as_ref(),
                format!("An ACT file must be either 768 or 772 bytes, not {len}"),
            )
            .finish()
            .eprint_();
            return Err(());
        }
    };

    match nb_colors % u16::from(nb_colors_per_pal.get()) {
        0 => {} // OK!
        leftover => crate::build_warning()
            .with_message(format!(
                "ACT file contains {nb_colors}, not a multiple of {nb_colors_per_pal}"
            ))
            .with_note(format!("The last {leftover} colors will be ignored"))
            .finish()
            .eprint_(),
    }

    Ok((0..(nb_colors / u16::from(nb_colors_per_pal.get())))
        .map(|pal_id| {
            (0..nb_colors_per_pal.get())
                .map(|color_id| {
                    let idx =
                        usize::from(pal_id * u16::from(nb_colors_per_pal.get() + color_id) * 3);
                    Some(
                        Rgba::from(Rgb {
                            red: buf[idx],
                            green: buf[idx + 1],
                            blue: buf[idx + 2],
                        })
                        .cgb_color(use_curve),
                    )
                })
                .collect()
        })
        .collect())
}

/// <https://www.adobe.com/devnet-apps/photoshop/fileformatashtml/#50577411_pgfId-1055819>
pub fn parse_aco_file(
    mut file: Input,
    path: &str,
    nb_colors_per_pal: NonZeroU8,
    use_curve: bool,
) -> Result<Vec<Vec<Option<Rgb16>>>, ()> {
    let mut buf = [0; 10];

    file.read_exact(&mut buf[..4]).map_err(|err| {
        Input::error(
            path.as_ref(),
            format!("Failed to read ACO file's header: {err}"),
        )
        .finish()
        .eprint_();
    })?;
    // Assert the file version.
    if u16::from_be_bytes([buf[0], buf[1]]) != 1 {
        Input::error(
            path.as_ref(),
            "Palette file does not appear to be an ACO v1 file",
        )
        .finish()
        .eprint_();
        return Err(());
    }

    let nb_colors = u16::from_be_bytes([buf[2], buf[3]]);
    match nb_colors % u16::from(nb_colors_per_pal.get()) {
        0 => {} // OK!
        leftover => crate::build_warning()
            .with_message(format!(
                "ACO file contains {nb_colors}, not a multiple of {nb_colors_per_pal}"
            ))
            .with_note(format!("The last {leftover} colors will be ignored"))
            .finish()
            .eprint_(),
    }

    (0..(nb_colors / u16::from(nb_colors_per_pal.get())))
        .map(|_| {
            (0..nb_colors_per_pal.get())
                .map(|_| {
                    file.read_exact(&mut buf).map_err(|err| {
                        Input::error(path.as_ref(), format!("Failed to read ACO file: {err}"))
                            .finish()
                            .eprint_();
                    })?;

                    let color_kind = u16::from_be_bytes([buf[0], buf[1]]);
                    let color = match color_kind {
                        0 => Ok(Rgb {
                            // Only keep the MSB of the (big-endian) 16-bit values.
                            red: buf[2],
                            green: buf[4],
                            blue: buf[6],
                        }),
                        1 => {
                            file.error_in("Color type HSB is not supported in ACO files")
                                .finish()
                                .eprint_();
                            Err(())
                        }
                        2 => {
                            file.error_in("Color type CMYK is not supported in ACO files")
                                .finish()
                                .eprint_();
                            Err(())
                        }
                        7 => {
                            file.error_in("Color type Lab is not supported in ACO files")
                                .finish()
                                .eprint_();
                            Err(())
                        }
                        8 => {
                            file.error_in("Color type Grayscale is not supported in ACO files")
                                .finish()
                                .eprint_();
                            Err(())
                        }
                        id => {
                            file.error_in(format!("Unknown color type {id} in ACO file"))
                                .finish()
                                .eprint_();
                            Err(())
                        }
                    }?;
                    Ok(Some(Rgba::from(color).cgb_color(use_curve)))
                })
                .collect()
        })
        .collect()
}

/// The kind of file that rgbgfx itself emits.
pub fn parse_gbc_file(
    mut file: Input,
    _path: &str,
    _nb_colors_per_pal: NonZeroU8,
    _use_curve: bool,
) -> Result<Vec<Vec<Option<Rgb16>>>, ()> {
    let mut pals = Vec::new();
    let mut buf = [0; 8];
    loop {
        match try_fill_buf(&mut file, &mut buf).map_err(|err| {
            file.error_in(format!("Failed to read GBC palette dump: {err}"))
                .finish()
                .eprint_();
        })? {
            Ok(()) => {}
            Err(0) => break,
            Err(n) => {
                file.error_in(
                    format!("The GBC palette dump does not contain an integer amount of palettes ({} plus {n} bytes)", pals.len())
                ).finish().eprint_();
                return Err(());
            }
        }

        pals.push(vec![
            Some(Rgb16(u16::from_le_bytes([buf[0], buf[1]]))),
            Some(Rgb16(u16::from_le_bytes([buf[2], buf[3]]))),
            Some(Rgb16(u16::from_le_bytes([buf[4], buf[5]]))),
            Some(Rgb16(u16::from_le_bytes([buf[6], buf[7]]))),
        ]);
    }

    Ok(pals)
}

fn parse_color(string: &str, file_path: &str, err_msg: &str) -> Result<Rgb, ()> {
    fn inner(string: &str) -> Option<Rgb> {
        let (red_str, rest) = string.split_once(WHITESPACE_CHARS)?;
        let (green_str, blue_str) = rest.split_once(WHITESPACE_CHARS)?;
        let red = red_str.parse().ok()?;
        let green = green_str.parse().ok()?;
        let blue = blue_str.parse().ok()?;
        Some(Rgb { red, green, blue })
    }

    inner(string).ok_or_else(|| {
        Input::error(
            file_path.as_ref(),
            format!("{err_msg}: failed to parse color out of \"{string}\""),
        )
        .with_help("Expected a RGB color like `128 255 42`")
        .finish()
        .eprint_();
    })
}

fn try_fill_buf<R: Read>(mut input: R, buf: &mut [u8]) -> std::io::Result<Result<(), usize>> {
    let mut nb_read = 0;
    while nb_read < buf.len() {
        match input.read(&mut buf[nb_read..]) {
            // EOF
            Ok(0) => return Ok(Err(nb_read)),
            Ok(n) => nb_read += n,
            Err(err) => match err.kind() {
                std::io::ErrorKind::Interrupted => continue, // Simply retry.
                _ => return Err(err),
            },
        }
    }
    Ok(Ok(()))
}

#[derive(Debug, Clone)]
struct Lines<BR> {
    file: BR,
    line_buf: String,
}

impl<BR: BufRead> Lines<BR> {
    fn new(file: BR) -> Self {
        Self {
            file,
            line_buf: String::new(),
        }
    }

    fn next_line(&mut self) -> std::io::Result<Option<&str>> {
        self.line_buf.clear();
        Ok(match self.file.read_line(&mut self.line_buf)? {
            0 => None,
            _ => Some(self.line_buf.trim_end_matches('\n')),
        })
    }
}
