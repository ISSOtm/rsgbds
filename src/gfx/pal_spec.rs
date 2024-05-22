use std::{
    io::{BufRead, Read},
    num::NonZeroU8,
};

use plumers::prelude::*;
use rgbds::{common::dash_stdio::Input, WHITESPACE_CHARS};

use crate::{
    rgb::{Rgb, Rgba},
    Diagnostic,
};

pub fn parse_palette_file(
    format: &str,
    path: &str,
    nb_colors_per_pal: NonZeroU8,
    use_curve: bool,
) -> Result<Vec<Vec<Option<Rgb16>>>, Diagnostic> {
    let file = Input::new(path).map_err(|err| {
        crate::input_error(format!("Failed to open external palette file: {err}"), path)
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
        Err(Diagnostic::error()
            .with_message(format!("Unknown external palette spec format \"{format}\"")))
    }
}

/// https://www.selapa.net/swatches/colors/fileformats.php#psp_pal
pub fn parse_psp_file(
    file: Input,
    path: &str,
    nb_colors_per_pal: NonZeroU8,
    use_curve: bool,
) -> Result<Vec<Vec<Option<Rgb16>>>, Diagnostic> {
    let input_error = |err_msg| crate::input_error(err_msg, path);

    let mut lines = Lines::new(file);
    macro_rules! next_line {
        () => {
            lines
                .next_line()
                .map_err(|err| input_error(format!("Failed to read PSP file: {err}")))
        };
        (require) => {
            next_line!().and_then(|opt| {
                opt.ok_or_else(|| {
                    input_error(format!(
                        "Failed to read PSP file: it appears to be truncated"
                    ))
                })
            })
        };
    }

    if next_line!(require)? != "JASC-PAL" {
        return Err(input_error(
            "Palette file does not appear to be a PSP palette file".into(),
        ));
    }
    match next_line!(require)? {
        "0100" => {}
        version => {
            return Err(input_error(format!(
                "PSP palette file version {version} is not supported"
            )))
        }
    }

    let nb_colors: u16 = next_line!(require)?
        .parse()
        .map_err(|err| input_error(format!("Invalid number of colors in PSP file: {err}")))?;
    match nb_colors % u16::from(nb_colors_per_pal.get()) {
        0 => {} // OK!
        leftover => {
            todo!() // Warn about it. (TODO: do the same elsewhere this division is performed...)
        }
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

/// https://gitlab.gnome.org/GNOME/gimp/-/blob/gimp-2-10/app/core/gimppalette-load.c#L39
pub fn parse_gpl_file(
    file: Input,
    path: &str,
    nb_colors_per_pal: NonZeroU8,
    use_curve: bool,
) -> Result<Vec<Vec<Option<Rgb16>>>, Diagnostic> {
    let input_error = |err_msg| crate::input_error(err_msg, path);

    let mut lines = Lines::new(file);
    macro_rules! next_line {
        () => {
            lines
                .next_line()
                .map_err(|err| input_error(format!("Failed to read GPL file: {err}")))
        };
        (require) => {
            next_line!().and_then(|opt| {
                opt.ok_or_else(|| {
                    input_error(format!(
                        "Failed to read GPL file: it appears to be truncated"
                    ))
                })
            })
        };
    }

    if !next_line!(require)?.starts_with("GIMP Palette") {
        return Err(input_error(
            "Palette file does not appear to be a GPL palette file".into(),
        ));
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
        }
        break;
    }

    Ok(palettes)
}

/// https://lospec.com/palette-list/tag/gbc
pub fn parse_hex_file(
    file: Input,
    path: &str,
    nb_colors_per_pal: NonZeroU8,
    use_curve: bool,
) -> Result<Vec<Vec<Option<Rgb16>>>, Diagnostic> {
    let input_error = |err_msg| crate::input_error(err_msg, path);

    let mut lines = Lines::new(file);
    let mut palettes = Vec::with_capacity(8);
    'next_pal: loop {
        palettes.push(Vec::with_capacity(nb_colors_per_pal.get().into()));
        let palette = palettes.last_mut().unwrap();

        while let Some(line) = lines
            .next_line()
            .map_err(|err| input_error(format!("Failed to read HEX file: {err}")))?
        {
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
                input_error(format!(
                    "Failed to read HEX file: failed to parse color out of \"{line}\""
                ))
            })?;

            palette.push(Some(Rgba::from(color).cgb_color(use_curve)));
            if palette.len() == nb_colors_per_pal.get().into() {
                continue 'next_pal;
            }
        }

        // Remove the last palette, in case it turns out to be empty.
        if palette.is_empty() {
            palettes.pop();
        }
        break;
    }

    Ok(palettes)
}

/// https://www.adobe.com/devnet-apps/photoshop/fileformatashtml/#50577411_pgfId-1070626
pub fn parse_act_file(
    mut file: Input,
    path: &str,
    nb_colors_per_pal: NonZeroU8,
    use_curve: bool,
) -> Result<Vec<Vec<Option<Rgb16>>>, Diagnostic> {
    let input_error = |err_msg| crate::input_error(err_msg, path);

    let mut buf = [0; 768 + 4];
    let nb_colors = match file
        .read_exact(&mut buf[..768])
        .and_then(|()| try_fill_buf(file, &mut buf[769..]))
        .map_err(|err| input_error(format!("Failed to read ACT file: {err}")))?
    {
        Ok(()) => u16::from_be_bytes([buf[768], buf[769]]),
        Err(0) => 256,
        Err(len) => {
            return Err(input_error(format!(
                "An ACT file must be either 768 or 772 bytes, not {len}"
            )))
        }
    };

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

/// https://www.adobe.com/devnet-apps/photoshop/fileformatashtml/#50577411_pgfId-1055819
pub fn parse_aco_file(
    mut file: Input,
    path: &str,
    nb_colors_per_pal: NonZeroU8,
    use_curve: bool,
) -> Result<Vec<Vec<Option<Rgb16>>>, Diagnostic> {
    let input_error = |err_msg| crate::input_error(err_msg, path);
    let mut buf = [0; 10];

    file.read_exact(&mut buf[..4])
        .map_err(|err| input_error(format!("Failed to read ACO file's header: {err}")))?;
    // Assert the file version.
    if u16::from_be_bytes([buf[0], buf[1]]) != 1 {
        return Err(input_error(
            "Palette file does not appear to be an ACO v1 file".into(),
        ));
    }

    let nb_colors = u16::from_be_bytes([buf[2], buf[3]]);
    (0..(nb_colors / u16::from(nb_colors_per_pal.get())))
        .map(|_| {
            (0..nb_colors_per_pal.get())
                .map(|_| {
                    file.read_exact(&mut buf)
                        .map_err(|err| input_error(format!("Failed to read ACO file: {err}")))?;

                    let color_kind = u16::from_be_bytes([buf[0], buf[1]]);
                    let color = match color_kind {
                        0 => Ok(Rgb {
                            // Only keep the MSB of the (big-endian) 16-bit values.
                            red: buf[2],
                            green: buf[4],
                            blue: buf[6],
                        }),
                        1 => Err(input_error(
                            "Color type HSB is not supported in ACO files".into(),
                        )),
                        2 => Err(input_error(
                            "Color type CMYK is not supported in ACO files".into(),
                        )),
                        7 => Err(input_error(
                            "Color type Lab is not supported in ACO files".into(),
                        )),
                        8 => Err(input_error(
                            "Color type Grayscale is not supported in ACO files".into(),
                        )),
                        id => Err(input_error(format!("Unknown color type {id} in ACO file"))),
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
    path: &str,
    _nb_colors_per_pal: NonZeroU8,
    _use_curve: bool,
) -> Result<Vec<Vec<Option<Rgb16>>>, Diagnostic> {
    let mut pals = Vec::new();
    let mut buf = [0; 8];
    loop {
        match try_fill_buf(&mut file, &mut buf).map_err(|err| {
            crate::input_error(format!("Failed to read GBC palette dump: {err}"), path)
        })? {
            Ok(()) => {}
            Err(0) => break,
            Err(n) => {
                let mut diag = crate::input_error(
                    "The GBC palette dump does not contain an integer amount of palettes",
                    path,
                );
                diag.notes.push(format!(
                    "There were only {n} bytes after the first {} palettes",
                    pals.len()
                ));
                return Err(diag);
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

fn parse_color(string: &str, file_path: &str, err_msg: &str) -> Result<Rgb, Diagnostic> {
    fn inner(string: &str) -> Option<Rgb> {
        let (red_str, rest) = string.split_once(WHITESPACE_CHARS)?;
        let (green_str, blue_str) = rest.split_once(WHITESPACE_CHARS)?;
        let red = red_str.parse().ok()?;
        let green = green_str.parse().ok()?;
        let blue = blue_str.parse().ok()?;
        Some(Rgb { red, green, blue })
    }

    inner(string).ok_or_else(|| {
        let mut diag = crate::input_error(
            format!("{err_msg}: failed to parse color out of \"{string}\""),
            file_path,
        );
        diag.notes
            .push("Expected a RGB color like `128 255 42`".into());
        diag
    })
}

fn try_fill_buf<R: Read>(mut input: R, buf: &mut [u8]) -> std::io::Result<Result<(), usize>> {
    let mut nb_read = 0;
    while !nb_read != buf.len() {
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
