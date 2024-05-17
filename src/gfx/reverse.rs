use std::{
    fs::File,
    num::{NonZeroU16, NonZeroUsize},
};

use arrayvec::ArrayVec;
use plumers::prelude::*;

use crate::{error::Reporter, rgb::Rgba, Diagnostic, Nth, Options, PalSpec};

pub(super) fn reverse(
    width: NonZeroU16,
    options: &Options,
    pal_spec: Option<PalSpec>,
    reporter: &mut Reporter,
) -> Result<(), Diagnostic> {
    // Check for weird CLI flag combinations.

    if options.allow_dedup && options.tilemap_path.is_none() {
        reporter.report(
            &Diagnostic::warning()
                .with_message("Tile deduplication is enabled, but no tilemap was provided?"),
        );
    }

    if options.use_color_curve {
        // TODO
        reporter.report(
            &Diagnostic::warning()
                .with_message("The color curve is not yet supported in reverse mode"),
        );
    }

    if let Some(slice) = options.input_slice.as_ref() {
        reporter.report(
            &Diagnostic::warning()
                .with_message("\"Sliced-off\" pixels are ignored in reverse mode"),
        );
        if slice.width != width {
            reporter.report(&Diagnostic::warning().with_message(format!(
                "Specified input slice width ({}) doesn't match reversing width ({})",
                slice.width, width
            )));
        }
    }

    // Read the tile data.

    let output_path = options.output_path.as_ref().unwrap(); // Guaranteed by CLI.
    let tiles = std::fs::read(output_path).map_err(|err| {
        crate::file_error(format!("Failed to read tile data: {err}"), output_path)
    })?;
    let tile_len = 8 * usize::from(options.bit_depth);
    let nb_tiles = tiles.len() / tile_len;
    let remainder = tiles.len() % tile_len;
    if remainder != 0 {
        let nb_bytes = tiles.len() - remainder;
        return Err(Diagnostic::error()
            .with_message("Tile data was not an integer amount of tiles")
            .with_notes(vec![
                format!(
                    "Read {} bytes, expected {nb_tiles} ({nb_bytes} tiles) or {} ({} tiles)",
                    tiles.len(),
                    nb_tiles + 1,
                    nb_bytes + tile_len,
                ),
                "Consider specifying a different bit depth".into(),
            ]));
    }

    // Determine the image's dimensions.

    let (nb_tile_instances, tilemap) = match options.tilemap_path.as_ref() {
        Some(path) => {
            let tilemap = std::fs::read(path)
                .map_err(|err| crate::file_error(format!("Failed to read tilemap: {err}"), path))?;
            (tilemap.len(), Some(tilemap))
        }
        // Assume what the user wants is basically a tile sheet/atlas.
        None => (nb_tiles + options.trim, None),
    };
    let nb_tile_instances = NonZeroUsize::new(nb_tile_instances)
        .ok_or_else(|| Diagnostic::error().with_message("Cannot generate an empty image"))?;
    if let Some(&[bank0, bank1]) = options.max_nb_tiles.as_ref() {
        if nb_tile_instances.get() > usize::from(bank0) + usize::from(bank1) {
            reporter.report(&Diagnostic::error().with_message(format!("Read more tiles ({nb_tile_instances}) than the limit would permit ({bank0} + {bank1})")));
        }
    }

    if nb_tile_instances.get() % NonZeroUsize::from(width) != 0 {
        return Err(Diagnostic::error().with_message(format!("Total number of tiles ({nb_tile_instances}) is not a multiple of the image width ({width} tiles)")));
    }
    let height = nb_tile_instances.get() / NonZeroUsize::from(width).get();
    let coords = |i| (i % usize::from(width.get()), i / usize::from(width.get()));

    // TODO: `-U` to configure tile size beyond 8x8 px ("deduplication units")

    let palettes = if let Some(path) = options.palettes_path.as_ref() {
        let mut file = File::open(path).map_err(|err| {
            crate::file_error(format!("Failed to open palettes file: {err}"), path)
        })?;
        let mut palettes = Vec::new(); // A `stat` call is probably slower than a couple of reallocs.

        let mut buf = [0; 2 * 4]; // Enough to read one palette.
        while let Some(()) = crate::try_reading(
            &mut buf[..2 * usize::from(options.nb_colors_per_pal.get())],
            &mut file,
        )
        .map_err(|err| crate::file_error(format!("Error reading palette data: {err}"), path))?
        {
            let mut palette = ArrayVec::new();
            for i in 0..usize::from(options.nb_colors_per_pal.get()) {
                let color = Rgb16(u16::from_le_bytes([buf[i * 2], buf[i * 2 + 1]]));
                palette.push(color.into());
            }

            palettes.push(palette);
        }

        if palettes.len() > options.nb_palettes.into() {
            reporter.report(&Diagnostic::warning().with_message(format!(
                "Read {} palettes, exceeding the limit of {}",
                palettes.len(),
                options.nb_palettes
            )));
        }

        if let Some(PalSpec::Explicit(colors)) = &pal_spec {
            if palettes.len() != colors.len()
                || std::iter::zip(&palettes, colors).any(|(effective, specified)| todo!())
            {
                reporter.report(
                    &Diagnostic::warning()
                        .with_message("Colors in the palette file do not match what's specified on the command line!")
                        // TODO: format the colors
                        .with_notes(vec![format!("File colors:\n"), format!("Command line colors;\n")])
                );
            }
        }

        palettes
    } else if let Some(PalSpec::Explicit(colors)) = &pal_spec {
        colors
            .iter()
            .map(|palette| palette.iter().map(|&rgb| rgb.into()).collect())
            .collect()
    } else {
        if matches!(&pal_spec, Some(PalSpec::Embedded)) {
            reporter.report(
                &Diagnostic::warning()
                    .with_message(
                        "An embedded palette was requested, but no palette file was specified",
                    )
                    .with_notes(vec!["Ignoring the request".into()]),
            );
        }

        let gray = |intensity| Rgba {
            red: intensity,
            green: intensity,
            blue: intensity,
            alpha: 0xFF,
        };
        vec![[gray(0xFF), gray(0xAA), gray(0x55), gray(0x00)].into()]
    };

    let attrmap = options.attrmap_path.as_ref().map(|path| {
        let attrmap = std::fs::read(path).map_err(|err| {
            crate::file_error(format!("Failed to open attribute map file: {err}"), path)
        })?;

        if attrmap.len() != nb_tile_instances.get() {
            return Err(
                Diagnostic::error()
                    .with_message("The attribute map's size doesn't match the image's")
                    .with_notes(vec![
                        format!("The attribute map is {} tiles long, but the image is {nb_tile_instances} instead", attrmap.len()),
                    ])
            );
        }

        // Scan through the attributes for inconsistencies.
        // We do this now for two reasons:
        // - Checking those during the image generation is more harmful to optimization.
        // - It helps keep the code more "focused" and locally less complex.
        let mut bad = false;
        for (i,attr) in attrmap.iter().enumerate() {
            let (x, y) = coords(i);

            if usize::from(attr & 0b111) >= palettes.len() {
                reporter.report(
                    &Diagnostic::error()
                        .with_message(format!("The attribute at ({x}, {y}) is referencing a palette that doesn't exist"))
                        .with_notes(vec![
                            format!("There are only {} palettes, but palette #{} is being referenced", palettes.len(), attr & 0b111),
                            pixel_coords_str(x, y),
                        ]));
                bad = true;
            }

            if attr & 0x08 != 0 && tilemap.is_none() {
                reporter.report(
                    &Diagnostic::warning()
                        .with_message(format!("The attribute at ({x}, {y}) indicates VRAM bank 1, but no tilemap was provided"))
                        .with_notes(vec![
                            "The bank bit will be ignored".into(),
                            pixel_coords_str(x, y),
                        ]),
                );
            }
        }
        if bad {
            return Err(
                Diagnostic::error()
                    .with_message("The attribute map is invalid")
                    .with_notes(vec!["See previous errors".into()])
            );
        }

        Ok(attrmap)
    }).transpose()?;

    if let (Some(tilemap), Some(max_nb_tiles)) = (tilemap.as_ref(), options.max_nb_tiles.as_ref()) {
        match attrmap.as_ref() {
            Some(attrmap) => {
                for (i, (&id, &attr)) in std::iter::zip(tilemap, attrmap).enumerate() {
                    let (x, y) = coords(i);
                    let bank = usize::from(attr >> 3 & 1);
                    let min_id = options.base_tile_ids[bank];

                    let pos = id.wrapping_sub(min_id);
                    if u16::from(pos) >= max_nb_tiles[bank] {
                        reporter.report(
                            &Diagnostic::error()
                                .with_message(format!(
                                    "The tile ID at ({x}, {y}) is outside of VRAM bank {bank}'s bounds"
                                ))
                                .with_notes(vec![
                                    format!("Tile ID {id} would be the {} tile, but VRAM bank {bank} can only contain {}", Nth(pos.into()), max_nb_tiles[bank]),
                                    pixel_coords_str(x, y),
                                ]),
                        );
                    }
                }
            }

            None => {
                for (i, &id) in tilemap.iter().enumerate() {
                    let (x, y) = coords(i);
                    let min_id = options.base_tile_ids[0];

                    let pos = id.wrapping_sub(min_id);
                    if u16::from(pos) >= max_nb_tiles[0] {
                        reporter.report(
                            &Diagnostic::error()
                                .with_message(format!(
                                    "The tile ID at ({x}, {y}) is outside of VRAM bank 0's bounds"
                                ))
                                .with_notes(vec![
                                    format!("Tile ID {id} would be the {} tile, but VRAM bank 0 can only contain {}", Nth(pos.into()), max_nb_tiles[0]),
                                    pixel_coords_str(x, y),
                                ]),
                        );
                    }
                }
            }
        }
    }

    let palmap = options.palmap_path.as_ref().map(|path| {
        let palmap = std::fs::read(path).map_err(|err| crate::file_error(format!("Failed to read palette map: {err}"), path))?;

        if palmap.len() != nb_tile_instances.get() {
            return Err(
                Diagnostic::error()
                    .with_message("The palette map's size doesn't match the image's")
                    .with_notes(vec![
                        format!("The palette map is {} tiles long, but the image is {nb_tile_instances} instead", palmap.len()),
                    ])
            );
        }

        Ok(palmap)
    }).transpose()?;

    // FIXME: not great to zero the whole image before overwriting all pixels, but `new` doesn't allow returning errors.
    //        Does this cost performance?
    let mut image = DirectImage32::new_zeroed(
        ImageFormat::Png,
        AlphaMode::ZeroIsOpaque,
        1,
        usize::from(width.get()) * 8,
        height * 8,
    );
    for tile_y in 0..height {
        for tile_x in 0..usize::from(width.get()) {
            let index = if options.column_major {
                tile_y + tile_x * height
            } else {
                tile_y * usize::from(width.get()) + tile_x
            };

            // By default, a tile is unflipped, in bank 0, and uses palette #0.
            let attribute = match &attrmap {
                Some(attrmap) => attrmap[index],
                None => 0x00,
            };
            let bank = attribute >> 3 & 1;

            // Get the tile ID at that location.
            let tile_id = match &tilemap {
                Some(tilemap) => {
                    let raw_id =
                        tilemap[index].wrapping_sub(options.base_tile_ids[usize::from(bank)]);
                    match options.max_nb_tiles {
                        Some([bank0, _]) if bank != 0 => raw_id.wrapping_add(bank0 as u8),
                        _ => raw_id,
                    }
                }
                // TODO: this ignores the bank and base tile IDs, but is a warning emitted to highlight that?
                // TODO: this is incorrect in column_major mode!
                None => index as u8,
            };
            debug_assert!(usize::from(tile_id) < nb_tiles + options.trim);

            let palette_id = match &palmap {
                Some(palmap) => palmap[index],
                None => attribute & 0b111,
            };
            let palette = &palettes[usize::from(palette_id)];

            let tile_ofs = usize::from(tile_id) * tile_len;
            let tile_data = tiles.get(tile_ofs..tile_ofs + 16).unwrap_or(&[0; 16]);

            for y in 0..8 {
                // If vertically mirrored, fetch the bytes from the other end.
                let y_ofs =
                    if attribute & 0x40 != 0 { 7 - y } else { y } * usize::from(options.bit_depth);

                let get_plane = |plane_id: u8| {
                    let raw_plane = tile_data[y_ofs + usize::from(plane_id % options.bit_depth)];
                    // Handle horizontal flip.
                    if attribute & 0x20 != 0 {
                        raw_plane.reverse_bits()
                    } else {
                        raw_plane
                    }
                };
                let mut bitplane0 = get_plane(0);
                let mut bitplane1 = get_plane(1);

                for x in 0..8 {
                    let bit0 = bitplane0 >> 7;
                    let bit1 = bitplane1 >> 7;
                    let color_id = bit0 | bit1 << 1;

                    let color = *palette.get(usize::from(color_id))
                            .ok_or_else(|| {
                                Diagnostic::error()
                                    .with_message("Attempting to index out of bounds into a palette")
                                    .with_notes(vec![
                                        format!("Palette #{palette_id} contains only {} colors, so color #{color_id} does not exist", palette.len()),
                                        pixel_coords_str(tile_x, tile_y),
                                    ])
                            })?;
                    *image.pixel_mut(0, tile_x * 8 + x, tile_y * 8 + y) = color.into();

                    // Shift the pixel out.
                    bitplane0 <<= 1;
                    bitplane1 <<= 1;
                }
            }
        }
    }

    let path = options
        .input_path
        .as_ref()
        .expect("Can't reverse to no file!");
    let output = File::create(path)
        .map_err(|err| crate::file_error(format!("Failed to create image file: {err}"), path))?;
    image
        .store(output)
        .map_err(|err| crate::file_error(format!("Failed to write image file: {err}"), path))?;

    Ok(())
}

fn pixel_coords_str(x: usize, y: usize) -> String {
    format!("The tile's top left pixel is at ({}, {})", x * 8, y * 8)
}
