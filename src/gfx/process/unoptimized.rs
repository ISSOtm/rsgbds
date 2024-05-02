use std::{
    fs::File,
    io::Write,
    path::{Path, PathBuf},
};

use plumers::{
    color::Rgb32,
    image::{DynImage32, Frame},
};

use crate::{palette::Palette, Diagnostic, InputSlice, Options};

use super::{AttrmapEntry, TileData};

pub(super) fn output_tile_data(
    frame: &Frame<'_, Rgb32, DynImage32>,
    attrmap: &[AttrmapEntry],
    palettes: &[Palette],
    mappings: &[usize],
    path: &Path,
    input_slice: &InputSlice,
    options: &Options,
    has_transparency: bool,
) -> Result<(), Diagnostic> {
    let mut output = File::create(path).map_err(|err| {
        super::file_error(format!("Failed to create tile data file: {err}"), path)
    })?;

    let width_in_tiles = input_slice.width.get() / 8;
    let height_in_tiles = input_slice.height.get() / 8;
    if let Some(remaining_tiles) =
        (usize::from(width_in_tiles) * usize::from(height_in_tiles)).checked_sub(options.trim)
    {
        debug_assert_eq!(
            input_slice.iter_tiles(frame, options.column_major).count(),
            attrmap.len()
        );
        for (tile, attr) in input_slice
            .iter_tiles(frame, options.column_major)
            .zip(attrmap)
            .take(remaining_tiles)
        {
            // If the tile is fully transparent, it will get palette 0, since that's always there.
            // (TODO: test what happens with a fully-transparent image?)
            let palette = &palettes[attr.get_pal_id(mappings)];
            for y in 0..8 {
                let bitplanes =
                    TileData::row_bitplanes(&tile, palette, y, options, has_transparency)
                        .to_le_bytes();
                output
                    .write_all(&bitplanes[..usize::from(options.bit_depth)])
                    .map_err(|err| {
                        super::file_error(format!("Failed to write tile data: {err}"), path)
                    })?;
            }
        }
    };

    Ok(())
}

pub(super) fn output_maps(
    attrmap: &[AttrmapEntry],
    mappings: &[usize],
    options: &Options,
) -> Result<(), Diagnostic> {
    fn auto_open_path<'path>(
        path: &'path Option<PathBuf>,
        what: &'static str,
    ) -> Result<Option<(&'path PathBuf, File)>, Diagnostic> {
        path.as_ref()
            .map(|path| {
                File::create(path)
                    .map_err(|err| {
                        super::file_error(format!("Failed to create {what} file: {err}"), path)
                    })
                    .map(|file| (path, file))
            })
            .transpose()
    }
    let mut tilemap_output = auto_open_path(&options.tilemap_path, "tilemap")?;
    let mut attrmap_output = auto_open_path(&options.attrmap_path, "attribute map")?;
    let mut palmap_output = auto_open_path(&options.palmap_path, "palette map")?;

    let mut tile_id = 0;
    let mut bank = 0;
    for attr in attrmap {
        if let Some(max_nb_tiles) = options.max_nb_tiles {
            if tile_id == max_nb_tiles[usize::from(bank)] {
                debug_assert_eq!(bank, 0);
                bank = 1;
                tile_id = 0;
            }
        }

        let emit = |opt: &mut Option<_>, byte, what| {
            opt.as_mut()
                .map_or(Ok(()), |(path, output): &mut (&PathBuf, File)| {
                    output.write_all(&[byte]).map_err(|err| {
                        super::file_error(format!("Failed to write {what}: {err}"), path)
                    })
                })
        };
        emit(
            &mut tilemap_output,
            tile_id + options.base_tile_ids[usize::from(bank)],
            "tilemap",
        )?;
        let pal_id = attr.get_pal_id(mappings) as u8;
        emit(&mut attrmap_output, pal_id & 7 | bank << 3, "attribute map")?;
        emit(&mut palmap_output, pal_id, "palette map")?;

        tile_id += 1;
    }

    Ok(())
}
