use std::{
    collections::{hash_map::Entry, HashMap},
    fs::File,
    io::Write,
    path::Path,
};

use plumers::{
    image::Frame,
    prelude::{DynImage32, Rgb32},
};

use crate::{palette::Palette, Diagnostic, InputSlice, Options};

use super::{AttrmapEntry, TileData, TileMatchKind};

pub(super) fn generate_unique_tiles(
    frame: &Frame<'_, Rgb32, DynImage32>,
    attrmap: &mut [AttrmapEntry],
    palettes: &[Palette],
    mappings: &[usize],
    input_slice: &InputSlice,
    options: &Options,
    has_transparency: bool,
) -> Vec<TileData> {
    let mut tiles = Vec::new();

    let mut tile_ids = HashMap::new();
    debug_assert_eq!(
        input_slice.iter_tiles(frame, options.column_major).count(),
        attrmap.len()
    );
    for (tile, attr) in input_slice
        .iter_tiles(frame, options.column_major)
        .zip(attrmap)
    {
        let tile_data = TileData::new(
            &tile,
            &palettes[mappings[attr.color_set_id]],
            options,
            has_transparency,
        );

        // FIXME: boo for that `.clone()`!
        let (tile_id, match_kind) = match tile_ids.entry(tile_data.clone()) {
            Entry::Vacant(entry) => {
                let new_tile_id = *entry.insert(tiles.len());
                tiles.push(tile_data);
                (
                    new_tile_id,
                    TileMatchKind {
                        vflip: false,
                        hflip: false,
                    },
                )
            }
            Entry::Occupied(entry) => {
                let tile_id = *entry.get();
                (
                    tile_id,
                    tiles[tile_id]
                        .try_matching(&tile_data)
                        .expect("Equal tiles are supposed to match?!?"),
                )
            }
        };
        attr.horiz_flip = match_kind.hflip;
        attr.vert_flip = match_kind.vflip;
        let tile_id = match &options.max_nb_tiles {
            Some(max_nb_tiles) if tile_id >= usize::from(max_nb_tiles[0]) => {
                debug_assert!(
                    tile_id < usize::from(max_nb_tiles[0]) + usize::from(max_nb_tiles[1])
                );
                attr.bank = true;
                tile_id - usize::from(max_nb_tiles[0])
            }
            _ => {
                debug_assert!(!attr.bank);
                tile_id
            }
        };
        attr.tile_id = (tile_id as u8).wrapping_add(options.base_tile_ids[attr.bank as usize]);
    }

    tiles
}

pub(super) fn output_tile_data(
    tile_data: &[TileData],
    path: &Path,
    options: &Options,
) -> Result<(), Diagnostic> {
    let mut output = File::create(path).map_err(|err| {
        crate::file_error(format!("Failed to create tile data file: {err}"), path)
    })?;

    let nb_tiles = tile_data.len().saturating_sub(options.trim);
    for tile in &tile_data[..nb_tiles] {
        output
            .write_all(&tile.bytes)
            .map_err(|err| crate::file_error(format!("Failed to write tile data: {err}"), path))?;
    }

    Ok(())
}

pub(super) fn output_tilemap(attrmap: &[AttrmapEntry], path: &Path) -> Result<(), Diagnostic> {
    let mut output = File::create(path)
        .map_err(|err| crate::file_error(format!("Failed to create tilemap file: {err}"), path))?;

    for attr in attrmap {
        output
            .write_all(&[attr.tile_id])
            .map_err(|err| crate::file_error(format!("Failed to write tilemap: {err}"), path))?;
    }

    Ok(())
}

pub(super) fn output_attrmap(
    attrmap: &[AttrmapEntry],
    mappings: &[usize],
    path: &Path,
) -> Result<(), Diagnostic> {
    let mut output = File::create(path).map_err(|err| {
        crate::file_error(format!("Failed to create attribute map file: {err}"), path)
    })?;

    for attr in attrmap {
        output
            .write_all(&[(attr.vert_flip as u8) << 6
                | (attr.horiz_flip as u8) << 5
                | (attr.bank as u8) << 3
                | attr.get_pal_id(mappings) as u8 & 7])
            .map_err(|err| {
                crate::file_error(format!("Failed to write attribute map: {err}"), path)
            })?;
    }

    Ok(())
}

pub(super) fn output_palmap(
    attrmap: &[AttrmapEntry],
    mappings: &[usize],
    path: &Path,
) -> Result<(), Diagnostic> {
    let mut output = File::create(path).map_err(|err| {
        crate::file_error(format!("Failed to create palette map file: {err}"), path)
    })?;

    for attr in attrmap {
        output
            .write_all(&[attr.get_pal_id(mappings) as u8])
            .map_err(|err| {
                crate::file_error(format!("Failed to write palette map: {err}"), path)
            })?;
    }

    Ok(())
}
