use std::{
    cmp::Ordering, collections::HashSet, fmt::Display, fs::File, io::Write, num::NonZeroU16,
    path::Path,
};

use plumers::{image::Frame, prelude::*};

use crate::{
    color_set::ColorSet,
    error::Reporter,
    palette::Palette,
    rgb::{Opacity, Rgb, Rgba},
    Diagnostic, InputSlice, Options, PalSpec, Tile,
};

mod optimized;
mod unoptimized;

pub(crate) fn process(
    input_path: &Path,
    options: &Options,
    pal_spec: Option<PalSpec>,
    reporter: &mut Reporter,
) -> Result<(), Diagnostic> {
    let file = File::open(input_path).map_err(|err| {
        Diagnostic::error().with_message(format!("Failed to open input image: {err}"))
    })?;
    let image = DynImage32::load(
        file,
        LoadFlags {
            remove_alpha: false,
            palette_sort: Default::default(),
            sort_existing: false,
            reduce_palette: false,
        },
        false,
        false,
    )
    .map_err(|err| {
        Diagnostic::error().with_message(format!("Couldn't load the input image: {err}"))
    })?;

    if image.nb_frames() != 1 {
        reporter.report(
            &Diagnostic::warning()
                .with_message("The input image has multiple animation frames")
                .with_notes(vec!["Only the first frame will be processed".into()]),
        )
    }
    let frame = image.frame(0);
    let slice = match options.input_slice {
        Some(slice) => slice,
        None => {
            if image.width() % 8 != 0 || image.height() % 8 != 0 {
                return Err(Diagnostic::error().with_message(format!(
                    "The input image's dimensions ({}x{}) must each be a multiple of 8 pixels",
                    image.width(),
                    image.height(),
                )).with_notes(vec![
                    "If you want to only process a portion of it, try using the `-L` option instead".into(),
                ]));
            }

            // Panicking if the input image exceeds 524288 pixels in either dimension seems reasonable.
            let dimension_in_tiles = |pixels: usize, overflow_msg| {
                NonZeroU16::new((pixels / 8).try_into().expect(overflow_msg))
            };
            let (Some(width), Some(height)) = (
                dimension_in_tiles(image.width(), "Image width too large!"),
                dimension_in_tiles(image.height(), "Image height too large!"),
            ) else {
                return Err(Diagnostic::error().with_message("The input image cannot be empty"));
            };

            InputSlice {
                left: 0,
                top: 0,
                width,
                height,
            }
        }
    };

    let (image_colors, has_transparency) =
        collect_image_colors(&image, &slice, &options, reporter)?;

    // This is done unconditionally because it validates the image (which we want to perform even
    // if no output is requested), and because it's necessary to generate any output (except an
    // un-dedup'd tilemap, but that's acceptable).
    let (color_sets, mut attrmap) = collect_color_sets(&frame, &slice, &options, has_transparency)?;

    let (mappings, palettes) = match pal_spec {
        Some(PalSpec::Embedded) => {
            let pal = image.palette().ok_or_else(|| {
                Diagnostic::error().with_message(
                    "`-c embedded` is being used, but the image lacks an embedded palette",
                )
            })?;
            make_palettes_as_specified(
                &[pal
                    .iter()
                    // Ignore any extraneous colors if they are unused.
                    // TODO: explicitly check that they are unused, to print a more friendly error if not
                    .take(options.colors_per_palette(has_transparency).into())
                    .copied()
                    .map(|color| todo!())
                    .collect()],
                &color_sets,
                has_transparency,
            )?
        }
        // TODO: this will generate duplicate colors... is that okay?
        Some(PalSpec::Explicit(spec)) => {
            make_palettes_as_specified(&spec, &color_sets, has_transparency)?
        }
        None => generate_palettes(&color_sets, &options, has_transparency),
    };

    // A lot of later operations depend on correct palette generation, so if the latter went wrong,
    // then they are likely to produce nonsensical results. So bail right now.
    if palettes.len() > options.nb_palettes.into() {
        return Err(Diagnostic::error()
            .with_message("Generated more palettes than the maximum")
            .with_notes(vec![format!(
                "Generated {} palettes, over the limit of {}",
                palettes.len(),
                options.nb_palettes
            )]));
    }

    if let Some(path) = &options.palettes_path {
        output_palettes(&palettes, path, &options)?;
    }

    if !options.allow_dedup {
        // All of the "optimised" outputs require the deduplication process to be performed to be output.
        // (Except for the palette map, I guess.)
        let tile_data = optimized::generate_unique_tiles(
            &frame,
            &mut attrmap,
            &palettes,
            &mappings,
            &slice,
            &options,
            has_transparency,
        );

        if let Some(path) = &options.output_path {
            optimized::output_tile_data(&tile_data, path, &options)?;
        }

        if let Some(path) = &options.tilemap_path {
            optimized::output_tilemap(&attrmap, path)?;
        }

        if let Some(path) = &options.attrmap_path {
            optimized::output_attrmap(&attrmap, &mappings, path)?;
        }

        if let Some(path) = &options.palmap_path {
            optimized::output_palmap(&attrmap, &mappings, path)?;
        }
    } else {
        if let Some([bank0, bank1]) = &options.max_nb_tiles {
            let height_in_tiles = (image.height() / 8) as u32;
            let width_in_tiles = (image.width() / 8) as u32;
            let nb_tiles = height_in_tiles * width_in_tiles;

            // Check the tile count.
            if nb_tiles > (bank0 + bank1).into() {
                let nb_tiles_msg = format!("The image contains {nb_tiles} (unoptimized) tiles");
                return Err(Diagnostic::error()
                    .with_message("The image contains more tiles than the limit")
                    .with_notes(vec![nb_tiles_msg]));
            }
        }

        if let Some(path) = &options.output_path {
            unoptimized::output_tile_data(
                &frame,
                &attrmap,
                &palettes,
                &mappings,
                path,
                &slice,
                &options,
                has_transparency,
            )?;
        }

        if options.tilemap_path.is_some()
            || options.attrmap_path.is_some()
            || options.palmap_path.is_some()
        {
            unoptimized::output_maps(&attrmap, &mappings, &options)?;
        }
    }

    Ok(())
}

pub(crate) fn process_palettes_only(
    pal_specs: &[Vec<Rgb>],
    path: &Path,
    options: &Options,
) -> Result<(), Diagnostic> {
    let (_, palettes) = make_palettes_as_specified(pal_specs, &[], false)?;

    output_palettes(&palettes, path, options)
}

/// Iterate through the image's pixels to check if any are transparent, and to build a table of
/// the image's colors.
fn collect_image_colors(
    image: &DynImage32,
    slice: &InputSlice,
    options: &Options,
    reporter: &mut Reporter,
) -> Result<(Vec<Rgb16>, bool), Diagnostic> {
    let mut has_transparency = false;
    let mut cgb_colors: [_; 0x8000] = std::array::from_fn(|_i| None);
    let mut ambiguous_alpha_pos = Vec::new();
    let mut ambiguous_alpha = HashSet::new();

    for y in 0..usize::from(slice.height.get()) * 8 {
        for x in 0..usize::from(slice.width.get()) * 8 {
            let rgba = Rgba::from(*image.pixel(0, x, y));
            match rgba.opacity() {
                None => {
                    ambiguous_alpha_pos.push((x, y));
                    ambiguous_alpha.insert(rgba);
                }
                Some(Opacity::Opaque) => {
                    let cgb_color = rgba.cgb_color(options.use_color_curve);
                    // The array is correctly sized for all opaque colors, so the index is in bounds.
                    let slot = &mut cgb_colors[usize::from(cgb_color.0)];
                    match slot {
                        None => *slot = Some((Rgb::from(rgba), Vec::new())),
                        Some((_first, conflicting)) => conflicting.push(Rgb::from(rgba)),
                    }
                }
                Some(Opacity::Transparent) => has_transparency = true,
            }
        }
    }

    let mut ambiguous_colors = ambiguous_alpha.into_iter();
    if let Some(first) = ambiguous_colors.next() {
        let offending_colors = if ambiguous_colors.len() == 0 {
            format!("Offending color: {first}")
        } else {
            // TODO: generate an image showing where the offending pixels are located.

            let mut msg = format!("Offending colors: {first}");
            while let Some(color) = ambiguous_colors.next() {
                use std::fmt::Write;

                msg.push_str(if ambiguous_colors.len() == 0 {
                    ", and "
                } else {
                    ", "
                });
                write!(msg, "{color}").unwrap();
            }
            msg
        };
        let acceptable_alpha = format!(
            "Acceptable alpha values are between 0 and #{:02x} ({}) for transparency, or between #{:02x} ({}) and #ff (255) for opaque pixels.",
            Rgba::TRANSPARENCY_THRESHOLD,
            Rgba::TRANSPARENCY_THRESHOLD,
            Rgba::OPACITY_THRESHOLD,
            Rgba::OPACITY_THRESHOLD,
        );
        return Err(Diagnostic::error()
            .with_message("Some colors have ambiguous transparency")
            .with_notes(vec![offending_colors, acceptable_alpha]));
    }

    let mut image_colors = match image.palette() {
        Some(pal) => Vec::with_capacity(pal.len()),
        None => Vec::with_capacity(
            usize::from(options.nb_colors_per_pal) * usize::from(options.nb_palettes),
        ),
    };
    for (cgb_color, slot) in cgb_colors.iter().enumerate() {
        let Some((first, conflicting)) = slot else {
            continue;
        };

        if !conflicting.is_empty() {
            reporter.report(
                &Diagnostic::warning()
                    .with_message(format!(
                        "Several colors in the image map to GBC color ${cgb_color:04x}"
                    ))
                    .with_notes(vec![format!(
                        "The colors are: {}",
                        ColorList(first, conflicting)
                    )]),
            );
        }

        image_colors.push(Rgb16(cgb_color as u16));
    }
    Ok((image_colors, has_transparency))
}

fn collect_color_sets(
    frame: &Frame<'_, Rgb32, DynImage32>,
    slice: &InputSlice,
    options: &Options,
    has_transparency: bool,
) -> Result<(Vec<ColorSet>, Vec<AttrmapEntry>), Diagnostic> {
    let colors_per_palette = options.colors_per_palette(has_transparency);

    let mut color_sets = Vec::new();
    let mut attrmap = Vec::with_capacity(frame.image().height() * frame.image().width());

    'tiles: for tile in slice.iter_tiles(frame) {
        attrmap.push(AttrmapEntry::default());
        let attrs = attrmap.last_mut().unwrap();

        // This is tracked separately from the set's len, because a tile may (erroneously) contain more than 4 colors.
        let mut nb_colors_in_tile = 0;
        let mut tile_colors = ColorSet::new();

        for y in 0..8 {
            for x in 0..8 {
                let color = Rgba::from(tile.pixel(x, y));
                // Do not count transparent colors for packing.
                if color.opacity() == Some(Opacity::Opaque) {
                    // Add the color to the set, and count it if it wasn't a duplicate.
                    if tile_colors.add(color.cgb_color(options.use_color_curve)) {
                        nb_colors_in_tile += 1;
                    }
                }
            }
        }

        // Empty color sets screw with the packing process, so discard them.
        if tile_colors.is_empty() {
            attrs.color_set_id = AttrmapEntry::TRANSPARENT;
            continue;
        }

        if nb_colors_in_tile > colors_per_palette {
            return Err(Diagnostic::error().with_message(format!(
                "The tile at (x: {}, y: {}) has more opaque colors than the maximum ({colors_per_palette})",
                tile.x, tile.y,
            )));
        }

        // Register the color set, avoiding overlap with existing ones.
        for (i, set) in color_sets.iter_mut().enumerate() {
            match tile_colors.partial_cmp(set) {
                // If neither set contains the other, keep seeking.
                None => {}
                // If this set is contained within an existing one, reuse the latter.
                Some(Ordering::Less | Ordering::Equal) => {
                    attrs.color_set_id = i;
                    continue 'tiles;
                }
                // If this set fully contains an existing one, "override" it.
                Some(Ordering::Greater) => {
                    *set = tile_colors;
                    attrs.color_set_id = i;
                    // TODO: could iterate the rest of the sets, to find if this set also fully
                    //       contains any others; then they could be removed.
                    //       But that would require updating all of the mappings, is that worth it?
                    continue 'tiles;
                }
            }
        }
        // The set needs to be added to the list.

        debug_assert_ne!(color_sets.len(), AttrmapEntry::TRANSPARENT); // Ensure IDs don't conflict.
        attrs.color_set_id = color_sets.len();
        color_sets.push(tile_colors);
    }

    Ok((color_sets, attrmap))
}

fn generate_palettes(
    color_sets: &[ColorSet],
    options: &Options,
    has_transparency: bool,
) -> (Vec<usize>, Vec<Palette>) {
    let mappings = crate::pal_packing::pack_palettes(color_sets, options, has_transparency);

    let nb_palettes = mappings.len();
    let mut palettes = vec![Palette::new(has_transparency); nb_palettes];

    // Generate the actual palettes from the mappings.
    debug_assert_eq!(mappings.len(), color_sets.len());
    for (&mapping, color_set) in std::iter::zip(&mappings, color_sets) {
        for &color in color_set.iter() {
            palettes[mapping].add_color(color);
        }
    }

    // Sort colors in the generated palettes.
    for palette in &mut palettes {
        fn luminance(color: &Rgb16) -> u16 {
            let (red, green, blue, _alpha) = color.decode();
            2126 * u16::from(red) + 7152 * u16::from(green) + 722 * u16::from(blue)
        }
        palette.colors.sort_unstable_by_key(luminance);
    }

    (mappings, palettes)
}

fn make_palettes_as_specified(
    pal_specs: &[Vec<Rgb>],
    color_sets: &[ColorSet],
    has_transparency: bool,
) -> Result<(Vec<usize>, Vec<Palette>), Diagnostic> {
    // Convert the palette spec to actual palettes.
    let mut palettes = Vec::from_iter(pal_specs.iter().map(|spec| {
        let mut palette = Palette::new(has_transparency);
        todo!();
        palette
    }));

    // Iterate through color sets, and try mapping them to the specified palettes.
    let mut misfits = Vec::new();
    let mappings = color_sets
        .iter()
        .map(|set| {
            match palettes.iter().position(|palette| {
                set.iter()
                    .all(|&color| palette.index_of(color, has_transparency).is_some())
            }) {
                Some(idx) => idx,
                None => {
                    misfits.push(set);
                    0 // Bogus value, will not really be used.
                }
            }
        })
        .collect();

    if misfits.is_empty() {
        Ok((mappings, palettes))
    } else {
        fn format_palettes(palettes: &[Palette]) -> String {
            use std::fmt::Write;

            const PREFIX: &str = "No palette contains colors ";
            let mut pals_str = String::with_capacity(palettes.len() * (PREFIX.len() + 4 * 7 + 5));
            let mut items = palettes.iter();
            writeln!(pals_str, "{PREFIX} {}", items.next().unwrap()).unwrap();
            pals_str
        }
        let mut notes = Vec::with_capacity(misfits.len() + 1);
        notes.push(format_palettes(&palettes));
        notes.extend(
            misfits
                .iter()
                .map(|set| format!("No palette contains colors {set}")),
        );
        Err(Diagnostic::error()
            .with_message("Some tiles cannot be displayed with the specified palettes")
            .with_notes(notes))
    }
}

fn output_palettes(palettes: &[Palette], path: &Path, options: &Options) -> Result<(), Diagnostic> {
    let mut output = File::create(path)
        .map_err(|err| file_error(format!("Failed to create palette file: {err}"), path))?;

    for palette in palettes {
        for i in 0..usize::from(options.nb_colors_per_pal) {
            let color = palette.colors.get(i).copied().unwrap_or(Rgba::TRANSPARENT);
            output
                .write_all(&color.0.to_le_bytes())
                .map_err(|err| file_error(format!("Failed to write palettes: {err}"), path))?;
        }
    }

    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
struct AttrmapEntry {
    color_set_id: usize,
    tile_id: u8,
    bank: bool,
    vert_flip: bool,
    horiz_flip: bool,
}

impl AttrmapEntry {
    /// For `color_set_id`.
    const TRANSPARENT: usize = usize::MAX;

    fn get_pal_id(&self, mappings: &[usize]) -> usize {
        match self.color_set_id {
            Self::TRANSPARENT => 0,
            id => mappings[id],
        }
    }
}

#[derive(Debug, Clone)]
struct TileData {
    bytes: [u8; 16],
    /// FIXME: blegh. This duplicate of [`Options::allow_mirroring`] is only necessary for [`Self::eq`] to be self-contained.
    allow_mirroring: bool,
}

impl TileData {
    fn row_bitplanes(
        tile: &Tile<'_, '_>,
        palette: &Palette,
        y: u8,
        options: &Options,
        has_transparency: bool,
    ) -> u16 {
        let mut row = 0;
        for x in 0..8 {
            row <<= 1;
            let index = palette
                .index_of(
                    Rgba::from(tile.pixel(x, y)).cgb_color(options.use_color_curve),
                    has_transparency,
                )
                .expect("The color should be in the palette?!?");
            if (index & 1) != 0 {
                row |= 1;
            }
            if (index & 2) != 0 {
                row |= 0x100;
            }
        }
        row
    }

    fn new(
        tile: &Tile<'_, '_>,
        palette: &Palette,
        options: &Options,
        has_transparency: bool,
    ) -> Self {
        let mut bytes = [0; 16];

        for (y, bitplanes) in bytes.chunks_exact_mut(2).enumerate() {
            let [bp0, bp1] = Self::row_bitplanes(tile, palette, y as u8, options, has_transparency)
                .to_le_bytes();
            bitplanes[0] = bp0;
            if options.bit_depth == 2 {
                bitplanes[1] = bp1;
            }
        }

        Self {
            bytes,
            allow_mirroring: options.allow_mirroring,
        }
    }

    fn try_matching(&self, other: &Self) -> Option<TileMatchKind> {
        // Check for strict equality first, as that can typically be optimized better by the compiler,
        // and it allows hoisting the `allow_mirroring` check out of any loop.
        if self.bytes == other.bytes {
            return Some(TileMatchKind {
                vflip: false,
                hflip: false,
            });
        } else if !self.allow_mirroring {
            return None;
        } else if std::iter::zip(&self.bytes, &other.bytes)
            .all(|(&our_bitplane, &their_bitplane)| our_bitplane == their_bitplane.reverse_bits())
        {
            return Some(TileMatchKind {
                vflip: false,
                hflip: true,
            });
        }

        let mut has_vflip = true;
        let mut has_vhflip = true;
        for (ours, theirs) in
            std::iter::zip(self.bytes.chunks_exact(2), other.bytes.rchunks_exact(2)).take(4)
        {
            let mut compare = |our_bitplane, their_bitplane: u8| {
                if our_bitplane != their_bitplane {
                    has_vflip = false;
                }
                if our_bitplane != their_bitplane.reverse_bits() {
                    has_vhflip = false;
                }
            };

            compare(ours[0], theirs[0]);
            compare(ours[1], theirs[1]);
        }

        // If we have both (i.e. the tile is horizontally symmetrical), default to vflip only.
        if has_vflip {
            Some(TileMatchKind {
                vflip: true,
                hflip: false,
            })
        } else if has_vhflip {
            Some(TileMatchKind {
                vflip: true,
                hflip: true,
            })
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct TileMatchKind {
    vflip: bool,
    hflip: bool,
}

impl PartialEq for TileData {
    fn eq(&self, other: &Self) -> bool {
        self.try_matching(other).is_some()
    }
}

impl Eq for TileData {}

/// This hash implementation is specifically tailored to return the same hash for mirrored tiles too.
/// It was contributed by aaaaaa123456789 / ax6; thank you very much!
///
/// The way that this is done, is by hashing each row individually, and adding the hashes;
/// since addition is commutative, this takes care of vertical mirroring.
/// On its own, this would induce collisions with other permutations of the same pixel rows;
/// to mitigate this, each row's hash is multiplied by a factor, with mirror-able rows using the same factor.
///
/// Horizontal mirroring is taken care of by giving any row the same hash as its horizontal mirror;
/// a trivial implementation of this is to simply use the largest of these two.
///
/// Finally, notice that each bitplane is hashed independently into a [`u8`],
/// which are combined into the final [`u16`] hash.
/// This performs better than trying to combine the bitplanes in any way, since the bitplanes are essentially independent.
impl std::hash::Hash for TileData {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        /// These factors are picked so that combinations of them (by addition) don't share common factors.
        /// Being mutually prime is a requirement, and being prime satisfies that with less work.
        /// (TODO: 37 + 23 being equal to 31 + 29 isn't ideal; consider picking other factors?)
        ///
        /// Ideally, we would like all combinations of the primes to have no common factors, but we also want them all to be odd.
        /// This is obviously possible, but the numbers would be extremely big.
        /// It's impossible for all combinations to be coprime, but we want the GCDs to be as small as possible,
        /// and the sums to have as few factors of 2 as possible.
        /// This is so that `hash([a, b, ...])` doesn't collide with `hash([b, a, ...])`,
        /// and `hash([a, a, ...])` doesn't guarantee zeroes at the end of the hash.
        ///
        /// In this case, since 23 + 37 = 29 + 31, (a, b, c, d) and (b, a, d, c) would have the same hash.
        const FACTORS: [u8; 8] = [23, 29, 31, 37, 37, 31, 29, 23];

        let mut low = 0;
        let mut high = 0;
        for (y, factor) in FACTORS.iter().enumerate() {
            let incorporate_row = |hash: &mut u8, bitplane: u8| {
                *hash = hash.wrapping_add(factor * std::cmp::max(bitplane, bitplane.reverse_bits()))
            };
            incorporate_row(&mut low, self.bytes[y * 2]);
            incorporate_row(&mut high, self.bytes[y * 2 + 1]);
        }
        state.write_u16(u16::from_le_bytes([low, high]));
    }
}

fn file_error<S: Into<String>, P: AsRef<Path>>(err_msg: S, path: P) -> Diagnostic {
    Diagnostic::error()
        .with_message(err_msg)
        .with_notes(vec![format!("File path: {}", path.as_ref().display())])
}

struct ColorList<'a>(&'a Rgb, &'a Vec<Rgb>);

impl Display for ColorList<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        debug_assert_ne!(self.1.len(), 0); // This guarantees that the "and" is preceded by a color.

        for color in self.1 {
            write!(f, "{color}, ")?;
        }
        write!(f, "and {}", self.0)
    }
}
