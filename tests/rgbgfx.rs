use std::{fmt::Display, fs::DirEntry, ops::Range, path::PathBuf, process::ExitCode};

use libtest_mimic::{Arguments, Failed, Trial};
use plumers::{image::ImageFormat, prelude::*};
use snapbox::{cmd::Command, data::DataFormat, path::PathFixture, Data};

const RGBGFX_PATH: &str = env!("CARGO_BIN_EXE_rgbgfx");
const TESTS_DIR: &str = "tests/rgbgfx";

fn main() -> ExitCode {
    let arguments = Arguments::from_args();

    let mut trials = Vec::new();
    for res in std::fs::read_dir(TESTS_DIR)
        .unwrap_or_else(|err| panic!("Failed to walk {TESTS_DIR}: {err}"))
    {
        let entry = res.unwrap_or_else(|err| panic!("Error walking {TESTS_DIR}: {err}"));
        make_test(entry, &mut trials)
    }
    libtest_mimic::run(&arguments, trials).exit_code()
}

fn make_test(input_entry: DirEntry, trials: &mut Vec<Trial>) {
    let input_path = input_entry.path();

    let Some(file_name) = input_path.file_name() else {
        return; // Ignore any directories.
    };
    let name = file_name
        .to_str()
        .expect("Test cases should have UTF-8 names")
        .to_owned();

    let Some(input_ext) = input_path.extension() else {
        return; // Ignore any files without an extension.
    };
    if input_ext == "png" {
        let stdin_name = format!("{name} via stdin");
        let clone_input_path = input_path.clone();
        trials.push(Trial::test(name, || test_png(clone_input_path, false)));
        trials.push(Trial::test(stdin_name, || test_png(input_path, true)));
    } else if input_ext == "bin" {
        trials.push(Trial::test(name, || randtilegen(input_path, &[])));
    } else {
        // Other kinds of files don't generate any tests.
    }
}

// ~=====- Png bulk testing -=====~

fn test_png(input_path: PathBuf, use_stdin: bool) -> Result<(), Failed> {
    let mut cmd = Command::new(RGBGFX_PATH);

    let flags_file_path = input_path.with_extension("flags");
    if flags_file_path.exists() {
        cmd = cmd.arg(format!("@{}", flags_file_path.display()));
    }

    let assert = if use_stdin {
        cmd.arg("-")
            .stdin(Data::read_from(&input_path, Some(DataFormat::Binary)))
    } else {
        cmd.arg(&input_path).stdin([].as_slice())
    }
    .assert();

    // Note that these checks panic instead of returning `Err()`. Ah well.
    let err_file_path = input_path.with_extension("err");
    if err_file_path.exists() {
        assert
            .failure()
            .stderr_matches(Data::read_from(&err_file_path, Some(DataFormat::Text)))
    } else {
        assert.success()
    }
    // Additionally, rgbgfx should not output anything on stdout.
    .stdout_eq([].as_slice());

    Ok(())
}

// ~=====- Pseudo-randomly generated image testing -=====~

/*
 * Originally:
 * // This program is hereby released to the public domain.
 * // ~aaaaaa123456789, released 2022-03-15
 *
 * https://gist.github.com/aaaaaa123456789/3feccf085ab4f82d144d9a47fb1b4bdf
 */

fn randtilegen(input_path: PathBuf, rgbgfx_args: &[&str]) -> Result<(), Failed> {
    use std::process::Command;

    let image = make_random_image(
        std::fs::read(&input_path)
            .map_err(|err| format!("Failed to read {}: {err}", input_path.display()))?,
    );
    let temp_dir =
        PathFixture::mutable_temp().map_err(|err| format!("Failed to create temp dir: {err}"))?;
    let temp_dir_path = temp_dir.path().expect("Cannot name temp dir!?");
    let rand_img_path = temp_dir_path.join("rand.png");

    image.store(rand_img_path.as_path()).map_err(|err| {
        format!(
            "Failed to write random image to {}: {err}",
            rand_img_path.display()
        )
    })?;

    const OUTPUTS: [(&str, &str); 3] = [
        ("-o", "result.2bpp"),
        ("-p", "result.pal"),
        ("-a", "result.attrmap"),
    ];

    let run_and_verify = |specific_args: &[_], name| {
        let mut cmd = Command::new(RGBGFX_PATH);
        cmd.args(rgbgfx_args).args(specific_args);
        for (flag, file_name) in &OUTPUTS {
            cmd.arg(flag).arg(temp_dir_path.join(file_name));
        }

        let output = cmd
            .output()
            .map_err(|err| format!("Failed to run the {name} command: {err}"))?;
        if !output.stderr.is_empty() {
            return Err(format!(
                "The {name} command returned error messages:\n```\n{}\n```",
                String::from_utf8_lossy(&output.stderr)
            ));
        }
        if !output.status.success() {
            return Err(format!(
                "The {name} command failed with status {}",
                output.status
            ));
        }
        if !output.stdout.is_empty() {
            return Err(format!(
                "The {name} command wrote to stdout!\n```\n{}\n```",
                String::from_utf8_lossy(&output.stdout)
            ));
        }
        Ok(())
    };
    run_and_verify(&[rand_img_path.as_os_str()], "conversion")?;
    let roundtripped_img_path = temp_dir_path.join("roundtripped.png");
    run_and_verify(
        &[
            "-r".as_ref(),
            (image.width() / 8).to_string().as_ref(),
            roundtripped_img_path.as_os_str(),
        ],
        "reverse",
    )?;

    let roundtripped = DirectImage16::load(
        roundtripped_img_path.as_path(),
        LoadFlags {
            remove_alpha: false,
            palette_sort: Default::default(),
            sort_existing: false,
            reduce_palette: false,
        },
        AlphaMode::ZeroIsTransparent,
    )
    .map_err(|err| {
        format!(
            "Failed to read roundtripped image from {}: {err}",
            roundtripped_img_path.display()
        )
    })?;

    // Compare the two images.
    debug_assert_eq!(image.nb_frames(), 1);
    debug_assert_eq!(roundtripped.nb_frames(), 1);
    if roundtripped.width() != image.width() {
        return Err(format!(
            "Image widths do not match!
Expected {}
     Got {}",
            image.width(),
            roundtripped.width()
        )
        .into());
    }
    if roundtripped.height() != image.height() {
        return Err(format!(
            "Image heights do not match!
Expected {}
     Got {}",
            image.height(),
            roundtripped.height()
        )
        .into());
    }
    let mut mismatches = Vec::new();
    for y in 0..image.height() {
        for x in 0..image.width() {
            let is_transparent = |color: Rgb16| color.0 & 0x8000 == 0;
            let mut expected = image.pixel(0, x, y);
            expected.invert_alpha(); // randtilegen has inverted alpha.
            let got = roundtripped.pixel(0, x, y);
            // If both colors are transparent, ignore the RGB components.
            // Otherwise (one of the two is not transparent), the colors must match exactly.
            if (!is_transparent(expected) || !is_transparent(got)) && got != expected {
                mismatches.push(ColorMismatch {
                    x,
                    y,
                    expected,
                    got,
                });
            }
        }
    }
    ColorMismatches::new(mismatches)?;

    Ok(())
}

fn make_random_image(randomness: Vec<u8>) -> DirectImage16 {
    let mut rng = RandBits::new(randomness);

    const MIN_TILES_PER_SIDE: usize = 3;
    const MAX_TILES: usize = (MIN_TILES_PER_SIDE + 7) * (MIN_TILES_PER_SIDE + 7);
    let mut attributes = [Attributes::default(); MAX_TILES];
    let mut tile_data = [TileData::default(); MAX_TILES];
    let width = rng.get_bits(3) as usize + MIN_TILES_PER_SIDE;
    let height = rng.get_bits(3) as usize + MIN_TILES_PER_SIDE;
    for tile_id in 0..width * height {
        attributes[tile_id] = rng.gen_tile_attributes();
        let attribute = &attributes[tile_id];
        // If a tile contains only one color, then there's no tile data to generate: all pixels will use color 0.
        if attribute.nb_colors < 2 {
            // The tile data is already all-zeros.
            continue;
        }

        // Find tiles with the same number of colors.
        let is_similar = |attr: &Attributes| attr.nb_colors == attribute.nb_colors;
        let copy_me_maybe = if attributes[..tile_id].iter().any(is_similar) {
            // If there are any such tiles, there's a random chance that this tile
            // will replicate one of them (potentially rotated).
            // (This avoids consuming 8 bits of randomness if failure is inevitable.)
            let index = rng.get_bits(8) as usize;
            attributes
                .iter()
                .enumerate()
                .filter(|(_i, attr)| is_similar(attr))
                .nth(index)
        } else {
            None
        };

        tile_data[tile_id] = match copy_me_maybe {
            Some((index, _attr)) => rng.copy_tile_data(&tile_data[index]),
            None => rng.gen_tile_data(attribute.nb_colors),
        };
    }

    let palettes = rng.gen_palettes();

    // SAFETY: the entire image is initialized, just not sequentially.
    unsafe {
        DirectImage16::new(
            ImageFormat::Png,
            AlphaMode::ZeroIsOpaque,
            1,
            width * 8,
            height * 8,
            |pixels| {
                for tile_id in 0..width * height {
                    let tile = &tile_data[tile_id];
                    let tile_x = (tile_id % width) * 8;
                    let tile_y = (tile_id / width) * 8;
                    let palette = &palettes[usize::from(attributes[tile_id].palette)];

                    for y in 0..8 {
                        for x in 0..8 {
                            pixels[(tile_y + y) * width * 8 + tile_x + x]
                                .write(Rgb16(palette[usize::from(tile[y][x])]));
                        }
                    }
                }
            },
        )
    }
}

type TileData = [[u8; 8]; 8];

#[derive(Debug, Clone)]
struct RandBits {
    random: Vec<u8>,
    ofs: usize,
    buffer: u32,
    nb_bits: u8,
}

impl RandBits {
    fn new(random: Vec<u8>) -> Self {
        Self {
            random,
            ofs: 0,
            buffer: 0,
            nb_bits: 0,
        }
    }

    fn get_bits(&mut self, nb_bits: u8) -> u32 {
        while nb_bits > self.nb_bits {
            // Get new random bytes from the file (assumed to be a stream of random data) to fulfill the request.
            self.buffer |= u32::from(self.random[self.ofs]) << self.nb_bits;
            self.nb_bits += 8;
            self.ofs += 1; // Consume the byte.
        }

        let result = self.buffer % (1 << nb_bits);
        self.buffer >>= nb_bits;
        self.nb_bits -= nb_bits;
        result
    }

    fn get_bits_resample(&mut self, range: Range<u32>) -> u32 {
        // "How many bits does this number span".
        let nb_bits = (usize::BITS - (range.len() - 1).leading_zeros()) as u8;
        loop {
            let value = self.get_bits(nb_bits);
            if range.contains(&value) {
                break value;
            }
        }
    }

    fn gen_tile_attributes(&mut self) -> Attributes {
        /*
         * Images have ten colors, grouped into two groups of 5 colors. The palette index indicates two
         * things: which one of those groups will be used, and which colors out of those 5 will be used
         * by the tile. The low bit indicates the group, and the rest of the value indicates the subset
         * of colors. The remainder of the number is treated as a bitfield, where each bit represents a
         * color: for instance, a value of 13 in the upper bits (binary 01101) indicates that colors 0,
         * 2 and 3 from that group will be used. Values of 0 and 31 are naturally invalid because they
         * indicate zero and five colors respectively, and 30 is also excluded to ensure that the
         * particular subset of colors 1, 2, 3 and 4 never shows up. This guarantees that every tile
         * will be representable using a palette containing color 0 (since those that don't contain
         * color 0 will have three colors at most), which in turn ensures that only 4 palettes per group
         * (and thus 8 total) are needed to cover the image: 0, 1, 2, 3; 0, 1, 2, 4; 0, 1, 3, 4; and 0,
         * 2, 3, 4. This also implies that making color 0 transparent (in both groups) adds a
         * transparent color to every palette.
         */
        let pal = self.get_bits_resample(1..30) as u8;

        Attributes {
            palette: pal * 2 + self.get_bits(1) as u8,
            nb_colors: pal.count_ones() as u8,
        }
    }

    fn gen_tile_data(&mut self, nb_colors: u8) -> TileData {
        std::array::from_fn(|_y| {
            std::array::from_fn(|_x| match nb_colors {
                2 => self.get_bits(1) as u8,
                3 => self.get_bits_resample(0..2) as u8,
                4 => self.get_bits(2) as u8,
                _ => unreachable!(),
            })
        })
    }

    fn copy_tile_data(&mut self, original: &TileData) -> TileData {
        // Apply a random rotation to the copy:
        // coord ^ 7 = inverted coordinate; coord ^ 0 = regular coordinate.
        let x_mask = self.get_bits(1) as usize * 7;
        let y_mask = self.get_bits(1) as usize * 7;
        std::array::from_fn(|y| std::array::from_fn(|x| original[y ^ y_mask][x ^ x_mask]))
    }

    fn gen_palettes(&mut self) -> [[u16; 4]; 60] {
        let mut colors: [[_; 5]; 2] = std::array::from_fn(|_group_id| {
            std::array::from_fn(|_color_id| self.get_bits(15) as _)
        });
        // Potentially make the first color of each group transparent.
        if self.get_bits(2) == 0 {
            colors[0][0] |= 0x8000;
            colors[1][0] |= 0x8000;
        }

        std::array::from_fn(|palette_id| {
            let mut palette = [0; 4];
            let mut i = 0;
            for (index, color) in colors[palette_id & 1].iter().copied().enumerate() {
                if palette_id & (2 << index) != 0 {
                    palette[i] = color;
                    i += 1;
                }
            }
            palette
        })
    }
}

#[derive(Debug, Clone, Copy, Default)]
struct Attributes {
    palette: u8,
    nb_colors: u8,
}

#[derive(Debug, Clone)]
struct ColorMismatches(Vec<ColorMismatch>);

impl ColorMismatches {
    fn new(mismatches: Vec<ColorMismatch>) -> Result<(), Self> {
        if mismatches.is_empty() {
            Ok(())
        } else {
            Err(Self(mismatches))
        }
    }
}

#[derive(Debug, Clone)]
struct ColorMismatch {
    x: usize,
    y: usize,
    expected: Rgb16,
    got: Rgb16,
}

impl Display for ColorMismatches {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "Some colors mismatched after round-tripping!
 X  |  Y  |  Expected |   Got
 ---+-----+-----------+----------"
        )?;
        let mut mismatches = self.0.iter();
        for ColorMismatch {
            x,
            y,
            expected,
            got,
        } in mismatches.by_ref().take(30)
        {
            writeln!(f, "{x:>3} | {y:>3} | {expected} | {got}")?;
        }
        let nb_remaining = mismatches.count();
        if nb_remaining != 0 {
            writeln!(f, "... and {nb_remaining} more")?;
        }
        Ok(())
    }
}
