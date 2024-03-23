use clap::Parser;
use clap_num::maybe_hex;
use std::fs::{File, OpenOptions};
use std::io::{self, Read, Seek, Write};
use std::path::{Path, PathBuf};
use std::str::FromStr;
use tracing::*;

const BANK_SIZE: usize = 0x4000;

#[derive(Debug, thiserror::Error)]
enum Error {
    #[error("failed to read file: {0}")]
    Io(#[from] io::Error),
    #[error("file too short: expected at least {expected} bytes, got only {got}")]
    TooShort { expected: usize, got: usize },
    #[error("not a regular file; cannot be modified in-place")]
    IrregularFile,
    #[error("cannot have over 65535 banks ({0})")]
    TooManyBanks(u64),
}

#[derive(Parser, Debug)]
#[clap(version, about = "A tool to fix ROM files for Game Boy.")]
struct Cli {
    #[clap(flatten)]
    model: Model,

    #[clap(short = 'f', long, value_name = "FIX_SPEC", default_value = "")]
    /// Specify a fix specification
    fix_spec: FixSpec,

    #[clap(short = 'i', long, value_name = "GAME_ID")]
    /// Specify a game ID
    game_id: Option<String>,

    #[clap(short = 'j', long)]
    /// Non-Japanese mode
    non_japanese: bool,

    #[clap(short = 'k', long, value_name = "NEW_LICENSEE")]
    /// Specify a new licensee
    new_licensee: Option<String>,

    #[clap(short = 'l', long, value_name = "OLD_LICENSEE", value_parser=maybe_hex::<u8>)]
    /// Specify an old licensee
    old_licensee: Option<u8>,

    #[clap(short = 'm', long, value_name = "MBC_TYPE")]
    /// Specify the MBC type
    mbc_type: Option<MbcType>,

    #[clap(short = 'n', long, value_name = "ROM_VERSION", value_parser=maybe_hex::<u8>)]
    /// Specify the ROM version
    rom_version: Option<u8>,

    #[clap(short = 'O', long)]
    /// Overwrite the file
    overwrite: bool,

    #[clap(short = 'p', long, value_name = "PAD_VALUE", value_parser=maybe_hex::<u8>)]
    /// Specify the padding value
    pad_value: Option<u8>,

    #[clap(short = 'r', long, value_name = "RAM_SIZE", value_parser=maybe_hex::<u8>)]
    /// Specify the RAM size
    ram_size: Option<u8>,

    #[clap(short = 's', long)]
    /// SGB-compatible mode
    sgb_compatible: bool,

    #[clap(short = 't', long, value_name = "TITLE")]
    /// Specify the title
    title: Option<String>,

    #[clap(short = 'v', long)]
    /// Fix the header logo and both checksums (-f lhg)
    validate: bool,

    #[clap(value_name = "FILES")]
    /// Files to process
    files: Vec<PathBuf>,
}

#[derive(clap::Args, Debug, Clone)]
struct Model {
    #[clap(short = 'C', long = "color-only")]
    /// Color-only mode
    color_only: bool,

    #[clap(short = 'c', long = "color-compatible")]
    /// Color-compatible mode
    color_compatible: bool,
}

impl Model {
    fn both(&self) -> bool {
        self.color_compatible
    }

    fn cgb(&self) -> bool {
        self.color_only
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FixState {
    Trash,
    Fix,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct FixSpec {
    logo: Option<FixState>,
    header: Option<FixState>,
    global: Option<FixState>,
}

impl FixSpec {
    fn is_empty(&self) -> bool {
        self.logo.is_some() & self.header.is_some() & self.global.is_some()
    }
}

#[derive(Debug, Clone, Copy, thiserror::Error)]
enum FixSpecError {
    #[error("invalid character: {0}")]
    InvalidCharacter(char),
    #[error("more than one logo value ('l' or 'L') specified")]
    DuplicateLogo,
    #[error("more than one logo value ('h' or 'H') specified")]
    DuplicateHeader,
    #[error("more than one logo value ('g' or 'G') specified")]
    DuplicateGlobal,
}

impl FromStr for FixSpec {
    type Err = FixSpecError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut fix_spec = FixSpec {
            logo: None,
            header: None,
            global: None,
        };

        type Fs = FixState;
        type E = FixSpecError;

        // Check for duplicate values.
        let update = |field: &mut Option<Fs>, value, err| -> Result<(), E> {
            if field.is_none() {
                *field = Some(value);
                Ok(())
            } else {
                Err(err)
            }
        };

        for c in s.chars() {
            match c {
                'l' => update(&mut fix_spec.logo, Fs::Fix, E::DuplicateLogo),
                'L' => update(&mut fix_spec.logo, Fs::Trash, E::DuplicateLogo),
                'h' => update(&mut fix_spec.header, Fs::Fix, E::DuplicateHeader),
                'H' => update(&mut fix_spec.header, Fs::Trash, E::DuplicateHeader),
                'g' => update(&mut fix_spec.global, Fs::Fix, E::DuplicateGlobal),
                'G' => update(&mut fix_spec.global, Fs::Trash, E::DuplicateGlobal),
                _ => Err(E::InvalidCharacter(c)),
            }?
        }

        Ok(fix_spec)
    }
}

static NINTENDO_LOGO: [u8; 48] = [
    0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
    0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
    0xBB, 0xBB, 0x67, 0x9F, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
];

fn main() {
    tracing_subscriber::fmt()
        .event_format(tracing_subscriber::fmt::format().without_time().compact())
        .init();
    let mut cli = Cli::parse();

    // TODO: move this to clap.
    if let Some(game_id) = &mut cli.game_id {
        if game_id.len() > 4 {
            // Use the debug display here to add quotes and escaping.
            warn!("truncating game ID {game_id:?} to 4 chars");
            game_id.truncate(4);
        } else {
            info!("game ID: {game_id}");
        }
    }

    // TODO: move this to clap.
    if let Some(new_licensee) = &mut cli.new_licensee {
        let len = new_licensee.len() as u8;
        if len > 2 {
            warn!("truncating new licensee {new_licensee:?} to 2 chars",);
            new_licensee.truncate(2);
        } else {
            info!("new licensee: {new_licensee}");
        }
    }

    // TODO: move this to clap (validator).
    if matches!(cli.mbc_type, Some(MbcType::Rom(Some(_)))) {
        warn!("ROM+RAM / ROM+RAM+BATTERY are under-specified and poorly supported");
    }

    // TODO: move this to clap.
    if let Some(title) = &mut cli.title {
        let max_len = match (cli.game_id.is_some(), cli.model.both() | cli.model.cgb()) {
            (true, _) => 11,
            (false, true) => 15,
            _ => 16,
        };
        if title.len() > max_len {
            warn!("truncating title {title:?} to {max_len} chars",);
            title.truncate(max_len);
        }
    }

    if cli.validate {
        if cli.fix_spec.is_empty() {
            warn!("-v overwrites -f");
        }
        cli.fix_spec = FixSpec {
            logo: Some(FixState::Fix),
            header: Some(FixState::Fix),
            global: Some(FixState::Fix),
        };
    }

    for i in &cli.files {
        if let Err(msg) = process_filename(i, &cli) {
            error!("{}: {msg}", i.display());
        }
    }
}

/// For cartridges with optional RAM and an optional battery.
///
/// You cannot have a battery but no RAM.
#[derive(Clone, Debug, Copy, PartialEq, Eq)]
enum RamBattery {
    Ram,
    RamBattery,
}

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
enum Mbc3Opts {
    /// Timers always have batteries.
    Timer {
        ram: bool,
    },
    Ram {
        battery: bool,
    },
}

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
enum Mbc5Opts {
    Ram { battery: bool },
    Rumble(Option<RamBattery>),
}

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
enum MbcType {
    Rom(Option<RamBattery>),
    Mbc1(Option<RamBattery>),
    Mbc2 {
        battery: bool,
    },
    Mmm01(Option<RamBattery>),
    Mbc3(Option<Mbc3Opts>),
    Mbc5(Option<Mbc5Opts>),
    Mbc6,
    Mbc7,
    PocketCamera,
    BandaiTama5,
    Huc3,
    Huc1,
    Tpp1 {
        rumble: bool,
        multi_rumble: bool,
        timer: bool,
        battery: bool,
    },
}

macro_rules! define_codes {
    ($(($code:expr, $name:expr) => $value:tt),+) => {
        impl MbcType {
            fn to_code(self) -> u16 {
                type M = MbcType;
                type R = RamBattery;
                #[allow(unused_parens)]
                match self {
                    $($value => $code,)+
                }
            }

            fn from_code(mbc_type: u16) -> Result<Self, ParseError> {
                type M = MbcType;
                type R = RamBattery;
                #[allow(unused_parens)]
                match mbc_type {
                    $($code => Ok($value),)+
                    _ => Err(ParseError::Bad),
                }
            }

            #[allow(unused)]
            fn get_name(self) -> &'static str {
                type M = MbcType;
                type R = RamBattery;
                #[allow(unused_parens)]
                match self {
                    $($value => $name,)+
                }
            }

            fn from_name(name: &str) -> Result<Self, ParseError> {
                type M = MbcType;
                type R = RamBattery;
                #[allow(unused_parens)]
                match name {
                    $(stringify!($name) => Ok($value),)+
                    _ => Err(ParseError::Bad),
                }
            }
        }

        impl FromStr for MbcType {
            type Err = ParseError;

            fn from_str(s: &str) -> Result<Self, Self::Err> {
                if s.chars().next().unwrap().is_ascii_digit() || s.starts_with('$') {
                    let base = if s.starts_with('$') { 16 } else { 10 };
                    let mbc = u16::from_str_radix(s.trim_start_matches('$'), base)
                        .map_err(|_| ParseError::Bad)?;
                    if mbc > 0xFF {
                        return Err(ParseError::BadRange);
                    }
                    return MbcType::from_code(mbc);
                }
                else {
                    return MbcType::from_name(s);
                }
            }
        }
    };
}

define_codes! {
    (0x00, "ROM") => (M::Rom(None)),
    (0x08, "ROM+RAM") => (M::Rom(Some(R::Ram))),
    (0x09, "ROM+RAM+BATTERY") => (M::Rom(Some(R::RamBattery))),
    (0x01, "MBC1") => (M::Mbc1(None)),
    (0x02, "MBC1+RAM") => (M::Mbc1(Some(R::Ram))),
    (0x03, "MBC1+RAM+BATTERY") => (M::Mbc1(Some(R::RamBattery))),
    (0x05, "MBC2") => (M::Mbc2 { battery: false }),
    (0x06, "MBC2+BATTERY") => (M::Mbc2 { battery: true }),
    (0x0B, "MMM01") => (M::Mmm01(None)),
    (0x0C, "MMM01+RAM") => (M::Mmm01(Some(R::Ram))),
    (0x0D, "MMM01+RAM+BATTERY") => (M::Mmm01(Some(R::RamBattery))),
    (0x11, "MBC3") => (M::Mbc3(None)),
    (0x0F, "MBC3+TIMER+BATTERY") => (M::Mbc3(Some(Mbc3Opts::Timer { ram: false }))),
    (0x10, "MBC3+TIMER+RAM+BATTERY") => (M::Mbc3(Some(Mbc3Opts::Timer { ram: true }))),
    (0x12, "MBC3+RAM") => (M::Mbc3(Some(Mbc3Opts::Ram { battery: false }))),
    (0x13, "MBC3+RAM+BATTERY") => (M::Mbc3(Some(Mbc3Opts::Ram { battery: true }))),
    (0x19, "MBC5") => (M::Mbc5(None)),
    (0x1A, "MBC5+RAM") => (M::Mbc5(Some(Mbc5Opts::Ram { battery: false }))),
    (0x1B, "MBC5+RAM+BATTERY") => (M::Mbc5(Some(Mbc5Opts::Ram { battery: true }))),
    (0x1C, "MBC5+RUMBLE") => (M::Mbc5(Some(Mbc5Opts::Rumble(None)))),
    (0x1D, "MBC5+RUMBLE+RAM") => (M::Mbc5(Some(Mbc5Opts::Rumble(Some(R::Ram))))),
    (0x1E, "MBC5+RUMBLE+RAM+BATTERY") => (M::Mbc5(Some(Mbc5Opts::Rumble(Some(R::RamBattery))))),
    (0x20, "MBC6") => (M::Mbc6),
    (0x22, "MBC7+SENSOR+RUMBLE+RAM+BATTERY") => (M::Mbc7),
    (0xFC, "POCKET CAMERA") => (M::PocketCamera),
    (0xFD, "BANDAI TAMA5") => (M::BandaiTama5),
    (0xFE, "HUC3") => (M::Huc3),
    (0xFF, "HUC1+RAM+BATTERY") => (M::Huc1),
    (0x100, "TPP1") => (M::Tpp1 {
        rumble: false,
        multi_rumble: false,
        timer: false,
        battery: false,
    }),
    (0x101, "TPP1+RUMBLE") => (M::Tpp1 {
        rumble: true,
        multi_rumble: false,
        timer: false,
        battery: false,
    }),
    (0x102, "TPP1+MULTIRUMBLE") => (M::Tpp1 {
        rumble: false,
        multi_rumble: true,
        timer: false,
        battery: false,
    }),
    (0x103, "TPP1+MULTIRUMBLE") => (M::Tpp1 {
        rumble: true,
        multi_rumble: true,
        timer: false,
        battery: false,
    }),
    (0x104, "TPP1+TIMER") => (M::Tpp1 {
        rumble: false,
        multi_rumble: false,
        timer: true,
        battery: false,
    }),
    (0x105, "TPP1+TIMER+RUMBLE") => (M::Tpp1 {
        rumble: true,
        multi_rumble: false,
        timer: true,
        battery: false,
    }),
    (0x106, "TPP1+TIMER+MULTIRUMBLE") => (M::Tpp1 {
        rumble: false,
        multi_rumble: true,
        timer: true,
        battery: false,
    }),
    (0x107, "TPP1+TIMER+MULTIRUMBLE") => (M::Tpp1 {
        rumble: true,
        multi_rumble: true,
        timer: true,
        battery: false,
    }),
    (0x108, "TPP1+BATTERY") => (M::Tpp1 {
        rumble: false,
        multi_rumble: false,
        timer: false,
        battery: true,
    }),
    (0x109, "TPP1+BATTERY+RUMBLE") => (M::Tpp1 {
        rumble: true,
        multi_rumble: false,
        timer: false,
        battery: true,
    }),
    (0x10A, "TPP1+BATTERY+MULTIRUMBLE") => (M::Tpp1 {
        rumble: false,
        multi_rumble: true,
        timer: false,
        battery: true,
    }),
    (0x10B, "TPP1+BATTERY+MULTIRUMBLE") => (M::Tpp1 {
        rumble: true,
        multi_rumble: true,
        timer: false,
        battery: true,
    }),
    (0x10C, "TPP1+BATTERY+TIMER") => (M::Tpp1 {
        rumble: false,
        multi_rumble: false,
        timer: true,
        battery: true,
    }),
    (0x10D, "TPP1+BATTERY+TIMER+RUMBLE") => (M::Tpp1 {
        rumble: true,
        multi_rumble: false,
        timer: true,
        battery: true,
    }),
    (0x10E, "TPP1+BATTERY+TIMER+MULTIRUMBLE") => (M::Tpp1 {
        rumble: false,
        multi_rumble: true,
        timer: true,
        battery: true,
    }),
    (0x10F, "TPP1+BATTERY+TIMER+MULTIRUMBLE") => (M::Tpp1 {
        rumble: true,
        multi_rumble: true,
        timer: true,
        battery: true,
    })
}

impl MbcType {
    #[allow(unused)]
    fn has_ram(self) -> Option<bool> {
        match self {
            MbcType::Rom(None)
            | MbcType::Mbc1(None)
            | MbcType::Mmm01(None)
            | MbcType::Mbc3(None)
            | MbcType::Mbc2 { .. }
            | MbcType::Mbc3(Some(Mbc3Opts::Timer { ram: false }))
            | MbcType::Mbc5(None)
            | MbcType::Mbc5(Some(Mbc5Opts::Rumble(None)))
            | MbcType::Mbc6
            | MbcType::BandaiTama5 => Some(false),

            MbcType::Rom(Some(_))
            | MbcType::Mbc1(Some(_))
            | MbcType::Mmm01(Some(_))
            | MbcType::Mbc3(Some(Mbc3Opts::Timer { ram: true }))
            | MbcType::Mbc3(Some(Mbc3Opts::Ram { .. }))
            | MbcType::Mbc5(Some(Mbc5Opts::Ram { .. }))
            | MbcType::Mbc5(Some(Mbc5Opts::Rumble(Some(_))))
            | MbcType::Mbc7
            | MbcType::PocketCamera
            | MbcType::Huc3
            | MbcType::Huc1 => Some(true),

            MbcType::Tpp1 { .. } => None,
        }
    }
}

#[derive(Debug, thiserror::Error, Clone, Copy)]
enum ParseError {
    #[error("unknown format")]
    Bad,
    #[error("outside of accepted range")]
    BadRange,
}

fn read_bytes<R: Read>(fd: &mut R, mut buf: &mut [u8]) -> io::Result<usize> {
    let mut total = 0;
    while !buf.is_empty() {
        match fd.read(buf) {
            Ok(0) => break, // EOF
            Ok(n) => {
                total += n;
                let tmp = buf;
                buf = &mut tmp[n..];
            }
            Err(e) if e.kind() == io::ErrorKind::Interrupted => continue,
            Err(e) => return Err(e),
        }
    }
    Ok(total)
}

fn overwrite_byte(rom0: &mut [u8], addr: u16, fixed_byte: u8, area: &str, overwrite: bool) {
    let orig_byte = rom0[addr as usize];

    if !overwrite && orig_byte != 0 && orig_byte != fixed_byte {
        warn!("overwrote a non-zero byte in the {area}");
    }

    rom0[addr as usize] = fixed_byte;
}

fn overwrite_prelude(
    rom0: &mut [u8],
    start_addr: usize,
    fixed: &[u8],
    area: &str,
    overwrite: bool,
) {
    if start_addr + fixed.len() >= rom0.len() {
        panic!("attempted to overwrite a byte outside of ROM0");
    }

    if !overwrite {
        for (i, &byte) in fixed.iter().enumerate() {
            if rom0[i + start_addr] != 0 && rom0[i + start_addr] != byte {
                warn!("overwrote a non-zero byte in the {area}");
                break;
            }
        }
    }
}

fn overwrite_bytes(rom0: &mut [u8], start_addr: u16, fixed: &[u8], area: &str, overwrite: bool) {
    let start_addr = start_addr as usize;
    overwrite_prelude(rom0, start_addr, fixed, area, overwrite);
    rom0[start_addr..start_addr + fixed.len()].copy_from_slice(fixed);
}

fn overwrite_inverted_bytes(
    rom0: &mut [u8],
    start_addr: u16,
    fixed: &[u8],
    area: &str,
    overwrite: bool,
) {
    let start_addr = start_addr as usize;
    overwrite_prelude(rom0, start_addr, fixed, area, overwrite);

    for (dest, source) in rom0[start_addr..start_addr + fixed.len()]
        .iter_mut()
        .zip(fixed)
    {
        *dest = *source ^ 0xFF
    }
}

fn process_file(
    input: &mut File,
    output: &mut File,
    file_size: u64,
    options: &Cli,
) -> Result<(), Error> {
    let mut rom0 = [0; BANK_SIZE];
    let mut rom0_len = read_bytes(input, &mut rom0)?;

    let header_size = if matches!(&options.mbc_type, Some(MbcType::Tpp1 { .. })) {
        0x154
    } else {
        0x150
    };

    if rom0_len < header_size {
        return Err(Error::TooShort {
            expected: header_size,
            got: rom0_len,
        });
    }

    match options.fix_spec.logo {
        Some(FixState::Fix) => overwrite_bytes(
            &mut rom0,
            0x0104,
            &NINTENDO_LOGO,
            "Nintendo logo",
            options.overwrite,
        ),
        Some(FixState::Trash) => overwrite_inverted_bytes(
            &mut rom0,
            0x0104,
            &NINTENDO_LOGO,
            "Nintendo logo",
            options.overwrite,
        ),
        None => (),
    }

    if let Some(title) = &options.title {
        overwrite_bytes(
            &mut rom0[..],
            0x134,
            title.as_bytes(),
            "title",
            options.overwrite,
        );
    }

    if let Some(game_id) = &options.game_id {
        overwrite_bytes(
            &mut rom0[..],
            0x13F,
            game_id.as_bytes(),
            "manufacturer code",
            options.overwrite,
        );
    }

    if options.model.both() {
        overwrite_byte(&mut rom0, 0x143, 0x80, "Cgb flag", options.overwrite);
    } else if options.model.cgb() {
        overwrite_byte(&mut rom0, 0x143, 0xC0, "Cgb flag", options.overwrite);
    }

    if let Some(new_licensee) = &options.new_licensee {
        overwrite_bytes(
            &mut rom0[..],
            0x144,
            new_licensee.as_bytes(),
            "new licensee code",
            options.overwrite,
        );
    }

    if options.sgb_compatible {
        overwrite_byte(&mut rom0[..], 0x146, 0x03, "SGB flag", options.overwrite);
    }

    let ram_size = options.ram_size;

    if let Some(cartridge_type) = options.mbc_type {
        let byte = cartridge_type.to_code().try_into().unwrap_or(
            // Out-of-range cartridge bytes aren't directly actionable, translate them.
            // The other TPP1 identification bytes will be written below
            0xBC,
        );

        overwrite_byte(
            &mut rom0[..],
            0x147,
            byte,
            "cartridge type",
            options.overwrite,
        );
    }

    // ROM size will be written last, after evaluating the file's size

    if let Some(cartridge_type @ MbcType::Tpp1 { .. }) = options.mbc_type {
        let tpp1_code = vec![0xC1, 0x65];
        let tpp1_rev = vec![0x1, 0x0];

        overwrite_bytes(
            &mut rom0[..],
            0x149,
            &tpp1_code,
            "TPP1 identification code",
            options.overwrite,
        );

        overwrite_bytes(
            &mut rom0[..],
            0x150,
            &tpp1_rev,
            "TPP1 revision number",
            options.overwrite,
        );

        if let Some(ram_size) = ram_size {
            overwrite_byte(
                &mut rom0[..],
                0x152,
                ram_size,
                "RAM size",
                options.overwrite,
            );
        }

        overwrite_byte(
            &mut rom0[..],
            0x153,
            // This truncates and that's fine.
            cartridge_type.to_code() as u8,
            "TPP1 feature flags",
            options.overwrite,
        );
    } else {
        // Regular mappers

        if let Some(ram_size) = ram_size {
            overwrite_byte(
                &mut rom0[..],
                0x149,
                ram_size,
                "RAM size",
                options.overwrite,
            );
        }

        if options.non_japanese {
            overwrite_byte(
                &mut rom0[..],
                0x14A,
                0x01,
                "destination code",
                options.overwrite,
            );
        }
    }

    if let Some(old_licensee) = options.old_licensee {
        overwrite_byte(
            &mut rom0[..],
            0x14B,
            old_licensee,
            "old licensee code",
            options.overwrite,
        );
    } else if options.sgb_compatible && rom0[0x14B] != 0x33 {
        warn!(
            "SGB compatibility enabled, but old licensee was 0x{:02x}, not 0x33",
            rom0[0x14B]
        );
    }

    if let Some(rom_version) = options.rom_version {
        overwrite_byte(
            &mut rom0[..],
            0x14C,
            rom_version,
            "mask ROM version number",
            options.overwrite,
        );
    }

    let romx = Vec::new(); // Buffer of ROMX bank data
    let mut nb_banks; // Number of banks *targeted*, including ROM0
    let total_romx_len; // *Actual* size of ROMX data
    let mut bank = [0; BANK_SIZE]; // Temp buffer used to store a whole bank's worth of data
    let mut global_sum = 0; // Global checksum variable

    // Handle ROMX
    /* input.as_raw_fd() == output.as_raw_fd() */
    {
        if file_size >= (0x10000 * BANK_SIZE) as u64 {
            return Err(Error::TooManyBanks(file_size / BANK_SIZE as u64));
        }
        // Compute number of banks and ROMX len from file size
        nb_banks = ((file_size + (BANK_SIZE - 1) as u64) / BANK_SIZE as u64) as u32;
        total_romx_len = if file_size >= BANK_SIZE as u64 {
            (file_size - BANK_SIZE as u64) as usize
        } else {
            0
        };
    } 

    if let Some(pad_value) = options.pad_value {
        // We want at least 2 banks
        if nb_banks == 1 {
            if rom0_len != rom0.len() {
                // Fill the remaining space in rom0 with pad_value
                for i in rom0.iter_mut().skip(rom0_len) {
                    *i = pad_value;
                }
                // Update rom0Len to reflect the full size of rom0
                rom0_len = rom0.len();
            }
            nb_banks = 2;
        } else {
            assert_eq!(rom0_len, rom0.len(), "rom0Len must equal the size of rom0");
        }
        assert!(nb_banks >= 2, "Number of banks must be at least 2");
        // Alter number of banks to reflect required value
        // x&(x-1) is zero iff x is a power of 2, or 0; we know for sure it's non-zero,
        // so this is true (non-zero) when we don't have a power of 2
        if !nb_banks.is_power_of_two() {
            nb_banks = nb_banks.next_power_of_two();
        }
        // Write final ROM size
        rom0[0x148] = (nb_banks / 2).trailing_zeros() as u8;
        // Alter global checksum based on how many bytes will be added (not counting ROM0)
        global_sum += pad_value.wrapping_mul(((nb_banks - 1) * BANK_SIZE as u32 - total_romx_len as u32) as u8) as u16;
    }

    // Handle the header checksum after the ROM size has been written
    if options.fix_spec.header.is_some() {
        let sum = rom0[0x134..0x14D]
            .iter()
            .fold(0u8, |a, i| a.wrapping_sub(i + 1));
        overwrite_byte(
            &mut rom0,
            0x14D,
            if matches!(options.fix_spec.header, Some(FixState::Trash)) {
                !sum
            } else {
                sum
            },
            "header checksum",
            options.overwrite,
        );
    }

    if options.fix_spec.global.is_some() {
        assert!(rom0_len >= 0x14E, "ROM0 length must be at least 0x14E");
        for i in rom0[0..0x14E].iter().chain(rom0[0x150..rom0_len].iter()) {
            global_sum = global_sum.wrapping_add(*i as u16);
        }
        // Pipes have already read ROMX and updated global_sum, but not regular files
        /* if true || input.as_raw_fd() == output.as_raw_fd() */
        {
            loop {
                let bank_len = read_bytes(input, &mut bank)?;

                for i in bank.into_iter().take(bank_len) {
                    global_sum = global_sum.wrapping_add(i as u16);
                }
                if bank_len != BANK_SIZE {
                    break;
                }
            }
        }

        if matches!(options.fix_spec.global, Some(FixState::Trash)) {
            global_sum = !global_sum;
        }

        let bytes = global_sum.to_be_bytes();

        overwrite_bytes(
            &mut rom0,
            0x14E,
            &bytes,
            "global checksum",
            options.overwrite,
        );
    }

    // In case the output depends on the input, reset to the beginning of the file, and only
    // write the header
    /* if true || input.as_raw_fd() == output.as_raw_fd() */
    {
        output.seek(io::SeekFrom::Start(0))?;
        // If modifying the file in-place, we only need to edit the header
        // However, padding may have modified ROM0 (added padding), so don't in that case
        if options.pad_value.is_some() {
            rom0_len = header_size;
        }
    }

    output.write_all(&rom0[..rom0_len])?;

    // Output ROMX if it was buffered
    if !romx.is_empty() {
        output.write_all(&romx[..total_romx_len])?;
    }

    // Output padding
    if options.pad_value.is_some() {
        /* if true || input.as_raw_fd() == output.as_raw_fd() */
        {
            output.seek(io::SeekFrom::End(0))?;
        }
        bank.fill(options.pad_value.unwrap());
        let mut len = (nb_banks - 1) * BANK_SIZE as u32 - total_romx_len as u32; // Don't count ROM0!

        while len > 0 {
            let this_len = if len > BANK_SIZE as u32 {
                BANK_SIZE
            } else {
                len as usize
            };
            output.write_all(&bank[..this_len])?;
            len -= this_len as u32;
        }
    }

    Ok(())
}

fn process_filename(path: impl AsRef<Path>, options: &Cli) -> Result<(), Error> {
    let path = path.as_ref();

    let mut file = OpenOptions::new().read(true).write(true).open(path)?;
    let mut temp_file = file.try_clone()?; //TOCHECK: This could be plain wrong, check what output file actually does in process_File
    if path == Path::new("-") {
        process_file(&mut file, &mut temp_file, 0, options)?;
    } else {
        let metadata = file.metadata()?;
        if !metadata.is_file() {
            return Err(Error::IrregularFile);
        }
        if metadata.len() < 0x150 {
            return Err(Error::TooShort {
                expected: 336,
                // This cast is safe because anything under 336 will always fit in `usize`.
                got: metadata.len() as usize,
            });
        }
        process_file(&mut file, &mut temp_file, metadata.len(), options)?;
    }

    Ok(())
}
