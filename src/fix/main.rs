//TODO restore cargo.toml bin before shipping

use std::str::FromStr;
use clap::Parser;
use std::io::{self, Read, Write, Seek};
use std::os::unix::io::AsRawFd;
use std::fs::{File, OpenOptions};
use clap_num::maybe_hex;
use std::process;

const BANK_SIZE: usize = 0x4000;

#[derive(Debug, Clone)]
pub struct CliOptions {
    game_id: Option<String>,
    japanese: bool,
    new_licensee: Option<String>,
    old_licensee: Option<u8>,
    cartridge_type: Option<MbcType>,
    rom_version: Option<u8>,
    overwrite_rom: bool,
    pad_value: Option<u8>,
    ram_size: Option<u8>,
    sgb: bool,
    fix_spec: FixSpec,
    model: Model,
    title: Option<String>,
    title_len: usize,
}

#[derive(Parser, Debug)]
#[clap(version = "1.0", about = "A tool to fix ROM files for Game Boy.")]
struct Cli {
    #[clap(short = 'C', long = "color-only")]
    /// Color-only mode
    color_only: bool,

    #[clap(short = 'c', long = "color-compatible")]
    /// Color-compatible mode
    color_compatible: bool,

    #[clap(short = 'f', long = "fix-spec", value_name = "FIX_SPEC", validator = parse_fix_spec)]
    /// Specify a fix specification
    fix_spec: Option<String>,

    #[clap(short = 'i', long = "game-id", value_name = "GAME_ID")]
    /// Specify a game ID
    game_id: Option<String>,

    #[clap(short = 'j', long = "non-japanese")]
    /// Non-Japanese mode
    non_japanese: bool,

    #[clap(short = 'k', long = "new-licensee", value_name = "NEW_LICENSEE")]
    /// Specify a new licensee
    new_licensee: Option<String>,

    #[clap(short = 'l', long = "old-licensee", value_name = "OLD_LICENSEE", value_parser=maybe_hex::<u8>)]
    /// Specify an old licensee
    old_licensee: Option<u8>,

    #[clap(short = 'm', long = "mbc-type", value_name = "MBC_TYPE")]
    /// Specify the MBC type
    mbc_type: Option<String>,

    #[clap(short = 'n', long = "rom-version", value_name = "ROM_VERSION", value_parser=maybe_hex::<u8>)]
    /// Specify the ROM version
    rom_version: Option<u8>,

    #[clap(short = 'O', long = "overwrite")]
    /// Overwrite the file
    overwrite: bool,

    #[clap(short = 'p', long = "pad-value", value_name = "PAD_VALUE", value_parser=maybe_hex::<u8>)]
    /// Specify the padding value
    pad_value: Option<u8>,

    #[clap(short = 'r', long = "ram-size", value_name = "RAM_SIZE", value_parser=maybe_hex::<u8>)]
    /// Specify the RAM size
    ram_size: Option<u8>,

    #[clap(short = 's', long = "sgb-compatible")]
    /// SGB-compatible mode
    sgb_compatible: bool,

    #[clap(short = 't', long = "title", value_name = "TITLE")]
    /// Specify the title
    title: Option<String>,

    #[clap(short = 'V', long = "version")]
    /// Print RGBFIX version and exit
    version: bool,

    #[clap(short = 'v', long = "validate")]
    /// Fix the header logo and both checksums (-f lhg)
    validate: bool,

    #[clap(value_name = "FILES")]
    /// Files to process
    files: Vec<String>,
}

fn parse_fix_spec(s: &str) -> Result<FixSpec, String> {
    s.parse::<FixSpec>()
}

#[derive(Debug, Clone, PartialEq)]
enum FixState {
    Trash,
    Fix,
}

#[derive(Debug, Clone, PartialEq)]
struct FixSpec {
    logo: Option<FixState>,
    header: Option<FixState>,
    global: Option<FixState>,
}

impl FromStr for FixSpec {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut fix_spec = FixSpec {
            logo: None,
            header: None,
            global: None,
        };

        for c in s.chars() { // TOCHECK if something weird like "lL" is provided, this could do unexpected default behaviour
            match c {
                'l' => fix_spec.logo = Some(FixState::Fix),
                'L' => fix_spec.logo = Some(FixState::Trash),
                'h' => fix_spec.header = Some(FixState::Fix),
                'H' => fix_spec.header = Some(FixState::Trash),
                'g' => fix_spec.global = Some(FixState::Fix),
                'G' => fix_spec.global = Some(FixState::Trash),
                _ => return Err(format!("Invalid character: {}", c)),
            }
        }

        Ok(fix_spec)
    }
}

macro_rules! logo {
    (
		$i01:expr, $i02:expr, $i03:expr, $i04:expr, $i05:expr, $i06:expr, $i07:expr, $i08:expr,
		$i11:expr, $i12:expr, $i13:expr, $i14:expr, $i15:expr, $i16:expr, $i17:expr, $i18:expr,
		$i21:expr, $i22:expr, $i23:expr, $i24:expr, $i25:expr, $i26:expr, $i27:expr, $i28:expr,
		$i31:expr, $i32:expr, $i33:expr, $i34:expr, $i35:expr, $i36:expr, $i37:expr, $i38:expr,
		$i41:expr, $i42:expr, $i43:expr, $i44:expr, $i45:expr, $i46:expr, $i47:expr, $i48:expr,
		$i51:expr, $i52:expr, $i53:expr, $i54:expr, $i55:expr, $i56:expr, $i57:expr, $i58:expr,
	) => {
        static NINTENDO_LOGO: [u8; 48] = [
            $i01, $i02, $i03, $i04, $i05, $i06, $i07, $i08, $i11, $i12, $i13, $i14, $i15, $i16,
            $i17, $i18, $i21, $i22, $i23, $i24, $i25, $i26, $i27, $i28, $i31, $i32, $i33, $i34,
            $i35, $i36, $i37, $i38, $i41, $i42, $i43, $i54, $i45, $i46, $i47, $i48, $i51, $i52,
            $i53, $i54, $i55, $i56, $i57, $i58,
        ];

        static TRASH_LOGO: [u8; 48] = [
            0xFF ^ $i01,
            0xFF ^ $i02,
            0xFF ^ $i03,
            0xFF ^ $i04,
            0xFF ^ $i05,
            0xFF ^ $i06,
            0xFF ^ $i07,
            0xFF ^ $i08,
            0xFF ^ $i11,
            0xFF ^ $i12,
            0xFF ^ $i13,
            0xFF ^ $i14,
            0xFF ^ $i15,
            0xFF ^ $i16,
            0xFF ^ $i17,
            0xFF ^ $i18,
            0xFF ^ $i21,
            0xFF ^ $i22,
            0xFF ^ $i23,
            0xFF ^ $i24,
            0xFF ^ $i25,
            0xFF ^ $i26,
            0xFF ^ $i27,
            0xFF ^ $i28,
            0xFF ^ $i31,
            0xFF ^ $i32,
            0xFF ^ $i33,
            0xFF ^ $i34,
            0xFF ^ $i35,
            0xFF ^ $i36,
            0xFF ^ $i37,
            0xFF ^ $i38,
            0xFF ^ $i41,
            0xFF ^ $i42,
            0xFF ^ $i43,
            0xFF ^ $i54,
            0xFF ^ $i45,
            0xFF ^ $i46,
            0xFF ^ $i47,
            0xFF ^ $i48,
            0xFF ^ $i51,
            0xFF ^ $i52,
            0xFF ^ $i53,
            0xFF ^ $i54,
            0xFF ^ $i55,
            0xFF ^ $i56,
            0xFF ^ $i57,
            0xFF ^ $i58,
        ];
    };
}

logo![
    0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B, 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D,
    0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E, 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99,
    0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC, 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E,
];

fn main() {

    let mut cli_options = CliOptions {
        game_id: None,
        japanese: true,
        new_licensee: None,
        old_licensee: None,
        cartridge_type: None,
        rom_version: None,
        overwrite_rom: false,
        pad_value: None,
        ram_size: None,
        sgb: false,
        model: Model::Dmg,
        fix_spec: FixSpec { logo: None, header: None, global: None },
        title: None,
        title_len: 0,
    };

    let cli = Cli::parse();

    if let Some(mut game_id) = cli.game_id {
        if game_id.len() > 4 {
            println!("warning: Truncating game ID \"{}\" to 4 chars", &game_id);
            // Truncate game_id to 4 characters if it's longer
            game_id.truncate(4);
            println!("Truncated game ID: {}", &game_id);
        } else {
            println!("Game ID: {}", &game_id);
        }
        cli_options.game_id = Some(game_id);
    }    

    if cli.non_japanese {
        cli_options.japanese = false;
    }

    if let Some(mut new_licensee) = cli.new_licensee {
        let len = new_licensee.len() as u8;
        if len > 2 {
            println!("warning: Truncating new licensee \"{}\" to 2 chars", &new_licensee);
            new_licensee = new_licensee[0..2].to_string();
            println!("Truncated new licencee: {}", &new_licensee[0..2]);
        } else {
            println!("New licensee: {}", &new_licensee);
        }
        cli_options.new_licensee = Some(new_licensee);
    }

    if let Some(old_licensee) = cli.old_licensee {
        cli_options.old_licensee = Some(old_licensee)
    }

    if let Some(mbc_type) = cli.mbc_type {
        let mbc_type_result = mbc_type.parse::<MbcType>();
        match mbc_type_result {
            Ok(mbc_type) => {
                cli_options.cartridge_type = Some(mbc_type);
            },
            Err(_e) => {
                eprintln!("Error parsing MbcType.");
                process::exit(1);
            }
        }
        if matches!(&cli_options.cartridge_type.clone().unwrap(), MbcType::RomRam | MbcType::RomRamBattery) {
            eprintln!("warning: ROM+RAM / ROM+RAM+BATTERY are under-specified and poorly supported");
        }
    }

    if let Some(rom_version) = cli.rom_version {
        cli_options.rom_version = Some(rom_version)
    }

    if cli.overwrite {
        cli_options.overwrite_rom = true;
    }

    if let Some(pad_value) = cli.pad_value {
        cli_options.pad_value = Some(pad_value)
    }

    if let Some(ram_size) = cli.ram_size {
        cli_options.ram_size = Some(ram_size)
    }

    if cli.sgb_compatible {
        cli_options.sgb = true;
    }

    if let Some(title) = cli.title {
        cli_options.title = Some(title.clone());
        let mut len = title.len();
        let max_len = max_title_len(&cli_options.game_id, &cli_options.model);
        if len > max_len {
            len = max_len;
            println!("warning: Truncating title \"{}\" to {} chars", title, max_len);
            cli_options.title_len = len;
        }
    }

    if cli.color_only || cli.color_compatible {
        cli_options.model = if cli.color_compatible { Model::Both } else { Model::Cgb };
        if cli_options.title_len > 15 {
            if let Some(mut title) = cli_options.title {
                title = title[0..15].to_string();
                eprintln!("warning: Truncating title \"{}\" to 15 chars", title);
                cli_options.title = Some(title);
            }
        }
    }

    if cli.version {
        println!("rgbfix version {}", cli.version);
        process::exit(0);
    }

    if cli.validate {
        cli_options.fix_spec = FixSpec {logo: Some(FixState::Fix), header: Some(FixState::Fix), global: Some(FixState::Fix)};
    }

    if !cli.files.is_empty() {
        let _ = process_filename(&cli.files[0], cli_options); // TOCHECK [0] is dubious, are multiple files getting fixed in one CLI call possible?
    }

}

#[derive(Debug, Clone, PartialEq)]
enum MbcType {
    Rom,
    RomRam,
    RomRamBattery,
    Mbc1,
    Mbc1Ram,
    Mbc1RamBattery,
    Mbc2,
    Mbc2Battery,
    Mmm01,
    Mmm01Ram,
    Mmm01RamBattery,
    Mbc3,
    Mbc3TimerBattery,
    Mbc3TimerRamBattery,
    Mbc3Ram,
    Mbc3RamBattery,
    Mbc5,
    Mbc5Ram,
    Mbc5RamBattery,
    Mbc5Rumble,
    Mbc5RumbleRam,
    Mbc5RumbleRamBattery,
    Mbc6,
    Mbc7SensorRumbleRamBattery,
    PocketCamera,
    BandaiTama5,
    Huc3,
    Huc1RamBattery,
    Tpp1,
    Tpp1Rumble,
    Tpp1MultiRumble,
    Tpp1MultiRumbleRumble,
    Tpp1Timer,
    Tpp1TimerRumble,
    Tpp1TimerMultiRumble,
    Tpp1TimerMultiRumbleRumble,
    Tpp1Battery,
    Tpp1BatteryRumble,
    Tpp1BatteryMultiRumble,
    Tpp1BatteryMultiRumbleRumble,
    Tpp1BatteryTimer,
    Tpp1BatteryTimerRumble,
    Tpp1BatteryTimerMultiRumble,
    Tpp1BatteryTimerMultiRumbleRumble,
}

fn get_mbc_type_code(mbc_type: &MbcType) -> u16 {
    match mbc_type {
        MbcType::Rom => 0x00,
        MbcType::RomRam => 0x08,
        MbcType::RomRamBattery => 0x09,
        MbcType::Mbc1 => 0x01,
        MbcType::Mbc1Ram => 0x02,
        MbcType::Mbc1RamBattery => 0x03,
        MbcType::Mbc2 => 0x05,
        MbcType::Mbc2Battery => 0x06,
        MbcType::Mmm01 => 0x0B,
        MbcType::Mmm01Ram => 0x0C,
        MbcType::Mmm01RamBattery => 0x0D,
        MbcType::Mbc3 => 0x11,
        MbcType::Mbc3TimerBattery => 0x0F,
        MbcType::Mbc3TimerRamBattery => 0x10,
        MbcType::Mbc3Ram => 0x12,
        MbcType::Mbc3RamBattery => 0x13,
        MbcType::Mbc5 => 0x19,
        MbcType::Mbc5Ram => 0x1A,
        MbcType::Mbc5RamBattery => 0x1B,
        MbcType::Mbc5Rumble => 0x1C,
        MbcType::Mbc5RumbleRam => 0x1D,
        MbcType::Mbc5RumbleRamBattery => 0x1E,
        MbcType::Mbc6 => 0x20,
        MbcType::Mbc7SensorRumbleRamBattery => 0x22,
        MbcType::PocketCamera => 0xFC,
        MbcType::BandaiTama5 => 0xFD,
        MbcType::Huc3 => 0xFE,
        MbcType::Huc1RamBattery => 0xFF,
        MbcType::Tpp1 => 0x100,
        MbcType::Tpp1Rumble => 0x101,
        MbcType::Tpp1MultiRumble => 0x102,
        MbcType::Tpp1MultiRumbleRumble => 0x103,
        MbcType::Tpp1Timer => 0x104,
        MbcType::Tpp1TimerRumble => 0x105,
        MbcType::Tpp1TimerMultiRumble => 0x106,
        MbcType::Tpp1TimerMultiRumbleRumble => 0x107,
        MbcType::Tpp1Battery => 0x108,
        MbcType::Tpp1BatteryRumble => 0x109,
        MbcType::Tpp1BatteryMultiRumble => 0x10A,
        MbcType::Tpp1BatteryMultiRumbleRumble => 0x10B,
        MbcType::Tpp1BatteryTimer => 0x10C,
        MbcType::Tpp1BatteryTimerRumble => 0x10D,
        MbcType::Tpp1BatteryTimerMultiRumble => 0x10E,
        MbcType::Tpp1BatteryTimerMultiRumbleRumble => 0x10F,
    }
}

fn get_mbc_enum(mbc_type: u16) -> MbcType {
    match mbc_type {
        0x00 => MbcType::Rom,
        0x08 => MbcType::RomRam,
        0x09 => MbcType::RomRamBattery,
        0x01 => MbcType::Mbc1,
        0x02 => MbcType::Mbc1Ram,
        0x03 => MbcType::Mbc1RamBattery,
        0x05 => MbcType::Mbc2,
        0x06 => MbcType::Mbc2Battery,
        0x0B => MbcType::Mmm01,
        0x0C => MbcType::Mmm01Ram,
        0x0D => MbcType::Mmm01RamBattery,
        0x11 => MbcType::Mbc3,
        0x0F => MbcType::Mbc3TimerBattery,
        0x10 => MbcType::Mbc3TimerRamBattery,
        0x12 => MbcType::Mbc3Ram,
        0x13 => MbcType::Mbc3RamBattery,
        0x19 => MbcType::Mbc5,
        0x1A => MbcType::Mbc5Ram,
        0x1B => MbcType::Mbc5RamBattery,
        0x1C => MbcType::Mbc5Rumble,
        0x1D => MbcType::Mbc5RumbleRam,
        0x1E => MbcType::Mbc5RumbleRamBattery,
        0x20 => MbcType::Mbc6,
        0x22 => MbcType::Mbc7SensorRumbleRamBattery,
        0xFC => MbcType::PocketCamera,
        0xFD => MbcType::BandaiTama5,
        0xFE => MbcType::Huc3,
        0xFF => MbcType::Huc1RamBattery,
        0x100 => MbcType::Tpp1,
        0x101 => MbcType::Tpp1Rumble,
        0x102 => MbcType::Tpp1MultiRumble,
        0x103 => MbcType::Tpp1MultiRumbleRumble,
        0x104 => MbcType::Tpp1Timer,
        0x105 => MbcType::Tpp1TimerRumble,
        0x106 => MbcType::Tpp1TimerMultiRumble,
        0x107 => MbcType::Tpp1TimerMultiRumbleRumble,
        0x108 => MbcType::Tpp1Battery,
        0x109 => MbcType::Tpp1BatteryRumble,
        0x10A => MbcType::Tpp1BatteryMultiRumble,
        0x10B => MbcType::Tpp1BatteryMultiRumbleRumble,
        0x10C => MbcType::Tpp1BatteryTimer,
        0x10D => MbcType::Tpp1BatteryTimerRumble,
        0x10E => MbcType::Tpp1BatteryTimerMultiRumble,
        0x10F => MbcType::Tpp1BatteryTimerMultiRumbleRumble,
        _ => {
            eprintln!("Invalid MbcType.");
            process::exit(1);
        }
    }
}

fn is_tpp1_type(mbc_type: &Option<MbcType>) -> bool {
    let mbc_type = if mbc_type.is_none() {
        return false; 
    }
    else {
        mbc_type.clone().unwrap()
    };
    match mbc_type {
        MbcType::Tpp1Rumble |
        MbcType::Tpp1MultiRumble |
        MbcType::Tpp1MultiRumbleRumble |
        MbcType::Tpp1Timer |
        MbcType::Tpp1TimerRumble |
        MbcType::Tpp1TimerMultiRumble |
        MbcType::Tpp1TimerMultiRumbleRumble |
        MbcType::Tpp1Battery |
        MbcType::Tpp1BatteryRumble |
        MbcType::Tpp1BatteryMultiRumble |
        MbcType::Tpp1BatteryMultiRumbleRumble |
        MbcType::Tpp1BatteryTimer |
        MbcType::Tpp1BatteryTimerRumble |
        MbcType::Tpp1BatteryTimerMultiRumble |
        MbcType::Tpp1BatteryTimerMultiRumbleRumble => true,
        _ => false,
    }
}

fn mbc_has_ram(mbc_type: &MbcType) -> Option<bool> {
    match mbc_type {
        MbcType::Rom
        | MbcType::Mbc1
        | MbcType::Mbc2
        | MbcType::Mbc2Battery
        | MbcType::Mmm01
        | MbcType::Mbc3
        | MbcType::Mbc3TimerBattery
        | MbcType::Mbc5
        | MbcType::Mbc5Rumble
        | MbcType::Mbc6
        | MbcType::BandaiTama5 => Some(false),
        MbcType::RomRam
        | MbcType::RomRamBattery
        | MbcType::Mbc1Ram
        | MbcType::Mbc1RamBattery
        | MbcType::Mmm01Ram
        | MbcType::Mmm01RamBattery
        | MbcType::Mbc3TimerRamBattery
        | MbcType::Mbc3Ram
        | MbcType::Mbc3RamBattery
        | MbcType::Mbc5Ram
        | MbcType::Mbc5RamBattery
        | MbcType::Mbc5RumbleRam
        | MbcType::Mbc5RumbleRamBattery
        | MbcType::Mbc7SensorRumbleRamBattery
        | MbcType::PocketCamera
        | MbcType::Huc3
        | MbcType::Huc1RamBattery => Some(true),
        // TPP1 may or may not have RAM, return None for it
        MbcType::Tpp1
        | MbcType::Tpp1Rumble
        | MbcType::Tpp1MultiRumble
        | MbcType::Tpp1MultiRumbleRumble
        | MbcType::Tpp1Timer
        | MbcType::Tpp1TimerRumble
        | MbcType::Tpp1TimerMultiRumble
        | MbcType::Tpp1TimerMultiRumbleRumble
        | MbcType::Tpp1Battery
        | MbcType::Tpp1BatteryRumble
        | MbcType::Tpp1BatteryMultiRumble
        | MbcType::Tpp1BatteryMultiRumbleRumble
        | MbcType::Tpp1BatteryTimer
        | MbcType::Tpp1BatteryTimerRumble
        | MbcType::Tpp1BatteryTimerMultiRumble
        | MbcType::Tpp1BatteryTimerMultiRumbleRumble => None,
    }
}

#[derive(Debug)]
enum ParseError {
    Bad,
    BadRange,
}

impl FromStr for MbcType {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        if s.chars().next().unwrap().is_ascii_digit() || s.starts_with('$') {
            let base = if s.starts_with('$') { 16 } else { 10 };
            let mbc = u16::from_str_radix(s.trim_start_matches('$'), base).map_err(|_| ParseError::Bad)?;
            if mbc > 0xFF {
                return Err(ParseError::BadRange);
            }
            return Ok(get_mbc_enum(mbc));
        }

        Err(ParseError::Bad)
    }
}

#[derive(Debug, Clone)]
enum Model {
    Dmg,
    Both,
    Cgb,
}

fn max_title_len(game_id: &Option<String>, model: &Model) -> usize {
    match (game_id.is_some(), !matches!(model, Model::Dmg)) {
        (true, _) => 11,
        (false, true) => 15,
        _ => 16,
    }
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
            },
            Err(e) if e.kind() == io::ErrorKind::Interrupted => continue,
            Err(e) => return Err(e),
        }
    }
    Ok(total)
}

fn write_bytes(file: &mut File, buf: &[u8]) -> io::Result<usize> {
    let mut total = 0;
    let mut len = buf.len();

    while len > 0 {
        match file.write(&buf[total..]) {
            Ok(0) => {
                // EOF reached
                break;
            }
            Ok(n) => {
                // If anything was written, accumulate it, and continue
                total += n;
                len -= n;
            }
            Err(ref e) if e.kind() == io::ErrorKind::Interrupted => {
                // Interrupted, continue
                continue;
            }
            Err(e) => {
                // Return errors, unless we only were interrupted
                return Err(e);
            }
        }
    }

    Ok(total)
}

fn overwrite_byte(rom0: &mut [u8], addr: u16, fixed_byte: u8, area_name: &str, overwrite_rom: bool) {
    let orig_byte = rom0[addr as usize];

    if !overwrite_rom && orig_byte != 0 && orig_byte != fixed_byte {
        eprintln!("warning: Overwrote a non-zero byte in the {}", area_name);
    }

    rom0[addr as usize] = fixed_byte;
}

fn overwrite_bytes(rom0: &mut [u8], start_addr: u16, fixed: &[u8], area_name: &str, overwrite_rom: bool) -> io::Result<()> {
    if start_addr as usize + fixed.len() > rom0.len() {
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "Address or size out of bounds"));
    }
    if !overwrite_rom {
        for (i, &byte) in fixed.iter().enumerate() {
            if rom0[i + start_addr as usize] != 0 && rom0[i + start_addr as usize] != byte {
                println!("warning: Overwrote a non-zero byte in the {}", area_name);
                break;
            }
        }
    }
    rom0[start_addr as usize..start_addr as usize + fixed.len()].copy_from_slice(fixed);
    Ok(())
}

fn process_file(input: &mut File, output: &mut File, name: &str, file_size: u64, options: CliOptions) -> io::Result<()> {
    // Check if the file is seekable
    if true || input.as_raw_fd() == output.as_raw_fd() { // TOCHECK: I have no idea what these checks are doing, so I disabled them. Check this.
        assert!(file_size != 0);
    } else {
        assert!(file_size == 0);
    }

    let mut rom0 = [0u8; BANK_SIZE];
    let mut rom0_len = match read_bytes(input, &mut rom0) {
        Ok(corr_len) => corr_len,
        Err(_e) => { 
            eprintln!("Invalid file input."); 
            process::exit(1); 
        }
    };
    
    let header_size = if is_tpp1_type(&options.cartridge_type) { 0x154 } else { 0x150 };

    if rom0_len < header_size as usize {
        eprintln!("FATAL: \"{}\" too short, expected at least {} bytes, got only {}",
                 name, header_size, rom0_len);
        return Err(io::Error::new(io::ErrorKind::InvalidInput, "File too short"));
    }

    let _ = match options.fix_spec.logo {
        Some(FixState::Fix) => overwrite_bytes(&mut rom0, 0x0104, &NINTENDO_LOGO, "Nintendo logo", options.overwrite_rom),
        Some(FixState::Trash) => overwrite_bytes(&mut rom0, 0x0104, &TRASH_LOGO, "Nintendo logo", options.overwrite_rom),
        None => Ok (()),
    };

    if let Some(title) = options.title {
        overwrite_bytes(&mut rom0[..], 0x134, title.as_bytes(), "title", options.overwrite_rom)?;
    }

    if let Some(game_id) = options.game_id {
        let _ = overwrite_bytes(&mut rom0[..], 0x13F, game_id.as_bytes(), "manufacturer code", options.overwrite_rom);
    }

    if !matches!(options.model, Model::Dmg) {
        match options.model {  
            Model::Both => overwrite_byte(&mut rom0, 0x143, 0x80, "Cgb flag", options.overwrite_rom),  
            Model::Cgb => overwrite_byte(&mut rom0, 0x143, 0xC0, "Cgb flag", options.overwrite_rom),  
            Model::Dmg => (),  
        };  
    };

    if let Some(new_licensee) = options.new_licensee {
        let _ = overwrite_bytes(&mut rom0[..], 0x144, new_licensee.as_bytes(), "new licensee code", options.overwrite_rom);
    }

    if options.sgb {
        let _ = overwrite_byte(&mut rom0[..], 0x146, 0x03, "SGB flag", options.overwrite_rom);
    }

    let ram_size = options.ram_size;

    if let Some(ref cartridge_type) = options.cartridge_type {
        let mut byte = cartridge_type.clone() as u8;

        if is_tpp1_type(&Some(cartridge_type.clone())) {
            // Cartridge type isn't directly actionable, translate it
            byte = 0xBC;
            // The other TPP1 identification bytes will be written below
        }
        overwrite_byte(&mut rom0[..], 0x147, byte, "cartridge type", options.overwrite_rom);
    }

    // ROM size will be written last, after evaluating the file's size
    
    if is_tpp1_type(&options.cartridge_type) {
        let cartridge_type = &options.cartridge_type.clone().unwrap(); // safe because it passed is_tpp1_type
        let tpp1_code = vec![0xC1, 0x65];
        let tpp1_rev = vec![0xC1, 0x65]; // TODO WARNING PLACEHOLDER NOT ACTUAL VALUES, PICK UP FROM OPTIONS INSTEAD?

        // TODO: I don't understand this part. Tpp1_rev comes from tryReadSlice and I still don't understand very well what it does.
        let _ = overwrite_bytes(&mut rom0[..], 0x149, &tpp1_code, "TPP1 identification code", options.overwrite_rom);

        let _ = overwrite_bytes(&mut rom0[..], 0x150, &tpp1_rev, "TPP1 revision number", options.overwrite_rom);

        if let Some(ram_size) = ram_size {
            overwrite_byte(&mut rom0[..], 0x152, ram_size as u8, "RAM size", options.overwrite_rom);
        }

        overwrite_byte(&mut rom0[..], 0x153, (get_mbc_type_code(cartridge_type) & 0xFF) as u8, "TPP1 feature flags", options.overwrite_rom);
    } else {
        // Regular mappers

        if let Some(ram_size) = ram_size {
            overwrite_byte(&mut rom0[..], 0x149, ram_size as u8, "RAM size", options.overwrite_rom);
        }

        if !options.japanese {
            overwrite_byte(&mut rom0[..], 0x14A, 0x01, "destination code", options.overwrite_rom);
        }
    }

    if let Some(old_licensee) = options.old_licensee {
        overwrite_byte(&mut rom0[..], 0x14B, old_licensee as u8, "old licensee code", options.overwrite_rom);
    } else if options.sgb && rom0[0x14B] != 0x33 {
        eprintln!("warning: SGB compatibility enabled, but old licensee was 0x{:02x}, not 0x33", rom0[0x14B]);
    }
    
    if let Some(rom_version) = options.rom_version {
        overwrite_byte(&mut rom0[..], 0x14C, rom_version as u8, "mask ROM version number", options.overwrite_rom);
    }

    let mut romx: Vec<u8> = Vec::new(); // Buffer of ROMX bank data
    let mut nb_banks: u32 = 1; // Number of banks *targeted*, including ROM0
    let mut total_romx_len: usize = 0; // *Actual* size of ROMX data
    let mut bank: [u8; BANK_SIZE] = [0; BANK_SIZE]; // Temp buffer used to store a whole bank's worth of data
    let mut global_sum: u16 = 0; // Global checksum variable

    // Handle ROMX
    if true || input.as_raw_fd() == output.as_raw_fd() {
        if file_size >= (0x10000 * BANK_SIZE) as u64 {
            eprintln!("FATAL: \"{}\" has more than 65536 banks", name);
            return Ok(());
        }
        // This should be guaranteed from the size cap...
        // Rust doesn't have static_assert in the same way C++ does, but we can use compile-time checks with const assertions
        // TOCHECK: I need this assert explained to me, it always fails
        // const _: () = assert!(0x10000 * BANK_SIZE <= std::mem::size_of::<usize>());
        // Compute number of banks and ROMX len from file size
        nb_banks = ((file_size + (BANK_SIZE - 1) as u64) / BANK_SIZE as u64) as u32;
        total_romx_len = if file_size >= BANK_SIZE as u64 { (file_size - BANK_SIZE as u64) as usize } else { 0 };
    } else if rom0_len == BANK_SIZE {
        // Copy ROMX when reading a pipe, and we're not at EOF yet
        loop {
            romx.resize((nb_banks * BANK_SIZE as u32) as usize, 0); // Initialize new elements to 0
            let bank_len = read_bytes(input, &mut romx[((nb_banks - 1) * BANK_SIZE as u32) as usize..])?;

            // Update bank count, ONLY IF at least one byte was read
            if bank_len > 0 {
                // We're gonna read another bank, check that it won't be too much
                // TOCHECK: same thing as the previous one
                // const _: () = assert!(0x10000 * BANK_SIZE <= std::mem::size_of::<usize>());
                if nb_banks == 0x10000 {
                    eprintln!("FATAL: \"{}\" has more than 65536 banks", name);
                    return Ok(());
                }
                nb_banks += 1;

                // Update global checksum, too
                for i in 0..bank_len {
                    global_sum += romx[total_romx_len + i] as u16;
                }
                total_romx_len += bank_len;
            }
            // Stop when an incomplete bank has been read
            if bank_len != BANK_SIZE {
                break;
            }
        }
    }

    if let Some(pad_value) = options.pad_value {
        // We want at least 2 banks
        if nb_banks == 1 {
            if rom0_len != rom0.len() {
                // Fill the remaining space in rom0 with pad_value
                for i in rom0_len..rom0.len() {
                    rom0[i] = pad_value as u8;
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

        let intermediate_result = ((nb_banks - 1) * BANK_SIZE as u32 - total_romx_len as u32) as u8; //TOCHECK This also uses the wrapping mult, assuming C++ was exploiting overflow on purpose
        let result = pad_value.wrapping_mul(intermediate_result);
        global_sum += result as u16;
    }

    // Handle the header checksum after the ROM size has been written
    if options.fix_spec.header.is_some() {
        let mut sum: u8 = 0;

        for i in 0x134..0x14D {
            sum = sum.wrapping_sub(rom0[i] + 1);
        }

        overwrite_byte(&mut rom0, 0x14D, if matches!(options.fix_spec.header, Some(FixState::Trash)) { !sum } else { sum }, "header checksum", options.overwrite_rom);
    }

    if options.fix_spec.global.is_some() { 
        assert!(rom0_len >= 0x14E, "ROM0 length must be at least 0x14E");
        for i in 0..0x14E {
            global_sum = global_sum.wrapping_add(rom0[i] as u16);
        }
        for i in 0x150..rom0_len {
            global_sum = global_sum.wrapping_add(rom0[i] as u16);
        }
        // Pipes have already read ROMX and updated global_sum, but not regular files
        if true || input.as_raw_fd() == output.as_raw_fd() {
            loop {
                let bank_len = read_bytes(input, &mut bank)?;

                for i in 0..bank_len {
                    global_sum = global_sum.wrapping_add(bank[i] as u16);
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

        let _ = overwrite_bytes(&mut rom0, 0x14E, &bytes, "global checksum", options.overwrite_rom);
    }

    let mut write_len: usize;

    // In case the output depends on the input, reset to the beginning of the file, and only
    // write the header
    if true || input.as_raw_fd() == output.as_raw_fd() {
        if let Err(e) = output.seek(io::SeekFrom::Start(0)) {
            eprintln!("FATAL: Failed to rewind \"{}\": {}", name, e);
            return Ok(());
        }
        // If modifying the file in-place, we only need to edit the header
        // However, padding may have modified ROM0 (added padding), so don't in that case
        if options.pad_value.is_some() {
            rom0_len = header_size;
        }
    }
    
    write_len = write_bytes(output, &rom0[..rom0_len])?;

    if (write_len) < rom0_len {
        eprintln!("FATAL: Could only write {} of \"{}\"'s {} ROM0 bytes",
            write_len, name, rom0_len);
        return Err(io::Error::new(io::ErrorKind::UnexpectedEof, ""));
    }

    // Output ROMX if it was buffered
    if !romx.is_empty() {
        write_len = write_bytes(output, &romx[..total_romx_len])?;
        if (write_len) < total_romx_len {
            eprintln!("FATAL: Could only write {} of \"{}\"'s {} ROMX bytes",
                write_len, name, total_romx_len);
            return Err(io::Error::new(io::ErrorKind::UnexpectedEof, ""));
        }
    }

    // Output padding
    if options.pad_value.is_some() {
        if true || input.as_raw_fd() == output.as_raw_fd() {
            if let Err(e) = output.seek(io::SeekFrom::End(0)) {
                eprintln!("FATAL: Failed to seek to end of \"{}\": {}", name, e);
                return Err(io::Error::new(io::ErrorKind::InvalidInput, ""));
            }
        }
        bank.fill(options.pad_value.unwrap());
        let mut len = (nb_banks - 1) * BANK_SIZE as u32 - total_romx_len as u32; // Don't count ROM0!

        while len > 0 {
            let this_len = if len > BANK_SIZE as u32 { BANK_SIZE } else { len as usize };
            write_len = write_bytes(output, &bank[..this_len])?;

            if write_len as usize != this_len {
                eprintln!("FATAL: Failed to write \"{}\"'s padding: {}", name, io::Error::last_os_error());
                break;
            }
            len -= this_len as u32;
        }
    }

    
    Ok(())
}

fn process_filename(name: &str, options: CliOptions) -> io::Result<bool> {
    let mut nb_errors = 0;
    let mut file = OpenOptions::new()
        .read(true)
        .write(true)
        .open(name)?;
    let mut temp_file = file.try_clone()?; //TOCHECK: This could be plain wrong, check what output file actually does in process_File
    if name == "-" {
        //TOCHECK: Make SURE only [u8] are used in processing to avoid Windows OS translating newline characters.
        process_file(&mut file, &mut temp_file, name, 0, options)?;
    } else {


        let metadata = file.metadata()?;
        if !metadata.is_file() {
            eprintln!("FATAL: \"{}\" is not a regular file, and thus cannot be modified in-place", name);
            nb_errors += 1;
        } else if metadata.len() < 0x150 {
            eprintln!("FATAL: \"{}\" too short, expected at least 336 ($150) bytes, got only {}", name, metadata.len());
            nb_errors += 1;
        } else {
            process_file(&mut file, &mut temp_file, name, metadata.len(), options)?;
        }
    }

    if nb_errors > 0 {
        eprintln!("Fixing \"{}\" failed with {} error{}", name, nb_errors, if nb_errors == 1 { "" } else { "s" });
    }

    Ok(nb_errors == 0)
}
