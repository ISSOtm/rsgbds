use std::{debug_assert, fs::File, io::Write, num::NonZeroU32, path::Path};

use parse_display::Display;

pub const OBJ_FMT_REV: u32 = 9;

pub fn generate_object_file<
    P: AsRef<Path>,
    Fstack: FileStackNodesProvider,
    Sections: SectionsProvider,
    Symbols: SymbolsProvider,
>(
    path: P,
    fstack: Fstack,
    sections: Sections,
    symbols: Symbols,
) -> Result<(), ErrorKind> {
    let mut file = File::create(&path).map_err(ErrorKind::OpenFailed)?;
    fn emit<W: std::io::Write, B: AsRef<[u8]>>(mut output: W, buf: B) -> Result<(), ErrorKind> {
        output
            .write_all(buf.as_ref())
            .map_err(ErrorKind::WriteFailed)
    }
    fn emit_string<W: std::io::Write>(mut output: W, string: &str) -> Result<(), ErrorKind> {
        let bytes = string.as_bytes();
        debug_assert!(
            !bytes.contains(&0),
            "Cannot serialize strings containing '\\0' to RGBDS object files!"
        );
        emit(&mut output, bytes)?;
        emit(&mut output, [0])?;
        Ok(())
    }

    let nb_fstack_nodes = u32::try_from(fstack.nodes().count()).unwrap();
    let adjust_fstack_node_id = |id: u32| nb_fstack_nodes - 1 - id;
    let adjust_opt_fstack_node_id = |opt_id: Option<NonZeroU32>| {
        opt_id
            .map_or(0u32, |id| nb_fstack_nodes - id.get())
            .wrapping_sub(1)
    };

    emit(&mut file, b"RGB9")?;
    emit(&mut file, OBJ_FMT_REV.to_le_bytes())?;
    emit(&mut file, 0u32.to_le_bytes())?;
    emit(&mut file, 0u32.to_le_bytes())?;

    emit(&mut file, nb_fstack_nodes.to_le_bytes())?;
    for node in fstack.nodes() {
        let parent_info = Fstack::parent_info(node);
        emit(
            &mut file,
            adjust_opt_fstack_node_id(parent_info.map(|(parent_id, _)| parent_id)).to_le_bytes(),
        )?;
        emit(
            &mut file,
            parent_info
                .map_or(0, |(_, parent_line_no)| parent_line_no)
                .to_le_bytes(),
        )?;

        let node_kind = Fstack::node_kind(node);
        emit(
            &mut file,
            match node_kind {
                NodeKind::Rept(..) => 0u8,
                NodeKind::File(..) => 1u8,
                NodeKind::Macro(..) => 2u8,
            }
            .to_le_bytes(),
        )?;
        match node_kind {
            NodeKind::Rept(iterations) => {
                emit(
                    &mut file,
                    u32::try_from(iterations.len()).unwrap().to_le_bytes(),
                )?;
                for iteration in iterations {
                    emit(&mut file, iteration.to_le_bytes())?;
                }
            }
            NodeKind::File(name) | NodeKind::Macro(name) => emit_string(&mut file, &name)?,
        }
    }

    // TODO: symbols

    // TODO: sections

    emit(&mut file, 0u32.to_le_bytes())?;
    // TODO: assertions

    Ok(())
}

pub trait FileStackNodesProvider {
    type Node;
    type Iter<'a>: Iterator<Item = &'a Self::Node>
    where
        Self: 'a;

    fn nodes(&self) -> Self::Iter<'_>;
    fn parent_info(node: &Self::Node) -> Option<(NonZeroU32, u32)>;
    fn node_kind(node: &Self::Node) -> NodeKind;
}

#[derive(Debug)]
pub enum NodeKind {
    Rept(Vec<u32>),
    File(String),
    Macro(String),
}

pub trait SectionsProvider {
    // TODO
}

pub trait SymbolsProvider {
    // TODO
}

#[derive(Debug, Display)]
#[non_exhaustive]
pub enum ErrorKind {
    #[display("failed to open the object file: {0}")]
    OpenFailed(std::io::Error),
    #[display("failed to write to the object file: {0}")]
    WriteFailed(std::io::Error),
}
