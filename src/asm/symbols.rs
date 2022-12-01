use std::{collections::HashMap, rc::Rc};

use string_interner::{backend::StringBackend, symbol::SymbolU32, StringInterner};

use crate::{
    fstack::Fstack,
    input::SourceString,
    language::{AsmError, AsmErrorKind, Location, ParseError},
};

#[derive(Debug)]
pub struct Symbols<'fstack> {
    // File stack that locations are to be resolved against.
    fstack: &'fstack Fstack,

    names: StringInterner<StringBackend<SymbolU32>>,
    symbols: HashMap<SymbolU32, SymbolData<'fstack>>,
    cur_scope: Option<Symbol<'fstack>>,
}
pub type Symbol<'fstack> = (SourceString, SymbolData<'fstack>);

impl<'fstack> Symbols<'fstack> {
    pub fn new(fstack: &'fstack Fstack) -> Self {
        const BUILTINS: &[(&str, SymbolKind)] = &[
            ("@", SymbolKind::Pc),
            ("_NARG", SymbolKind::Narg),
            ("_RS", SymbolKind::Variable(0)),
        ];

        // Build the name interner and symbol table, starting with builtins.
        let names: StringInterner = BUILTINS.iter().map(|(name, _)| name).collect();
        let symbols = BUILTINS
            .iter()
            .map(|(name, kind)| {
                (
                    names.get(name).unwrap(),
                    SymbolData {
                        kind: kind.clone(),
                        is_builtin: true,
                        definition: (Location::builtin(), Location::builtin()),
                    },
                )
            })
            .collect();

        Self {
            fstack,

            names,
            symbols,
            cur_scope: None,
        }
    }

    fn def_non_reloc(
        &mut self,
        name_begin: Location<'fstack>,
        name_string: SourceString,
        name_end: Location<'fstack>,
        kind: SymbolKind,
    ) -> Result<(), AsmError> {
        let name = self.names.get_or_intern(&name_string);
        // `try_insert` would be nicer, but it's unstable for now.
        if let Some(other) = self.symbols.get(&name) {
            let (file_id, range) =
                Fstack::make_diag_info(&other.definition.0, Some(&other.definition.1));
            return Err(AsmError {
                begin: name_begin,
                end: name_end,
                kind: AsmErrorKind::SymAlreadyDefined(name_string, file_id, range),
            });
        }

        self.symbols.insert(
            name,
            SymbolData {
                kind,
                is_builtin: false,
                definition: (name_begin, name_end),
            },
        );
        Ok(())
    }

    pub fn def_constant(
        &mut self,
        name_begin: Location<'fstack>,
        name_string: SourceString,
        name_end: Location<'fstack>,
        value: i32,
    ) -> Result<(), ParseError> {
        self.def_non_reloc(
            name_begin,
            name_string,
            name_end,
            SymbolKind::Constant(value),
        )?;
        Ok(())
    }

    pub fn def_variable(
        &mut self,
        name_begin: Location<'fstack>,
        name_string: SourceString,
        name_end: Location<'fstack>,
        value: i32,
    ) -> Result<(), ParseError> {
        self.def_non_reloc(
            name_begin,
            name_string,
            name_end,
            SymbolKind::Variable(value),
        )?;
        Ok(())
    }

    pub fn def_string(
        &mut self,
        name_begin: Location<'fstack>,
        name_string: SourceString,
        name_end: Location<'fstack>,
        string: Rc<SourceString>,
    ) -> Result<(), ParseError> {
        self.def_non_reloc(
            name_begin,
            name_string,
            name_end,
            SymbolKind::String(string),
        )?;
        Ok(())
    }

    pub fn get_string<'this, S: AsRef<str>>(
        &'this self,
        name_str: &S,
    ) -> Option<&'this Rc<SourceString>> {
        let name = self.names.get(name_str)?;
        self.symbols.get(&name)?.get_string()
    }
}

#[derive(Debug)]
pub struct SymbolData<'fstack> {
    kind: SymbolKind,
    is_builtin: bool,
    definition: (Location<'fstack>, Location<'fstack>),
}

#[derive(Debug, Clone)]
pub enum SymbolKind {
    Constant(i32),
    Variable(i32),
    String(Rc<SourceString>),

    // Special symbol types.
    Pc,
    Narg,
}

impl SymbolData<'_> {
    pub fn get_string(&self) -> Option<&Rc<SourceString>> {
        match &self.kind {
            SymbolKind::Constant(_) => None,
            SymbolKind::Variable(_) => None,
            SymbolKind::String(equs) => Some(equs),
            SymbolKind::Pc => None,
            SymbolKind::Narg => None,
        }
    }
}
