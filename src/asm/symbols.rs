use std::{collections::HashMap, rc::Rc};

use string_interner::{backend::StringBackend, symbol::SymbolU32, StringInterner, Symbol};

use crate::{
    fstack::Fstack,
    language::{AsmError, AsmErrorKind, Location, SymEvalErrKind},
    macro_args::MacroArgs,
    sections::Sections,
};

#[derive(Debug)]
pub struct Symbols<'fstack> {
    names: StringInterner<StringBackend<SymbolU32>>,
    symbols: HashMap<SymbolU32, SymbolData<'fstack>>,
}

#[derive(Debug, Clone, Copy)]
pub struct SymbolId(pub u32);

impl<'fstack> Symbols<'fstack> {
    pub fn new() -> Self {
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
                        is_referenced: false,
                    },
                )
            })
            .collect();

        Self { names, symbols }
    }

    fn def_non_reloc(
        &mut self,
        name_begin: Location<'fstack>,
        name_string: String,
        name_end: Location<'fstack>,
        kind: SymbolKind,
        allow_redef: bool,
    ) -> Result<(), AsmError<'fstack>> {
        let name = self.names.get_or_intern(&name_string);
        match self.symbols.get_mut(&name) {
            // The symbol just doesn't exist.
            None => {
                self.symbols.insert(
                    name,
                    SymbolData {
                        kind,
                        is_builtin: false,
                        definition: (name_begin, name_end),
                        is_referenced: false,
                    },
                );
                Ok(())
            }
            // The symbol was previously created.
            Some(other) => {
                // Can the existing symbol be overwritten?
                // A numeric symbol can overwrite a reference to such,
                // and we may have been given permission to redefine, but only of the same kind.
                if (matches!(other.kind, SymbolKind::NumRef) && kind.is_numeric())
                    || (allow_redef
                        && std::mem::discriminant(&other.kind) == std::mem::discriminant(&kind))
                {
                    debug_assert!(!other.is_builtin);
                    // Additionally, inherit the "referenced" state.
                    other.kind = kind;
                    other.definition = (name_begin, name_end); // Forget about the previous definition.
                    Ok(())
                } else {
                    let other_def_info =
                        Fstack::make_diag_info(&other.definition.0, Some(&other.definition.1));
                    Err(AsmError {
                        begin: name_begin,
                        end: name_end,
                        kind: AsmErrorKind::SymAlreadyDefined(name_string, other_def_info),
                    })
                }
            }
        }
    }

    pub fn def_constant(
        &mut self,
        name_begin: Location<'fstack>,
        name_string: String,
        name_end: Location<'fstack>,
        value: i32,
        allow_redef: bool,
    ) -> Result<(), AsmError<'fstack>> {
        self.def_non_reloc(
            name_begin,
            name_string,
            name_end,
            SymbolKind::Constant(value),
            allow_redef,
        )
    }

    pub fn def_variable(
        &mut self,
        name_begin: Location<'fstack>,
        name_string: String,
        name_end: Location<'fstack>,
        value: i32,
    ) -> Result<(), AsmError<'fstack>> {
        self.def_non_reloc(
            name_begin,
            name_string,
            name_end,
            SymbolKind::Variable(value),
            true,
        )
    }

    pub fn def_string(
        &mut self,
        name_begin: Location<'fstack>,
        name_string: String,
        name_end: Location<'fstack>,
        string: Rc<String>,
    ) -> Result<(), AsmError<'fstack>> {
        self.def_non_reloc(
            name_begin,
            name_string,
            name_end,
            SymbolKind::String(string),
            false,
        )
    }

    pub fn def_macro(
        &mut self,
        name_begin: Location<'fstack>,
        name_string: String,
        name_end: Location<'fstack>,
        body: Rc<String>,
    ) -> Result<(), AsmError<'fstack>> {
        self.def_non_reloc(
            name_begin,
            name_string,
            name_end,
            SymbolKind::Macro(body),
            false,
        )
    }

    fn get_sym(&self, name_str: &str) -> Option<&SymbolData<'fstack>> {
        self.names
            .get(name_str)
            .and_then(|name| self.symbols.get(&name))
    }

    fn get_sym_mut(&mut self, name_str: &str) -> Option<&mut SymbolData<'fstack>> {
        self.names
            .get(name_str)
            .and_then(|name| self.symbols.get_mut(&name))
    }

    pub fn get_number(
        &self,
        name_str: &String,
        macro_args: Option<&MacroArgs>,
        sections: &Sections,
    ) -> Result<i32, SymEvalErrKind> {
        self.get_sym(name_str)
            .ok_or_else(|| SymEvalErrKind::NoSuchSymbol(String::clone(name_str)))?
            .get_number(name_str, macro_args, sections)
    }

    pub fn get_number_from_id(
        &self,
        id: SymbolId,
        macro_args: Option<&MacroArgs>,
        sections: &Sections,
    ) -> Result<i32, SymEvalErrKind> {
        let name = SymbolU32::try_from_usize(id.0.try_into().unwrap()).unwrap();
        let name_str = self
            .names
            .resolve(name)
            .expect("Generated invalid sym ID in RPN!?")
            .into();
        match self.symbols.get(&name) {
            Some(sym_data) => sym_data.get_number(&name_str, macro_args, sections),
            None => Err(SymEvalErrKind::NoSuchSymbol(name_str)),
        }
    }

    pub fn get_mut_number(&mut self, name_str: &str) -> Result<&mut i32, SymEvalErrKind> {
        self.get_sym_mut(name_str)
            .ok_or_else(|| SymEvalErrKind::NoSuchSymbol(name_str.into()))?
            .get_mut_number(name_str)
    }

    pub fn get_rs(&mut self) -> &mut i32 {
        self.get_mut_number("_RS")
            .expect("Built-in symbol _RS somehow got undefined?!?")
    }

    pub fn get_string(&self, name_str: &String) -> Result<&Rc<String>, AsmErrorKind> {
        self.get_sym(name_str)
            .ok_or_else(|| AsmErrorKind::NoSuchSymbol(name_str.clone()))?
            .get_string()
            .ok_or_else(|| AsmErrorKind::SymNotEqus(name_str.clone()))
    }

    pub fn get_macro(&self, name_str: &String) -> Result<(SymbolU32, &Rc<String>), AsmErrorKind> {
        let (name, data) = self
            .names
            .get(name_str)
            .and_then(|name| self.symbols.get(&name).map(|data| (name, data)))
            .ok_or_else(|| AsmErrorKind::NoSuchSymbol(name_str.clone()))?;
        Ok((
            name,
            data.get_macro()
                .ok_or_else(|| AsmErrorKind::SymNotMacro(name_str.clone()))?,
        ))
    }

    pub fn purge(&mut self, name_str: &str) -> Result<(), AsmErrorKind> {
        let name = self
            .names
            .get(name_str)
            .ok_or_else(|| AsmErrorKind::NoSuchSymbol(name_str.into()))?;
        let std::collections::hash_map::Entry::Occupied(entry) = self.symbols.entry(name) else {
            return Err(AsmErrorKind::NoSuchSymbol(name_str.into()));
        };
        let symbol = entry.get();
        if symbol.is_builtin {
            Err(AsmErrorKind::PurgingBuiltin(name_str.into()))
        } else if symbol.is_referenced() {
            Err(AsmErrorKind::PurgingReferenced(name_str.into()))
        } else {
            // TODO: do not keep a reference to the label's name as the label scope, if applicable
            entry.remove();
            Ok(())
        }
    }

    /// References a symbol in a numeric expression, creating it as an empty "reference" if it doesn't exist.
    /// On success, returns a unique identifier for that symbol.
    pub fn add_num_ref(
        &mut self,
        name_str: &String,
        begin: &Location<'fstack>,
        end: &Location<'fstack>,
    ) -> Result<SymbolId, SymEvalErrKind> {
        use std::collections::hash_map::Entry;

        let name = self.names.get_or_intern(name_str);
        match self.symbols.entry(name) {
            Entry::Vacant(entry) => {
                entry.insert(SymbolData {
                    kind: SymbolKind::NumRef,
                    is_builtin: false,
                    definition: (begin.clone(), end.clone()),
                    is_referenced: true,
                });
            }
            Entry::Occupied(mut entry) => {
                let symbol = entry.get_mut();
                if !symbol.kind.is_numeric() {
                    return Err(SymEvalErrKind::NotNumeric(String::clone(name_str)));
                }
                symbol.is_referenced = true;
            }
        }
        Ok(SymbolId(name.to_usize() as u32)) // This cast can't truncate, because the symbol is internally 32-bit.
    }
}

#[derive(Debug)]
pub struct SymbolData<'fstack> {
    kind: SymbolKind,
    is_builtin: bool,
    definition: (Location<'fstack>, Location<'fstack>),
    is_referenced: bool,
}

#[derive(Debug, Clone)]
pub enum SymbolKind {
    Constant(i32),
    Variable(i32),
    Label {
        section: (),
        offset: u16,
    },
    /// Empty reference, but only numeric types allow that.
    NumRef,
    String(Rc<String>),
    Macro(Rc<String>),

    // Special symbol types.
    Pc,
    Narg,
}

impl SymbolData<'_> {
    fn get_number(
        &self,
        name: &String,
        macro_args: Option<&MacroArgs>,
        sections: &Sections<'_>,
    ) -> Result<i32, SymEvalErrKind> {
        match &self.kind {
            SymbolKind::Constant(value) | SymbolKind::Variable(value) => Ok(*value),
            SymbolKind::Label { section, offset } => todo!(),
            SymbolKind::Pc => match sections
                .active_section()
                .ok_or(SymEvalErrKind::PcOutsideSection)?
                .try_get_pc()
            {
                Some(pc) => Ok(pc.into()),
                None => Err(SymEvalErrKind::NonConst(String::clone(name))),
            },
            SymbolKind::Narg => match macro_args {
                Some(args) => Ok(args.nb_args().try_into().expect("Macro has too many args!")),
                None => Err(SymEvalErrKind::NargOutsideMacro),
            },
            SymbolKind::NumRef => Err(SymEvalErrKind::NonConst(String::clone(name))),
            SymbolKind::String(_) | SymbolKind::Macro(_) => {
                Err(SymEvalErrKind::NotNumeric(String::clone(name)))
            }
        }
    }

    fn get_mut_number(&mut self, name: &str) -> Result<&mut i32, SymEvalErrKind> {
        match &mut self.kind {
            SymbolKind::Constant(value) | SymbolKind::Variable(value) => Ok(value),
            SymbolKind::String(_) | SymbolKind::Macro(_) => {
                Err(SymEvalErrKind::NotNumeric(name.into()))
            }
            SymbolKind::Label { .. } | SymbolKind::NumRef | SymbolKind::Pc | SymbolKind::Narg => {
                Err(SymEvalErrKind::NotMutable(name.into()))
            }
        }
    }

    fn get_string(&self) -> Option<&Rc<String>> {
        match &self.kind {
            SymbolKind::String(equs) => Some(equs),
            SymbolKind::Constant(..)
            | SymbolKind::Variable(..)
            | SymbolKind::Label { .. }
            | SymbolKind::NumRef
            | SymbolKind::Macro(..)
            | SymbolKind::Pc
            | SymbolKind::Narg => None,
        }
    }

    fn get_macro(&self) -> Option<&Rc<String>> {
        match &self.kind {
            SymbolKind::Macro(body) => Some(body),
            SymbolKind::Constant(..)
            | SymbolKind::Variable(..)
            | SymbolKind::Label { .. }
            | SymbolKind::NumRef
            | SymbolKind::String(..)
            | SymbolKind::Pc
            | SymbolKind::Narg => None,
        }
    }

    fn is_referenced(&self) -> bool {
        self.is_referenced
    }
}

impl SymbolKind {
    /// Whether this entry is numeric; in particular, whether it supports overriding a `NumRef`.
    fn is_numeric(&self) -> bool {
        matches!(
            self,
            Self::Constant(..)
                | Self::Variable(..)
                | Self::Label { .. }
                | Self::NumRef
                | Self::Pc
                | Self::Narg
        )
    }
}
