use chrono::prelude::*;
use compact_str::CompactString;
use rustc_hash::{FxBuildHasher, FxHashMap};
use string_interner::{backend::StringBackend, symbol::SymbolU32, StringInterner};

use crate::{
    context_stack::Span, diagnostics, format::FormatSpec, macro_args::MacroArgs,
    source_store::SourceSlice,
};

type Symbol = SymbolU32;
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymName(Symbol);
type SymbolNames = StringInterner<StringBackend<Symbol>, FxBuildHasher>;

// TODO: consider using a `Vec<Option<SymbolData>>` instead of a hash map?
#[derive(Debug)]
pub struct Symbols<'ctx_stack> {
    names: SymbolNames,
    symbols: FxHashMap<SymName, SymbolData<'ctx_stack>>,
    scope: Option<SymName>,
}

#[derive(Debug)]
pub enum SymbolData<'ctx_stack> {
    User {
        definition: Span<'ctx_stack>,
        kind: SymbolKind,
        exported: bool,
    },

    /// Built-in symbols, but that don't have special behaviour.
    Builtin(SymbolKind),
    // These builtins *do* have special behaviour.
    Pc,
    Narg,
    Dot,
    DotDot,

    /// Placeholder left over after purging a symbol, to improve error messages.
    Deleted(Span<'ctx_stack>),
}

#[derive(Debug)]
pub enum SymbolKind {
    Numeric { value: i32, mutable: bool },
    String(CompactString),
    Macro(SourceSlice),
    Label, // TODO
    Ref,
}

impl<'ctx_stack> Symbols<'ctx_stack> {
    pub fn new() -> Self {
        let mut this = Self {
            names: SymbolNames::new(),
            symbols: FxHashMap::with_hasher(FxBuildHasher),
            scope: None,
        };

        let mut def_builtin = |name, kind| {
            let name_sym = this.names.get_or_intern_static(name);
            let res = this.symbols.insert(SymName(name_sym), kind);
            debug_assert!(res.is_none());
        };
        let numeric = |value, mutable| SymbolData::Builtin(SymbolKind::Numeric { value, mutable });
        let string = |string| SymbolData::Builtin(SymbolKind::String(string));

        def_builtin("@", SymbolData::Pc);
        def_builtin("_NARG", SymbolData::Narg);
        def_builtin("_RS", numeric(0, true));
        def_builtin(".", SymbolData::Dot);
        def_builtin("..", SymbolData::DotDot);

        def_builtin(
            "__RGBDS_VERSION__",
            string(CompactString::const_new(crate::common::build::PKG_VERSION)),
        );
        def_builtin(
            "__RGBDS_MAJOR__",
            numeric(
                crate::common::build::PKG_VERSION_MAJOR
                    .parse()
                    .expect(crate::common::build::PKG_VERSION_MAJOR),
                false,
            ),
        );
        def_builtin(
            "__RGBDS_MINOR__",
            numeric(
                crate::common::build::PKG_VERSION_MINOR
                    .parse()
                    .expect(crate::common::build::PKG_VERSION_MINOR),
                false,
            ),
        );
        def_builtin(
            "__RGBDS_PATCH__",
            numeric(
                crate::common::build::PKG_VERSION_PATCH
                    .parse()
                    .expect(crate::common::build::PKG_VERSION_PATCH),
                false,
            ),
        );
        // This symbol is only defined for release candidates.
        if let Some(rc) = crate::common::build::PKG_VERSION_PRE.strip_prefix("-rc") {
            def_builtin(
                "__RGBDS_RC__",
                numeric(
                    rc.parse().expect(crate::common::build::PKG_VERSION_PRE),
                    false,
                ),
            );
        }

        let now = chrono::Local::now();
        let now_utc = now.with_timezone(&chrono::Utc);
        def_builtin(
            "__TIME__",
            string(now.format("\"%H:%M:%S\"").to_string().into()),
        );
        def_builtin(
            "__DATE__",
            string(now.format("\"%d %B %Y\"").to_string().into()),
        );
        def_builtin(
            "__ISO_8601_LOCAL__",
            string(
                now.to_rfc3339_opts(chrono::SecondsFormat::Secs, true)
                    .into(),
            ),
        );
        def_builtin(
            "__ISO_8601_UTC__",
            string(
                now_utc
                    .to_rfc3339_opts(chrono::SecondsFormat::Secs, true)
                    .into(),
            ),
        );
        def_builtin("__UTC_YEAR__", numeric(now_utc.year(), false));
        def_builtin("__UTC_MONTH__", numeric(now_utc.month() as i32, false));
        def_builtin("__UTC_DAY__", numeric(now_utc.day() as i32, false));
        def_builtin("__UTC_HOUR__", numeric(now_utc.hour() as i32, false));
        def_builtin("__UTC_MINUTE__", numeric(now_utc.minute() as i32, false));
        def_builtin("__UTC_SECOND__", numeric(now_utc.second() as i32, false));

        this
    }

    pub fn intern_name<S: AsRef<str>>(&mut self, name: S) -> SymName {
        SymName(self.names.get_or_intern(name))
    }

    pub fn get_interned_name<S: AsRef<str>>(&mut self, name: S) -> Option<SymName> {
        self.names.get(name).map(SymName)
    }

    pub fn resolve(&self, name: SymName) -> &str {
        self.names.resolve(name.0).unwrap()
    }

    fn find<S: AsRef<str>>(&mut self, name: S) -> Option<&SymbolData<'_>> {
        self.names
            .get(name)
            .and_then(|sym| self.symbols.get(&SymName(sym)))
    }

    pub fn find_macro_interned(&self, name: &SymName) -> Option<Result<&SourceSlice, &SymbolData>> {
        match self.symbols.get(name) {
            Some(SymbolData::User {
                kind: SymbolKind::Macro(slice),
                ..
            }) => Some(Ok(slice)),
            None => None,
            Some(sym) => Some(Err(sym)),
        }
    }

    pub fn find_interned(&self, name: &SymName) -> Option<&SymbolData<'_>> {
        self.symbols.get(name)
    }

    pub fn format_as<'sym: 'ctx_stack>(
        &'sym self,
        name: Option<SymName>,
        fmt: &FormatSpec,
        buf: &mut CompactString,
        macro_args: Option<&MacroArgs>,
    ) -> Result<(), FormatError<'ctx_stack, 'sym>> {
        let Some(sym) = name.and_then(|name| self.find_interned(&name)) else {
            return Err(FormatError::NotFound);
        };
        if let Some(value) = sym.get_number(macro_args) {
            todo!()
        } else if let Some(s) = sym.get_string() {
            fmt.write_str(&s, buf);
        } else if let SymbolData::Deleted(span) = sym {
            return Err(FormatError::Deleted(span));
        } else {
            return Err(FormatError::BadKind);
        };

        Ok(())
    }

    fn define_symbol(
        &mut self,
        name: SymName,
        definition: Span<'ctx_stack>,
        kind: SymbolKind,
        exported: bool,
    ) -> Result<(), &mut SymbolData<'ctx_stack>> {
        use std::collections::hash_map::Entry;

        match self.symbols.entry(name) {
            Entry::Vacant(entry) => {
                entry.insert(SymbolData::User {
                    definition,
                    kind,
                    exported,
                });
                Ok(())
            }
            Entry::Occupied(entry) => {
                let existing = entry.into_mut();
                match existing {
                    // If the entry is merely occupied by a placeholder, just override it.
                    SymbolData::Deleted(..) => {
                        *existing = SymbolData::User {
                            definition,
                            kind,
                            exported,
                        };
                        Ok(())
                    }
                    // Numeric symbols override "references" (themselves essentially placeholders).
                    SymbolData::User {
                        kind: SymbolKind::Ref,
                        exported: previously_exported,
                        ..
                    } if matches!(kind, SymbolKind::Label | SymbolKind::Numeric { .. }) => {
                        // It should be impossible for references to be marked as exported.
                        // TODO: even when you purge an exported symbol referenced in a link-time expression?
                        debug_assert!(!*previously_exported);

                        *existing = SymbolData::User {
                            definition,
                            kind,
                            exported,
                        };
                        Ok(())
                    }
                    _ => Err(existing),
                }
            }
        }
    }

    pub fn define_string_interned(
        &mut self,
        name: SymName,
        definition: Span<'ctx_stack>,
        string: CompactString,
    ) {
        if let Err(existing) =
            self.define_symbol(name, definition, SymbolKind::String(string), false)
        {
            todo!()
        }
    }
}

pub enum FormatError<'ctx_stack, 'sym> {
    NotFound,
    Deleted(&'sym Span<'ctx_stack>),
    BadKind,
}

impl<'ctx_stack> SymbolData<'ctx_stack> {
    pub fn def_span(&self) -> &Span<'ctx_stack> {
        match self {
            Self::User { definition, .. } => definition,
            _ => &Span::BUILTIN,
        }
    }

    pub fn get_number(&self, macro_args: Option<&MacroArgs>) -> Option<i32> {
        match self {
            Self::User { kind, .. } | Self::Builtin(kind) => match kind {
                SymbolKind::Numeric { value, .. } => Some(*value),
                SymbolKind::String(..) => None,
                SymbolKind::Macro(_) => None,
                SymbolKind::Label => todo!(),
                SymbolKind::Ref => None,
            },
            Self::Pc => todo!(),
            Self::Narg => todo!(),
            Self::Dot => None,
            Self::DotDot => None,
            Self::Deleted(..) => None,
        }
    }

    pub fn get_string(&self) -> Option<CompactString> {
        match self {
            Self::User { kind, .. } | Self::Builtin(kind) => match kind {
                SymbolKind::Numeric { .. } => None,
                SymbolKind::String(string) => Some(string.clone()),
                SymbolKind::Macro(_) => None,
                SymbolKind::Label => None,
                SymbolKind::Ref => None,
            },
            Self::Pc => None,
            Self::Narg => None,
            Self::Dot => todo!(),
            Self::DotDot => todo!(),
            Self::Deleted(..) => None,
        }
    }
}
