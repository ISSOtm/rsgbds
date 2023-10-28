use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Warnings, attributes(warning))]
pub fn warnings(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    impls::warnings(input)
        .unwrap_or_else(syn::Error::into_compile_error)
        .into()
}

mod impls {
    use std::{
        default::Default,
        fmt::{Display, Write},
    };

    use proc_macro2::{Delimiter, Group, Ident, Span, TokenStream};
    use quote::{quote, ToTokens, TokenStreamExt};
    use syn::{
        ext::IdentExt, Data, DataEnum, DeriveInput, Error, Fields, Lit, Meta, NestedMeta, Token,
        Variant,
    };

    pub(crate) fn warnings(input: DeriveInput) -> Result<TokenStream, Error> {
        let id_enum_name = parse_input_attrs(&input)?;

        let DeriveInput {
            data: Data::Enum(DataEnum { variants, .. }),
            ident: input_name,
            ..
        } = input
        else {
            return Err(Error::new_spanned(
                input,
                "#[derive(Warnings)] can only be applied to an enum",
            ));
        };

        let warnings = variants
            .into_iter()
            .map(parse_variant)
            .collect::<Result<Vec<_>, _>>()?;

        let mut warning_ids = Vec::with_capacity(warnings.len());
        let mut handle_parse = Vec::with_capacity(warnings.len());
        for warning in &warnings {
            let base_ident = &warning.name;
            let pat_kind = warning.pat_kind;
            let warning_name = to_kebab_case(&base_ident.to_string());

            match &warning.kind {
                &WarningKind::Boolean(default) => {
                    let warning_id = WarningId {
                        base_ident,
                        ident_suffix: None,
                        pat_kind,
                        default,
                    };

                    handle_parse.push(quote! {
                        (#warning_name, None) => {
                            levels[#id_enum_name::#warning_id as usize] = new_state;
                            Ok(())
                        }
                    });
                    warning_ids.push(warning_id);
                }
                &WarningKind::Numeric { max, default } => {
                    let make_ident = move |i| WarningId {
                        base_ident,
                        ident_suffix: Some(i),
                        pat_kind,
                        default: i <= default,
                    };

                    let first_ident = make_ident(1);
                    handle_parse.push(quote! {
                        (#warning_name, arg) => {
                            let level = match arg {
                                Some(arg) => {
                                    // `no-FLAG=1` is invalid.
                                    if new_state == WarningState::Disabled {
                                        return Err(AsmErrorKind::NegatedParametricWarning(#warning_name));
                                    }

                                    arg.parse().map_err(|err| AsmErrorKind::BadWarningArg {
                                        flag: #warning_name.to_string(),
                                        arg: arg.to_string(),
                                        err,
                                    })?
                                }
                                None => #default,
                            };
                            let base_id = #id_enum_name::#first_ident as usize;
                            let (enable, disable) = levels[base_id..][..usize::from(#max)].split_at_mut(level.into());
                            enable.fill(new_state);
                            disable.fill(WarningState::Disabled);
                            Ok(())
                        }
                    });

                    warning_ids.extend((1..=max).map(make_ident));
                }
                WarningKind::Meta(enabled_warnings) => {
                    let handle = match &enabled_warnings[..] {
                        [single] if single == "Everything" => quote! {
                            levels.fill(WarningState::Enabled);
                        },
                        warnings => quote! { #(
                            levels[#id_enum_name::#warnings as usize] = WarningState::Enabled;
                        )* },
                    };

                    handle_parse.push(quote! {
                        (#warning_name, None) => {
                            if new_state != WarningState::Enabled {
                                Err(AsmErrorKind::ModifiedMetaWarning(#warning_name))
                            } else {
                                #handle
                                Ok(())
                            }
                        }
                    });
                }
            }
        }

        let ids_doc = format!("The numeric values corresponding to individual [`{input_name}`]s, for use to index the \"warning enabled?\" array. (This is also why \"meta\" warnings are not included.)");
        let vis = input.vis;
        let nb_warnings = warning_ids.len();
        let id_flags = warning_ids.iter().map(|id| format!("`-W{id}`"));
        let defaults = warning_ids.iter().map(|id| id.default);
        let patterns = warning_ids.iter().map(WarningId::pat);
        let id_strings = warning_ids.iter().map(|id| format!("{id}"));
        Ok(quote! {
            #[doc = #ids_doc]
            #[derive(Debug, Clone, Copy)]
            #vis enum #id_enum_name { #(
                #[doc = #id_flags]
                #warning_ids,
            )* }

            impl #id_enum_name {
                #vis const NB_WARNINGS: usize = #nb_warnings;

                #vis const DEFAULTS: [bool; Self::NB_WARNINGS] = [ #( #defaults, )* ];
            }

            impl ::core::convert::From<& #input_name> for #id_enum_name {
                fn from(value: & #input_name) -> Self {
                    match value {
                        #( #input_name::#patterns => Self::#warning_ids, )*
                        _ => unreachable!(),
                    }
                }
            }

            impl ::std::fmt::Display for #id_enum_name {
                fn fmt(&self, f: &mut ::std::fmt::Formatter<'_>) -> ::std::fmt::Result {
                    match self {
                        #( Self::#warning_ids => f.write_str(#id_strings), )*
                    }
                }
            }

            impl #input_name {
                #vis fn handle_flag(flag: &str, levels: &mut [WarningState], new_state: WarningState) -> Result<(), AsmErrorKind> {
                    let (kind_name, arg) = match flag.split_once('=') {
                        Some((kind_name, arg)) => (kind_name, Some(arg)),
                        None => (flag, None),
                    };

                    match (kind_name, arg) {
                        #( #handle_parse )*
                        (kind_name, Some(arg)) => Err(AsmErrorKind::UnexpectedWarningArg(kind_name.to_string())),
                        (kind_name, None) => Err(AsmErrorKind::UnknownWarningFlag(kind_name.to_string())),
                    }
                }
            }
        })
    }

    fn to_kebab_case(original: &str) -> String {
        let mut new = String::with_capacity(original.len());
        let mut chars = original.chars();

        if let Some(first) = chars.next() {
            new.push(first.to_ascii_lowercase());
            for c in chars {
                if c.is_ascii_uppercase() {
                    new.push('-');
                }
                new.push(c.to_ascii_lowercase());
            }
        }
        new
    }

    struct Warning {
        name: Ident,
        kind: WarningKind,
        pat_kind: PatKind,
    }

    #[derive(Debug)]
    enum WarningKind {
        Boolean(bool),
        Numeric { default: u8, max: u8 },
        Meta(Vec<Ident>),
    }

    impl TryFrom<(Lit, Option<u8>)> for WarningKind {
        type Error = Error;

        fn try_from(value: (Lit, Option<u8>)) -> Result<Self, Self::Error> {
            match value {
                (Lit::Bool(lit), None) => Ok(Self::Boolean(lit.value)),
                (Lit::Int(lit), Some(max)) => lit
                    .base10_parse()
                    .map(|default| Self::Numeric { default, max }),
                (default, _) => Err(Error::new_spanned(
                    default,
                    "Warning default must either be a boolean or a decimal number",
                )),
            }
        }
    }

    #[derive(Debug)]
    struct WarningId<'a> {
        base_ident: &'a Ident,
        ident_suffix: Option<u8>,
        pat_kind: PatKind,
        default: bool,
    }

    #[derive(Debug, Clone, Copy)]
    enum PatKind {
        Unit,
        Tuple,
        Struct,
    }

    impl From<&Fields> for PatKind {
        fn from(value: &Fields) -> Self {
            match value {
                Fields::Named(_) => Self::Struct,
                Fields::Unnamed(_) => Self::Tuple,
                Fields::Unit => Self::Unit,
            }
        }
    }

    impl ToTokens for PatKind {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            let delimiter = match self {
                PatKind::Unit => return,
                PatKind::Tuple => Delimiter::Parenthesis,
                PatKind::Struct => Delimiter::Brace,
            };
            tokens.append(Group::new(
                delimiter,
                <Token![..]>::default().to_token_stream(),
            ));
        }
    }

    impl WarningId<'_> {
        fn pat(&self) -> TokenStream {
            let ident = self.base_ident;
            let pat_kind = self.pat_kind;

            match self.ident_suffix {
                Some(level) => {
                    quote! { #ident { level: #level, .. } }
                }
                None => {
                    quote! { #ident #pat_kind }
                }
            }
        }
    }

    impl Display for WarningId<'_> {
        fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
            let ident_name = format!("{}", self.base_ident);
            // Convert the name to "kebab-case".
            let mut ident_chars = ident_name.chars();
            f.write_char(
                ident_chars
                    .next()
                    .expect("Enum variant has an empty name!?")
                    .to_ascii_lowercase(),
            )?;
            for ch in ident_chars {
                if ch.is_ascii_uppercase() {
                    f.write_char('-')?;
                    f.write_char(ch.to_ascii_lowercase())?;
                } else {
                    f.write_char(ch)?;
                }
            }

            if let Some(suffix) = self.ident_suffix {
                write!(f, "={suffix}")
            } else {
                Ok(())
            }
        }
    }

    impl ToTokens for WarningId<'_> {
        fn to_tokens(&self, tokens: &mut TokenStream) {
            match self.ident_suffix {
                None => self.base_ident.to_tokens(tokens),
                Some(suffix) => Ident::new_raw(
                    &format!("{}{suffix}", self.base_ident.unraw()),
                    Span::call_site(),
                )
                .to_tokens(tokens),
            }
        }
    }

    fn parse_input_attrs(input: &DeriveInput) -> syn::Result<Ident> {
        let mut id_enum_name = None;

        fn parse_ident(lit: Lit) -> syn::Result<Ident> {
            match lit {
                Lit::Str(name) => Ok(Ident::new_raw(&name.value(), name.span())),
                _ => Err(Error::new_spanned(lit, "Expected a string")),
            }
        }

        for attr in input
            .attrs
            .iter()
            .filter(|attr| attr.path.is_ident("warning"))
        {
            let Meta::List(args) = attr.parse_meta()? else {
                return Err(Error::new_spanned(
                    attr,
                    "Expected `#[warning(list = \"of args\")]`",
                ));
            };
            for arg in args.nested {
                match arg {
                    NestedMeta::Meta(Meta::NameValue(name_val)) => {
                        match name_val.path.get_ident() {
                            Some(name) if name == "id_enum" => {
                                id_enum_name = Some(parse_ident(name_val.lit)?);
                            }
                            _ => return Err(Error::new_spanned(name_val.path, "Unknown argument")),
                        }
                    }
                    arg => return Err(Error::new_spanned(arg, "Unknown argument")),
                }
            }
        }

        match id_enum_name {
            Some(id_enum_name) => Ok(id_enum_name),
            None => Err(Error::new_spanned(
                input,
                "Missing `#[warning(id_enum = \"EnumName\")]`",
            )),
        }
    }

    fn parse_variant(variant: Variant) -> syn::Result<Warning> {
        let mut default = None;
        let mut max = None;
        let mut meta = None;

        for attr in variant
            .attrs
            .iter()
            .filter(|attr| attr.path.is_ident("warning"))
        {
            let Meta::List(args) = attr.parse_meta()? else {
                return Err(Error::new_spanned(
                    attr,
                    "Expected `#[warning(list = \"of args\")]`",
                ));
            };
            for arg in args.nested {
                match arg {
                    NestedMeta::Meta(Meta::NameValue(name_val)) => {
                        match name_val.path.get_ident() {
                            Some(name) if name == "default" => default = Some(name_val.lit),
                            Some(name) if name == "max" => {
                                max = Some(match name_val.lit {
                                    Lit::Int(int) => int.base10_parse()?,
                                    lit => {
                                        return Err(Error::new_spanned(
                                            lit,
                                            "Argument to `max =` must be a decimal number",
                                        ));
                                    }
                                });
                            }
                            _ => return Err(Error::new_spanned(name_val, "Unknown argument")),
                        }
                    }
                    NestedMeta::Meta(Meta::List(list)) => match list.path.get_ident() {
                        Some(name) if name == "meta" => {
                            meta = Some(parse_meta_warning_list(list.nested.iter())?);
                        }
                        _ => return Err(Error::new_spanned(list.path, "Unknown argument")),
                    },
                    _ => {
                        return Err(Error::new_spanned(
                            arg,
                            "Expected #[warning(list = \"of args\")]",
                        ))
                    }
                }
            }
        }

        let pat_kind = (&variant.fields).into();

        match (default, meta) {
            (Some(default), None) => Ok(Warning {
                name: variant.ident,
                kind: (default, max).try_into()?,
                pat_kind,
            }),
            (None, Some(meta)) => Ok(Warning {
                name: variant.ident,
                kind: WarningKind::Meta(meta),
                pat_kind,
            }),
            (None, None) => Err(Error::new_spanned(
                variant,
                "Missing `#[warning(default = ...)]` or `#[warning(meta(...))]`",
            )),
            (Some(_), Some(_)) => Err(Error::new_spanned(
                variant,
                "`#[warning(default = ...)]` and `#[warning(meta(...))]` are mutually exclusive",
            )),
        }
    }

    fn parse_meta_warning_list<'a, It: Iterator<Item = &'a NestedMeta>>(
        iter: It,
    ) -> syn::Result<Vec<Ident>> {
        iter.map(|nested| {
            match nested {
                NestedMeta::Meta(Meta::Path(path)) => path.get_ident().cloned(), // TODO: boo, `cloned()`
                _ => None,
            }
            .ok_or_else(|| {
                Error::new_spanned(
                    nested,
                    "`#[warning(meta(...))]` must contain a list of warning identifiers",
                )
            })
        })
        .collect()
    }
}
