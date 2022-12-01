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

        let DeriveInput { data: Data::Enum(DataEnum { variants, .. }), ident : input_name, .. } = input else {
            return Err(Error::new_spanned(input, "#[derive(Warnings)] can only be applied to an enum"));
        };

        let warnings = variants
            .into_iter()
            .map(parse_variant)
            .collect::<Result<Vec<_>, _>>()?;

        let mut warning_ids = Vec::with_capacity(warnings.len());
        for warning in &warnings {
            let base_ident = &warning.name;
            let pat_kind = warning.pat_kind;

            match warning.kind {
                WarningKind::Boolean(default) => {
                    warning_ids.push(WarningId {
                        base_ident,
                        ident_suffix: None,
                        pat_kind,
                        default,
                    });
                }
                WarningKind::Numeric { max, default } => {
                    warning_ids.extend((1..=max).map(move |i| WarningId {
                        base_ident,
                        ident_suffix: Some(i),
                        pat_kind,
                        default: i <= default,
                    }));
                }
                WarningKind::Meta(..) => {}
            }
        }

        let vis = input.vis;
        let nb_warnings = warning_ids.len();
        let id_flags = warning_ids.iter().map(|id| format!("`-W{id}`"));
        let defaults = warning_ids.iter().map(|id| id.default);
        let patterns = warning_ids.iter().map(WarningId::pat);
        let id_strings = warning_ids.iter().map(|id| format!("{id}"));
        Ok(quote! {
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
        })
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
                return Err(Error::new_spanned(attr, "Expected `#[warning(list = \"of args\")]`"));
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
                return Err(Error::new_spanned(attr, "Expected `#[warning(list = \"of args\")]`"));
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
