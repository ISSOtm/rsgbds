/* SPDX-License-Identifier: MPL-2.0 */

use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, AttributeArgs, ItemEnum};

#[proc_macro_attribute]
pub fn keyword_trie(attr: TokenStream, item: TokenStream) -> TokenStream {
    let args = parse_macro_input!(attr as AttributeArgs);
    let input = parse_macro_input!(item as ItemEnum);

    let trie = impls::trie(args, &input).unwrap_or_else(syn::Error::into_compile_error);
    quote! {
        #input
        #trie
    }
    .into()
}

mod impls {
    use proc_macro2::{Literal, Span, TokenStream};
    use quote::{format_ident, quote};
    use syn::{
        AttributeArgs, Error, Fields, Ident, ItemEnum, Lit, LitStr, Meta, MetaNameValue, NestedMeta,
    };

    #[derive(Debug)]
    struct TrieNode {
        children: Vec<usize>,
        value: Option<Ident>,
    }

    impl TrieNode {
        pub fn new(nb_children: usize) -> Self {
            Self {
                children: vec![0; nb_children],
                value: None,
            }
        }
    }

    pub(crate) fn trie(args: AttributeArgs, input: &ItemEnum) -> Result<TokenStream, Error> {
        let (trie_name, ty_name, chars) = parse_trie_args(&args)?;
        let mod_name = Ident::new(&format!("__trie_impl_{}", trie_name), trie_name.span());
        let input_name = &input.ident;
        let vis = &input.vis;
        let chars_string = chars.value();
        let nb_chars = chars_string.chars().count();

        if chars_string.chars().any(|c| c.is_ascii_lowercase()) {
            return Err(Error::new_spanned(
                chars,
                "Keyword charset cannot contain lowercase characters",
            ));
        }

        let indices = (0..nb_chars)
            .map(|i| format_ident!("Char{i}"))
            .collect::<Vec<_>>();
        let index_docs = chars_string.chars().map(|ch| format!("`'{ch}'`"));

        let chars_iter = chars_string.chars();

        let keywords = input
            .variants
            .iter()
            .filter(|variant| matches!(variant.fields, Fields::Unit) && variant.attrs.is_empty())
            .map(|variant| &variant.ident)
            .collect::<Vec<_>>();

        // Generate the trie.
        let mut nodes = vec![TrieNode::new(nb_chars)];
        for keyword in &keywords {
            let mut cur_node = 0;
            for ch in format!("{keyword}").chars() {
                let upper = ch.to_ascii_uppercase();
                let Some(res) = chars_string.chars().enumerate().find(|(_, c)| *c == upper) else {
                    return Err(Error::new_spanned(
                        keyword,
                        "Keyword contains a character not in the character set",
                    ));
                };
                let index = res.0;

                cur_node = match nodes[cur_node].children[index] {
                    0 => {
                        let idx = nodes.len();
                        nodes[cur_node].children[index] = idx;
                        nodes.push(TrieNode::new(nb_chars));
                        idx
                    }
                    idx => idx,
                };
            }
            nodes[cur_node].value = Some((*keyword).clone());
        }
        let nb_nodes = nodes.len();
        let nodes = nodes.iter().map(|node| {
            let children = node
                .children
                .iter()
                .map(|child| Literal::u16_suffixed((*child).try_into().unwrap()));
            let value = node
                .value
                .as_ref()
                .map(|value| {
                    quote! {
                        Some(#ty_name::#value)
                    }
                })
                .unwrap_or_else(|| quote! { None });

            quote! {
                TrieNode {
                    children: [ #( ::core::num::NonZeroU16::new(#children), )* ],
                    value: #value,
                }
            }
        });

        Ok(quote! {
            #[allow(non_snake_case)]
            mod #mod_name {
                #[derive(Debug, Clone, Copy)]
                #vis enum TrieIndex {
                    #( #[doc = #index_docs] #indices, )*
                }

                impl ::core::convert::TryFrom<char> for TrieIndex {
                    type Error = char;

                    fn try_from(ch: char) -> Result<Self, Self::Error> {
                        match ch.to_ascii_uppercase() {
                            #( #chars_iter => Ok(Self::#indices), )*
                            _ => Err(ch),
                        }
                    }
                }

                #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
                #vis enum #ty_name {
                    #( #keywords, )*
                }

                impl ::core::convert::From<#ty_name> for super::#input_name {
                    fn from(keyword: #ty_name) -> Self {
                        match keyword {
                            #( #ty_name::#keywords => Self::#keywords, )*
                        }
                    }
                }

                #[derive(Debug)]
                struct TrieNode {
                    children: [::core::option::Option<::core::num::NonZeroU16>; #nb_chars],
                    value: ::core::option::Option<#ty_name>,
                }

                #[derive(Debug)]
                #vis struct Trie([TrieNode; #nb_nodes]);

                #vis static #trie_name: Trie = Trie([
                    #( #nodes, )*
                ]);

                #vis struct TrieIter<'trie>(&'trie Trie, usize);

                impl<'trie> TrieIter<'trie> {
                    #vis fn new(trie: &'trie Trie) -> Self {
                        Self(trie, 0)
                    }

                    #vis fn next(self, idx: TrieIndex) -> ::core::option::Option<Self> {
                            self.0.0[self.1].children[idx as usize]
                                .map(|node_index| Self(self.0, usize::from(u16::from(node_index))))
                    }

                    #vis fn done(self) -> ::core::option::Option<#ty_name> {
                        self.0.0[self.1].value
                    }
                }

                impl ::core::fmt::Debug for TrieIter<'_> {
                    fn fmt(&self, f: &mut ::core::fmt::Formatter<'_>) -> ::core::fmt::Result {
                        write!(f, "TrieIter(Trie {{ ... }}, {:?})", self.1)
                    }
                }
            }
            #vis use #mod_name::*;
        })
    }

    fn parse_trie_args(args: &AttributeArgs) -> Result<(Ident, Ident, &LitStr), Error> {
        let mut errors: Option<Error> = None;
        let mut report = |err| {
            if let Some(error) = &mut errors {
                error.combine(err);
            } else {
                errors = Some(err);
            }
        };
        let mut trie_name = None;
        let mut ty_name = None;
        let mut chars = None;

        for arg in args {
            if let NestedMeta::Meta(Meta::NameValue(MetaNameValue {
                path,
                lit: Lit::Str(string),
                ..
            })) = arg
            {
                if path.is_ident("trie_name") {
                    if trie_name.is_some() {
                        report(Error::new_spanned(
                            arg,
                            "`trie_name` argument specified twice",
                        ));
                    } else {
                        match string.parse::<Ident>() {
                            Ok(ident) => trie_name = Some(ident),
                            Err(err) => report(err),
                        }
                    }
                } else if path.is_ident("ty_name") {
                    if ty_name.is_some() {
                        report(Error::new_spanned(
                            arg,
                            "`ty_name` argument specified twice",
                        ));
                    } else {
                        match string.parse::<Ident>() {
                            Ok(ident) => ty_name = Some(ident),
                            Err(err) => report(err),
                        }
                    }
                } else if path.is_ident("chars") {
                    if chars.is_some() {
                        report(Error::new_spanned(arg, "`chars` argument specified twice"));
                    } else {
                        chars = Some(string);
                    }
                } else {
                    report(Error::new_spanned(path, "Unknown argument"));
                }
            } else {
                report(Error::new_spanned(
                    arg,
                    "#[trie] arguments must all be key-string value pairs",
                ));
            }
        }

        if trie_name.is_none() {
            report(Error::new(
                Span::call_site(),
                "Missing `trie_name` argument",
            ));
        }
        if ty_name.is_none() {
            report(Error::new(Span::call_site(), "Missing `ty_name` argument"));
        }
        if chars.is_none() {
            report(Error::new(Span::call_site(), "Missing `chars` argument"));
        }
        if let Some(errors) = errors {
            Err(errors)
        } else {
            Ok((trie_name.unwrap(), ty_name.unwrap(), chars.unwrap()))
        }
    }
}
