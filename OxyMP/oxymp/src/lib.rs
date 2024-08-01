use std::collections::{HashMap, HashSet};

use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream},
    parse_macro_input,
    punctuated::Punctuated,
    spanned::Spanned,
};

fn group_attrs(
    attrs: &Vec<syn::Attribute>,
    groups: &HashSet<String>,
) -> HashMap<String, Vec<proc_macro::TokenStream>> {
    return attrs.iter().fold(HashMap::new(), |mut acc, attr| {
        let attr_ident = attr.path().segments.first().unwrap().ident.to_string();

        if groups.contains(&attr_ident) {
            acc.entry(attr_ident)
                .or_insert_with(Vec::new)
                .extend(vec![attr.to_token_stream().into()]);
        }

        return acc;
    });
}

#[derive(Debug)]
struct KeyValue {
    name: String,
    value: String,
}

impl Parse for KeyValue {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name: syn::Ident = input.parse()?;
        let _eq: syn::Token![=] = input.parse()?;
        let value: syn::LitStr = input.parse()?;
        Ok(KeyValue {
            name: name.to_string(),
            value: value.value(),
        })
    }
}

type KeyValueList = Punctuated<KeyValue, syn::Token![,]>;

#[derive(Debug)]
struct AttribList {
    attr: String,
    values: Vec<KeyValue>,
}

impl Parse for AttribList {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let _hash: syn::Token![#] = input.parse()?;

        let content_bracketed;
        let _bracketed = syn::bracketed!(content_bracketed in input);

        let attr: syn::Ident = content_bracketed.parse()?;

        let content_parenthesized;
        let _parenthesized = syn::parenthesized!(content_parenthesized in content_bracketed);

        let values = KeyValueList::parse_terminated(&content_parenthesized)?;

        Ok(AttribList {
            attr: attr.to_string(),
            values: values.into_iter().collect(),
        })
    }
}

#[proc_macro_derive(
    RecursiveDescent,
    attributes(exact_token, regex_token, ignore_pattern, grammar)
)]
pub fn derive_my_description(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let syn::DeriveInput { attrs, .. } = syn::parse(item).unwrap();

    //eprintln!("{:#?}", attrs);

    for attr in &attrs {
        match &attr.meta {
            syn::Meta::Path(path) => {
                let span = path.segments.first().unwrap().span();
                return syn::Error::new(
                    span,
                    "This macro only accepts attributes of type:\n - list: #[attr(a, b, c)]\n - name-value: #[attr = a]",
                ).to_compile_error().into();
            }
            _ => {}
        }
    }

    let mut set = HashSet::new();
    set.insert("exact_token".to_string());
    set.insert("regex_token".to_string());
    set.insert("ignore_pattern".to_string());
    set.insert("grammar".to_string());

    let groups = group_attrs(&attrs, &set);

    //groups.iter().for_each(|(key, meta)| {
    //    eprintln!("{:#?}\n=>\n{:#?}\n\n", key, meta);
    //});

    let a = groups
        .get("exact_token")
        .unwrap()
        .first()
        .unwrap()
        .to_owned();

    let b = parse_macro_input!(a as AttribList);
    //AttribList::parse(a).into();

    eprintln!("{:#?}", b);

    let output = quote! {};
    output.into()
}
