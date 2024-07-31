use std::collections::{HashMap, HashSet};

use quote::{quote, ToTokens};
use syn::{
    parse::{Parse, ParseStream, Peek},
    parse_macro_input,
    punctuated::Punctuated,
    spanned::Spanned,
};

fn group_attrs(
    attrs: &Vec<syn::Attribute>,
    groups: &HashSet<String>,
) -> HashMap<String, Vec<syn::Meta>> {
    return attrs.iter().fold(HashMap::new(), |mut acc, attr| {
        let attr_ident = attr.path().segments.first().unwrap().ident.to_string();

        if groups.contains(&attr_ident) {
            acc.entry(attr_ident)
                .or_insert_with(Vec::new)
                .extend(vec![attr.meta.clone()]);
        }

        return acc;
    });
}

#[derive(Debug)]
struct KeyValue {
    name: syn::Ident,
    value: syn::LitStr,
}

impl Parse for KeyValue {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name: syn::Ident = input.parse()?;
        let _eq: syn::Token![=] = input.parse()?;
        let value: syn::LitStr = input.parse()?;
        Ok(KeyValue { name, value })
    }
}

type KeyValueList = Punctuated<KeyValue, syn::Token![,]>;

struct Attrib {
    attr: syn::Ident,
    values: KeyValueList,
}

impl Parse for Attrib {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        todo!();
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

    let a = groups.get("exact_token").unwrap().first().unwrap();

    let a = match a {
        syn::Meta::List(meta) => meta.tokens.clone().into(),
        _ => unreachable!(),
    };

    eprintln!("{:#?}", a);

    let b = parse_macro_input!(a with KeyValueList::parse_terminated);

    eprintln!("{:#?}", b);

    let output = quote! {};
    output.into()
}
