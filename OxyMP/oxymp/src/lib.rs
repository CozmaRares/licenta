use std::collections::HashMap;

use quote::{quote, ToTokens};
use syn::{parse::Parse, parse_macro_input};

mod attribute;
use crate::attribute::*;

fn group_attrs(attrs: &Vec<syn::Attribute>) -> HashMap<String, Vec<proc_macro2::TokenStream>> {
    return attrs.iter().fold(HashMap::new(), |mut acc, attr| {
        let attr_ident = attr.path().segments.first().unwrap().ident.to_string();

        acc.entry(attr_ident)
            .or_insert_with(Vec::new)
            .extend(vec![attr.to_token_stream()]);

        return acc;
    });
}

#[proc_macro_derive(
    RecursiveDescent,
    attributes(exact_token, regex_token, ignore_pattern, grammar)
)]
pub fn derive(item: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let syn::DeriveInput { attrs, .. } = syn::parse(item).unwrap();

    let groups = group_attrs(&attrs);

    for (key, attrs) in groups {
        for attr in attrs {
            if &key == "grammar" {
                let parsed: AttributeKeyValue = match syn::parse2(attr) {
                    Ok(o) => o,
                    Err(e) => return e.to_compile_error().into(),
                };
                eprintln!("{:#?}", parsed);
            } else {
                let parsed: AttributeList = match syn::parse2(attr) {
                    Ok(o) => o,
                    Err(e) => return e.to_compile_error().into(),
                };
                eprintln!("{:#?}", parsed);
            }
        }
    }

    let output = quote! {};
    output.into()
}
