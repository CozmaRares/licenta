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

fn derive_impl(input: proc_macro::TokenStream) -> syn::Result<proc_macro::TokenStream> {
    let syn::DeriveInput { attrs, .. } = syn::parse(input).unwrap();

    let groups = group_attrs(&attrs);

    for (key, attrs) in groups {
        for attr in attrs {
            if &key == "grammar" {
                let parsed: AttributeKeyValue = syn::parse2(attr)?;
                eprintln!("{:#?}", parsed);
            } else {
                let parsed: AttributeList = syn::parse2(attr)?;
                eprintln!("{:#?}", parsed);
            }
        }
    }

    let output = quote! {};
    Ok(output.into())
}

#[proc_macro_derive(
    RecursiveDescent,
    attributes(exact_token, regex_token, ignore_pattern, grammar)
)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    match derive_impl(input) {
        Ok(o) => o,
        Err(e) => e.to_compile_error().into(),
    }
}
