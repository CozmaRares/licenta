mod attribute;
mod lexer;

use std::collections::HashMap;

use quote::{quote, ToTokens};

use crate::lexer::*;

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

    let mut groups = group_attrs(&attrs);

    for attr in groups.remove("exact_token").unwrap() {
        let token = Token::exact_token(attr.to_owned())?;
        eprintln!("{:#?}", token);
    }

    for attr in groups.remove("regex_token").unwrap() {
        let token = Token::regex_token(attr.to_owned())?;
        eprintln!("{:#?}", token);
    }

    for attr in groups.remove("ignore_pattern").unwrap() {
        let token = Token::ignore_pattern(attr.to_owned())?;
        eprintln!("{:#?}", token);
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
