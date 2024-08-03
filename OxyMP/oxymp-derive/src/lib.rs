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

fn derive_impl(input: proc_macro::TokenStream) -> syn::Result<proc_macro2::TokenStream> {
    let syn::DeriveInput { attrs, .. } = syn::parse(input).unwrap();

    let mut attr_groups = group_attrs(&attrs);

    let _grammar = attr_groups.remove("grammar");

    type CreateTokenInfo = dyn Fn(proc_macro2::TokenStream) -> syn::Result<TokenInfo>;
    let mut token_type_handlers: HashMap<String, &CreateTokenInfo> = HashMap::new();
    token_type_handlers.insert("exact_token".to_string(), &TokenInfo::exact_token);
    token_type_handlers.insert("regex_token".to_string(), &TokenInfo::regex_token);
    token_type_handlers.insert("ignore_pattern".to_string(), &TokenInfo::ignore_pattern);

    let mut token_info = Vec::new();

    for (attr, streams) in attr_groups {
        for stream in streams {
            match token_type_handlers.get(&attr) {
                Some(handler) => token_info.push(handler(stream)?),
                None => {}
            }
        }
    }

    eprintln!("{:#?}", token_info);

    let lexer_def = Lexer::generate_def();
    let tokens_def = Lexer::generate_tokens(&token_info);

    let output = quote! {
        pub mod lexer {
            #lexer_def

            #tokens_def
        }
    };

    return Ok(output);
}

#[proc_macro_derive(
    RecursiveDescent,
    attributes(exact_token, regex_token, ignore_pattern, grammar)
)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    return match derive_impl(input) {
        Ok(o) => o,
        Err(e) => e.to_compile_error(),
    }
    .into();
}
