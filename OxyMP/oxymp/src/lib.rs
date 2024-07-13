use std::collections::{HashMap, HashSet};

use proc_macro::{self};
use proc_macro2::TokenStream;
use quote::quote;
use syn::{parse_macro_input, Attribute, DeriveInput};

fn group_attrs(
    attrs: Vec<Attribute>,
    accepted: &HashSet<String>,
) -> Result<HashMap<String, Vec<TokenStream>>, syn::Error> {
    let mut grouped_attrs: HashMap<String, Vec<TokenStream>> = HashMap::new();

    for attr in attrs {
        let segments = &attr.path.segments;

        if segments.len() != 1 {
            let ident = &segments.iter().nth(1).unwrap().ident;

            return Err(syn::Error::new(
                ident.span(),
                "expected only one path segment",
            ));
        }

        let ident = &segments.first().unwrap().ident;
        let ident_str = ident.to_string();

        if accepted.contains(&ident_str) {
            grouped_attrs
                .entry(ident_str)
                .or_insert_with(Vec::new)
                .extend(vec![attr.tokens]);
        }
    }

    return Ok(grouped_attrs);
}

#[proc_macro_derive(RecursiveDescent, attributes(grammar, token))]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let DeriveInput { attrs, .. } = parse_macro_input!(input);

    let mut set = HashSet::new();
    set.insert("token".to_string());
    set.insert("grammar".to_string());

    let groups = match group_attrs(attrs, &set) {
        Ok(g) => g,
        Err(e) => return e.to_compile_error().into(),
    };

    eprintln!("{:#?}", groups);

    let output = quote! {};
    output.into()
}
