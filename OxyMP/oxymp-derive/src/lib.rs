mod attribute;
mod grammar;
mod lexer;

use std::collections::HashMap;

use attribute::{AttributeNameValue, NameValue};
use grammar::{aggragate_rules, RawGrammarRule};
use quote::{quote, ToTokens};
use syn::spanned::Spanned;

use crate::lexer::{Lexer, TokenInfo};

fn group_attrs(attrs: &Vec<syn::Attribute>) -> HashMap<String, Vec<proc_macro2::TokenStream>> {
    return attrs.iter().fold(HashMap::new(), |mut acc, attr| {
        let attr_ident = attr.path().segments.first().unwrap().ident.to_string();

        acc.entry(attr_ident)
            .or_insert_with(Vec::new)
            .extend(vec![attr.to_token_stream()]);

        return acc;
    });
}

fn parse_token_attrs(
    attr_groups: HashMap<String, Vec<proc_macro2::TokenStream>>,
) -> syn::Result<Vec<TokenInfo>> {
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

    return Ok(token_info);
}

fn parse_grammar_attrs(
    grammar_attrs: Vec<proc_macro2::TokenStream>,
) -> syn::Result<Vec<RawGrammarRule>> {
    let mut rules = Vec::new();

    for attr in grammar_attrs {
        let AttributeNameValue(NameValue { name, value }) = syn::parse2(attr)?;
        match grammar::new_rule(&value) {
            Ok(rule) => rules.push(rule),
            Err(err) => return Err(syn::Error::new(name.span, err.to_string())),
        }
    }

    return Ok(rules);
}

fn derive_impl(input: proc_macro::TokenStream) -> syn::Result<proc_macro2::TokenStream> {
    let ast: syn::DeriveInput = syn::parse(input)?;

    let mut attr_groups = group_attrs(&ast.attrs);

    let grammar_attrs = match attr_groups.remove("grammar") {
        Some(v) => v,
        None => return Err(syn::Error::new(ast.span(), "Missing grammar rules.")),
    };
    let token_info = parse_token_attrs(attr_groups)?;
    let grammar_rules = parse_grammar_attrs(grammar_attrs)?;
    let grammar_rules = aggragate_rules(grammar_rules, &token_info);
    eprintln!("{:#?}", grammar_rules);

    let lexer = Lexer::generate(&token_info);

    let output = quote! {
        #lexer
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
