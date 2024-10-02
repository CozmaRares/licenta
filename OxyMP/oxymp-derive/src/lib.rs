mod attribute;
mod grammar;
mod idents;
mod lexer;
mod parser;
mod tokens;

use std::{collections::HashMap, rc::Rc};

use attribute::{
    parse_exact_token, parse_grammar_attribute, parse_ignore_pattern, parse_regex_token,
};
use grammar::{aggragate_grammar_rules, new_grammar_rule, RawGrammarRule};
use quote::{quote, ToTokens};
use syn::spanned::Spanned;

use crate::tokens::TokenInfo;

fn group_attrs(attrs: &Vec<syn::Attribute>) -> HashMap<Rc<str>, Vec<proc_macro2::TokenStream>> {
    return attrs.iter().fold(HashMap::new(), |mut acc, attr| {
        let attr_ident = attr
            .path()
            .segments
            .first()
            .unwrap()
            .ident
            .to_string()
            .into();

        acc.entry(attr_ident)
            .or_insert_with(Vec::new)
            .extend(vec![attr.to_token_stream()]);

        return acc;
    });
}

fn parse_token_attrs(
    attr_groups: HashMap<Rc<str>, Vec<proc_macro2::TokenStream>>,
) -> syn::Result<Vec<TokenInfo>> {
    type CreateTokenInfo = dyn Fn(proc_macro2::TokenStream) -> syn::Result<TokenInfo>;
    let mut token_type_handlers: HashMap<&str, &CreateTokenInfo> = HashMap::new();
    token_type_handlers.insert("exact_token", &parse_exact_token);
    token_type_handlers.insert("regex_token", &parse_regex_token);
    token_type_handlers.insert("ignore_pattern", &parse_ignore_pattern);

    let mut token_info = Vec::new();

    for (attr, streams) in attr_groups {
        for stream in streams {
            match token_type_handlers.get(&*attr) {
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
        let (name_span, rule) = parse_grammar_attribute(attr)?;
        match new_grammar_rule(&*rule) {
            Ok(rule) => rules.push(rule),
            Err(err) => return Err(syn::Error::new(name_span, err.to_string())),
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

    let grammar_rules = aggragate_grammar_rules(grammar_rules, &token_info);
    let parser = parser::generate_parser(&ast.ident, &grammar_rules);

    let lexer = lexer::generate_lexer(&token_info);

    let output = quote! {
        #lexer
        #parser
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
