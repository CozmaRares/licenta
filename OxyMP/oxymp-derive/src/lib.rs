#[macro_use]
mod macros;

mod attribute;
mod data;
mod generation;
mod grammar;
mod idents;
mod symbols;
mod tokens;

use std::{collections::HashMap, rc::Rc};

use attribute::{
    parse_depth_limit_attr, parse_exact_token, parse_grammar_attribute, parse_ignore_pattern,
    parse_regex_token,
};
use data::MacroData;
use grammar::{aggragate_grammar_rules, new_grammar_rule, GrammarNode};
use quote::ToTokens;
use syn::spanned::Spanned;

use crate::tokens::TokenInfo;

fn group_attrs(attrs: &[syn::Attribute]) -> HashMap<Rc<str>, Vec<proc_macro2::TokenStream>> {
    attrs.iter().fold(HashMap::new(), |mut acc, attr| {
        let attr_ident = attr
            .path()
            .segments
            .first()
            .unwrap()
            .ident
            .to_string()
            .into();

        acc.entry(attr_ident)
            .or_default()
            .extend(vec![attr.to_token_stream()]);

        acc
    })
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
            if let Some(handler) = token_type_handlers.get(&*attr) {
                token_info.push(handler(stream)?)
            }
        }
    }

    Ok(token_info)
}

fn parse_grammar_attrs(
    grammar_attrs: Vec<proc_macro2::TokenStream>,
    data: &MacroData,
) -> syn::Result<HashMap<Rc<str>, GrammarNode>> {
    let mut rules = Vec::new();

    for attr in grammar_attrs {
        let (name_span, rule) = parse_grammar_attribute(attr)?;
        match new_grammar_rule(&rule) {
            Ok(rule) => rules.push(rule),
            Err(err) => return Err(syn::Error::new(name_span, err.to_string())),
        }
    }

    match aggragate_grammar_rules(rules, data) {
        Ok(grammar_rules) => Ok(grammar_rules),
        Err(err) => Err(syn::Error::new(data.parser_ident.span(), err)),
    }
}

fn derive_impl(input: proc_macro::TokenStream) -> syn::Result<proc_macro2::TokenStream> {
    let ast: syn::DeriveInput = syn::parse(input)?;

    let mut attr_groups = group_attrs(&ast.attrs);
    let grammar_attrs = match attr_groups.remove("grammar") {
        Some(v) => v,
        None => return Err(syn::Error::new(ast.span(), "Missing grammar rules.")),
    };

    let simple_types = match attr_groups.remove("simple_types") {
        Some(v) => match v.len() {
            0 => false,
            1 => true,
            _ => {
                return Err(syn::Error::new(
                    ast.span(),
                    "Multiple '#[simple_types]' attributes.",
                ))
            }
        },
        None => false,
    };

    let depth_limit = match attr_groups.remove("depth_limit") {
        None => 10,
        Some(mut v) => match v.len() {
            0 => 10,
            1 => {
                let v = v.remove(0);
                parse_depth_limit_attr(v)?
            }
            _ => {
                return Err(syn::Error::new(
                    ast.span(),
                    "Multiple '#[depth_limit]' attributes.",
                ))
            }
        },
    };

    let token_info = parse_token_attrs(attr_groups)?;

    let mut vis_toks = proc_macro2::TokenStream::new();
    ast.vis.to_tokens(&mut vis_toks);

    let data = MacroData {
        tokens: token_info,
        parser_ident: ast.ident,
        visibility: vis_toks,
        simple_types,
        depth_limit,
    };

    let grammar_rules = parse_grammar_attrs(grammar_attrs, &data)?;

    Ok(generation::generate(data, grammar_rules))
}

#[proc_macro_derive(
    RecursiveDescent,
    attributes(
        exact_token,
        regex_token,
        ignore_pattern,
        grammar,
        simple_types,
        depth_limit
    )
)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    match derive_impl(input) {
        Ok(o) => o,
        Err(e) => e.to_compile_error(),
    }
    .into()
}
