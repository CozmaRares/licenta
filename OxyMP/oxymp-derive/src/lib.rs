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

use attribute::parse_grammar_attribute;
use data::{attributes::MacroAttributes, MacroData};
use grammar::{aggragate_grammar_rules, new_grammar_rule, GrammarNode};
use syn::spanned::Spanned;

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

    let macro_attributes = MacroAttributes::new(ast.span(), &ast.attrs)?;

    let data = MacroData::new(
        ast,
        macro_attributes.exact_patterns,
        macro_attributes.regex_patterns,
        macro_attributes.ignore_patterns,
        macro_attributes.simple_types,
        macro_attributes.depth_limit,
    )?;

    let grammar_rules = parse_grammar_attrs(macro_attributes.grammar, &data)?;

    Ok(generation::generate(data, grammar_rules))
}

#[proc_macro_derive(
    RecursiveDescent,
    attributes(
        exact_pattern,
        regex_pattern,
        ignore_pattern,
        grammar,
        simple_types,
        depth_limit,
        sync_tokens
    )
)]
pub fn derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    match derive_impl(input) {
        Ok(o) => o,
        Err(e) => e.to_compile_error(),
    }
    .into()
}
