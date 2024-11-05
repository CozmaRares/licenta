#![allow(non_snake_case)]

use std::{collections::HashMap, rc::Rc};

use proc_macro2::TokenStream;
use quote::quote;

use crate::{
    data::MacroData,
    grammar::{GrammarNode, GrammarNodeContent},
    idents::{parser, tokens},
    symbols::{get_def, Symbol},
};

pub(crate) fn generate_ast(data: &MacroData, rules: &HashMap<Rc<str>, GrammarNode>) -> TokenStream {
    let visibility = &data.visibility;

    let _Debug = get_def(Symbol::DeriveDebug, data.simple_types);

    let structs = rules.iter().map(|(rule, node)| {
        let ASTNode {
            main_struct,
            external_choices,
        } = generate_ast_node(rule, node, data);

        let external_choices = match external_choices {
            None => quote! {},
            Some(structs) => quote! { #(#structs)* },
        };

        let rule_ident = parser::rule_ident(rule);

        quote! {
            #external_choices
            #[derive(#_Debug)]
            #visibility struct #rule_ident (
                #visibility #main_struct
            );
        }
    });

    quote! {
        #(#structs)*
    }
}

struct ASTNode {
    main_struct: TokenStream,
    external_choices: Option<Vec<TokenStream>>,
}

fn generate_ast_node(rule: &str, node: &GrammarNode, data: &MacroData) -> ASTNode {
    let visibility = &data.visibility;

    let _Box = get_def(Symbol::Box, data.simple_types);
    let _Debug = get_def(Symbol::DeriveDebug, data.simple_types);
    let _Option = get_def(Symbol::Option, data.simple_types);

    match &node.content {
        GrammarNodeContent::Rule(rule) => {
            let ident = parser::rule_ident(rule);
            ASTNode {
                main_struct: quote! {#_Box<#ident>},
                external_choices: None,
            }
        }
        GrammarNodeContent::Token(token) => {
            let ident = tokens::struct_ident(token);
            ASTNode {
                main_struct: quote! { #ident },
                external_choices: None,
            }
        }
        GrammarNodeContent::List(exprs) => {
            let defs = exprs.iter().map(|expr| generate_ast_node(rule, expr, data));
            let main_struct = defs.clone().map(|d| d.main_struct);
            let external_choices = defs.filter_map(|d| d.external_choices).flatten();

            ASTNode {
                main_struct: quote! { ( #(#main_struct),* ) },
                external_choices: Some(external_choices.collect()),
            }
        }
        GrammarNodeContent::Choice(choices, choice_idx) => {
            let defs = choices
                .iter()
                .map(|choice| generate_ast_node(rule, choice, data));
            let enum_entries = defs
                .clone()
                .map(|d| d.main_struct)
                .enumerate()
                .map(|(idx, s)| {
                    let idx_ident = parser::idx_ident(idx + 1);
                    quote! {
                        #idx_ident(#s)
                    }
                });

            let mut external_choices: Vec<_> =
                defs.filter_map(|d| d.external_choices).flatten().collect();
            let enum_ident = parser::choice_ident(rule, *choice_idx);
            external_choices.push(quote! {
                #[derive(#_Debug)]
                #visibility enum #enum_ident {
                    #(#enum_entries),*
                }
            });

            ASTNode {
                main_struct: quote! { #enum_ident },
                external_choices: Some(external_choices),
            }
        }
        GrammarNodeContent::Optional(opt) => {
            let generated = generate_ast_node(rule, opt, data);
            let main_struct = generated.main_struct;
            ASTNode {
                main_struct: quote! { #_Option<#main_struct> },
                external_choices: generated.external_choices,
            }
        }
    }
}
