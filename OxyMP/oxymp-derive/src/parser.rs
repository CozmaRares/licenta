use std::collections::HashMap;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::{grammar::GrammarNode, lexer::TokenInfo};

pub fn generate_parser(rules: &HashMap<String, GrammarNode>) -> TokenStream {
    let parser_def = generate_def();
    let ast = generate_ast(rules);

    quote! {
        pub mod parser {
            #parser_def
            #ast
        }
    }
}

fn generate_def() -> TokenStream {
    quote! {
        type Inp<'a> = &'a [super::lexer::Token];
        type ParserState<'a> = ::std::result::Result<(Inp<'a>, AST), ParserError>;

        #[derive(::std::fmt::Debug)]
        pub struct ParserError {
            pub place: ::std::string::String,
            pub reason: ::std::string::String,
        }

    }
}

fn generate_ast(rules: &HashMap<String, GrammarNode>) -> TokenStream {
    let structs = rules.iter().map(|(rule, node)| {
        let ASTNode {
            main_struct,
            external_choices,
        } = generate_ast_node(rule, node);

        let external_choices = match external_choices {
            None => quote! {},
            Some(structs) => quote! { #(#structs)* },
        };

        let rule_ident = format_ident!("{}", rule);

        quote! {
            #external_choices
            #[derive(::std::fmt::Debug)]
            pub struct #rule_ident (
                #main_struct
            );
        }
    });

    let enum_entries = rules.keys().map(|rule| {
        let ident = format_ident!("{}", rule);
        quote! { #ident(#ident) }
    });

    quote! {
        #(#structs)*
        #[derive(::std::fmt::Debug)]
        enum AST {
            #(#enum_entries),*
        }
    }
}

struct ASTNode {
    main_struct: TokenStream,
    external_choices: Option<Vec<TokenStream>>,
}

fn generate_ast_node(rule: &String, node: &GrammarNode) -> ASTNode {
    match node {
        GrammarNode::Rule(rule) => {
            let ident = format_ident!("{}", rule);
            ASTNode {
                main_struct: quote! {::std::boxed::Box<#ident>},
                external_choices: None,
            }
        }
        GrammarNode::Token(token) => {
            let ident = TokenInfo::struct_ident(token);
            ASTNode {
                main_struct: quote! { super::lexer::#ident },
                external_choices: None,
            }
        }
        GrammarNode::Expr(exprs) => {
            if exprs.len() == 1 {
                return generate_ast_node(rule, exprs.first().unwrap());
            }

            let defs = exprs.iter().map(|expr| generate_ast_node(rule, expr));
            let main_struct = defs.clone().map(|d| d.main_struct);
            let external_choices = defs.filter_map(|d| d.external_choices).flatten();

            ASTNode {
                main_struct: quote! { ( #(#main_struct),* ) },
                external_choices: Some(external_choices.collect()),
            }
        }
        GrammarNode::Choice(choices) => {
            if choices.len() == 1 {
                return generate_ast_node(rule, choices.first().unwrap());
            }

            let defs = choices.iter().map(|choice| generate_ast_node(rule, choice));
            let enum_entries = defs
                .clone()
                .map(|d| d.main_struct)
                .enumerate()
                .map(|(idx, s)| {
                    let idx_ident = format_ident!("_{}", idx + 1);

                    quote! {
                        #idx_ident(#s)
                    }
                });

            let mut external_choices: Vec<_> =
                defs.filter_map(|d| d.external_choices).flatten().collect();
            let enum_ident = format_ident!("{}_choice_{}", rule, external_choices.len() + 1);
            external_choices.push(quote! {
                #[derive(::std::fmt::Debug)]
                enum #enum_ident {
                    #(#enum_entries),*
                }
            });

            ASTNode {
                main_struct: quote! { #enum_ident },
                external_choices: Some(external_choices),
            }
        }
        GrammarNode::Optional(opt) => {
            let generated = generate_ast_node(rule, opt);
            let main_struct = generated.main_struct;
            ASTNode {
                main_struct: quote! { ::std::option::Option<#main_struct> },
                external_choices: generated.external_choices,
            }
        }
    }
}
