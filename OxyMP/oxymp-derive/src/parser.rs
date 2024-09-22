use std::collections::HashMap;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::{
    grammar::{GrammarNode, GrammarNodeContent},
    lexer::TokenInfo,
};

pub fn generate_parser(
    parser_ident: &syn::Ident,
    rules: &HashMap<String, GrammarNode>,
) -> TokenStream {
    let parser_def = generate_def();
    let ast = generate_ast(rules);
    let parser_impl = generate_impl(parser_ident, rules);

    quote! {
        #parser_def
        #ast
        #parser_impl
    }
}

fn generate_def() -> TokenStream {
    quote! {
        #[derive(::std::fmt::Debug)]
        pub struct ParserInput {
            tokens: ::std::rc::Rc<[Token]>,
            current: ::core::primitive::usize,
        }
        impl<T> From<T> for ParserInput
        where
            T: ::std::ops::Deref<Target = [Token]>,
        {
            fn from(tokens: T) -> Self {
                ParserInput {
                    tokens: tokens.deref().into(),
                    current: 0
                }
            }
        }
        impl ParserInput {
            pub fn get_current(&self) -> ::std::option::Option<&Token> {
                self.tokens.get(self.current)
            }

            pub fn increment(&self) -> Self {
                ParserInput {
                    tokens: self.tokens.clone(),
                    current: self.current + 1,
                }
            }
        }
        type ParserState<T> = ::std::result::Result<(ParserInput, T), ParseError>;

        // TODO: better error handling (with enums)
        #[derive(::std::fmt::Debug)]
        pub struct ParseError {
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
    match &node.content {
        GrammarNodeContent::Rule(rule) => {
            let ident = format_ident!("{}", rule);
            ASTNode {
                main_struct: quote! {::std::boxed::Box<#ident>},
                external_choices: None,
            }
        }
        GrammarNodeContent::Token(token) => {
            let ident = TokenInfo::struct_ident(token);
            ASTNode {
                main_struct: quote! { ::std::rc::Rc<#ident> },
                external_choices: None,
            }
        }
        GrammarNodeContent::Expr(exprs) => {
            let defs = exprs.iter().map(|expr| generate_ast_node(rule, expr));
            let main_struct = defs.clone().map(|d| d.main_struct);
            let external_choices = defs.filter_map(|d| d.external_choices).flatten();

            ASTNode {
                main_struct: quote! { ( #(#main_struct),* ) },
                external_choices: Some(external_choices.collect()),
            }
        }
        GrammarNodeContent::Choice(choices, choice_idx) => {
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
            let enum_ident = format_ident!("{}_choice_{}", rule, choice_idx);
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
        GrammarNodeContent::Optional(opt) => {
            let generated = generate_ast_node(rule, opt);
            let main_struct = generated.main_struct;
            ASTNode {
                main_struct: quote! { ::std::option::Option<#main_struct> },
                external_choices: generated.external_choices,
            }
        }
    }
}

fn generate_impl(parser_ident: &syn::Ident, rules: &HashMap<String, GrammarNode>) -> TokenStream {
    let methods = rules
        .iter()
        .map(|(rule, node)| generate_rule(parser_ident, rule, node));

    quote! {
        impl #parser_ident {
            #(#methods),*
        }
    }
}

fn generate_rule(parser_ident: &syn::Ident, rule: &String, node: &GrammarNode) -> TokenStream {
    let rule_ident = format_ident!("{}", rule);
    let defs = generate_rule_def(parser_ident, rule, node);
    let toks = defs.0;
    let ident = defs.1;

    quote! {
        fn #rule_ident(inp: ParserInput) -> ParserState<#rule_ident> {
            #toks
            Ok((
                inp,
                #rule_ident(#ident)
            ))
        }
    }
}

// TODO: rename
fn generate_rule_def(
    parser_ident: &syn::Ident,
    rule: &String,
    node: &GrammarNode,
) -> (TokenStream, syn::Ident) {
    let node_ident = format_ident!("_{}", node.index);

    let toks = match &node.content {
        GrammarNodeContent::Rule(rule) => {
            let rule_ident = format_ident!("{}", rule);
            quote! {
                let (inp, #node_ident) = #parser_ident::#rule_ident(inp).map(|(remaining, ast)| (remaining, ::std::boxed::Box::new(ast)))?;
            }
        }
        GrammarNodeContent::Token(token) => {
            let token_enum_entry = TokenInfo::enum_entry_ident(token);
            let error_msg = format!("Expected a {}", token);
            quote! {
                let (inp, #node_ident) = match inp.get_current() {
                    None => Err(ParseError {
                        place: #rule.into(),
                        reason: "Input is empty".into(),
                    }),
                    Some(Token::#token_enum_entry(tok)) =>
                        Ok((inp.increment(), tok.clone())),
                    Some(tok) => Err(ParseError {
                        place: #rule.into(),
                        reason: #error_msg.into(),
                    })
                }?;
            }
        }
        GrammarNodeContent::Expr(exprs) => {
            let defs = exprs
                .iter()
                .map(|expr| generate_rule_def(parser_ident, rule, expr));
            let toks = defs.clone().map(|d| d.0);
            let idents = defs.map(|d| d.1);

            quote! {
                let (inp, #node_ident) = {
                     #(#toks)*

                    Ok((
                        inp,
                        ( #(#idents),* )
                    ))
                }?;
            }
        }
        GrammarNodeContent::Choice(choices, choice_idx) => todo!(),
        GrammarNodeContent::Optional(opt) => todo!(),
    };

    (toks, node_ident)
}
