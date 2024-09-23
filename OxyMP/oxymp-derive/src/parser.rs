use std::collections::HashMap;

use proc_macro2::TokenStream;
use quote::quote;

use crate::{
    grammar::{GrammarNode, GrammarNodeContent},
    idents::{parser, tokens},
};

pub fn generate_parser(
    parser_ident: &proc_macro2::Ident,
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
        #[derive(::std::fmt::Debug, ::std::clone::Clone)]
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

        let rule_ident = parser::rule_ident(rule);

        quote! {
            #external_choices
            #[derive(::std::fmt::Debug)]
            pub struct #rule_ident (
                #main_struct
            );
        }
    });

    let enum_entries = rules.keys().map(|rule| {
        let ident = parser::rule_ident(rule);
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
            let ident = parser::rule_ident(rule);
            ASTNode {
                main_struct: quote! {::std::boxed::Box<#ident>},
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
                    let idx_ident = parser::idx_ident(idx + 1);
                    quote! {
                        #idx_ident(#s)
                    }
                });

            let mut external_choices: Vec<_> =
                defs.filter_map(|d| d.external_choices).flatten().collect();
            let enum_ident = parser::choice_ident(rule, *choice_idx);
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

fn generate_impl(
    parser_ident: &proc_macro2::Ident,
    rules: &HashMap<String, GrammarNode>,
) -> TokenStream {
    let methods = rules
        .iter()
        .map(|(rule, node)| generate_rule(parser_ident, rule, node));

    quote! {
        impl #parser_ident {
            #(#methods),*
        }
    }
}

fn generate_rule(
    parser_ident: &proc_macro2::Ident,
    rule: &String,
    node: &GrammarNode,
) -> TokenStream {
    let rule_ident = parser::rule_ident(rule);
    let defs = generate_rule_def(parser_ident, rule, node);
    let toks = defs.0;
    let ident = defs.1;

    quote! {
        fn #rule_ident(inp: ParserInput) -> ParserState<#rule_ident> {
            #toks
            ::std::result::Result::Ok((
                inp,
                #rule_ident(#ident)
            ))
        }
    }
}

fn generate_rule_def(
    parser_ident: &proc_macro2::Ident,
    rule: &String,
    node: &GrammarNode,
) -> (TokenStream, proc_macro2::Ident) {
    let node_ident = parser::idx_ident(node.index);

    let toks = match &node.content {
        GrammarNodeContent::Rule(rule) => {
            let rule_ident = parser::rule_ident(rule);
            quote! {
                let (inp, #node_ident) =
                    #parser_ident::#rule_ident(inp)
                        .map(|(remaining, ast)| (remaining, ::std::boxed::Box::new(ast)))?;
            }
        }
        GrammarNodeContent::Token(token) => {
            let token_enum_entry = tokens::enum_ident(token);
            let error_msg = format!("Expected a {}", token);
            quote! {
                let (inp, #node_ident) = match inp.get_current() {
                    ::std::option::Option::None => ::std::result::Result::Err(ParseError {
                        place: #rule.into(),
                        reason: "Input is empty".into(),
                    }),
                    ::std::option::Option::Some(Token::#token_enum_entry(tok)) =>
                        ::std::result::Result::Ok((inp.increment(), tok.clone())),
                    ::std::option::Option::Some(tok) => ::std::result::Result::Err(ParseError {
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
                let (inp, #node_ident) = (|| {
                     #(#toks)*

                    ::std::result::Result::Ok((
                        inp,
                        ( #(#idents),* )
                    ))
                })()?;
            }
        }
        GrammarNodeContent::Choice(choices, choice_idx) => {
            let defs = choices
                .iter()
                .map(|expr| generate_rule_def(parser_ident, rule, expr))
                .enumerate()
                .map(|(idx, (toks, ident))| {
                    let idx_ident = parser::idx_ident(idx + 1);
                    let choice_ident = parser::choice_ident(rule, *choice_idx);

                    quote! {
                        let r: ParserState<_> = (|| {
                            #toks
                            ::std::result::Result::Ok((inp, #ident))
                        })();
                        match r {
                            ::std::result::Result::Ok((inp, ast)) => {
                                return ::std::result::Result::Ok((inp, #choice_ident::#idx_ident(ast)));
                            }
                            ::std::result::Result::Err(_) => {}
                        };
                    }
                });

            quote! {
                let (inp, #node_ident) =  (|| {
                     #(#defs)*

                    ::std::result::Result::Err(ParseError {
                        place: #rule.into(),
                        reason: "All choices failed".into(),
                    })
                })()?;
            }
        }
        GrammarNodeContent::Optional(opt) => {
            let (toks, ident) = generate_rule_def(parser_ident, rule, opt);

            quote! {
                let res: ParserState<_> = (|| {
                    let inp = inp.clone();
                    #toks
                    ::std::result::Result::Ok((inp, #ident))
                })();
                let (inp, #node_ident) =  match res {
                    ::std::result::Result::Ok((new_inp, ast)) => (new_inp, ::std::option::Option::Some(ast)),
                    ::std::result::Result::Err(_) => (inp, ::std::option::Option::None),
                };
            }
        }
    };

    (toks, node_ident)
}
