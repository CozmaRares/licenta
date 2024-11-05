#![allow(non_snake_case)]

use proc_macro2::{Ident, TokenStream};
use quote::quote;

use crate::{
    data::MacroData,
    idents::{base_ident, tokens::*},
    symbols::{get_def, Symbol},
    tokens::*,
};

pub fn generate_lexer(data: &MacroData) -> TokenStream {
    let tokens = generate_tokens(data);
    let constructor = generate_constructor(data);

    quote! {
        #tokens
        #constructor
    }
}

fn generate_token_idents(token: &TokenInfo) -> Option<(Ident, Ident, Option<Ident>)> {
    match token {
        TokenInfo::Exact(ExactToken { name, .. }) => {
            Some((enum_ident(name), struct_ident(name), None))
        }
        TokenInfo::Regex(RegexToken { name, kind, .. }) => {
            Some((enum_ident(name), struct_ident(name), Some(base_ident(kind))))
        }
        TokenInfo::Ignore(_) => None,
    }
}

fn generate_tokens(data: &MacroData) -> TokenStream {
    let visibility = &data.visibility;
    let token_info = &data.tokens;

    let idents = token_info
        .iter()
        .filter_map(|info| generate_token_idents(info));

    let enum_entries = idents.clone().map(|(enum_entry, struct_ident, _)| {
        quote! {
            #enum_entry(#struct_ident)
        }
    });

    let _Rc = get_def(Symbol::Rc, data.simple_types);
    let _Debug = get_def(Symbol::DeriveDebug, data.simple_types);
    let _Clone = get_def(Symbol::DeriveClone, data.simple_types);

    let structs = idents
        .map(|idents| match idents {
            (_, struct_ident, None) => quote! {
                #visibility struct #struct_ident;
            },
            (_, struct_ident, Some(kind_ident)) => quote! {
                #visibility struct #struct_ident(#visibility #_Rc<#kind_ident>);
            },
        })
        .map(|struct_def| {
            quote! {
                #[derive(#_Debug, #_Clone)]
                #struct_def
            }
        });

    quote! {
        #(#structs)*

        #[derive(#_Debug, #_Clone)]
        #visibility enum Token {
            #(#enum_entries),*
        }
    }
}

fn generate_constructor(data: &MacroData) -> TokenStream {
    let visibility = &data.visibility;
    let token_info = &data.tokens;

    let _Rc = get_def(Symbol::Rc, data.simple_types);
    let _Box = get_def(Symbol::Box, data.simple_types);
    let _vec = get_def(Symbol::VecMacro, data.simple_types);
    let _LexRule = get_def(Symbol::UtilLexRule, data.simple_types);
    let _Lexer = get_def(Symbol::UtilLexer, data.simple_types);
    let _TokenHandler = get_def(Symbol::UtilTokenHandler, data.simple_types);
    let _TokenMatcher = get_def(Symbol::UtilTokenMatcher, data.simple_types);

    let rules = token_info.iter().map(|tok| match tok {
        TokenInfo::Exact(ExactToken { name, pattern }) => {
            let enum_ident = enum_ident(name);
            let struct_ident = struct_ident(name);

            quote! {
                #_LexRule {
                    matcher: #_TokenMatcher::Exact(#pattern.to_string()),
                    handler: #_TokenHandler::Pattern(Token::#enum_ident(#struct_ident))
                }
            }
        }
        TokenInfo::Regex(RegexToken {
            name,
            regex,
            transformer_fn,
            ..
        }) => {
            let enum_ident = enum_ident(name);
            let struct_ident = struct_ident(name);
            let fn_ident = base_ident(transformer_fn);

            quote! {
                #_LexRule {
                    matcher: #_TokenMatcher::regex(#regex),
                    handler: #_TokenHandler::Regex(
                        #_Box::new(
                            |matched| Token::#enum_ident(#struct_ident(#_Rc::new(#fn_ident(matched))))
                        )
                    )
                }
            }
        }
        TokenInfo::Ignore(IgnorePattern { regex }) => quote! {
            #_LexRule {
                matcher: #_TokenMatcher::regex(#regex),
                handler: #_TokenHandler::Ignore
            }
        },
    });

    quote! {
        #visibility fn create_lexer() -> #_Lexer<Token> {
            return #_Lexer { rules: #_vec![#(#rules),*] };
        }
    }
}
