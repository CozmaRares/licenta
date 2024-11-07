#![allow(non_snake_case)]

use quote::quote;

use crate::{
    data::MacroData,
    idents::{base_ident, tokens},
    symbols::{get_def, Symbol},
    tokens::{ExactToken, IgnorePattern, RegexToken, TokenInfo},
};

pub fn generate_constructor(data: &MacroData) -> proc_macro2::TokenStream {
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
            let enum_ident = tokens::enum_ident(name);
            let struct_ident = tokens::struct_ident(name);

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
            let enum_ident = tokens::enum_ident(name);
            let struct_ident = tokens::struct_ident(name);

            quote! {
                #_LexRule {
                    matcher: #_TokenMatcher::regex(#regex),
                    handler: #_TokenHandler::Regex(
                        #_Box::new(
                            |matched| Token::#enum_ident(#struct_ident(#_Rc::new(#transformer_fn(matched))))
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
            #_Lexer { rules: #_vec![#(#rules),*] }
        }
    }
}
