#![allow(non_snake_case)]

use quote::quote;

use crate::{
    data::MacroData,
    idents::tokens,
    symbols::{get_def, Symbol},
    tokens::{ExactToken, IgnorePattern, RegexToken, TokenInfo},
};

pub fn generate_constructor(data: &MacroData) -> proc_macro2::TokenStream {
    let visibility = &data.visibility;
    let token_info = &data.tokens;

    let rules = token_info
        .iter()
        .map(|tok| generate_rule(tok, data.simple_types));

    let has_tier = rules.clone().any(|(tier, _rule)| tier.is_some());

    let rules = rules.map(|(tier, rule)| match tier {
        None => quote! {
            builder.add_rule(#rule);
        },
        Some(tier) => quote! {
            builder.add_tiered_rule(#tier, #rule);
        },
    });

    let _Lexer = get_def(Symbol::UtilLexer, data.simple_types);
    let _LexerBuilder = get_def(Symbol::UtilLexerBuilder, data.simple_types);

    let builder_tokens = if has_tier {
        quote! {
            #_LexerBuilder::default()
        }
    } else {
        quote! {
            #_LexerBuilder::<Token>::default()
        }
    };

    quote! {
        #visibility fn create_lexer() -> #_Lexer<Token> {
            let mut builder = #builder_tokens;
            #(#rules)*
            builder.build()
        }
    }
}

fn generate_rule(
    tok: &TokenInfo,
    simple: bool,
) -> (&Option<proc_macro2::TokenStream>, proc_macro2::TokenStream) {
    let _Rc = get_def(Symbol::Rc, simple);
    let _Box = get_def(Symbol::Box, simple);
    let _LexRule = get_def(Symbol::UtilLexRule, simple);
    let _TokenHandler = get_def(Symbol::UtilTokenHandler, simple);
    let _TokenMatcher = get_def(Symbol::UtilTokenMatcher, simple);

    match tok {
        TokenInfo::Exact(ExactToken {
            name,
            pattern,
            tier,
        }) => {
            let enum_ident = tokens::enum_ident(name);
            let struct_ident = tokens::struct_ident(name);

            (
                tier,
                quote! {
                    #_LexRule {
                        matcher: #_TokenMatcher::Exact(#pattern.to_string()),
                        handler: #_TokenHandler::Pattern(Token::#enum_ident(#struct_ident))
                    }
                },
            )
        }
        TokenInfo::Regex(RegexToken {
            name,
            regex,
            transformer_fn,
            tier,
            ..
        }) => {
            let enum_ident = tokens::enum_ident(name);
            let struct_ident = tokens::struct_ident(name);

            (
                tier,
                quote! {
                    #_LexRule {
                        matcher: #_TokenMatcher::regex(#regex),
                        handler: #_TokenHandler::Regex(
                            #_Box::new(
                                |matched| Token::#enum_ident(#struct_ident(#_Rc::new(#transformer_fn(matched))))
                            )
                        )
                    }
                },
            )
        }
        TokenInfo::Ignore(IgnorePattern { regex, tier }) => (
            tier,
            quote! {
                #_LexRule {
                    matcher: #_TokenMatcher::regex(#regex),
                    handler: #_TokenHandler::Ignore
                }
            },
        ),
    }
}
