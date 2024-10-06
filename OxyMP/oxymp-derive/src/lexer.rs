use quote::quote;

use crate::{
    data::MacroData,
    idents::{base_ident, tokens::*},
    tokens::*,
};

pub fn generate_lexer(data: &MacroData) -> proc_macro2::TokenStream {
    let defs = generate_static_defs();
    let tokens = generate_tokens(&data.tokens);
    let constructor = generate_constructor(&data.tokens);

    quote! {
        #defs
        #tokens
        #constructor
    }
}

fn generate_static_defs() -> proc_macro2::TokenStream {
    quote! {
        enum TokenMatcher {
            Exact(::std::string::String),
            Regex(::regex::Regex),
        }

        impl TokenMatcher {
            fn regex(re: &::core::primitive::str) -> TokenMatcher {
                let re = ::std::format!("^{re}");
                let re = ::regex::Regex::new(&re).unwrap();
                TokenMatcher::Regex(re)
            }
        }

        enum TokenHandler {
            Pattern(Token),
            Regex(::std::boxed::Box<dyn ::std::ops::Fn(&::core::primitive::str) -> Token>),
            Ignore,
        }

        struct LexRule {
            matcher: TokenMatcher,
            handler: TokenHandler,
        }

        impl LexRule {
            fn matches(&self, input: &::core::primitive::str) -> ::std::option::Option<::core::primitive::usize> {
                match &self.matcher {
                    TokenMatcher::Exact(exact_match) => {
                        input.starts_with(exact_match).then(|| exact_match.len())
                    }
                    TokenMatcher::Regex(re) => {
                        re
                            .captures(input)
                            .map(|captures| captures.get(0))
                            .flatten()
                            .map(|matched| matched.end() - matched.start())
                    }
                }
            }

            fn consume<'a>(
                &self,
                input: &'a ::core::primitive::str,
            ) -> ::std::option::Option<(::std::option::Option<Token>, &'a ::core::primitive::str)> {
                self.matches(input).map(|matched_size| {
                    let token = match &self.handler {
                        TokenHandler::Ignore     => ::std::option::Option::None ,
                        TokenHandler::Pattern(t) => ::std::option::Option::Some(t.clone()),
                        TokenHandler::Regex(f)   => ::std::option::Option::Some(f(&input[..matched_size])),
                    };
                    ::std::option::Option::Some((token, &input[matched_size..]))
                })?
            }
        }

        #[derive(::std::fmt::Debug)]
        pub struct LexError<'a> {
            pub input:   &'a ::core::primitive::str,
            pub message: ::std::string::String,
        }

        pub struct Lexer {
            rules: ::std::vec::Vec<LexRule>,
        }

        impl Lexer {
            pub fn tokenize(self, mut input: &::core::primitive::str) -> ::std::result::Result<::std::vec::Vec<Token>, LexError> {
                let mut tokens = ::std::vec::Vec::new();
                while input.len() > 0 {
                    let mut was_consumed = false;
                    for rule in &self.rules {
                        if let ::std::option::Option::Some((token, remaining)) = rule.consume(input) {
                            if let ::std::option::Option::Some(token) = token {
                                tokens.push(token);
                            }
                            input = remaining;
                            was_consumed = true;
                            break;
                        }
                    }
                    if !was_consumed {
                        return ::std::result::Result::Err(LexError {
                            input,
                            message: "Unknown token".to_string(),
                        });
                    }
                }
                ::std::result::Result::Ok(tokens)
            }
        }
    }
}

fn generate_token_idents(
    token: &TokenInfo,
) -> Option<(
    proc_macro2::Ident,
    proc_macro2::Ident,
    Option<proc_macro2::Ident>,
)> {
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

fn generate_tokens(token_info: &Vec<TokenInfo>) -> proc_macro2::TokenStream {
    let idents = token_info
        .iter()
        .filter_map(|info| generate_token_idents(info));

    let enum_entries = idents.clone().map(|(enum_entry, struct_ident, _)| {
        quote! {
            #enum_entry(#struct_ident)
        }
    });

    let structs = idents
        .map(|idents| match idents {
            (_, struct_ident, None) => quote! {
                pub struct #struct_ident;
            },
            (_, struct_ident, Some(kind_ident)) => quote! {
                pub struct #struct_ident(pub ::std::rc::Rc<#kind_ident>);
            },
        })
        .map(|struct_def| {
            quote! {
                #[derive(::std::fmt::Debug, ::std::clone::Clone)]
                #struct_def
            }
        });

    quote! {
        #(#structs)*

        #[derive(::std::fmt::Debug, ::std::clone::Clone)]
        pub enum Token {
            #(#enum_entries),*
        }
    }
}

fn generate_constructor(token_info: &Vec<TokenInfo>) -> proc_macro2::TokenStream {
    let rules = token_info.iter().map(|tok| match tok {
        TokenInfo::Exact(ExactToken { name, pattern }) => {
            let enum_ident = enum_ident(name);
            let struct_ident = struct_ident(name);

            quote! {
                LexRule {
                    matcher: TokenMatcher::Exact(#pattern.to_string()),
                    handler: TokenHandler::Pattern(Token::#enum_ident(#struct_ident))
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
                LexRule {
                    matcher: TokenMatcher::regex(#regex),
                    handler: TokenHandler::Regex(
                        ::std::boxed::Box::new(
                            |matched| Token::#enum_ident(#struct_ident(::std::rc::Rc::new(#fn_ident(matched))))
                        )
                    )
                }
            }
        }
        TokenInfo::Ignore(IgnorePattern { regex }) => quote! {
            LexRule {
                matcher: TokenMatcher::regex(#regex),
                handler: TokenHandler::Ignore
            }
        },
    });

    quote! {
        impl Lexer {
            pub fn new() -> Lexer {
                return Lexer { rules: ::std::vec![#(#rules),*] };
            }
        }
    }
}
