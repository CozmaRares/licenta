use std::collections::HashSet;

use quote::quote;

use crate::{
    attribute::AttributeList,
    idents::{base_ident, tokens::*},
};

#[derive(Debug)]
pub struct ExactToken {
    pub name: String,
    pub pattern: String,
}

#[derive(Debug)]
pub struct RegexToken {
    pub name: String,
    pub regex: String,
    pub transformer_fn: String,
    pub kind: String,
}

#[derive(Debug)]
pub struct IgnorePattern {
    pub regex: String,
}

#[derive(Debug)]
pub enum TokenInfo {
    Exact(ExactToken),
    Regex(RegexToken),
    Ignore(IgnorePattern),
}

impl TokenInfo {
    fn generate_idents(
        &self,
    ) -> Option<(
        proc_macro2::Ident,
        proc_macro2::Ident,
        Option<proc_macro2::Ident>,
    )> {
        match self {
            TokenInfo::Exact(ExactToken { name, .. }) => {
                Some((enum_ident(name), struct_ident(name), None))
            }
            TokenInfo::Regex(RegexToken { name, kind, .. }) => {
                Some((enum_ident(name), struct_ident(name), Some(base_ident(kind))))
            }
            _ => None,
        }
    }
}

impl TokenInfo {
    //#[exact_token(name = "Minus", pattern = "-")]
    pub fn exact_token(tokens: proc_macro2::TokenStream) -> syn::Result<Self> {
        let mut expected_properties = HashSet::new();
        expected_properties.insert("name".to_string());
        expected_properties.insert("pattern".to_string());

        let map = AttributeList::prepare_token_info(
            tokens,
            "exact_token".to_string(),
            expected_properties,
        )?;

        return Ok(TokenInfo::Exact(ExactToken {
            name: map.get("name").unwrap().to_string(),
            pattern: map.get("pattern").unwrap().to_string(),
        }));
    }

    //#[regex_token(name = "Number", regex = r"-?[0-9]+", transformer_fn = "match_number", kind = "i64")]
    pub fn regex_token(tokens: proc_macro2::TokenStream) -> syn::Result<Self> {
        let mut expected_properties = HashSet::new();
        expected_properties.insert("name".to_string());
        expected_properties.insert("regex".to_string());
        expected_properties.insert("transformer_fn".to_string());
        expected_properties.insert("kind".to_string());

        let map = AttributeList::prepare_token_info(
            tokens,
            "regex_token".to_string(),
            expected_properties,
        )?;

        return Ok(TokenInfo::Regex(RegexToken {
            name: map.get("name").unwrap().to_string(),
            regex: map.get("regex").unwrap().to_string(),
            transformer_fn: map.get("transformer_fn").unwrap().to_string(),
            kind: map.get("kind").unwrap().to_string(),
        }));
    }

    //#[ignore_pattern(regex = r"\s+")]
    pub fn ignore_pattern(tokens: proc_macro2::TokenStream) -> syn::Result<Self> {
        let mut expected_properties = HashSet::new();
        expected_properties.insert("regex".to_string());

        let map = AttributeList::prepare_token_info(
            tokens,
            "ignore_pattern".to_string(),
            expected_properties,
        )?;

        return Ok(TokenInfo::Ignore(IgnorePattern {
            regex: map.get("regex").unwrap().to_string(),
        }));
    }
}

pub fn generate_lexer(token_info: &Vec<TokenInfo>) -> proc_macro2::TokenStream {
    let lexer_def = generate_def();
    let token_def = generate_tokens(&token_info);
    let lex_rules = generate_rules(&token_info);

    return quote! {
        #lexer_def
        #token_def
        #lex_rules
    };
}

fn generate_def() -> proc_macro2::TokenStream {
    return quote! {
        enum TokenMatcher {
            Exact(::std::string::String),
            Regex(::regex::Regex),
        }

        impl TokenMatcher {
            fn regex(re: &::core::primitive::str) -> Self {
                let re = ::std::format!("^{re}");
                let re = ::regex::Regex::new(&re).unwrap();
                return TokenMatcher::Regex(re);
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
                    return ::std::option::Option::Some((token, &input[matched_size..]));
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
                return ::std::result::Result::Ok(tokens);
            }
        }
    };
}

fn generate_tokens(token_info: &Vec<TokenInfo>) -> proc_macro2::TokenStream {
    let idents = token_info
        .iter()
        .map(|info| info.generate_idents())
        .filter(|opt| opt.is_some())
        .map(|idents| idents.unwrap());

    let enum_entries = idents.clone().map(|(enum_entry, struct_ident, _)| {
        return quote! {
            #enum_entry(#struct_ident)
        };
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

    return quote! {
        #(#structs)*

        #[derive(::std::fmt::Debug, ::std::clone::Clone)]
        pub enum Token {
            #(#enum_entries),*
        }
    };
}

fn generate_rules(token_info: &Vec<TokenInfo>) -> proc_macro2::TokenStream {
    let rules = token_info.iter().map(|tok| match tok {
        TokenInfo::Exact(ExactToken { name, pattern }) => {
            let enum_ident = enum_ident(name);
            let struct_ident = struct_ident(name);

            return quote! {
                LexRule {
                    matcher: TokenMatcher::Exact(#pattern.to_string()),
                    handler: TokenHandler::Pattern(Token::#enum_ident(#struct_ident))
                }
            };
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

            return quote! {
                LexRule {
                    matcher: TokenMatcher::regex(#regex),
                    handler: TokenHandler::Regex(
                        ::std::boxed::Box::new(
                            |matched| Token::#enum_ident(#struct_ident(::std::rc::Rc::new(#fn_ident(matched))))
                        )
                    )
                }
            };
        }
        TokenInfo::Ignore(IgnorePattern { regex }) => quote! {
            LexRule {
                matcher: TokenMatcher::regex(#regex),
                handler: TokenHandler::Ignore
            }
        },
    });

    return quote! {
        impl Lexer {
            pub fn new() -> Self {
                return Lexer { rules: ::std::vec![#(#rules),*] };
            }
        }
    };
}
