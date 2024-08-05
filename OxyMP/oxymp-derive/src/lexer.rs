use std::collections::HashSet;

use quote::{format_ident, quote};

use crate::attribute::AttributeList;

#[derive(Debug)]
pub struct ExactToken {
    name: String,
    pattern: String,
}

#[derive(Debug)]
pub struct RegexToken {
    name: String,
    regex: String,
    transformer_fn: String,
    kind: String,
}

#[derive(Debug)]
pub struct IgnorePattern {
    regex: String,
}

#[derive(Debug)]
pub enum TokenInfo {
    Exact(ExactToken),
    Regex(RegexToken),
    Ignore(IgnorePattern),
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

pub struct Lexer;

impl Lexer {
    pub fn generate(token_info: &Vec<TokenInfo>) -> proc_macro2::TokenStream {
        let lexer_def = Lexer::generate_def();
        let token_def = Lexer::generate_tokens(&token_info);
        let lex_rules = Lexer::generate_rules(&token_info);

        return quote! {
            pub mod lexer {
                #lexer_def
                #token_def
                #lex_rules
            }
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
                Token(::std::boxed::Box<dyn ::std::ops::Fn() -> Token>),
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
                            let captures = re.captures(input);
                            if captures.is_none() {
                                return ::std::option::Option::None;
                            }
                            match captures.unwrap().get(0) {
                                ::std::option::Option::Some(matched) => ::std::option::Option::Some(matched.end() - matched.start()),
                                ::std::option::Option::None => ::std::option::Option::None
                            }
                        }
                    }
                }

                fn consume<'a>(
                    &self,
                    input: &'a ::core::primitive::str,
                ) -> ::std::option::Option<(::std::option::Option<Token>, &'a ::core::primitive::str)> {
                    self.matches(input).map(|matched_size| {
                        let token = match &self.handler {
                            TokenHandler::Ignore => ::std::option::Option::None ,
                            TokenHandler::Token(t) => ::std::option::Option::Some(t()),
                            TokenHandler::Regex(l) => ::std::option::Option::Some(l(&input[..matched_size])),
                        };
                        return ::std::option::Option::Some((token, &input[matched_size..]));
                    })?
                }

            }

            #[derive(::std::fmt::Debug)]
            pub struct LexError<'a> {
                pub message: ::std::string::String,
                pub input: &'a ::core::primitive::str,
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
                                message: "Unknown token".to_string(),
                                input,
                            });
                        }
                    }
                    return ::std::result::Result::Ok(tokens);
                }
            }
        };
    }

    fn generate_tokens(token_info: &Vec<TokenInfo>) -> proc_macro2::TokenStream {
        let token_info = token_info.iter();

        let (enum_entries, structs) = token_info
            .filter(|info| match info {
                TokenInfo::Exact(_) => true,
                TokenInfo::Regex(_) => true,
                _ => false,
            })
            .map(|info| match info {
                TokenInfo::Exact(ExactToken { name, .. }) => (name, None),
                TokenInfo::Regex(RegexToken { name, kind, .. }) => (name, Some(kind)),
                _ => unreachable!(),
            })
            .map(|(name, kind)| {
                let ident = format_ident!("{}", name);
                let struct_ident = format_ident!("Token_{}", name);

                let enum_entry = quote! {
                    #ident(#struct_ident)
                };

                let mut inner_type = quote! {};

                if let Some(kind) = kind {
                    let kind = format_ident!("{}", kind);
                    inner_type = quote! { (pub #kind) };
                }

                let struct_def = quote! {
                    #[derive(::std::fmt::Debug)]
                    pub struct #struct_ident #inner_type;
                };

                return (enum_entry, struct_def);
            })
            .fold((Vec::new(), Vec::new()), |(mut vec1, mut vec2), (x, y)| {
                vec1.push(x);
                vec2.push(y);
                (vec1, vec2)
            });

        return quote! {
            #(#structs)*

            #[derive(::std::fmt::Debug)]
            pub enum Token {
                #(#enum_entries),*
            }
        };
    }

    fn generate_rules(token_info: &Vec<TokenInfo>) -> proc_macro2::TokenStream {
        let rules = token_info.iter().map(|tok| match tok {
            TokenInfo::Exact(ExactToken { name, pattern }) => {
                let ident = format_ident!("{}", name);
                let struct_ident = format_ident!("Token_{}", name);

                return quote! {
                    LexRule {
                        matcher: TokenMatcher::Exact(#pattern.to_string()),
                        handler: TokenHandler::Token(Box::new(|| Token::#ident(#struct_ident)))
                    }
                };
            }
            TokenInfo::Regex(RegexToken {
                name,
                regex,
                transformer_fn,
                ..
            }) => {
                let ident = format_ident!("{}", name);
                let struct_ident = format_ident!("Token_{}", name);
                let fn_ident = format_ident!("{}", transformer_fn);

                return quote! {
                    LexRule {
                        matcher: TokenMatcher::regex(#regex),
                        handler: TokenHandler::Regex(
                            ::std::boxed::Box::new(
                                |matched| Token::#ident(#struct_ident(crate::#fn_ident(matched)))
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
                    return Lexer {rules: ::std::vec![#(#rules),*] };
                }
            }
        };
    }
}
