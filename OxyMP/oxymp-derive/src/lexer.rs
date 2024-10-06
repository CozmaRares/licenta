use proc_macro2::{Ident, TokenStream};
use quote::quote;

use crate::{
    data::MacroData,
    idents::{base_ident, tokens::*},
    symbols::{get_def, Symbol},
    tokens::*,
};

pub fn generate_lexer(data: &MacroData) -> TokenStream {
    let defs = generate_static_defs(data);
    let tokens = generate_tokens(data);
    let constructor = generate_constructor(data);

    quote! {
        #defs
        #tokens
        #constructor
    }
}

#[allow(non_snake_case)]
fn generate_static_defs(data: &MacroData) -> TokenStream {
    let visibility = &data.visibility;

    let _str = get_def(Symbol::CoreStr, data.simple_types);
    let _usize = get_def(Symbol::CoreUsize, data.simple_types);

    let _format = get_def(Symbol::FormatMacro, data.simple_types);

    let _String = get_def(Symbol::String, data.simple_types);
    let _Regex = get_def(Symbol::Regex, data.simple_types);
    let _Box = get_def(Symbol::Box, data.simple_types);
    let _Fn = get_def(Symbol::Fn, data.simple_types);
    let _Vec = get_def(Symbol::Vec, data.simple_types);

    let _Option = get_def(Symbol::Option, data.simple_types);
    let _Some = get_def(Symbol::Some, data.simple_types);
    let _None = get_def(Symbol::None, data.simple_types);

    let _Result = get_def(Symbol::Result, data.simple_types);
    let _Ok = get_def(Symbol::Ok, data.simple_types);
    let _Err = get_def(Symbol::Err, data.simple_types);

    let _Debug = get_def(Symbol::DeriveDebug, data.simple_types);

    quote! {
        enum TokenMatcher {
            Exact(#_String),
            Regex(#_Regex),
        }

        impl TokenMatcher {
            fn regex(re: &#_str) -> TokenMatcher {
                let re = #_format!("^{re}");
                let re = #_Regex::new(&re).unwrap();
                TokenMatcher::Regex(re)
            }
        }

        enum TokenHandler {
            Pattern(Token),
            Regex(#_Box<dyn #_Fn(&#_str) -> Token>),
            Ignore,
        }

        struct LexRule {
            matcher: TokenMatcher,
            handler: TokenHandler,
        }

        impl LexRule {
            fn matches(&self, input: &#_str) -> #_Option<#_usize> {
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
                input: &'a #_str,
            ) -> #_Option<(#_Option<Token>, &'a #_str)> {
                self.matches(input).map(|matched_size| {
                    let token = match &self.handler {
                        TokenHandler::Ignore     => #_None ,
                        TokenHandler::Pattern(t) => #_Some(t.clone()),
                        TokenHandler::Regex(f)   => #_Some(f(&input[..matched_size])),
                    };
                    #_Some((token, &input[matched_size..]))
                })?
            }
        }

        #[derive(#_Debug)]
        #visibility struct LexError<'a> {
            #visibility input:   &'a #_str,
            #visibility message: #_String,
        }

        #visibility struct Lexer {
            rules: #_Vec<LexRule>,
        }

        impl Lexer {
            #visibility fn tokenize(self, mut input: &#_str) -> #_Result<#_Vec<Token>, LexError> {
                let mut tokens = #_Vec::new();
                while input.len() > 0 {
                    let mut was_consumed = false;
                    for rule in &self.rules {
                        if let #_Some((token, remaining)) = rule.consume(input) {
                            if let #_Some(token) = token {
                                tokens.push(token);
                            }
                            input = remaining;
                            was_consumed = true;
                            break;
                        }
                    }
                    if !was_consumed {
                        return #_Err(LexError {
                            input,
                            message: "Unknown token".to_string(),
                        });
                    }
                }
                #_Ok(tokens)
            }
        }
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

#[allow(non_snake_case)]
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

#[allow(non_snake_case)]
fn generate_constructor(data: &MacroData) -> TokenStream {
    let visibility = &data.visibility;
    let token_info = &data.tokens;

    let _Rc = get_def(Symbol::Rc, data.simple_types);
    let _Box = get_def(Symbol::Box, data.simple_types);
    let _vec = get_def(Symbol::VecMacro, data.simple_types);

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
                        #_Box::new(
                            |matched| Token::#enum_ident(#struct_ident(#_Rc::new(#fn_ident(matched))))
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
            #visibility fn new() -> Lexer {
                return Lexer { rules: #_vec![#(#rules),*] };
            }
        }
    }
}
