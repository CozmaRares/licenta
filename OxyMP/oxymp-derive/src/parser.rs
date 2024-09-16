use std::collections::HashMap;

use quote::quote;

use crate::grammar::GrammarNode;

pub fn generate_parser(rules: &HashMap<String, GrammarNode>) -> proc_macro2::TokenStream {
    let parser_def = generate_def();
    let ast = generate_ast(rules);

    quote! {
        pub mod parser {
            #parser_def
            #ast
        }
    }
}

fn generate_def() -> proc_macro2::TokenStream {
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

fn generate_ast(rules: &HashMap<String, GrammarNode>) -> proc_macro2::TokenStream {
    quote! {
        enum AST {
            Null,
        }
    }
}
