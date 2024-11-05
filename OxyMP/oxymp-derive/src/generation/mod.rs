mod ast;
mod lexer;
mod parser;
mod tokens;

use std::{collections::HashMap, rc::Rc};

use crate::{grammar::GrammarNode, MacroData};
use quote::quote;

use ast::generate_ast;
use lexer::generate_constructor;
use parser::generate_impl;
use tokens::generate_tokens;

pub fn generate(data: MacroData, rules: HashMap<Rc<str>, GrammarNode>) -> proc_macro2::TokenStream {
    let tokens = generate_tokens(&data);
    let lexer_constructor = generate_constructor(&data);

    let ast = generate_ast(&data, &rules);
    let parser = generate_impl(&data, &rules);

    quote! {
        #tokens
        #lexer_constructor

        #ast
        #parser
    }
}
