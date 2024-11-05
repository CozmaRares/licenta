use std::{cell::RefCell, collections::HashMap, rc::Rc};

use lazy_static::lazy_static;
use quote::quote;

use crate::idents::base_ident;

#[derive(PartialEq, Eq, Hash)]
pub enum Symbol {
    CoreStr,
    CoreUsize,

    DeriveDebug,
    DeriveClone,

    Result,
    Ok,
    Err,

    Option,
    None,
    Some,

    String,
    Box,
    Vec,
    Rc,

    Fn,
    Deref,

    FormatMacro,
    VecMacro,

    Regex,

    UtilTokenMatcher,
    UtilTokenHandler,
    UtilLexRule,
    UtilLexError,
    UtilLexer,
}

type Definition<'a> = (&'a [&'a str], &'a str);

#[rustfmt::skip]
lazy_static! {
    static ref STD_LIB: HashMap<Symbol, Definition<'static>> = {
        let mut m = HashMap::new();

        m.insert(Symbol::CoreStr,   (&["core", "primitive"][..], "str"  ));
        m.insert(Symbol::CoreUsize, (&["core", "primitive"][..], "usize"));

        m.insert(Symbol::DeriveDebug, (&["std", "fmt"  ][..], "Debug"));
        m.insert(Symbol::DeriveClone, (&["std", "clone"][..], "Clone"));

        m.insert(Symbol::Result, (&["std", "result"          ][..], "Result"));
        m.insert(Symbol::Ok,     (&["std", "result", "Result"][..], "Ok"    ));
        m.insert(Symbol::Err,    (&["std", "result", "Result"][..], "Err"   ));

        m.insert(Symbol::Option, (&["std", "option"          ][..], "Option"));
        m.insert(Symbol::None,   (&["std", "option", "Option"][..], "None"  ));
        m.insert(Symbol::Some,   (&["std", "option", "Option"][..], "Some"  ));

        m.insert(Symbol::String, (&["std", "string"][..], "String"));
        m.insert(Symbol::Box,    (&["std", "boxed" ][..], "Box"   ));
        m.insert(Symbol::Vec,    (&["std", "vec"   ][..], "Vec"   ));
        m.insert(Symbol::Rc,     (&["std", "rc"    ][..], "Rc"    ));

        m.insert(Symbol::Fn,    (&["std", "ops"][..], "Fn"    ));
        m.insert(Symbol::Deref, (&["std", "ops"][..], "Deref" ));

        m.insert(Symbol::FormatMacro, (&["std"][..], "format"));
        m.insert(Symbol::VecMacro,    (&["std"][..], "vec")   );

        m.insert(Symbol::Regex, (&["regex"][..], "Regex"));

        m.insert(Symbol::UtilTokenMatcher, (&["oxymp_util", "lexer"][..], "TokenMatcher"));
        m.insert(Symbol::UtilTokenHandler, (&["oxymp_util", "lexer"][..], "TokenHandler"));
        m.insert(Symbol::UtilLexRule,      (&["oxymp_util", "lexer"][..], "LexRule"     ));
        m.insert(Symbol::UtilLexError,     (&["oxymp_util", "lexer"][..], "LexError"    ));
        m.insert(Symbol::UtilLexer,        (&["oxymp_util", "lexer"][..], "Lexer"       ));

        m
    };
}

thread_local! {
    static SYMBOL_CACHE: RefCell<HashMap<Symbol, Rc<proc_macro2::TokenStream>>> = RefCell::new(HashMap::new());
}

pub fn get_def(symbol: Symbol, simple: bool) -> Rc<proc_macro2::TokenStream> {
    if let Some(cached) = SYMBOL_CACHE.with(|c| c.borrow().get(&symbol).cloned()) {
        return cached;
    }

    let def = STD_LIB.get(&symbol).unwrap();
    let base = base_ident(def.1);

    let toks = Rc::new(if simple {
        quote! { #base }
    } else {
        let path = def.0.iter().map(|s| base_ident(s));
        quote! { ::#(#path)::*::#base }
    });

    SYMBOL_CACHE.with(|c| c.borrow_mut().insert(symbol, toks.clone()));
    toks
}
