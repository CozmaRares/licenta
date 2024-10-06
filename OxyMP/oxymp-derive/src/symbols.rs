use std::{collections::HashMap, sync::Arc};

use lazy_static::lazy_static;
use quote::quote;

use crate::idents::base_ident;

#[derive(PartialEq, Eq, Hash)]
pub enum Symbol {
    CoreStr,

    Result,
    Ok,
    Err,

    Option,
    None,
    Some,

    String,

    Regex,
}

struct Definition {
    path: Arc<[&'static str]>,
    base: &'static str,
}

impl From<(&[&'static str], &'static str)> for Definition {
    fn from(value: (&[&'static str], &'static str)) -> Definition {
        let (path, base) = value;
        let path = path.to_vec().into();
        Definition { path, base }
    }
}

lazy_static! {
    static ref STD_LIB: HashMap<Symbol, Definition> = {
        let mut m = HashMap::new();

        m.insert(Symbol::CoreStr, (&["core", "primitive"][..], "str").into());

        m.insert(Symbol::Result, (&["std", "result"][..], "Result").into());
        m.insert(Symbol::Ok, (&["std", "result"][..], "Ok").into());
        m.insert(Symbol::Err, (&["std", "result"][..], "Err").into());

        m.insert(Symbol::Option, (&["std", "option"][..], "Option").into());
        m.insert(Symbol::None, (&["std", "option"][..], "None").into());
        m.insert(Symbol::Some, (&["std", "option"][..], "Some").into());

        m.insert(Symbol::String, (&["std", "string"][..], "String").into());

        m.insert(Symbol::Regex, (&["regex"][..], "Regex").into());

        m
    };
}

pub fn get_def(symbol: Symbol, simple: bool) -> proc_macro2::TokenStream {
    let def = STD_LIB.get(&symbol).unwrap();
    let base = base_ident(def.base);

    if simple {
        quote! { #base }
    } else {
        let path = def.path.iter().map(|s| base_ident(s));
        quote! { ::#(#path)::*::base_ident }
    }
}
