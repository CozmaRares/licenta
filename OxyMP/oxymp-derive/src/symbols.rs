use std::{collections::HashMap, sync::Arc};

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

#[rustfmt::skip]
lazy_static! {
    static ref STD_LIB: HashMap<Symbol, Definition> = {
        let mut m = HashMap::new();

        m.insert(Symbol::CoreStr,   (&["core", "primitive"][..], "str"  ).into());
        m.insert(Symbol::CoreUsize, (&["core", "primitive"][..], "usize").into());

        m.insert(Symbol::DeriveDebug, (&["std", "fmt"  ][..], "Debug").into());
        m.insert(Symbol::DeriveClone, (&["std", "clone"][..], "Clone").into());

        m.insert(Symbol::Result, (&["std", "result"          ][..], "Result").into());
        m.insert(Symbol::Ok,     (&["std", "result", "Result"][..], "Ok"    ).into());
        m.insert(Symbol::Err,    (&["std", "result", "Result"][..], "Err"   ).into());

        m.insert(Symbol::Option, (&["std", "option"          ][..], "Option").into());
        m.insert(Symbol::None,   (&["std", "option", "Option"][..], "None"  ).into());
        m.insert(Symbol::Some,   (&["std", "option", "Option"][..], "Some"  ).into());

        m.insert(Symbol::String, (&["std", "string"][..], "String").into());
        m.insert(Symbol::Box,    (&["std", "boxed" ][..], "Box"   ).into());
        m.insert(Symbol::Vec,    (&["std", "vec"   ][..], "Vec"   ).into());
        m.insert(Symbol::Rc,     (&["std", "rc"    ][..], "Rc"    ).into());

        m.insert(Symbol::Fn,    (&["std", "ops"][..], "Fn"    ).into());
        m.insert(Symbol::Deref, (&["std", "ops"][..], "Deref" ).into());

        m.insert(Symbol::FormatMacro, (&["std"][..], "format").into());
        m.insert(Symbol::VecMacro,    (&["std"][..], "vec")   .into());

        m.insert(Symbol::Regex, (&["regex"][..], "Regex").into());

        m
    };
}

// TODO: add a cache
pub fn get_def(symbol: Symbol, simple: bool) -> proc_macro2::TokenStream {
    let def = STD_LIB.get(&symbol).unwrap();
    let base = base_ident(def.base);

    if simple {
        quote! { #base }
    } else {
        let path = def.path.iter().map(|s| base_ident(s));
        quote! { ::#(#path)::*::#base }
    }
}
