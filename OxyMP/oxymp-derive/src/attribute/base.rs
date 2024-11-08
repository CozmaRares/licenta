use std::collections::{HashMap, HashSet};

use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
};

pub struct Attribute<T>(pub T)
where
    T: Parse;

impl<T> Parse for Attribute<T>
where
    T: Parse,
{
    // Parse `#[...]`
    fn parse(input: ParseStream) -> syn::Result<Attribute<T>> {
        let _hash: syn::Token![#] = input.parse()?;

        let content_bracketed;
        let _bracketed = syn::bracketed!(content_bracketed in input);

        let inner: T = content_bracketed.parse()?;

        Ok(Attribute(inner))
    }
}

pub struct KeyValue<T>
where
    T: Parse,
{
    pub key_ident: syn::Ident,
    pub value: T,
}

impl<T> Parse for KeyValue<T>
where
    T: Parse,
{
    // Parse `a='b'`, `b=1`, `c=true`, ...
    fn parse(input: ParseStream) -> syn::Result<KeyValue<T>> {
        let ident: syn::Ident = input.parse()?;
        let _eq: syn::Token![=] = input.parse()?;
        let value: T = input.parse()?;
        Ok(KeyValue {
            key_ident: ident,
            value,
        })
    }
}

pub type CommaSeparated<T> = Punctuated<T, syn::Token![,]>;

pub struct AttributeList<T> {
    pub attribute_ident: syn::Ident,
    pub list: Vec<T>,
}

impl<T> Parse for AttributeList<T>
where
    T: Parse,
{
    // Parse `#[attr(a, b, ...)]`
    fn parse(input: ParseStream) -> syn::Result<AttributeList<T>> {
        let _hash: syn::Token![#] = input.parse()?;

        let content_bracketed;
        let _bracketed = syn::bracketed!(content_bracketed in input);

        let attr: syn::Ident = content_bracketed.parse()?;

        let content_parenthesized;
        let _parenthesized = syn::parenthesized!(content_parenthesized in content_bracketed);

        let list = CommaSeparated::<T>::parse_terminated(&content_parenthesized)?;

        Ok(AttributeList {
            attribute_ident: attr,
            list: list.into_iter().collect(),
        })
    }
}

pub enum PropertyType {
    LitStr,
    Path,
}

impl std::fmt::Display for PropertyType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let variant_str = match self {
            PropertyType::LitStr => "string literal",
            PropertyType::Path => "path definition",
        };
        write!(f, "{}", variant_str)
    }
}

pub struct ExpectedProperty {
    type_: PropertyType,
    optional: bool,
}

impl ExpectedProperty {
    fn matches(&self, expr: &syn::Expr) -> bool {
        match (&self.type_, expr) {
            (PropertyType::LitStr, syn::Expr::Lit(expr_lit)) => {
                matches!(expr_lit.lit, syn::Lit::Str(_))
            }
            (PropertyType::Path, syn::Expr::Path(_)) => true,
            _ => false,
        }
    }
}

impl ExpectedProperty {
    pub fn new(type_: PropertyType) -> ExpectedProperty {
        ExpectedProperty {
            type_,
            optional: false,
        }
    }

    pub fn optional(mut self) -> ExpectedProperty {
        self.optional = true;
        self
    }
}

pub fn fit_attribute_list(
    tokens: proc_macro2::TokenStream,
    expected_attribute: &str,
    expected_properties: HashMap<&str, ExpectedProperty>,
) -> syn::Result<HashMap<String, syn::Expr>> {
    let parsed: AttributeList<KeyValue<syn::Expr>> = syn::parse2(tokens)?;

    let ident = parsed.attribute_ident.to_string();

    if ident.to_string() != *expected_attribute {
        return Err(syn::Error::new(
            parsed.attribute_ident.span(),
            format!(
                "Wrong attribute\nExpected: {}\nGot: {}",
                expected_attribute, ident
            ),
        ));
    }

    let mut found_properties: HashSet<String> = HashSet::new();

    for item in &parsed.list {
        let KeyValue {
            key_ident, value, ..
        } = item;
        let key = key_ident.to_string();
        let expected_type = match expected_properties.get(&*key) {
            None => {
                return Err(syn::Error::new(
                    key_ident.span(),
                    format!("Unknown property: {}", key),
                ))
            }
            Some(ty) => ty,
        };

        if found_properties.contains(&key) {
            return Err(syn::Error::new(
                key_ident.span(),
                format!("Duplicated property: {}", key),
            ));
        }

        if !expected_type.matches(value) {
            return Err(syn::Error::new(
                key_ident.span(),
                format!(
                    "Invalid type for property: {}\nExpected: {}",
                    key, expected_type.type_
                ),
            ));
        }

        found_properties.insert(key);
    }

    for expected_property in expected_properties {
        if expected_property.1.optional {
            continue;
        }

        if !found_properties.contains(&*expected_property.0) {
            return Err(syn::Error::new(
                parsed.attribute_ident.span(),
                format!("Missing property: {}", expected_property.0),
            ));
        }
    }

    Ok(parsed
        .list
        .into_iter()
        .fold(HashMap::new(), |mut acc, KeyValue { key_ident, value }| {
            acc.insert(key_ident.to_string(), value);
            acc
        }))
}

macro_rules! get_str {
    ($val:expr) => {
        match $val {
            Some(syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Str(name),
                ..
            })) => name,
            _ => unreachable!(),
        }
    };
}

macro_rules! get_str_opt {
    ($val:expr) => {
        match $val {
            None => None,
            Some(syn::Expr::Lit(syn::ExprLit {
                lit: syn::Lit::Str(name),
                ..
            })) => Some(name),
            _ => unreachable!(),
        }
    };
}

macro_rules! get_path {
    ($val:expr) => {
        match $val {
            Some(syn::Expr::Path(syn::ExprPath { path, .. })) => path,
            _ => unreachable!(),
        }
    };
}

macro_rules! get_path_opt {
    ($val:expr) => {
        match $val {
            None => None,
            Some(syn::Expr::Path(syn::ExprPath { path, .. })) => Some(path),
            _ => unreachable!(),
        }
    };
}
