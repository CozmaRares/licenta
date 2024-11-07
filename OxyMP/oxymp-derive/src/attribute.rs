use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use quote::ToTokens;
use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
};

use crate::tokens::{ExactToken, IgnorePattern, RegexToken, TokenInfo};

struct Attribute<T>(T)
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

struct KeyValue<T>
where
    T: Parse,
{
    key_ident: syn::Ident,
    value: T,
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

type CommaSeparated<T> = Punctuated<T, syn::Token![,]>;

struct AttributeList<T> {
    attribute_ident: syn::Ident,
    list: Vec<T>,
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

enum PropertyType {
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

struct ExpectedProperty {
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
    fn new(type_: PropertyType) -> ExpectedProperty {
        ExpectedProperty {
            type_,
            optional: false,
        }
    }

    fn optional(mut self) -> ExpectedProperty {
        self.optional = true;
        self
    }
}

fn fit_attribute_list(
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
            Some(syn::Expr::Path(syn::ExprPath { path, .. })) => path,
            _ => unreachable!(),
        }
    };
}

//#[exact_token(name = "Minus", pattern = "-")]
pub fn parse_exact_token(tokens: proc_macro2::TokenStream) -> syn::Result<TokenInfo> {
    let mut expected_properties = HashMap::new();
    expected_properties.insert("name", ExpectedProperty::new(PropertyType::LitStr));
    expected_properties.insert("pattern", ExpectedProperty::new(PropertyType::LitStr));

    let mut attr = fit_attribute_list(tokens, "exact_token", expected_properties)?;
    let name = get_str!(attr.remove("name")).value().into();
    let pattern = get_str!(attr.remove("pattern")).value().into();

    Ok(TokenInfo::Exact(ExactToken { name, pattern }))
}

//#[regex_token(name = "Number", regex = r"-?[0-9]+", transformer_fn = "match_number", kind = "i64")]
pub fn parse_regex_token(tokens: proc_macro2::TokenStream) -> syn::Result<TokenInfo> {
    let mut expected_properties = HashMap::new();
    expected_properties.insert("name", ExpectedProperty::new(PropertyType::LitStr));
    expected_properties.insert("regex", ExpectedProperty::new(PropertyType::LitStr));
    expected_properties.insert("transformer_fn", ExpectedProperty::new(PropertyType::Path));
    expected_properties.insert("kind", ExpectedProperty::new(PropertyType::Path));

    let mut attr = fit_attribute_list(tokens, "regex_token", expected_properties)?;
    let name = get_str!(attr.remove("name")).value().into();
    let regex = get_str!(attr.remove("regex")).value().into();
    let transformer_fn = get_path!(attr.remove("transformer_fn")).into_token_stream().into();
    let kind = get_path!(attr.remove("kind")).into_token_stream().into();

    Ok(TokenInfo::Regex(RegexToken {
        name,
        regex,
        transformer_fn,
        kind,
    }))
}

//#[ignore_pattern(regex = r"\s+")]
pub fn parse_ignore_pattern(tokens: proc_macro2::TokenStream) -> syn::Result<TokenInfo> {
    let mut expected_properties = HashMap::new();
    expected_properties.insert("regex", ExpectedProperty::new(PropertyType::LitStr));

    let mut attr = fit_attribute_list(tokens, "ignore_pattern", expected_properties)?;
    let regex = get_str!(attr.remove("regex")).value().into();

    Ok(TokenInfo::Ignore(IgnorePattern { regex }))
}

//#[grammar = "abc"]
pub fn parse_grammar_attribute(
    input: proc_macro2::TokenStream,
) -> syn::Result<(proc_macro2::Span, Rc<str>)> {
    let kv: Attribute<KeyValue<syn::LitStr>> = syn::parse2(input)?;
    let Attribute(KeyValue { key_ident, value }) = kv;
    let key = key_ident.to_string();
    if &*key != "grammar" {
        return Err(syn::Error::new(key_ident.span(), "Wrong attribute"));
    }
    Ok((key_ident.span(), value.value().into()))
}

//#[depth_limit = 123]
pub fn parse_depth_limit_attr(input: proc_macro2::TokenStream) -> syn::Result<usize> {
    let kv: Attribute<KeyValue<syn::LitInt>> = syn::parse2(input)?;
    let Attribute(KeyValue { key_ident, value }) = kv;
    let key = key_ident.to_string();
    if &*key != "depth_limit" {
        return Err(syn::Error::new(key_ident.span(), "Wrong attribute"));
    }
    Ok(value.base10_parse()?)
}
