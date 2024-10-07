use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
};

use crate::tokens::{ExactToken, IgnorePattern, RegexToken, TokenInfo};

#[derive(Debug)]
struct Spanned<T>
where
    T: ?Sized,
{
    content: Rc<T>,
    span: proc_macro2::Span,
}

#[derive(Debug)]
struct NameValue {
    name: Spanned<str>,
    value: Rc<str>,
}

impl Parse for NameValue {
    fn parse(input: ParseStream) -> syn::Result<NameValue> {
        let name: proc_macro2::Ident = input.parse()?;
        let _eq: syn::Token![=] = input.parse()?;
        let value: syn::LitStr = input.parse()?;

        Ok(NameValue {
            name: Spanned {
                content: name.to_string().into(),
                span: name.span(),
            },
            value: value.value().into(),
        })
    }
}

#[derive(Debug)]
struct NameValueUsize {
    name: Spanned<str>,
    value: usize,
}

impl Parse for NameValueUsize {
    fn parse(input: ParseStream) -> syn::Result<NameValueUsize> {
        let name: proc_macro2::Ident = input.parse()?;
        let _eq: syn::Token![=] = input.parse()?;
        let value: syn::LitInt = input.parse()?;

        let value: usize = match value.base10_parse() {
            Ok(v) => v,
            Err(_) => return Err(syn::Error::new(value.span(), "Value is not a usize")),
        };

        Ok(NameValueUsize {
            name: Spanned {
                content: name.to_string().into(),
                span: name.span(),
            },
            value,
        })
    }
}


struct AttributeList {
    attr: Spanned<str>,
    pairs: Vec<NameValue>,
}

impl Parse for AttributeList {
    /// Parse `#[attr(a = "1", b = "2", ...)]`
    fn parse(input: ParseStream) -> syn::Result<AttributeList> {
        let _hash: syn::Token![#] = input.parse()?;

        let content_bracketed;
        let _bracketed = syn::bracketed!(content_bracketed in input);

        let attr: proc_macro2::Ident = content_bracketed.parse()?;

        let content_parenthesized;
        let _parenthesized = syn::parenthesized!(content_parenthesized in content_bracketed);

        type CommaSeparated = Punctuated<NameValue, syn::Token![,]>;
        let pairs = CommaSeparated::parse_terminated(&content_parenthesized)?;

        Ok(AttributeList {
            attr: Spanned {
                content: attr.to_string().into(),
                span: attr.span(),
            },
            pairs: pairs.into_iter().collect(),
        })
    }
}
#[derive(Debug)]
struct AttributeNameValue<T>(T)
where
    T: Parse;

impl<T> Parse for AttributeNameValue<T>
where
    T: Parse,
{
    /// Parse `#[attr = "1"]`
    fn parse(input: ParseStream) -> syn::Result<AttributeNameValue<T>> {
        let _hash: syn::Token![#] = input.parse()?;

        let content_bracketed;
        let _bracketed = syn::bracketed!(content_bracketed in input);

        let key_value: T = content_bracketed.parse()?;

        Ok(AttributeNameValue(key_value))
    }
}

pub fn fit_attribute_list(
    tokens: proc_macro2::TokenStream,
    expected_attribute_name: &str,
    expected_properties: HashSet<&str>,
) -> syn::Result<HashMap<Rc<str>, Rc<str>>> {
    let parsed_attribute: AttributeList = syn::parse2(tokens)?;

    if *parsed_attribute.attr.content != *expected_attribute_name {
        return Err(syn::Error::new(
            parsed_attribute.attr.span,
            format!(
                "Wrong attribute\nExpected: {}\nGot: {}",
                expected_attribute_name, parsed_attribute.attr.content
            ),
        ));
    }

    let mut found_properties: HashSet<&str> = HashSet::new();

    for pair in &parsed_attribute.pairs {
        let NameValue { name, .. } = pair;

        if !expected_properties.contains(&*name.content) {
            return Err(syn::Error::new(
                name.span,
                format!("Unknown property: {}", name.content),
            ));
        }

        if found_properties.contains(&*name.content) {
            return Err(syn::Error::new(
                name.span,
                format!("Duplicated property: {}", name.content),
            ));
        }

        found_properties.insert(&name.content);
    }

    for expected_property in expected_properties {
        if !found_properties.contains(&*expected_property) {
            return Err(syn::Error::new(
                parsed_attribute.attr.span,
                format!("Missing property: {}", expected_property),
            ));
        }
    }

    Ok(parsed_attribute
        .pairs
        .iter()
        .fold(HashMap::new(), |mut acc, NameValue { name, value }| {
            acc.insert(name.content.clone(), value.clone());
            acc
        }))
}

//#[exact_token(name = "Minus", pattern = "-")]
pub fn parse_exact_token(tokens: proc_macro2::TokenStream) -> syn::Result<TokenInfo> {
    let mut expected_properties = HashSet::new();
    expected_properties.insert("name");
    expected_properties.insert("pattern");

    let mut attr = fit_attribute_list(tokens, "exact_token", expected_properties)?;
    let name = attr.remove("name").unwrap();
    let pattern = attr.remove("pattern").unwrap();

    Ok(TokenInfo::Exact(ExactToken { name, pattern }))
}

//#[regex_token(name = "Number", regex = r"-?[0-9]+", transformer_fn = "match_number", kind = "i64")]
pub fn parse_regex_token(tokens: proc_macro2::TokenStream) -> syn::Result<TokenInfo> {
    let mut expected_properties = HashSet::new();
    expected_properties.insert("name");
    expected_properties.insert("regex");
    expected_properties.insert("transformer_fn");
    expected_properties.insert("kind");

    let mut attr = fit_attribute_list(tokens, "regex_token", expected_properties)?;
    let name = attr.remove("name").unwrap();
    let regex = attr.remove("regex").unwrap();
    let transformer_fn = attr.remove("transformer_fn").unwrap();
    let kind = attr.remove("kind").unwrap();

    Ok(TokenInfo::Regex(RegexToken {
        name,
        regex,
        transformer_fn,
        kind,
    }))
}

//#[ignore_pattern(regex = r"\s+")]
pub fn parse_ignore_pattern(tokens: proc_macro2::TokenStream) -> syn::Result<TokenInfo> {
    let mut expected_properties = HashSet::new();
    expected_properties.insert("regex");

    let mut attr = fit_attribute_list(tokens, "ignore_pattern", expected_properties)?;
    let regex = attr.remove("regex").unwrap();

    Ok(TokenInfo::Ignore(IgnorePattern { regex }))
}

//#[grammar = "abc"]
pub fn parse_grammar_attribute(
    input: proc_macro2::TokenStream,
) -> syn::Result<(proc_macro2::Span, Rc<str>)> {
    let AttributeNameValue(NameValue { name, value }) = syn::parse2(input)?;
    if &*name.content != "grammar" {
        return Err(syn::Error::new(name.span, "Wrong attribute"));
    }

    Ok((name.span, value))
}

//#[depth_limit = 123]
pub fn parse_depth_limit_attr(input: proc_macro2::TokenStream) -> syn::Result<usize> {
    let AttributeNameValue(NameValueUsize { name, value }) = syn::parse2(input)?;
    if &*name.content != "depth_limit" {
        return Err(syn::Error::new(name.span, "Wrong attribute"));
    }
    Ok(value)
}
