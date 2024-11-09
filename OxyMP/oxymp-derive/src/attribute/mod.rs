#[macro_use]
mod base;

use std::{collections::HashMap, rc::Rc};

use quote::ToTokens;

use crate::tokens::{ExactToken, IgnorePattern, RegexToken, TokenInfo};
use base::*;

//#[exact_token(name = "Minus", pattern = "-")]
//#[exact_token(name = "Minus", pattern = "-", tier = Tier::Level)]
pub fn parse_exact_token(tokens: proc_macro2::TokenStream) -> syn::Result<TokenInfo> {
    let mut expected_properties = HashMap::new();
    expected_properties.insert("name", ExpectedProperty::new(PropertyType::LitStr));
    expected_properties.insert("pattern", ExpectedProperty::new(PropertyType::LitStr));
    expected_properties.insert("tier", ExpectedProperty::new(PropertyType::Path).optional());

    let mut attr = fit_attribute_list(tokens, "exact_token", expected_properties)?;
    let name = get_str!(attr.remove("name")).value().into();
    let pattern = get_str!(attr.remove("pattern")).value().into();
    let tier = get_path_opt!(attr.remove("tier")).map(|path| path.into_token_stream());

    Ok(TokenInfo::Exact(ExactToken {
        name,
        pattern,
        tier,
    }))
}
//#[regex_token(name = "Number", regex = r"-?[0-9]+", transformer_fn = "match_number", kind = "i64")]
//#[regex_token(name = "Number", regex = r"-?[0-9]+", transformer_fn = "match_number", kind = "i64", tier = Tier::Level)]
pub fn parse_regex_token(tokens: proc_macro2::TokenStream) -> syn::Result<TokenInfo> {
    let mut expected_properties = HashMap::new();
    expected_properties.insert("name", ExpectedProperty::new(PropertyType::LitStr));
    expected_properties.insert("regex", ExpectedProperty::new(PropertyType::LitStr));
    expected_properties.insert("transformer_fn", ExpectedProperty::new(PropertyType::Path));
    expected_properties.insert("kind", ExpectedProperty::new(PropertyType::Path));
    expected_properties.insert("tier", ExpectedProperty::new(PropertyType::Path).optional());

    let mut attr = fit_attribute_list(tokens, "regex_token", expected_properties)?;
    let name = get_str!(attr.remove("name")).value().into();
    let regex = get_str!(attr.remove("regex")).value().into();
    let transformer_fn = get_path!(attr.remove("transformer_fn")).into_token_stream();
    let kind = get_path!(attr.remove("kind")).into_token_stream();
    let tier = get_path_opt!(attr.remove("tier")).map(|path| path.into_token_stream());

    Ok(TokenInfo::Regex(RegexToken {
        name,
        regex,
        transformer_fn,
        kind,
        tier,
    }))
}

//#[ignore_pattern(regex = r"\s+")]
//#[ignore_pattern(regex = r"\s+", tier = Tier::Level)]
pub fn parse_ignore_pattern(tokens: proc_macro2::TokenStream) -> syn::Result<TokenInfo> {
    let mut expected_properties = HashMap::new();
    expected_properties.insert("regex", ExpectedProperty::new(PropertyType::LitStr));
    expected_properties.insert("tier", ExpectedProperty::new(PropertyType::Path).optional());

    let mut attr = fit_attribute_list(tokens, "ignore_pattern", expected_properties)?;
    let regex = get_str!(attr.remove("regex")).value().into();
    let tier = get_path_opt!(attr.remove("tier")).map(|path| path.into_token_stream());

    Ok(TokenInfo::Ignore(IgnorePattern { regex, tier }))
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
    value.base10_parse()
}
