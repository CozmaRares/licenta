use std::collections::HashSet;

use crate::attribute::AttributeList;

//#[ignore_pattern(regex = r"\s+")]

#[derive(Debug)]
pub struct ExactToken {
    name: String,
    pattern: String,
}

#[derive(Debug)]
pub struct RegexToken {
    name: String,
    regex: String,
    transformer_fn: String,
    kind: String,
}

#[derive(Debug)]
pub struct IgnorePattern {
    regex: String,
}

#[derive(Debug)]
pub enum TokenInfo {
    Exact(ExactToken),
    Regex(RegexToken),
    Ignore(IgnorePattern),
}

impl TokenInfo {
    //#[exact_token(name = "Minus", pattern = "-")]
    pub fn exact_token(tokens: proc_macro2::TokenStream) -> syn::Result<Self> {
        let mut expected_properties = HashSet::new();
        expected_properties.insert("name".to_string());
        expected_properties.insert("pattern".to_string());

        let map = AttributeList::prepare_token_info(
            tokens,
            "exact_token".to_string(),
            expected_properties,
        )?;

        return Ok(TokenInfo::Exact(ExactToken {
            name: map.get("name").unwrap().to_string(),
            pattern: map.get("pattern").unwrap().to_string(),
        }));
    }

    //#[regex_token(name = "Number", regex = r"-?[0-9]+", transformer_fn = "match_number", kind = "i64")]
    pub fn regex_token(tokens: proc_macro2::TokenStream) -> syn::Result<Self> {
        let mut expected_properties = HashSet::new();
        expected_properties.insert("name".to_string());
        expected_properties.insert("regex".to_string());
        expected_properties.insert("transformer_fn".to_string());
        expected_properties.insert("kind".to_string());

        let map = AttributeList::prepare_token_info(
            tokens,
            "regex_token".to_string(),
            expected_properties,
        )?;

        return Ok(TokenInfo::Regex(RegexToken {
            name: map.get("name").unwrap().to_string(),
            regex: map.get("regex").unwrap().to_string(),
            transformer_fn: map.get("transformer_fn").unwrap().to_string(),
            kind: map.get("kind").unwrap().to_string(),
        }));
    }

    //#[ignore_pattern(regex = r"\s+")]
    pub fn ignore_pattern(tokens: proc_macro2::TokenStream) -> syn::Result<Self> {
        let mut expected_properties = HashSet::new();
        expected_properties.insert("regex".to_string());

        let map = AttributeList::prepare_token_info(
            tokens,
            "ignore_pattern".to_string(),
            expected_properties,
        )?;

        return Ok(TokenInfo::Ignore(IgnorePattern {
            regex: map.get("regex").unwrap().to_string(),
        }));
    }
}
