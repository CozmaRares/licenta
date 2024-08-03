use std::collections::{HashMap, HashSet};

use crate::attribute::{AttributeList, KeyValue};

//#[regex_token(
//    name = "Number",
//    regex = r"-?[0-9]+",
//    transformer_fn = "match_number",
//    kind = "i64"
//)]
//#[ignore_pattern(regex = r"\s+")]

#[derive(Debug)]
pub struct TokenExactMatch {
    name: String,
    pattern: String,
}

#[derive(Debug)]
pub struct TokenRegex {
    name: String,
    regex: String,
    transformer_fn: String,
    kind: String,
}

#[derive(Debug)]
pub struct TokenIgnore {
    regex: String,
}

#[derive(Debug)]
pub enum Token {
    ExactMatch(TokenExactMatch),
    Regex(TokenRegex),
    Ignore(TokenIgnore),
}

impl Token {
    //#[exact_token(name = "Minus", pattern = "-")]
    pub fn exact_token(tokens: proc_macro2::TokenStream) -> syn::Result<Self> {
        let mut expected_properties = HashSet::new();
        expected_properties.insert("name".to_string());
        expected_properties.insert("pattern".to_string());

        let AttributeList { pairs, .. } = AttributeList::prepare_token_info(
            tokens,
            "exact_token".to_string(),
            expected_properties,
        )?;

        let map = pairs
            .iter()
            .fold(HashMap::new(), |mut acc, KeyValue { name, value }| {
                acc.insert(name.content.clone(), value);
                acc
            });

        return Ok(Token::ExactMatch(TokenExactMatch {
            name: map.get("name").unwrap().to_string(),
            pattern: map.get("pattern").unwrap().to_string(),
        }));
    }
}
