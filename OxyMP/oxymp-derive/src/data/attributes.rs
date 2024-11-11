use std::collections::HashMap;

use quote::ToTokens;

pub struct MacroAttributes {
    pub exact_patterns: Vec<(usize,proc_macro2::TokenStream)>,
    pub regex_patterns: Vec<(usize,proc_macro2::TokenStream)>,
    pub ignore_patterns: Vec<(usize,proc_macro2::TokenStream)>,
    pub grammar: Vec<proc_macro2::TokenStream>,
    pub simple_types: Option<proc_macro2::TokenStream>,
    pub depth_limit: Option<proc_macro2::TokenStream>,
    pub sync_tokens: Option<Vec<proc_macro2::TokenStream>>,
}

fn group_attrs(
    attrs: &[syn::Attribute],
) -> HashMap<String, Vec<(usize, proc_macro2::TokenStream)>> {
    attrs
        .iter()
        .enumerate()
        .fold(HashMap::new(), |mut acc, (idx, attr)| {
            let attr_ident = attr.path().segments.first().unwrap().ident.to_string();

            acc.entry(attr_ident)
                .or_default()
                .extend(vec![(idx, attr.to_token_stream())]);

            acc
        })
}

fn from_map(
    ast_span: proc_macro2::Span,
    mut attributes: HashMap<String, Vec<(usize, proc_macro2::TokenStream)>>,
) -> syn::Result<MacroAttributes> {
    let exact_patterns = attributes.remove("exact_pattern").unwrap_or_default();
    let regex_patterns = attributes.remove("regex_pattern").unwrap_or_default();
    let ignore_patterns = attributes.remove("ignore_pattern").unwrap_or_default();

    if exact_patterns.is_empty() && regex_patterns.is_empty() && ignore_patterns.is_empty() {
        return Err(syn::Error::new(ast_span, "Missing token definitions."));
    }

    let grammar = match attributes.remove("grammar") {
        Some(v) => v,
        None => return Err(syn::Error::new(ast_span, "Missing grammar rules.")),
    }
    .into_iter()
    .map(|item| item.1)
    .collect();

    let simple_types = match attributes.remove("simple_types") {
        None => None,
        Some(mut v) => match v.len() {
            0 => None,
            1 => Some(v.remove(0)),
            _ => {
                return Err(syn::Error::new(
                    ast_span,
                    "Multiple '#[simple_types]' attributes.",
                ))
            }
        },
    }
    .map(|item| item.1);

    let depth_limit = match attributes.remove("depth_limit") {
        None => None,
        Some(mut v) => match v.len() {
            0 => None,
            1 => Some(v.remove(0)),
            _ => {
                return Err(syn::Error::new(
                    ast_span,
                    "Multiple '#[depth_limit]' attributes.",
                ))
            }
        },
    }
    .map(|item| item.1);

    let sync_tokens = attributes
        .remove("sync_tokens")
        .map(|v| v.into_iter().map(|item| item.1).collect());

    if !attributes.is_empty() {
        return Err(syn::Error::new(
            ast_span,
            format!(
                "Unknown attributes: [{}]",
                attributes
                    .keys()
                    .fold(String::new(), |acc, key| format!("{}, {}", acc, key))
            ),
        ));
    }

    Ok(MacroAttributes {
        exact_patterns,
        regex_patterns,
        ignore_patterns,
        grammar,
        simple_types,
        depth_limit,
        sync_tokens,
    })
}

impl MacroAttributes {
    pub fn new(
        ast_span: proc_macro2::Span,
        attrs: &[syn::Attribute],
    ) -> syn::Result<MacroAttributes> {
        let map = group_attrs(attrs);
        from_map(ast_span, map)
    }
}
