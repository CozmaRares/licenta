pub mod attributes;

use quote::ToTokens;

use crate::{
    attribute::{
        parse_depth_limit_attr, parse_exact_pattern, parse_ignore_pattern, parse_regex_pattern,
        parse_simple_types_attr,
    },
    tokens::TokenInfo,
};

pub struct MacroData {
    pub parser_ident: proc_macro2::Ident,
    pub tokens: Vec<TokenInfo>,
    pub visibility: proc_macro2::TokenStream,
    pub simple_types: bool,
    pub depth_limit: usize,
}

fn parse_token_attrs(
    exact_patterns: Vec<(usize, proc_macro2::TokenStream)>,
    regex_patterns: Vec<(usize, proc_macro2::TokenStream)>,
    ignore_patterns: Vec<(usize, proc_macro2::TokenStream)>,
) -> syn::Result<Vec<TokenInfo>> {
    let exact_patterns = exact_patterns
        .into_iter()
        .map(|(idx, token)| parse_exact_pattern(token).map(|t| (idx, t)))
        .collect::<Result<Vec<_>, _>>()?;
    let regex_patterns = regex_patterns
        .into_iter()
        .map(|(idx, token)| parse_regex_pattern(token).map(|t| (idx, t)))
        .collect::<Result<Vec<_>, _>>()?;
    let ignore_patterns = ignore_patterns
        .into_iter()
        .map(|(idx, token)| parse_ignore_pattern(token).map(|t| (idx, t)))
        .collect::<Result<Vec<_>, _>>()?;

    let mut token_info = Vec::new();
    token_info.extend(regex_patterns);
    token_info.extend(exact_patterns);
    token_info.extend(ignore_patterns);

    token_info.sort_by(|a, b| a.0.cmp(&b.0));

    let token_info = token_info.into_iter().map(|(_, t)| t).collect();

    Ok(token_info)
}

impl MacroData {
    pub fn new(
        ast: syn::DeriveInput,
        exact_patterns: Vec<(usize, proc_macro2::TokenStream)>,
        regex_patterns: Vec<(usize, proc_macro2::TokenStream)>,
        ignore_patterns: Vec<(usize, proc_macro2::TokenStream)>,
        simple_types: Option<proc_macro2::TokenStream>,
        depth_limit: Option<proc_macro2::TokenStream>,
    ) -> syn::Result<MacroData> {
        let tokens = parse_token_attrs(exact_patterns, regex_patterns, ignore_patterns)?;

        let simple_types = match simple_types {
            None => false,
            Some(stream) => parse_simple_types_attr(stream).map(|_| true)?,
        };

        let depth_limit = match depth_limit {
            None => 10,
            Some(stream) => parse_depth_limit_attr(stream)?,
        };

        Ok(MacroData {
            parser_ident: ast.ident,
            tokens,
            visibility: ast.vis.into_token_stream(),
            simple_types,
            depth_limit,
        })
    }
}
