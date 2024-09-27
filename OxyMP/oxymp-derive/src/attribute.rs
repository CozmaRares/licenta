use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
};

#[derive(Debug)]
pub struct Spanned<T>
where
    T: ?Sized,
{
    pub content: Rc<T>,
    pub span: proc_macro2::Span,
}

#[derive(Debug)]
pub struct NameValue {
    pub name: Spanned<str>,
    pub value: Rc<str>,
}

impl Parse for NameValue {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name: proc_macro2::Ident = input.parse()?;
        let _eq: syn::Token![=] = input.parse()?;
        let value: syn::LitStr = input.parse()?;
        return Ok(NameValue {
            name: Spanned {
                content: name.to_string().into(),
                span: name.span(),
            },
            value: value.value().into(),
        });
    }
}

#[derive(Debug)]
pub struct AttributeList {
    pub attr: Spanned<str>,
    pub pairs: Vec<NameValue>,
}

impl Parse for AttributeList {
    /// Parse `#[attr(a = "1", b = "2", ...)]`
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let _hash: syn::Token![#] = input.parse()?;

        let content_bracketed;
        let _bracketed = syn::bracketed!(content_bracketed in input);

        let attr: proc_macro2::Ident = content_bracketed.parse()?;

        let content_parenthesized;
        let _parenthesized = syn::parenthesized!(content_parenthesized in content_bracketed);

        type CommaSeparated = Punctuated<NameValue, syn::Token![,]>;
        let pairs = CommaSeparated::parse_terminated(&content_parenthesized)?;

        return Ok(AttributeList {
            attr: Spanned {
                content: attr.to_string().into(),
                span: attr.span(),
            },
            pairs: pairs.into_iter().collect(),
        });
    }
}

#[derive(Debug)]
pub struct AttributeNameValue(pub NameValue);

impl Parse for AttributeNameValue {
    /// Parse `#[attr = "1"]`
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let _hash: syn::Token![#] = input.parse()?;

        let content_bracketed;
        let _bracketed = syn::bracketed!(content_bracketed in input);

        let key_value: NameValue = content_bracketed.parse()?;

        return Ok(AttributeNameValue(key_value));
    }
}

impl AttributeList {
    pub fn prepare_token_info(
        tokens: proc_macro2::TokenStream,
        expected_attribute_name: String,
        expected_properties: HashSet<String>,
    ) -> syn::Result<HashMap<String, String>> {
        let parsed_attribute: AttributeList = syn::parse2(tokens)?;

        if *parsed_attribute.attr.content != expected_attribute_name {
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

        return Ok(parsed_attribute.pairs.iter().fold(
            HashMap::new(),
            |mut acc, NameValue { name, value }| {
                acc.insert(name.content.to_string(), value.to_string());
                acc
            },
        ));
    }
}
