use syn::{
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
};

#[derive(Debug)]
pub struct Spanned<T> {
    content: T,
    span: proc_macro2::Span,
}

#[derive(Debug)]
pub struct KeyValue {
    pub name: Spanned<String>,
    pub value: Spanned<String>,
}

impl Parse for KeyValue {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let name: syn::Ident = input.parse()?;
        let _eq: syn::Token![=] = input.parse()?;
        let value: syn::LitStr = input.parse()?;
        Ok(KeyValue {
            name: Spanned {
                content: name.to_string(),
                span: name.span(),
            },
            value: Spanned {
                content: value.value(),
                span: value.span(),
            },
        })
    }
}

#[derive(Debug)]
pub struct AttributeList {
    pub attr: Spanned<String>,
    pub values: Vec<KeyValue>,
}

impl Parse for AttributeList {
    /// Parse `#[attr(a = "1", b = "2", ...)]`
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let _hash: syn::Token![#] = input.parse()?;

        let content_bracketed;
        let _bracketed = syn::bracketed!(content_bracketed in input);

        let attr: syn::Ident = content_bracketed.parse()?;

        let content_parenthesized;
        let _parenthesized = syn::parenthesized!(content_parenthesized in content_bracketed);

        type A = Punctuated<KeyValue, syn::Token![,]>;
        let values = A::parse_terminated(&content_parenthesized)?;

        Ok(AttributeList {
            attr: Spanned {
                content: attr.to_string(),
                span: attr.span(),
            },
            values: values.into_iter().collect(),
        })
    }
}

#[derive(Debug)]
pub struct AttributeKeyValue(pub KeyValue);

impl Parse for AttributeKeyValue {
    /// Parse `#[attr = "1"]`
    fn parse(input: ParseStream) -> syn::Result<Self> {
        let _hash: syn::Token![#] = input.parse()?;

        let content_bracketed;
        let _bracketed = syn::bracketed!(content_bracketed in input);

        let key_value: KeyValue = content_bracketed.parse()?;

        Ok(AttributeKeyValue(key_value))
    }
}
