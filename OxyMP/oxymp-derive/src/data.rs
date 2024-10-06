use crate::tokens::TokenInfo;

pub struct MacroData {
    pub parser_ident: proc_macro2::Ident,
    pub tokens: Vec<TokenInfo>,
    pub visibility: proc_macro2::TokenStream,
    pub simple_types: bool,
}
