use std::rc::Rc;

#[derive(Debug)]
pub struct ExactToken {
    pub name: Rc<str>,
    pub pattern: Rc<str>,
}

#[derive(Debug)]
pub struct RegexToken {
    pub name: Rc<str>,
    pub regex: Rc<str>,
    pub transformer_fn: Rc<proc_macro2::TokenStream>,
    pub kind: Rc<proc_macro2::TokenStream>,
}

#[derive(Debug)]
pub struct IgnorePattern {
    pub regex: Rc<str>,
}

#[derive(Debug)]
pub enum TokenInfo {
    Exact(ExactToken),
    Regex(RegexToken),
    Ignore(IgnorePattern),
}
