use change_case::{pascal_case, snake_case};
use quote::format_ident;

pub mod tokens {
    use super::*;

    pub fn enum_ident(name: &str) -> proc_macro2::Ident {
        format_ident!("{}", pascal_case(name))
    }

    pub fn struct_ident(name: &str) -> proc_macro2::Ident {
        format_ident!("Token{}", pascal_case(name))
    }
}

pub mod parser {
    use super::*;

    pub fn rule_struct_ident(name: &str) -> proc_macro2::Ident {
        format_ident!("{}", pascal_case(name))
    }

    pub fn rule_method_ident(rule: &str) -> proc_macro2::Ident {
        format_ident!("{}", snake_case(rule))
    }

    pub fn choice_ident(rule: &str, choice_idx: usize) -> proc_macro2::Ident {
        format_ident!("{}Choice{}", pascal_case(rule), choice_idx)
    }

    pub fn idx_ident(idx: usize) -> proc_macro2::Ident {
        format_ident!("_{}", idx)
    }
}

pub fn base_ident(name: &str) -> proc_macro2::Ident {
    format_ident!("{}", name)
}
