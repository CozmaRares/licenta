#![allow(non_snake_case)]

use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use proc_macro2::{Ident, TokenStream};
use quote::quote;

use crate::{
    data::MacroData,
    grammar::{GrammarNode, GrammarNodeContent},
    idents::{parser, tokens},
    symbols::{get_def, Symbol},
};

pub fn generate_parser(data: &MacroData, rules: &HashMap<Rc<str>, GrammarNode>) -> TokenStream {
    let defs = generate_static_defs(data);
    let ast = generate_ast(rules, data);
    let parser_impl = generate_impl(rules, data);

    quote! {
        #defs
        #ast
        #parser_impl
    }
}

fn generate_static_defs(data: &MacroData) -> TokenStream {
    let visibility = &data.visibility;

    let _Debug = get_def(Symbol::DeriveDebug, data.simple_types);
    let _Clone = get_def(Symbol::DeriveClone, data.simple_types);
    let _usize = get_def(Symbol::CoreUsize, data.simple_types);
    let _Rc = get_def(Symbol::Rc, data.simple_types);
    let _Deref = get_def(Symbol::Deref, data.simple_types);
    let _Option = get_def(Symbol::Option, data.simple_types);
    let _Result = get_def(Symbol::Result, data.simple_types);
    let _String = get_def(Symbol::String, data.simple_types);

    quote! {
        #[derive(#_Debug, #_Clone)]
        #visibility struct ParserInput {
           #visibility tokens: #_Rc<[Token]>,
           #visibility current: #_usize,
        }
        impl<T> From<T> for ParserInput
        where
            T: #_Deref<Target = [Token]>,
        {
            fn from(tokens: T) -> ParserInput {
                ParserInput {
                    tokens: tokens.deref().into(),
                    current: 0
                }
            }
        }
        impl ParserInput {
            #visibility fn get_current(&self) -> #_Option<&Token> {
                self.tokens.get(self.current)
            }

            #visibility fn increment(&self) -> ParserInput {
                ParserInput {
                    tokens: self.tokens.clone(),
                    current: self.current + 1,
                }
            }
        }
        #visibility type ParserState<T> = #_Result<(ParserInput, T), ParseError>;

        #[derive(#_Debug)]
        #visibility struct ParseError {
            #visibility place: #_String,
            #visibility reason: #_String,
        }
    }
}

fn generate_ast(rules: &HashMap<Rc<str>, GrammarNode>, data: &MacroData) -> TokenStream {
    let visibility = &data.visibility;

    let _Debug = get_def(Symbol::DeriveDebug, data.simple_types);

    let structs = rules.iter().map(|(rule, node)| {
        let ASTNode {
            main_struct,
            external_choices,
        } = generate_ast_node(rule, node, data);

        let external_choices = match external_choices {
            None => quote! {},
            Some(structs) => quote! { #(#structs)* },
        };

        let rule_ident = parser::rule_ident(rule);

        quote! {
            #external_choices
            #[derive(#_Debug)]
            #visibility struct #rule_ident (
                #main_struct
            );
        }
    });

    let enum_entries = rules.keys().map(|rule| {
        let ident = parser::rule_ident(rule);
        quote! { #ident(#ident) }
    });

    quote! {
        #(#structs)*
        #[derive(#_Debug)]
        #visibility enum AST {
            #(#enum_entries),*
        }
    }
}

struct ASTNode {
    main_struct: TokenStream,
    external_choices: Option<Vec<TokenStream>>,
}

fn generate_ast_node(rule: &str, node: &GrammarNode, data: &MacroData) -> ASTNode {
    let visibility = &data.visibility;

    let _Box = get_def(Symbol::Box, data.simple_types);
    let _Debug = get_def(Symbol::DeriveDebug, data.simple_types);
    let _Option = get_def(Symbol::Option, data.simple_types);

    match &node.content {
        GrammarNodeContent::Rule(rule) => {
            let ident = parser::rule_ident(rule);
            ASTNode {
                main_struct: quote! {#_Box<#ident>},
                external_choices: None,
            }
        }
        GrammarNodeContent::Token(token) => {
            let ident = tokens::struct_ident(token);
            ASTNode {
                main_struct: quote! { #ident },
                external_choices: None,
            }
        }
        GrammarNodeContent::List(exprs) => {
            let defs = exprs.iter().map(|expr| generate_ast_node(rule, expr, data));
            let main_struct = defs.clone().map(|d| d.main_struct);
            let external_choices = defs.filter_map(|d| d.external_choices).flatten();

            ASTNode {
                main_struct: quote! { ( #(#main_struct),* ) },
                external_choices: Some(external_choices.collect()),
            }
        }
        GrammarNodeContent::Choice(choices, choice_idx) => {
            let defs = choices
                .iter()
                .map(|choice| generate_ast_node(rule, choice, data));
            let enum_entries = defs
                .clone()
                .map(|d| d.main_struct)
                .enumerate()
                .map(|(idx, s)| {
                    let idx_ident = parser::idx_ident(idx + 1);
                    quote! {
                        #idx_ident(#s)
                    }
                });

            let mut external_choices: Vec<_> =
                defs.filter_map(|d| d.external_choices).flatten().collect();
            let enum_ident = parser::choice_ident(rule, *choice_idx);
            external_choices.push(quote! {
                #[derive(#_Debug)]
                #visibility enum #enum_ident {
                    #(#enum_entries),*
                }
            });

            ASTNode {
                main_struct: quote! { #enum_ident },
                external_choices: Some(external_choices),
            }
        }
        GrammarNodeContent::Optional(opt) => {
            let generated = generate_ast_node(rule, opt, data);
            let main_struct = generated.main_struct;
            ASTNode {
                main_struct: quote! { #_Option<#main_struct> },
                external_choices: generated.external_choices,
            }
        }
    }
}

fn generate_impl(rules: &HashMap<Rc<str>, GrammarNode>, data: &MacroData) -> TokenStream {
    let parser_ident = &data.parser_ident;

    populate_first_terminal_cache(rules, data.depth_limit);

    NODE_FIRST_CACHE.with(|c| eprintln!("{:#?}", c));
    eprintln!("");
    RULE_FIRST_CACHE.with(|c| eprintln!("{:#?}", c));

    let methods = rules
        .iter()
        .map(|(rule, node)| generate_rule(rule, node, data));

    quote! {
        impl #parser_ident {
            #(#methods),*
        }
    }
}

fn generate_rule(rule: &str, node: &GrammarNode, data: &MacroData) -> TokenStream {
    let visibility = &data.visibility;

    let rule_ident = parser::rule_ident(rule);
    let defs = expand_node(rule, node, data);
    let toks = defs.0;
    let ident = defs.1;

    let _Ok = get_def(Symbol::Ok, data.simple_types);

    quote! {
        #visibility fn #rule_ident(inp: ParserInput) -> ParserState<#rule_ident> {
            #toks
            #_Ok((
                inp,
                #rule_ident(#ident)
            ))
        }
    }
}

fn expand_node(rule: &str, node: &GrammarNode, data: &MacroData) -> (TokenStream, Ident) {
    let node_ident = parser::idx_ident(node.index);
    let parser_ident = &data.parser_ident;

    let _Box = get_def(Symbol::Box, data.simple_types);
    let _None = get_def(Symbol::None, data.simple_types);
    let _Some = get_def(Symbol::Some, data.simple_types);
    let _Err = get_def(Symbol::Err, data.simple_types);
    let _Ok = get_def(Symbol::Ok, data.simple_types);

    let toks = match &node.content {
        GrammarNodeContent::Rule(rule) => {
            let rule_ident = parser::rule_ident(rule);
            quote! {
                let (inp, #node_ident) =
                    #parser_ident::#rule_ident(inp)
                        .map(|(remaining, ast)| (remaining, #_Box::new(ast)))?;
            }
        }
        GrammarNodeContent::Token(token) => {
            let token_enum_entry = tokens::enum_ident(token);
            let error_msg = format!("Expected a {}", token);
            quote! {
                let (inp, #node_ident) = match inp.get_current() {
                    #_None => #_Err(ParseError {
                        place: #rule.into(),
                        reason: "Input is empty".into(),
                    }),
                    #_Some(Token::#token_enum_entry(tok)) =>
                        #_Ok((inp.increment(), tok.clone())),
                    #_Some(tok) => #_Err(ParseError {
                        place: #rule.into(),
                        reason: #error_msg.into(),
                    })
                }?;
            }
        }
        GrammarNodeContent::List(exprs) => {
            let defs = exprs.iter().map(|expr| expand_node(rule, expr, data));
            let toks = defs.clone().map(|d| d.0);
            let idents = defs.map(|d| d.1);

            quote! {
                let (inp, #node_ident) = (|| {
                     #(#toks)*

                    #_Ok((
                        inp,
                        ( #(#idents),* )
                    ))
                })()?;
            }
        }
        GrammarNodeContent::Choice(choices, choice_idx) => {
            let defs = choices
                .iter()
                .map(|expr| expand_node(rule, expr, data))
                .enumerate()
                .map(|(idx, (toks, ident))| {
                    let idx_ident = parser::idx_ident(idx + 1);
                    let choice_ident = parser::choice_ident(rule, *choice_idx);

                    quote! {
                        let r: ParserState<_> = (|| {
                            #toks
                            #_Ok((inp, #ident))
                        })();
                        match r {
                            #_Ok((inp, ast)) => {
                                return #_Ok((inp, #choice_ident::#idx_ident(ast)));
                            }
                            #_Err(_) => {}
                        };
                    }
                });

            quote! {
                let (inp, #node_ident) =  (|| {
                     #(#defs)*

                    #_Err(ParseError {
                        place: #rule.into(),
                        reason: "All choices failed".into(),
                    })
                })()?;
            }
        }
        GrammarNodeContent::Optional(opt) => {
            let (toks, ident) = expand_node(rule, opt, data);

            quote! {
                let res: ParserState<_> = (|| {
                    let inp = inp.clone();
                    #toks
                    #_Ok((inp, #ident))
                })();
                let (inp, #node_ident) =  match res {
                    #_Ok((new_inp, ast)) => (new_inp, #_Some(ast)),
                    #_Err(_) => (inp, #_None),
                };
            }
        }
    };

    (toks, node_ident)
}

fn populate_first_terminal_cache(rules: &HashMap<Rc<str>, GrammarNode>, depth: usize) {
    rules.iter().for_each(|rule| {
        compute_rule_first(rule.0.clone(), depth, rules);
    });
}

thread_local! {
    static NODE_FIRST_CACHE: RefCell<HashMap<*const GrammarNode, Rc<HashSet<Rc<str>>>>> = RefCell::new(HashMap::new());
    static RULE_FIRST_CACHE: RefCell<HashMap<Rc<str>, Rc<HashSet<Rc<str>>>>> = RefCell::new(HashMap::new());
}


fn compute_rule_first(
    rule: Rc<str>,
    depth: usize,
    rules: &HashMap<Rc<str>, GrammarNode>,
) -> Rc<HashSet<Rc<str>>> {
    if depth == 0 {
        panic!("Reached depth limit when computing the first token for a grammar rule.")
    }

    if let Some(cached) = RULE_FIRST_CACHE.with(|c| c.borrow().get(&*rule).cloned()) {
        return cached;
    }

    let computed = compute_node_first(rules.get(&*rule).unwrap(), depth, rules);
    RULE_FIRST_CACHE.with(|c| c.borrow_mut().insert(rule, computed.clone()));
    computed
}

fn compute_node_first(
    node: &GrammarNode,
    depth: usize,
    rules: &HashMap<Rc<str>, GrammarNode>,
) -> Rc<HashSet<Rc<str>>> {
    if let Some(cached) =
        NODE_FIRST_CACHE.with(|c| c.borrow().get(&(node as *const GrammarNode)).cloned())
    {
        return cached;
    }

    let computed = match &node.content {
        GrammarNodeContent::Rule(rule) => compute_rule_first(rule.clone(), depth - 1, rules),
        GrammarNodeContent::Token(token) => {
            pipe!([token.clone()] => HashSet::from => Rc::new)
        }
        GrammarNodeContent::List(list) => compute_node_first(list.first().unwrap(), depth, rules),
        GrammarNodeContent::Optional(opt) => compute_node_first(opt, depth, rules),
        GrammarNodeContent::Choice(choices, _) => {
            let firsts: HashSet<_> = choices
                .iter()
                .map(|choice| compute_node_first(choice, depth, rules))
                .map(|choice| choice.iter().cloned().collect::<Vec<_>>())
                .flatten()
                .collect();

            pipe!(firsts => Rc::new)
        }
    };

    NODE_FIRST_CACHE.with(|c| {
        c.borrow_mut()
            .insert(node as *const GrammarNode, computed.clone())
    });
    computed
}
