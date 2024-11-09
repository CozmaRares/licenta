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

pub fn generate_impl(data: &MacroData, rules: &HashMap<Rc<str>, GrammarNode>) -> TokenStream {
    let parser_ident = &data.parser_ident;

    let methods = rules
        .iter()
        .map(|(rule, node)| generate_rule(rule, node, rules, data));

    quote! {
        impl #parser_ident {
            #(#methods)*
        }
    }
}

fn generate_rule(
    rule: &str,
    node: &GrammarNode,
    rules: &HashMap<Rc<str>, GrammarNode>,
    data: &MacroData,
) -> TokenStream {
    let visibility = &data.visibility;

    let rule_ident = parser::rule_ident(rule);
    let defs = expand_node(rule, node, rules, data, false);
    let toks = defs.0;
    let ident = defs.1;

    let _Ok = get_def(Symbol::Ok, data.simple_types);
    let _ParserInput = get_def(Symbol::UtilParserInput, data.simple_types);
    let _ParserState = get_def(Symbol::UtilParserState, data.simple_types);

    quote! {
        #visibility fn #rule_ident(inp: #_ParserInput<Token>) -> #_ParserState<Token, #rule_ident> {
            #toks
            #_Ok((
                inp,
                #rule_ident(#ident)
            ))
        }
    }
}

fn expand_node(
    rule: &str,
    node: &GrammarNode,
    rules: &HashMap<Rc<str>, GrammarNode>,
    data: &MacroData,
    needs_check: bool,
) -> (TokenStream, Ident) {
    let node_ident = parser::idx_ident(node.index);
    let parser_ident = &data.parser_ident;

    let _Box = get_def(Symbol::Box, data.simple_types);
    let _None = get_def(Symbol::None, data.simple_types);
    let _Some = get_def(Symbol::Some, data.simple_types);
    let _Err = get_def(Symbol::Err, data.simple_types);
    let _Ok = get_def(Symbol::Ok, data.simple_types);
    let _ParseError = get_def(Symbol::UtilParseError, data.simple_types);
    let _ParserState = get_def(Symbol::UtilParserState, data.simple_types);

    let toks = match &node.content {
        GrammarNodeContent::Rule(nested_rule) => {
            let rule_ident = parser::rule_ident(nested_rule);
            let first = compute_node_first(node, data.depth_limit, rules);
            let check = generate_token_check(rule, &first, data.simple_types, needs_check);

            quote! {
                #check
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
                    #_None => #_Err(#_ParseError {
                        place: #rule.into(),
                        reason: "Input is empty".into(),
                    }),
                    #_Some(Token::#token_enum_entry(tok)) =>
                        #_Ok((inp.increment(), tok.clone())),
                    #_Some(tok) => #_Err(#_ParseError {
                        place: #rule.into(),
                        reason: #error_msg.into(),
                    })
                }?;
            }
        }
        GrammarNodeContent::List(exprs) => {
            let defs = exprs
                .iter()
                .enumerate()
                .map(|(idx, expr)| expand_node(rule, expr, rules, data, idx != 0));
            let toks = defs.clone().map(|d| d.0);
            let idents = defs.map(|d| d.1);

            let first = compute_node_first(node, data.depth_limit, rules);
            let check = generate_token_check(rule, &first, data.simple_types, needs_check);

            quote! {
                #check
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
                .map(|expr| expand_node(rule, expr, rules, data, false))
                .enumerate()
                .map(|(idx, (toks, ident))| {
                    let idx_ident = parser::idx_ident(idx + 1);
                    let choice_ident = parser::choice_ident(rule, *choice_idx);

                    quote! {
                        let r: #_ParserState<_, _> = (|| {
                            #toks
                            #_Ok((inp, #ident))
                        })();
                        if let #_Ok((inp, ast)) = r {
                            return #_Ok((inp, #choice_ident::#idx_ident(ast)));
                        };
                    }
                });

            let first = compute_node_first(node, data.depth_limit, rules);
            let check = generate_token_check(rule, &first, data.simple_types, needs_check);

            quote! {
                #check
                let (inp, #node_ident) =  (|| {
                     #(#defs)*

                    #_Err(#_ParseError {
                        place: #rule.into(),
                        reason: "All choices failed".into(),
                    })
                })()?;
            }
        }
        GrammarNodeContent::Optional(opt) => {
            let (toks, ident) = expand_node(rule, opt, rules, data, false);

            let first = compute_node_first(node, data.depth_limit, rules);
            let check = generate_token_check(rule, &first, data.simple_types, needs_check);

            quote! {
                let res: #_ParserState<_, _> = (|| {
                    #check
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

type FirstTokens = HashSet<Rc<str>>;

#[derive(Clone)]
struct FirstCheck {
    tokens: Rc<FirstTokens>,
    nullable: bool,
}

impl FirstCheck {
    fn nullable(self) -> FirstCheck {
        FirstCheck {
            tokens: self.tokens,
            nullable: true,
        }
    }
}

thread_local! {
    static NODE_FIRST_CACHE: RefCell<HashMap<*const GrammarNode, FirstCheck>> = RefCell::new(HashMap::new());
    static RULE_FIRST_CACHE: RefCell<HashMap<Rc<str>, FirstCheck>> = RefCell::new(HashMap::new());
}

fn compute_rule_first(
    rule: Rc<str>,
    depth: usize,
    rules: &HashMap<Rc<str>, GrammarNode>,
) -> FirstCheck {
    if depth == 0 {
        panic!("Possible left recursion deteted! Reached depth limit when computing the first token for the rule: {}", rule);
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
) -> FirstCheck {
    if let Some(cached) =
        NODE_FIRST_CACHE.with(|c| c.borrow().get(&(node as *const GrammarNode)).cloned())
    {
        return cached;
    }

    let computed = match &node.content {
        GrammarNodeContent::Rule(rule) => compute_rule_first(rule.clone(), depth - 1, rules),
        GrammarNodeContent::Token(token) => FirstCheck {
            tokens: pipe!([token.clone()] => HashSet::from => Rc::new),
            nullable: false,
        },
        GrammarNodeContent::List(list) => {
            let mut firsts = HashSet::new();
            let mut nullable = true;

            for list_node in list {
                let first = compute_node_first(list_node, depth, rules);

                firsts.extend(<HashSet<Rc<str>> as Clone>::clone(&first.tokens).into_iter());

                if !first.nullable {
                    nullable = false;
                    break;
                }
            }

            FirstCheck {
                tokens: pipe!(firsts => Rc::new),
                nullable,
            }
        }
        GrammarNodeContent::Optional(opt) => compute_node_first(opt, depth, rules).nullable(),
        GrammarNodeContent::Choice(choices, _) => {
            let mut choices = choices
                .iter()
                .map(|choice| compute_node_first(choice, depth, rules));

            let firsts: HashSet<_> = choices
                .clone()
                .flat_map(|choice| choice.tokens.iter().cloned().collect::<Vec<_>>())
                .collect();

            let nullable = choices.any(|choice| choice.nullable);

            FirstCheck {
                tokens: pipe!(firsts => Rc::new),
                nullable,
            }
        }
    };

    NODE_FIRST_CACHE.with(|c| {
        c.borrow_mut()
            .insert(node as *const GrammarNode, computed.clone())
    });
    computed
}

fn generate_token_check(
    rule: &str,
    check: &FirstCheck,
    simple_types: bool,
    needs_check: bool,
) -> TokenStream {
    if !needs_check || check.nullable {
        return quote! {};
    }

    let _None = get_def(Symbol::None, simple_types);
    let _Some = get_def(Symbol::Some, simple_types);
    let _Err = get_def(Symbol::Err, simple_types);
    let _ParseError = get_def(Symbol::UtilParseError, simple_types);

    let branches = check
        .tokens
        .iter()
        .map(|token| tokens::enum_ident(token))
        .map(|ident| quote! { #_Some(Token::#ident(_)) => {} });

    quote! {
        match inp.get_current() {
            #_None =>
                return #_Err(#_ParseError {
                    place: #rule.into(),
                    reason: "Input is empty".into(),
                }),
            #(#branches)*
            #_Some(tok) =>
                return #_Err(#_ParseError {
                    place: #rule.into(),
                    reason: "Unknown token".into(),
                }),
        };
    }
}
