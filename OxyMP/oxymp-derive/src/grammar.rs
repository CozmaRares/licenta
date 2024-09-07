use core::panic;
use std::collections::{HashMap, HashSet};

use nom::{
    branch::alt,
    bytes::complete::{tag, take_till1},
    character::complete::{alpha1, char},
    combinator::eof,
    multi::{many1, separated_list1},
    sequence::{delimited, tuple},
    IResult,
};
use proc_macro2::Span;

use crate::lexer::TokenInfo;

#[derive(Debug)]
enum RawGrammarNode {
    Name(String),
    Pattern(String),
    Expr(Vec<RawGrammarNode>),
    Choice(Vec<RawGrammarNode>),
    Optional(Box<RawGrammarNode>),
}

#[derive(Debug)]
pub enum GrammarNode {
    Rule(String),
    Token(String),
    Pattern(String),
    Expr(Vec<GrammarNode>),
    Choice(Vec<GrammarNode>),
    Optional(Box<GrammarNode>),
}

enum NameType {
    Rule,
    Token,
}

impl GrammarNode {
    fn from_raw(node: RawGrammarNode, names: &HashMap<String, NameType>) -> Self {
        match node {
            RawGrammarNode::Name(name) => match names.get(&name) {
                Some(ty) => match ty {
                    NameType::Rule => GrammarNode::Rule(name),
                    NameType::Token => GrammarNode::Token(name),
                },
                None => panic!("Unknown name: {}", name),
            },
            RawGrammarNode::Pattern(pattern) => GrammarNode::Pattern(pattern),
            RawGrammarNode::Expr(exprs) => GrammarNode::Expr(
                exprs
                    .into_iter()
                    .map(|e| GrammarNode::from_raw(e, names))
                    .collect(),
            ),
            RawGrammarNode::Choice(exprs) => GrammarNode::Choice(
                exprs
                    .into_iter()
                    .map(|e| GrammarNode::from_raw(e, names))
                    .collect(),
            ),
            RawGrammarNode::Optional(expr) => {
                GrammarNode::Optional(Box::new(GrammarNode::from_raw(*expr, names)))
            }
        }
    }
}

#[derive(Debug)]
pub struct RawGrammarRule {
    name: String,
    rule: RawGrammarNode,
}

pub fn new_rule(rule: &str) -> Result<RawGrammarRule, nom::Err<nom::error::Error<&str>>> {
    grammar_rule(rule).map(|(_, rule)| rule)
}

pub fn aggragate_rules(
    rules: Vec<RawGrammarRule>,
    token_info: &Vec<TokenInfo>,
) -> HashMap<String, GrammarNode> {
    let mut names = HashMap::new();

    rules.iter().for_each(|rule| {
        names.insert(rule.name.clone(), NameType::Rule);
    });

    token_info
        .iter()
        .filter(|info| match info {
            TokenInfo::Exact(_) => true,
            TokenInfo::Regex(_) => true,
            _ => false,
        })
        .map(|info| match info {
            TokenInfo::Exact(tok) => tok.name.clone(),
            TokenInfo::Regex(tok) => tok.name.clone(),
            _ => unreachable!(),
        })
        .for_each(|tok_name| {
            if names.contains_key(&tok_name) {
                panic!("A token and a rule share the same name: {}", tok_name)
            }

            names.insert(tok_name, NameType::Token);
        });

    rules
        .into_iter()
        .fold(HashMap::new(), |mut acc, RawGrammarRule { name, rule }| {
            acc.insert(name, GrammarNode::from_raw(rule, &names));
            acc
        })
}

// TODO: support whitespace
fn grammar_rule(input: &str) -> IResult<&str, RawGrammarRule> {
    let (input, name) = name(input)?;
    let (input, _) = tag("->")(input)?;
    let (input, rule) = expr(input)?;
    let (input, _) = eof(input)?;

    let inner;

    match name {
        RawGrammarNode::Name(name) => inner = name,
        _ => unreachable!(),
    }

    Ok((input, RawGrammarRule { name: inner, rule }))
}

fn name(input: &str) -> IResult<&str, RawGrammarNode> {
    let (remaining, matched) = alpha1(input)?;
    Ok((remaining, RawGrammarNode::Name(matched.into())))
}

fn expr(input: &str) -> IResult<&str, RawGrammarNode> {
    let (remaining, exprs) = many1(expr1)(input)?;
    Ok((remaining, RawGrammarNode::Expr(exprs)))
}

fn expr1(input: &str) -> IResult<&str, RawGrammarNode> {
    alt((optional, choice, literal))(input)
}

fn literal(input: &str) -> IResult<&str, RawGrammarNode> {
    alt((token, name))(input)
}

// TODO: support \'
fn token(input: &str) -> IResult<&str, RawGrammarNode> {
    let (remaining, matched) = delimited(char('\''), take_till1(|c| c == '\''), char('\''))(input)?;
    Ok((remaining, RawGrammarNode::Pattern(matched.into())))
}

fn optional(input: &str) -> IResult<&str, RawGrammarNode> {
    let (remaining, (opt, _)) = tuple((delimited(char('('), expr, char(')')), char('?')))(input)?;
    Ok((remaining, RawGrammarNode::Optional(Box::new(opt))))
}

fn choice(input: &str) -> IResult<&str, RawGrammarNode> {
    let (remaining, choices) =
        delimited(char('('), separated_list1(char('|'), expr), char(')'))(input)?;
    Ok((remaining, RawGrammarNode::Choice(choices)))
}
