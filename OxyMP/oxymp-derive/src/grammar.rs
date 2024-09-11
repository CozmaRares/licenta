use core::panic;
use std::collections::{HashMap, HashSet};

use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, tag, take_till1},
    character::complete::{alpha1, char, multispace0},
    combinator::{eof, value},
    multi::{many1, separated_list1},
    sequence::delimited,
    IResult,
};

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
    fn from_raw(
        node: RawGrammarNode,
        names: &HashMap<String, NameType>,
        patterns: &HashSet<String>,
    ) -> Self {
        match node {
            RawGrammarNode::Name(name) => match names.get(&name) {
                None => panic!("Error when parsing grammar rules\nUnknown name: {}", name),
                Some(ty) => match ty {
                    NameType::Rule => GrammarNode::Rule(name),
                    NameType::Token => GrammarNode::Token(name),
                },
            },
            RawGrammarNode::Pattern(pattern) => match patterns.contains(&pattern) {
                false => panic!(
                    "Error when parsing grammar rules\nUnknown token pattern: {}",
                    pattern
                ),
                true => GrammarNode::Pattern(pattern),
            },
            RawGrammarNode::Expr(exprs) => GrammarNode::Expr(
                exprs
                    .into_iter()
                    .map(|e| GrammarNode::from_raw(e, names, patterns))
                    .collect(),
            ),
            RawGrammarNode::Choice(exprs) => GrammarNode::Choice(
                exprs
                    .into_iter()
                    .map(|e| GrammarNode::from_raw(e, names, patterns))
                    .collect(),
            ),
            RawGrammarNode::Optional(expr) => {
                GrammarNode::Optional(Box::new(GrammarNode::from_raw(*expr, names, patterns)))
            }
        }
    }
}

#[derive(Debug)]
pub struct RawGrammarRule {
    name: String,
    rule: RawGrammarNode,
}

pub fn new_grammar_rule(rule: &str) -> Result<RawGrammarRule, nom::Err<nom::error::Error<&str>>> {
    grammar_rule(rule).map(|(_, rule)| rule)
}

pub fn aggragate_grammar_rules(
    rules: Vec<RawGrammarRule>,
    token_info: &Vec<TokenInfo>,
) -> HashMap<String, GrammarNode> {
    let mut names = HashMap::new();
    let mut patterns = HashSet::new();

    rules.iter().for_each(|rule| {
        names.insert(rule.name.clone(), NameType::Rule);
    });

    let mut add_token_name = |tok_name| {
        if names.contains_key(&tok_name) {
            panic!(
                "Error when parsing grammar rules\nA token and a rule share the same name: {}",
                tok_name
            )
        }

        names.insert(tok_name, NameType::Token);
    };

    let mut add_token_pattern = |tok_pattern| {
        if patterns.contains(&tok_pattern) {
            panic!(
                "Error when parsing grammar rules\nTwo tokens share the same pattern: {}",
                tok_pattern
            )
        }

        patterns.insert(tok_pattern);
    };

    token_info.iter().for_each(|info| match info {
        TokenInfo::Exact(tok) => {
            add_token_name(tok.name.clone());
            add_token_pattern(tok.pattern.clone())
        }
        TokenInfo::Regex(tok) => add_token_name(tok.name.clone()),
        _ => {}
    });

    rules
        .into_iter()
        .fold(HashMap::new(), |mut acc, RawGrammarRule { name, rule }| {
            acc.insert(name, GrammarNode::from_raw(rule, &names, &patterns));
            acc
        })
}

fn ws<'a, O, E: nom::error::ParseError<&'a str>, F>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: nom::Parser<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

fn paranthesized<'a, O, E: nom::error::ParseError<&'a str>, F>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: nom::Parser<&'a str, O, E>,
{
    delimited(ws(char('(')), inner, ws(char(')')))
}

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
    let (input, matched) = ws(alpha1)(input)?;
    Ok((input, RawGrammarNode::Name(matched.into())))
}

fn expr(input: &str) -> IResult<&str, RawGrammarNode> {
    let (input, exprs) = many1(expr1)(input)?;
    Ok((input, RawGrammarNode::Expr(exprs)))
}

fn expr1(input: &str) -> IResult<&str, RawGrammarNode> {
    let (input, expr) = alt((optional, choice, literal))(input)?;
    Ok((input, expr))
}

fn literal(input: &str) -> IResult<&str, RawGrammarNode> {
    let (input, literal) = alt((token, name))(input)?;
    Ok((input, literal))
}

fn pattern(input: &str) -> IResult<&str, String> {
    escaped_transform(
        take_till1(|c| c == '\'' || c == '\\'),
        '\\',
        alt((value(r"\", tag(r"\")), value("'", tag("'")))),
    )(input)
}

fn token(input: &str) -> IResult<&str, RawGrammarNode> {
    let (input, matched) = ws(delimited(char('\''), pattern, char('\'')))(input)?;
    Ok((input, RawGrammarNode::Pattern(matched)))
}

fn optional(input: &str) -> IResult<&str, RawGrammarNode> {
    let (input, opt) = paranthesized(expr)(input)?;
    let (input, _) = ws(char('?'))(input)?;
    Ok((input, RawGrammarNode::Optional(Box::new(opt))))
}

fn choice(input: &str) -> IResult<&str, RawGrammarNode> {
    let (input, choices) = paranthesized(separated_list1(char('|'), expr))(input)?;
    Ok((input, RawGrammarNode::Choice(choices)))
}
