use std::{collections::HashMap, rc::Rc};

use nom::{
    branch::alt,
    bytes::complete::{escaped_transform, tag},
    character::complete::{alpha1, alphanumeric1, char, multispace0, none_of},
    combinator::{eof, opt, recognize, value},
    multi::{many0, many1, separated_list1},
    sequence::{delimited, pair},
    IResult,
};

use crate::{data::MacroData, tokens::TokenInfo};

#[derive(Debug)]
enum RawGrammarNode {
    Name(Rc<str>),
    Pattern(Rc<str>),
    List(Vec<RawGrammarNode>),
    Optional(Box<RawGrammarNode>),
    Choice(Vec<RawGrammarNode>),
}

#[derive(Debug)]
pub struct RawGrammarRule {
    name: Rc<str>,
    rule: RawGrammarNode,
}

pub fn new_grammar_rule(rule: &str) -> Result<RawGrammarRule, nom::Err<nom::error::Error<&str>>> {
    grammar_rule(rule).map(|(_, rule)| rule)
}

#[derive(Debug)]
pub enum GrammarNodeContent {
    Rule(Rc<str>),
    Token(Rc<str>),
    List(Vec<GrammarNode>),
    Optional(Box<GrammarNode>),
    Choice(Vec<GrammarNode>, usize),
}

#[derive(Debug)]
pub struct GrammarNode {
    pub content: GrammarNodeContent,
    pub index: usize,
}

enum NameType {
    Rule,
    Token,
}

fn convert_raw_node(
    node: RawGrammarNode,
    names: &HashMap<Rc<str>, NameType>,
    patterns: &HashMap<Rc<str>, Rc<str>>,
    node_idx: usize,
    choice_idx: usize,
) -> Result<(GrammarNode, usize, usize), String> {
    match node {
        RawGrammarNode::Name(name) => match names.get(&*name) {
            None => Err(format!("Unknown name: {}", name)),
            Some(ty) => Ok((
                match ty {
                    NameType::Rule => GrammarNode {
                        content: GrammarNodeContent::Rule(name),
                        index: node_idx,
                    },
                    NameType::Token => GrammarNode {
                        content: GrammarNodeContent::Token(name),
                        index: node_idx,
                    },
                },
                node_idx + 1,
                choice_idx,
            )),
        },
        RawGrammarNode::Pattern(pattern) => match patterns.get(&*pattern) {
            None => Err(format!(
                "Error when parsing grammar rules\nUnknown token pattern: {}",
                pattern
            )),
            Some(tok) => Ok((
                GrammarNode {
                    content: GrammarNodeContent::Token(tok.to_string().into()),
                    index: node_idx,
                },
                node_idx + 1,
                choice_idx,
            )),
        },
        RawGrammarNode::List(exprs) => {
            if exprs.len() == 1 {
                let mut exprs = exprs;
                let expr = exprs.pop().unwrap();
                return convert_raw_node(expr, names, patterns, node_idx, choice_idx);
            }

            let mut node_idx = node_idx;
            let mut choice_idx = choice_idx;
            let mut nodes = Vec::new();

            for expr in exprs {
                let node;
                (node, node_idx, choice_idx) =
                    convert_raw_node(expr, names, patterns, node_idx, choice_idx)?;

                nodes.push(node);
            }

            Ok((
                GrammarNode {
                    content: GrammarNodeContent::List(nodes),
                    index: node_idx,
                },
                node_idx + 1,
                choice_idx,
            ))
        }
        RawGrammarNode::Choice(choices) => {
            if choices.len() == 1 {
                let mut choices = choices;
                let choice = choices.pop().unwrap();
                return convert_raw_node(choice, names, patterns, node_idx, choice_idx);
            }

            let mut node_idx = node_idx;
            let mut choice_idx = choice_idx;
            let mut nodes = Vec::new();

            for choice in choices {
                let node;
                (node, node_idx, choice_idx) =
                    convert_raw_node(choice, names, patterns, node_idx, choice_idx)?;

                nodes.push(node);
            }

            Ok((
                GrammarNode {
                    content: GrammarNodeContent::Choice(nodes, choice_idx),
                    index: node_idx,
                },
                node_idx + 1,
                choice_idx + 1,
            ))
        }
        RawGrammarNode::Optional(node) => {
            let (node, node_idx, choice_idx) =
                convert_raw_node(*node, names, patterns, node_idx, choice_idx)?;

            Ok((
                GrammarNode {
                    content: GrammarNodeContent::Optional(Box::new(node)),
                    index: node_idx,
                },
                node_idx + 1,
                choice_idx,
            ))
        }
    }
}

pub fn aggragate_grammar_rules(
    rules: Vec<RawGrammarRule>,
    data: &MacroData,
) -> Result<HashMap<Rc<str>, GrammarNode>, String> {
    let mut names = HashMap::new();
    let mut patterns = HashMap::new();

    rules.iter().for_each(|rule| {
        names.insert(rule.name.clone(), NameType::Rule);
    });

    let mut add_token_name = |tok_name| {
        if names.contains_key(&tok_name) {
            return Err(format!(
                "Error when parsing grammar rules\nA token and a rule share the same name: {}",
                tok_name
            ));
        }

        names.insert(tok_name, NameType::Token);
        Ok(())
    };

    let mut add_token_pattern = |tok_pattern, tok_name| {
        if patterns.contains_key(&tok_pattern) {
            return Err(format!(
                "Error when parsing grammar rules\nTwo tokens share the same pattern: {}",
                tok_pattern
            ));
        }

        patterns.insert(tok_pattern, tok_name);
        Ok(())
    };

    for info in &data.tokens {
        match info {
            TokenInfo::Exact(tok) => {
                add_token_name(tok.name.clone())?;
                add_token_pattern(tok.pattern.clone(), tok.name.clone())?
            }
            TokenInfo::Regex(tok) => add_token_name(tok.name.clone())?,
            TokenInfo::Ignore(_) => {}
        }
    }

    let mut aggregated_rules = HashMap::new();

    for rule in rules {
        let RawGrammarRule { name, rule } = rule;
        let node = convert_raw_node(rule, &names, &patterns, 1, 1)?;
        aggregated_rules.insert(name, node.0);
    }

    Ok(aggregated_rules)
}

fn ws<'a, O, E: nom::error::ParseError<&'a str>, F>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: nom::Parser<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

fn grammar_rule(input: &str) -> IResult<&str, RawGrammarRule> {
    let (input, name) = name(input)?;
    let (input, _) = tag("::=")(input)?;
    let (input, rule) = choice(input)?;
    let (input, _) = eof(input)?;

    let inner = match name {
        RawGrammarNode::Name(name) => name,
        _ => unreachable!(),
    };

    Ok((input, RawGrammarRule { name: inner, rule }))
}
pub fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(alpha1, many0(alt((alphanumeric1, tag("_"))))))(input)
}

fn name(input: &str) -> IResult<&str, RawGrammarNode> {
    let (input, matched) = ws(identifier)(input)?;
    Ok((input, RawGrammarNode::Name(matched.into())))
}

fn choice(input: &str) -> IResult<&str, RawGrammarNode> {
    let (input, choices) = separated_list1(ws(char('|')), list)(input)?;
    Ok((input, RawGrammarNode::Choice(choices)))
}

fn list(input: &str) -> IResult<&str, RawGrammarNode> {
    let (input, items) = many1(list_item)(input)?;
    Ok((input, RawGrammarNode::List(items)))
}

fn list_item(input: &str) -> IResult<&str, RawGrammarNode> {
    let (input, node) = alt((group, token_pattern, name))(input)?;
    let (input, modifier) = ws(opt(char('?')))(input)?;

    match modifier {
        None => Ok((input, node)),
        Some('?') => Ok((input, RawGrammarNode::Optional(Box::new(node)))),
        Some(_) => unreachable!(),
    }
}

fn group(input: &str) -> IResult<&str, RawGrammarNode> {
    delimited(ws(char('(')), choice, ws(char(')')))(input)
}

fn token_pattern(input: &str) -> IResult<&str, RawGrammarNode> {
    let base = escaped_transform(
        none_of(r"\'"),
        '\\',
        alt((value(r"\", tag(r"\")), value("'", tag("'")))),
    );

    let (input, matched) = ws(delimited(char('\''), base, char('\'')))(input)?;
    Ok((input, RawGrammarNode::Pattern(matched.into())))
}
