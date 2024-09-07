use nom::{
    branch::alt,
    bytes::complete::{tag, take_till1},
    character::complete::{alpha1, char},
    combinator::eof,
    multi::{many1, separated_list1},
    sequence::{delimited, tuple},
    IResult,
};

#[derive(Debug)]
pub enum GrammarNode {
    Name(String),
    Token(String),
    Expr(Vec<GrammarNode>),
    Choice(Vec<GrammarNode>),
    Optional(Box<GrammarNode>),
}

#[derive(Debug)]
pub struct GrammarRule {
    name: String,
    rule: GrammarNode,
}

impl GrammarRule {
    pub fn new(rule: &str) -> Self {
        grammar_rule(rule).unwrap().1
    }
}

// TODO: support spaces
fn grammar_rule(input: &str) -> IResult<&str, GrammarRule> {
    let (input, name) = name(input)?;
    let (input, _) = tag("->")(input)?;
    let (input, rule) = expr(input)?;
    let (input, _) = eof(input)?;

    let inner;

    match name {
        GrammarNode::Name(name) => inner = name,
        _ => unreachable!(),
    }

    Ok((input, GrammarRule { name: inner, rule }))
}

fn name(input: &str) -> IResult<&str, GrammarNode> {
    let (remaining, matched) = alpha1(input)?;
    Ok((remaining, GrammarNode::Name(matched.into())))
}

fn expr(input: &str) -> IResult<&str, GrammarNode> {
    let (remaining, exprs) = many1(expr1)(input)?;
    Ok((remaining, GrammarNode::Expr(exprs)))
}

fn expr1(input: &str) -> IResult<&str, GrammarNode> {
    alt((optional, choice, literal))(input)
}

fn literal(input: &str) -> IResult<&str, GrammarNode> {
    alt((token, name))(input)
}

// TODO: support \'
fn token(input: &str) -> IResult<&str, GrammarNode> {
    let (remaining, matched) = delimited(char('\''), take_till1(|c| c == '\''), char('\''))(input)?;
    Ok((remaining, GrammarNode::Token(matched.into())))
}

fn optional(input: &str) -> IResult<&str, GrammarNode> {
    let (remaining, (opt, _)) = tuple((delimited(char('('), expr, char(')')), char('?')))(input)?;
    Ok((remaining, GrammarNode::Optional(Box::new(opt))))
}

fn choice(input: &str) -> IResult<&str, GrammarNode> {
    let (remaining, choices) =
        delimited(char('('), separated_list1(char('|'), expr), char(')'))(input)?;
    Ok((remaining, GrammarNode::Choice(choices)))
}
