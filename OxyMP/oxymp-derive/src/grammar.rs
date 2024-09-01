use crate::combinators::*;
use crate::lexer::{ExactToken, RegexToken, TokenInfo};

#[derive(Debug)]
enum GrammarTerminal {
    Pattern(String),
    Token(String),
}
#[derive(Debug)]
struct GrammarNonTerminal(String);
#[derive(Debug)]
struct GrammarGroup(Box<GrammarNode>);
#[derive(Debug)]
struct GrammarChoice(Vec<GrammarNode>);
#[derive(Debug)]
struct GrammarOptional(Box<GrammarNode>);

#[derive(Debug)]
pub enum GrammarNode {
    Terminal(GrammarTerminal),
    NonTerminal(GrammarNonTerminal),
    Group(GrammarGroup),
    Choice(GrammarChoice),
    Optional(GrammarOptional),
}

#[derive(Debug)]
pub struct GrammarRule {
    name: String,
    rule: GrammarNode,
}

impl GrammarRule {
    fn new(rule: &str) -> Self {
        return GrammarRule {
            name: "name".to_string(),
            rule: GrammarNode::Terminal(GrammarTerminal::Token("rule".to_string())),
        };
    }
}

pub struct Grammar<'a> {
    tokens_parser: Parser<'a, &'a str>,
}

impl<'a> Grammar<'a> {
    pub fn new(token_info: &'a [TokenInfo]) -> Self {
        Grammar {
            tokens_parser: Grammar::generate_token_parser(token_info),
        }
    }

    fn generate_token_parser(token_info: &'a [TokenInfo]) -> Parser<'a, &'a str> {
        let mut parsers = Vec::new();

        for info in token_info {
            match info {
                TokenInfo::Exact(ExactToken { name, pattern }) => {
                    parsers.push(tag(name.to_string()));
                    parsers.push(tag(format!("'{}'", pattern)));
                }
                TokenInfo::Regex(RegexToken { name, .. }) => parsers.push(tag(name.to_string())),
                TokenInfo::Ignore(_) => {}
            };
        }

        first_of(parsers.into())
    }

    // expr -> Number (('+' | '-') expr)?

    fn choice(&self) -> Parser<'a, (&'a str, Vec<(char, &'a str)>)> {
        delimited(
            exact_char('('),
            separated(self.tokens_parser.clone(), exact_char('|')),
            exact_char(')'),
        )
    }
}
