use std::collections::HashSet;

use crate::lexer::TokenInfo;

#[derive(Debug)]
enum GrammarTerminal{
    Pattern(String),
    Token(String)
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
    pub fn new(rule: &str) -> Self {
        return GrammarRule {
            name: "name".to_string(),
            rule: GrammarNode::Terminal(GrammarTerminal::Token("rule".to_string())),
        };
    }
}

// expr -> Number (('+' | '-') expr)?

