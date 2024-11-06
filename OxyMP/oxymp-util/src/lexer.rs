use std::{collections::HashMap, fmt::Formatter};

use regex::Regex;

#[derive(Debug)]
pub enum TokenMatcher {
    Exact(String),
    Regex(Regex),
}

impl TokenMatcher {
    pub fn regex(re: &str) -> TokenMatcher {
        let re = format!("^{re}");
        let re = Regex::new(&re).unwrap();
        TokenMatcher::Regex(re)
    }
}

pub enum TokenHandler<Token>
where
    Token: std::fmt::Debug,
{
    Pattern(Token),
    Regex(Box<dyn Fn(&str) -> Token>),
    Ignore,
}

impl<Token> std::fmt::Debug for TokenHandler<Token>
where
    Token: std::fmt::Debug,
{
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenHandler::Pattern(token) => f.debug_tuple("Pattern").field(token).finish(),
            TokenHandler::Regex(_) => f.debug_tuple("Regex").finish(),
            TokenHandler::Ignore => f.debug_tuple("Ignore").finish(),
        }
    }
}

#[derive(Debug)]
pub struct LexRule<Token>
where
    Token: std::fmt::Debug + Clone,
{
    pub matcher: TokenMatcher,
    pub handler: TokenHandler<Token>,
}

impl<Token> LexRule<Token>
where
    Token: std::fmt::Debug + Clone,
{
    fn matches(&self, input: &str) -> Option<usize> {
        match &self.matcher {
            TokenMatcher::Exact(exact_match) => {
                input.starts_with(exact_match).then_some(exact_match.len())
            }
            TokenMatcher::Regex(re) => re
                .captures(input)
                .and_then(|captures| captures.get(0))
                .map(|matched| matched.end() - matched.start()),
        }
    }

    pub fn consume<'a>(&self, input: &'a str) -> Option<(Option<Token>, &'a str)> {
        self.matches(input).map(|matched_size| {
            let token = match &self.handler {
                TokenHandler::Ignore => None,
                TokenHandler::Pattern(t) => Some(t.clone()),
                TokenHandler::Regex(f) => Some(f(&input[..matched_size])),
            };
            Some((token, &input[matched_size..]))
        })?
    }
}

#[derive(Debug)]
pub struct LexError<'a> {
    pub input: &'a str,
    pub message: String,
}

#[derive(Debug)]
pub struct Lexer<Token>
where
    Token: std::fmt::Debug + Clone,
{
    pub rules: Vec<LexRule<Token>>,
}

impl<Token> Lexer<Token>
where
    Token: std::fmt::Debug + Clone,
{
    pub fn tokenize(self, mut input: &str) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();
        while !input.is_empty() {
            let mut was_consumed = false;
            for rule in &self.rules {
                if let Some((token, remaining)) = rule.consume(input) {
                    if let Some(token) = token {
                        tokens.push(token);
                    }
                    input = remaining;
                    was_consumed = true;
                    break;
                }
            }
            if !was_consumed {
                return Err(LexError {
                    input,
                    message: "Unknown token".to_string(),
                });
            }
        }
        Ok(tokens)
    }
}

pub struct LexerBuilder<Tier, Token>
where
    Tier: std::hash::Hash + Ord + Default,
    Token: std::fmt::Debug + Clone,
{
    rules: HashMap<Tier, Vec<LexRule<Token>>>,
}

impl<Tier, Token> LexerBuilder<Tier, Token>
where
    Tier: std::hash::Hash + Ord + Default,
    Token: std::fmt::Debug + Clone,
{
    pub fn new() -> Self {
        LexerBuilder {
            rules: HashMap::new(),
        }
    }

    pub fn add_rule(&mut self, rule: LexRule<Token>) -> &Self {
        let tier = Tier::default();
        self.rules.entry(tier).or_default().extend(vec![rule]);
        self
    }

    pub fn add_tiered_rule(&mut self, tier: Tier, rule: LexRule<Token>) -> &Self {
        self.rules.entry(tier).or_default().extend(vec![rule]);
        self
    }

    pub fn build(self) -> Lexer<Token> {
        let mut rules: Vec<_> = self.rules.into_iter().collect();
        rules.sort_by(|(key1, _), (key2, _)| key1.cmp(key2));

        let rules = rules
            .into_iter()
            .map(|(_, rules)| rules)
            .flatten()
            .collect();

        Lexer { rules }
    }
}
