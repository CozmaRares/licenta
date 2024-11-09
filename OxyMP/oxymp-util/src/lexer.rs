use std::collections::HashMap;

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
    Regex(Box<dyn Fn(&str) -> Result<Token, LexError>>),
    Ignore,
}

impl<Token> std::fmt::Debug for TokenHandler<Token>
where
    Token: std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
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

    pub fn consume<'a>(
        &self,
        input: &'a str,
    ) -> Option<Result<(Option<Token>, &'a str), LexError<'a>>> {
        let matched_size = match self.matches(input) {
            Some(s) => s,
            None => return None,
        };

        let token = match &self.handler {
            TokenHandler::Ignore => None,
            TokenHandler::Pattern(t) => Some(t.clone()),
            TokenHandler::Regex(f) => match f(&input[..matched_size]) {
                Ok(t) => Some(t),
                Err(e) => return Some(Err(e)),
            },
        };

        Some(Ok((token, &input[matched_size..])))
    }
}

#[derive(Debug)]
pub enum LexError<'a> {
    UnknownPattern(&'a str),
    UnparsableToken(&'a str),
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
    pub fn tokenize<'a>(&'a self, mut input: &'a str) -> Result<Vec<Token>, LexError<'a>> {
        let mut tokens = Vec::new();
        while !input.is_empty() {
            let mut was_consumed = false;
            for rule in &self.rules {
                if let Some(result) = rule.consume(input) {
                    let (token, remaining) = result?;
                    if let Some(token) = token {
                        tokens.push(token);
                    }
                    input = remaining;
                    was_consumed = true;
                    break;
                }
            }
            if !was_consumed {
                return Err(LexError::UnknownPattern(input));
            }
        }
        Ok(tokens)
    }
}

#[derive(Ord, Eq, PartialEq, PartialOrd, Default, Hash)]
pub enum DefaultTokenTier {
    High,
    Medium,
    #[default]
    Low,
}

pub struct LexerBuilder<Token, Tier = DefaultTokenTier>
where
    Token: std::fmt::Debug + Clone,
    Tier: std::hash::Hash + Ord + Default,
{
    rules: HashMap<Tier, Vec<LexRule<Token>>>,
}

impl<Token, Tier> LexerBuilder<Token, Tier>
where
    Token: std::fmt::Debug + Clone,
    Tier: std::hash::Hash + Ord + Default,
{
    pub fn new() -> Self {
        LexerBuilder {
            rules: HashMap::new(),
        }
    }

    pub fn add_rule(&mut self, rule: LexRule<Token>) {
        let tier = Tier::default();
        self.rules.entry(tier).or_default().extend(vec![rule]);
    }

    pub fn add_tiered_rule(&mut self, tier: Tier, rule: LexRule<Token>) {
        self.rules.entry(tier).or_default().extend(vec![rule]);
    }

    pub fn build(self) -> Lexer<Token> {
        let mut rules: Vec<_> = self.rules.into_iter().collect();
        rules.sort_by(|(key1, _), (key2, _)| key1.cmp(key2));

        let rules = rules.into_iter().flat_map(|(_, rules)| rules).collect();

        Lexer { rules }
    }
}

impl<Token, Tier> Default for LexerBuilder<Token, Tier>
where
    Token: std::fmt::Debug + Clone,
    Tier: std::hash::Hash + Ord + Default,
{
    fn default() -> Self {
        Self::new()
    }
}
