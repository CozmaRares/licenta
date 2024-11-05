use regex::Regex;

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

pub enum TokenHandler<Token> {
    Pattern(Token),
    Regex(Box<dyn Fn(&str) -> Token>),
    Ignore,
}

pub struct LexRule<Token>
where
    Token: Clone,
{
    pub matcher: TokenMatcher,
    pub handler: TokenHandler<Token>,
}

impl<Token> LexRule<Token>
where
    Token: Clone,
{
    fn matches(&self, input: &str) -> Option<usize> {
        match &self.matcher {
            TokenMatcher::Exact(exact_match) => {
                input.starts_with(exact_match).then(|| exact_match.len())
            }
            TokenMatcher::Regex(re) => re
                .captures(input)
                .map(|captures| captures.get(0))
                .flatten()
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

pub struct Lexer<Token>
where
    Token: Clone,
{
    pub rules: Vec<LexRule<Token>>,
}

impl<Token> Lexer<Token>
where
    Token: Clone,
{
    pub fn tokenize(self, mut input: &str) -> Result<Vec<Token>, LexError> {
        let mut tokens = Vec::new();
        while input.len() > 0 {
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
