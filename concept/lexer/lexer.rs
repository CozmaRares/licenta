use regex::Regex;

// TODO: create a LexToken trait that identifies the tokens used in this system

// TODO: make a #derive macro that autogenerates
// eq, ne, type_eq, and type_ne functions
// and adds Null/Ignore and EOF in the enum
// NOTE: maybe make the handler function return an enum that has 3 states: ignore, eof, token(token) (could be more?)
// NOTE: maybe apply the macro itself to the tokens
pub trait CanIgnoreToken {
    fn ignore() -> Self;
}

// TODO: there could defiently be more types of matchers
// TODO: can define matchers for commonly used things like numbers, letters, whitespace, etc.
pub enum Matcher {
    Char(char),
    ExactMatch(String),

    Regex(Regex),
    // FIXME: right now, regex must always start with ^
}

impl Matcher {
    pub fn regex(re: &str) -> Result<Self, regex::Error> {
        let re = ::alloc::macros::format!("^{re}");
        let re = Regex::new(&re);

        return re.map(|re| Matcher::Regex(re));
    }
}

// NOTE: maybe include lambda to use only char when matching chars
pub enum Handler<T>
where
    T: CanIgnoreToken + PartialEq,
{
    Ignore,
    Token(T),
    Lambda(Box<dyn Fn(&str) -> T>),
}

pub struct TokenRule<T>
where
    T: Clone + CanIgnoreToken + PartialEq,
{
    pub matcher: Matcher,
    pub handler: Handler<T>,
}

impl<T> TokenRule<T>
where
    T: Clone + CanIgnoreToken + PartialEq,
{
    fn matches(&self, input: &str) -> Option<usize> {
        match &self.matcher {
            Matcher::Char(ch) => input.starts_with(*ch).then(|| 1),

            Matcher::ExactMatch(exact_match) => {
                input.starts_with(exact_match).then(|| exact_match.len())
            }

            Matcher::Regex(re) => {
                if let Some(captures) = re.captures(input) {
                    if let Some(matched) = captures.get(0) {
                        Some(matched.end() - matched.start())
                    } else {
                        None
                    }
                } else {
                    None
                }
            }
        }
    }

    fn make_token<'a>(&self, input: &'a str) -> Option<(T, &'a str)> {
        self.matches(input).map(|matched_size| {
            let token = match &self.handler {
                Handler::Ignore => T::ignore(),
                Handler::Token(t) => t.clone(),
                Handler::Lambda(l) => l(&input[..matched_size]),
            };

            Some((token, &input[matched_size..]))
        })?
    }
}

pub struct Lexer<T>
where
    T: Clone + CanIgnoreToken + PartialEq,
{
    rules: Vec<TokenRule<T>>,
}

#[derive(Debug)]
pub struct LexerError<'a> {
    pub message: String,
    pub input: &'a str,
}

impl<T> Lexer<T>
where
    T: Clone + CanIgnoreToken + PartialEq + std::fmt::Debug,
{
    pub fn new(rules: Vec<TokenRule<T>>) -> Self {
        return Lexer { rules };
    }

    pub fn tokenize(self, mut input: &str) -> Result<Vec<T>, LexerError> {
        let ignored_token = T::ignore();

        let mut tokens: Vec<T> = Vec::new();

        while input.len() > 0 {
            let mut was_consumed = false;

            for rule in &self.rules {
                if let Some((token, consumed)) = rule.make_token(input) {
                    if token != ignored_token {
                        tokens.push(token);
                    }

                    input = consumed;
                    was_consumed = true;
                    break;
                }
            }

            if !was_consumed {
                return Err(LexerError {
                    message: "Unknown token".to_string(),
                    input,
                });
            }
        }

        return Ok(tokens);
    }
}
