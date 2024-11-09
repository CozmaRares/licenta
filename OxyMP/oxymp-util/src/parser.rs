use std::{ops::Deref, rc::Rc};

#[derive(Debug, Clone)]
pub struct ParserInput<Token>
where
    Token: Clone,
{
    pub tokens: Rc<[Token]>,
    pub current: usize,
}
impl<Token, T> From<T> for ParserInput<Token>
where
    T: Deref<Target = [Token]>,
    Token: Clone,
{
    fn from(tokens: T) -> ParserInput<Token> {
        ParserInput {
            tokens: tokens.deref().into(),
            current: 0,
        }
    }
}
impl<Token> ParserInput<Token>
where
    Token: Clone,
{
    pub fn get_current(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    pub fn increment(&self) -> ParserInput<Token> {
        ParserInput {
            tokens: self.tokens.clone(),
            current: self.current + 1,
        }
    }
}
pub type ParserState<Token, Value> = Result<(ParserInput<Token>, Value), ParseError<Token>>;

#[derive(Debug)]
pub enum ParseErrorReason<Token> {
    UnexpectedEOI { expected: Vec<String> },
    UnexpectedToken { expected: Vec<String>, token: Token },
    AllChoicesFailed,
}

#[derive(Debug)]
pub struct ParseError<Token> {
    pub rule: String,
    pub input_location: usize,
    pub reason: ParseErrorReason<Token>,
}
