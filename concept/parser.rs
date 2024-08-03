#![allow(unused)]

use std::rc::Rc;
use token::Token;
use crate::lexer::lexer::*;

// #[derive(RecursiveDescent)]
//
// #[token("ParanLeft", '(']
// #[token("ParanRight", ')']
// #[token("NewLine", '\n']
// #[token("Plus", '+']
// #[token("Minus", '-']
// #[token("Number", ("/-?[0-9]+/", |matched| matched.parser::<i64>().unwrap()), "<i64>"]
//
// #[grammar("expr -> Number (('+' | '-') expr)?")]
//
// #[start("expr")]
pub struct Parser;

// auto generated code

pub mod token {

    use crate::CanIgnoreToken;

    #[derive(Debug, PartialEq, Clone)]
    pub struct Null;

    #[derive(Debug, PartialEq, Clone)]
    pub struct NewLine;

    #[derive(Debug, PartialEq, Clone)]
    pub struct Plus;

    #[derive(Debug, PartialEq, Clone)]
    pub struct Minus;

    #[derive(Debug, PartialEq, Clone)]
    pub struct Number(pub i64);

    #[derive(Debug, PartialEq, Clone)]
    pub enum Token {
        Null(Null),
        NewLine(NewLine),
        Plus(Plus),
        Minus(Minus),
        Number(Number),
    }

    impl CanIgnoreToken for Token {
        fn ignore() -> Self {
            Token::Null(Null)
        }
    }
}

impl Token {
    pub fn null() -> Self {
        Token::Null(token::Null)
    }
    fn to_null(s: Self) -> Option<token::Null> {
        match s {
            Token::Null(x) => Some(x),
            _ => None,
        }
    }

    pub fn new_line() -> Self {
        Token::NewLine(token::NewLine)
    }
    fn to_new_line(s: Self) -> Option<token::NewLine> {
        match s {
            Token::NewLine(x) => Some(x),
            _ => None,
        }
    }

    pub fn plus() -> Self {
        Token::Plus(token::Plus)
    }
    fn to_plus(s: Option<&Self>) -> Option<&token::Plus> {
        match s? {
            Token::Plus(x) => Some(x),
            _ => None,
        }
    }

    pub fn minus() -> Self {
        Token::Minus(token::Minus)
    }
    pub fn to_minus(s: Option<&Self>) -> Option<&token::Minus> {
        match s? {
            Token::Minus(x) => Some(x),
            _ => None,
        }
    }

    pub fn number(v: i64) -> Self {
        Token::Number(token::Number(v))
    }
    pub fn to_number(s: Option<&Self>) -> Option<&token::Number> {
        match s? {
            Token::Number(x) => Some(x),
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum ExprChoice1 {
    _1(token::Plus),
    _2(token::Minus),
}

#[derive(Debug)]
pub struct Expr {
    pub _1: token::Number, // because of #[token("Number", "<i64>"]
    pub _2: Option<(ExprChoice1, Box<Expr>)>,
}

#[derive(Debug)]
pub enum AST {
    Expr(Expr),
}

impl AST {
    fn to_expr(s: Self) -> Option<Expr> {
        match s {
            AST::Expr(x) => Some(x),
            // if more than 1 node
            // _ => None,
        }
    }
}

pub struct ParserState<'a> {
    pub toks: &'a [token::Token],
}

impl<'a> ParserState<'a> {
    pub fn new(tokens: &'a Vec<token::Token>) -> Self {
        ParserState { toks: tokens }
    }

    pub fn current(&self) -> Option<&token::Token> {
        self.toks.first()
    }

    pub fn next(&self) -> Option<&token::Token> {
        self.toks.iter().nth(1)
    }

    pub fn increment(&self) -> Self {
        ParserState {
            toks: &self.toks[1..],
        }
    }
}

#[derive(Debug)]
pub struct ParseError {}

impl Parser {
    pub fn expr(state: ParserState) -> Result<(AST, ParserState), ParseError> {
        let mut state = state;

        let _1 = match Token::to_number(state.current()) {
            Some(x) => x,
            None => return Err(ParseError {}),
        };

        let state = state.increment();

        let c1 = match state.current() {
            Some(token::Token::Plus(_)) => ExprChoice1::_1(token::Plus),
            Some(token::Token::Minus(_)) => ExprChoice1::_2(token::Minus),
            _ => {
                return Ok((
                    AST::Expr(Expr {
                        _1: _1.clone(),
                        _2: None,
                    }),
                    state,
                ))
            }
        };

        let state = state.increment();

        let (r1, state) = Parser::expr(state)?;
        let r1 = match AST::to_expr(r1) {
            Some(x) => x,
            None => return Err(ParseError {}),
        };

        return Ok((
            AST::Expr(Expr {
                _1: _1.clone(),
                _2: Some((c1, Box::new(r1))),
            }),
            state,
        ));
    }

    pub fn parse_state(state: ParserState) -> Result<(AST, ParserState), ParseError> {
        Parser::expr(state)
    }

    pub fn parse(tokens: &Vec<token::Token>) -> Result<AST, ParseError> {
        let state = ParserState::new(tokens);
        Parser::parse_state(state).map(|(ast, _)| ast)
    }
}

pub fn make_lexer() -> Lexer<Token> {
    let rules = vec![
        TokenRule {
            matcher: Matcher::Char('\n'),
            handler: Handler::Token(Token::new_line()),
        },
        TokenRule {
            matcher: Matcher::Char('+'),
            handler: Handler::Token(Token::plus()),
        },
        TokenRule {
            matcher: Matcher::Char('-'),
            handler: Handler::Token(Token::minus()),
        },
        TokenRule {
            matcher: Matcher::regex("[0-9]+").unwrap(),
            handler: Handler::Lambda(Box::new(|matched| Token::number(matched.parse().unwrap()))),
        },
    ];

    Lexer::new(rules)
}
