use std::rc::Rc;

use oxymp::RecursiveDescent;
use oxymp_util::{
    lexer::{DefaultTokenTier, LexRule, Lexer, LexerBuilder, TokenHandler, TokenMatcher},
    parser::{ParseError, ParseErrorReason, ParserInput, ParserState},
};

mod nested {
    pub type Integer = i64;

    pub fn match_number(matched: &str) -> i64 {
        matched.parse().unwrap()
    }
}

#[derive(RecursiveDescent)]
#[exact_token(name = "ParanLeft", pattern = "(", tier = DefaultTokenTier::Low)]
#[exact_token(name = "ParanRight", pattern = ")", tier = DefaultTokenTier::Medium)]
#[exact_token(name = "Plus", pattern = "+", tier = DefaultTokenTier::High)]
#[exact_token(name = "Minus", pattern = "-")]
#[regex_token(
    name = "Number",
    regex = r"[0-9]+",
    transformer_fn = nested::match_number,
    kind = nested::Integer
)]
#[ignore_pattern(regex = r"\s+")]
#[grammar = r"Integer ::= Number | Minus Integer"]
#[grammar = r"Expr ::= Integer ExprCont?"]
#[grammar = r"ExprCont ::= ('+' | '-') Integer ExprCont?"]
#[simple_types]
#[depth_limit = 10]
pub(crate) struct Parser;

impl Integer {
    fn eval(&self) -> i64 {
        match &self.0 {
            IntegerChoice1::_1(TokenNumber(n)) => **n,
            IntegerChoice1::_2((_, e)) => -e.eval(),
        }
    }
}

impl ExprCont {
    fn eval(&self) -> i64 {
        let mult = match &self.0 .0 {
            ExprContChoice1::_1(_) => 1,
            ExprContChoice1::_2(_) => -1,
        };

        let num = mult * self.0 .1.eval();

        match &self.0 .2 {
            None => num,
            Some(e) => num + e.eval(),
        }
    }
}

impl Expr {
    fn eval(&self) -> i64 {
        let num = self.0 .0.eval();

        match &self.0 .1 {
            None => num,
            Some(e) => num + e.eval(),
        }
    }
}

fn main() {
    let l = create_lexer();

    let a = l.tokenize("1 - -2 + 3").unwrap();
    let a = Parser::expr(a.into()).unwrap();
    let a = a.1.eval();
    println!("{:#?}", a);
}
