use std::rc::Rc;

use oxymp::RecursiveDescent;
use oxymp_util::{
    lexer::{LexRule, Lexer, TokenHandler, TokenMatcher},
    parser::{ParseError, ParserInput, ParserState},
};

mod nested {
    pub type Int = i64;

    pub fn match_number(matched: &str) -> i64 {
        matched.parse().unwrap()
    }
}

#[derive(RecursiveDescent)]
#[exact_token(name = "ParanLeft", pattern = "(")]
#[exact_token(name = "ParanRight", pattern = ")")]
#[exact_token(name = "Plus", pattern = "+")]
#[exact_token(name = "Minus", pattern = "-")]
#[regex_token(
    name = "Number",
    regex = r"[0-9]+",
    transformer_fn = nested::match_number,
    kind = nested::Int
)]
#[ignore_pattern(regex = r"\s+")]
#[grammar = r"Int ::= Number | Minus Int"]
#[grammar = r"Expr ::= Int ExprCont?"]
#[grammar = r"ExprCont ::= ('+' | '-') Int ExprCont?"]
#[simple_types]
#[depth_limit = 10]
pub(crate) struct Parser;

impl Int {
    fn eval(&self) -> i64 {
        match &self.0 {
            IntChoice1::_1(TokenNumber(n)) => **n,
            IntChoice1::_2((_, e)) => -1 * e.eval(),
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
    let a = Parser::Expr(a.into()).unwrap();
    let a = a.1.eval();

    println!("{:#?}", a);
}
