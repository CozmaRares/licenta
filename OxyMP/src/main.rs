use oxymp::RecursiveDescent;
use oxymp_util::lexer::{LexRule, Lexer, TokenHandler, TokenMatcher};
use std::{ops::Deref, rc::Rc};

fn match_number(matched: &str) -> i64 {
    matched.parse().unwrap()
}

#[derive(RecursiveDescent)]
#[exact_token(name = "ParanLeft", pattern = "(")]
#[exact_token(name = "ParanRight", pattern = ")")]
#[exact_token(name = "Plus", pattern = "+")]
#[exact_token(name = "Minus", pattern = "-")]
#[regex_token(
    name = "Number",
    regex = r"-?[0-9]+",
    transformer_fn = "match_number",
    kind = "i64"
)]
#[ignore_pattern(regex = r"\s+")]
#[grammar = r"expr ::= Number (('+' | '-') expr)?"]
#[simple_types]
#[depth_limit = 10]
pub(crate) struct Parser;

impl expr {
    fn eval(self) -> i64 {
        match self.0 {
            (TokenNumber(n), None) => *n,
            (TokenNumber(n), Some((expr_choice_1::_1(_), e))) => *n + e.eval(),
            (TokenNumber(n), Some((expr_choice_1::_2(_), e))) => *n - e.eval(),
        }
    }
}

fn main() {
    let l = create_lexer();

    let a = l.tokenize("1+2+3").unwrap();
    let a = Parser::expr(a.into()).unwrap();
    let a = a.1.eval();

    println!("{:#?}", a);
}
