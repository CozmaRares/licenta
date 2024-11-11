use std::rc::Rc;

use oxymp::RecursiveDescent;
use oxymp_util::{
    lexer::{LexError, LexRule, Lexer, LexerBuilder, TokenHandler, TokenMatcher},
    parser::{ParseError, ParseErrorReason, ParserInput, ParserState},
};

pub fn match_number(matched: &str) -> Result<i64, LexError> {
    matched
        .parse()
        .map_err(|_| LexError::UnparsableToken(matched))
}

pub fn match_ident(matched: &str) -> Result<String, LexError> {
    if matched.len() == 1 {
        Ok(matched.to_string())
    } else {
        Err(LexError::UnparsableToken(matched))
    }
}

#[derive(RecursiveDescent)]
#[exact_pattern(name = "While", pattern = "while")]
#[exact_pattern(name = "ParanLeft", pattern = "(")]
#[exact_pattern(name = "ParanRight", pattern = ")")]
#[exact_pattern(name = "If", pattern = "if")]
#[exact_pattern(name = "Else", pattern = "else")]
#[regex_pattern(
    name = "Identifier",
    regex = "[a-z]",
    transformer_fn = match_ident,
    kind = String,
)]
#[exact_pattern(name = "Equal", pattern = "=")]
#[exact_pattern(name = "Plus", pattern = "+")]
#[exact_pattern(name = "Let", pattern = "let")]
#[regex_pattern(
    name = "Number",
    regex = r"[0-9]+",
    transformer_fn = match_number,
    kind = i64
)]
#[ignore_pattern(regex = r"\s+")]
#[grammar = "EWh ::= While '(' E ')' E"]
#[grammar = "EIf ::= If '(' E ')' Else E"]
#[grammar = "EEq ::= Identifier '=' E"]
#[grammar = "EPl1 ::= '+' T EPl1?"]
#[grammar = "EPl ::= T EPl1?"]
#[grammar = "E ::= EWh | EIf | EEq | EPl"]
#[grammar = "T ::= Number | Identifier | '(' E ')'"]
#[simple_types]
#[depth_limit = 10]
pub(crate) struct Parser;

impl EWh {
    fn visit(&self) {
        println!("_s:");
        self.0 .2.visit();
        println!("jz _e");
        self.0 .4.visit();
        println!("jmp _s");
        println!("_e:");
    }
}

impl EIf {
    fn visit(&self) {
        self.0 .2.visit();
        println!("jz _else");
        self.0 .5.visit();
        println!("jmp _after");
        println!("_else:");
        println!("_after")
    }
}

impl EEq {
    fn visit(&self) {
        self.0 .2.visit();
        let TokenIdentifier(var) = &self.0 .0;
        println!("    pop {}", var);
    }
}

impl EPl1 {
    fn visit(&self) {
        self.0 .1.visit();
        println!("    add");
        if let Some(expr) = &self.0 .2 {
            expr.visit();
        }
    }
}

impl EPl {
    fn visit(&self) {
        self.0 .0.visit();
        if let Some(expr) = &self.0 .1 {
            expr.visit();
        }
    }
}

impl E {
    fn visit(&self) {
        match &self.0 {
            EChoice1::_1(ewh) => ewh.visit(),
            EChoice1::_2(eif) => eif.visit(),
            EChoice1::_3(eeq) => eeq.visit(),
            EChoice1::_4(epl) => epl.visit(),
        }
    }
}

impl T {
    fn visit(&self) {
        match &self.0 {
            TChoice1::_1(TokenNumber(n)) => println!("    push {}", *n),
            TChoice1::_2(TokenIdentifier(s)) => println!("    push {}", s),
            TChoice1::_3((_, e, _)) => e.visit(),
        }
    }
}

fn main() {
    let l = create_lexer();

    let a = l.tokenize("x = 1 + 2 + 3").unwrap();
    let a = Parser::e(a.into()).unwrap();
    a.1.visit();

    let a = l.tokenize("x + 1").unwrap();
    let a = Parser::e(a.into()).unwrap();
    a.1.visit();

    let a = l.tokenize("while (1) x").unwrap();
    let a = Parser::e(a.into()).unwrap();
    a.1.visit();
}
