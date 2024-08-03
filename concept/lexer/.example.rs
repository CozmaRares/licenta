use lexer::lexer::{CanIgnoreToken, Handler, Lexer, Matcher, TokenRule};

mod lexer;

#[derive(Debug, Clone, PartialEq)]
enum Token {
    None,
    Number(u64),
}

impl CanIgnoreToken for Token {
    fn ignore() -> Self {
        Token::None
    }
}

fn main() {
    let rules = vec![TokenRule {
        matcher: Matcher::regex("[0-9]+").unwrap(),
        handler: Handler::Lambda(Box::new(|matched| Token::Number(matched.parse().unwrap()))),
    }];

    let lex = Lexer::new(rules);

    let r = lex.tokenize("123+123");

    println!("{:#?}", r);
}

