mod expr;
mod lexer;
mod parser;

use expr::{Expr, Operator};
use lexer::lexer::*;
use parser::{make_lexer, AST};

impl AST {
    pub fn visit(ast: &Self) -> Expr {
        match ast {
            AST::Expr(e) => AST::visit_expr(e),
        }
    }

    pub fn visit_expr(expr: &parser::Expr) -> Expr {
        let parser::token::Number(left) = expr._1;

        if expr._2.is_none() {
            return Expr::Number(left);
        }

        let (choice, expr) = expr._2.as_ref().unwrap();

        let operator = match choice {
            parser::ExprChoice1::_1(_) => Operator::Plus,
            parser::ExprChoice1::_2(_) => Operator::Minus,
        };

        let right = AST::visit_expr(expr);

        return Expr::Expr(expr::Operation {
            left,
            operator,
            right: Box::new(right),
        });
    }
}

fn main() {
    let lex = make_lexer();
    let input = "1+2";
    let toks = lex.tokenize(input).unwrap();

    let ast = parser::Parser::parse(&toks).unwrap();

    let op = AST::visit(&ast);

    println!(
        "{}\n\n{:?}\n\n{:?}\n\n{:?}\n\n{}",
        input,
        toks,
        ast,
        op,
        op.execute()
    );
}
