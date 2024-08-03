#[derive(Debug)]
pub enum Expr {
    Number(i64),
    Expr(Operation),
}

#[derive(Debug)]
pub enum Operator {
    Plus,
    Minus,
}

#[derive(Debug)]
pub struct Operation {
    pub left: i64,
    pub operator: Operator,
    pub right: Box<Expr>,
}

impl Operation {
    pub fn execute(&self) -> i64 {
        let right = self.right.execute();

        match self.operator {
            Operator::Plus => self.left + right,
            Operator::Minus => self.left - right,
        }
    }
}

impl Expr {
    pub fn execute(&self) -> i64 {
        match self {
            Expr::Number(n) => *n,
            Expr::Expr(op) => op.execute(),
        }
    }
}
