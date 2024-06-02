pub enum Stmt {
    Expr(Expr),
    Let(Name, Expr),
}

pub struct Name {
    value: String,
}

pub enum Expr {
    IntLit(i64),
    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
}
