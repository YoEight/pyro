use crate::literal::Literal;

pub enum Proc {
    Null,
    Parallel(Box<Proc>, Box<Proc>),
    Decl(Box<Proc>),
}

pub enum Val {
    Literal(Literal),
    Path(Path),
}

pub enum Path {
    Ident(String),
    Dot(Box<Path>, String),
}
