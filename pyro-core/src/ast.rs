use crate::literal::Literal;

pub struct Program<A> {
    pub proc: Tag<Proc<A>, A>,
}

pub struct Tag<I, A> {
    pub item: I,
    pub tag: A,
}

pub enum Proc<A> {
    Output(Tag<Val, A>, Tag<Val, A>),
    Input(Val, Abs),
    Null, // ()
    Parallel(Tag<Box<Proc<A>>, A>, Tag<Box<Proc<A>>, A>),
    Decl(Tag<Box<Proc<A>>, A>),
    Cond(Tag<Val, A>, Tag<Box<Proc<A>>, A>, Tag<Box<Proc<A>>, A>),
}

pub enum Abs {}

pub enum Val {
    Literal(Literal),
    Path(Path),
}

pub enum Path {
    Ident(String),
    Dot(Box<Path>, String),
}
