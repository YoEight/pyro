use crate::sym::Literal;
use std::collections::VecDeque;

#[derive(Debug, PartialEq, Eq)]
pub struct Program<A> {
    pub proc: Tag<Proc<A>, A>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Tag<I, A> {
    pub item: I,
    pub tag: A,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Proc<A> {
    Output(Tag<Val, A>, Tag<Val, A>),
    Input(Tag<Val, A>, Tag<Abs, A>),
    Null, // ()
    Parallel(Tag<Box<Proc<A>>, A>, Tag<Box<Proc<A>>, A>),
    Decl(Tag<Box<Proc<A>>, A>),
    Cond(Tag<Val, A>, Tag<Box<Proc<A>>, A>, Tag<Box<Proc<A>>, A>),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Abs {}

#[derive(Debug, PartialEq, Eq)]
pub enum Val {
    Literal(Literal),
    Path(VecDeque<String>),
}
