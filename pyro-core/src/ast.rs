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
    Input(Tag<Val, A>, Tag<Abs<A>, A>),
    Null, // ()
    Parallel(VecDeque<Proc<A>>),
    Decl(Tag<Box<Proc<A>>, A>),
    Cond(Tag<Val, A>, Tag<Box<Proc<A>>, A>, Tag<Box<Proc<A>>, A>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Abs<A> {
    pub pattern: Pat,
    pub proc: Box<Proc<A>>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Pat {
    Var(Var),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Val {
    Literal(Literal),
    Path(VecDeque<String>),
    Record(Record<Val>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Var {
    pub id: String,
    pub r#type: Type,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Name(String),
    Channel(String),
    Anonymous,
    Record(Record<Type>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Record<A> {
    pub props: VecDeque<Prop<A>>,
}

impl<A> Record<A> {
    pub fn map<F, B>(self, fun: F) -> Record<B>
    where
        F: Fn(A) -> B,
    {
        let props = self
            .props
            .into_iter()
            .map(move |p| p.map(&fun))
            .collect::<VecDeque<_>>();

        Record { props }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Prop<A> {
    pub label: Option<String>,
    pub val: A,
}

impl<A> Prop<A> {
    pub fn map<F, B>(self, fun: F) -> Prop<B>
    where
        F: FnOnce(A) -> B,
    {
        Prop {
            val: fun(self.val),
            label: self.label,
        }
    }
}
