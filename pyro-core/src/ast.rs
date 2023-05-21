use crate::sym::Literal;
use std::collections::VecDeque;

#[derive(Debug, PartialEq, Eq)]
pub struct Program<A> {
    pub procs: VecDeque<Tag<Proc<A>, A>>,
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
    Decl(Decl<A>, Tag<Box<Proc<A>>, A>),
    Cond(Tag<Val, A>, Tag<Box<Proc<A>>, A>, Tag<Box<Proc<A>>, A>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Abs<A> {
    pub pattern: Pat,
    pub proc: Box<Proc<A>>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Pat {
    Var(PatVar),
    Record(Record<Pat>),
    Wildcard(Type),
}

#[derive(Debug, PartialEq, Eq)]
pub struct PatVar {
    pub var: Var,
    pub pattern: Option<Box<Pat>>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Val {
    Literal(Literal),
    Path(Vec<String>),
    Record(Record<Val>),
}

impl std::fmt::Display for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Literal(l) => write!(f, "{}", l),
            Val::Path(p) => write!(f, "{}", p.join(".")),
            Val::Record(r) => write!(f, "{}", r),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Var {
    pub id: String,
    pub r#type: Type,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Type {
    Name(String),
    Channel(Box<Type>),
    Anonymous,
    Record(Record<Type>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Record<A> {
    pub props: VecDeque<Prop<A>>,
}

impl<A> std::fmt::Display for Record<A>
where
    A: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;

        let mut first = true;
        for prop in &self.props {
            if first {
                first = false;
                write!(f, ", ")?;
            }

            write!(f, "{}", prop)?;
        }

        write!(f, "]")
    }
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

    pub fn traverse_result<F, B, E>(self, fun: F) -> Result<Record<B>, E>
    where
        F: Fn(A) -> Result<B, E>,
    {
        let mut props = VecDeque::new();

        for prop in self.props {
            props.push_back(prop.traverse_result(&fun)?);
        }

        Ok(Record { props })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Prop<A> {
    pub label: Option<String>,
    pub val: A,
}

impl<A> std::fmt::Display for Prop<A>
where
    A: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(label) = self.label.as_ref() {
            write!(f, "{}: ", label)?;
        }

        write!(f, "{}", self.val)
    }
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

    pub fn traverse_result<F, B, E>(self, fun: F) -> Result<Prop<B>, E>
    where
        F: FnOnce(A) -> Result<B, E>,
    {
        let val = fun(self.val)?;

        Ok(Prop {
            val,
            label: self.label,
        })
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Def<A> {
    pub name: String,
    pub abs: Abs<A>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Decl<A> {
    Channel(String, Type),
    Def(VecDeque<Def<A>>),
    Type(String, Type),
}
