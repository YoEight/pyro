use crate::sym::{Literal, TypeSym};
use std::future::Future;

#[derive(Debug, PartialEq, Eq)]
pub struct Program<A> {
    pub procs: Vec<Tag<Proc<A>, A>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Tag<I, A> {
    pub item: I,
    pub tag: A,
}

impl<I, A> std::fmt::Display for Tag<I, A>
where
    I: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.item)
    }
}

impl<I, A> Tag<I, A> {
    pub fn map_item<F, J>(self, fun: F) -> Tag<J, A>
    where
        F: FnOnce(I) -> J,
    {
        Tag {
            item: fun(self.item),
            tag: self.tag,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Proc<A> {
    Output(Tag<Val<A>, A>, Tag<Val<A>, A>),
    Input(Tag<Val<A>, A>, Tag<Abs<A>, A>),
    Null, // ()
    Parallel(Vec<Tag<Proc<A>, A>>),
    Decl(Tag<Decl<A>, A>, Box<Tag<Proc<A>, A>>),
    Cond(Tag<Val<A>, A>, Box<Tag<Proc<A>, A>>, Box<Tag<Proc<A>, A>>),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Abs<A> {
    pub pattern: Tag<Pat<A>, A>,
    pub proc: Box<Tag<Proc<A>, A>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Pat<A> {
    Var(PatVar<A>),
    Record(Record<Tag<Pat<A>, A>>),
    Wildcard(TypeSym),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PatVar<A> {
    pub var: Var<A>,
    pub pattern: Option<Box<Tag<Pat<A>, A>>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Val<A> {
    Literal(Literal),
    Path(Vec<String>),
    Record(Record<Tag<Val<A>, A>>),
    AnoClient(Tag<Abs<A>, A>),
    App(Box<Tag<Val<A>, A>>, Box<Tag<Val<A>, A>>),
}

impl<A> std::fmt::Display for Val<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Literal(l) => write!(f, "{}", l),
            Val::Path(p) => write!(f, "{}", p.join(".")),
            Val::Record(r) => write!(f, "{}", r),
            Val::AnoClient(_) => write!(f, "<anonymous client>"),
            Val::App(fun, a) => {
                write!(f, "{} -> ", fun)?;
                write!(f, "{}", a)
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Var<A> {
    pub id: String,
    pub r#type: TypeSym,
    pub tag: A,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Record<A> {
    pub props: Vec<Prop<A>>,
}

impl<A> Default for Record<A> {
    fn default() -> Self {
        Record { props: vec![] }
    }
}

impl<A> Record<A> {
    pub fn empty() -> Self {
        Self { props: vec![] }
    }
}

impl<A> std::fmt::Display for Record<A>
where
    A: std::fmt::Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[")?;

        let mut first = true;
        for prop in &self.props {
            if !first {
                write!(f, ", ")?;
            }

            first = false;
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
            .collect::<Vec<_>>();

        Record { props }
    }

    pub fn traverse_result<F, B, E>(self, mut fun: F) -> Result<Record<B>, E>
    where
        F: FnMut(A) -> Result<B, E>,
    {
        let mut props = Vec::new();

        for prop in self.props {
            props.push(prop.traverse_result(&mut fun)?);
        }

        Ok(Record { props })
    }

    pub async fn traverse_result_async<F, Fut, B, E>(self, mut fun: F) -> Result<Record<B>, E>
    where
        F: FnMut(A) -> Fut,
        Fut: Future<Output = Result<B, E>>,
    {
        let mut props = Vec::new();

        for prop in self.props {
            props.push(prop.traverse_result_async(&mut fun).await?);
        }

        Ok(Record { props })
    }

    pub fn fold<B, F>(&self, def: B, fun: F) -> B
    where
        F: Fn(&A, B) -> B,
    {
        let mut acc = def;

        for prop in &self.props {
            acc = fun(&prop.val, acc);
        }

        acc
    }

    pub fn for_each<F>(&mut self, mut fun: F)
    where
        F: FnMut(&mut A),
    {
        for prop in self.props.iter_mut() {
            fun(&mut prop.val)
        }
    }

    pub fn find_by_prop(&self, name: &str) -> Option<Prop<A>>
    where
        A: Clone,
    {
        for prop in &self.props {
            if let Some(label) = &prop.label {
                if label == name {
                    return Some(prop.clone());
                }
            }
        }

        None
    }

    pub fn is_array(&self) -> bool {
        self.props.iter().all(|p| p.label.is_none())
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
    pub fn new(label: Option<impl AsRef<str>>, val: A) -> Self {
        Prop {
            label: label.map(|s| s.as_ref().to_string()),
            val,
        }
    }

    pub fn ano(val: A) -> Self {
        Self::new(None::<String>, val)
    }

    pub fn map<F, B>(self, fun: F) -> Prop<B>
    where
        F: FnOnce(A) -> B,
    {
        Prop {
            val: fun(self.val),
            label: self.label,
        }
    }

    pub fn replace<B>(self, val: B) -> (A, Prop<B>) {
        let old = self.val;

        (
            old,
            Prop {
                label: self.label,
                val,
            },
        )
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

    pub(crate) async fn traverse_result_async<F, Fut, B, E>(self, fun: &mut F) -> Result<Prop<B>, E>
    where
        F: FnMut(A) -> Fut,
        Fut: Future<Output = Result<B, E>>,
    {
        let val = fun(self.val).await?;

        Ok(Prop {
            label: self.label,
            val,
        })
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Def<A> {
    pub name: String,
    pub abs: Tag<Abs<A>, A>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Decl<A> {
    Channels(Vec<Tag<(String, TypeSym), A>>),
    Def(Vec<Tag<Def<A>, A>>),
    Type(String, TypeSym),
}
