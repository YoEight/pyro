use crate::sym::Literal;

#[derive(Debug, PartialEq, Eq)]
pub struct Program<A> {
    pub procs: Vec<Tag<Proc<A>, A>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Tag<I, A> {
    pub item: I,
    pub tag: A,
}

impl<I, A> Tag<Box<I>, A> {
    pub fn unbox(self) -> Tag<I, A> {
        Tag {
            item: *self.item,
            tag: self.tag,
        }
    }
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
    Wildcard(Type),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct PatVar<A> {
    pub var: Var,
    pub pattern: Option<Box<Pat<A>>>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Val<A> {
    Literal(Literal),
    Path(Vec<String>),
    Record(Record<Tag<Val<A>, A>>),
}

impl<A> std::fmt::Display for Val<A> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Val::Literal(l) => write!(f, "{}", l),
            Val::Path(p) => write!(f, "{}", p.join(".")),
            Val::Record(r) => write!(f, "{}", r),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Var {
    pub id: String,
    pub r#type: Type,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Name(String),
    Channel(Box<Type>),
    Anonymous,
    Record(Record<Type>),
    Process,
}

impl Type {
    pub fn typecheck(&self, other: &Type) -> bool {
        if self == &Type::Anonymous || other == &Type::Anonymous {
            return true;
        }

        self == other
    }

    pub fn inner_type(&self) -> Type {
        if let Type::Channel(typ) = self {
            return *typ.clone();
        }

        self.clone()
    }

    pub fn is_channel(&self) -> bool {
        match self {
            Type::Anonymous | Type::Channel(_) => true,
            _ => false,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Name(n) => write!(f, "{}", n),
            Type::Channel(t) => write!(f, "^{}", t),
            Type::Anonymous => write!(f, "<anonymous>"), // TODO - we could implement universal type.
            Type::Process => write!(f, "Process"), // TODO - we could implement universal type.
            Type::Record(r) => write!(f, "{}", r),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Record<A> {
    pub props: Vec<Prop<A>>,
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

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Def<A> {
    pub name: String,
    pub abs: Tag<Abs<A>, A>,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Decl<A> {
    Channel(String, Type),
    Def(Vec<Def<A>>),
    Type(String, Type),
}
