use crate::sym::Literal;
use crate::{Error, Pos};

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
pub struct Var {
    pub id: String,
    pub r#type: Type,
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum Type {
    Name {
        parent: Vec<Type>,
        name: String,
        kind: u16,
        generic: bool,
    },
    App(Box<Type>, Box<Type>),
    Anonymous,
    Record(Record<Type>),
}

impl Type {
    pub fn inner_type(&self) -> Self {
        if let Type::App(_, r) = self {
            return r.as_ref().clone();
        }

        self.clone()
    }

    pub fn ancestor_tree_contains(&self, r#type: &Type) -> bool {
        if r#type == &Type::Anonymous {
            return true;
        }

        if self != r#type {
            match self {
                Type::Name { parent, .. } => parent
                    .iter()
                    .any(|ancestor| ancestor.ancestor_tree_contains(r#type)),

                Type::App(self_cons, self_inner) => {
                    if let Type::App(type_cons, type_inner) = r#type {
                        self_cons.ancestor_tree_contains(type_cons)
                            && self_inner.ancestor_tree_contains(type_inner)
                    } else {
                        self_cons.ancestor_tree_contains(r#type)
                    }
                }
                Type::Anonymous => true,
                Type::Record(self_rec) => {
                    if let Type::Record(rec_type) = r#type {
                        if self_rec.props.len() != rec_type.props.len() {
                            return false;
                        }

                        for (rec_prop, type_prop) in
                            self_rec.props.iter().zip(rec_type.props.iter())
                        {
                            if !rec_prop.val.ancestor_tree_contains(&type_prop.val) {
                                return false;
                            }
                        }

                        true
                    } else {
                        false
                    }
                }
            }
        } else {
            true
        }
    }

    pub fn parent_type_of(&self, child: &Type) -> bool {
        child.ancestor_tree_contains(self)
    }

    pub fn apply(&self, param: &Type) -> Option<Type> {
        match self {
            Type::App(caller, tail) => {
                if !caller.ancestor_tree_contains(&Type::func_type()) {
                    return None;
                }

                if let Type::App(exp_typ, result) = tail.as_ref() {
                    if exp_typ.parent_type_of(param) {
                        return Some(*result.clone());
                    }
                }

                None
            }

            _ => None,
        }
    }

    pub fn kind(&self, location: Pos) -> crate::Result<u16> {
        match self {
            Type::Name { kind, .. } => Ok(*kind),
            Type::Anonymous => Ok(0),
            Type::Record(_) => Ok(0),

            Type::App(l, r) => {
                let l_kind = l.kind(location)?;
                let r_kind = r.kind(location)?;

                if l_kind < r_kind {
                    return Err(Error {
                        pos: location,
                        message: "Kind mismatch".to_string(),
                    });
                }

                Ok(l_kind - (1 + r_kind))
            }
        }
    }

    pub fn channel(r#type: Type) -> Self {
        Type::App(Box::new(Type::channel_type()), Box::new(r#type))
    }

    pub fn channel_type() -> Self {
        Type::Name {
            parent: vec![Type::client_type(), Type::server_type()],
            name: "Channel".to_string(),
            kind: 1,
            generic: false,
        }
    }

    pub fn client(r#type: Type) -> Self {
        Type::App(Box::new(Type::client_type()), Box::new(r#type))
    }

    pub fn client_type() -> Self {
        Type::Name {
            parent: vec![],
            name: "Client".to_string(),
            kind: 1,
            generic: false,
        }
    }

    pub fn server(r#type: Type) -> Self {
        Type::App(Box::new(Type::server_type()), Box::new(r#type))
    }

    pub fn server_type() -> Self {
        Type::Name {
            parent: vec![],
            name: "Server".to_string(),
            kind: 1,
            generic: false,
        }
    }

    pub fn integer() -> Self {
        Type::Name {
            parent: vec![Type::show()],
            name: "Integer".to_string(),
            kind: 0,
            generic: false,
        }
    }

    pub fn string() -> Self {
        Type::Name {
            parent: vec![Type::show()],
            name: "String".to_string(),
            kind: 0,
            generic: false,
        }
    }

    pub fn char() -> Self {
        Type::Name {
            parent: vec![Type::show()],
            name: "Char".to_string(),
            kind: 0,
            generic: false,
        }
    }

    pub fn bool() -> Self {
        Type::Name {
            parent: vec![Type::show()],
            name: "Bool".to_string(),
            kind: 0,
            generic: false,
        }
    }

    pub fn process() -> Self {
        Type::Name {
            parent: vec![],
            name: "Process".to_string(),
            kind: 0,
            generic: false,
        }
    }

    pub fn show() -> Self {
        Type::Name {
            parent: vec![],
            name: "Show".to_string(),
            kind: 0,
            generic: false,
        }
    }

    pub fn func(param: Type, result: Type) -> Self {
        Type::App(
            Box::new(Type::func_type()),
            Box::new(Type::App(Box::new(param), Box::new(result))),
        )
    }

    pub fn func_type() -> Self {
        Type::Name {
            parent: vec![],
            name: "Fn".to_string(),
            kind: 2,
            generic: false,
        }
    }

    pub fn named(name: impl AsRef<str>) -> Self {
        Type::Name {
            parent: vec![],
            name: name.as_ref().to_string(),
            kind: 0,
            generic: false,
        }
    }

    pub fn typecheck_client_call(&self, params: &Type) -> bool {
        if !self.ancestor_tree_contains(&Type::client_type()) {
            return false;
        }

        self.inner_type().parent_type_of(params)
    }

    pub fn typecheck_server_call(&self, params: &Type) -> bool {
        if !self.ancestor_tree_contains(&Type::server_type()) {
            return false;
        }

        self.inner_type().parent_type_of(params)
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Name { name, kind, .. } => match name.as_str() {
                "Channel" => write!(f, "^"),
                "Client" => write!(f, "!"),
                "Server" => write!(f, "?"),
                _ if *kind != 0 => write!(f, "{} ", name),
                _ => write!(f, "{}", name),
            },

            Type::App(a, b) => {
                a.fmt(f)?;
                b.fmt(f)
            }

            Type::Anonymous => write!(f, "_"),
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
    Channels(Vec<Tag<(String, Type), A>>),
    Def(Vec<Tag<Def<A>, A>>),
    Type(String, Type),
}
