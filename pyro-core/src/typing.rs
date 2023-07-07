mod dynamic;
pub(crate) mod nominal;
#[cfg(test)]
mod tests;

use crate::ast::{Prop, Record};
use crate::context::{LocalScope, Scope};
use crate::sym::{Literal, TypeSym};
pub use crate::typing::dynamic::DynamicTyping;
use crate::{Pos, STDLIB};
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypePointer {
    Ref(TypeRef),
    Rec(Record<TypePointer>),
    Fun(Box<TypePointer>, Box<TypePointer>),
    App(Box<TypePointer>, Box<TypePointer>),
    ForAll(bool, LocalScope, Vec<String>, Box<TypePointer>),
    Qual(Vec<TypePointer>, Box<TypePointer>),
}

impl TypePointer {
    pub fn as_type_ref(&self) -> &TypeRef {
        if let TypePointer::Ref(type_ref) = self {
            return type_ref;
        }

        panic!("Provided type pointer is not reference")
    }

    pub fn is_type_ref(&self) -> bool {
        if let TypePointer::Ref(_) = self {
            return true;
        }

        false
    }

    pub fn app(a: TypePointer, b: TypePointer) -> Self {
        TypePointer::App(Box::new(a), Box::new(b))
    }

    pub fn fun(a: TypePointer, b: TypePointer) -> Self {
        TypePointer::Fun(Box::new(a), Box::new(b))
    }

    pub fn rec(props: Vec<Prop<TypePointer>>) -> Self {
        TypePointer::Rec(Record { props })
    }

    pub fn process() -> TypePointer {
        TypePointer::Ref(TypeRef {
            scope: STDLIB.as_local_scope(),
            name: "Process".to_string(),
        })
    }

    pub fn bool() -> TypePointer {
        TypePointer::Ref(TypeRef {
            scope: STDLIB.as_local_scope(),
            name: "Bool".to_string(),
        })
    }

    pub fn integer() -> TypePointer {
        TypePointer::Ref(TypeRef {
            scope: STDLIB.as_local_scope(),
            name: "Integer".to_string(),
        })
    }

    pub fn char() -> TypePointer {
        TypePointer::Ref(TypeRef {
            scope: STDLIB.as_local_scope(),
            name: "Char".to_string(),
        })
    }

    pub fn client() -> TypePointer {
        TypePointer::Ref(TypeRef {
            scope: STDLIB.as_local_scope(),
            name: "Client".to_string(),
        })
    }

    pub fn string() -> TypePointer {
        TypePointer::Ref(TypeRef {
            scope: STDLIB.as_local_scope(),
            name: "String".to_string(),
        })
    }

    pub fn show() -> TypePointer {
        TypePointer::Ref(TypeRef {
            scope: STDLIB.as_local_scope(),
            name: "Show".to_string(),
        })
    }
}

#[derive(Clone)]
pub struct TypeInfo {
    pub pos: Pos,
    pub pointer: TypePointer,
}

#[derive(Clone, Eq, PartialEq, Debug)]
pub enum Type {
    /// Universally quantified type.
    ForAll {
        explicit: bool,
        binders: Vec<String>,
        body: Box<Type>,
    },
    /// Type constraint
    Qual { ctx: Vec<Type>, body: Box<Type> },
    /// Type variable.
    Var { name: String },
    /// Function type.
    Fun { lhs: Box<Type>, rhs: Box<Type> },
    /// Type application.
    App { lhs: Box<Type>, rhs: Box<Type> },
    /// Record
    Rec { props: Record<Type> },
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::ForAll {
                explicit,
                binders,
                body,
            } => {
                if *explicit {
                    write!(f, "forall ")?;

                    for (idx, bind) in binders.iter().enumerate() {
                        if idx == 0 {
                            write!(f, "{}", bind)?;
                        } else {
                            write!(f, " {}", bind)?;
                        }
                    }

                    write!(f, ". ")?;
                }

                body.fmt(f)
            }

            Type::Qual { ctx, body } => {
                write!(f, "(")?;
                for (idx, constr) in ctx.iter().enumerate() {
                    if idx != 0 {
                        write!(f, ", ")?;
                    }

                    constr.fmt(f)?;
                }
                write!(f, ") => ")?;
                body.fmt(f)
            }

            Type::Var { name } => write!(f, "{}", name),

            Type::Fun { lhs, rhs } => {
                lhs.fmt(f)?;
                write!(f, " -> ")?;
                rhs.fmt(f)
            }

            Type::App { lhs, rhs } => {
                write!(f, "(")?;
                lhs.fmt(f)?;
                write!(f, " ")?;
                rhs.fmt(f)?;
                write!(f, ")")
            }

            Type::Rec { props } => write!(f, "{}", props),
        }
    }
}

impl Type {
    pub fn from_literal(lit: &Literal) -> Self {
        match lit {
            Literal::Integer(_) => Self::integer(),
            Literal::String(_) => Self::string(),
            Literal::Char(_) => Self::char(),
            Literal::Bool(_) => Self::bool(),
        }
    }

    pub fn inner(&self) -> &Type {
        if let Type::App { rhs, .. } = self {
            return rhs;
        }

        self
    }

    pub fn inner_mut(&mut self) -> &mut Type {
        if let Type::App { rhs, .. } = self {
            return rhs;
        }

        self
    }

    pub fn named(name: impl AsRef<str>) -> Self {
        Type::Var {
            name: name.as_ref().to_string(),
        }
    }

    pub fn func(param: Type, result: Type) -> Self {
        Type::Fun {
            lhs: Box::new(param),
            rhs: Box::new(result),
        }
    }

    pub fn app(constr: Type, inner: Type) -> Self {
        Type::App {
            lhs: Box::new(constr),
            rhs: Box::new(inner),
        }
    }

    pub fn client(inner: Type) -> Self {
        Type::App {
            lhs: Box::new(Type::named("Client")),
            rhs: Box::new(inner),
        }
    }

    pub fn server(inner: Type) -> Self {
        Type::App {
            lhs: Box::new(Type::named("Server")),
            rhs: Box::new(inner),
        }
    }

    pub fn channel(inner: Type) -> Self {
        Type::App {
            lhs: Box::new(Type::named("Channel")),
            rhs: Box::new(inner),
        }
    }

    pub fn process() -> Self {
        Type::named("Process")
    }

    pub fn integer() -> Self {
        Type::named("Integer")
    }

    pub fn string() -> Self {
        Type::named("String")
    }

    pub fn char() -> Self {
        Type::named("Char")
    }

    pub fn bool() -> Self {
        Type::named("Bool")
    }

    pub fn record(props: Vec<Prop<Type>>) -> Self {
        Type::Rec {
            props: Record { props },
        }
    }

    pub fn is_generic(&self) -> bool {
        if let Type::Var { name } = self {
            return name.chars().nth(0).unwrap().is_ascii_lowercase();
        }

        false
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypeStatus {
    NeedInference,
    Generic,
    Defined,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Dict {
    pub impls: HashSet<String>,
    pub status: TypeStatus,
    pub r#type: Type,
}

impl Dict {
    pub fn new(r#type: Type) -> Self {
        Self::with_impls(r#type, vec![])
    }

    pub fn with_impls<I>(r#type: Type, types: I) -> Self
    where
        I: IntoIterator<Item = String>,
    {
        let mut impls = HashSet::new();

        for r#type in types {
            impls.insert(r#type);
        }

        Self {
            r#type,
            impls,
            status: TypeStatus::Defined,
        }
    }

    pub fn implements(&self, r#type: &str) -> bool {
        self.impls.contains(r#type)
    }

    pub fn add(&mut self, r#type: impl AsRef<str>) {
        self.impls.insert(r#type.as_ref().to_string());
    }

    pub fn is_generic(&self) -> bool {
        let mut r#type = &self.r#type;
        loop {
            match r#type {
                Type::Var { name } => {
                    return is_generic_name(name);
                }

                Type::App { lhs, .. } => {
                    r#type = lhs.as_ref();
                }

                _ => return false,
            }
        }
    }
}

#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct TypeRef {
    pub scope: LocalScope,
    pub name: String,
}

#[derive(Default, Clone)]
pub struct UsedVariables {
    inner: HashMap<u32, HashSet<String>>,
}

impl UsedVariables {
    pub fn list_used_variables<S: Scope>(&self, scope: &S) -> HashSet<String> {
        let mut set = HashSet::new();

        for scope_id in scope.ancestors().iter().rev() {
            if let Some(used) = self.inner.get(scope_id) {
                set.extend(used.clone());
            }
        }

        set
    }
}

fn is_generic_name(name: &str) -> bool {
    name.chars().nth(0).unwrap() == '\''
}

#[derive(Clone)]
pub enum TypeCreated {
    Defined {
        name: String,
        constraints: Vec<String>,
        generic: bool,
    },

    Inferred,
}

#[derive(Clone)]
pub enum Event {
    PushScope,
    PopScope,
    SetScope(LocalScope),

    VariableIntroduced {
        type_ref: TypeRef,
        location: TypePointer,
    },

    TypeCreated(TypeCreated),

    TypeConstraintSuggested {
        type_ref: TypeRef,
        constraint: String,
    },

    TypeSuggested {
        target: TypeRef,
        suggested: TypePointer,
    },
}

pub trait Machine: Clone {
    type Model: Types;

    fn apply(&mut self, event: Event);
    fn model(&self) -> &Self::Model;
}

pub trait Types {
    fn project(&self, target: &TypePointer) -> Type;
    fn type_check(&self, expect: &TypePointer, provided: &TypePointer) -> bool;
    fn look_up<S: Scope>(&self, scope: &S, name: &str) -> Option<TypePointer>;

    fn current_scope(&self) -> LocalScope;

    /// What a terrible name to describe a function that returns a random name based on the current
    /// type machine state. If the internal state hasn't changed, this function will return the same
    /// value.
    fn current_available_name(&self) -> TypeRef;

    fn as_function<'a>(
        &'a self,
        r#type: &'a TypePointer,
    ) -> Option<(&'a TypePointer, &'a TypePointer)>;

    fn as_type_constructor<'a>(
        &'a self,
        r#type: &'a TypePointer,
    ) -> Option<(&'a TypePointer, &'a TypePointer)>;

    fn type_implements(&self, r#type: &TypePointer, constraint: &str) -> bool;
}

#[derive(Clone)]
pub struct TypeSystem<A> {
    machine: A,
}

impl<M> TypeSystem<M> {
    pub fn new(machine: M) -> TypeSystem<M> {
        Self { machine }
    }
}

impl<M: Machine> TypeSystem<M> {
    pub fn implements(&self, target: &TypePointer, constraint: &str) -> bool {
        self.machine.model().type_implements(target, constraint)
    }

    pub fn project_type(&self, target: &TypePointer) -> Type {
        self.machine.model().project(target)
    }

    pub fn param_matches(&self, expect: &TypePointer, provided: &TypePointer) -> bool {
        self.machine.model().type_check(expect, provided)
    }

    pub fn suggest_constraint(&mut self, target: &TypePointer, constraint: &str) {
        if let TypePointer::Ref(target) = target {
            self.machine.apply(Event::TypeConstraintSuggested {
                type_ref: target.clone(),
                constraint: constraint.to_string(),
            })
        }
    }

    pub fn suggest_type(&mut self, target: &TypePointer, provided: &TypePointer) {
        if let TypePointer::Ref(target) = target {
            self.machine.apply(Event::TypeSuggested {
                target: target.clone(),
                suggested: provided.clone(),
            })
        }
    }

    pub fn suggest_inner_type(&mut self, target: &TypePointer, provided: &TypePointer) {
        if let Some((_, target)) = self.as_type_constructor(target) {
            self.suggest_type(&target, provided);
        }
    }

    pub fn push_scope(&mut self) -> LocalScope {
        self.machine.apply(Event::PushScope);
        self.machine.model().current_scope()
    }

    pub fn pop_scope(&mut self) {
        self.machine.apply(Event::PopScope);
    }

    pub fn set_scope<S: Scope>(&mut self, scope: &S) {
        self.machine.apply(Event::SetScope(scope.as_local()));
    }

    pub fn current_scope(&self) -> LocalScope {
        self.machine.model().current_scope()
    }

    pub fn look_up<S: Scope>(&self, scope: &S, name: impl AsRef<str>) -> Option<TypePointer> {
        self.machine.model().look_up(scope, name.as_ref())
    }

    pub fn declare_from_pointer(
        &mut self,
        name: impl AsRef<str>,
        location: &TypePointer,
    ) -> TypePointer {
        let scope = self.machine.model().current_scope();
        let type_ref = TypeRef {
            scope,
            name: name.as_ref().to_string(),
        };

        self.machine.apply(Event::VariableIntroduced {
            type_ref: type_ref.clone(),
            location: location.clone(),
        });

        TypePointer::Ref(type_ref)
    }

    pub fn declare_from_dict(&mut self, name: impl AsRef<str>, dict: Dict) -> TypePointer {
        let scope = self.machine.model().current_scope();
        let type_ref = TypeRef {
            scope,
            name: name.as_ref().to_string(),
        };

        self.machine.apply(Event::TypeCreated(TypeCreated::Defined {
            name: name.as_ref().to_string(),
            constraints: dict.impls.into_iter().collect(),
            generic: false,
        }));

        TypePointer::Ref(type_ref)
    }

    pub fn look_up_type_or_fail<S: Scope>(
        &mut self,
        scope: &S,
        sym: &TypeSym,
    ) -> Result<TypePointer, String> {
        match sym {
            TypeSym::Name(n) => {
                if let Some(pointer) = self.look_up(scope, n.as_str()) {
                    Ok(pointer)
                } else {
                    Err(n.clone())
                }
            }

            TypeSym::App(c, i) => {
                let c = self.look_up_type_or_fail(scope, c.as_ref())?;
                let i = self.look_up_type_or_fail(scope, i.as_ref())?;

                Ok(TypePointer::App(Box::new(c), Box::new(i)))
            }

            TypeSym::Rec(rec) => {
                let mut props = Vec::new();
                for prop in &rec.props {
                    props.push(Prop {
                        label: prop.label.clone(),
                        val: self.look_up_type_or_fail(scope, &prop.val)?,
                    });
                }

                Ok(TypePointer::Rec(Record { props }))
            }

            TypeSym::Unknown => Ok(self.declare_inferred_type()),
        }
    }

    pub fn new_generic(&mut self, name: impl AsRef<str>) -> TypePointer {
        self.machine.apply(Event::TypeCreated(TypeCreated::Defined {
            name: name.as_ref().to_string(),
            constraints: vec![],
            generic: true,
        }));

        TypePointer::Ref(TypeRef {
            scope: self.machine.model().current_scope(),
            name: name.as_ref().to_string(),
        })
    }

    pub fn declare_inferred_type(&mut self) -> TypePointer {
        let type_ref = self.machine.model().current_available_name();

        self.machine
            .apply(Event::TypeCreated(TypeCreated::Inferred));

        TypePointer::Ref(type_ref)
    }

    pub fn as_function<'a>(
        &'a self,
        r#type: &'a TypePointer,
    ) -> Option<(&'a TypePointer, &'a TypePointer)> {
        self.machine.model().as_function(r#type)
    }

    pub fn as_type_constructor<'a>(
        &self,
        r#type: &TypePointer,
    ) -> Option<(TypePointer, TypePointer)> {
        self.machine
            .model()
            .as_type_constructor(r#type)
            .map(|(a, b)| (a.clone(), b.clone()))
    }
}
