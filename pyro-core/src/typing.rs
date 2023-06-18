use crate::ast::Record;
use crate::context::Scope;
use crate::utils::generate_generic_type_name;
use crate::STDLIB;
use std::collections::{HashMap, HashSet};

#[derive(Clone)]
pub enum Type {
    /// Universally quantified type.
    ForAll {
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

impl Type {
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
}

#[derive(Clone)]
pub struct Dict {
    pub impls: HashSet<String>,
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

        Self { r#type, impls }
    }

    pub fn implements(&self, r#type: &str) -> bool {
        self.impls.contains(r#type)
    }

    pub fn add(&mut self, r#type: impl AsRef<str>) {
        self.impls.insert(r#type.as_ref().to_string());
    }
}

#[derive(Clone)]
pub struct TypeRef {
    scope: u32,
    name: String,
}

#[derive(Clone, Default)]
struct ScopedTypes {
    name_gen: usize,
    types: HashMap<String, Dict>,
}

impl ScopedTypes {
    fn insert(&mut self, name: impl AsRef<str>, dict: Dict) {
        self.types.insert(name.as_ref().to_string(), dict);
    }
}

#[derive(Clone, Default)]
pub struct Knowledge {
    inner: HashMap<u32, ScopedTypes>,
}

impl Knowledge {
    pub fn standard() -> Self {
        let mut this = Knowledge::default();
        let types = this.inner.entry(STDLIB.id()).or_default();

        types.insert(
            "Bool",
            Dict::with_impls(Type::bool(), vec!["Show".to_string()]),
        );

        types.insert(
            "Integer",
            Dict::with_impls(Type::integer(), vec!["Show".to_string()]),
        );

        types.insert(
            "String",
            Dict::with_impls(Type::string(), vec!["Show".to_string()]),
        );

        types.insert(
            "Char",
            Dict::with_impls(Type::char(), vec!["Show".to_string()]),
        );

        this
    }

    pub fn declare<S: Scope>(&mut self, scope: &S, name: impl AsRef<str>, dict: Dict) -> TypeRef {
        let types = self.inner.entry(scope.id()).or_default();
        let type_ref = TypeRef {
            scope: scope.id(),
            name: name.as_ref().to_string(),
        };

        types.insert(name.as_ref().to_string(), dict);

        type_ref
    }

    pub fn dict_mut(&mut self, id: &TypeRef) -> &mut Dict {
        self.inner
            .entry(id.scope)
            .or_default()
            .types
            .get_mut(id.name.as_str())
            .unwrap()
    }

    pub fn dict(&self, id: &TypeRef) -> &Dict {
        self.inner
            .get(&id.scope)
            .unwrap()
            .types
            .get(id.name.as_str())
            .unwrap()
    }

    pub fn look_up<S: Scope>(&self, scope: &S, name: &str) -> Option<TypeRef> {
        self.inner.get(&scope.id())?.types.get(name)?;

        Some(TypeRef {
            scope: scope.id(),
            name: name.to_string(),
        })
    }

    pub fn new_generic<S: Scope>(&mut self, scope: &S) -> TypeRef {
        let types = self.inner.entry(scope.id()).or_default();
        let id = types.name_gen;
        let name = generate_generic_type_name(id);
        types.name_gen += 1;

        types.insert(&name, Dict::new(Type::named(name.clone())));

        TypeRef {
            scope: scope.id(),
            name,
        }
    }
}
