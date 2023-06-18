use crate::ast::{Prop, Record};
use crate::context::{LocalScope, Scope};
use crate::utils::generate_generic_type_name;
use crate::{Ctx, STDLIB};
use std::collections::{HashMap, HashSet};

#[derive(Clone, Eq, PartialEq)]
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
    scope_gen: u32,
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

        types.insert(
            "Client",
            Dict::with_impls(Type::named("Client"), vec!["Send".to_string()]),
        );

        types.insert(
            "Server",
            Dict::with_impls(Type::named("Server"), vec!["Receive".to_string()]),
        );

        types.insert(
            "Channel",
            Dict::with_impls(
                Type::named("Channel"),
                vec!["Send".to_string(), "Receive".to_string()],
            ),
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
        for scope_id in scope.ancestors().iter().rev() {
            if let Some(types) = self.inner.get(&scope_id) {
                if types.types.contains_key(name) {
                    return Some(TypeRef {
                        scope: *scope_id,
                        name: name.to_string(),
                    });
                }
            }
        }

        None
    }

    pub fn look_up_dict<S: Scope>(&self, scope: &S, name: &str) -> Option<&Dict> {
        let type_ref = self.look_up(scope, name)?;

        Some(self.dict(&type_ref))
    }

    pub fn look_up_dict_mut<S: Scope>(&mut self, scope: &S, name: &str) -> Option<&mut Dict> {
        let type_ref = self.look_up(scope, name)?;

        Some(self.dict_mut(&type_ref))
    }

    pub fn type_ref<S: Scope>(&self, scope: &S, r#type: &Type) -> Option<TypeRef> {
        if let Type::Var { name } = r#type {
            return self.look_up(scope, name);
        }

        None
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

    pub fn new_scope<S: Scope>(&mut self, parent: &S) -> LocalScope {
        self.scope_gen += 1;
        let id = self.scope_gen;
        let mut ancestors = parent.ancestors().to_vec();

        ancestors.push(id);

        LocalScope { ancestors }
    }

    pub fn param_matches<S: Scope>(
        &mut self,
        scope: &S,
        require: &Type,
        provided: &TypeRef,
    ) -> bool {
        match require {
            Type::Var { name } => {
                let provided_dict = self.dict(provided);
                let require_dict = self
                    .look_up_dict(scope, name)
                    .expect("Must be defined at that level");

                if is_generic_name(name) {
                    require_dict.impls.is_subset(&provided_dict.impls)
                } else {
                    require_dict.r#type == provided_dict.r#type
                }
            }

            Type::ForAll { binders, body } => {
                let new_scope = self.new_scope(scope);
                for binder in binders {
                    self.declare(&new_scope, binder, Dict::new(Type::named(binder)));
                }

                self.param_matches(&new_scope, body.as_ref(), provided)
            }

            Type::Qual { ctx, body } => {
                for constraint in ctx {
                    if let Type::App { lhs, rhs } = constraint {
                        match (lhs.as_ref(), rhs.as_ref()) {
                            (Type::Var { name: constr_name }, Type::Var { name: var_name }) => {
                                let var_dict = self
                                    .look_up_dict_mut(scope, var_name)
                                    .expect("Must be defined at that level");

                                var_dict.add(constr_name);
                            }

                            _ => unreachable!(),
                        }
                    }
                }

                self.param_matches(scope, body.as_ref(), provided)
            }

            Type::Fun {
                lhs: require_lhs,
                rhs: require_rhs,
            } => {
                let provided_type = { self.dict(provided).r#type.clone() };

                if let Type::Fun {
                    lhs: provided_lhs,
                    rhs: provided_rhs,
                } = &provided_type
                {
                    let param_matches =
                        if let Some(provided) = self.type_ref(scope, provided_lhs.as_ref()) {
                            self.param_matches(scope, require_lhs.as_ref(), &provided)
                        } else {
                            require_lhs == provided_lhs
                        };

                    let result_matches =
                        if let Some(provided) = self.type_ref(scope, provided_rhs.as_ref()) {
                            self.param_matches(scope, require_rhs.as_ref(), &provided)
                        } else {
                            require_rhs == provided_rhs
                        };

                    return param_matches && result_matches;
                }

                false
            }

            Type::App {
                lhs: require_lhs,
                rhs: require_rhs,
            } => {
                let provided_type = { self.dict(provided).r#type.clone() };

                if let Type::App {
                    lhs: provided_lhs,
                    rhs: provided_rhs,
                } = &provided_type
                {
                    let constr_matches =
                        if let Some(provided) = self.type_ref(scope, provided_lhs.as_ref()) {
                            self.param_matches(scope, require_lhs.as_ref(), &provided)
                        } else {
                            require_lhs == provided_lhs
                        };

                    let inner_matches =
                        if let Some(provided) = self.type_ref(scope, provided_rhs.as_ref()) {
                            self.param_matches(scope, require_rhs.as_ref(), &provided)
                        } else {
                            require_rhs == provided_rhs
                        };

                    return constr_matches && inner_matches;
                }

                false
            }

            Type::Rec { props: require_rec } => {
                let provided_type = { self.dict(provided).r#type.clone() };

                if let Type::Rec {
                    props: provided_rec,
                } = &provided_type
                {
                    if require_rec.props.len() != provided_rec.props.len() {
                        return false;
                    }

                    for (require_prop, provided_prop) in
                        require_rec.props.iter().zip(provided_rec.props.iter())
                    {
                        if require_prop.label != provided_prop.label {
                            return false;
                        }

                        let matches =
                            if let Some(provided) = self.type_ref(scope, &provided_prop.val) {
                                self.param_matches(scope, &require_prop.val, &provided)
                            } else {
                                require_prop.val == provided_prop.val
                            };

                        if !matches {
                            return false;
                        }
                    }

                    return true;
                }

                false
            }
        }
    }

    pub fn type_check_send<S: Scope>(
        &mut self,
        scope: &S,
        target: &TypeRef,
        params: &TypeRef,
    ) -> bool {
        let inner_type = {
            let target_dict = self.dict(target);

            if !target_dict.implements("Send") {
                return false;
            }

            if let Type::App { rhs, .. } = &target_dict.r#type {
                rhs.clone()
            } else {
                unreachable!()
            }
        };

        self.param_matches(scope, &inner_type, &params)
    }
}

fn is_generic_name(name: &str) -> bool {
    name.chars().nth(0).unwrap().is_lowercase()
}

#[test]
fn test_type_check_client_easy() {
    let mut know = Knowledge::standard();
    let scope = know.new_scope(&STDLIB);
    let foo_type = Type::client(Type::integer());
    let client = know.look_up(&scope, "Client").unwrap();
    let param = know.look_up(&scope, "Integer").unwrap();
    let mut foo_dict = know.dict(&client).clone();
    let param_dict = know.dict(&param).clone();
    foo_dict.r#type = foo_type;

    let foo = know.declare(&scope, "foo", foo_dict);
    let bar = know.declare(&scope, "bar", param_dict);

    assert!(know.type_check_send(&scope, &foo, &bar));
}

#[test]
fn test_type_check_generic() {
    let mut know = Knowledge::standard();
    let scope = know.new_scope(&STDLIB);
    let client_ref = know.look_up(&scope, "Client").unwrap();
    let client_dict = know.dict(&client_ref);
    let target_type = Type::app(
        client_dict.r#type.clone(),
        Type::ForAll {
            binders: vec!["a".to_string()],
            body: Box::new(Type::Qual {
                ctx: vec![Type::app(Type::named("Show"), Type::named("a"))],
                body: Box::new(Type::named("a")),
            }),
        },
    );

    let mut target_dict = client_dict.clone();
    target_dict.r#type = target_type;

    let integer = know.look_up(&scope, "Integer").unwrap();
    let integer_dict = know.dict(&integer).clone();

    let target = know.declare(&scope, "foo", target_dict);
    let param = know.declare(&scope, "bar", integer_dict);

    assert!(know.type_check_send(&scope, &target, &param));
}

#[test]
fn test_type_check_generic_complex() {
    let mut know = Knowledge::standard();
    let scope = know.new_scope(&STDLIB);
    let client_ref = know.look_up(&scope, "Client").unwrap();
    let client_dict = know.dict(&client_ref);
    let target_type = Type::app(
        client_dict.r#type.clone(),
        Type::record(vec![Prop::ano(Type::ForAll {
            binders: vec!["a".to_string()],
            body: Box::new(Type::Qual {
                ctx: vec![Type::app(Type::named("Show"), Type::named("a"))],
                body: Box::new(Type::named("a")),
            }),
        })]),
    );

    let mut target_dict = client_dict.clone();
    target_dict.r#type = target_type;

    let integer = know.look_up(&scope, "Integer").unwrap();
    let integer_dict = know.dict(&integer).clone();
    let param_dict = Dict::new(Type::record(vec![Prop::ano(integer_dict.r#type)]));

    let target = know.declare(&scope, "foo", target_dict);
    let param = know.declare(&scope, "bar", param_dict);

    assert!(know.type_check_send(&scope, &target, &param));
}
