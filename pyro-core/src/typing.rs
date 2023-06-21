use crate::ast::{Prop, Record};
use crate::context::{LocalScope, Scope};
use crate::sym::{Literal, TypeSym};
use crate::utils::generate_generic_type_name;
use crate::{Pos, STDLIB};
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};

#[derive(Clone)]
pub enum TypePointer {
    Ref(TypeRef),
    Rec(Record<TypePointer>),
    Fun(Box<TypePointer>, Box<TypePointer>),
    App(Box<TypePointer>, Box<TypePointer>),
}

impl TypePointer {
    pub fn app(a: TypePointer, b: TypePointer) -> Self {
        TypePointer::App(Box::new(a), Box::new(b))
    }
}

#[derive(Clone)]
pub struct TypeInfo {
    pub pos: Pos,
    pub scope: LocalScope,
    pub pointer: TypePointer,
}

#[derive(Clone, Eq, PartialEq, Debug)]
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

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::ForAll { binders, body } => {
                write!(f, "forall ")?;
                for bind in binders {
                    write!(f, "{} ", bind)?;
                }

                write!(f, ". ")?;
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

#[derive(Clone, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct TypeRef {
    scope: LocalScope,
    name: String,
}

#[derive(Clone, Default)]
struct ScopedTypes {
    name_gen: usize,
    ano_gen: usize,
    types: HashMap<String, Dict>,
    used: HashSet<String>,
}

impl ScopedTypes {
    fn insert(&mut self, name: impl AsRef<str>, dict: Dict) {
        self.types.insert(name.as_ref().to_string(), dict);
    }
}

#[derive(Clone)]
enum Either<A, B> {
    Left(A),
    Right(B),
}

#[derive(Clone)]
pub struct Knowledge {
    scope_gen: u32,
    inner: HashMap<u32, ScopedTypes>,
    dict_tables: HashMap<TypeRef, Either<TypePointer, Dict>>,
    default_rec_dict: Dict,
    default_fun_dict: Dict,
}

impl Knowledge {
    pub fn standard() -> Self {
        let mut this = Knowledge {
            scope_gen: 0,
            inner: Default::default(),
            dict_tables: Default::default(),
            default_rec_dict: Dict::new(Type::Rec {
                props: Record::default(),
            }),

            default_fun_dict: Dict::new(Type::Rec {
                props: Record::default(),
            }),
        };

        let types = this.inner.entry(STDLIB.id()).or_default();

        types.insert("Process", Dict::new(Type::process()));

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

    pub fn process_type_ref(&self) -> TypePointer {
        self.look_up(&STDLIB, "Process").unwrap()
    }

    pub fn client_type_ref(&self) -> TypePointer {
        self.look_up(&STDLIB, "Client").unwrap()
    }

    pub fn channel_type_ref(&self) -> TypePointer {
        self.look_up(&STDLIB, "Channel").unwrap()
    }

    pub fn integer_type_ref(&self) -> TypePointer {
        self.look_up(&STDLIB, "Integer").unwrap()
    }

    pub fn string_type_ref(&self) -> TypePointer {
        self.look_up(&STDLIB, "String").unwrap()
    }

    pub fn bool_type_ref(&self) -> TypePointer {
        self.look_up(&STDLIB, "Bool").unwrap()
    }

    pub fn char_type_ref(&self) -> TypePointer {
        self.look_up(&STDLIB, "Char").unwrap()
    }

    pub fn declare<S: Scope>(
        &mut self,
        scope: &S,
        name: impl AsRef<str>,
        target: Either<TypePointer, Dict>,
    ) -> TypePointer {
        // let types = self.inner.entry(scope.id()).or_default();
        let type_ref = TypeRef {
            scope: scope.as_local(),
            name: name.as_ref().to_string(),
        };

        self.dict_tables.insert(type_ref.clone(), target);
        // types.insert(name.as_ref().to_string(), dict);

        TypePointer::Ref(type_ref)
    }

    pub fn declare_from_ref<S: Scope>(
        &mut self,
        scope: &S,
        name: impl AsRef<str>,
        other: TypeRef,
    ) -> TypePointer {
        self.declare(scope, name, Either::Left(TypePointer::Ref(other)))
    }

    pub fn declare_from_pointer<S: Scope>(
        &mut self,
        scope: &S,
        name: impl AsRef<str>,
        other: TypePointer,
    ) -> TypePointer {
        self.declare(scope, name, Either::Left(other))
    }

    pub fn declare_from_dict<S: Scope>(
        &mut self,
        scope: &S,
        name: impl AsRef<str>,
        other: Dict,
    ) -> TypePointer {
        self.declare(scope, name, Either::Right(other))
    }

    pub fn declare_ano<S: Scope>(&mut self, scope: &S, dict: Dict) -> TypeRef {
        let types = self.inner.entry(scope.id()).or_default();
        let name = format!("@ano_{}", types.ano_gen);
        let type_ref = TypeRef {
            scope: scope.as_local(),
            name: name.clone(),
        };

        types.insert(name, dict);

        type_ref
    }

    pub fn dict_mut(&mut self, id: &TypeRef) -> &mut Dict {
        self.inner
            .entry(id.scope.id())
            .or_default()
            .types
            .get_mut(id.name.as_str())
            .unwrap()
    }

    pub fn dict(&self, id: &TypeRef) -> &Dict {
        self.inner
            .get(&id.scope.id())
            .unwrap()
            .types
            .get(id.name.as_str())
            .unwrap()
    }

    pub fn look_up<S: Scope>(&self, scope: &S, name: &str) -> Option<TypePointer> {
        let type_ref = TypeRef {
            scope: scope.as_local(),
            name: name.to_string(),
        };

        if self.dict_tables.contains_key(&type_ref) {
            return Some(TypePointer::Ref(type_ref));
        }

        None
        // for (height, scope_id) in scope.ancestors().iter().rev().enumerate() {
        //     if let Some(types) = self.inner.get(&scope_id) {
        //         if types.types.contains_key(name) {
        //             let mut ancestors = scope.ancestors().to_vec();
        //
        //             for _ in 0..height {
        //                 ancestors.pop();
        //             }
        //
        //             return Some(TypeRef {
        //                 scope: LocalScope { ancestors },
        //                 name: name.to_string(),
        //             });
        //         }
        //     }
        // }
        //
        // None
    }

    pub fn look_up_dict<S: Scope>(&self, scope: &S, name: &str) -> Option<&Dict> {
        let type_ref = self.look_up(scope, name)?;

        Some(self.dict(&type_ref))
    }

    pub fn look_up_type_dict<S: Scope>(&self, scope: &S, r#type: &Type) -> Option<&Dict> {
        match r#type {
            Type::Var { name } => self.look_up_dict(scope, name),
            Type::App { lhs, .. } => self.look_up_type_dict(scope, lhs.as_ref()),
            _ => None,
        }
    }

    pub fn look_up_type_dict_mut<S: Scope>(
        &mut self,
        scope: &S,
        r#type: &Type,
    ) -> Option<&mut Dict> {
        match r#type {
            Type::Var { name } => self.look_up_dict_mut(scope, name),
            Type::App { lhs, .. } => self.look_up_type_dict_mut(scope, lhs.as_ref()),
            _ => None,
        }
    }

    pub fn look_up_dict_mut<S: Scope>(&mut self, scope: &S, name: &str) -> Option<&mut Dict> {
        let type_ref = self.look_up(scope, name)?;

        Some(self.dict_mut(&type_ref))
    }

    pub fn new_generic<S: Scope>(&mut self, scope: &S) -> TypePointer {
        let types = self.inner.entry(scope.id()).or_default();
        let id = types.name_gen;
        let name = generate_generic_type_name(id);
        types.name_gen += 1;

        // types.insert(&name, Dict::new(Type::named(name.clone())));

        let type_ref = TypeRef {
            scope: scope.as_local(),
            name,
        };

        self.declare_from_dict(scope, &name, Dict::new(Type::named(name.clone())));

        TypePointer::Ref(type_ref)
    }

    pub fn project_type_pointer<S: Scope>(
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
                let c = self.project_type_pointer(scope, c.as_ref())?;
                let i = self.project_type_pointer(scope, i.as_ref())?;

                Ok(TypePointer::App(Box::new(c), Box::new(i)))
            }

            TypeSym::Rec(rec) => {
                let mut props = Vec::new();
                for prop in &rec.props {
                    props.push(Prop {
                        label: prop.label.clone(),
                        val: self.project_type_pointer(scope, &prop.val)?,
                    });
                }

                Ok(TypePointer::Rec(Record { props }))
            }

            TypeSym::Unknown => Ok(self.new_generic(scope)),
        }
    }

    pub fn project_type_pointer_dict(&self, pointer: &TypePointer) -> Dict {
        match pointer {
            TypePointer::Ref(r) => self.dict(r).clone(),

            TypePointer::Rec(rec) => {
                let mut props = Vec::new();

                for prop in &rec.props {
                    props.push(Prop {
                        label: prop.label.clone(),
                        val: self.project_type(&prop.val),
                    });
                }

                let mut dict = self.default_rec_dict.clone();
                dict.r#type = Type::Rec {
                    props: Record { props },
                };

                dict
            }

            TypePointer::Fun(p, r) => {
                let r#type =
                    Type::func(self.project_type(p.as_ref()), self.project_type(r.as_ref()));

                let mut dict = self.default_fun_dict.clone();
                dict.r#type = r#type;
                dict
            }

            TypePointer::App(c, i) => {
                let mut dict = self.project_type_pointer_dict(c.as_ref());
                let inner = self.project_type(i.as_ref());

                dict.r#type = Type::app(dict.r#type, inner);
                dict
            }
        }
    }

    pub fn project_type(&self, id: &TypePointer) -> Type {
        match id {
            TypePointer::Ref(r) => self.dict(r).r#type.clone(),

            TypePointer::Rec(rec) => {
                let mut props = Vec::new();

                for prop in &rec.props {
                    props.push(Prop {
                        label: prop.label.clone(),
                        val: self.project_type(&prop.val),
                    });
                }

                Type::Rec {
                    props: Record { props },
                }
            }

            TypePointer::Fun(lhs, rhs) => {
                let lhs = self.project_type(lhs.as_ref());
                let rhs = self.project_type(rhs.as_ref());

                Type::Fun {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
            }

            TypePointer::App(lhs, rhs) => {
                let lhs = self.project_type(lhs.as_ref());
                let rhs = self.project_type(rhs.as_ref());

                Type::App {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
            }
        }
    }

    pub fn implements<S: Scope>(&self, scope: &S, r#type: &Type, constraint: &str) -> bool {
        if let Some(dict) = self.look_up_type_dict(scope, r#type) {
            return dict.implements(constraint);
        }

        false
    }

    pub fn new_scope<S: Scope>(&mut self, parent: &S) -> LocalScope {
        self.scope_gen += 1;
        let id = self.scope_gen;
        let mut ancestors = parent.ancestors().to_vec();

        ancestors.push(id);

        LocalScope { ancestors }
    }

    pub fn param_matches<S: Scope>(&mut self, scope: &S, require: &Type, provided: &Type) -> bool {
        match require {
            Type::Var { name } => {
                let provided_dict = self.look_up_type_dict(scope, provided).cloned();
                let require_dict = self.look_up_dict_mut(scope, name).expect("to be defined");

                if require_dict.is_generic() {
                    let matches = if let Some(provided_dict) = provided_dict {
                        require_dict.impls.is_subset(&provided_dict.impls)
                    } else {
                        require_dict.impls.is_empty()
                    };

                    if matches {
                        require_dict.r#type = provided.clone();
                    }

                    matches
                } else {
                    require == provided
                }
            }

            Type::ForAll { binders, body } => {
                let new_scope = self.new_scope(scope);
                for binder in binders {
                    self.declare_from_dict(&new_scope, binder, Dict::new(Type::named(binder)));
                }

                self.param_matches(&new_scope, body.as_ref(), provided)
            }

            Type::Qual { ctx, body } => {
                for constraint in ctx {
                    if let Type::App { lhs, rhs } = constraint {
                        match (lhs.as_ref(), rhs.as_ref()) {
                            // TODO - If we want Haskell-like higher kinded types, we need to make that
                            // part more flexible. Right now we don't support universally quantified higher
                            // kinded types.
                            (Type::Var { name: constr }, Type::Var { .. }) => {
                                let var_dict = self
                                    .look_up_type_dict_mut(scope, rhs.as_ref())
                                    .expect("to be defined");

                                var_dict.add(constr);
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
                if let Type::Fun {
                    lhs: provided_lhs,
                    rhs: provided_rhs,
                } = &provided
                {
                    return self.param_matches(scope, require_lhs.as_ref(), provided_lhs.as_ref())
                        && self.param_matches(scope, require_rhs.as_ref(), provided_rhs.as_ref());
                }

                false
            }

            Type::App {
                lhs: require_lhs,
                rhs: require_rhs,
            } => {
                if let Type::App {
                    lhs: provided_lhs,
                    rhs: provided_rhs,
                } = &provided
                {
                    return self.param_matches(scope, require_lhs.as_ref(), provided_lhs.as_ref())
                        && self.param_matches(scope, require_rhs.as_ref(), provided_rhs.as_ref());
                }

                false
            }

            Type::Rec { props: require_rec } => {
                if let Type::Rec {
                    props: provided_rec,
                } = provided
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

                        if !self.param_matches(scope, &require_prop.val, &provided_prop.val) {
                            return false;
                        }
                    }

                    return true;
                }

                false
            }
        }
    }

    pub fn suggest_constraint(&mut self, pointer: &TypePointer, constraint: &str) {
        let mut current = pointer;

        loop {
            match current {
                TypePointer::Ref(p) => {
                    let dict = self.dict_mut(p);

                    if dict.is_generic() && !dict.implements(constraint) {
                        dict.add(constraint);
                    }

                    break;
                }

                TypePointer::App(constr, _) => {
                    current = constr.as_ref();
                }

                TypePointer::Rec(_) | TypePointer::Fun(_, _) => {
                    break;
                }
            }
        }
    }

    pub fn suggest_type<S: Scope>(
        &mut self,
        scope: &S,
        pointer: &TypePointer,
        r#type: &TypePointer,
    ) {
        let mut current = pointer;

        loop {
            match current {
                TypePointer::Ref(p) => {
                    let suggested_dict =
                        self.simplify_dict(scope, self.project_type_pointer_dict(r#type));
                    let dict = self.dict_mut(p);

                    if dict.is_generic() {
                        if suggested_dict.is_generic() {
                            dict.impls.extend(suggested_dict.impls);
                        } else {
                            *dict = suggested_dict;
                        }
                    }

                    break;
                }

                TypePointer::App(constr, _) => {
                    current = constr.as_ref();
                }

                TypePointer::Rec(_) | TypePointer::Fun(_, _) => {
                    break;
                }
            }
        }
    }

    pub fn suggest_inner_type<S: Scope>(
        &mut self,
        scope: &S,
        pointer: &TypePointer,
        r#type: &TypePointer,
    ) {
        loop {
            match pointer {
                TypePointer::Ref(p) => {
                    let suggested_dict =
                        self.simplify_dict(scope, self.project_type_pointer_dict(r#type));

                    let dict = self.dict_mut(p);

                    match dict.r#type.clone() {
                        Type::Var { .. } if dict.is_generic() => {
                            dict.r#type = Type::app(dict.r#type.clone(), suggested_dict.r#type);
                        }

                        Type::App { rhs, .. } => {
                            // Should be always defined if the code node was parsed from a file.
                            // If types were injected by the user manually, everything goes. It won't
                            // crash the inference system but might lose accuracy.
                            if let Some(inner_dict) =
                                self.look_up_type_dict_mut(&p.scope, rhs.as_ref())
                            {
                                if inner_dict.is_generic() {
                                    if suggested_dict.is_generic() {
                                        inner_dict.impls.extend(suggested_dict.impls);
                                    } else {
                                        *inner_dict = suggested_dict;
                                    }
                                }
                            }
                        }
                        _ => {}
                    }

                    break;
                }

                TypePointer::App(_, inner) => {
                    return self.suggest_type(scope, inner.as_ref(), r#type);
                }

                TypePointer::Rec(_) | TypePointer::Fun(_, _) => {
                    break;
                }
            }
        }
    }

    fn simplify_dict<S: Scope>(&self, scope: &S, dict: Dict) -> Dict {
        match dict.r#type.clone() {
            Type::Fun { rhs, .. } => self
                .look_up_type_dict(scope, rhs.as_ref())
                .cloned()
                .unwrap(),

            _ => dict,
        }
    }

    pub fn register_used_variable<S: Scope>(&mut self, scope: &S, name: &str) {
        let types = self.inner.entry(scope.id()).or_default();

        types.used.insert(name.to_string());
    }

    pub fn list_used_variables<S: Scope>(&self, scope: &S) -> HashSet<String> {
        let mut set = HashSet::new();

        for scope_id in scope.ancestors().iter().rev() {
            if let Some(types) = self.inner.get(scope_id) {
                set.extend(types.used.clone());
            }
        }

        set
    }

    pub fn type_check_send<S: Scope>(&mut self, scope: &S, target: &Type, params: &Type) -> bool {
        let inner_type = {
            let target_dict = self.look_up_type_dict_mut(scope, target);

            if !target_dict
                .map(|d| d.implements("Send"))
                .unwrap_or_default()
            {
                return false;
            }

            if let Type::App { rhs, .. } = target {
                rhs.as_ref()
            } else {
                unreachable!()
            }
        };

        self.param_matches(scope, inner_type, params)
    }

    pub fn type_check_receive<S: Scope>(
        &mut self,
        scope: &S,
        target: &Type,
        params: &Type,
    ) -> bool {
        let inner_type = {
            let target_dict = self.look_up_type_dict_mut(scope, target);

            if !target_dict
                .map(|d| d.implements("Receive"))
                .unwrap_or_default()
            {
                return false;
            }

            if let Type::App { rhs, .. } = target {
                rhs.as_ref()
            } else {
                unreachable!()
            }
        };

        self.param_matches(scope, inner_type, params)
    }
}

fn is_generic_name(name: &str) -> bool {
    name.chars().nth(0).unwrap().is_lowercase()
}

#[test]
fn test_type_check_client_easy() {
    let mut know = Knowledge::standard();
    let scope = know.new_scope(&STDLIB);

    assert!(know.type_check_send(&scope, &Type::client(Type::integer()), &Type::integer()));
}

#[test]
fn test_type_check_generic() {
    let mut know = Knowledge::standard();
    let target_type = Type::client(Type::ForAll {
        binders: vec!["a".to_string()],
        body: Box::new(Type::Qual {
            ctx: vec![Type::app(Type::named("Show"), Type::named("a"))],
            body: Box::new(Type::named("a")),
        }),
    });

    assert!(know.type_check_send(&STDLIB, &target_type, &Type::integer()));
}

#[test]
fn test_type_check_generic_complex() {
    let mut know = Knowledge::standard();
    let target_type = Type::client(Type::record(vec![Prop::ano(Type::ForAll {
        binders: vec!["a".to_string()],
        body: Box::new(Type::Qual {
            ctx: vec![Type::app(Type::named("Show"), Type::named("a"))],
            body: Box::new(Type::named("a")),
        }),
    })]));

    assert!(know.type_check_send(
        &STDLIB,
        &target_type,
        &Type::record(vec![Prop::ano(Type::integer())])
    ));
}
