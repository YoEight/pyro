use crate::ast::{Prop, Record};
use crate::context::{LocalScope, Scope};
use crate::sym::{Literal, TypeSym};
use crate::utils::generate_generic_type_name;
use crate::{Pos, STDLIB};
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};
use std::rc::Rc;

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
    pub fn app(a: TypePointer, b: TypePointer) -> Self {
        TypePointer::App(Box::new(a), Box::new(b))
    }

    pub fn fun(a: TypePointer, b: TypePointer) -> Self {
        TypePointer::Fun(Box::new(a), Box::new(b))
    }

    pub fn rec(props: Vec<Prop<TypePointer>>) -> Self {
        TypePointer::Rec(Record { props })
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

#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub struct TypeRef {
    pub scope: LocalScope,
    pub name: String,
}

type DictRef = Rc<RefCell<Dict>>;

#[derive(Clone, Default)]
struct ScopedTypes {
    name_gen: usize,
    symbols: HashMap<String, Either<TypePointer, DictRef>>,
    used: HashSet<String>,
}

#[derive(Clone)]
pub enum Either<A, B> {
    Left(A),
    Right(B),
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

#[derive(Clone)]
pub struct Knowledge {
    scope_gen: u32,
    dict_tables: HashMap<u32, ScopedTypes>,
    pub(crate) default_rec_dict: Dict,
    pub(crate) default_fun_dict: Dict,
    pub(crate) default_dict: Dict,
}

impl Knowledge {
    pub fn standard() -> Self {
        let mut this = Knowledge {
            scope_gen: 0,
            dict_tables: Default::default(),
            default_rec_dict: Dict::new(Type::Rec {
                props: Record::default(),
            }),

            default_fun_dict: Dict::new(Type::Rec {
                props: Record::default(),
            }),
            default_dict: Dict::new(Type::named("@@@@@@@")),
        };

        this.declare_from_dict(&STDLIB, "Show", Dict::new(Type::named("Show")));
        this.declare_from_dict(&STDLIB, "Process", Dict::new(Type::named("Process")));

        this.declare_from_dict(
            &STDLIB,
            "Bool",
            Dict::with_impls(Type::named("Bool"), vec!["Show".to_string()]),
        );

        this.declare_from_dict(
            &STDLIB,
            "Integer",
            Dict::with_impls(Type::named("Integer"), vec!["Show".to_string()]),
        );

        this.declare_from_dict(
            &STDLIB,
            "String",
            Dict::with_impls(Type::named("String"), vec!["Show".to_string()]),
        );

        this.declare_from_dict(
            &STDLIB,
            "Char",
            Dict::with_impls(Type::named("Char"), vec!["Show".to_string()]),
        );

        this.declare_from_dict(
            &STDLIB,
            "Client",
            Dict::with_impls(Type::named("Client"), vec!["Send".to_string()]),
        );

        this.declare_from_dict(
            &STDLIB,
            "Server",
            Dict::with_impls(Type::named("Server"), vec!["Receive".to_string()]),
        );

        this.declare_from_dict(
            &STDLIB,
            "Channel",
            Dict::with_impls(
                Type::named("Send"),
                vec!["Send".to_string(), "Receive".to_string()],
            ),
        );

        this
    }

    pub fn show_pointer(&self) -> TypePointer {
        self.look_up(&STDLIB, "Show").unwrap()
    }

    pub fn process_pointer(&self) -> TypePointer {
        self.look_up(&STDLIB, "Process").unwrap()
    }

    pub fn client_pointer(&self) -> TypePointer {
        self.look_up(&STDLIB, "Client").unwrap()
    }

    pub fn channel_pointer(&self) -> TypePointer {
        self.look_up(&STDLIB, "Channel").unwrap()
    }

    pub fn integer_pointer(&self) -> TypePointer {
        self.look_up(&STDLIB, "Integer").unwrap()
    }

    pub fn string_pointer(&self) -> TypePointer {
        self.look_up(&STDLIB, "String").unwrap()
    }

    pub fn bool_pointer(&self) -> TypePointer {
        self.look_up(&STDLIB, "Bool").unwrap()
    }

    pub fn char_pointer(&self) -> TypePointer {
        self.look_up(&STDLIB, "Char").unwrap()
    }

    pub fn type_builder(&mut self) -> TypeBuilder {
        TypeBuilder { know: self }
    }

    pub fn declare<S: Scope>(
        &mut self,
        scope: &S,
        name: impl AsRef<str>,
        target: Either<TypePointer, DictRef>,
    ) -> TypePointer {
        let type_ref = TypeRef {
            scope: scope.as_local(),
            name: name.as_ref().to_string(),
        };

        let types = self.dict_tables.entry(scope.id()).or_default();
        types.symbols.insert(name.as_ref().to_string(), target);

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
        self.declare(scope, name, Either::Right(Rc::new(RefCell::new(other))))
    }

    pub fn follow_link(&self, pointer: &TypePointer) -> TypePointer {
        let mut current = pointer;

        loop {
            let type_ref = match current {
                TypePointer::Ref(r) => r,
                _ => return current.clone(),
            };

            let types = self.dict_tables.get(&type_ref.scope.id()).unwrap();
            match types.symbols.get(&type_ref.name).unwrap() {
                Either::Left(next) => {
                    current = if let TypePointer::Ref(_) = next {
                        next
                    } else {
                        return next.clone();
                    };
                }

                Either::Right(_) => return TypePointer::Ref(type_ref.clone()),
            }
        }
    }

    pub fn implements(&self, id: &TypePointer, constraint: &str) -> bool {
        let mut current = id.clone();
        loop {
            match self.follow_link(&current) {
                TypePointer::Ref(type_ref) => {
                    let dict = self.dict(&type_ref).unwrap();
                    return dict.borrow().implements(constraint);
                }

                TypePointer::Rec(_) => return self.default_rec_dict.implements(constraint),
                TypePointer::Fun(_, _) => return self.default_fun_dict.implements(constraint),
                TypePointer::App(constr, _) => current = constr.as_ref().clone(),
                _ => return false,
            }
        }
    }

    pub fn dict(&self, id: &TypeRef) -> Option<DictRef> {
        let mut current = id;

        loop {
            let types = self.dict_tables.get(&current.scope.id()).unwrap();
            match types.symbols.get(&current.name).unwrap() {
                Either::Left(next) => {
                    let mut local = next;
                    current = loop {
                        match local {
                            TypePointer::Ref(next) => break next,
                            TypePointer::App(constr, _) => local = constr.as_ref(),
                            _ => return None,
                        }
                    };
                }

                Either::Right(dict) => return Some(dict.clone()),
            }
        }
    }

    pub fn look_up<S: Scope>(&self, scope: &S, name: &str) -> Option<TypePointer> {
        for (height, scope_id) in scope.ancestors().iter().rev().enumerate() {
            if let Some(types) = self.dict_tables.get(&scope_id) {
                if types.symbols.contains_key(name) {
                    let mut ancestors = scope.ancestors().to_vec();

                    for _ in 0..height {
                        ancestors.pop();
                    }

                    return Some(TypePointer::Ref(TypeRef {
                        scope: LocalScope { ancestors },
                        name: name.to_string(),
                    }));
                }
            }
        }

        None
    }

    pub fn unsafe_new_generic(&mut self, scope: &LocalScope, name: &str) {
        self.declare_from_dict(scope, &name, Dict::new(Type::named(name.clone())));
    }

    pub fn new_generic<S: Scope>(&mut self, scope: &S) -> TypePointer {
        self.new_generic_with_constraints(scope, vec![])
    }

    pub fn new_generic_with_constraints<S: Scope, I>(&mut self, scope: &S, impls: I) -> TypePointer
    where
        I: IntoIterator<Item = String>,
    {
        let types = self.dict_tables.entry(scope.id()).or_default();
        let id = types.name_gen;
        let name = format!("'{}", generate_generic_type_name(id));
        types.name_gen += 1;

        // types.insert(&name, Dict::new(Type::named(name.clone())));

        self.declare_from_dict(
            scope,
            &name,
            Dict::with_impls(Type::named(name.clone()), impls),
        )
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

    pub fn project_type(&self, id: &TypePointer) -> Dict {
        match self.follow_link(id) {
            TypePointer::Ref(r) => self.dict(&r).unwrap().borrow().clone(),

            TypePointer::Rec(rec) => {
                let mut props = Vec::new();

                for prop in &rec.props {
                    props.push(Prop {
                        label: prop.label.clone(),
                        val: self.project_type(&prop.val).r#type,
                    });
                }

                let mut dict = self.default_rec_dict.clone();
                dict.r#type = Type::Rec {
                    props: Record { props },
                };

                dict
            }

            TypePointer::Fun(lhs, rhs) => {
                let lhs = self.project_type(lhs.as_ref());
                let rhs = self.project_type(rhs.as_ref());
                let mut dict = self.default_fun_dict.clone();

                dict.r#type = Type::Fun {
                    lhs: Box::new(lhs.r#type),
                    rhs: Box::new(rhs.r#type),
                };

                dict
            }

            TypePointer::App(lhs, rhs) => {
                let mut lhs = self.project_type(lhs.as_ref());
                let rhs = self.project_type(rhs.as_ref());

                lhs.r#type = Type::App {
                    lhs: Box::new(lhs.r#type.clone()),
                    rhs: Box::new(rhs.r#type),
                };

                lhs
            }

            TypePointer::ForAll(explicit, _, binders, body) => {
                let r#type = Type::ForAll {
                    explicit,
                    binders,
                    body: Box::new(self.project_type(body.as_ref()).r#type),
                };

                let mut dict = self.default_dict.clone();
                dict.r#type = r#type;
                dict
            }

            TypePointer::Qual(ctx, body) => {
                let ctx = ctx
                    .iter()
                    .map(|t| self.project_type(t).r#type)
                    .collect::<Vec<_>>();

                let r#type = Type::Qual {
                    ctx,
                    body: Box::new(self.project_type(body.as_ref()).r#type),
                };

                let mut dict = self.default_dict.clone();
                dict.r#type = r#type;
                dict
            }
        }
    }

    pub fn new_scope<S: Scope>(&mut self, parent: &S) -> LocalScope {
        self.scope_gen += 1;
        let id = self.scope_gen;
        let mut ancestors = parent.ancestors().to_vec();

        ancestors.push(id);

        LocalScope { ancestors }
    }

    pub fn param_matches(&mut self, require: &TypePointer, provided: &TypePointer) -> bool {
        match require {
            TypePointer::Ref(type_ref) => {
                let require_dict = self.project_type(require);
                let provided_dict = self.project_type(provided);

                if require_dict.is_generic() {
                    if provided_dict.is_generic() {
                        let require_ref = self.dict(type_ref).unwrap();
                        let mut require_ref = require_ref.borrow_mut();
                        let provided_ref = self.dict(provided.as_type_ref()).unwrap();
                        let mut provided_ref = provided_ref.borrow_mut();

                        require_ref.impls.extend(provided_ref.impls.clone());
                        provided_ref.impls.extend(require_ref.impls.clone());

                        true
                    } else {
                        if require_dict.impls.is_subset(&provided_dict.impls) {
                            self.declare_from_pointer(
                                &type_ref.scope,
                                &type_ref.name,
                                provided.clone(),
                            );

                            true
                        } else {
                            false
                        }
                    }
                } else {
                    require_dict.r#type == provided_dict.r#type
                }
            }

            TypePointer::ForAll(_, scope, binders, body) => {
                for binder in binders {
                    self.unsafe_new_generic(scope, binder.as_str());
                }

                self.param_matches(body.as_ref(), provided)
            }

            TypePointer::Qual(ctx, body) => {
                for constraint in ctx {
                    if let TypePointer::App(lhs, rhs) = constraint {
                        let project_lhs = self.project_type(lhs.as_ref());
                        // TODO - If we want Haskell-like higher kinded types, we need to make that
                        // part more flexible. Right now we don't support universally quantified higher
                        // kinded types.
                        if let Type::Var { name } = &project_lhs.r#type {
                            self.suggest_constraint(rhs.as_ref(), name);
                        }
                    }
                }

                self.param_matches(body.as_ref(), provided)
            }

            TypePointer::Fun(require_lhs, require_rhs) => {
                if let TypePointer::Fun(provided_lhs, provided_rhs) = self.follow_link(&provided) {
                    return self.param_matches(require_lhs.as_ref(), provided_lhs.as_ref())
                        && self.param_matches(require_rhs.as_ref(), provided_rhs.as_ref());
                }

                false
            }

            TypePointer::App(require_lhs, require_rhs) => {
                if let TypePointer::App(provided_lhs, provided_rhs) = self.follow_link(&provided) {
                    return self.param_matches(require_lhs.as_ref(), provided_lhs.as_ref())
                        && self.param_matches(require_rhs.as_ref(), provided_rhs.as_ref());
                }

                false
            }

            TypePointer::Rec(require_rec) => {
                if let TypePointer::Rec(provided_rec) = self.follow_link(&provided) {
                    if require_rec.props.len() != provided_rec.props.len() {
                        return false;
                    }

                    for (require_prop, provided_prop) in
                        require_rec.props.iter().zip(provided_rec.props.iter())
                    {
                        if require_prop.label != provided_prop.label {
                            return false;
                        }

                        if !self.param_matches(&require_prop.val, &provided_prop.val) {
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
                    if let Some(dict) = self.dict(&p) {
                        let mut dict = dict.borrow_mut();
                        if dict.is_generic() && !dict.implements(constraint) {
                            dict.add(constraint);
                        }
                    }

                    break;
                }

                TypePointer::App(constr, _) => {
                    current = constr.as_ref();
                }

                _ => {
                    break;
                }
            }
        }
    }

    pub fn suggest_type(&mut self, pointer: &TypePointer, r#type: &TypePointer) {
        let mut current = pointer;

        loop {
            match current {
                TypePointer::Ref(p) => {
                    let suggested_dict = self.simplify_dict(r#type);
                    if let Some(dict) = self.dict(p) {
                        let mut dict = dict.borrow_mut();
                        if dict.is_generic() {
                            if suggested_dict.is_generic() {
                                dict.impls.extend(suggested_dict.impls);
                            } else {
                                self.dict_tables
                                    .entry(p.scope.id())
                                    .or_default()
                                    .symbols
                                    .insert(p.name.clone(), Either::Left(r#type.clone()));
                            }
                        }
                    }

                    break;
                }

                TypePointer::App(constr, _) => {
                    current = constr.as_ref();
                }

                _ => {
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
        match pointer {
            TypePointer::Ref(p) => {
                let dict = self.dict(p).unwrap();
                let dict = dict.borrow();

                if dict.is_generic() {
                    let var = self.new_generic_with_constraints(scope, dict.impls.clone());
                    self.dict_tables
                        .entry(p.scope.id())
                        .or_default()
                        .symbols
                        .insert(
                            p.name.clone(),
                            Either::Left(TypePointer::app(var, r#type.clone())),
                        );
                }
            }

            TypePointer::App(_, inner) => {
                return self.suggest_type(inner.as_ref(), r#type);
            }

            _ => {}
        }
    }

    fn simplify_dict(&self, r#type: &TypePointer) -> Dict {
        match r#type {
            TypePointer::Fun(_, rhs) => self.project_type(rhs),
            _ => self.project_type(r#type),
        }
    }

    pub fn register_used_variable<S: Scope>(&mut self, scope: &S, name: &str) {
        let types = self.dict_tables.entry(scope.id()).or_default();

        types.used.insert(name.to_string());
    }

    pub fn build_used_variables_table(&self) -> UsedVariables {
        let mut table = UsedVariables::default();

        for (scope, types) in self.dict_tables.iter() {
            table.inner.insert(*scope, types.used.clone());
        }

        table
    }
}

pub trait PyroType {
    fn r#type(builder: &TypeBuilder) -> eyre::Result<TypePointer>
    where
        Self: Sized;
}

impl PyroType for i64 {
    fn r#type(builder: &TypeBuilder) -> eyre::Result<TypePointer> {
        builder.look_up("Integer")
    }
}

impl PyroType for char {
    fn r#type(builder: &TypeBuilder) -> eyre::Result<TypePointer> {
        builder.look_up("Char")
    }
}

impl PyroType for bool {
    fn r#type(builder: &TypeBuilder) -> eyre::Result<TypePointer> {
        builder.look_up("Bool")
    }
}

impl PyroType for String {
    fn r#type(builder: &TypeBuilder) -> eyre::Result<TypePointer> {
        builder.look_up("String")
    }
}

pub struct TypeBuilder<'a> {
    know: &'a mut Knowledge,
}

impl<'a> TypeBuilder<'a> {
    pub fn new(know: &'a mut Knowledge) -> TypeBuilder<'a> {
        Self { know }
    }

    pub fn look_up(&self, name: impl AsRef<str>) -> eyre::Result<TypePointer> {
        if let Some(pointer) = self.know.look_up(&STDLIB, name.as_ref()) {
            return Ok(pointer.clone());
        }

        eyre::bail!("Type {} doesn't exist", name.as_ref())
    }

    pub fn for_all(self, var_name: impl AsRef<str>) -> ForAllVarBuilder<'a> {
        let binders = vec![var_name.as_ref().to_string()];
        let scope = self.know.new_scope(&STDLIB);
        let var_dict = Dict::new(Type::named(var_name.as_ref()));
        let var = self
            .know
            .declare_from_dict(&scope, var_name.as_ref(), var_dict);
        let mut vars = HashMap::new();

        vars.insert(var_name.as_ref().to_string(), var.clone());

        let for_all = ForAllBuilder {
            inner: self,
            scope,
            binders,
            constraints: vec![],
            vars,
        };

        ForAllVarBuilder {
            var,
            inner: for_all,
        }
    }

    pub fn named(self, name: impl AsRef<str>) -> eyre::Result<TypePointer> {
        if let Some(pointer) = self.know.look_up(&STDLIB, name.as_ref()) {
            return Ok(pointer.clone());
        }

        eyre::bail!("Type '{}' doesn't exist", name.as_ref())
    }

    pub fn of<T: PyroType>(&self) -> eyre::Result<TypePointer> {
        T::r#type(self)
    }

    pub fn func_of<T: PyroType>(self) -> eyre::Result<FuncBuilder<'a>>
    where
        T: Sized,
    {
        let param = T::r#type(&self)?;

        Ok(FuncBuilder {
            apps: vec![param],
            inner: self,
        })
    }

    pub fn func_type(self, name: impl AsRef<str>) -> eyre::Result<FuncBuilder<'a>> {
        let param = self.look_up(name)?;

        Ok(FuncBuilder {
            apps: vec![param],
            inner: self,
        })
    }

    pub fn type_constructor(self, name: impl AsRef<str>) -> eyre::Result<TypeConstrBuilder<'a>> {
        let constr = self.look_up(name.as_ref())?;

        Ok(TypeConstrBuilder {
            constr,
            inner: self,
        })
    }
}

pub struct FuncBuilder<'a> {
    apps: Vec<TypePointer>,
    inner: TypeBuilder<'a>,
}

impl<'a> FuncBuilder<'a> {
    pub fn param_of<T: PyroType>(mut self) -> eyre::Result<Self>
    where
        T: Sized,
    {
        self.apps.push(T::r#type(&self.inner)?);
        Ok(self)
    }

    pub fn param(mut self, name: impl AsRef<str>) -> eyre::Result<Self> {
        self.apps.push(self.inner.look_up(name)?);
        Ok(self)
    }

    pub fn result(mut self, name: impl AsRef<str>) -> eyre::Result<TypePointer> {
        let mut acc = self.inner.look_up(name)?;

        while let Some(param) = self.apps.pop() {
            acc = TypePointer::fun(param, acc);
        }

        Ok(acc)
    }

    pub fn result_of<T: PyroType>(mut self) -> eyre::Result<TypePointer>
    where
        T: Sized,
    {
        let mut acc = T::r#type(&self.inner)?;

        while let Some(param) = self.apps.pop() {
            acc = TypePointer::fun(param, acc);
        }

        Ok(acc)
    }
}

pub struct TypeConstrBuilder<'a> {
    constr: TypePointer,
    inner: TypeBuilder<'a>,
}

impl<'a> TypeConstrBuilder<'a> {
    pub fn inner_type_of<T: PyroType>(self) -> eyre::Result<TypePointer>
    where
        T: Sized,
    {
        let inner_type = T::r#type(&self.inner)?;

        Ok(TypePointer::app(self.constr, inner_type))
    }

    pub fn inner_type_name(self, name: impl AsRef<str>) -> eyre::Result<TypePointer> {
        let inner_type = self.inner.look_up(name)?;
        Ok(TypePointer::app(self.constr, inner_type))
    }
}

pub struct ForAllBuilder<'a> {
    inner: TypeBuilder<'a>,
    scope: LocalScope,
    binders: Vec<String>,
    constraints: Vec<TypePointer>,
    vars: HashMap<String, TypePointer>,
}

impl<'a> ForAllBuilder<'a> {
    pub fn add_var(mut self, var_name: impl AsRef<str>) -> ForAllVarBuilder<'a> {
        let scope = self.scope.clone();
        let var_dict = Dict::new(Type::named(var_name.as_ref()));
        let var = self
            .inner
            .know
            .declare_from_dict(&scope, var_name.as_ref(), var_dict);

        self.binders.push(var_name.as_ref().to_string());
        self.vars.insert(var_name.as_ref().to_string(), var.clone());

        ForAllVarBuilder { var, inner: self }
    }

    pub fn body(self, var_name: impl AsRef<str>) -> eyre::Result<TypePointer> {
        if let Some(pointer) = self.vars.get(var_name.as_ref()) {
            let body = if self.constraints.is_empty() {
                pointer.clone()
            } else {
                TypePointer::Qual(self.constraints, Box::new(pointer.clone()))
            };

            return Ok(TypePointer::ForAll(
                false,
                self.scope,
                self.binders,
                Box::new(body),
            ));
        }

        eyre::bail!(
            "Type variable {} doesn't exist in this forall type expression",
            var_name.as_ref()
        )
    }
}

pub struct ForAllVarBuilder<'a> {
    var: TypePointer,
    inner: ForAllBuilder<'a>,
}

impl<'a> ForAllVarBuilder<'a> {
    pub fn done(self) -> ForAllBuilder<'a> {
        self.inner
    }

    pub fn add_constraint(mut self, constraint: impl AsRef<str>) -> eyre::Result<Self> {
        if let Some(constr) = self.inner.inner.know.look_up(&STDLIB, constraint.as_ref()) {
            self.inner
                .constraints
                .push(TypePointer::app(constr, self.var.clone()));

            return Ok(self);
        }

        eyre::bail!("Constraint '{}' doesn't exist", constraint.as_ref())
    }
}

fn is_generic_name(name: &str) -> bool {
    name.chars().nth(0).unwrap() == '\''
}

#[cfg(test)]
fn type_check_send(know: &mut Knowledge, require: &TypePointer, provided: &TypePointer) -> bool {
    let dict = know.project_type(require);
    if !dict.implements("Send") {
        return false;
    }

    if let TypePointer::App(_, inner) = &require {
        know.param_matches(inner.as_ref(), provided)
    } else {
        false
    }
}

#[test]
fn test_type_check_client_easy() {
    let mut know = Knowledge::standard();
    let client = know.client_pointer();
    let integer = know.integer_pointer();

    assert!(type_check_send(
        &mut know,
        &TypePointer::app(client, integer.clone()),
        &integer
    ));
}

#[test]
fn test_type_check_generic() {
    let mut know = Knowledge::standard();
    let scope = know.new_scope(&STDLIB);
    let var = know.declare_from_dict(&scope, "'a", Dict::new(Type::named("'a")));
    let target_type = TypePointer::app(
        know.client_pointer(),
        TypePointer::ForAll(
            false,
            scope,
            vec!["'a".to_string()],
            Box::new(TypePointer::Qual(
                vec![TypePointer::app(know.show_pointer(), var.clone())],
                Box::new(var),
            )),
        ),
    );

    let integer = know.integer_pointer();
    assert!(type_check_send(&mut know, &target_type, &integer));
}

#[test]
fn test_type_check_generic_complex() {
    let mut know = Knowledge::standard();
    let scope = know.new_scope(&STDLIB);
    let var = know.declare_from_dict(&scope, "'a", Dict::new(Type::named("'a")));
    let target_type = TypePointer::app(
        know.client_pointer(),
        TypePointer::rec(vec![Prop::ano(TypePointer::ForAll(
            false,
            scope,
            vec!["'a".to_string()],
            Box::new(TypePointer::Qual(
                vec![TypePointer::app(know.show_pointer(), var.clone())],
                Box::new(var),
            )),
        ))]),
    );

    let integer = know.integer_pointer();
    assert!(type_check_send(
        &mut know,
        &target_type,
        &TypePointer::rec(vec![Prop::ano(integer)])
    ));
}
