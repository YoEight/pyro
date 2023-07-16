use crate::ast::{Prop, Record};
use crate::typing::{Event, Machine, TypeCreated, TypeStatus, Types};
use crate::utils::generate_generic_type_name;
use crate::{LocalScope, Scope, Type, TypePointer, TypeRef, STDLIB};
use std::collections::{BTreeMap, HashMap, HashSet};

#[derive(Clone)]
struct Dict {
    impls: HashSet<String>,
    name: String,
    status: TypeStatus,
}

impl Dict {
    fn is_generic(&self) -> bool {
        self.status == TypeStatus::Generic
    }

    fn implements(&self, constaint: &str) -> bool {
        self.impls.contains(constaint)
    }

    fn add_constraint(&mut self, constraint: String) {
        self.impls.insert(constraint);
    }

    fn rec() -> Self {
        Self {
            impls: HashSet::from_iter(vec!["Show".to_string()]),
            name: "@Record".to_string(),
            status: TypeStatus::Defined,
        }
    }

    fn function() -> Self {
        Self {
            impls: Default::default(),
            name: "@Function".to_string(),
            status: TypeStatus::Defined,
        }
    }

    fn for_all() -> Self {
        Self {
            impls: Default::default(),
            name: "@ForAll".to_string(),
            status: TypeStatus::Defined,
        }
    }

    fn qualification() -> Self {
        Self {
            impls: Default::default(),
            name: "@Qualification".to_string(),
            status: TypeStatus::Defined,
        }
    }
}

#[derive(Clone)]
struct Symbols {
    infer_id: usize,
    scope: LocalScope,
    inner: HashMap<String, Dict>,
    links: HashMap<String, TypePointer>,
}

impl Symbols {
    fn new(scope: LocalScope) -> Self {
        Self {
            infer_id: 0,
            scope,
            inner: Default::default(),
            links: Default::default(),
        }
    }
}

#[derive(Clone)]
pub struct NominalTyping {
    scoped_symbols: BTreeMap<u32, Symbols>,
    scope: LocalScope,
}

impl Default for NominalTyping {
    fn default() -> Self {
        let mut scoped_symbols = BTreeMap::new();

        let symbols = scoped_symbols
            .entry(STDLIB.id())
            .or_insert_with(|| Symbols::new(STDLIB.as_local_scope()));

        symbols.inner.insert(
            "Show".to_string(),
            Dict {
                impls: Default::default(),
                name: "Show".to_string(),
                status: TypeStatus::Defined,
            },
        );

        symbols.inner.insert(
            "Send".to_string(),
            Dict {
                impls: Default::default(),
                name: "Send".to_string(),
                status: TypeStatus::Defined,
            },
        );

        symbols.inner.insert(
            "Receive".to_string(),
            Dict {
                impls: Default::default(),
                name: "Receive".to_string(),
                status: TypeStatus::Defined,
            },
        );

        symbols.inner.insert(
            "Bool".to_string(),
            Dict {
                impls: HashSet::from_iter(vec!["Show".to_string()]),
                name: "Bool".to_string(),
                status: TypeStatus::Defined,
            },
        );

        symbols.inner.insert(
            "Integer".to_string(),
            Dict {
                impls: HashSet::from_iter(vec!["Show".to_string()]),
                name: "Integer".to_string(),
                status: TypeStatus::Defined,
            },
        );

        symbols.inner.insert(
            "String".to_string(),
            Dict {
                impls: HashSet::from_iter(vec!["Show".to_string()]),
                name: "String".to_string(),
                status: TypeStatus::Defined,
            },
        );

        symbols.inner.insert(
            "Char".to_string(),
            Dict {
                impls: HashSet::from_iter(vec!["Show".to_string()]),
                name: "Char".to_string(),
                status: TypeStatus::Defined,
            },
        );

        symbols.inner.insert(
            "Client".to_string(),
            Dict {
                impls: HashSet::from_iter(vec!["Send".to_string()]),
                name: "Client".to_string(),
                status: TypeStatus::Defined,
            },
        );

        symbols.inner.insert(
            "Server".to_string(),
            Dict {
                impls: HashSet::from_iter(vec!["Receive".to_string()]),
                name: "Server".to_string(),
                status: TypeStatus::Defined,
            },
        );

        symbols.inner.insert(
            "Channel".to_string(),
            Dict {
                impls: HashSet::from_iter(vec!["Receive".to_string(), "Send".to_string()]),
                name: "Channel".to_string(),
                status: TypeStatus::Defined,
            },
        );

        Self {
            scoped_symbols,
            scope: STDLIB.as_local_scope(),
        }
    }
}

impl NominalTyping {
    fn dict<'a>(&'a self, location: &'a TypePointer) -> Dict {
        let location = self.follow_link(location);

        match &location {
            TypePointer::Ref(type_ref) => self
                .scoped_symbols
                .get(&type_ref.scope.id())
                .unwrap()
                .inner
                .get(type_ref.name.as_str())
                .cloned()
                .unwrap(),

            TypePointer::Rec(_) => {
                // TODO - Generates on-the-fly type classes as a proof that the record has certain
                // property names.
                Dict::rec()
            }

            TypePointer::Fun(_, _) => Dict::function(),
            TypePointer::App(constr, _) => self.dict(constr.as_ref()),
            TypePointer::ForAll(_, _, _, _) => Dict::for_all(),
            TypePointer::Qual(_, _) => Dict::qualification(),
        }
    }
}

impl Types for NominalTyping {
    fn project(&self, target: &TypePointer) -> Type {
        let target = self.follow_link(target);

        match target {
            TypePointer::Ref(_) => Type::named(self.dict(&target).name.as_str()),

            TypePointer::Rec(rec) => {
                let mut props = Vec::new();

                for prop in &rec.props {
                    props.push(Prop {
                        label: prop.label.clone(),
                        val: self.project(&prop.val),
                    });
                }

                Type::Rec {
                    props: Record { props },
                }
            }

            TypePointer::Fun(lhs, rhs) => {
                let lhs = self.project(lhs.as_ref());
                let rhs = self.project(rhs.as_ref());

                Type::Fun {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
            }

            TypePointer::App(lhs, rhs) => {
                let lhs = self.project(lhs.as_ref());
                let rhs = self.project(rhs.as_ref());

                Type::App {
                    lhs: Box::new(lhs),
                    rhs: Box::new(rhs),
                }
            }

            // No need to do too much as so far most of universally quantified type are simple.
            TypePointer::ForAll(_, _, binders, body) => Type::ForAll {
                explicit: false,
                binders: binders.clone(),
                body: Box::new(self.project(body.as_ref())),
            },

            TypePointer::Qual(constraints, body) => {
                let mut type_constraints = Vec::new();

                for constraint in constraints {
                    type_constraints.push(self.project(&constraint));
                }

                Type::Qual {
                    ctx: type_constraints,
                    body: Box::new(self.project(body.as_ref())),
                }
            }
        }
    }

    fn type_check(&self, require: &TypePointer, provided: &TypePointer) -> bool {
        let require = self.follow_link(require);
        let provided = self.follow_link(provided);

        if let TypePointer::ForAll(_, _, binders, body) = provided {
            let mut local = self.clone();
            local.apply(Event::PushScope);

            for binder in binders {
                local.apply(Event::TypeCreated(TypeCreated::Defined {
                    name: binder.clone(),
                    generic: true,
                    constraints: vec![],
                }));
            }

            if let TypePointer::Qual(ctx, body) = body.as_ref() {
                for constraint in ctx {
                    if let TypePointer::App(lhs, rhs) = constraint {
                        // TODO - If we want Haskell-like higher kinded types, we need to make that
                        // part more flexible. Right now we don't support universally quantified higher
                        // kinded types.
                        if let TypePointer::Ref(target) = rhs.as_ref() {
                            let constraint = local.dict(lhs.as_ref());
                            let dict = local
                                .scoped_symbols
                                .get_mut(&target.scope.id())
                                .unwrap()
                                .inner
                                .get_mut(target.name.as_str())
                                .unwrap();

                            if dict.status == TypeStatus::Generic {
                                dict.impls.insert(constraint.name);
                            }
                        }
                    }
                }

                return local.type_check(&require, body.as_ref());
            }

            return local.type_check(&require, body.as_ref());
        }

        match &require {
            TypePointer::Ref(_) => {
                let require_dict = self.dict(&require);
                let provided_dict = self.dict(&provided);

                match require_dict.status {
                    TypeStatus::Generic => provided_dict.impls.is_superset(&require_dict.impls),
                    TypeStatus::NeedInference => unreachable!(),
                    TypeStatus::Defined => {
                        if provided_dict.status == TypeStatus::NeedInference {
                            require_dict.impls.is_superset(&provided_dict.impls)
                        } else {
                            // TODO - Comparing just name is probably a bad idea.
                            !provided_dict.is_generic() && require_dict.name == provided_dict.name
                        }
                    }
                }
            }

            TypePointer::ForAll(_, _, binders, body) => {
                let mut local = self.clone();
                local.apply(Event::PushScope);

                for binder in binders {
                    local.apply(Event::TypeCreated(TypeCreated::Defined {
                        name: binder.clone(),
                        generic: true,
                        constraints: vec![],
                    }));
                }

                local.type_check(body.as_ref(), &provided)
            }

            TypePointer::Qual(ctx, body) => {
                let mut local = self.clone();
                for constraint in ctx {
                    if let TypePointer::App(lhs, rhs) = constraint {
                        // TODO - If we want Haskell-like higher kinded types, we need to make that
                        // part more flexible. Right now we don't support universally quantified higher
                        // kinded types.
                        if let TypePointer::Ref(target) = rhs.as_ref() {
                            let constraint = local.dict(lhs.as_ref());
                            let dict = local
                                .scoped_symbols
                                .get_mut(&target.scope.id())
                                .unwrap()
                                .inner
                                .get_mut(target.name.as_str())
                                .unwrap();

                            if dict.status == TypeStatus::Generic {
                                dict.impls.insert(constraint.name);
                            }
                        }
                    }
                }

                local.type_check(body.as_ref(), &provided)
            }

            TypePointer::Fun(require_lhs, require_rhs) => {
                if let TypePointer::Fun(provided_lhs, provided_rhs) = self.follow_link(&provided) {
                    return self.type_check(require_lhs.as_ref(), provided_lhs.as_ref())
                        && self.type_check(require_rhs.as_ref(), provided_rhs.as_ref());
                }

                false
            }

            TypePointer::App(require_lhs, require_rhs) => {
                if let TypePointer::App(provided_lhs, provided_rhs) = self.follow_link(&provided) {
                    return self.type_check(require_lhs.as_ref(), provided_lhs.as_ref())
                        && self.type_check(require_rhs.as_ref(), provided_rhs.as_ref());
                }

                false
            }

            TypePointer::Rec(require_rec) => match provided {
                _ if provided.is_type_ref() => {
                    let require_dict = self.dict(&require);
                    let provided_dict = self.dict(&provided);

                    if !provided_dict.is_generic() {
                        return false;
                    }

                    require_dict.impls.is_superset(&provided_dict.impls)
                }

                TypePointer::Rec(provided_rec) => {
                    if require_rec.props.len() != provided_rec.props.len() {
                        return false;
                    }

                    for (require_prop, provided_prop) in
                        require_rec.props.iter().zip(provided_rec.props.iter())
                    {
                        if require_prop.label != provided_prop.label {
                            return false;
                        }

                        if !self.type_check(&require_prop.val, &provided_prop.val) {
                            return false;
                        }
                    }

                    true
                }

                _ => false,
            },
        }
    }

    fn look_up<S: Scope>(&self, scope: &S, name: &str) -> Option<TypePointer> {
        for scope_id in scope.ancestors().iter().rev() {
            if let Some(symbols) = self.scoped_symbols.get(scope_id) {
                if symbols.inner.contains_key(name) || symbols.links.contains_key(name) {
                    return Some(TypePointer::Ref(TypeRef {
                        scope: symbols.scope.clone(),
                        name: name.to_string(),
                    }));
                }
            }
        }

        None
    }

    fn current_scope(&self) -> LocalScope {
        self.scope.clone()
    }

    fn current_available_name(&self) -> TypeRef {
        let name_id = self.scoped_symbols.get(&self.scope.id()).unwrap().infer_id;
        let name = format!("@{}", generate_generic_type_name(name_id));

        TypeRef {
            scope: self.scope.clone(),
            name,
        }
    }

    fn as_function<'a>(
        &'a self,
        mut r#type: &'a TypePointer,
    ) -> Option<(&'a TypePointer, &'a TypePointer)> {
        loop {
            if let TypePointer::Fun(param, result) = r#type {
                return Some((param.as_ref(), result.as_ref()));
            }

            if let TypePointer::Ref(type_ref) = r#type {
                let symbols = self.scoped_symbols.get(&type_ref.scope.id())?;
                r#type = symbols.links.get(type_ref.name.as_str())?;
                continue;
            }

            return None;
        }
    }

    fn as_type_constructor<'a>(
        &'a self,
        mut r#type: &'a TypePointer,
    ) -> Option<(&'a TypePointer, &'a TypePointer)> {
        loop {
            if let TypePointer::App(constr, inner) = r#type {
                return Some((constr.as_ref(), inner.as_ref()));
            }

            if let TypePointer::Ref(type_ref) = r#type {
                let symbols = self.scoped_symbols.get(&type_ref.scope.id())?;
                r#type = symbols.links.get(type_ref.name.as_str())?;
                continue;
            }

            return None;
        }
    }

    fn type_implements(&self, r#type: &TypePointer, constraint: &str) -> bool {
        let dict = self.dict(r#type);

        dict.implements(constraint)
    }
}

impl Machine for NominalTyping {
    type Model = NominalTyping;

    fn apply(&mut self, event: Event) {
        match event {
            Event::PopScope => {
                self.scope.ancestors.pop();
            }

            Event::PushScope => {
                self.scope.ancestors.push(self.scoped_symbols.len() as u32);

                self.scoped_symbols
                    .insert(self.scope.id(), Symbols::new(self.scope.clone()));
            }

            Event::SetScope(scope) => {
                self.scope = scope;
            }

            Event::VariableIntroduced { type_ref, location } => {
                // simplify links and detect potential cycles
                let location = self.follow_link(&location);
                let symbols = self
                    .scoped_symbols
                    .entry(type_ref.scope.id())
                    .or_insert_with(|| Symbols::new(self.scope.clone()));

                symbols.links.insert(type_ref.name.clone(), location);
            }

            Event::TypeCreated(params) => {
                let symbols = self
                    .scoped_symbols
                    .entry(self.scope.id())
                    .or_insert_with(|| Symbols::new(self.scope.clone()));

                match params {
                    TypeCreated::Defined {
                        name,
                        constraints,
                        generic,
                    } => {
                        let status = if generic {
                            TypeStatus::Generic
                        } else {
                            TypeStatus::Defined
                        };

                        symbols.inner.insert(
                            name.clone(),
                            Dict {
                                impls: HashSet::from_iter(constraints),
                                name,
                                status,
                            },
                        );
                    }

                    TypeCreated::Inferred => {
                        let name = format!("@{}", generate_generic_type_name(symbols.infer_id));
                        symbols.infer_id += 1;

                        symbols.inner.insert(
                            name.clone(),
                            Dict {
                                impls: HashSet::default(),
                                name,
                                status: TypeStatus::NeedInference,
                            },
                        );
                    }
                }
            }

            Event::TypeConstraintSuggested {
                type_ref,
                constraint,
            } => {
                let mut final_ref = type_ref.clone();
                if let TypePointer::Ref(t) = self.follow_link(&TypePointer::Ref(type_ref.clone())) {
                    final_ref = t;
                }

                let mut need_deeper_inference = false;

                if let Some(symbols) = self.scoped_symbols.get_mut(&final_ref.scope.id()) {
                    if let Some(dict) = symbols.inner.get_mut(final_ref.name.as_str()) {
                        if (constraint == "Send" || constraint == "Receive")
                            && dict.status == TypeStatus::NeedInference
                            && type_ref != final_ref
                        {
                            need_deeper_inference = true;
                        }

                        dict.add_constraint(constraint);
                    }
                }

                if need_deeper_inference {
                    let symbols = self.scoped_symbols.get_mut(&type_ref.scope.id()).unwrap();
                    let name = format!("@{}", generate_generic_type_name(symbols.infer_id));

                    symbols.infer_id += 1;
                    let new_inner_type = TypeRef {
                        scope: symbols.scope.clone(),
                        name: name.clone(),
                    };

                    symbols.inner.insert(
                        name.clone(),
                        Dict {
                            impls: Default::default(),
                            name,
                            status: TypeStatus::NeedInference,
                        },
                    );

                    symbols.links.insert(
                        type_ref.name.clone(),
                        TypePointer::app(
                            TypePointer::Ref(final_ref),
                            TypePointer::Ref(new_inner_type),
                        ),
                    );
                }
            }

            Event::TypeSuggested {
                target,
                mut suggested,
            } => {
                let final_target = if let TypePointer::Ref(t) =
                    self.follow_link(&TypePointer::Ref(target.clone()))
                {
                    t
                } else {
                    return;
                };

                let target_dict = self.dict(&TypePointer::Ref(final_target.clone()));

                // Suggestion are mainly derived from the AST so it's possible we still have to
                // follow links.
                suggested = self.follow_link(&suggested);

                // In case of a function, we suggest the returned type.
                if let TypePointer::Fun(_, result) = suggested {
                    suggested = result.as_ref().clone();
                }

                match suggested {
                    TypePointer::Rec(rec) => {
                        if target_dict.status == TypeStatus::NeedInference {
                            let symbols = self
                                .scoped_symbols
                                .get_mut(&final_target.scope.id())
                                .unwrap();

                            symbols.inner.remove(final_target.name.as_str());
                            symbols
                                .links
                                .insert(final_target.name.clone(), TypePointer::Rec(rec));
                        }
                    }

                    TypePointer::Ref(suggested) => {
                        if let Some(suggested_dict) = self
                            .scoped_symbols
                            .get(&suggested.scope.id())
                            .and_then(|syms| syms.inner.get(suggested.name.as_str()))
                            .cloned()
                        {
                            match (target_dict.status, suggested_dict.status) {
                                (TypeStatus::NeedInference, TypeStatus::NeedInference) => {
                                    if target_dict.impls.is_subset(&suggested_dict.impls) {
                                        self.redirect_reference_to(target, suggested);
                                    } else if suggested_dict.impls.is_subset(&target_dict.impls) {
                                        self.redirect_reference_to(suggested, target);
                                    }
                                }

                                (TypeStatus::NeedInference, TypeStatus::Defined) => {
                                    if target_dict.impls.is_subset(&suggested_dict.impls) {
                                        self.redirect_reference_to(target, suggested);
                                    }
                                }

                                (TypeStatus::NeedInference, TypeStatus::Generic) => {
                                    self.scoped_symbols
                                        .get_mut(&final_target.scope.id())
                                        .unwrap()
                                        .inner
                                        .get_mut(final_target.name.as_str())
                                        .unwrap()
                                        .impls
                                        .extend(suggested_dict.impls);
                                }

                                (TypeStatus::Defined, TypeStatus::NeedInference) => {
                                    if target_dict.impls.is_superset(&suggested_dict.impls) {
                                        self.redirect_reference_to(suggested, target);
                                    }
                                }

                                _ => {}
                            }
                        }
                    }

                    TypePointer::ForAll(_, _, binders, body) => {
                        self.apply(Event::PushScope);

                        for binder in binders {
                            self.apply(Event::TypeCreated(TypeCreated::Defined {
                                name: binder,
                                constraints: vec![],
                                generic: true,
                            }))
                        }

                        let body = match *body {
                            TypePointer::Qual(constraints, body) => {
                                for constraint in constraints {
                                    if let TypePointer::App(lhs, rhs) = constraint {
                                        // TODO - If we want Haskell-like higher kinded types, we need to make that
                                        // part more flexible. Right now we don't support universally quantified higher
                                        // kinded types.
                                        if let TypePointer::Ref(target) = rhs.as_ref() {
                                            let constraint = self.dict(lhs.as_ref());
                                            let dict = self
                                                .scoped_symbols
                                                .get_mut(&target.scope.id())
                                                .unwrap()
                                                .inner
                                                .get_mut(target.name.as_str())
                                                .unwrap();

                                            if dict.status == TypeStatus::Generic {
                                                dict.impls.insert(constraint.name);
                                            }
                                        }
                                    }
                                }
                                *body
                            }

                            x => x,
                        };

                        self.apply(Event::TypeSuggested {
                            target: final_target,
                            suggested: body,
                        });

                        self.apply(Event::PopScope);
                    }

                    _ => {}
                }
            }
        }
    }

    fn model(&self) -> &Self::Model {
        self
    }
}

impl NominalTyping {
    fn redirect_reference_to(&mut self, source: TypeRef, location: TypeRef) {
        if let Some(symbols) = self.scoped_symbols.get_mut(&source.scope.id()) {
            symbols.inner.remove(source.name.as_str());
            symbols
                .links
                .insert(source.name.clone(), TypePointer::Ref(location));
        }
    }

    fn follow_link<'a>(&'a self, mut location: &'a TypePointer) -> TypePointer {
        let head = location;
        let mut round = 0;

        loop {
            match location {
                TypePointer::Ref(type_ref) => {
                    if round > 0 && head == location {
                        panic!(
                            "Cyclic variable dependency detected on reference {:?}",
                            location
                        );
                    }

                    let symbols = self.scoped_symbols.get(&type_ref.scope.id()).unwrap();

                    if symbols.inner.contains_key(&type_ref.name) {
                        return TypePointer::Ref(type_ref.clone());
                    }

                    location = symbols.links.get(&type_ref.name).unwrap();
                    round += 1;
                }

                TypePointer::App(constr, inner) => {
                    let constr = self.follow_link(constr.as_ref());
                    let inner = self.follow_link(inner.as_ref());

                    return TypePointer::App(Box::new(constr), Box::new(inner));
                }

                TypePointer::Fun(param, result) => {
                    let param = self.follow_link(param.as_ref());
                    let result = self.follow_link(result.as_ref());

                    return TypePointer::Fun(Box::new(param), Box::new(result));
                }

                TypePointer::Rec(rec) => {
                    let mut props = Vec::new();

                    for prop in &rec.props {
                        props.push(Prop {
                            label: prop.label.clone(),
                            val: self.follow_link(&prop.val),
                        });
                    }

                    return TypePointer::Rec(Record { props });
                }

                other => return other.clone(),
            }
        }
    }
}
