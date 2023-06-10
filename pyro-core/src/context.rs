use crate::ast::Type;
use std::collections::HashMap;

pub trait Scope {
    fn id(&self) -> u32;
    fn ancestors(&self) -> &[u32];
}

pub struct STDLIB;

impl Scope for STDLIB {
    fn id(&self) -> u32 {
        0
    }

    fn ancestors(&self) -> &[u32] {
        &[0]
    }
}

#[derive(Default, Clone)]
pub struct LocalScope {
    ancestors: Vec<u32>,
}

impl Scope for LocalScope {
    fn id(&self) -> u32 {
        self.ancestors[self.ancestors.len() - 1]
    }

    fn ancestors(&self) -> &[u32] {
        self.ancestors.as_slice()
    }
}

#[derive(Default)]
pub struct Ctx {
    scope_id: u32,
    variables_new: HashMap<u32, HashMap<String, Type>>,
}

impl Ctx {
    pub fn new_scope<S: Scope>(&mut self, parent: &S) -> LocalScope {
        self.scope_id += 1;
        let id = self.scope_id;
        let mut ancestors = parent.ancestors().to_vec();

        ancestors.push(id);

        LocalScope { ancestors }
    }

    pub fn look_up<S: Scope>(&self, scope: &S, name: &str) -> Option<Type> {
        for scope_id in scope.ancestors().iter().rev() {
            if let Some(variables) = self.variables_new.get(&scope_id) {
                if let Some(r#type) = variables.get(name) {
                    return Some(r#type.clone());
                }
            }
        }

        None
    }

    pub fn declare<S: Scope>(&mut self, scope: &S, name: impl AsRef<str>, r#type: Type) -> bool {
        let variables = self.variables_new.entry(scope.id()).or_default();

        if variables.contains_key(name.as_ref()) {
            return false;
        }

        variables.insert(name.as_ref().to_string(), r#type);

        true
    }

    pub fn update(&mut self, scope: &LocalScope, name: impl AsRef<str>, r#type: Type) {
        for scope_id in scope.ancestors().iter().rev() {
            if let Some(variables) = self.variables_new.get_mut(scope_id) {
                variables.insert(name.as_ref().to_string(), r#type);
                return;
            }
        }
    }
}
