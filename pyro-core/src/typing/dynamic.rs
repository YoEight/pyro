use crate::typing::{Event, TypeCreated, Types};
use crate::utils::generate_generic_type_name;
use crate::{LocalScope, Machine, Scope, Type, TypePointer, TypeRef, STDLIB};

#[derive(Clone, Default)]
pub struct DynamicTyping {
    id_gen: usize,
}

impl Types for DynamicTyping {
    fn project(&self, _target: &TypePointer) -> Type {
        Type::Var {
            name: "Dynamic".to_string(),
        }
    }

    fn type_check(&self, _expect: &TypePointer, _provided: &TypePointer) -> bool {
        true
    }

    fn look_up<S: Scope>(&self, scope: &S, name: &str) -> Option<TypePointer> {
        Some(TypePointer::Ref(TypeRef {
            scope: scope.as_local(),
            name: name.to_string(),
        }))
    }

    fn current_scope(&self) -> LocalScope {
        let mut scope = STDLIB.as_local_scope();
        scope.ancestors.push(1);

        scope
    }

    fn current_available_name(&self) -> TypeRef {
        let name = format!("@{}", generate_generic_type_name(self.id_gen));

        TypeRef {
            scope: STDLIB.as_local_scope(),
            name,
        }
    }

    fn as_function<'a>(
        &self,
        r#type: &'a TypePointer,
    ) -> Option<(&'a TypePointer, &'a TypePointer)> {
        Some((r#type, r#type))
    }

    fn as_type_constructor<'a>(
        &self,
        r#type: &'a TypePointer,
    ) -> Option<(&'a TypePointer, &'a TypePointer)> {
        Some((r#type, r#type))
    }

    fn type_implements(&self, _type: &TypePointer, _constraint: &str) -> bool {
        true
    }
}

impl Machine for DynamicTyping {
    type Model = Self;

    fn apply(&mut self, event: Event) {
        if let Event::TypeCreated(TypeCreated::Inferred) = event {
            self.id_gen += 1;
        }
    }

    fn model(&self) -> &Self::Model {
        self
    }
}
