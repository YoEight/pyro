use pyro_core::ast::Prop;

use crate::value::PyroType;

pub struct Declared(pub(crate) TypeCommand);

pub(crate) enum TypeCommand {
    LookUp(String),

    CreateType {
        name: String,
        constraints: Vec<String>,
    },

    CreateForAll {
        constraints: Vec<String>,
    },

    CreateFunc {
        params: Vec<Declared>,
    },

    CreateTypeConstr {
        constr: String,
        inner_type: InnerType,
    },

    Rec {
        props: Vec<Prop<Declared>>,
    },
}

pub(crate) enum InnerType {
    Declared(Box<TypeCommand>),
    ForAll { constraints: Vec<String> },
}

pub struct CreateType {
    name: String,
    constraints: Vec<String>,
}

impl CreateType {
    pub fn with_constraint(mut self, constraint: impl AsRef<str>) -> Self {
        self.constraints.push(constraint.as_ref().to_string());
        self
    }

    pub fn build(self) -> Declared {
        Declared(TypeCommand::CreateType {
            name: self.name,
            constraints: self.constraints,
        })
    }
}

pub struct ForAllBuilder {
    constraints: Vec<String>,
}

impl ForAllBuilder {
    pub fn with_constraint(mut self, constraint: impl AsRef<str>) -> Self {
        self.constraints.push(constraint.as_ref().to_string());
        self
    }

    pub fn build(self) -> Declared {
        Declared(TypeCommand::CreateForAll {
            constraints: self.constraints,
        })
    }
}

pub struct FuncBuilder {
    params: Vec<Declared>,
}

impl FuncBuilder {
    pub fn param_of<T: PyroType>(mut self) -> Self {
        self.params.push(T::r#type(TypeBuilder(())));
        self
    }

    pub fn result_of<T: PyroType>(mut self) -> Declared {
        self.params.push(T::r#type(TypeBuilder(())));

        Declared(TypeCommand::CreateFunc {
            params: self.params,
        })
    }
}

pub struct TypeConstructorBuilder {
    constr: String,
}

impl TypeConstructorBuilder {
    pub fn of<T: PyroType>(self) -> Declared {
        Declared(TypeCommand::CreateTypeConstr {
            constr: self.constr,
            inner_type: InnerType::Declared(Box::new(T::r#type(TypeBuilder(())).0)),
        })
    }

    pub fn for_all_type(self) -> TypeConstrForAllVarBuilder {
        TypeConstrForAllVarBuilder {
            inner: self,
            constraints: vec![],
        }
    }
}

pub struct TypeConstrForAllVarBuilder {
    inner: TypeConstructorBuilder,
    constraints: Vec<String>,
}

impl TypeConstrForAllVarBuilder {
    pub fn with_constraint(mut self, constraint: impl AsRef<str>) -> Self {
        self.constraints.push(constraint.as_ref().to_string());
        self
    }

    pub fn done(self) -> Declared {
        Declared(TypeCommand::CreateTypeConstr {
            constr: self.inner.constr,
            inner_type: InnerType::ForAll {
                constraints: self.constraints,
            },
        })
    }
}

pub struct RecTypeBuilder {
    props: Vec<Prop<Declared>>,
}

impl RecTypeBuilder {
    pub fn prop<T: PyroType>(mut self, name: impl AsRef<str>) -> Self {
        self.props
            .push(Prop::new(Some(name), T::r#type(TypeBuilder(()))));
        self
    }

    pub fn ano<T: PyroType>(mut self) -> Self {
        self.props
            .push(Prop::new(None::<String>, T::r#type(TypeBuilder(()))));
        self
    }

    pub fn done(self) -> Declared {
        Declared(TypeCommand::Rec { props: self.props })
    }
}

#[derive(Default)]
pub struct TypeBuilder(pub(crate) ());

impl TypeBuilder {
    pub fn of(self, name: impl AsRef<str>) -> Declared {
        Declared(TypeCommand::LookUp(name.as_ref().to_string()))
    }

    pub fn create(self, name: impl AsRef<str>) -> CreateType {
        CreateType {
            name: name.as_ref().to_string(),
            constraints: vec![],
        }
    }

    pub fn for_all_type(self) -> ForAllBuilder {
        ForAllBuilder {
            constraints: vec![],
        }
    }

    pub fn func_of<T: PyroType>(self) -> FuncBuilder {
        FuncBuilder {
            params: vec![T::r#type(TypeBuilder(()))],
        }
    }

    pub fn rec(self) -> RecTypeBuilder {
        RecTypeBuilder { props: vec![] }
    }

    pub fn constr(self, name: impl AsRef<str>) -> TypeConstructorBuilder {
        TypeConstructorBuilder {
            constr: name.as_ref().to_string(),
        }
    }
}
