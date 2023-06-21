pub trait Scope {
    fn id(&self) -> u32;
    fn ancestors(&self) -> &[u32];
    fn as_local(&self) -> LocalScope;
}

pub struct STDLIB;

impl STDLIB {
    pub fn as_local_scope(&self) -> LocalScope {
        LocalScope {
            ancestors: vec![self.id()],
        }
    }
}

impl Scope for STDLIB {
    fn id(&self) -> u32 {
        0
    }

    fn ancestors(&self) -> &[u32] {
        &[0]
    }

    fn as_local(&self) -> LocalScope {
        self.as_local_scope()
    }
}

#[derive(Default, Clone)]
pub struct LocalScope {
    pub ancestors: Vec<u32>,
}

impl Scope for LocalScope {
    fn id(&self) -> u32 {
        self.ancestors[self.ancestors.len() - 1]
    }

    fn ancestors(&self) -> &[u32] {
        self.ancestors.as_slice()
    }

    fn as_local(&self) -> LocalScope {
        self.clone()
    }
}
