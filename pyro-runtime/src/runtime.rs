use crate::env::Env;
use crate::value::{Channel, RuntimeValue};
use pyro_core::annotate::Ann;
use pyro_core::ast::{Decl, Def};
use pyro_core::{Scope, UsedVariables};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::{mpsc, Mutex};

#[derive(Clone)]
pub struct Runtime {
    pub(crate) env: Option<Env>,
    pub(crate) variables: HashMap<String, RuntimeValue>,
    pub(crate) used: UsedVariables,
}

impl Runtime {
    pub fn println(&self, msg: impl AsRef<str>) {
        if let Some(env) = self.env.as_ref() {
            let _ = env.stdout_handle.send(RuntimeValue::string(msg));
        }
    }

    pub(crate) fn look_up(&self, name: &str) -> eyre::Result<RuntimeValue> {
        if let Some(value) = self.variables.get(name).cloned() {
            return Ok(value);
        }

        eyre::bail!("Unknown identifier '{}'", name)
    }

    pub(crate) fn insert(&mut self, name: String, value: RuntimeValue) {
        self.variables.insert(name, value);
    }

    pub fn register(&mut self, decl: Decl<Ann>) {
        match decl {
            Decl::Channels(chans) => {
                for chan_tag in chans {
                    let (name, _) = chan_tag.item;
                    let (output, input) = mpsc::unbounded_channel();
                    let input = Arc::new(Mutex::new(input));

                    self.insert(name, RuntimeValue::Channel(Channel::Dual(input, output)));
                }
            }

            Decl::Def(defs) => {
                for def in defs {
                    self.register_def(def.item);
                }
            }

            Decl::Type(_, _) => {
                // There is no value at runtime to register a new type declaration.
            }
        }
    }

    pub fn keeps<S: Scope>(&mut self, scope: &S) {
        let set = self.used.list_used_variables(scope);
        self.variables.retain(|k, _| set.contains(k));
    }

    pub fn register_def(&mut self, def: Def<Ann>) {
        self.variables
            .insert(def.name.clone(), RuntimeValue::Abs(def.abs));
    }
}
