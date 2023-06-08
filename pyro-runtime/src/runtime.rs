use crate::env::Env;
use crate::value::{Channel, RuntimeValue};
use pyro_core::annotate::Ann;
use pyro_core::ast::{Decl, Def};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use tokio::sync::{mpsc, Mutex};

#[derive(Clone)]
pub struct Runtime {
    env: Env,
    variables: HashMap<String, RuntimeValue>,
}

impl Runtime {
    pub fn new(env: Env) -> Self {
        Self {
            env,
            variables: Default::default(),
        }
    }

    pub fn println(&self, msg: impl AsRef<str>) {
        let _ = self.env.stdout_handle.send(RuntimeValue::string(msg));
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

    pub fn keeps<'a, I>(&mut self, keys: I)
    where
        I: Iterator<Item = &'a String>,
    {
        let mut set = HashSet::new();
        let mut stack = Vec::new();

        for key in keys {
            set.insert(key.clone());

            if let Some(value) = self.variables.get(key) {
                if let RuntimeValue::Abs(abs) = value {
                    stack.push(abs.tag.used.keys().cloned());
                }
            }
        }

        while let Some(deps) = stack.pop() {
            for dep in deps {
                if set.contains(&dep) {
                    continue;
                }

                set.insert(dep.clone());
                if let Some(RuntimeValue::Abs(abs)) = self.variables.get(&dep) {
                    stack.push(abs.tag.used.keys().cloned());
                }
            }
        }

        self.variables.retain(|k, _| set.contains(k));
    }

    pub fn register_def(&mut self, def: Def<Ann>) {
        self.variables
            .insert(def.name.clone(), RuntimeValue::Abs(def.abs));
    }
}
