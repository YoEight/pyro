use crate::env::Env;
use crate::runtime::Runtime;
use crate::value::{Channel, RuntimeValue};

pub fn load_prelude_symbols(env: &Env) -> Runtime {
    let mut scope = Runtime::new(env.clone());

    scope.insert(
        "print".to_string(),
        RuntimeValue::Channel(Channel::Client(env.stdout())),
    );

    scope.insert(
        "+".to_string(),
        RuntimeValue::func_2(|a: u64, b: u64| a + b),
    );

    scope.insert(
        "<=".to_string(),
        RuntimeValue::func_2(|a: u64, b: u64| a <= b),
    );

    scope
}
