use crate::env::Env;
use crate::runtime::Runtime;
use crate::value::{Channel, RuntimeValue};
use pyro_core::sym::Literal;
use std::sync::Arc;

pub fn load_prelude_symbols(env: &Env) -> Runtime {
    let mut scope = Runtime::new(env.clone());

    scope.insert(
        "print".to_string(),
        RuntimeValue::Channel(Channel::Client(env.stdout())),
    );

    let stdout = env.stdout();
    scope.insert(
        "+".to_string(),
        RuntimeValue::Fun(Arc::new(move |a| {
            let local = stdout.clone();
            RuntimeValue::Fun(Arc::new(move |b| match (a.clone(), b.clone()) {
                (
                    RuntimeValue::Literal(Literal::Integer(a)),
                    RuntimeValue::Literal(Literal::Integer(b)),
                ) => RuntimeValue::Literal(Literal::Integer(a + b)),

                // Code has been type-checked prior to execution!
                _ => {
                    let _ = local.send(RuntimeValue::string(
                        "BUG: Unexpected type exception on '+' function",
                    ));
                    unreachable!()
                }
            }))
        })),
    );

    let stdout = env.stdout();
    scope.insert(
        "<=".to_string(),
        RuntimeValue::Fun(Arc::new(move |a| {
            let local = stdout.clone();
            RuntimeValue::Fun(Arc::new(move |b| match (a.clone(), b.clone()) {
                (
                    RuntimeValue::Literal(Literal::Integer(a)),
                    RuntimeValue::Literal(Literal::Integer(b)),
                ) => RuntimeValue::Literal(Literal::Bool(a <= b)),

                // Code has been type-checked prior to execution!
                _ => {
                    let _ = local.send(RuntimeValue::string(
                        "BUG: Unexpected type exception on '<=' function",
                    ));
                    unreachable!()
                }
            }))
        })),
    );

    scope
}
