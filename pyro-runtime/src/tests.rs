use crate::helpers::{Declared, TypeBuilder};
use crate::{Engine, PyroType};

struct EventEntry {}

impl PyroType for EventEntry {
    fn r#type(builder: TypeBuilder) -> Declared {
        builder.create("Entry").with_constraint("Show").build()
    }
}

struct EventRecord;

impl PyroType for EventRecord {
    fn r#type(builder: TypeBuilder) -> Declared {
        builder
            .rec()
            .prop::<String>("id")
            .prop::<String>("stream_name")
            .prop::<String>("event_type")
            .prop::<i64>("event_revision")
            .prop::<i64>("position")
            .prop::<EventEntry>("payload")
            .done()
    }
}

struct ProgramOutput;

impl PyroType for ProgramOutput {
    fn r#type(builder: TypeBuilder) -> Declared {
        builder.constr("Client").for_all_type().done()
    }
}

struct Subscribe;

impl PyroType for Subscribe {
    fn r#type(builder: TypeBuilder) -> Declared {
        builder.func_of::<String>().result_of::<Sub>()
    }
}

struct Sub;

impl PyroType for Sub {
    fn r#type(builder: TypeBuilder) -> Declared {
        builder.constr("Server").of::<EventRecord>()
    }
}

#[test]
fn test_geth_should_infer() {
    let source_code = include_str!("tests/infer_process.pi");

    let engine = Engine::builder()
        .register_type::<EventEntry>("Entry")
        .register_type::<EventRecord>("EventRecord")
        .register_type::<ProgramOutput>("output")
        .register_type::<Subscribe>("subscribe")
        .build()
        .unwrap();

    engine.compile(source_code).unwrap();
}

struct Loop;

impl PyroType for Loop {
    fn r#type(builder: TypeBuilder) -> Declared {
        builder.constr("Client").of::<i64>()
    }
}

#[test]
fn test_geth_should_infer_loop() {
    let source_code = include_str!("tests/infer_loop.pi");

    let engine = Engine::builder()
        .register_function_2("+", |x: i64, y: i64| x + y)
        .build()
        .unwrap();

    engine.compile(source_code).unwrap();
}
