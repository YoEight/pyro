use crate::ast::{Prop, Record, Type};

#[test]
fn test_typematch_contravariance_simple() {
    assert!(Type::show().parent_type_of(&Type::integer()));
}

#[test]
fn test_typematch_contravariance_record() {
    let mut target = Type::Record(Record {
        props: vec![Prop {
            label: None,
            val: Type::integer(),
        }],
    });

    target = Type::client(target);

    let param = Type::Record(Record {
        props: vec![Prop {
            label: None,
            val: Type::show(),
        }],
    });

    assert!(target.typecheck_client_call(&param));
}
