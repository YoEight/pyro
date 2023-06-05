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
            val: Type::generic_with_constraints("a", vec![Type::show()]),
        }],
    });

    target = Type::client(target);

    let param = Type::Record(Record {
        props: vec![Prop {
            label: None,
            val: Type::integer(),
        }],
    });

    assert!(target.typecheck_client_call(&param));
}
