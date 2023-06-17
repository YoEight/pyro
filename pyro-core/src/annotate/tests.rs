use crate::ast::Type;

#[test]
fn test_typematch_contravariance_simple() {
    let parent_type = Type::Name {
        parent: vec![],
        name: "Parent".to_string(),
        kind: 0,
        generic: false,
    };

    let child_type = Type::Name {
        parent: vec![parent_type.clone()],
        name: "Child".to_string(),
        kind: 0,
        generic: false,
    };

    assert!(parent_type.parent_type_of(&child_type));
}
