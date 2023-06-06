use crate::ast::Type;

#[test]
fn test_typematch_contravariance_simple() {
    assert!(Type::show().parent_type_of(&Type::integer()));
}
