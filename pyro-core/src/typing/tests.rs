mod compiler;

use crate::ast::Prop;
use crate::typing::tests::compiler::TestCompiler;
use crate::{NominalTyping, TypePointer, TypeSystem};

fn type_check_send(
    know: &mut TypeSystem<NominalTyping>,
    require: &TypePointer,
    provided: &TypePointer,
) -> bool {
    if !know.implements(require, "Send") {
        return false;
    }

    if let Some((_, inner)) = know.as_type_constructor(require) {
        return know.param_matches(&inner, provided);
    }

    false
}

#[test]
fn test_type_check_client_easy() {
    let mut know = TypeSystem::new(NominalTyping::default());
    let client = TypePointer::client();
    let integer = TypePointer::integer();

    assert!(type_check_send(
        &mut know,
        &TypePointer::app(client, integer.clone()),
        &integer
    ));
}

#[test]
fn test_type_check_generic() {
    let mut know = TypeSystem::new(NominalTyping::default());
    let scope = know.push_scope();
    let var = know.new_generic("'a");
    let target_type = TypePointer::app(
        TypePointer::client(),
        TypePointer::ForAll(
            false,
            scope,
            vec!["'a".to_string()],
            Box::new(TypePointer::Qual(
                vec![TypePointer::app(TypePointer::show(), var.clone())],
                Box::new(var),
            )),
        ),
    );

    let integer = TypePointer::integer();
    assert!(type_check_send(&mut know, &target_type, &integer));
}

#[test]
fn test_type_check_generic_complex() {
    let mut know = TypeSystem::new(NominalTyping::default());
    let scope = know.push_scope();
    let var = know.new_generic("'a");
    let target_type = TypePointer::app(
        TypePointer::client(),
        TypePointer::rec(vec![Prop::ano(TypePointer::ForAll(
            false,
            scope,
            vec!["'a".to_string()],
            Box::new(TypePointer::Qual(
                vec![TypePointer::app(TypePointer::show(), var.clone())],
                Box::new(var),
            )),
        ))]),
    );

    let integer = TypePointer::integer();
    assert!(type_check_send(
        &mut know,
        &target_type,
        &TypePointer::rec(vec![Prop::ano(integer)])
    ));
}

#[test]
fn test_forall_inner_type_as_parameter() {
    let mut compiler = TestCompiler::new();

    compiler.load_module("def foobar [done: ![]] = ()").unwrap();
    compiler.compile("run foobar ! [print]").unwrap();
}
