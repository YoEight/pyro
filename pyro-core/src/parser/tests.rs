use crate::ast::{Abs, Pat, Proc, Program, Tag, Type, Val, Var};
use crate::parser::Parser;
use crate::tokenizer::{Pos, Tokenizer};
use pretty_assertions::assert_eq;

#[test]
fn test_parse_output() {
    let query = "run x!y";
    let tokenizer = Tokenizer::new(query);
    let tokens = tokenizer.tokenize().unwrap();
    let parser = Parser::new(tokens.as_slice());
    let ast = parser.parse();

    let expected = Program {
        proc: Tag {
            item: Proc::Output(
                Tag {
                    item: Val::Path(vec!["x".to_string()].into()),
                    tag: Pos { line: 1, column: 5 },
                },
                Tag {
                    item: Val::Path(vec!["y".to_string()].into()),
                    tag: Pos { line: 1, column: 7 },
                },
            ),
            tag: Pos { line: 1, column: 1 },
        },
    };

    assert_eq!(expected, ast);
}

#[test]
fn test_parse_abs_with_input() {
    let query = "run a?b = x!y";
    let tokenizer = Tokenizer::new(query);
    let tokens = tokenizer.tokenize().unwrap();
    let parser = Parser::new(tokens.as_slice());
    let ast = parser.parse();

    let expected = Program {
        proc: Tag {
            item: Proc::Input(
                Tag {
                    item: Val::Path(vec!["a".to_string()].into()),
                    tag: Pos { line: 1, column: 5 },
                },
                Tag {
                    item: Abs {
                        pattern: Pat::Var(Var {
                            id: "b".to_string(),
                            r#type: Type::Anonymous,
                        }),
                        proc: Box::new(Proc::Output(
                            Tag {
                                item: Val::Path(vec!["x".to_string()].into()),
                                tag: Pos {
                                    line: 1,
                                    column: 11,
                                },
                            },
                            Tag {
                                item: Val::Path(vec!["y".to_string()].into()),
                                tag: Pos {
                                    line: 1,
                                    column: 13,
                                },
                            },
                        )),
                    },
                    tag: Pos { line: 1, column: 7 },
                },
            ),
            tag: Pos { line: 1, column: 1 },
        },
    };

    assert_eq!(expected, ast);
}

#[test]
fn test_parse_parallel() {
    let query = "run ( a?b = x!y | c?d = () | e!f )";
    let tokenizer = Tokenizer::new(query);
    let tokens = tokenizer.tokenize().unwrap();
    let parser = Parser::new(tokens.as_slice());
    let ast = parser.parse();

    let expected = Program {
        proc: Tag {
            item: Proc::Parallel(
                vec![
                    Proc::Input(
                        Tag {
                            item: Val::Path(vec!["a".to_string()].into()),
                            tag: Pos { line: 1, column: 7 },
                        },
                        Tag {
                            item: Abs {
                                pattern: Pat::Var(Var {
                                    id: "b".to_string(),
                                    r#type: Type::Anonymous,
                                }),
                                proc: Box::new(Proc::Output(
                                    Tag {
                                        item: Val::Path(vec!["x".to_string()].into()),
                                        tag: Pos {
                                            line: 1,
                                            column: 13,
                                        },
                                    },
                                    Tag {
                                        item: Val::Path(vec!["y".to_string()].into()),
                                        tag: Pos {
                                            line: 1,
                                            column: 15,
                                        },
                                    },
                                )),
                            },
                            tag: Pos { line: 1, column: 9 },
                        },
                    ),
                    Proc::Input(
                        Tag {
                            item: Val::Path(vec!["c".to_string()].into()),
                            tag: Pos {
                                line: 1,
                                column: 19,
                            },
                        },
                        Tag {
                            item: Abs {
                                pattern: Pat::Var(Var {
                                    id: "d".to_string(),
                                    r#type: Type::Anonymous,
                                }),
                                proc: Box::new(Proc::Null),
                            },
                            tag: Pos {
                                line: 1,
                                column: 21,
                            },
                        },
                    ),
                    Proc::Output(
                        Tag {
                            item: Val::Path(vec!["e".to_string()].into()),
                            tag: Pos {
                                line: 1,
                                column: 30,
                            },
                        },
                        Tag {
                            item: Val::Path(vec!["f".to_string()].into()),
                            tag: Pos {
                                line: 1,
                                column: 32,
                            },
                        },
                    ),
                ]
                .into(),
            ),
            tag: Pos { line: 1, column: 1 },
        },
    };

    assert_eq!(expected, ast);
}
