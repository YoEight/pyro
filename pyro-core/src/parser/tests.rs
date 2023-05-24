use crate::ast::{Abs, Decl, Def, Pat, PatVar, Proc, Program, Prop, Record, Tag, Type, Val, Var};
use crate::parser::{Parser, ParserState};
use crate::sym::{Literal, Sym};
use crate::tokenizer::{Pos, Tokenizer};
use pretty_assertions::assert_eq;

#[test]
fn test_parse_output() {
    let query = "run x!y";
    let tokenizer = Tokenizer::new(query);
    let tokens = tokenizer.tokenize().unwrap();
    let parser = Parser::new(tokens.as_slice());
    let ast = parser.parse().unwrap();

    let expected = Program {
        procs: vec![Tag {
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
            tag: Pos { line: 1, column: 5 },
        }]
        .into(),
    };

    assert_eq!(expected, ast);
}

#[test]
fn test_parse_abs_with_input() {
    let query = "run a?b = x!y";
    let tokenizer = Tokenizer::new(query);
    let tokens = tokenizer.tokenize().unwrap();
    let parser = Parser::new(tokens.as_slice());
    let ast = parser.parse().unwrap();

    let expected = Program {
        procs: vec![Tag {
            item: Proc::Input(
                Tag {
                    item: Val::Path(vec!["a".to_string()].into()),
                    tag: Pos { line: 1, column: 5 },
                },
                Tag {
                    item: Abs {
                        pattern: Pat::Var(PatVar {
                            var: Var {
                                id: "b".to_string(),
                                r#type: Type::Anonymous,
                            },
                            pattern: None,
                        }),
                        proc: Box::new(Tag {
                            tag: Pos {
                                line: 1,
                                column: 11,
                            },
                            item: Proc::Output(
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
                            ),
                        }),
                    },
                    tag: Pos { line: 1, column: 7 },
                },
            ),
            tag: Pos { line: 1, column: 5 },
        }]
        .into(),
    };

    assert_eq!(expected, ast);
}

#[test]
fn test_parse_parallel() {
    let query = "run ( a?b = x!y | c?d = () | e!f )";
    let tokenizer = Tokenizer::new(query);
    let tokens = tokenizer.tokenize().unwrap();
    let parser = Parser::new(tokens.as_slice());
    let ast = parser.parse().unwrap();

    let expected = Program {
        procs: vec![Tag {
            item: Proc::Parallel(
                vec![
                    Tag {
                        tag: Pos { line: 1, column: 7 },
                        item: Proc::Input(
                            Tag {
                                item: Val::Path(vec!["a".to_string()].into()),
                                tag: Pos { line: 1, column: 7 },
                            },
                            Tag {
                                item: Abs {
                                    pattern: Pat::Var(PatVar {
                                        var: Var {
                                            id: "b".to_string(),
                                            r#type: Type::Anonymous,
                                        },
                                        pattern: None,
                                    }),
                                    proc: Box::new(Tag {
                                        tag: Pos {
                                            line: 1,
                                            column: 13,
                                        },
                                        item: Proc::Output(
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
                                        ),
                                    }),
                                },
                                tag: Pos { line: 1, column: 9 },
                            },
                        ),
                    },
                    Tag {
                        tag: Pos {
                            line: 1,
                            column: 19,
                        },
                        item: Proc::Input(
                            Tag {
                                item: Val::Path(vec!["c".to_string()].into()),
                                tag: Pos {
                                    line: 1,
                                    column: 19,
                                },
                            },
                            Tag {
                                item: Abs {
                                    pattern: Pat::Var(PatVar {
                                        var: Var {
                                            id: "d".to_string(),
                                            r#type: Type::Anonymous,
                                        },
                                        pattern: None,
                                    }),
                                    proc: Box::new(Tag {
                                        tag: Pos {
                                            line: 1,
                                            column: 25,
                                        },
                                        item: Proc::Null,
                                    }),
                                },
                                tag: Pos {
                                    line: 1,
                                    column: 21,
                                },
                            },
                        ),
                    },
                    Tag {
                        tag: Pos {
                            line: 1,
                            column: 30,
                        },
                        item: Proc::Output(
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
                    },
                ]
                .into(),
            ),
            tag: Pos { line: 1, column: 5 },
        }]
        .into(),
    };

    assert_eq!(expected, ast);
}

#[test]
fn test_parse_record_type() {
    let query = "[a=Foo Baz]";
    let tokenizer = Tokenizer::new(query);
    let tokens = tokenizer.tokenize().unwrap();
    let mut state = ParserState::new(tokens.as_slice());
    let ast = state.parse_record_type().unwrap();

    let expected = Type::Record(Record {
        props: vec![
            Prop {
                label: Some("a".to_string()),
                val: Type::Name("Foo".to_string()),
            },
            Prop {
                label: None,
                val: Type::Name("Baz".to_string()),
            },
        ]
        .into(),
    });

    assert_eq!(expected, ast);
    assert_eq!(state.look_ahead().item(), &Sym::EOF);
}

#[test]
fn test_parse_record_value() {
    let query = "[a=b true c]";
    let tokenizer = Tokenizer::new(query);
    let tokens = tokenizer.tokenize().unwrap();
    let mut state = ParserState::new(tokens.as_slice());
    let ast = state.parse_record_value().unwrap().item;

    let expected = Val::Record(Record {
        props: vec![
            Prop {
                label: Some("a".to_string()),
                val: Val::Path(vec!["b".to_string()].into()),
            },
            Prop {
                label: None,
                val: Val::Literal(Literal::Bool(true)),
            },
            Prop {
                label: None,
                val: Val::Path(vec!["c".to_string()].into()),
            },
        ]
        .into(),
    });

    assert_eq!(expected, ast);
    assert_eq!(state.look_ahead().item(), &Sym::EOF);
}

#[test]
fn test_parse_new_channel() {
    let query = "new stream : Bool";
    let tokenizer = Tokenizer::new(query);
    let tokens = tokenizer.tokenize().unwrap();
    let mut state = ParserState::new(tokens.as_slice());
    let ast = state.parse_decl().unwrap();

    let expected = Decl::Channel("stream".to_string(), Type::Name("Bool".to_string()));

    assert_eq!(expected, ast);
    assert_eq!(state.look_ahead().item(), &Sym::EOF);
}

#[test]
fn test_parse_type_decl() {
    let query = "type Foobar = ^[too=String ^[Int]]";
    let tokenizer = Tokenizer::new(query);
    let tokens = tokenizer.tokenize().unwrap();
    let mut state = ParserState::new(tokens.as_slice());
    let ast = state.parse_type_decl().unwrap();

    let expected = Decl::Type(
        "Foobar".to_string(),
        Type::Channel(Box::new(Type::Record(Record {
            props: vec![
                Prop {
                    label: Some("too".to_string()),
                    val: Type::Name("String".to_string()),
                },
                Prop {
                    label: None,
                    val: Type::Channel(Box::new(Type::Record(Record {
                        props: vec![Prop {
                            label: None,
                            val: Type::Name("Int".to_string()),
                        }]
                        .into(),
                    }))),
                },
            ]
            .into(),
        }))),
    );

    assert_eq!(expected, ast);
    assert_eq!(state.look_ahead().item(), &Sym::EOF);
}

#[test]
fn test_parse_defs() {
    let query = include_str!("parse_defs.pi");
    let tokenizer = Tokenizer::new(query);
    let tokens = tokenizer.tokenize().unwrap();
    let mut state = ParserState::new(tokens.as_slice());
    let ast = state.parse_def().unwrap();

    let expected = Decl::Def(
        vec![
            Def {
                name: "tt".to_string(),
                abs: Abs {
                    pattern: Pat::Record(Record {
                        props: vec![Prop {
                            label: None,
                            val: Pat::Var(PatVar {
                                var: Var {
                                    id: "b".to_string(),
                                    r#type: Type::Name("Boolean".to_string()),
                                },
                                pattern: None,
                            }),
                        }]
                        .into(),
                    }),
                    proc: Box::new(Tag {
                        tag: Pos {
                            line: 1,
                            column: 21,
                        },
                        item: Proc::Input(
                            Tag {
                                item: Val::Path(vec!["b".to_string()].into()),
                                tag: Pos {
                                    line: 1,
                                    column: 21,
                                },
                            },
                            Tag {
                                item: Abs {
                                    pattern: Pat::Record(Record {
                                        props: vec![
                                            Prop {
                                                label: None,
                                                val: Pat::Var(PatVar {
                                                    var: Var {
                                                        id: "t".to_string(),
                                                        r#type: Type::Anonymous,
                                                    },
                                                    pattern: None,
                                                }),
                                            },
                                            Prop {
                                                label: None,
                                                val: Pat::Var(PatVar {
                                                    var: Var {
                                                        id: "f".to_string(),
                                                        r#type: Type::Anonymous,
                                                    },
                                                    pattern: None,
                                                }),
                                            },
                                        ]
                                        .into(),
                                    }),
                                    proc: Box::new(Tag {
                                        tag: Pos {
                                            line: 1,
                                            column: 31,
                                        },
                                        item: Proc::Output(
                                            Tag {
                                                item: Val::Path(vec!["t".to_string()].into()),
                                                tag: Pos {
                                                    line: 1,
                                                    column: 31,
                                                },
                                            },
                                            Tag {
                                                item: Val::Record(Record {
                                                    props: vec![].into(),
                                                }),
                                                tag: Pos {
                                                    line: 1,
                                                    column: 33,
                                                },
                                            },
                                        ),
                                    }),
                                },
                                tag: Pos {
                                    line: 1,
                                    column: 23,
                                },
                            },
                        ),
                    }),
                },
            },
            Def {
                name: "ff".to_string(),
                abs: Abs {
                    pattern: Pat::Record(Record {
                        props: vec![Prop {
                            label: None,
                            val: Pat::Var(PatVar {
                                var: Var {
                                    id: "b".to_string(),
                                    r#type: Type::Name("Boolean".to_string()),
                                },
                                pattern: None,
                            }),
                        }]
                        .into(),
                    }),
                    proc: Box::new(Tag {
                        tag: Pos {
                            line: 2,
                            column: 21,
                        },
                        item: Proc::Input(
                            Tag {
                                item: Val::Path(vec!["b".to_string()].into()),
                                tag: Pos {
                                    line: 2,
                                    column: 21,
                                },
                            },
                            Tag {
                                item: Abs {
                                    pattern: Pat::Record(Record {
                                        props: vec![
                                            Prop {
                                                label: None,
                                                val: Pat::Var(PatVar {
                                                    var: Var {
                                                        id: "t".to_string(),
                                                        r#type: Type::Anonymous,
                                                    },
                                                    pattern: None,
                                                }),
                                            },
                                            Prop {
                                                label: None,
                                                val: Pat::Var(PatVar {
                                                    var: Var {
                                                        id: "f".to_string(),
                                                        r#type: Type::Anonymous,
                                                    },
                                                    pattern: None,
                                                }),
                                            },
                                        ]
                                        .into(),
                                    }),
                                    proc: Box::new(Tag {
                                        tag: Pos {
                                            line: 2,
                                            column: 31,
                                        },
                                        item: Proc::Output(
                                            Tag {
                                                item: Val::Path(vec!["f".to_string()].into()),
                                                tag: Pos {
                                                    line: 2,
                                                    column: 31,
                                                },
                                            },
                                            Tag {
                                                item: Val::Record(Record {
                                                    props: vec![].into(),
                                                }),
                                                tag: Pos {
                                                    line: 2,
                                                    column: 33,
                                                },
                                            },
                                        ),
                                    }),
                                },
                                tag: Pos {
                                    line: 2,
                                    column: 23,
                                },
                            },
                        ),
                    }),
                },
            },
        ]
        .into(),
    );

    assert_eq!(expected, ast);
    assert_eq!(state.look_ahead().item(), &Sym::EOF);
}
