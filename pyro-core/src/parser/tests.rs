use crate::ast::{Proc, Program, Tag, Val};
use crate::parser::Parser;
use crate::tokenizer::{Pos, Tokenizer};

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
