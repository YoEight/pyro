use crate::sym::Literal;
use crate::tokenizer::{Keyword, Pos, Sym, Token, Tokenizer};

#[test]
fn tokenize_simple_expr() {
    let src = include_str!("simple_expr.pi");
    let tokenizer = Tokenizer { query: src };

    let expected = vec![
        Token {
            item: Sym::Keyword(Keyword::Run),
            pos: Pos { line: 1, column: 1 },
        },
        Token {
            item: Sym::Whitespace,
            pos: Pos { line: 1, column: 4 },
        },
        Token {
            item: Sym::LParen,
            pos: Pos { line: 1, column: 5 },
        },
        Token {
            item: Sym::Id("x".to_string()),
            pos: Pos { line: 1, column: 6 },
        },
        Token {
            item: Sym::QuestionMark,
            pos: Pos { line: 1, column: 7 },
        },
        Token {
            item: Sym::Id("z".to_string()),
            pos: Pos { line: 1, column: 8 },
        },
        Token {
            item: Sym::Whitespace,
            pos: Pos { line: 1, column: 9 },
        },
        Token {
            item: Sym::Eq,
            pos: Pos {
                line: 1,
                column: 10,
            },
        },
        Token {
            item: Sym::Whitespace,
            pos: Pos {
                line: 1,
                column: 11,
            },
        },
        Token {
            item: Sym::Id("print".to_string()),
            pos: Pos {
                line: 1,
                column: 12,
            },
        },
        Token {
            item: Sym::ExclamationMark,
            pos: Pos {
                line: 1,
                column: 17,
            },
        },
        Token {
            item: Sym::Literal(Literal::String("Got it!".to_string())),
            pos: Pos {
                line: 1,
                column: 18,
            },
        },
        Token {
            item: Sym::Whitespace,
            pos: Pos {
                line: 1,
                column: 27,
            },
        },
        Token {
            item: Sym::Pipe,
            pos: Pos {
                line: 1,
                column: 28,
            },
        },
        Token {
            item: Sym::Whitespace,
            pos: Pos {
                line: 1,
                column: 29,
            },
        },
        Token {
            item: Sym::Id("x".to_string()),
            pos: Pos {
                line: 1,
                column: 30,
            },
        },
        Token {
            item: Sym::ExclamationMark,
            pos: Pos {
                line: 1,
                column: 31,
            },
        },
        Token {
            item: Sym::Id("y".to_string()),
            pos: Pos {
                line: 1,
                column: 32,
            },
        },
        Token {
            item: Sym::RParen,
            pos: Pos {
                line: 1,
                column: 33,
            },
        },
        Token {
            item: Sym::EOF,
            pos: Pos {
                line: 1,
                column: 34,
            },
        },
    ];

    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(expected, tokens);
}
