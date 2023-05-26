use crate::sym::{Literal, Punctuation};
use crate::tokenizer::{Keyword, Pos, Sym, Token, Tokenizer};
use pretty_assertions::assert_eq;

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
            item: Sym::Punctuation(Punctuation::LParen),
            pos: Pos { line: 1, column: 5 },
        },
        Token {
            item: Sym::Id("x".to_string()),
            pos: Pos { line: 1, column: 6 },
        },
        Token {
            item: Sym::Punctuation(Punctuation::QuestionMark),
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
            item: Sym::Punctuation(Punctuation::ExclamationMark),
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
            item: Sym::Punctuation(Punctuation::Pipe),
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
            item: Sym::Punctuation(Punctuation::ExclamationMark),
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
            item: Sym::Punctuation(Punctuation::RParen),
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

#[test]
fn test_token_number() {
    let tokenizer = Tokenizer { query: "1234" };
    let expected = vec![
        Token {
            item: Sym::Literal(Literal::Integer(1234)),
            pos: Pos { line: 1, column: 1 },
        },
        Token {
            item: Sym::EOF,
            pos: Pos { line: 1, column: 5 },
        },
    ];

    let actual = tokenizer.tokenize().unwrap();
    assert_eq!(expected, actual);
}
