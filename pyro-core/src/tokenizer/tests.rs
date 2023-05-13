use crate::tokenizer::{Keyword, Literal, Pos, Token, TokenItem, Tokenizer};

#[test]
fn tokenize_simple_expr() {
    let src = include_str!("simple_expr.pi");
    let tokenizer = Tokenizer { query: src };

    let expected = vec![
        Token {
            item: TokenItem::Keyword(Keyword::Run),
            pos: Pos { line: 1, column: 1 },
        },
        Token {
            item: TokenItem::Whitespace,
            pos: Pos { line: 1, column: 4 },
        },
        Token {
            item: TokenItem::LParen,
            pos: Pos { line: 1, column: 5 },
        },
        Token {
            item: TokenItem::Literal(Literal::Id("x".to_string())),
            pos: Pos { line: 1, column: 6 },
        },
        Token {
            item: TokenItem::QuestionMark,
            pos: Pos { line: 1, column: 7 },
        },
        Token {
            item: TokenItem::Literal(Literal::Id("z".to_string())),
            pos: Pos { line: 1, column: 8 },
        },
        Token {
            item: TokenItem::Whitespace,
            pos: Pos { line: 1, column: 9 },
        },
        Token {
            item: TokenItem::Eq,
            pos: Pos {
                line: 1,
                column: 10,
            },
        },
        Token {
            item: TokenItem::Whitespace,
            pos: Pos {
                line: 1,
                column: 11,
            },
        },
        Token {
            item: TokenItem::Literal(Literal::Id("print".to_string())),
            pos: Pos {
                line: 1,
                column: 12,
            },
        },
        Token {
            item: TokenItem::ExclamationMark,
            pos: Pos {
                line: 1,
                column: 17,
            },
        },
        Token {
            item: TokenItem::Literal(Literal::String("Got it!".to_string())),
            pos: Pos {
                line: 1,
                column: 18,
            },
        },
        Token {
            item: TokenItem::Whitespace,
            pos: Pos {
                line: 1,
                column: 27,
            },
        },
        Token {
            item: TokenItem::Pipe,
            pos: Pos {
                line: 1,
                column: 28,
            },
        },
        Token {
            item: TokenItem::Whitespace,
            pos: Pos {
                line: 1,
                column: 29,
            },
        },
        Token {
            item: TokenItem::Literal(Literal::Id("x".to_string())),
            pos: Pos {
                line: 1,
                column: 30,
            },
        },
        Token {
            item: TokenItem::ExclamationMark,
            pos: Pos {
                line: 1,
                column: 31,
            },
        },
        Token {
            item: TokenItem::Literal(Literal::Id("y".to_string())),
            pos: Pos {
                line: 1,
                column: 32,
            },
        },
        Token {
            item: TokenItem::RParen,
            pos: Pos {
                line: 1,
                column: 33,
            },
        },
        Token {
            item: TokenItem::EOF,
            pos: Pos {
                line: 1,
                column: 34,
            },
        },
    ];

    let tokens = tokenizer.tokenize().unwrap();

    assert_eq!(expected, tokens);
}
