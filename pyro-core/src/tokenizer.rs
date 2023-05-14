#[cfg(test)]
mod tests;

use crate::sym::{Keyword, Literal, Sym};
use std::fmt;
use std::iter::Peekable;
use std::str::Chars;

/// Tokenizer error
#[derive(Debug, PartialEq, Eq)]
pub struct TokenizerError {
    pub message: String,
    pub pos: Pos,
}

impl fmt::Display for TokenizerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} at Line: {}, Column {}",
            self.message, self.pos.line, self.pos.column
        )
    }
}

impl std::error::Error for TokenizerError {}

/// Location in input string
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct Pos {
    /// Line number, starting from 1
    pub line: u64,
    /// Line column, starting from 1
    pub column: u64,
}

struct State<'a> {
    peekable: Peekable<Chars<'a>>,
    pub line: u64,
    pub col: u64,
}

impl<'a> State<'a> {
    pub fn next(&mut self) -> Option<char> {
        match self.peekable.next() {
            None => None,
            Some(s) => {
                if s == '\n' {
                    self.line += 1;
                    self.col = 1;
                } else {
                    self.col += 1;
                }
                Some(s)
            }
        }
    }

    pub fn peek(&mut self) -> Option<&char> {
        self.peekable.peek()
    }

    pub fn pos(&self) -> Pos {
        Pos {
            line: self.line,
            column: self.col,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    pub item: Sym,
    pub pos: Pos,
}

pub struct Tokenizer<'a> {
    query: &'a str,
}

impl<'a> Tokenizer<'a> {
    pub fn new(query: &'a str) -> Self {
        Self { query }
    }

    /// Tokenize the statement and produce a vector of tokens with location information
    pub fn tokenize(&self) -> Result<Vec<Token>, TokenizerError> {
        let mut state = State {
            peekable: self.query.chars().peekable(),
            line: 1,
            col: 1,
        };

        let mut tokens: Vec<Token> = vec![];
        let mut pos = state.pos();

        while let Some(item) = self.next_token_item(&mut state)? {
            tokens.push(Token { item, pos });

            pos = state.pos();
        }

        tokens.push(Token {
            item: Sym::EOF,
            pos,
        });

        Ok(tokens)
    }

    fn next_token_item(&self, chars: &mut State) -> Result<Option<Sym>, TokenizerError> {
        match chars.peek() {
            None => Ok(None),
            Some(ch) => match ch {
                ' ' | '\t' => {
                    chars.next();
                    while let Some(' ' | '\t') = chars.peek() {
                        chars.next();
                    }

                    Ok(Some(Sym::Whitespace))
                }

                '\n' => {
                    chars.next();
                    while let Some('\n') = chars.peek() {
                        chars.next();
                    }

                    Ok(Some(Sym::Newline))
                }

                '?' => self.consume(chars, Sym::QuestionMark),
                '.' => self.consume(chars, Sym::Dot),
                '^' => self.consume(chars, Sym::Caret),
                ',' => self.consume(chars, Sym::Comma),
                ':' => self.consume(chars, Sym::Colon),
                '[' => self.consume(chars, Sym::LBracket),
                ']' => self.consume(chars, Sym::RBracket),
                '(' => self.consume(chars, Sym::LParen),
                ')' => self.consume(chars, Sym::RParen),
                '@' => self.consume(chars, Sym::At),
                '_' => self.consume(chars, Sym::Underscore),
                '+' => self.consume(chars, Sym::Plus),
                '-' => self.consume(chars, Sym::Minus),
                '*' => self.consume(chars, Sym::Mul),
                '/' => self.consume(chars, Sym::Div),
                '%' => self.consume(chars, Sym::Mod),
                '|' => self.consume(chars, Sym::Pipe),

                '!' => {
                    chars.next();
                    if let Some('=') = chars.peek() {
                        return self.consume(chars, Sym::Neq);
                    }

                    Ok(Some(Sym::ExclamationMark))
                }

                '>' => {
                    chars.next();
                    if let Some('=') = chars.peek() {
                        return self.consume(chars, Sym::GtEq);
                    }

                    Ok(Some(Sym::Gt))
                }

                '<' => {
                    chars.next();
                    if let Some('=') = chars.peek() {
                        return self.consume(chars, Sym::LtEq);
                    }

                    Ok(Some(Sym::Lt))
                }

                '"' => {
                    let mut string = String::new();

                    chars.next();

                    while let Some(ch) = chars.peek() {
                        if *ch == '"' {
                            return self.consume(chars, Sym::Literal(Literal::String(string)));
                        }

                        if *ch == '\n' {
                            return Err(TokenizerError {
                                message: "String literal is malformed".to_string(),
                                pos: chars.pos(),
                            });
                        }

                        string.push(*ch);
                        chars.next();
                    }

                    Err(TokenizerError {
                        message: "Incomplete string literal".to_string(),
                        pos: chars.pos(),
                    })
                }

                '=' => {
                    chars.next();
                    if let Some('=') = chars.peek() {
                        return self.consume(chars, Sym::DoubleEq);
                    }

                    Ok(Some(Sym::Eq))
                }

                _ if ch.is_ascii_lowercase() && ch.is_ascii_alphabetic() => {
                    let mut ident = String::new();

                    ident.push(*ch);
                    chars.next();

                    while let Some(ch) = chars.peek() {
                        if !ch.is_ascii_alphanumeric() {
                            break;
                        }

                        ident.push(*ch);
                        chars.next();
                    }

                    match ident.as_str() {
                        "true" => Ok(Some(Sym::Literal(Literal::Bool(true)))),
                        "false" => Ok(Some(Sym::Literal(Literal::Bool(false)))),
                        "if" => Ok(Some(Sym::Keyword(Keyword::If))),
                        "then" => Ok(Some(Sym::Keyword(Keyword::Then))),
                        "else" => Ok(Some(Sym::Keyword(Keyword::Else))),
                        "def" => Ok(Some(Sym::Keyword(Keyword::Def))),
                        "run" => Ok(Some(Sym::Keyword(Keyword::Run))),
                        "new" => Ok(Some(Sym::Keyword(Keyword::New))),
                        "and" => Ok(Some(Sym::Keyword(Keyword::And))),
                        "or" => Ok(Some(Sym::Keyword(Keyword::Or))),
                        "type" => Ok(Some(Sym::Keyword(Keyword::Or))),
                        _ => Ok(Some(Sym::Id(ident))),
                    }
                }

                // Start of a type
                _ if ch.is_ascii_uppercase() && ch.is_ascii_alphabetic() => {
                    let mut ident = String::new();

                    ident.push(*ch);
                    chars.next();
                    while let Some(ch) = chars.peek() {
                        if !ch.is_ascii_alphanumeric() {
                            break;
                        }

                        ident.push(*ch);
                        chars.next();
                    }

                    Ok(Some(Sym::Type(ident)))
                }

                _ => Err(TokenizerError {
                    message: format!("Unexpected symbol '{}'", ch),
                    pos: chars.pos(),
                }),
            },
        }
    }

    fn consume(&self, chars: &mut State, item: Sym) -> Result<Option<Sym>, TokenizerError> {
        chars.next();
        Ok(Some(item))
    }
}
