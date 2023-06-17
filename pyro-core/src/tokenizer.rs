#[cfg(test)]
mod tests;

use crate::sym::{Keyword, Literal, Punctuation, Sym};
use crate::{Error, Pos, Result};
use std::iter::Peekable;
use std::str::Chars;

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

impl Token {
    pub fn item(&self) -> &Sym {
        &self.item
    }
}

pub struct Tokenizer<'a> {
    query: &'a str,
}

impl<'a> Tokenizer<'a> {
    pub fn new(query: &'a str) -> Self {
        Self { query }
    }

    /// Tokenize the statement and produce a vector of tokens with location information
    pub fn tokenize(&self) -> Result<Vec<Token>> {
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

    fn next_token_item(&self, chars: &mut State) -> Result<Option<Sym>> {
        match chars.peek() {
            None => Ok(None),
            Some(ch) => match ch {
                _ if ch.is_ascii_whitespace() => {
                    chars.next();
                    while let Some(c) = chars.peek() {
                        if !c.is_whitespace() {
                            break;
                        }

                        chars.next();
                    }

                    Ok(Some(Sym::Whitespace))
                }

                '\'' => {
                    let char = if let Some(c) = chars.next() {
                        c
                    } else {
                        return Err(Error {
                            message: "Expected a char but reached the end of the file".to_string(),
                            pos: chars.pos(),
                        });
                    };

                    let pos = chars.pos();
                    let end = chars.next();

                    if let Some(end) = end {
                        if end != '\'' {
                            return Err(Error {
                                message: format!(
                                    "Expected ' when parsing a char but got '{}' instead",
                                    end
                                ),
                                pos,
                            });
                        }
                    } else {
                        return Err(Error {
                            message: "Expected ' but reached the end of the file".to_string(),
                            pos: chars.pos(),
                        });
                    }

                    Ok(Some(Sym::Literal(Literal::Char(char))))
                }

                '?' => self.consume(chars, Sym::Punctuation(Punctuation::QuestionMark)),
                '.' => self.consume(chars, Sym::Punctuation(Punctuation::Dot)),
                '^' => self.consume(chars, Sym::Punctuation(Punctuation::Caret)),
                ',' => self.consume(chars, Sym::Punctuation(Punctuation::Comma)),
                ':' => self.consume(chars, Sym::Punctuation(Punctuation::Colon)),
                '[' => self.consume(chars, Sym::Punctuation(Punctuation::LBracket)),
                ']' => self.consume(chars, Sym::Punctuation(Punctuation::RBracket)),
                '(' => self.consume(chars, Sym::Punctuation(Punctuation::LParen)),
                ')' => self.consume(chars, Sym::Punctuation(Punctuation::RParen)),
                '@' => self.consume(chars, Sym::At),
                '_' => self.consume(chars, Sym::Underscore),
                '\\' => self.consume(chars, Sym::BackSlash),
                '|' => self.consume(chars, Sym::Punctuation(Punctuation::Pipe)),

                '!' => {
                    chars.next();

                    if let Some('=') = chars.peek() {
                        return self.consume(chars, Sym::Id("!=".to_string()));
                    }

                    Ok(Some(Sym::Punctuation(Punctuation::ExclamationMark)))
                }

                '<' => {
                    chars.next();
                    if let Some('=') = chars.peek() {
                        return self.consume(chars, Sym::Id("<=".to_string()));
                    }

                    Ok(Some(Sym::Id("<".to_string())))
                }

                '>' => {
                    chars.next();
                    if let Some('=') = chars.peek() {
                        return self.consume(chars, Sym::Id(">=".to_string()));
                    }

                    Ok(Some(Sym::Id(">".to_string())))
                }

                '"' => {
                    let mut string = String::new();

                    chars.next();

                    while let Some(ch) = chars.peek() {
                        if *ch == '"' {
                            return self.consume(chars, Sym::Literal(Literal::String(string)));
                        }

                        if *ch == '\n' {
                            return Err(Error {
                                message: "String literal is malformed".to_string(),
                                pos: chars.pos(),
                            });
                        }

                        string.push(*ch);
                        chars.next();
                    }

                    Err(Error {
                        message: "Incomplete string literal".to_string(),
                        pos: chars.pos(),
                    })
                }

                '+' => {
                    chars.next();
                    Ok(Some(Sym::Id("+".to_string())))
                }

                '-' => {
                    chars.next();
                    Ok(Some(Sym::Id("-".to_string())))
                }

                '=' => {
                    chars.next();
                    if let Some('=') = chars.peek() {
                        return self.consume(chars, Sym::Id("==".to_string()));
                    }

                    Ok(Some(Sym::Eq))
                }

                _ if ch.is_ascii_lowercase() => {
                    let mut ident = String::new();

                    ident.push(*ch);
                    chars.next();

                    while let Some(ch) = chars.peek() {
                        if !ch.is_ascii_alphanumeric() && *ch != '_' {
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
                        "type" => Ok(Some(Sym::Keyword(Keyword::Type))),
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

                _ if ch.is_ascii_digit() => {
                    let mut num = String::new();

                    num.push(*ch);
                    chars.next();

                    while let Some(ch) = chars.peek() {
                        if !ch.is_ascii_digit() {
                            break;
                        }

                        num.push(*ch);
                        chars.next();
                    }

                    let num = match num.parse::<i64>() {
                        Ok(num) => num,
                        Err(e) => {
                            return Err(Error {
                                message: format!("Error when parsing natural number: {}", e),
                                pos: chars.pos(),
                            })
                        }
                    };

                    Ok(Some(Sym::Literal(Literal::Integer(num))))
                }

                _ => Err(Error {
                    message: format!("Unexpected symbol '{}'", ch),
                    pos: chars.pos(),
                }),
            },
        }
    }

    fn consume(&self, chars: &mut State, item: Sym) -> Result<Option<Sym>> {
        chars.next();
        Ok(Some(item))
    }
}
