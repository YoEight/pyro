#[cfg(test)]
mod tests;

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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Keyword {
    New,
    Def,
    If,
    Then,
    Else,
    Run,
    And,
    Or,
    Type,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    // Identifier
    Id(String),
    /// An unsigned numeric literal
    Number(String, bool),
    /// Double quoted string: i.e: "string"
    String(String),
    Char(char),
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    item: TokenItem,
    pos: Pos,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenItem {
    /// An end-of-file marker, not a real token
    EOF,
    // Type
    Type(String),
    Literal(Literal),
    /// A keyword
    Keyword(Keyword),
    /// Comma
    Comma,
    /// Whitespace (space, tab, etc)
    Whitespace,
    /// `\n`
    Newline,
    /// Double equals sign `==`
    DoubleEq,
    /// Equality operator `=`
    Eq,
    /// Not Equals operator `!=`
    Neq,
    /// Less Than operator `<`
    Lt,
    /// Greater Than operator `>`
    Gt,
    /// Less Than Or Equals operator `<=`
    LtEq,
    /// Greater Than Or Equals operator `>=`
    GtEq,
    /// Plus operator `+`
    Plus,
    /// Minus operator `-`
    Minus,
    /// Multiplication operator `*`
    Mul,
    /// Division operator `/`
    Div,
    /// Modulo Operator `%`
    Mod,
    /// Left parenthesis `(`
    LParen,
    /// Right parenthesis `)`
    RParen,
    /// Colon `:`
    Colon,
    /// Left bracket `[`
    LBracket,
    /// Right bracket `]`
    RBracket,
    /// Pipe `|`
    Pipe,
    /// Caret `^`
    Caret,
    /// Exclamation Mark `!`
    ExclamationMark,
    /// Question Mark `?`
    QuestionMark,
    /// `.`
    Dot,
    /// `@`
    At,
    /// `_`
    Underscore,
}

pub struct Tokenizer<'a> {
    query: &'a str,
}

impl<'a> Tokenizer<'a> {
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
            item: TokenItem::EOF,
            pos,
        });

        Ok(tokens)
    }

    fn next_token_item(&self, chars: &mut State) -> Result<Option<TokenItem>, TokenizerError> {
        match chars.peek() {
            None => Ok(None),
            Some(ch) => match ch {
                ' ' | '\t' => {
                    chars.next();
                    while let Some(' ' | '\t') = chars.peek() {
                        chars.next();
                    }

                    Ok(Some(TokenItem::Whitespace))
                }

                '\n' => {
                    chars.next();
                    while let Some('\n') = chars.peek() {
                        chars.next();
                    }

                    Ok(Some(TokenItem::Newline))
                }

                '?' => self.consume(chars, TokenItem::QuestionMark),
                '.' => self.consume(chars, TokenItem::Dot),
                '^' => self.consume(chars, TokenItem::Caret),
                ',' => self.consume(chars, TokenItem::Comma),
                ':' => self.consume(chars, TokenItem::Colon),
                '[' => self.consume(chars, TokenItem::LBracket),
                ']' => self.consume(chars, TokenItem::RBracket),
                '(' => self.consume(chars, TokenItem::LParen),
                ')' => self.consume(chars, TokenItem::RParen),
                '@' => self.consume(chars, TokenItem::At),
                '_' => self.consume(chars, TokenItem::Underscore),
                '+' => self.consume(chars, TokenItem::Plus),
                '-' => self.consume(chars, TokenItem::Minus),
                '*' => self.consume(chars, TokenItem::Mul),
                '/' => self.consume(chars, TokenItem::Div),
                '%' => self.consume(chars, TokenItem::Mod),
                '|' => self.consume(chars, TokenItem::Pipe),

                '!' => {
                    chars.next();
                    if let Some('=') = chars.peek() {
                        return self.consume(chars, TokenItem::Neq);
                    }

                    Ok(Some(TokenItem::ExclamationMark))
                }

                '>' => {
                    chars.next();
                    if let Some('=') = chars.peek() {
                        return self.consume(chars, TokenItem::GtEq);
                    }

                    Ok(Some(TokenItem::Gt))
                }

                '<' => {
                    chars.next();
                    if let Some('=') = chars.peek() {
                        return self.consume(chars, TokenItem::LtEq);
                    }

                    Ok(Some(TokenItem::Lt))
                }

                '"' => {
                    let mut string = String::new();

                    chars.next();

                    while let Some(ch) = chars.peek() {
                        if *ch == '"' {
                            return self
                                .consume(chars, TokenItem::Literal(Literal::String(string)));
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
                        return self.consume(chars, TokenItem::DoubleEq);
                    }

                    Ok(Some(TokenItem::Eq))
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
                        "true" => Ok(Some(TokenItem::Literal(Literal::Bool(true)))),
                        "false" => Ok(Some(TokenItem::Literal(Literal::Bool(false)))),
                        "if" => Ok(Some(TokenItem::Keyword(Keyword::If))),
                        "then" => Ok(Some(TokenItem::Keyword(Keyword::Then))),
                        "else" => Ok(Some(TokenItem::Keyword(Keyword::Else))),
                        "def" => Ok(Some(TokenItem::Keyword(Keyword::Def))),
                        "run" => Ok(Some(TokenItem::Keyword(Keyword::Run))),
                        "new" => Ok(Some(TokenItem::Keyword(Keyword::New))),
                        "and" => Ok(Some(TokenItem::Keyword(Keyword::And))),
                        "or" => Ok(Some(TokenItem::Keyword(Keyword::Or))),
                        "type" => Ok(Some(TokenItem::Keyword(Keyword::Or))),
                        _ => Ok(Some(TokenItem::Literal(Literal::Id(ident)))),
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

                    Ok(Some(TokenItem::Type(ident)))
                }

                _ => Err(TokenizerError {
                    message: format!("Unexpected symbol '{}'", ch),
                    pos: chars.pos(),
                }),
            },
        }
    }

    fn consume(
        &self,
        chars: &mut State,
        item: TokenItem,
    ) -> Result<Option<TokenItem>, TokenizerError> {
        chars.next();
        Ok(Some(item))
    }
}
