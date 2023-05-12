use std::fmt;
use std::iter::Peekable;
use std::str::Chars;

/// Tokenizer error
#[derive(Debug, PartialEq, Eq)]
pub struct TokenizerError {
    pub message: String,
    pub line: u64,
    pub col: u64,
}

impl fmt::Display for TokenizerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} at Line: {}, Column {}",
            self.message, self.line, self.col
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
pub enum TokenItem{
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
    /// Not Equals operator `<>` (or `!=` in some dialects)
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
    /// `&&`
    DoubleAmpersand,
    /// Pipe `|`
    Pipe,
    /// Caret `^`
    Caret,
    /// Exclamation Mark `!`
    ExclamationMark,
    /// Question Mark `?`
    QuestionMark,
}

pub struct Tokenizer<'a> {
    query: &'a str,
}

impl<'a> Tokenizer<'a> {
    /// Tokenize the statement and produce a vector of tokens with location information
    pub fn tokenize(&mut self) -> Result<Vec<Token>, TokenizerError> {
        let mut state = State {
            peekable: self.query.chars().peekable(),
            line: 1,
            col: 1,
        };

        let mut tokens: Vec<Token> = vec![];

        let mut pos = state.pos();
        while let Some(item) = self.next_token_item(&mut state)? {
            tokens.push(Token {
                item,
                pos,
            });

            pos = state.pos();
        }
        Ok(tokens)
    }

    fn next_token_item(&self, chars: &mut State) -> Result<Option<TokenItem>, TokenizerError> {
        match chars.peek() {
            None => Ok(Some(TokenItem::EOF)),
            Some(ch) => match ch {
                ' ' | '\t'  => {
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

                // Start of an identifier
                _ if ch.is_ascii_lowercase() && ch.is_ascii_alphabetic() => {
                    let mut ident = String::new();

                    ident.push(*ch);
                    while let Some(ch) = chars.peek() {
                        if ch.is_ascii_alphanumeric() {
                            ident.push(*ch);
                            chars.next();
                        }

                        break;
                    }

                    Ok(Some(TokenItem::Literal(Literal::Id(ident))))
                }

                // Start of a type
                _ if ch.is_ascii_uppercase() && ch.is_ascii_alphabetic() => {
                    let mut ident = String::new();

                    ident.push(*ch);
                    while let Some(ch) = chars.peek() {
                        if ch.is_ascii_alphanumeric() {
                            ident.push(*ch);
                            chars.next();
                        }

                        break;
                    }

                    Ok(Some(TokenItem::Type(ident)))
                }

                _ => {
                    todo!()
                }
            }
        }
    }

    fn consume(&self, chars: &mut State, item: TokenItem) -> Result<Option<TokenItem>, TokenizerError> {
        chars.next();
        Ok(Some(item))
    }
}