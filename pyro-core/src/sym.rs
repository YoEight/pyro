use crate::ast::Record;
use std::fmt::{Display, Formatter};

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

impl Keyword {
    pub fn as_str(&self) -> &'static str {
        match self {
            Keyword::New => "new",
            Keyword::Def => "def",
            Keyword::If => "if",
            Keyword::Then => "then",
            Keyword::Else => "else",
            Keyword::Run => "run",
            Keyword::And => "and",
            Keyword::Or => "or",
            Keyword::Type => "type",
        }
    }
}

impl Display for Keyword {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.as_str())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TypeSym {
    Name(String),
    App(Box<TypeSym>, Box<TypeSym>),
    Rec(Record<TypeSym>),
    Unknown,
}

impl Display for TypeSym {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeSym::Name(n) => write!(f, "{}", n),
            TypeSym::Unknown => write!(f, "_"),
            TypeSym::App(c, inner) => {
                write!(f, "(")?;
                c.fmt(f)?;
                write!(f, " ")?;
                inner.fmt(f)?;
                write!(f, ")")
            }
            TypeSym::Rec(rec) => rec.fmt(f),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    /// An unsigned numeric literal
    Integer(i64),
    /// Double quoted string: i.e: "string"
    String(String),
    Char(char),
    Bool(bool),
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Literal::Integer(num) => num.to_string(),
            Literal::String(s) => format!("\"{}\"", s),
            Literal::Char(c) => format!("'{}'", c),
            Literal::Bool(b) => b.to_string(),
        };

        write!(f, "{}", str)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Punctuation {
    /// '.'
    Dot,
    /// ':'
    Colon,
    /// Left parenthesis `)`
    LParen,
    /// Right parenthesis `)`
    RParen,
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
    /// ','
    Comma,
}

impl Punctuation {
    pub fn communication_category(&self) -> bool {
        match self {
            Punctuation::Caret | Punctuation::QuestionMark | Punctuation::ExclamationMark => true,
            _ => false,
        }
    }
}

impl Display for Punctuation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Punctuation::Dot => ".",
            Punctuation::Colon => ":",
            Punctuation::LParen => "(",
            Punctuation::RParen => ")",
            Punctuation::LBracket => "[",
            Punctuation::RBracket => "]",
            Punctuation::Pipe => "|",
            Punctuation::Caret => "^",
            Punctuation::ExclamationMark => "!",
            Punctuation::QuestionMark => "?",
            Punctuation::Comma => ",",
        };

        write!(f, "{}", str)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Sym {
    /// An end-of-file marker, not a real token
    EOF,
    // Type
    Type(String),
    Literal(Literal),
    // Identifier
    Id(String),
    Punctuation(Punctuation),
    /// A keyword
    Keyword(Keyword),
    /// Whitespace (space, tab, etc)
    Whitespace,
    /// Equality operator `=`
    Eq,
    /// `@`
    At,
    /// `_`
    Underscore,
    /// `\`
    BackSlash,
}

impl Display for Sym {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Sym::EOF => "EOF",
            Sym::Type(t) => t.as_str(),
            Sym::Literal(l) => return write!(f, "{}", l),
            Sym::Punctuation(p) => return write!(f, "{}", p),
            Sym::Id(i) => i.as_str(),
            Sym::Keyword(k) => k.as_str(),
            Sym::Whitespace => " ",
            Sym::Eq => "=",
            Sym::At => "@",
            Sym::Underscore => "_",
            Sym::BackSlash => "\\",
        };

        write!(f, "{}", str)
    }
}
