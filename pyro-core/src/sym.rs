use std::fmt::{Display, Formatter};

use crate::ast::Type;

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    /// An unsigned numeric literal
    Number(u64),
    /// Double quoted string: i.e: "string"
    String(String),
    Char(char),
    Bool(bool),
}

impl Literal {
    pub fn typematch(&self, r#type: &Type) -> bool {
        match (self, r#type) {
            (Literal::Number(_), Type::Name(name)) => name == "Int",
            (Literal::String(_), Type::Name(name)) => name == "String",
            (Literal::Char(_), Type::Name(name)) => name == "Char",
            (Literal::Bool(_), Type::Name(name)) => name == "Bool",
            (_, Type::Anonymous) => true,
            _ => false,
        }
    }

    pub fn r#type(&self) -> Type {
        match self {
            Literal::Number(_) => Type::Name("Int".to_string()),
            Literal::String(_) => Type::Name("String".to_string()),
            Literal::Char(_) => Type::Name("Char".to_string()),
            Literal::Bool(_) => Type::Name("Bool".to_string()),
        }
    }
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Literal::Number(num) => num.to_string(),
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
    /// `@`
    At,
    /// `_`
    Underscore,
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
            Sym::Newline => "\\n",
            Sym::DoubleEq => "==",
            Sym::Eq => "=",
            Sym::Neq => "!=",
            Sym::Lt => "<",
            Sym::Gt => ">",
            Sym::LtEq => "<=",
            Sym::GtEq => ">=",
            Sym::Plus => "+",
            Sym::Minus => "-",
            Sym::Mul => "*",
            Sym::Div => "/",
            Sym::Mod => "%",
            Sym::At => "@",
            Sym::Underscore => "_",
        };

        write!(f, "{}", str)
    }
}
