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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    /// An unsigned numeric literal
    Number(String, bool),
    /// Double quoted string: i.e: "string"
    String(String),
    Char(char),
    Bool(bool),
}

impl Display for Literal {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Literal::Number(s, sign) => {
                let sign = if *sign { "-" } else { "" };
                format!("{}{}", sign, s)
            }

            Literal::String(s) => format!("\"{}\"", s),
            Literal::Char(c) => format!("'{}'", c),
            Literal::Bool(b) => b.to_string(),
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

impl Display for Sym {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Sym::EOF => "EOF",
            Sym::Type(t) => t.as_str(),
            Sym::Literal(l) => return write!(f, "{}", l),
            Sym::Id(i) => i.as_str(),
            Sym::Keyword(k) => k.as_str(),
            Sym::Comma => ",",
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
            Sym::LParen => "(",
            Sym::RParen => ")",
            Sym::Colon => ":",
            Sym::LBracket => "[",
            Sym::RBracket => "]",
            Sym::Pipe => "|",
            Sym::Caret => "^",
            Sym::ExclamationMark => "!",
            Sym::QuestionMark => "?",
            Sym::Dot => ".",
            Sym::At => "@",
            Sym::Underscore => "_",
        };

        write!(f, "{}", str)
    }
}
