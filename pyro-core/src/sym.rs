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
    /// An unsigned numeric literal
    Number(String, bool),
    /// Double quoted string: i.e: "string"
    String(String),
    Char(char),
    Bool(bool),
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
