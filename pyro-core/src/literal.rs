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
