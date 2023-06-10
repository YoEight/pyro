use crate::annotate::{annotate_program, Ann};
use ast::{Proc, Tag};
use parser::Parser;
use tokenizer::{Token, Tokenizer};

pub mod annotate;
pub mod ast;
mod context;
pub mod parser;
pub mod sym;
pub mod tokenizer;

pub use context::{Ctx, STDLIB};

/// Location in input string
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct Pos {
    /// Line number, starting from 1
    pub line: u64,
    /// Line column, starting from 1
    pub column: u64,
}

#[derive(Clone, Debug)]
pub struct Error {
    pos: Pos,
    message: String,
}

impl std::error::Error for Error {}

pub type Result<A> = std::result::Result<A, Error>;

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}: {}", self.pos.line, self.pos.column, self.message)
    }
}
pub fn tokenize(src: &str) -> eyre::Result<Vec<Token>> {
    let tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize()?;

    Ok(tokens)
}

pub fn parse(ctx: Ctx, src: &str) -> eyre::Result<Vec<Tag<Proc<Ann>, Ann>>> {
    let tokens = tokenize(src)?;
    let parser = Parser::new(tokens.as_slice());
    let program = annotate_program(ctx, parser.parse()?)?;

    Ok(program.procs)
}
