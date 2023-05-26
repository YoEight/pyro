use crate::annotate::{annotate_program, Ann};
use ast::{Proc, Tag};
use parser::Parser;
use tokenizer::{Token, Tokenizer};

pub mod annotate;
pub mod ast;
pub mod parser;
pub mod sym;
pub mod tokenizer;

pub fn tokenize(src: &str) -> eyre::Result<Vec<Token>> {
    let tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize()?;

    Ok(tokens)
}

pub fn parse(src: &str) -> eyre::Result<Vec<Tag<Proc<Ann>, Ann>>> {
    let tokens = tokenize(src)?;
    let parser = Parser::new(tokens.as_slice());
    let program = annotate_program(parser.parse()?);

    Ok(program.procs)
}
