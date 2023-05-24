use std::collections::VecDeque;

use ast::{Proc, Tag};
use parser::Parser;
use tokenizer::{Pos, Token, Tokenizer};

// pub mod annotate;
pub mod ast;
pub mod parser;
pub mod sym;
pub mod tokenizer;

pub fn tokenize(src: &str) -> eyre::Result<Vec<Token>> {
    let tokenizer = Tokenizer::new(src);
    let tokens = tokenizer.tokenize()?;

    Ok(tokens)
}

pub fn parse(src: &str) -> eyre::Result<VecDeque<Tag<Proc<Pos>, Pos>>> {
    let tokens = tokenize(src)?;
    let parser = Parser::new(tokens.as_slice());
    let program = parser.parse()?;

    Ok(program.procs)
}
