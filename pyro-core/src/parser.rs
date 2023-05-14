use crate::ast::{Proc, Program, Tag};
use crate::tokenizer::{Keyword, Pos, Token, TokenItem};
use std::iter::Peekable;
use std::slice::Iter;

pub struct ParserState<'a> {
    peekable: Peekable<Iter<'a, Token>>,
}

impl<'a> ParserState<'a> {
    pub fn look_ahead(&mut self) -> &Token {
        self.peekable.peek().unwrap()
    }

    pub fn shift(&mut self) -> &'a Token {
        self.peekable.next().unwrap()
    }
}

pub struct Parser<'a> {
    tokens: &'a [Token],
}

impl<'a> Parser<'a> {
    pub fn parse(&self) -> Program<Pos> {
        let mut state = ParserState {
            peekable: self.tokens.iter().peekable(),
        };

        let mut start = state.shift();

        if start.item != TokenItem::Keyword(Keyword::Run) {
            panic!("{:?}: must start with run", start.pos);
        }

        let proc = self.parse_proc(&mut state);

        Program {
            proc: Tag {
                item: proc,
                tag: start.pos,
            },
        }
    }

    fn parse_proc(&self, state: &mut ParserState) -> Proc<Pos> {
        let mut token = state.look_ahead();

        match token.item {
            TokenItem::Literal(_) => {
                let lhs = state.shift();
                token = state.look_ahead();

                match token.item {
                    TokenItem::ExclamationMark => {
                        state.shift();
                        self.parse_output(state, lhs)
                    }
                    TokenItem::QuestionMark => {
                        state.shift();
                        self.parse_input(state, lhs)
                    }
                    _ => panic!(),
                }
            }
            _ => panic!(),
        }
    }

    fn parse_output(&self, state: &mut ParserState, lhs: &Token) -> Proc<Pos> {
        todo!()
    }

    fn parse_input(&self, state: &mut ParserState, lhs: &Token) -> Proc<Pos> {
        todo!()
    }
}
