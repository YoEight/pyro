#[cfg(test)]
mod tests;

use crate::ast::{Abs, Pat, Proc, Program, Tag, Type, Val, Var};
use crate::sym::{Keyword, Sym};
use crate::tokenizer::{Pos, Token};
use std::collections::VecDeque;
use std::iter::Peekable;
use std::slice::Iter;

pub struct ParserState<'a> {
    peekable: Peekable<Iter<'a, Token>>,
}

impl<'a> ParserState<'a> {
    pub fn look_ahead(&mut self) -> &'a Token {
        self.peekable.peek().unwrap()
    }

    pub fn shift(&mut self) -> &'a Token {
        self.peekable.next().unwrap()
    }

    pub fn skip_whitespace(&mut self) {
        if let Sym::Whitespace = &self.look_ahead().item {
            self.shift();
            return;
        }
    }
}

pub struct Parser<'a> {
    tokens: &'a [Token],
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self { tokens }
    }

    pub fn parse(&self) -> Program<Pos> {
        let mut state = ParserState {
            peekable: self.tokens.iter().peekable(),
        };

        let mut start = state.shift();

        if start.item != Sym::Keyword(Keyword::Run) {
            panic!("{:?}: must start with run", start.pos);
        }

        state.skip_whitespace();

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
            _ if start_like_val(token) => {
                let lhs = state.shift();
                let lhs = self.parse_value(state, lhs);

                state.skip_whitespace();
                token = state.look_ahead();

                match token.item {
                    Sym::ExclamationMark => {
                        state.shift();
                        state.skip_whitespace();

                        let rhs = state.shift();
                        let rhs = self.parse_value(state, rhs);

                        Proc::Output(lhs, rhs)
                    }

                    Sym::QuestionMark => {
                        state.shift();
                        state.skip_whitespace();

                        let rhs = self.parse_abs(state);

                        Proc::Input(lhs, rhs)
                    }
                    _ => panic!(),
                }
            }
            _ => unimplemented!(),
        }
    }

    fn parse_output(&self, state: &mut ParserState, lhs: &Token) -> Proc<Pos> {
        todo!()
    }

    fn parse_input(&self, state: &mut ParserState, lhs: &Token) -> Proc<Pos> {
        todo!()
    }

    fn parse_value(&self, state: &mut ParserState, token: &Token) -> Tag<Val, Pos> {
        match &token.item {
            Sym::Literal(lit) => Tag {
                item: Val::Literal(lit.clone()),
                tag: token.pos,
            },

            Sym::Id(head) => self.parse_path(state, token.pos, head.clone()),

            _ => panic!("{:?} Expecting parsing a value", token.pos),
        }
    }

    fn parse_path(&self, state: &mut ParserState, start: Pos, head: String) -> Tag<Val, Pos> {
        let mut path = VecDeque::new();

        path.push_back(head);

        loop {
            let mut token = state.look_ahead();

            if token.item != Sym::Dot {
                break;
            }

            state.shift();
            token = state.shift();

            match &token.item {
                Sym::Id(id) => path.push_back(id.clone()),
                _ => panic!("{:?}: Expecting an identifier", token.pos),
            }
        }

        Tag {
            item: Val::Path(path),
            tag: start,
        }
    }

    fn parse_abs(&self, state: &mut ParserState) -> Tag<Abs<Pos>, Pos> {
        let mut token = state.look_ahead();

        match token.item() {
            Sym::Id(_) => {
                let var = self.parse_variable(state);
                let pat = Pat::Var(var);

                state.skip_whitespace();
                let sep = state.shift();

                if sep.item() != &Sym::Eq {
                    panic!("{:?}: Expecting '='", sep.pos);
                }

                state.skip_whitespace();
                let proc = self.parse_proc(state);

                Tag {
                    item: Abs {
                        pattern: pat,
                        proc: Box::new(proc),
                    },
                    tag: token.pos,
                }
            }
            _ => todo!(),
        }
    }

    fn parse_variable(&self, state: &mut ParserState) -> Var {
        let mut token = state.shift();
        match &token.item {
            Sym::Id(name) => {
                state.skip_whitespace();
                let r#type = self.parse_type(state);

                Var {
                    id: name.clone(),
                    r#type,
                }
            }

            _ => panic!("{:?}: Expecting an identifier", token.pos),
        }
    }

    fn parse_type(&self, state: &mut ParserState) -> Type {
        let mut token = state.look_ahead();

        match &token.item {
            Sym::Colon => {
                state.shift();
                state.skip_whitespace();

                token = state.look_ahead();
                let is_channel = if let Sym::Caret = token.item() {
                    state.shift();
                    true
                } else {
                    false
                };

                token = state.look_ahead();
                match token.item() {
                    Sym::Type(name) => {
                        if is_channel {
                            Type::Channel(name.clone())
                        } else {
                            Type::Name(name.clone())
                        }
                    }

                    Sym::LBracket => {
                        state.shift();
                        self.parse_record_type(state)
                    }

                    _ => panic!("{:?}: Expected a type", token.pos),
                }
            }

            _ => Type::Anonymous,
        }
    }

    fn parse_record_type(&self, state: &mut ParserState) -> Type {
        unimplemented!();
    }
}

fn start_like_val(token: &Token) -> bool {
    match &token.item {
        Sym::Literal(_) | Sym::LBracket => true,
        _ => start_like_path(token),
    }
}

fn start_like_path(token: &Token) -> bool {
    if let Sym::Id(_) = &token.item {
        return true;
    }

    false
}
