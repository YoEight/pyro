use crate::ast::{Abs, Proc, Program, Tag, Val};
use crate::sym::{Keyword, Sym};
use crate::tokenizer::{Pos, Token};
use std::collections::VecDeque;
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

        if start.item != Sym::Keyword(Keyword::Run) {
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
            _ if start_like_val(token) => {
                let lhs = state.shift();
                let lhs = self.parse_value(state, lhs);

                token = state.look_ahead();

                match token.item {
                    Sym::ExclamationMark => {
                        state.shift();
                        let rhs = state.shift();
                        let rhs = self.parse_value(state, rhs);

                        Proc::Output(lhs, rhs)
                    }

                    Sym::QuestionMark => {
                        state.shift();
                        let rhs = state.shift();
                        let rhs = self.parse_abs(state, rhs);

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

            _ => panic!(),
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

    fn parse_abs(&self, state: &mut ParserState, token: &Token) -> Tag<Abs, Pos> {
        todo!()
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
