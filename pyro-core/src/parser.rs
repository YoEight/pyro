#[cfg(test)]
mod tests;

use crate::ast::{Abs, Pat, Proc, Program, Prop, Record, Tag, Type, Val, Var};
use crate::sym::{Keyword, Sym};
use crate::tokenizer::{Pos, Token};
use std::collections::VecDeque;
use std::iter::Peekable;
use std::slice::Iter;

#[derive(Clone, Debug)]
pub struct Error {
    pos: Pos,
    message: String,
}

pub type Result<A> = std::result::Result<A, Error>;

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}: {}", self.pos.line, self.pos.column, self.message)
    }
}

#[derive(Clone)]
pub struct ParserState<'a> {
    peekable: Peekable<Iter<'a, Token>>,
}

impl<'a> ParserState<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            peekable: tokens.iter().peekable(),
        }
    }

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

    pub fn pos(&mut self) -> Pos {
        self.look_ahead().pos
    }
}

pub struct Parser<'a> {
    tokens: &'a [Token],
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self { tokens }
    }

    pub fn parse(&self) -> Result<Program<Pos>> {
        let mut state = ParserState {
            peekable: self.tokens.iter().peekable(),
        };

        let start = state.shift();

        if start.item != Sym::Keyword(Keyword::Run) {
            return Err(Error {
                pos: start.pos,
                message: "Program must start with run keyword".to_string(),
            });
        }

        state.skip_whitespace();

        let proc = self.parse_proc(&mut state)?;

        Ok(Program {
            proc: Tag {
                item: proc,
                tag: start.pos,
            },
        })
    }

    pub fn parse_proc(&self, state: &mut ParserState) -> Result<Proc<Pos>> {
        let mut token = state.look_ahead();

        match token.item {
            _ if start_like_val(token) => {
                let lhs = self.parse_value(state)?;

                state.skip_whitespace();
                token = state.look_ahead();

                match token.item {
                    Sym::ExclamationMark => {
                        state.shift();
                        state.skip_whitespace();

                        let rhs = self.parse_value(state)?;

                        Ok(Proc::Output(lhs, rhs))
                    }

                    Sym::QuestionMark => {
                        state.shift();
                        state.skip_whitespace();

                        let rhs = self.parse_abs(state)?;

                        Ok(Proc::Input(lhs, rhs))
                    }
                    _ => {
                        return Err(Error {
                            pos: token.pos,
                            message: format!("Unexpected token '{}'", token.item()),
                        })
                    }
                }
            }

            Sym::LParen => {
                state.shift();
                state.skip_whitespace();

                let token = state.look_ahead();
                if token.item() == &Sym::RParen {
                    state.shift();

                    Ok(Proc::Null)
                } else if start_like_decl(token) {
                    Ok(self.parse_local_decl(state))
                } else {
                    self.parse_parallel_composition(state)
                }
            }

            _ => unimplemented!(),
        }
    }

    pub fn parse_value(&self, state: &mut ParserState) -> Result<Tag<Val, Pos>> {
        let token = state.shift();

        match token.item() {
            Sym::Literal(lit) => Ok(Tag {
                item: Val::Literal(lit.clone()),
                tag: token.pos,
            }),

            Sym::Id(head) => self.parse_path(state, token.pos, head.clone()),

            Sym::RBracket => self.parse_record_value(state),

            _ => Err(Error {
                pos: token.pos,
                message: format!("Expected parsing a value but got {} instead", token.item),
            }),
        }
    }

    pub fn parse_path(
        &self,
        state: &mut ParserState,
        start: Pos,
        head: String,
    ) -> Result<Tag<Val, Pos>> {
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
                _ => {
                    return Err(Error {
                        pos: token.pos,
                        message: format!("Expected an identifier but got {} instead", token.item),
                    });
                }
            }
        }

        Ok(Tag {
            item: Val::Path(path),
            tag: start,
        })
    }

    pub fn parse_abs(&self, state: &mut ParserState) -> Result<Tag<Abs<Pos>, Pos>> {
        let token = state.look_ahead();

        match token.item() {
            Sym::Id(_) => {
                let var = self.parse_variable(state)?;
                let pat = Pat::Var(var);

                state.skip_whitespace();
                let sep = state.shift();

                if sep.item() != &Sym::Eq {
                    return Err(Error {
                        pos: sep.pos,
                        message: format!("Expecting '=' but got '{}' instead", sep.item()),
                    });
                }

                state.skip_whitespace();
                let proc = self.parse_proc(state)?;

                Ok(Tag {
                    item: Abs {
                        pattern: pat,
                        proc: Box::new(proc),
                    },
                    tag: token.pos,
                })
            }
            _ => unimplemented!(),
        }
    }

    pub fn parse_variable(&self, state: &mut ParserState) -> Result<Var> {
        let token = state.shift();
        match &token.item {
            Sym::Id(name) => {
                state.skip_whitespace();
                let r#type = self.parse_rtype(state)?;

                Ok(Var {
                    id: name.clone(),
                    r#type,
                })
            }

            _ => Err(Error {
                pos: token.pos,
                message: format!("Expecting an identifier but got {} instead", token.item()),
            }),
        }
    }

    pub fn parse_rtype(&self, state: &mut ParserState) -> Result<Type> {
        let token = state.look_ahead();
        match token.item() {
            Sym::Colon => {
                state.shift();
                state.skip_whitespace();

                self.parse_type(state)
            }

            _ => Ok(Type::Anonymous),
        }
    }

    pub fn parse_type(&self, state: &mut ParserState) -> Result<Type> {
        let mut token = state.look_ahead();

        let is_channel = if let Sym::Caret = token.item() {
            state.shift();
            true
        } else {
            false
        };

        token = state.look_ahead();
        match token.item() {
            Sym::Type(name) => {
                state.shift();

                if is_channel {
                    Ok(Type::Channel(name.clone()))
                } else {
                    Ok(Type::Name(name.clone()))
                }
            }

            Sym::LBracket => self.parse_record_type(state),

            _ => Err(Error {
                pos: token.pos,
                message: format!("expected a type but got {}", token.item),
            }),
        }
    }

    pub fn parse_record_type(&self, state: &mut ParserState) -> Result<Type> {
        let mut token = state.shift();

        if token.item() != &Sym::LBracket {
            return Err(Error {
                pos: token.pos,
                message: format!(
                    "Expected '[' to parse a record but got {} instead",
                    token.item()
                ),
            });
        }

        state.skip_whitespace();

        token = state.look_ahead();
        let mut props = VecDeque::new();

        loop {
            if token.item() == &Sym::RBracket {
                state.shift();
                break;
            }

            let label = self.parse_type_label(state)?;
            let r#type = self.parse_type(state)?;

            state.skip_whitespace();
            props.push_back(Prop { label, val: r#type });
            token = state.look_ahead();
        }

        Ok(Type::Record(Record { props }))
    }

    fn parse_local_decl(&self, state: &mut ParserState) -> Proc<Pos> {
        todo!()
    }

    fn parse_parallel_composition(&self, state: &mut ParserState) -> Result<Proc<Pos>> {
        let mut processes = VecDeque::new();

        processes.push_back(self.parse_proc(state)?);

        state.skip_whitespace();
        let mut first_time = true;
        let mut token = state.look_ahead();

        loop {
            match token.item {
                Sym::RParen if !first_time => {
                    state.shift();
                    break;
                }

                Sym::Pipe => {
                    state.shift();
                    state.skip_whitespace();
                    processes.push_back(self.parse_proc(state)?);
                    state.skip_whitespace();
                    first_time = false;
                    token = state.look_ahead();
                }

                _ => {
                    panic!("{:?}: Expecting a '|' to separate processes", token.pos);
                }
            }
        }

        Ok(Proc::Parallel(processes))
    }

    fn parse_type_label(&self, state: &mut ParserState) -> Result<Option<String>> {
        let mut token = state.look_ahead();

        match token.item() {
            Sym::Id(id) => {
                state.shift();
                token = state.shift();

                if token.item() != &Sym::Eq {
                    return Err(Error {
                        pos: token.pos,
                        message: format!(
                            "Expected '=' when parsing a record label property but got {} instead",
                            token.item()
                        ),
                    });
                }

                Ok(Some(id.clone()))
            }

            _ => Ok(None),
        }
    }

    fn parse_record_value(&self, state: &mut ParserState) -> Result<Tag<Val, Pos>> {
        let pos = state.pos();
        let mut token = state.shift();

        if token.item() != &Sym::LBracket {
            return Err(Error {
                pos: token.pos,
                message: format!(
                    "Expected '[' to parse a record but got {} instead",
                    token.item
                ),
            });
        }

        state.skip_whitespace();

        token = state.look_ahead();
        let mut props = VecDeque::new();

        loop {
            if token.item() == &Sym::RBracket {
                state.shift();
                break;
            }

            let chk_state = state.clone();
            token = state.shift();

            let label = match token.item() {
                Sym::Id(id) => {
                    token = state.shift();

                    if token.item() == &Sym::Eq {
                        Some(id.clone())
                    } else {
                        *state = chk_state;
                        None
                    }
                }

                _ => {
                    *state = chk_state;
                    None
                }
            };

            let tag = self.parse_value(state)?;

            state.skip_whitespace();
            props.push_back(Prop {
                label,
                val: tag.item,
            });

            token = state.look_ahead();
        }

        Ok(Tag {
            item: Val::Record(Record { props }),
            tag: pos,
        })
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

fn start_like_decl(token: &Token) -> bool {
    match token.item() {
        Sym::Keyword(word) => {
            word == &Keyword::New || word == &Keyword::Def || word == &Keyword::Type
        }
        _ => false,
    }
}
