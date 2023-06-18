#[cfg(test)]
mod tests;

use crate::ast::{Abs, Decl, Def, Pat, PatVar, Proc, Program, Prop, Record, Tag, Type, Val, Var};
use crate::sym::{Keyword, Punctuation, Sym};
use crate::tokenizer::Token;
use crate::utils::generate_generic_type_name;
use crate::{Error, Pos, Result};
use std::iter::Peekable;
use std::slice::Iter;

pub trait RecordKind {
    type Value;

    fn parse_value(&self, state: &mut ParserState) -> Result<Self::Value>;
}

pub struct RecordValue;
pub struct RecordType;
pub struct RecordPat;

impl RecordKind for RecordValue {
    type Value = Val<Pos>;

    fn parse_value(&self, state: &mut ParserState) -> Result<Self::Value> {
        let tag = state.parse_value()?;
        Ok(tag.item)
    }
}

impl RecordKind for RecordType {
    type Value = Type;

    fn parse_value(&self, state: &mut ParserState) -> Result<Self::Value> {
        state.parse_type()
    }
}

impl RecordKind for RecordPat {
    type Value = Pat<Pos>;

    fn parse_value(&self, state: &mut ParserState) -> Result<Self::Value> {
        state.parse_pat()
    }
}

#[derive(Clone)]
pub struct ParserState<'a> {
    peekable: Peekable<Iter<'a, Token>>,
    gen_type_id: usize,
}

impl<'a> ParserState<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            peekable: tokens.iter().peekable(),
            gen_type_id: 0,
        }
    }

    pub fn next_type_id(&mut self) -> usize {
        let value = self.gen_type_id;
        self.gen_type_id += 1;

        value
    }

    pub fn look_ahead(&mut self) -> &'a Token {
        self.peekable.peek().unwrap()
    }

    pub fn shift(&mut self) -> &'a Token {
        self.peekable.next().unwrap()
    }

    pub fn skip_spaces(&mut self) {
        loop {
            match self.look_ahead().item() {
                Sym::Whitespace => {
                    self.shift();
                }

                _ => break,
            }
        }
    }

    pub fn next_keyword(&mut self, key: Keyword) -> bool {
        self.next_sym(Sym::Keyword(key))
    }

    fn expect(&mut self, expected: Sym) -> Result<()> {
        let pos = self.pos();
        let token = self.shift();

        if &expected != token.item() {
            return Err(Error {
                pos,
                message: format!("Expected '{}' but got '{}' instead", expected, token.item()),
            });
        }

        Ok(())
    }

    fn expect_keyword(&mut self, expected: Keyword) -> Result<()> {
        self.expect(Sym::Keyword(expected))
    }

    fn expect_punctuation(&mut self, expected: Punctuation) -> Result<()> {
        self.expect(Sym::Punctuation(expected))
    }

    pub fn followed_by<F>(&mut self, fun: F) -> bool
    where
        F: FnOnce(&Sym) -> bool,
    {
        fun(self.look_ahead().item())
    }

    pub fn followed_by_keyword(&mut self, keyword: Keyword) -> bool {
        self.followed_by(|sym| sym == &Sym::Keyword(keyword))
    }

    pub fn next_sym(&mut self, sym: Sym) -> bool {
        self.followed_by(|s| s == &sym)
    }

    pub fn next_punct(&mut self, expected: Punctuation) -> bool {
        self.next_sym(Sym::Punctuation(expected))
    }

    pub fn parse_variable(&mut self) -> Result<Var> {
        let token = self.shift();
        match &token.item {
            Sym::Id(name) => {
                self.skip_spaces();
                let r#type = self.parse_rtype()?;

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

    pub fn parse_rtype(&mut self) -> Result<Type> {
        if self.next_punct(Punctuation::Colon) {
            self.shift();
            self.skip_spaces();
            self.parse_type()
        } else {
            Ok(Type::generic(generate_generic_type_name(
                self.next_type_id(),
            )))
        }
    }

    fn parse_type(&mut self) -> Result<Type> {
        enum Constr {
            InOut,
            In,
            Out,
        }

        let mut constrs = Vec::new();
        let mut r#type = loop {
            let token = self.look_ahead();

            match token.item() {
                Sym::Type(s) => {
                    self.shift();
                    break Type::named(s);
                }

                Sym::Punctuation(p)
                    if p.communication_category() || p == &Punctuation::LBracket =>
                {
                    if p == &Punctuation::Caret {
                        self.shift();
                        constrs.push(Constr::InOut);
                    } else if p == &Punctuation::ExclamationMark {
                        self.shift();
                        constrs.push(Constr::Out);
                    } else if p == &Punctuation::QuestionMark {
                        self.shift();
                        constrs.push(Constr::In);
                    } else {
                        break self.parse_record_type()?;
                    }
                }

                _ => {
                    return Err(Error {
                        pos: token.pos,
                        message: format!(
                            "Expected an identifier but got '{:?} instead",
                            token.item()
                        ),
                    })
                }
            }
        };

        while let Some(constr) = constrs.pop() {
            match constr {
                Constr::InOut => r#type = Type::channel(r#type),
                Constr::In => r#type = Type::server(r#type),
                Constr::Out => r#type = Type::client(r#type),
            }
        }

        return Ok(r#type);
    }

    pub fn parse_value(&mut self) -> Result<Tag<Val<Pos>, Pos>> {
        let token = self.look_ahead();

        match token.item() {
            Sym::Literal(lit) => {
                self.shift();

                Ok(Tag {
                    item: Val::Literal(lit.clone()),
                    tag: token.pos,
                })
            }

            Sym::Id(head) => {
                self.shift();
                self.parse_path(token.pos, head.clone())
            }

            Sym::Punctuation(Punctuation::LBracket) => self.parse_record_value(),

            Sym::BackSlash => {
                self.shift();
                self.skip_spaces();

                let pos = self.pos();
                let abs = self.parse_abs()?;
                Ok(Tag {
                    item: Val::AnoClient(abs),
                    tag: pos,
                })
            }

            Sym::Punctuation(Punctuation::LParen) => {
                let pos = self.pos();
                self.shift();
                self.skip_spaces();
                let mut caller = self.parse_value()?;
                self.skip_spaces();
                let mut params = self.parse_value()?;
                loop {
                    self.skip_spaces();
                    let chk = self.clone();

                    match self.parse_value() {
                        Err(_) => {
                            *self = chk;
                            break;
                        }

                        Ok(value) => {
                            caller = Tag {
                                tag: pos,
                                item: Val::App(Box::new(caller), Box::new(params)),
                            };

                            params = value;
                        }
                    }
                }

                self.expect_punctuation(Punctuation::RParen)?;

                Ok(Tag {
                    item: Val::App(Box::new(caller), Box::new(params)),
                    tag: pos,
                })
            }

            _ => Err(Error {
                pos: token.pos,
                message: format!("Expected parsing a value but got {} instead", token.item),
            }),
        }
    }

    pub fn parse_path(&mut self, start: Pos, head: String) -> Result<Tag<Val<Pos>, Pos>> {
        let mut path = Vec::new();

        path.push(head);

        loop {
            if !self.next_punct(Punctuation::Dot) {
                break;
            }

            self.shift();
            let token = self.shift();

            match &token.item {
                Sym::Id(id) => path.push(id.clone()),
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

    pub fn parse_record<K>(&mut self, kind: K) -> Result<Record<Tag<K::Value, Pos>>>
    where
        K: RecordKind,
    {
        self.expect_punctuation(Punctuation::LBracket)?;
        self.skip_spaces();

        let mut props = Vec::new();

        loop {
            if self.next_punct(Punctuation::RBracket) {
                self.shift();
                break;
            }

            let label = self.parse_type_label()?;
            let pos = self.pos();
            let value = kind.parse_value(self)?;

            self.skip_spaces();
            props.push(Prop {
                label,
                val: Tag {
                    item: value,
                    tag: pos,
                },
            });
        }

        Ok(Record { props })
    }

    pub fn parse_record_type(&mut self) -> Result<Type> {
        Ok(Type::Record(self.parse_record(RecordType)?.map(|p| p.item)))
    }

    pub fn parse_record_value(&mut self) -> Result<Tag<Val<Pos>, Pos>> {
        let pos = self.pos();
        let record = self.parse_record(RecordValue)?;

        Ok(Tag {
            item: Val::Record(record),
            tag: pos,
        })
    }

    pub fn parse_record_pat(&mut self) -> Result<Record<Tag<Pat<Pos>, Pos>>> {
        self.parse_record(RecordPat)
    }

    fn parse_type_label(&mut self) -> Result<Option<String>> {
        let chk = self.clone();
        let token = self.shift();

        match token.item() {
            Sym::Id(id) => {
                if self.next_sym(Sym::Eq) {
                    self.shift();
                    return Ok(Some(id.clone()));
                }

                *self = chk;
                Ok(None)
            }

            _ => {
                *self = chk;
                Ok(None)
            }
        }
    }

    pub fn parse_proc(&mut self) -> Result<Proc<Pos>> {
        let mut token = self.look_ahead();

        match token.item {
            _ if start_like_val(token) => {
                let lhs = self.parse_value()?;

                self.skip_spaces();
                token = self.look_ahead();

                match token.item {
                    Sym::Punctuation(p)
                        if p == Punctuation::ExclamationMark || p == Punctuation::QuestionMark =>
                    {
                        if p == Punctuation::ExclamationMark {
                            self.shift();
                            self.skip_spaces();

                            Ok(Proc::Output(lhs, self.parse_value()?))
                        } else {
                            self.shift();
                            self.skip_spaces();

                            Ok(Proc::Input(lhs, self.parse_abs()?))
                        }
                    }

                    _ => {
                        return Err(Error {
                            pos: token.pos,
                            message: format!("Unexpected token '{}'", token.item()),
                        })
                    }
                }
            }

            Sym::Punctuation(Punctuation::LParen) => {
                self.shift();
                self.skip_spaces();

                if self.next_punct(Punctuation::RParen) {
                    self.shift();

                    Ok(Proc::Null)
                } else if start_like_decl(self.look_ahead()) {
                    self.parse_local_decl()
                } else {
                    self.parse_parallel_composition()
                }
            }

            Sym::Keyword(Keyword::If) => self.parse_cond(),

            _ => Err(Error {
                pos: token.pos,
                message: format!("Unexpected token '{}'", token.item()),
            }),
        }
    }

    pub fn parse_abs(&mut self) -> Result<Tag<Abs<Pos>, Pos>> {
        let pos = self.pos();
        let pattern = self.parse_pat()?;

        self.skip_spaces();
        self.expect(Sym::Eq)?;
        self.skip_spaces();

        let proc_pos = self.pos();
        let proc = self.parse_proc()?;

        Ok(Tag {
            item: Abs {
                pattern: Tag {
                    item: pattern,
                    tag: pos,
                },
                proc: Box::new(Tag {
                    item: proc,
                    tag: proc_pos,
                }),
            },
            tag: pos,
        })
    }

    pub fn parse_pat(&mut self) -> Result<Pat<Pos>> {
        let token = self.look_ahead();

        match token.item() {
            Sym::Id(_) => {
                let var = self.parse_variable()?;
                self.skip_spaces();

                let pattern = if self.next_sym(Sym::At) {
                    self.shift();
                    Some(Box::new(self.parse_pat()?))
                } else {
                    None
                };

                Ok(Pat::Var(PatVar { var, pattern }))
            }

            Sym::Punctuation(Punctuation::LBracket) => Ok(Pat::Record(self.parse_record_pat()?)),

            Sym::Underscore => {
                self.shift();
                self.skip_spaces();

                Ok(Pat::Wildcard(self.parse_type()?))
            }

            _ => Err(Error {
                pos: token.pos,
                message: format!("Unexpected token when parsing a pattern '{}'", token.item()),
            }),
        }
    }

    fn parse_local_decl(&mut self) -> Result<Proc<Pos>> {
        let decl_pos = self.pos();
        let decl = self.parse_decl()?;

        self.skip_spaces();

        let pos = self.pos();
        let proc = self.parse_proc()?;

        self.skip_spaces();
        self.expect_punctuation(Punctuation::RParen)?;

        Ok(Proc::Decl(
            Tag {
                item: decl,
                tag: decl_pos,
            },
            Box::new(Tag {
                item: proc,
                tag: pos,
            }),
        ))
    }

    pub fn parse_decl(&mut self) -> Result<Decl<Pos>> {
        let token = self.look_ahead();

        match token.item() {
            Sym::Keyword(key) => match key {
                Keyword::New => self.parse_new_channel(),
                Keyword::Def => self.parse_def(),
                Keyword::Type => self.parse_type_decl(),
                _ => Err(Error {
                    pos: token.pos,
                    message: format!("Unexpected keyword '{}'", key),
                }),
            },

            _ => Err(Error {
                pos: token.pos,
                message: format!("Expecting a declaration but got '{}' instead", token.item),
            }),
        }
    }

    fn parse_parallel_composition(&mut self) -> Result<Proc<Pos>> {
        let mut processes = Vec::new();

        let pos = self.pos();
        processes.push(Tag {
            item: self.parse_proc()?,
            tag: pos,
        });

        self.skip_spaces();
        let mut first_time = true;

        loop {
            if self.next_punct(Punctuation::RParen) && !first_time {
                self.shift();
                break;
            }

            if self.next_punct(Punctuation::Pipe) {
                self.shift();
                self.skip_spaces();
                let pos = self.pos();
                processes.push(Tag {
                    item: self.parse_proc()?,
                    tag: pos,
                });
                self.skip_spaces();
                first_time = false;
                continue;
            }

            return Err(Error {
                pos: self.pos(),
                message: format!(
                    "Expected a '|' to separate processes but got '{}' instead",
                    self.look_ahead().item()
                ),
            });
        }

        Ok(Proc::Parallel(processes))
    }

    fn parse_def(&mut self) -> Result<Decl<Pos>> {
        self.expect_keyword(Keyword::Def)?;
        self.skip_spaces();

        let mut defs = Vec::new();

        loop {
            let pos = self.pos();
            let id = self.parse_id()?;
            self.skip_spaces();
            let abs = self.parse_abs()?;
            self.skip_spaces();

            defs.push(Tag {
                item: Def { name: id, abs },
                tag: pos,
            });

            if !self.next_keyword(Keyword::And) {
                break;
            }

            self.shift();
            self.skip_spaces();
        }

        Ok(Decl::Def(defs))
    }

    fn parse_type_decl(&mut self) -> Result<Decl<Pos>> {
        self.expect_keyword(Keyword::Type)?;
        self.skip_spaces();

        let id = self.parse_type_id()?;
        self.skip_spaces();
        self.expect(Sym::Eq)?;
        self.skip_spaces();

        let r#type = self.parse_type()?;

        Ok(Decl::Type(id, r#type))
    }

    fn parse_new_channel(&mut self) -> Result<Decl<Pos>> {
        let mut chans = Vec::new();

        loop {
            self.skip_spaces();
            if !chans.is_empty() && !self.followed_by_keyword(Keyword::New) {
                break;
            }

            self.expect_keyword(Keyword::New)?;
            self.skip_spaces();

            let pos = self.pos();
            let id = self.parse_id()?;
            self.skip_spaces();
            self.expect_punctuation(Punctuation::Colon)?;
            self.skip_spaces();
            let r#type = self.parse_type()?;

            chans.push(Tag {
                item: (id, r#type),
                tag: pos,
            });
        }

        Ok(Decl::Channels(chans))
    }

    fn parse_id(&mut self) -> Result<String> {
        let token = self.shift();

        if let Sym::Id(s) = token.item() {
            return Ok(s.clone());
        }

        Err(Error {
            pos: token.pos,
            message: format!(
                "Expected an identifier but got '{:?}' instead",
                token.item()
            ),
        })
    }

    fn parse_type_id(&mut self) -> Result<String> {
        let token = self.shift();

        if let Sym::Type(s) = token.item() {
            return Ok(s.clone());
        }

        Err(Error {
            pos: token.pos,
            message: format!(
                "Expected an identifier but got '{:?}' instead",
                token.item()
            ),
        })
    }

    pub fn pos(&mut self) -> Pos {
        self.look_ahead().pos
    }

    fn parse_cond(&mut self) -> Result<Proc<Pos>> {
        self.expect_keyword(Keyword::If)?;
        self.skip_spaces();

        let val = self.parse_value()?;

        self.skip_spaces();
        self.expect_keyword(Keyword::Then)?;
        self.skip_spaces();

        let if_pos = self.pos();
        let if_proc = self.parse_proc()?;

        self.skip_spaces();
        self.expect_keyword(Keyword::Else)?;
        self.skip_spaces();

        let else_pos = self.pos();
        let else_proc = self.parse_proc()?;

        Ok(Proc::Cond(
            val,
            Box::new(Tag {
                item: if_proc,
                tag: if_pos,
            }),
            Box::new(Tag {
                item: else_proc,
                tag: else_pos,
            }),
        ))
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
        let mut state = ParserState::new(self.tokens);

        state.expect_keyword(Keyword::Run)?;
        state.skip_spaces();

        let mut procs = Vec::new();

        loop {
            let pos = state.pos();

            procs.push(Tag {
                item: state.parse_proc()?,
                tag: pos,
            });

            state.skip_spaces();
            if !state.next_keyword(Keyword::New) {
                break;
            }

            state.shift();
        }

        state.skip_spaces();
        state.expect(Sym::EOF)?;

        Ok(Program { procs })
    }
}

fn start_like_val(token: &Token) -> bool {
    match &token.item {
        Sym::Literal(_) | Sym::Punctuation(Punctuation::LBracket) => true,
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
