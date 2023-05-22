#[cfg(test)]
mod tests;

use crate::ast::{Abs, Decl, Def, Pat, PatVar, Proc, Program, Prop, Record, Tag, Type, Val, Var};
use crate::sym::{Keyword, Punctuation, Sym};
use crate::tokenizer::{Pos, Token};
use std::collections::VecDeque;
use std::iter::Peekable;
use std::slice::Iter;

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

pub trait RecordKind {
    type Value;

    fn parse_value(&self, state: &mut ParserState) -> Result<Self::Value>;
}

pub struct RecordValue;
pub struct RecordType;
pub struct RecordPat;

impl RecordKind for RecordValue {
    type Value = Val;

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
    type Value = Pat;

    fn parse_value(&self, state: &mut ParserState) -> Result<Self::Value> {
        state.parse_pat()
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

    pub fn skip_newline(&mut self) {
        if let Sym::Newline = &self.look_ahead().item {
            self.shift();
            return;
        }
    }

    pub fn skip_spaces(&mut self) {
        loop {
            match self.look_ahead().item() {
                Sym::Whitespace | Sym::Newline => {
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
                self.skip_whitespace();
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
            self.skip_whitespace();
            self.parse_type()
        } else {
            Ok(Type::Anonymous)
        }
    }

    fn parse_type(&mut self) -> Result<Type> {
        enum Constr {
            InOut,
        }

        let mut constrs = Vec::new();
        let mut r#type = loop {
            let token = self.look_ahead();

            match token.item() {
                Sym::Type(s) => {
                    self.shift();
                    break Type::Name(s.clone());
                }

                Sym::Punctuation(p) if p == &Punctuation::Caret || p == &Punctuation::LBracket => {
                    if p == &Punctuation::Caret {
                        self.shift();
                        constrs.push(Constr::InOut);
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
                Constr::InOut => r#type = Type::Channel(Box::new(r#type)),
            }
        }

        return Ok(r#type);
    }

    pub fn parse_value(&mut self) -> Result<Tag<Val, Pos>> {
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

            _ => Err(Error {
                pos: token.pos,
                message: format!("Expected parsing a value but got {} instead", token.item),
            }),
        }
    }

    pub fn parse_path(&mut self, start: Pos, head: String) -> Result<Tag<Val, Pos>> {
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

    pub fn parse_record<K>(&mut self, kind: K) -> Result<Record<K::Value>>
    where
        K: RecordKind,
    {
        self.expect_punctuation(Punctuation::LBracket)?;
        self.skip_whitespace();

        let mut props = Vec::new();

        loop {
            if self.next_punct(Punctuation::RBracket) {
                self.shift();
                break;
            }

            let label = self.parse_type_label()?;
            let value = kind.parse_value(self)?;

            self.skip_whitespace();
            props.push(Prop { label, val: value });
        }

        Ok(Record { props })
    }

    pub fn parse_record_type(&mut self) -> Result<Type> {
        Ok(Type::Record(self.parse_record(RecordType)?))
    }

    pub fn parse_record_value(&mut self) -> Result<Tag<Val, Pos>> {
        let pos = self.pos();
        let record = self.parse_record(RecordValue)?;

        Ok(Tag {
            item: Val::Record(record),
            tag: pos,
        })
    }

    pub fn parse_record_pat(&mut self) -> Result<Record<Pat>> {
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

                self.skip_whitespace();
                token = self.look_ahead();

                match token.item {
                    Sym::Punctuation(p)
                        if p == Punctuation::ExclamationMark || p == Punctuation::QuestionMark =>
                    {
                        if p == Punctuation::ExclamationMark {
                            self.shift();
                            self.skip_whitespace();

                            Ok(Proc::Output(lhs, self.parse_value()?))
                        } else {
                            self.shift();
                            self.skip_whitespace();

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
                self.skip_whitespace();

                if self.next_punct(Punctuation::RParen) {
                    self.shift();

                    Ok(Proc::Null)
                } else if start_like_decl(self.look_ahead()) {
                    self.parse_local_decl()
                } else {
                    self.parse_parallel_composition()
                }
            }

            _ => Err(Error {
                pos: token.pos,
                message: format!("Unexpected token '{}'", token.item()),
            }),
        }
    }

    pub fn parse_abs(&mut self) -> Result<Tag<Abs<Pos>, Pos>> {
        let pos = self.pos();
        let pattern = self.parse_pat()?;

        self.skip_whitespace();
        self.expect(Sym::Eq)?;
        self.skip_whitespace();

        let proc = self.parse_proc()?;

        Ok(Tag {
            item: Abs {
                pattern,
                proc: Box::new(proc),
            },
            tag: pos,
        })
    }

    pub fn parse_pat(&mut self) -> Result<Pat> {
        let token = self.look_ahead();

        match token.item() {
            Sym::Id(_) => {
                let var = self.parse_variable()?;
                self.skip_whitespace();

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
                self.skip_whitespace();

                Ok(Pat::Wildcard(self.parse_type()?))
            }

            _ => Err(Error {
                pos: token.pos,
                message: format!("Unexpected token when parsing a pattern '{}'", token.item()),
            }),
        }
    }

    fn parse_local_decl(&mut self) -> Result<Proc<Pos>> {
        let decl = self.parse_decl()?;

        self.skip_spaces();

        let pos = self.pos();
        let proc = self.parse_proc()?;

        self.expect_punctuation(Punctuation::RParen)?;

        Ok(Proc::Decl(
            decl,
            Tag {
                item: Box::new(proc),
                tag: pos,
            },
        ))
    }

    fn parse_decl(&mut self) -> Result<Decl<Pos>> {
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
        let mut processes = VecDeque::new();

        processes.push_back(self.parse_proc()?);

        self.skip_whitespace();
        let mut first_time = true;

        loop {
            if self.next_punct(Punctuation::RParen) && !first_time {
                self.shift();
                break;
            }

            if self.next_punct(Punctuation::Pipe) {
                self.shift();
                self.skip_whitespace();
                processes.push_back(self.parse_proc()?);
                self.skip_whitespace();
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
        self.skip_whitespace();

        let mut defs = VecDeque::new();

        loop {
            let id = self.parse_id()?;
            self.skip_whitespace();
            let abs = self.parse_abs()?;
            self.skip_spaces();

            defs.push_back(Def {
                name: id,
                abs: abs.item,
            });

            if !self.next_keyword(Keyword::And) {
                break;
            }

            self.shift();
            self.skip_whitespace();
        }

        Ok(Decl::Def(defs))
    }

    fn parse_type_decl(&mut self) -> Result<Decl<Pos>> {
        self.expect_keyword(Keyword::Type)?;
        self.skip_whitespace();

        let id = self.parse_type_id()?;
        self.skip_whitespace();
        self.expect(Sym::Eq)?;
        self.skip_whitespace();

        let r#type = self.parse_type()?;

        Ok(Decl::Type(id, r#type))
    }

    fn parse_new_channel(&mut self) -> Result<Decl<Pos>> {
        self.expect_keyword(Keyword::New)?;
        self.skip_whitespace();

        let id = self.parse_id()?;
        self.skip_whitespace();
        self.expect_punctuation(Punctuation::Colon)?;
        self.skip_whitespace();
        let r#type = self.parse_type()?;

        Ok(Decl::Channel(id, r#type))
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

        state.expect_keyword(Keyword::Run)?;
        state.skip_whitespace();

        let mut procs = VecDeque::new();

        loop {
            let pos = state.pos();

            procs.push_back(Tag {
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
