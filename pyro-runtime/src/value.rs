use futures::future::BoxFuture;
use pyro_core::annotate::Ann;
use pyro_core::ast::{Abs, Record, Tag};
use pyro_core::sym::Literal;
use std::fmt::{Display, Formatter};
use std::sync::Arc;
use tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender};
use tokio::sync::{mpsc, Mutex};

use crate::helpers::{Declared, TypeBuilder};

pub trait PyroType {
    fn r#type(builder: TypeBuilder) -> Declared;
}

impl PyroType for i64 {
    fn r#type(builder: TypeBuilder) -> Declared {
        builder.of("Integer")
    }
}

impl PyroType for char {
    fn r#type(builder: TypeBuilder) -> Declared {
        builder.of("Char")
    }
}

impl PyroType for bool {
    fn r#type(builder: TypeBuilder) -> Declared {
        builder.of("Bool")
    }
}

impl PyroType for String {
    fn r#type(builder: TypeBuilder) -> Declared {
        builder.of("String")
    }
}

pub trait PyroValue: PyroType {
    fn deserialize(value: Arc<RuntimeValue>) -> eyre::Result<Self>
    where
        Self: Sized;

    fn serialize(self) -> eyre::Result<RuntimeValue>;
}

impl PyroValue for i64 {
    fn deserialize(value: Arc<RuntimeValue>) -> eyre::Result<Self>
    where
        Self: Sized,
    {
        if let RuntimeValue::Literal(Literal::Integer(v)) = value.as_ref() {
            return Ok(*v);
        }

        eyre::bail!("Expected an u64 runtime value")
    }

    fn serialize(self) -> eyre::Result<RuntimeValue> {
        Ok(RuntimeValue::Literal(Literal::Integer(self)))
    }
}

impl PyroValue for char {
    fn deserialize(value: Arc<RuntimeValue>) -> eyre::Result<Self>
    where
        Self: Sized,
    {
        if let RuntimeValue::Literal(Literal::Char(v)) = value.as_ref() {
            return Ok(*v);
        }

        eyre::bail!("Expected a char runtime value")
    }

    fn serialize(self) -> eyre::Result<RuntimeValue> {
        Ok(RuntimeValue::Literal(Literal::Char(self)))
    }
}

impl PyroValue for String {
    fn deserialize(value: Arc<RuntimeValue>) -> eyre::Result<Self>
    where
        Self: Sized,
    {
        if let RuntimeValue::Literal(Literal::String(v)) = value.as_ref() {
            return Ok(v.clone());
        }

        eyre::bail!("Expected a char runtime value")
    }

    fn serialize(self) -> eyre::Result<RuntimeValue> {
        Ok(RuntimeValue::Literal(Literal::String(self)))
    }
}

impl PyroValue for bool {
    fn deserialize(value: Arc<RuntimeValue>) -> eyre::Result<Self>
    where
        Self: Sized,
    {
        if let RuntimeValue::Literal(Literal::Bool(v)) = value.as_ref() {
            return Ok(*v);
        }

        eyre::bail!("Expected a bool runtime value")
    }

    fn serialize(self) -> eyre::Result<RuntimeValue> {
        Ok(RuntimeValue::Literal(Literal::Bool(self)))
    }
}

pub type Suspension = Arc<
    dyn Fn(RuntimeValue) -> BoxFuture<'static, eyre::Result<RuntimeValue>> + Send + Sync + 'static,
>;

#[derive(Clone)]
pub enum RuntimeValue {
    Channel(Channel),
    Literal(Literal),
    Record(Record<RuntimeValue>),
    Abs(Tag<Abs<Ann>, Ann>),
    Fun(Suspension),
}

impl Display for RuntimeValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeValue::Channel(c) => write!(f, "{}", c),
            RuntimeValue::Literal(l) => write!(f, "{}", l),
            RuntimeValue::Record(r) => write!(f, "{}", r),
            RuntimeValue::Abs(_) => write!(f, "<anonymous_client>"),
            RuntimeValue::Fun(_) => write!(f, "<function>"),
        }
    }
}

impl RuntimeValue {
    pub fn string(value: impl AsRef<str>) -> Self {
        RuntimeValue::Literal(Literal::String(value.as_ref().to_string()))
    }

    pub fn bool(self) -> eyre::Result<bool> {
        if let RuntimeValue::Literal(Literal::Bool(b)) = self {
            return Ok(b);
        }

        eyre::bail!(
            "Unexpected runtime exception: type mismatch, expected Bool but got something else"
        )
    }

    pub fn suspension(self) -> eyre::Result<Suspension> {
        if let RuntimeValue::Fun(sus) = self {
            return Ok(sus);
        }

        eyre::bail!("Unexpected runtime exception: expected a suspension")
    }

    pub fn channel(self) -> eyre::Result<Channel> {
        match self {
            RuntimeValue::Channel(c) => Ok(c),
            _ => eyre::bail!("Unexpected runtime exception: type mismatch, expected a channel"),
        }
    }

    pub fn server(self) -> eyre::Result<Arc<Mutex<mpsc::UnboundedReceiver<RuntimeValue>>>> {
        match self.channel()? {
            Channel::Dual(s, _) | Channel::Server(s) => Ok(s),
            _ => {
                eyre::bail!("Unexpected runtime exception: typemismatch, expected a server channel")
            }
        }
    }
}

#[derive(Clone)]
pub enum Channel {
    Dual(
        Arc<Mutex<UnboundedReceiver<RuntimeValue>>>,
        UnboundedSender<RuntimeValue>,
    ),
    Client(UnboundedSender<RuntimeValue>),
    Server(Arc<Mutex<UnboundedReceiver<RuntimeValue>>>),
}

impl Display for Channel {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Channel::Dual(_, _) => write!(f, "<channel>"),
            Channel::Client(_) => write!(f, "<client>"),
            Channel::Server(_) => write!(f, "<server>"),
        }
    }
}
