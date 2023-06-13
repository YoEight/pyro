use futures::future::BoxFuture;
use pyro_core::annotate::Ann;
use pyro_core::ast::{Abs, Record, Tag, Type};
use pyro_core::sym::Literal;
use std::sync::Arc;
use tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender};
use tokio::sync::{mpsc, Mutex};

pub trait PyroLiteral {
    fn try_from_value(value: Arc<RuntimeValue>) -> eyre::Result<Self>
    where
        Self: Sized;

    fn r#type() -> Type;

    fn value(self) -> RuntimeValue;
}

impl PyroLiteral for u64 {
    fn try_from_value(value: Arc<RuntimeValue>) -> eyre::Result<Self>
    where
        Self: Sized,
    {
        if let RuntimeValue::Literal(Literal::Integer(v)) = value.as_ref() {
            return Ok(*v);
        }

        eyre::bail!("Expected an u64 runtime value")
    }

    fn r#type() -> Type {
        Type::integer()
    }

    fn value(self) -> RuntimeValue {
        RuntimeValue::Literal(Literal::Integer(self))
    }
}

impl PyroLiteral for char {
    fn try_from_value(value: Arc<RuntimeValue>) -> eyre::Result<Self>
    where
        Self: Sized,
    {
        if let RuntimeValue::Literal(Literal::Char(v)) = value.as_ref() {
            return Ok(*v);
        }

        eyre::bail!("Expected a char runtime value")
    }

    fn r#type() -> Type {
        Type::char()
    }

    fn value(self) -> RuntimeValue {
        RuntimeValue::Literal(Literal::Char(self))
    }
}

impl PyroLiteral for String {
    fn try_from_value(value: Arc<RuntimeValue>) -> eyre::Result<Self>
    where
        Self: Sized,
    {
        if let RuntimeValue::Literal(Literal::String(v)) = value.as_ref() {
            return Ok(v.clone());
        }

        eyre::bail!("Expected a char runtime value")
    }

    fn r#type() -> Type {
        Type::string()
    }

    fn value(self) -> RuntimeValue {
        RuntimeValue::Literal(Literal::String(self))
    }
}

impl PyroLiteral for bool {
    fn try_from_value(value: Arc<RuntimeValue>) -> eyre::Result<Self>
    where
        Self: Sized,
    {
        if let RuntimeValue::Literal(Literal::Bool(v)) = value.as_ref() {
            return Ok(*v);
        }

        eyre::bail!("Expected a bool runtime value")
    }

    fn r#type() -> Type {
        Type::bool()
    }

    fn value(self) -> RuntimeValue {
        RuntimeValue::Literal(Literal::Bool(self))
    }
}

pub struct Symbol {
    pub name: String,
    pub r#type: Type,
    pub value: RuntimeValue,
}

impl Symbol {
    pub fn func_2<F, A, B, C>(name: impl AsRef<str>, func: F) -> Self
    where
        F: Fn(A, B) -> C + Send + Sync + 'static,
        A: PyroLiteral,
        B: PyroLiteral,
        C: PyroLiteral,
    {
        let func = Arc::new(func);
        let value = RuntimeValue::Fun(Arc::new(move |a| {
            let a = Arc::new(a);
            let func_local_1 = func.clone();
            Box::pin(async move {
                let a_1 = a.clone();
                let func_local_2 = func_local_1.clone();
                Ok(RuntimeValue::Fun(Arc::new(move |b| {
                    let a_2 = a_1.clone();
                    let b = Arc::new(b);
                    let func_local_3 = func_local_2.clone();
                    Box::pin(async move {
                        Ok(func_local_3(
                            PyroLiteral::try_from_value(a_2.clone())?,
                            PyroLiteral::try_from_value(b.clone())?,
                        )
                        .value())
                    })
                })))
            })
        }));

        let r#type = Type::func(A::r#type(), Type::func(B::r#type(), C::r#type()));

        Symbol {
            name: name.as_ref().to_string(),
            r#type,
            value,
        }
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
    Server(Arc<Mutex<mpsc::UnboundedReceiver<RuntimeValue>>>),
}