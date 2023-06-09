use futures::future::BoxFuture;
use pyro_core::annotate::Ann;
use pyro_core::ast::{Abs, Record, Tag};
use pyro_core::sym::Literal;
use std::sync::Arc;
use tokio::sync::mpsc::{UnboundedReceiver, UnboundedSender};
use tokio::sync::{mpsc, Mutex};

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

    pub fn func_2<F, A, B, C>(func: F) -> Self
    where
        F: Fn(A, B) -> C + Send + Sync + 'static,
        A: TryFromValue,
        B: TryFromValue,
        C: ToRuntimeValue,
    {
        let func = Arc::new(func);
        RuntimeValue::Fun(Arc::new(move |a| {
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
                            TryFromValue::try_from_value(a_2.clone())?,
                            TryFromValue::try_from_value(b.clone())?,
                        )
                        .runtime_value())
                    })
                })))
            })
        }))
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
            Channel::Dual(s, _) | Channel::_Server(s) => Ok(s),
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
    _Server(Arc<Mutex<mpsc::UnboundedReceiver<RuntimeValue>>>),
}

pub trait TryFromValue {
    fn try_from_value(value: Arc<RuntimeValue>) -> eyre::Result<Self>
    where
        Self: Sized;
}

impl TryFromValue for u64 {
    fn try_from_value(value: Arc<RuntimeValue>) -> eyre::Result<Self> {
        if let RuntimeValue::Literal(Literal::Integer(v)) = value.as_ref() {
            return Ok(*v);
        }

        eyre::bail!("Expected an u64 runtime value")
    }
}

impl TryFromValue for bool {
    fn try_from_value(value: Arc<RuntimeValue>) -> eyre::Result<Self> {
        if let RuntimeValue::Literal(Literal::Bool(v)) = value.as_ref() {
            return Ok(*v);
        }

        eyre::bail!("Expected a bool runtime value")
    }
}

impl TryFromValue for char {
    fn try_from_value(value: Arc<RuntimeValue>) -> eyre::Result<Self> {
        if let RuntimeValue::Literal(Literal::Char(v)) = value.as_ref() {
            return Ok(*v);
        }

        eyre::bail!("Expected a char runtime value")
    }
}

impl TryFromValue for String {
    fn try_from_value(value: Arc<RuntimeValue>) -> eyre::Result<Self> {
        if let RuntimeValue::Literal(Literal::String(s)) = value.as_ref() {
            return Ok(s.clone());
        }

        eyre::bail!("Expected an string runtime value")
    }
}

pub trait ToRuntimeValue {
    fn runtime_value(self) -> RuntimeValue;
}

impl ToRuntimeValue for u64 {
    fn runtime_value(self) -> RuntimeValue {
        RuntimeValue::Literal(Literal::Integer(self))
    }
}

impl ToRuntimeValue for char {
    fn runtime_value(self) -> RuntimeValue {
        RuntimeValue::Literal(Literal::Char(self))
    }
}

impl ToRuntimeValue for String {
    fn runtime_value(self) -> RuntimeValue {
        RuntimeValue::Literal(Literal::String(self))
    }
}

impl<'a> ToRuntimeValue for &'a str {
    fn runtime_value(self) -> RuntimeValue {
        RuntimeValue::Literal(Literal::String(self.to_string()))
    }
}

impl ToRuntimeValue for bool {
    fn runtime_value(self) -> RuntimeValue {
        RuntimeValue::Literal(Literal::Bool(self))
    }
}
