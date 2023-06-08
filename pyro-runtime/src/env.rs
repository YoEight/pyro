use crate::value::RuntimeValue;
use tokio::sync::mpsc;

#[derive(Clone)]
pub struct Env {
    pub(crate) stdout_handle: mpsc::UnboundedSender<RuntimeValue>,
}

impl Env {
    pub fn stdout(&self) -> mpsc::UnboundedSender<RuntimeValue> {
        self.stdout_handle.clone()
    }
}
