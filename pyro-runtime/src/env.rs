use crate::value::RuntimeValue;
use tokio::sync::mpsc;

#[derive(Clone)]
pub struct Env {
    pub stdout_handle: mpsc::UnboundedSender<RuntimeValue>,
}

impl Env {
    pub fn stdout(&self) -> mpsc::UnboundedSender<RuntimeValue> {
        self.stdout_handle.clone()
    }

    pub fn stdio() -> Self {
        let (print_output, print_input) = mpsc::unbounded_channel();

        spawn_stdout_process(print_input);

        Self {
            stdout_handle: print_output,
        }
    }
}

fn spawn_stdout_process(mut input: mpsc::UnboundedReceiver<RuntimeValue>) {
    tokio::spawn(async move {
        while let Some(value) = input.recv().await {
            println!("{}", value);
        }
    });
}
