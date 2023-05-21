use std::collections::{HashMap, VecDeque};
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use clap::Parser;
use pyro_core::ast::{Proc, Record, Tag, Val};
use pyro_core::sym::Literal;
use pyro_core::tokenizer::Pos;
use tokio::sync::{mpsc, Mutex};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(long)]
    tokens: bool,

    file: PathBuf,
}

#[tokio::main]
async fn main() -> eyre::Result<()> {
    let args = Args::parse();
    let source = fs::read_to_string(args.file.as_path())?;

    if args.tokens {
        println!("Tokens: {:?}", pyro_core::tokenize(source.as_str()));
    }

    let ast = pyro_core::parse(source.as_str())?;
    let runtime = Runtime::default();

    execute(runtime, ast).await
}

async fn execute(_runtime: Runtime, progs: VecDeque<Tag<Proc<Pos>, Pos>>) -> eyre::Result<()> {
    let default_scope = Scope::default();

    for tag in progs {
        execute_proc(default_scope.clone(), tag).await?;
    }

    Ok(())
}

async fn execute_proc(scope: Scope, tag: Tag<Proc<Pos>, Pos>) -> eyre::Result<()> {
    match tag.item {
        Proc::Output(target, param) => execute_output(&scope, target, param).await,
        Proc::Input(_, _) => todo!(),
        Proc::Null => todo!(),
        Proc::Parallel(_) => todo!(),
        Proc::Decl(_, _) => todo!(),
        Proc::Cond(_, _, _) => todo!(),
    }
}

async fn execute_output(
    scope: &Scope,
    target: Tag<Val, Pos>,
    param: Tag<Val, Pos>,
) -> eyre::Result<()> {
    let target = if let Val::Path(path) = &target.item {
        let id = build_var_id(path.as_slice());
        if let RuntimeValue::Channel(chan) = scope.look_up(&id)? {
            chan.output
        } else {
            eyre::bail!("'{}' is not an output", id);
        }
    } else {
        eyre::bail!(
            "Unexpected value '{}' when looking for a channel's output",
            target.item
        );
    };

    let _ = target.send(resolve(scope, param.item)?);

    Ok(())
}

fn resolve(scope: &Scope, value: Val) -> eyre::Result<RuntimeValue> {
    match value {
        Val::Literal(l) => Ok(RuntimeValue::Literal(l.clone())),
        Val::Path(paths) => scope.look_up(&build_var_id(&paths)),
        Val::Record(r) => Ok(RuntimeValue::Record(
            r.traverse_result(|v| resolve(scope, v))?,
        )),
    }
}

fn build_var_id(paths: &[String]) -> String {
    paths.join(".").to_string().to_string()
}

#[derive(Default)]
struct Runtime {
    scopes: VecDeque<Scope>,
    scope: u64,
}

#[derive(Clone)]
enum RuntimeValue {
    Channel(Channel),
    Literal(Literal),
    Record(Record<RuntimeValue>),
}

#[derive(Clone)]
struct Channel {
    input: Arc<Mutex<mpsc::UnboundedReceiver<RuntimeValue>>>,
    output: mpsc::UnboundedSender<RuntimeValue>,
}

#[derive(Default, Clone)]
struct Scope {
    variables: HashMap<String, RuntimeValue>,
}

impl Scope {
    fn look_up(&self, name: &str) -> eyre::Result<RuntimeValue> {
        if let Some(value) = self.variables.get(name) {
            return Ok(value.clone());
        }

        eyre::bail!("Unknown identifier '{}'", name)
    }
}
