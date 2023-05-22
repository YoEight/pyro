use std::collections::{HashMap, VecDeque};
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use clap::Parser;
use pyro_core::ast::{Pat, Proc, Record, Tag, Type, Val};
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
        Proc::Input(source, abs) => execute_input(&scope, source, abs).await,
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
    let pos = target.tag;
    let (typ, target) = if let Val::Path(path) = &target.item {
        let id = build_var_id(path.as_slice());
        if let RuntimeValue::Channel(typ, chan) = scope.look_up(&id)? {
            (typ, chan.output)
        } else {
            eyre::bail!("'{}' is not an output", id);
        }
    } else {
        eyre::bail!(
            "Unexpected value '{}' when looking for a channel's output",
            target.item
        );
    };

    let value = resolve(scope, param.item)?;
    let value_type = value.r#type();

    if value_type != typ {
        eyre::bail!(
            "{}:{}: Type mismatch, expected channel type to be {} but got {} instead",
            pos.line,
            pos.column,
            typ,
            value_type,
        );
    }

    let _ = target.send(value);

    Ok(())
}

async fn execute_input(
    scope: &Scope,
    source: Tag<Val, Pos>,
    abs: Tag<pyro_core::ast::Abs<Pos>, Pos>,
) -> eyre::Result<()> {
    let source = if let Val::Path(path) = &source.item {
        let id = build_var_id(path.as_slice());
        if let RuntimeValue::Channel(_, chan) = scope.look_up(&id)? {
            chan.input.clone()
        } else {
            eyre::bail!("'{}' is not an output", id);
        }
    } else {
        eyre::bail!(
            "Unexpected value '{}' when looking for a channel's input",
            source.item
        );
    };

    let mut scope = scope.clone();
    tokio::spawn(async move {
        let value = {
            let mut recv = source.blocking_lock();
            recv.recv().await
        };

        if let Some(value) = value {
            update_scope(&mut scope, &value, abs.item.pattern);
        }
    });

    Ok(())
}

fn update_scope(scope: &mut Scope, value: &RuntimeValue, pattern: Pat) {
    match pattern {
        Pat::Var(var) => {
            scope.insert(var.var.id, value.clone());

            if let Some(pattern) = var.pattern {
                update_scope(scope, value, *pattern.clone());
            }
        }
        Pat::Record(_) => todo!(),
        Pat::Wildcard(_) => todo!(),
    }
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
    Channel(Type, Channel),
    Literal(Literal),
    Record(Record<RuntimeValue>),
}

impl RuntimeValue {
    fn typematch(&self, r#type: &Type) -> bool {
        match self {
            RuntimeValue::Channel(run_typ, _) => {
                if let Type::Channel(inner) = r#type {
                    return run_typ == inner.as_ref();
                }

                false
            }
            RuntimeValue::Literal(l) => l.typematch(r#type),
            RuntimeValue::Record(rec) => rec.fold(true, |val, acc| acc && val.typematch(r#type)),
        }
    }

    pub fn r#type(&self) -> Type {
        match self {
            RuntimeValue::Channel(t, _) => t.clone(),
            RuntimeValue::Literal(l) => l.r#type(),
            RuntimeValue::Record(r) => record_type(&r),
        }
    }
}

fn record_type(value: &Record<RuntimeValue>) -> Type {
    let rec = value.clone().map(|r| r.r#type());

    Type::Record(rec)
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

    fn insert(&mut self, name: String, value: RuntimeValue) {
        self.variables.insert(name, value);
    }
}
