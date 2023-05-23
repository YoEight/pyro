use std::collections::{HashMap, VecDeque};
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use clap::Parser;
use pyro_core::ast::{Abs, Decl, Def, Pat, Proc, Record, Tag, Type, Val};
use pyro_core::sym::Literal;
use pyro_core::tokenizer::Pos;
use tokio::sync::mpsc::UnboundedSender;
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

    execute(runtime, ast).await?;

    Ok(())
}

fn default_scope() -> Scope {
    let mut scope = Scope::default();
    let (print_output, mut print_input) = mpsc::unbounded_channel();

    scope.insert(
        "print".to_string(),
        RuntimeValue::Output(Type::Name("String".to_string()), print_output),
    );

    tokio::spawn(async move {
        while let Some(value) = print_input.recv().await {
            if let RuntimeValue::Literal(Literal::String(s)) = value {
                println!("{}", s);
            }
        }
    });

    scope
}

async fn execute(_runtime: Runtime, progs: VecDeque<Tag<Proc<Pos>, Pos>>) -> eyre::Result<()> {
    enum Msg {
        Job(UnboundedSender<Msg>, Suspend),
        Completed,
    }

    let default_scope = default_scope();
    let mut work_items = Vec::new();
    let (sender, mut mailbox) = mpsc::unbounded_channel::<Msg>();

    for tag in progs {
        work_items.push(Suspend {
            scope: default_scope.clone(),
            proc: tag.item,
        });
    }

    let handle = tokio::spawn(async move {
        let mut gauge = 0i32;

        while let Some(msg) = mailbox.recv().await {
            match msg {
                Msg::Job(out, s) => {
                    tokio::spawn(async move {
                        for next in execute_proc(s.scope, s.proc).await? {
                            let _ = out.send(Msg::Job(out.clone(), next));
                        }

                        let _ = out.send(Msg::Completed);
                        Ok::<_, eyre::Error>(())
                    });

                    gauge += 1;
                }

                Msg::Completed => {
                    gauge -= 1;
                    if gauge <= 0 {
                        break;
                    }
                }
            }
        }
    });

    for job in work_items {
        let _ = sender.send(Msg::Job(sender.clone(), job));
    }

    handle.await?;
    Ok(())
}

struct Suspend {
    scope: Scope,
    proc: Proc<Pos>,
}

async fn execute_proc(mut scope: Scope, proc: Proc<Pos>) -> eyre::Result<Vec<Suspend>> {
    let mut sus = Vec::new();

    match proc {
        Proc::Output(target, param) => {
            execute_output(&mut scope, target, param)?;
        }

        Proc::Input(source, abs) => {
            if let Some(suspend) = execute_input(&mut scope, source, abs).await? {
                sus.push(suspend);
            }
        }
        Proc::Null => {}

        Proc::Parallel(procs) => {
            for proc in procs {
                sus.push(Suspend {
                    scope: scope.clone(),
                    proc,
                });
            }
        }

        Proc::Decl(decl, proc) => {
            scope.register(decl);
            sus.push(Suspend {
                scope,
                proc: *proc.item,
            });
        }

        Proc::Cond(_, _, _) => todo!(),
    };

    Ok(sus)
}

fn execute_output(
    scope: &mut Scope,
    target: Tag<Val, Pos>,
    param: Tag<Val, Pos>,
) -> eyre::Result<()> {
    let pos = target.tag;
    let (typ, target) = if let Val::Path(path) = &target.item {
        let id = build_var_id(path.as_slice());

        match id.as_str() {
            "print" => {
                let RuntimeValue::Output(typ, out) = scope.look_up(id.as_str())? else {
                    eyre::bail!("unreachable code");
                };

                (typ, out)
            }

            _ => {
                let key = format!("{}-output", id);

                if let RuntimeValue::Output(typ, out) = scope.take(&key)? {
                    (typ, out)
                } else {
                    eyre::bail!("'{}' is not an output", id);
                }
            }
        }
    } else {
        eyre::bail!(
            "Unexpected value '{}' when looking for a channel's output",
            target.item
        );
    };

    let value = resolve(scope, param.item)?;
    let value_type = value.r#type();

    if !value_type.typecheck(&typ.inner_type()) {
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
    scope: &mut Scope,
    source: Tag<Val, Pos>,
    abs: Tag<Abs<Pos>, Pos>,
) -> eyre::Result<Option<Suspend>> {
    let pos = source.tag;
    let (typ, source) = if let Val::Path(path) = &source.item {
        let id = build_var_id(path.as_slice());
        let key = format!("{}-input", id);

        if let RuntimeValue::Input(typ, chan) = scope.take(&key)? {
            (typ, chan)
        } else {
            eyre::bail!("'{}' is not an input", id);
        }
    } else {
        eyre::bail!(
            "Unexpected value '{}' when looking for a channel's input",
            source.item
        );
    };

    let mut recv = source.lock().await;
    if let Some(value) = recv.recv().await {
        let value_type = value.r#type();
        if !value_type.typecheck(&typ.inner_type()) {
            eyre::bail!(
                "{}:{}: Type mismatch, expected channel type to be {} but got {} instead",
                pos.line,
                pos.column,
                typ,
                value_type,
            );
        }

        update_scope(scope, &value, abs.item.pattern.clone());

        return Ok(Some(Suspend {
            scope: scope.clone(),
            proc: *abs.item.proc,
        }));
    }

    Ok(None)
}

fn update_scope(scope: &mut Scope, value: &RuntimeValue, pattern: Pat) {
    match pattern {
        Pat::Var(var) => {
            scope.insert(var.var.id, value.clone());

            if let Some(pattern) = var.pattern {
                update_scope(scope, value, *pattern.clone());
            }
        }

        Pat::Record(rec) => {
            // We already typecheck at that level so it's safe to assume that
            // the runtime value is indeed a record that is comprises of all the
            // properties that we need.
            if let RuntimeValue::Record(src) = value {
                for (pat, value) in rec.props.iter().zip(src.props.iter()) {
                    update_scope(scope, &value.val, pat.val.clone());

                    if let Some(name) = &pat.label {
                        scope.insert(name.clone(), value.val.clone());
                    }
                }
            }
        }

        Pat::Wildcard(_) => {
            // We do nothing as wildcard is meant to not pollute the scope with
            // variable we are not going to use!
        }
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
    Input(Type, Arc<Mutex<mpsc::UnboundedReceiver<RuntimeValue>>>),
    Output(Type, mpsc::UnboundedSender<RuntimeValue>),
    Literal(Literal),
    Record(Record<RuntimeValue>),
    Abs(Abs<Pos>),
}

impl RuntimeValue {
    pub fn r#type(&self) -> Type {
        match self {
            RuntimeValue::Input(t, _) => t.clone(),
            RuntimeValue::Output(t, _) => t.clone(),
            RuntimeValue::Literal(l) => l.r#type(),
            RuntimeValue::Record(r) => record_type(&r),
            RuntimeValue::Abs(_) => todo!(),
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

    fn take(&mut self, name: &str) -> eyre::Result<RuntimeValue> {
        if let Some(value) = self.variables.remove(name) {
            return Ok(value);
        }

        eyre::bail!("Unknown identififer '{}'", name)
    }

    fn insert(&mut self, name: String, value: RuntimeValue) {
        self.variables.insert(name, value);
    }

    fn register(&mut self, decl: Decl<Pos>) {
        match decl {
            Decl::Channel(name, r#type) => {
                let (output, input) = mpsc::unbounded_channel();
                let input = Arc::new(Mutex::new(input));

                self.insert(
                    format!("{}-output", name),
                    RuntimeValue::Output(r#type.clone(), output),
                );

                self.insert(
                    format!("{}-input", name),
                    RuntimeValue::Input(r#type, input),
                );
            }

            Decl::Def(defs) => {
                for def in defs {
                    self.register_def(def);
                }
            }

            Decl::Type(_, _) => {
                // There is no value at runtime to register a new type declaration.
            }
        }
    }

    fn register_def(&self, def: Def<Pos>) {
        todo!()
    }
}
