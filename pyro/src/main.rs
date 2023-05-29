use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use clap::Parser;
use pyro_core::annotate::Ann;
use pyro_core::ast::{Abs, Decl, Def, Pat, Proc, Record, Tag, Val};
use pyro_core::sym::Literal;
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

    execute(ast).await?;

    Ok(())
}

fn default_scope() -> Scope {
    let mut scope = Scope::default();
    let (print_output, mut print_input) = mpsc::unbounded_channel();

    scope.insert(
        "print".to_string(),
        RuntimeValue::Channel(Channel::Client(print_output)),
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

async fn execute(progs: Vec<Tag<Proc<Ann>, Ann>>) -> eyre::Result<()> {
    enum Msg {
        Job(UnboundedSender<Msg>, Suspend),
        Completed(u64),
    }

    let default_scope = default_scope();
    let mut work_items = Vec::new();
    let (sender, mut mailbox) = mpsc::unbounded_channel::<Msg>();

    for tag in progs {
        work_items.push(Suspend {
            scope: default_scope.clone(),
            proc: tag,
        });
    }

    let handle = tokio::spawn(async move {
        let mut gauge = 0i32;
        let mut proc_id_gen = 0u64;

        while let Some(msg) = mailbox.recv().await {
            match msg {
                Msg::Job(out, s) => {
                    let proc_id = proc_id_gen;
                    proc_id_gen += 1;

                    tokio::spawn(async move {
                        for next in execute_proc(s.scope, s.proc).await? {
                            let _ = out.send(Msg::Job(out.clone(), next));
                        }

                        let _ = out.send(Msg::Completed(proc_id));
                        Ok::<_, eyre::Error>(())
                    });

                    gauge += 1;
                }

                Msg::Completed(_proc_id) => {
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
    proc: Tag<Proc<Ann>, Ann>,
}

async fn execute_proc(mut scope: Scope, proc: Tag<Proc<Ann>, Ann>) -> eyre::Result<Vec<Suspend>> {
    let mut sus = Vec::new();

    match proc.item {
        Proc::Output(target, param) => {
            if let Some(suspend) = execute_output(&mut scope, target, param)? {
                sus.push(suspend);
            }
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
            scope.register(decl.item);
            sus.push(Suspend { scope, proc: *proc });
        }

        Proc::Cond(test, if_proc, else_proc) => {
            let test = resolve(&mut scope, test.item)?;
            let test = to_bool(test)?;
            let proc = if test { if_proc } else { else_proc };

            sus.push(Suspend { scope, proc: *proc })
        }
    };

    Ok(sus)
}

fn to_bool(val: RuntimeValue) -> eyre::Result<bool> {
    if let RuntimeValue::Literal(Literal::Bool(b)) = val {
        return Ok(b);
    }

    eyre::bail!("Unexpected runtime exception: type mismatch, expected Bool but got something else")
}

fn to_channel(val: RuntimeValue) -> eyre::Result<Channel> {
    match val {
        RuntimeValue::Channel(c) => Ok(c),
        RuntimeValue::Abs(_) => todo!(),
        _ => eyre::bail!("Unexpected runtime exception: type mismatch, expected a channel"),
    }
}

fn to_server(val: RuntimeValue) -> eyre::Result<Arc<Mutex<mpsc::UnboundedReceiver<RuntimeValue>>>> {
    match to_channel(val)? {
        Channel::Dual(s, _) | Channel::_Server(s) => Ok(s),
        _ => eyre::bail!("Unexpected runtime exception: typemismatch, expected a server channel"),
    }
}

fn execute_output(
    scope: &mut Scope,
    target: Tag<Val<Ann>, Ann>,
    param: Tag<Val<Ann>, Ann>,
) -> eyre::Result<Option<Suspend>> {
    let pos = target.tag.pos;
    let target_type = target.tag.r#type;
    let param_type = param.tag.r#type;
    let target = if let Val::Path(path) = &target.item {
        let id = build_var_id(path.as_slice());

        match scope.look_up(&id)? {
            RuntimeValue::Channel(Channel::Client(c) | Channel::Dual(_, c)) => c,

            RuntimeValue::Abs(abs) => {
                let value = resolve(scope, param.item)?;

                update_scope(scope, &value, abs.item.pattern.item);

                return Ok(Some(Suspend {
                    scope: scope.clone(),
                    proc: *abs.item.proc,
                }));
            }

            _ => {
                eyre::bail!("Unexpected runtime exception: typemismatch, expected a client channel")
            }
        }
    } else {
        eyre::bail!(
            "Unexpected value '{}' when looking for a channel's output",
            target.item
        );
    };

    let value = resolve(scope, param.item)?;

    if !param_type.typecheck(&target_type.inner_type()) {
        eyre::bail!(
            "{}:{}: Type mismatch, expected channel type to be {} but got {} instead",
            pos.line,
            pos.column,
            target_type,
            param_type,
        );
    }

    let _ = target.send(value);

    Ok(None)
}

async fn execute_input(
    scope: &mut Scope,
    source: Tag<Val<Ann>, Ann>,
    abs: Tag<Abs<Ann>, Ann>,
) -> eyre::Result<Option<Suspend>> {
    let source = if let Val::Path(path) = &source.item {
        let id = build_var_id(path.as_slice());
        to_server(scope.look_up(&id)?)?
    } else {
        eyre::bail!(
            "Unexpected value '{}' when looking for a channel's input",
            source.item
        );
    };

    scope.retain(|name| abs.tag.used.contains_key(name));
    let mut recv = source.lock().await;
    if let Some(value) = recv.recv().await {
        update_scope(scope, &value, abs.item.pattern.item.clone());

        return Ok(Some(Suspend {
            scope: scope.clone(),
            proc: *abs.item.proc,
        }));
    }

    Ok(None)
}

fn update_scope(scope: &mut Scope, value: &RuntimeValue, pattern: Pat<Ann>) {
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
                    update_scope(scope, &value.val, pat.val.item.clone());

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

fn resolve(scope: &mut Scope, value: Val<Ann>) -> eyre::Result<RuntimeValue> {
    match value {
        Val::Literal(l) => Ok(RuntimeValue::Literal(l.clone())),
        Val::Path(paths) => scope.look_up(&build_var_id(&paths)),
        Val::Record(r) => Ok(RuntimeValue::Record(
            r.traverse_result(|v| resolve(scope, v.item))?,
        )),
        Val::AnoFun(abs) => Ok(RuntimeValue::Abs(abs)),
    }
}

fn build_var_id(paths: &[String]) -> String {
    paths.join(".").to_string().to_string()
}

#[derive(Clone)]
enum RuntimeValue {
    Channel(Channel),
    Literal(Literal),
    Record(Record<RuntimeValue>),
    Abs(Tag<Abs<Ann>, Ann>),
}

#[derive(Clone)]
enum Channel {
    Dual(
        Arc<Mutex<mpsc::UnboundedReceiver<RuntimeValue>>>,
        UnboundedSender<RuntimeValue>,
    ),
    Client(UnboundedSender<RuntimeValue>),
    _Server(Arc<Mutex<mpsc::UnboundedReceiver<RuntimeValue>>>),
}

#[derive(Default, Clone)]
struct Scope {
    variables: HashMap<String, RuntimeValue>,
}

impl Scope {
    fn look_up(&self, name: &str) -> eyre::Result<RuntimeValue> {
        if let Some(value) = self.variables.get(name).cloned() {
            return Ok(value);
        }

        eyre::bail!("Unknown identifier '{}'", name)
    }

    fn insert(&mut self, name: String, value: RuntimeValue) {
        self.variables.insert(name, value);
    }

    fn register(&mut self, decl: Decl<Ann>) {
        match decl {
            Decl::Channels(chans) => {
                for (name, _) in chans {
                    let (output, input) = mpsc::unbounded_channel();
                    let input = Arc::new(Mutex::new(input));

                    self.insert(name, RuntimeValue::Channel(Channel::Dual(input, output)));
                }
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

    fn retain<F>(&mut self, fun: F)
    where
        F: Fn(&str) -> bool,
    {
        self.variables.retain(|key, _| fun(key))
    }

    fn register_def(&mut self, def: Def<Ann>) {
        self.variables
            .insert(def.name.clone(), RuntimeValue::Abs(def.abs));
    }
}
