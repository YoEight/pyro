use std::collections::{HashMap, VecDeque};
use std::fs;
use std::path::PathBuf;
use std::sync::Arc;

use clap::Parser;
use pyro_core::ast::{Abs, Decl, Def, Pat, Proc, Record, Tag, Type, Val};
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

    execute(runtime, ast)?;

    std::io::read_to_string(std::io::stdin())?;

    Ok(())
}

fn default_scope() -> Scope {
    let mut scope = Scope::default();

    let (print_output, print_input) = mpsc::unbounded_channel();
    let print_input = Arc::new(Mutex::new(print_input));

    scope.insert(
        "print".to_string(),
        RuntimeValue::Channel(
            Type::Name("String".to_string()),
            Channel {
                input: print_input.clone(),
                output: print_output,
            },
        ),
    );

    tokio::spawn(async move {
        let mut mailbox = print_input.lock().await;
        while let Some(value) = mailbox.recv().await {
            if let RuntimeValue::Literal(Literal::String(s)) = value {
                println!("{}", s);
            }
        }
    });

    scope
}

fn execute(_runtime: Runtime, progs: VecDeque<Tag<Proc<Pos>, Pos>>) -> eyre::Result<()> {
    let default_scope = default_scope();

    for tag in progs {
        execute_proc(default_scope.clone(), tag.item)?;
    }

    Ok(())
}

fn execute_proc(mut scope: Scope, proc: Proc<Pos>) -> eyre::Result<()> {
    match proc {
        Proc::Output(target, param) => execute_output(&scope, target, param),
        Proc::Input(source, abs) => execute_input(&scope, source, abs),
        Proc::Null => Ok(()),

        Proc::Parallel(procs) => {
            for proc in procs {
                let local_scope = scope.clone();
                tokio::spawn(async move {
                    execute_proc(local_scope, proc)?;

                    Ok::<_, eyre::Error>(())
                });
            }

            Ok(())
        }

        Proc::Decl(decl, proc) => {
            scope.register(decl);
            execute_proc(scope, *proc.item)
        }

        Proc::Cond(_, _, _) => todo!(),
    }
}

fn execute_output(scope: &Scope, target: Tag<Val, Pos>, param: Tag<Val, Pos>) -> eyre::Result<()> {
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

fn execute_input(
    scope: &Scope,
    source: Tag<Val, Pos>,
    abs: Tag<Abs<Pos>, Pos>,
) -> eyre::Result<()> {
    let pos = source.tag;
    let (typ, source) = if let Val::Path(path) = &source.item {
        let id = build_var_id(path.as_slice());
        if let RuntimeValue::Channel(typ, chan) = scope.look_up(&id)? {
            (typ.clone(), chan.input.clone())
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
            let mut recv = source.lock().await;
            recv.recv().await
        };

        if let Some(value) = value {
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

            update_scope(&mut scope, &value, abs.item.pattern.clone());
            execute_proc(scope, *abs.item.proc)?;
        }

        Ok(())
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
    Channel(Type, Channel),
    Literal(Literal),
    Record(Record<RuntimeValue>),
    Abs(Abs<Pos>),
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
            _ => todo!(),
        }
    }

    pub fn r#type(&self) -> Type {
        match self {
            RuntimeValue::Channel(t, _) => t.clone(),
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

    fn insert(&mut self, name: String, value: RuntimeValue) {
        self.variables.insert(name, value);
    }

    fn register(&mut self, decl: Decl<Pos>) {
        match decl {
            Decl::Channel(name, r#type) => {
                let (output, input) = mpsc::unbounded_channel();
                let input = Arc::new(Mutex::new(input));
                let channel = Channel { input, output };

                self.insert(name, RuntimeValue::Channel(r#type, channel));
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
