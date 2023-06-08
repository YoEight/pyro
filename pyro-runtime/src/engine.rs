use crate::env::Env;
use crate::prelude::load_prelude_symbols;
use crate::runtime::Runtime;
use crate::value::{Channel, RuntimeValue};
use pyro_core::annotate::Ann;
use pyro_core::ast::{Abs, Pat, Proc, Tag, Val};
use pyro_core::sym::Literal;
use tokio::sync::mpsc;

struct Suspend {
    runtime: Runtime,
    proc: Tag<Proc<Ann>, Ann>,
}

enum Msg {
    Job(mpsc::UnboundedSender<Msg>, Suspend),
    Completed(u64),
}

pub struct Engine {
    runtime: Runtime,
}

impl Engine {
    pub fn new() -> Self {
        let (print_output, print_input) = mpsc::unbounded_channel();
        let env = Env {
            stdout_handle: print_output,
        };

        let runtime = load_prelude_symbols(&env);

        spawn_stdout_process(print_input);

        Self { runtime }
    }

    pub async fn run(self, source_code: &str) -> eyre::Result<()> {
        let progs = pyro_core::parse(source_code)?;
        let mut work_items = Vec::new();
        let (sender, mut mailbox) = mpsc::unbounded_channel::<Msg>();

        for tag in progs {
            work_items.push(Suspend {
                runtime: self.runtime.clone(),
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
                            let local_runtime = s.runtime.clone();
                            match execute_proc(s.runtime, s.proc).await {
                                Err(e) => {
                                    local_runtime
                                        .println(format!("UNEXPECTED RUNTIME ERROR: {}", e));
                                }

                                Ok(jobs) => {
                                    for next in jobs {
                                        let _ = out.send(Msg::Job(out.clone(), next));
                                    }
                                }
                            };

                            let _ = out.send(Msg::Completed(proc_id));
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
}

fn spawn_stdout_process(mut input: mpsc::UnboundedReceiver<RuntimeValue>) {
    tokio::spawn(async move {
        while let Some(value) = input.recv().await {
            if let RuntimeValue::Literal(lit) = value {
                match lit {
                    Literal::Integer(n) => println!("{}", n),
                    Literal::String(s) => println!("{}", s),
                    Literal::Char(c) => println!("'{}'", c),
                    Literal::Bool(b) => println!("{}", b),
                }
            }
        }
    });
}

async fn execute_proc(
    mut runtime: Runtime,
    proc: Tag<Proc<Ann>, Ann>,
) -> eyre::Result<Vec<Suspend>> {
    let mut sus = Vec::new();

    match proc.item {
        Proc::Output(target, param) => {
            if let Some(suspend) = execute_output(&mut runtime, target, param)? {
                sus.push(suspend);
            }
        }

        Proc::Input(source, abs) => {
            if let Some(suspend) = execute_input(&mut runtime, source, abs).await? {
                sus.push(suspend);
            }
        }
        Proc::Null => {}

        Proc::Parallel(procs) => {
            for proc in procs {
                sus.push(Suspend {
                    runtime: runtime.clone(),
                    proc,
                });
            }
        }

        Proc::Decl(decl, proc) => {
            runtime.register(decl.item);
            sus.push(Suspend {
                runtime,
                proc: *proc,
            });
        }

        Proc::Cond(test, if_proc, else_proc) => {
            let test = resolve(&mut runtime, test.item)?;
            let test = test.bool()?;
            let proc = if test { if_proc } else { else_proc };

            sus.push(Suspend {
                runtime,
                proc: *proc,
            })
        }
    };

    Ok(sus)
}

fn execute_output(
    runtime: &mut Runtime,
    target: Tag<Val<Ann>, Ann>,
    param: Tag<Val<Ann>, Ann>,
) -> eyre::Result<Option<Suspend>> {
    let target = if let Val::Path(path) = &target.item {
        let id = build_var_id(path.as_slice());

        match runtime.look_up(&id)? {
            RuntimeValue::Channel(Channel::Client(c) | Channel::Dual(_, c)) => c,

            RuntimeValue::Abs(abs) => {
                let value = resolve(runtime, param.item)?;

                update_scope(runtime, &value, abs.item.pattern.item);

                return Ok(Some(Suspend {
                    runtime: runtime.clone(),
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

    let value = resolve(runtime, param.item)?;
    let _ = target.send(value);

    Ok(None)
}

async fn execute_input(
    runtime: &mut Runtime,
    source: Tag<Val<Ann>, Ann>,
    abs: Tag<Abs<Ann>, Ann>,
) -> eyre::Result<Option<Suspend>> {
    let receiver = if let Val::Path(path) = &source.item {
        let id = build_var_id(path.as_slice());
        runtime.look_up(&id)?.server()?
    } else {
        eyre::bail!(
            "Unexpected value '{}' when looking for a channel's input",
            source.item
        );
    };

    runtime.keeps(abs.tag.used.keys());
    let mut recv = receiver.lock().await;
    if let Some(value) = recv.recv().await {
        update_scope(runtime, &value, abs.item.pattern.item.clone());

        return Ok(Some(Suspend {
            runtime: runtime.clone(),
            proc: *abs.item.proc,
        }));
    }

    Ok(None)
}

fn update_scope(runtime: &mut Runtime, value: &RuntimeValue, pattern: Pat<Ann>) {
    match pattern {
        Pat::Var(var) => {
            runtime.insert(var.var.id, value.clone());

            if let Some(pattern) = var.pattern {
                update_scope(runtime, value, *pattern.clone());
            }
        }

        Pat::Record(rec) => {
            // We already typecheck at that level so it's safe to assume that
            // the runtime value is indeed a record that is comprises of all the
            // properties that we need.
            if let RuntimeValue::Record(src) = value {
                for (pat, value) in rec.props.iter().zip(src.props.iter()) {
                    update_scope(runtime, &value.val, pat.val.item.clone());

                    if let Some(name) = &pat.label {
                        runtime.insert(name.clone(), value.val.clone());
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

fn resolve(runtime: &mut Runtime, value: Val<Ann>) -> eyre::Result<RuntimeValue> {
    match value {
        Val::Literal(l) => Ok(RuntimeValue::Literal(l.clone())),
        Val::Path(paths) => runtime.look_up(&build_var_id(&paths)),
        Val::Record(r) => Ok(RuntimeValue::Record(
            r.traverse_result(|v| resolve(runtime, v.item))?,
        )),
        Val::AnoClient(abs) => Ok(RuntimeValue::Abs(abs)),
        Val::App(func, param) => {
            let func = resolve(runtime, func.item)?;
            let param = resolve(runtime, param.item)?;

            if let RuntimeValue::Fun(cont) = func {
                return Ok(cont(param));
            }

            eyre::bail!("Unreachable code, means the type checking failed us :'-(")
        }
    }
}

fn build_var_id(paths: &[String]) -> String {
    paths.join(".").to_string().to_string()
}
