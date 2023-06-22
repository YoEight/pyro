use crate::env::Env;
use crate::runtime::Runtime;
use crate::value::{Channel, RuntimeValue, Symbol};
use pyro_core::annotate::Ann;
use pyro_core::ast::{Abs, Pat, Proc, Prop, Record, Tag, Val};
use pyro_core::{Dict, Knowledge, LocalScope, Type, TypePointer, TypeRef, STDLIB};
use tokio::sync::mpsc;

struct Suspend {
    runtime: Runtime,
    proc: Tag<Proc<Ann>, Ann>,
}

enum Msg {
    Job(mpsc::UnboundedSender<Msg>, Suspend),
    Completed(u64),
}

#[derive(Default, Clone)]
pub struct EngineBuilder {
    symbols: Vec<Symbol>,
    types: Vec<(String, Type)>,
}

impl EngineBuilder {
    pub fn build(mut self) -> Engine {
        let (print_output, print_input) = mpsc::unbounded_channel();
        let env = Env {
            stdout_handle: print_output,
        };
        let stdout = env.stdout();
        let mut knowledge = Knowledge::standard();
        let mut runtime = Runtime::new(env);
        let local = LocalScope {
            ancestors: vec![0, 999999],
        };
        let var = TypePointer::Ref(TypeRef {
            scope: local.clone(),
            name: "'a".to_string(),
        });
        self.symbols.push(Symbol {
            name: "print".to_string(),
            r#type: TypePointer::app(
                TypePointer::Ref(TypeRef {
                    scope: STDLIB.as_local_scope(),
                    name: "Client".to_string(),
                }),
                TypePointer::ForAll(
                    false,
                    local.clone(),
                    vec!["'a".to_string()],
                    Box::new(TypePointer::Qual(
                        vec![TypePointer::app(
                            TypePointer::Ref(TypeRef {
                                scope: STDLIB.as_local_scope(),
                                name: "Show".to_string(),
                            }),
                            var.clone(),
                        )],
                        Box::new(var),
                    )),
                ),
            ),
            value: RuntimeValue::Channel(Channel::Client(stdout)),
        });

        self.symbols
            .push(Symbol::func_2("+", |a: i64, b: i64| a + b));
        self.symbols
            .push(Symbol::func_2("-", |a: i64, b: i64| a - b));
        self.symbols
            .push(Symbol::func_2("*", |a: i64, b: i64| a * b));
        self.symbols
            .push(Symbol::func_2("<=", |a: i64, b: i64| a <= b));
        self.symbols
            .push(Symbol::func_2("<", |a: i64, b: i64| a < b));
        self.symbols
            .push(Symbol::func_2(">=", |a: i64, b: i64| a >= b));
        self.symbols
            .push(Symbol::func_2(">", |a: i64, b: i64| a > b));
        self.symbols
            .push(Symbol::func_2("==", |a: i64, b: i64| a == b));
        self.symbols.push(Symbol::func_2("&&", |a, b| a && b));
        self.symbols.push(Symbol::func_2("||", |a, b| a || b));

        for sym in self.symbols {
            knowledge.declare_from_pointer(&STDLIB, &sym.name, sym.r#type);
            runtime.insert(sym.name, sym.value);
        }

        for (name, r#type) in self.types {
            knowledge.declare_from_dict(&STDLIB, name, Dict::new(r#type));
        }

        spawn_stdout_process(print_input);

        Engine { runtime, knowledge }
    }

    pub fn add_symbol(mut self, sym: Symbol) -> Self {
        self.symbols.push(sym);
        self
    }

    pub fn add_type(mut self, name: impl AsRef<str>, r#type: Type) -> Self {
        self.types.push((name.as_ref().to_string(), r#type));
        self
    }
}

#[derive(Clone)]
pub struct Engine {
    runtime: Runtime,
    knowledge: Knowledge,
}

impl Engine {
    pub fn builder() -> EngineBuilder {
        EngineBuilder::default()
    }

    pub fn context(&mut self) -> &mut Knowledge {
        &mut self.knowledge
    }

    pub fn runtime(&mut self) -> &mut Runtime {
        &mut self.runtime
    }

    pub async fn run(mut self, source_code: &str) -> eyre::Result<()> {
        let progs = pyro_core::parse(&mut self.context(), source_code)?;
        self.runtime.used = self.context().build_used_variables_table();
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
            println!("{}", value);
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
            if let Some(suspend) = execute_output(&mut runtime, target, param).await? {
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
            let test = interpret(&mut runtime, test.item).await?;
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

async fn execute_output(
    runtime: &mut Runtime,
    target: Tag<Val<Ann>, Ann>,
    param: Tag<Val<Ann>, Ann>,
) -> eyre::Result<Option<Suspend>> {
    let target = if let Val::Path(path) = &target.item {
        let id = build_var_id(path.as_slice());

        match runtime.look_up(&id)? {
            RuntimeValue::Channel(Channel::Client(c) | Channel::Dual(_, c)) => c,

            RuntimeValue::Abs(abs) => {
                match (abs.item.pattern.item, param.item) {
                    (Pat::Var(pat_var), value) => {
                        let value = interpret(runtime, value).await?;
                        runtime.insert(pat_var.var.id, value);
                    }

                    (Pat::Record(pat_rec), Val::Record(param_rec)) => {
                        for (pat, value) in
                            pat_rec.props.into_iter().zip(param_rec.props.into_iter())
                        {
                            // TODO - implement deeper pattern matching into the record. Needs to be stack-based because interpret is async.
                            if let Pat::Var(pat_var) = pat.val.item {
                                let value = interpret(runtime, value.val.item).await?;
                                runtime.insert(pat_var.var.id, value);
                            }
                        }
                    }
                    _ => {}
                }

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

    let value = interpret(runtime, param.item).await?;
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

    runtime.keeps(&abs.tag.scope);
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
                update_scope(runtime, value, pattern.item.clone());
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

async fn interpret(runtime: &mut Runtime, value: Val<Ann>) -> eyre::Result<RuntimeValue> {
    let mut stack = vec![Instr::I(value)];

    enum Instr {
        I(Val<Ann>),
        V(RuntimeValue),
    }

    enum State {
        Idle,
        Apply(u8, Option<RuntimeValue>),
        Rec(Vec<Prop<()>>, Vec<Prop<RuntimeValue>>),
    }

    let mut state = State::Idle;
    while let Some(instr) = stack.pop() {
        match instr {
            Instr::V(value) => match state {
                State::Idle => return Ok(value),
                State::Apply(mut count, func) => {
                    if let Some(func) = func {
                        let func = func.suspension()?;
                        let value = func(value).await?;
                        count -= 1;

                        if count == 0 {
                            stack.push(Instr::V(value));
                            state = State::Idle;
                        } else {
                            let new_func = value;
                            state = State::Apply(count, Some(new_func));
                        }
                    } else {
                        state = State::Apply(count, Some(value));
                    }
                }
                State::Rec(mut expectations, mut props) => {
                    if let Some(prop) = expectations.pop() {
                        let (_, prop) = prop.replace(value);
                        props.push(prop);

                        if expectations.is_empty() {
                            state = State::Idle;
                            stack.push(Instr::V(RuntimeValue::Record(Record { props })));
                        } else {
                            state = State::Rec(expectations, props);
                        }

                        continue;
                    }

                    eyre::bail!("Unexpected runtime error when building record");
                }
            },

            Instr::I(val) => match val {
                Val::Literal(l) => stack.push(Instr::V(RuntimeValue::Literal(l))),
                Val::Path(paths) => stack.push(Instr::V(runtime.look_up(&build_var_id(&paths))?)),
                Val::AnoClient(abs) => stack.push(Instr::V(RuntimeValue::Abs(abs))),

                Val::Record(mut r) => {
                    let mut props = Vec::new();

                    while let Some(prop) = r.props.pop() {
                        let (tag, prop) = prop.replace(());
                        stack.push(Instr::I(tag.item));
                        props.push(prop);
                    }

                    if props.is_empty() {
                        stack.push(Instr::V(RuntimeValue::Record(Record::empty())))
                    } else {
                        state = State::Rec(props, Vec::new());
                    }
                }

                Val::App(func, param) => {
                    stack.push(Instr::I(param.item));
                    stack.push(Instr::I(func.item));

                    if let State::Apply(count, _) = &mut state {
                        *count += 1;
                    } else {
                        state = State::Apply(1, None);
                    }
                }
            },
        }
    }

    eyre::bail!("Invalid stack error!")
}

fn build_var_id(paths: &[String]) -> String {
    paths.join(".").to_string().to_string()
}
