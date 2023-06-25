use crate::env::Env;
use crate::helpers::{Declared, InnerType, TypeBuilder, TypeCommand};
use crate::runtime::Runtime;
use crate::value::{Channel, PyroType, RuntimeValue};
use crate::PyroValue;
use pyro_core::annotate::Ann;
use pyro_core::ast::{Abs, Pat, Proc, Prop, Record, Tag, Val};
use pyro_core::{Dict, Knowledge, Type, TypePointer, STDLIB};
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::mpsc;

struct Suspend {
    runtime: Runtime,
    proc: Tag<Proc<Ann>, Ann>,
}

enum Msg {
    Job(mpsc::UnboundedSender<Msg>, Suspend),
    Completed(u64),
}

enum Command {
    CreateType {
        name: String,
        command: TypeCommand,
    },

    CreateSymbol {
        name: String,
        r#type: Declared,
        repr: RuntimeValue,
    },
}

pub struct EngineBuilder {
    commands: Vec<Command>,
    env: Option<Env>,
}

impl Default for EngineBuilder {
    fn default() -> Self {
        Self {
            commands: vec![],
            env: None,
        }
    }
}

impl EngineBuilder {
    pub fn register_function<F, A, B>(mut self, name: impl AsRef<str>, func: F) -> Self
    where
        F: Fn(A) -> B + Send + Sync + 'static,
        A: PyroValue,
        B: PyroValue,
    {
        let func = Arc::new(func);
        let repr = RuntimeValue::Fun(Arc::new(move |a| {
            let a = Arc::new(a);
            let func_local = func.clone();
            Box::pin(async move { Ok(func_local(PyroValue::deserialize(a.clone())?).serialize()?) })
        }));

        let r#type = TypeBuilder(()).func_of::<A>().result_of::<B>();

        self.commands.push(Command::CreateSymbol {
            name: name.as_ref().to_string(),
            r#type,
            repr,
        });

        self
    }

    pub fn register_function_2<F, A, B, C>(mut self, name: impl AsRef<str>, func: F) -> Self
    where
        F: Fn(A, B) -> C + Send + Sync + 'static,
        A: PyroValue,
        B: PyroValue,
        C: PyroValue,
    {
        let func = Arc::new(func);
        let repr = RuntimeValue::Fun(Arc::new(move |a| {
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
                            PyroValue::deserialize(a_2.clone())?,
                            PyroValue::deserialize(b.clone())?,
                        )
                        .serialize()?)
                    })
                })))
            })
        }));

        let r#type = TypeBuilder(())
            .func_of::<A>()
            .param_of::<B>()
            .result_of::<C>();

        self.commands.push(Command::CreateSymbol {
            name: name.as_ref().to_string(),
            r#type,
            repr,
        });

        self
    }

    pub fn register_type<T: PyroType>(mut self, name: impl AsRef<str>) -> Self {
        self.commands.push(Command::CreateType {
            name: name.as_ref().to_string(),
            command: T::r#type(TypeBuilder(())).0,
        });
        self
    }

    pub fn register_value<T: PyroValue>(mut self, name: impl AsRef<str>, value: T) -> Self {
        self.commands.push(Command::CreateSymbol {
            name: name.as_ref().to_string(),
            r#type: T::r#type(TypeBuilder(())),
            repr: value.serialize().expect(
                "We expect register_value to not fail when serializing into a runtime value",
            ),
        });

        self
    }

    pub fn env(mut self, env: Env) -> Self {
        self.env = Some(env);
        self
    }

    pub fn stdlib(mut self, env: Env) -> Self {
        let print_type = TypeBuilder(())
            .constr("Client")
            .for_all_type()
            .with_constraint("Show")
            .done();

        self.commands.push(Command::CreateSymbol {
            name: "print".to_string(),
            r#type: print_type,
            repr: RuntimeValue::Channel(Channel::Client(env.stdout())),
        });

        self.env = Some(env);

        self.register_function_2("+", |a: i64, b: i64| a + b)
            .register_function_2("-", |a: i64, b: i64| a - b)
            .register_function_2("*", |a: i64, b: i64| a * b)
            .register_function_2("<=", |a: i64, b: i64| a <= b)
            .register_function_2("<", |a: i64, b: i64| a < b)
            .register_function_2(">=", |a: i64, b: i64| a >= b)
            .register_function_2(">", |a: i64, b: i64| a > b)
            .register_function_2("==", |a: i64, b: i64| a == b)
            .register_function_2("&&", |a, b| a && b)
            .register_function_2("||", |a, b| a || b)
    }

    pub fn build(self) -> eyre::Result<Engine> {
        let mut runtime_values = HashMap::<String, RuntimeValue>::new();
        let mut knowledge = Knowledge::standard();

        for cmd in self.commands {
            match cmd {
                Command::CreateType { name, command } => {
                    let pointer = handle_builder_type_command(&mut knowledge, command)?;
                    knowledge.declare_from_pointer(&STDLIB, name, pointer);
                }

                Command::CreateSymbol { name, r#type, repr } => {
                    let pointer = handle_builder_type_command(&mut knowledge, r#type.0)?;
                    knowledge.declare_from_pointer(&STDLIB, name.as_str(), pointer);
                    runtime_values.insert(name, repr);
                }
            }
        }

        let runtime = Runtime {
            env: self.env,
            variables: runtime_values,
            used: Default::default(),
        };

        Ok(Engine { runtime, knowledge })
    }
}

fn handle_builder_type_command(
    knowledge: &mut Knowledge,
    cmd: TypeCommand,
) -> eyre::Result<TypePointer> {
    match cmd {
        TypeCommand::LookUp(name) => match knowledge.look_up(&STDLIB, name.as_str()) {
            Some(pointer) => Ok(pointer),
            None => eyre::bail!("Type '{}' doesn't exist", name),
        },

        TypeCommand::CreateType { name, constraints } => Ok(knowledge.declare_from_dict(
            &STDLIB,
            name.as_str(),
            Dict::with_impls(Type::named(name.as_str()), constraints),
        )),

        TypeCommand::CreateForAll { constraints } => {
            let scope = knowledge.new_scope(&STDLIB);
            let var = knowledge.new_generic(&scope);
            let mut constraint_pointers = Vec::new();
            let body = if constraints.is_empty() {
                var.clone()
            } else {
                for constraint in constraints {
                    if let Some(constraint) = knowledge.look_up(&STDLIB, &constraint) {
                        constraint_pointers.push(TypePointer::app(constraint, var.clone()));
                        continue;
                    }

                    eyre::bail!("Constraint '{}' doesn't exist", constraint);
                }

                TypePointer::Qual(constraint_pointers, Box::new(var.clone()))
            };

            Ok(TypePointer::ForAll(
                false,
                scope,
                vec![var.as_type_ref().name.clone()],
                Box::new(body),
            ))
        }

        TypeCommand::CreateFunc { mut params } => {
            let mut result = handle_builder_type_command(knowledge, params.pop().unwrap().0)?;

            while let Some(param) = params.pop() {
                result = TypePointer::fun(handle_builder_type_command(knowledge, param.0)?, result);
            }

            Ok(result)
        }

        TypeCommand::CreateTypeConstr { constr, inner_type } => {
            let constr = if let Some(p) = knowledge.look_up(&STDLIB, &constr) {
                p
            } else {
                eyre::bail!("Type constructor '{}' doesn't exist", constr);
            };

            let inner = match inner_type {
                InnerType::Declared(cmd) => handle_builder_type_command(knowledge, *cmd)?,

                InnerType::ForAll { constraints } => {
                    let scope = knowledge.new_scope(&STDLIB);
                    let var = knowledge.new_generic(&scope);
                    let mut constraint_pointers = Vec::new();
                    let body = if constraints.is_empty() {
                        var.clone()
                    } else {
                        for constraint in constraints {
                            if let Some(constraint) = knowledge.look_up(&STDLIB, &constraint) {
                                constraint_pointers.push(TypePointer::app(constraint, var.clone()));
                                continue;
                            }

                            eyre::bail!("Constraint '{}' doesn't exist", constraint);
                        }

                        TypePointer::Qual(constraint_pointers, Box::new(var.clone()))
                    };

                    TypePointer::ForAll(
                        false,
                        scope,
                        vec![var.as_type_ref().name.clone()],
                        Box::new(body),
                    )
                }
            };

            Ok(TypePointer::app(constr, inner))
        }

        TypeCommand::Rec { props } => {
            let mut new_props = Vec::new();

            for prop in props {
                new_props
                    .push(prop.traverse_result(|d| handle_builder_type_command(knowledge, d.0))?);
            }

            Ok(TypePointer::rec(new_props))
        }
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

    pub fn compile(&self, source_code: &str) -> eyre::Result<PyroProcess> {
        let mut this = self.clone();
        let program = pyro_core::parse(&mut this.context(), source_code)?;

        Ok(PyroProcess {
            runtime: self.runtime.clone(),
            program,
        })
    }
}

pub struct PyroProcess {
    runtime: Runtime,
    program: Vec<Tag<Proc<Ann>, Ann>>,
}

impl PyroProcess {
    pub async fn run(self) -> eyre::Result<()> {
        let mut work_items = Vec::new();
        let (sender, mut mailbox) = mpsc::unbounded_channel::<Msg>();

        for tag in self.program {
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
