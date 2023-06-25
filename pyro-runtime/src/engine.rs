use crate::env::Env;
use crate::runtime::Runtime;
use crate::value::{Channel, RuntimeValue};
use crate::PyroValue;
use pyro_core::annotate::Ann;
use pyro_core::ast::{Abs, Pat, Proc, Prop, Record, Tag, Val};
use pyro_core::{Knowledge, PyroType, TypePointer, STDLIB};
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

#[derive(Clone)]
pub struct EngineBuilder {
    knowledge: Knowledge,
    runtime_values: HashMap<String, RuntimeValue>,
    env: Option<Env>,
}

impl Default for EngineBuilder {
    fn default() -> Self {
        Self {
            knowledge: Knowledge::standard(),
            runtime_values: Default::default(),
            env: None,
        }
    }
}

impl EngineBuilder {
    pub fn register_function<F, A, B>(
        mut self,
        name: impl AsRef<str>,
        func: F,
    ) -> eyre::Result<Self>
    where
        F: Fn(A) -> B + Send + Sync + 'static,
        A: PyroValue,
        B: PyroValue,
    {
        let func = Arc::new(func);
        let value = RuntimeValue::Fun(Arc::new(move |a| {
            let a = Arc::new(a);
            let func_local = func.clone();
            Box::pin(async move { Ok(func_local(PyroValue::deserialize(a.clone())?).serialize()?) })
        }));

        let r#type = self
            .knowledge
            .type_builder()
            .func_of::<A>()?
            .result_of::<B>()?;

        self.knowledge.declare_from_pointer(&STDLIB, &name, r#type);
        self.runtime_values.insert(name.as_ref().to_string(), value);

        Ok(self)
    }

    pub fn register_function_2<F, A, B, C>(
        mut self,
        name: impl AsRef<str>,
        func: F,
    ) -> eyre::Result<Self>
    where
        F: Fn(A, B) -> C + Send + Sync + 'static,
        A: PyroValue,
        B: PyroValue,
        C: PyroValue,
    {
        let func = Arc::new(func);
        let value = RuntimeValue::Fun(Arc::new(move |a| {
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

        let r#type = self
            .knowledge
            .type_builder()
            .func_of::<A>()?
            .param_of::<B>()?
            .result_of::<C>()?;

        self.knowledge.declare_from_pointer(&STDLIB, &name, r#type);
        self.runtime_values.insert(name.as_ref().to_string(), value);

        Ok(self)
    }

    pub fn register_type<T: PyroType>(mut self, name: impl AsRef<str>) -> eyre::Result<Self> {
        let pointer = T::r#type(&self.knowledge.type_builder())?;

        if let TypePointer::Ref(_) = &pointer {
            return Ok(self);
        } else {
            self.knowledge.declare_from_pointer(&STDLIB, name, pointer);
        }

        Ok(self)
    }

    pub fn register_value<T: PyroValue>(
        mut self,
        name: impl AsRef<str>,
        value: T,
    ) -> eyre::Result<Self> {
        let pointer = T::r#type(&self.knowledge.type_builder())?;
        self.knowledge
            .declare_from_pointer(&STDLIB, name.as_ref(), pointer);
        self.runtime_values
            .insert(name.as_ref().to_string(), value.serialize()?);

        Ok(self)
    }

    pub fn env(mut self, env: Env) -> Self {
        self.env = Some(env);
        self
    }

    pub fn stdlib(mut self, env: Env) -> eyre::Result<Self> {
        let print_type = self
            .knowledge
            .type_builder()
            .type_constructor("Client")?
            .for_all("'a")
            .add_constraint("Show")?
            .done();

        self.knowledge
            .declare_from_pointer(&STDLIB, "print", print_type);

        self.runtime_values.insert(
            "print".to_string(),
            RuntimeValue::Channel(Channel::Client(env.stdout())),
        );

        self.env = Some(env);

        self.register_function_2("+", |a: i64, b: i64| a + b)?
            .register_function_2("-", |a: i64, b: i64| a - b)?
            .register_function_2("*", |a: i64, b: i64| a * b)?
            .register_function_2("<=", |a: i64, b: i64| a <= b)?
            .register_function_2("<", |a: i64, b: i64| a < b)?
            .register_function_2(">=", |a: i64, b: i64| a >= b)?
            .register_function_2(">", |a: i64, b: i64| a > b)?
            .register_function_2("==", |a: i64, b: i64| a == b)?
            .register_function_2("&&", |a, b| a && b)?
            .register_function_2("||", |a, b| a || b)
    }

    pub fn build(self) -> Engine {
        let runtime = Runtime {
            env: self.env,
            variables: self.runtime_values,
            used: Default::default(),
        };

        Engine {
            runtime,
            knowledge: self.knowledge,
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
