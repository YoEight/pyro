use crate::ast::{Abs, Decl, Pat, PatVar, Proc, Program, Prop, Record, Tag, Val};
use crate::context::Scope;
use crate::sym::Literal;
use crate::typing::{Dict, Knowledge, Type, TypeRef};
use crate::Pos;

#[derive(Clone)]
pub enum TypePointer {
    Ref(TypeRef),
    Rec(Record<TypePointer>),
    Fun(Box<TypePointer>, Box<TypePointer>),
    App(Box<TypePointer>, Box<TypePointer>),
}

#[derive(Clone)]
pub struct TypeInfo {
    pointer: TypePointer,
}

pub fn infer_program<S: Scope>(
    know: &mut Knowledge,
    scope: &S,
    prog: Program<Pos>,
) -> eyre::Result<Program<TypeInfo>> {
    let mut procs = Vec::new();

    for proc in prog.procs {
        procs.push(infer_proc(know, scope, proc)?);
    }

    Ok(Program { procs })
}

pub fn infer_proc<S: Scope>(
    know: &mut Knowledge,
    scope: &S,
    proc: Tag<Proc<Pos>, Pos>,
) -> eyre::Result<Tag<Proc<TypeInfo>, TypeInfo>> {
    match proc.item {
        Proc::Output(target, param) => {
            let target = infer_val(know, scope, target)?;
            let param = infer_val(know, scope, param)?;

            Ok(Tag {
                item: Proc::Output(target, param),
                tag: TypeInfo {
                    pointer: TypePointer::Ref(know.process_type_ref()),
                },
            })
        }

        Proc::Input(source, event) => {
            let source = infer_val(know, scope, source)?;
            let event = infer_abs(know, scope, event)?;

            Ok(Tag {
                item: Proc::Input(source, event),
                tag: TypeInfo {
                    pointer: TypePointer::Ref(know.process_type_ref()),
                },
            })
        }

        Proc::Null => Ok(Tag {
            item: Proc::Null,
            tag: TypeInfo {
                pointer: TypePointer::Ref(know.process_type_ref()),
            },
        }),

        Proc::Parallel(ps) => {
            let mut new_ps = Vec::new();

            for p in ps {
                new_ps.push(infer_proc(know, scope, p)?);
            }

            Ok(Tag {
                item: Proc::Parallel(new_ps),
                tag: TypeInfo {
                    pointer: TypePointer::Ref(know.process_type_ref()),
                },
            })
        }

        Proc::Decl(def, local) => {
            let def = infer_decl(know, scope, def)?;
            let local = infer_proc(know, scope, *local)?;

            Ok(Tag {
                item: Proc::Decl(def, Box::new(local)),
                tag: TypeInfo {
                    pointer: TypePointer::Ref(know.process_type_ref()),
                },
            })
        }

        Proc::Cond(cond, if_proc, else_proc) => {
            let cond = infer_val(know, scope, cond)?;
            let if_proc = infer_proc(know, scope, *if_proc)?;
            let else_proc = infer_proc(know, scope, *else_proc)?;

            Ok(Tag {
                item: Proc::Cond(cond, Box::new(if_proc), Box::new(else_proc)),
                tag: TypeInfo {
                    pointer: TypePointer::Ref(know.process_type_ref()),
                },
            })
        }
    }
}

pub fn infer_val<S: Scope>(
    know: &mut Knowledge,
    scope: &S,
    val: Tag<Val<Pos>, Pos>,
) -> eyre::Result<Tag<Val<TypeInfo>, TypeInfo>> {
    match val.item {
        Val::Literal(l) => {
            let r#type = match &l {
                Literal::Integer(_) => know.integer_type_ref(),
                Literal::String(_) => know.string_type_ref(),
                Literal::Char(_) => know.char_type_ref(),
                Literal::Bool(_) => know.bool_type_ref(),
            };

            Ok(Tag {
                item: Val::Literal(l),
                tag: TypeInfo {
                    pointer: TypePointer::Ref(r#type),
                },
            })
        }

        Val::Path(ps) => {
            // TODO - I want to support path projection (ex x.y) as a typeclass. At the end, we
            // won't have path expressed as a vec.
            let name = ps.first().unwrap();

            let r#type = if let Some(type_ref) = know.look_up(scope, name) {
                type_ref
            } else {
                return variable_not_found(val.tag, name);
            };

            Ok(Tag {
                item: Val::Path(ps),
                tag: TypeInfo {
                    pointer: TypePointer::Ref(r#type),
                },
            })
        }

        Val::Record(rec) => {
            let mut props = Vec::new();
            let mut types = Vec::new();
            for prop in rec.props {
                let val = infer_val(know, scope, prop.val)?;

                types.push(Prop {
                    label: prop.label.clone(),
                    val: val.tag.pointer.clone(),
                });

                props.push(Prop {
                    label: prop.label,
                    val,
                })
            }

            Ok(Tag {
                item: Val::Record(Record { props }),
                tag: TypeInfo {
                    pointer: TypePointer::Rec(Record { props: types }),
                },
            })
        }

        Val::App(param, result) => {
            let param = infer_val(know, scope, *param)?;
            let result = infer_val(know, scope, *result)?;
            let pointer = TypePointer::Fun(
                Box::new(param.tag.pointer.clone()),
                Box::new(result.tag.pointer.clone()),
            );

            Ok(Tag {
                item: Val::App(Box::new(param), Box::new(result)),
                tag: TypeInfo { pointer },
            })
        }

        Val::AnoClient(abs) => {
            let abs = infer_abs(know, scope, abs)?;
            let pointer = TypePointer::App(
                Box::new(TypePointer::Ref(know.client_type_ref())),
                Box::new(abs.tag.pointer.clone()),
            );

            Ok(Tag {
                item: Val::AnoClient(abs),
                tag: TypeInfo { pointer },
            })
        }
    }
}

pub fn infer_abs<S: Scope>(
    know: &mut Knowledge,
    scope: &S,
    abs: Tag<Abs<Pos>, Pos>,
) -> eyre::Result<Tag<Abs<TypeInfo>, TypeInfo>> {
    let new_scope = know.new_scope(scope);
    let pattern = infer_pattern(know, &new_scope, abs.item.pattern)?;
    let proc = infer_proc(know, &new_scope, *abs.item.proc)?;
    let pointer = pattern.tag.pointer.clone();

    Ok(Tag {
        item: Abs {
            pattern,
            proc: Box::new(proc),
        },
        tag: TypeInfo { pointer },
    })
}

fn infer_pattern<S: Scope>(
    know: &mut Knowledge,
    scope: &S,
    pat: Tag<Pat<Pos>, Pos>,
) -> eyre::Result<Tag<Pat<TypeInfo>, TypeInfo>> {
    match pat.item {
        Pat::Var(var) => {
            // TODO - Don't forget to adapt the type you code in the pattern matching.
            let type_ref = know.new_generic(scope);
            let pat_ref = know.declare(scope, &var.var.id, know.dict(&type_ref).clone());
            let pattern = if let Some(pattern) = var.pattern {
                // TODO - Fix the lexer so we get the proper tag at that level.
                let node = Tag {
                    item: *pattern,
                    tag: pat.tag,
                };

                let pattern = infer_pattern(know, scope, node)?;
                Some(Box::new(pattern.item))
            } else {
                None
            };

            Ok(Tag {
                item: Pat::Var(PatVar {
                    var: var.var,
                    pattern,
                }),
                tag: TypeInfo {
                    pointer: TypePointer::Ref(pat_ref),
                },
            })
        }

        Pat::Record(rec) => {
            let mut props = Vec::new();
            let mut types = Vec::new();

            for prop in rec.props {
                let value = infer_pattern(know, scope, prop.val)?;

                types.push(Prop {
                    label: prop.label.clone(),
                    val: value.tag.pointer.clone(),
                });

                props.push(Prop {
                    label: prop.label,
                    val: value,
                });
            }

            Ok(Tag {
                item: Pat::Record(Record { props }),
                tag: TypeInfo {
                    pointer: TypePointer::Rec(Record { props: types }),
                },
            })
        }

        Pat::Wildcard(t) => {
            // TODO - Don't forget to adapt the type you code in the pattern matching.
            let type_ref = know.new_generic(scope);

            Ok(Tag {
                item: Pat::Wildcard(t),
                tag: TypeInfo {
                    pointer: TypePointer::Ref(type_ref),
                },
            })
        }
    }
}

fn infer_decl<S: Scope>(
    know: &mut Knowledge,
    scope: &S,
    decl: Tag<Decl<Pos>, Pos>,
) -> eyre::Result<Tag<Decl<TypeInfo>, TypeInfo>> {
    let decl = match decl.item {
        Decl::Channels(cs) => {
            let mut new_cs = Vec::new();
            for dec in cs {
                // TODO - Don't forget to adapt when you move from the old Type type.
                let type_ref = know.new_generic(scope);
                let channel_ref = know.channel_type_ref();
                let mut channel_dict = know.dict(&channel_ref).clone();
                channel_dict.r#type = Type::channel(know.dict(&type_ref).r#type.clone());
                let this_ref = know.declare(scope, &dec.item.0, channel_dict);

                new_cs.push(Tag {
                    item: dec.item,
                    tag: TypeInfo {
                        pointer: TypePointer::Ref(this_ref),
                    },
                })
            }

            Decl::Channels(new_cs)
        }
        Decl::Def(_) => {
            todo!()
        }
        Decl::Type(_, _) => {
            todo!()
        }
    };

    todo!()
}

fn variable_not_found<A>(pos: Pos, name: &str) -> eyre::Result<A> {
    eyre::bail!(
        "{}:{}: Variable '{}' is not found",
        pos.line,
        pos.column,
        name
    )
}
