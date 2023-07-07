use crate::ast::{Abs, Decl, Def, Pat, PatVar, Proc, Program, Prop, Record, Tag, Val, Var};
use crate::sym::Literal;
use crate::typing::{Machine, TypeInfo, TypePointer, TypeSystem};
use crate::{type_not_found, variable_not_found, Pos};

pub fn infer_program<M: Machine>(
    know: &mut TypeSystem<M>,
    prog: Program<Pos>,
) -> eyre::Result<Program<TypeInfo>> {
    let mut procs = Vec::new();

    for proc in prog.procs {
        procs.push(infer_proc(know, proc)?);
    }

    Ok(Program { procs })
}

pub fn infer_proc<M: Machine>(
    know: &mut TypeSystem<M>,
    proc: Tag<Proc<Pos>, Pos>,
) -> eyre::Result<Tag<Proc<TypeInfo>, TypeInfo>> {
    match proc.item {
        Proc::Output(target, param) => {
            let target = infer_val(know, target)?;
            let param = infer_val(know, param)?;

            // TODO - Improve the algo so the system infer that `Send` is a constraint for an higher
            // kinded type.
            know.suggest_constraint(&target.tag.pointer, "Send");
            know.suggest_inner_type(&target.tag.pointer, &param.tag.pointer);

            if let Some((_, inner)) = know.as_type_constructor(&target.tag.pointer) {
                know.suggest_type(&param.tag.pointer, &inner);
            }

            Ok(Tag {
                item: Proc::Output(target, param),
                tag: TypeInfo {
                    pos: proc.tag,
                    pointer: TypePointer::process(),
                },
            })
        }

        Proc::Input(source, event) => {
            let source = infer_val(know, source)?;
            let event = infer_abs(know, event)?;

            // TODO - Improve the algo so the system infer that `Receive` is a constraint for an higher
            // kinded type.
            know.suggest_constraint(&source.tag.pointer, "Receive");
            know.suggest_inner_type(&source.tag.pointer, &event.tag.pointer);

            if let Some((_, inner)) = know.as_type_constructor(&source.tag.pointer) {
                know.suggest_type(&event.tag.pointer, &inner);
            }

            Ok(Tag {
                item: Proc::Input(source, event),
                tag: TypeInfo {
                    pos: proc.tag,
                    pointer: TypePointer::process(),
                },
            })
        }

        Proc::Null => Ok(Tag {
            item: Proc::Null,
            tag: TypeInfo {
                pos: proc.tag,
                pointer: TypePointer::process(),
            },
        }),

        Proc::Parallel(ps) => {
            let mut new_ps = Vec::new();

            for p in ps {
                know.push_scope();
                new_ps.push(infer_proc(know, p)?);
                know.pop_scope();
            }

            Ok(Tag {
                item: Proc::Parallel(new_ps),
                tag: TypeInfo {
                    pos: proc.tag,
                    pointer: TypePointer::process(),
                },
            })
        }

        Proc::Decl(def, local) => {
            let def = infer_decl(know, def)?;
            let local = infer_proc(know, *local)?;

            Ok(Tag {
                item: Proc::Decl(def, Box::new(local)),
                tag: TypeInfo {
                    pos: proc.tag,
                    pointer: TypePointer::process(),
                },
            })
        }

        Proc::Cond(cond, if_proc, else_proc) => {
            let cond = infer_val(know, cond)?;
            let if_proc = infer_proc(know, *if_proc)?;
            let else_proc = infer_proc(know, *else_proc)?;

            Ok(Tag {
                item: Proc::Cond(cond, Box::new(if_proc), Box::new(else_proc)),
                tag: TypeInfo {
                    pos: proc.tag,
                    pointer: TypePointer::process(),
                },
            })
        }
    }
}

pub fn infer_val<M: Machine>(
    know: &mut TypeSystem<M>,
    val: Tag<Val<Pos>, Pos>,
) -> eyre::Result<Tag<Val<TypeInfo>, TypeInfo>> {
    match val.item {
        Val::Literal(l) => {
            let pointer = match &l {
                Literal::Integer(_) => TypePointer::integer(),
                Literal::String(_) => TypePointer::string(),
                Literal::Char(_) => TypePointer::char(),
                Literal::Bool(_) => TypePointer::bool(),
            };

            Ok(Tag {
                item: Val::Literal(l),
                tag: TypeInfo {
                    pos: val.tag,
                    pointer,
                },
            })
        }

        Val::Path(ps) => {
            // TODO - I want to support path projection (ex x.y) as a typeclass. At the end, we
            // won't have path expressed as a vec.
            let name = ps.first().unwrap();

            let scope = know.current_scope();
            let pointer = if let Some(p) = know.look_up(&scope, name) {
                p
            } else {
                return variable_not_found(val.tag, name);
            };

            Ok(Tag {
                item: Val::Path(ps),
                tag: TypeInfo {
                    pos: val.tag,
                    pointer,
                },
            })
        }

        Val::Record(rec) => {
            let mut props = Vec::new();
            let mut types = Vec::new();
            for prop in rec.props {
                let val = infer_val(know, prop.val)?;

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
                    pos: val.tag,
                    pointer: TypePointer::Rec(Record { props: types }),
                },
            })
        }

        Val::App(param, result) => {
            let param = infer_val(know, *param)?;
            let result = infer_val(know, *result)?;
            let pointer = TypePointer::Fun(
                Box::new(param.tag.pointer.clone()),
                Box::new(result.tag.pointer.clone()),
            );

            Ok(Tag {
                item: Val::App(Box::new(param), Box::new(result)),
                tag: TypeInfo {
                    pos: val.tag,
                    pointer,
                },
            })
        }

        Val::AnoClient(abs) => {
            let abs = infer_abs(know, abs)?;
            let pointer = TypePointer::app(TypePointer::client(), abs.tag.pointer.clone());

            Ok(Tag {
                item: Val::AnoClient(abs),
                tag: TypeInfo {
                    pos: val.tag,
                    pointer,
                },
            })
        }
    }
}

pub fn infer_abs<M: Machine>(
    know: &mut TypeSystem<M>,
    abs: Tag<Abs<Pos>, Pos>,
) -> eyre::Result<Tag<Abs<TypeInfo>, TypeInfo>> {
    know.push_scope();

    let pattern = infer_pattern(know, abs.item.pattern)?;
    let proc = infer_proc(know, *abs.item.proc)?;
    let pointer = pattern.tag.pointer.clone();

    know.pop_scope();

    Ok(Tag {
        item: Abs {
            pattern,
            proc: Box::new(proc),
        },
        tag: TypeInfo {
            pos: abs.tag,
            pointer,
        },
    })
}

fn infer_pattern<M: Machine>(
    know: &mut TypeSystem<M>,
    pat: Tag<Pat<Pos>, Pos>,
) -> eyre::Result<Tag<Pat<TypeInfo>, TypeInfo>> {
    let scope = know.current_scope();
    match pat.item {
        Pat::Var(var) => {
            let projected_ref = match know.look_up_type_or_fail(&scope, &var.var.r#type) {
                Ok(p) => p,
                Err(n) => type_not_found(pat.tag, n.as_str())?,
            };

            let pointer = know.declare_from_pointer(&var.var.id, &projected_ref);

            let pattern = if let Some(pattern) = var.pattern {
                let pattern = infer_pattern(know, *pattern)?;
                Some(Box::new(pattern))
            } else {
                None
            };

            Ok(Tag {
                item: Pat::Var(PatVar {
                    var: Var {
                        id: var.var.id,
                        r#type: var.var.r#type,
                        tag: TypeInfo {
                            pos: pat.tag,
                            pointer: pointer.clone(),
                        },
                    },
                    pattern,
                }),
                tag: TypeInfo {
                    pos: pat.tag,
                    pointer,
                },
            })
        }

        Pat::Record(rec) => {
            let mut props = Vec::new();
            let mut types = Vec::new();

            for prop in rec.props {
                let value = infer_pattern(know, prop.val)?;

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
                    pos: pat.tag,
                    pointer: TypePointer::Rec(Record { props: types }),
                },
            })
        }

        Pat::Wildcard(t) => {
            let pointer = match know.look_up_type_or_fail(&scope, &t) {
                Ok(p) => p,
                Err(n) => type_not_found(pat.tag, n.as_str())?,
            };

            Ok(Tag {
                item: Pat::Wildcard(t),
                tag: TypeInfo {
                    pos: pat.tag,
                    pointer,
                },
            })
        }
    }
}

pub fn infer_decl<M: Machine>(
    know: &mut TypeSystem<M>,
    decl: Tag<Decl<Pos>, Pos>,
) -> eyre::Result<Tag<Decl<TypeInfo>, TypeInfo>> {
    let pos = decl.tag;
    let scope = know.current_scope();
    let decl = match decl.item {
        Decl::Channels(cs) => {
            let mut new_cs = Vec::new();
            for dec in cs {
                let projected_pointer = match know.look_up_type_or_fail(&scope, &dec.item.1) {
                    Ok(p) => p,
                    Err(n) => type_not_found(dec.tag, n.as_str())?,
                };

                let pointer = know.declare_from_pointer(&dec.item.0, &projected_pointer);

                new_cs.push(Tag {
                    item: dec.item,
                    tag: TypeInfo {
                        pos: decl.tag,
                        pointer,
                    },
                })
            }

            Decl::Channels(new_cs)
        }

        Decl::Def(defs) => {
            let mut new_defs = Vec::new();
            for def in defs {
                let abs_pos = def.item.abs.tag;
                let prev_scope = know.current_scope();
                let new_scope = know.push_scope();
                let pattern = infer_pattern(know, def.item.abs.item.pattern)?;
                know.set_scope(&prev_scope);
                let projected_type =
                    TypePointer::app(TypePointer::client(), pattern.tag.pointer.clone());
                let pointer = know.declare_from_pointer(&def.item.name, &projected_type);
                let pattern_pointer = pattern.tag.pointer.clone();
                know.set_scope(&new_scope);
                let proc = infer_proc(know, *def.item.abs.item.proc)?;
                know.set_scope(&prev_scope);

                new_defs.push(Tag {
                    item: Def {
                        name: def.item.name,
                        abs: Tag {
                            item: Abs {
                                pattern,
                                proc: Box::new(proc),
                            },
                            tag: TypeInfo {
                                pos: abs_pos,
                                pointer: pattern_pointer,
                            },
                        },
                    },
                    tag: TypeInfo { pos, pointer },
                })
            }

            Decl::Def(new_defs)
        }

        Decl::Type(name, r#type) => {
            let pointer = match know.look_up_type_or_fail(&scope, &r#type) {
                Ok(p) => p,
                Err(n) => type_not_found(decl.tag, n.as_str())?,
            };

            know.declare_from_pointer(&name, &pointer);
            Decl::Type(name, r#type)
        }
    };

    Ok(Tag {
        item: decl,
        tag: TypeInfo {
            pos,
            pointer: TypePointer::process(),
        },
    })
}
