use crate::ast::{Abs, Decl, Pat, PatVar, Proc, Program, Prop, Record, Tag, Val, Var};
use crate::context::Scope;
use crate::sym::{Literal, TypeSym};
use crate::typing::{Dict, Knowledge, Type, TypeInfo, TypePointer, TypeRef};
use crate::{type_not_found, variable_not_found, Pos};

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
                    pos: proc.tag,
                    scope: scope.as_local(),
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
                    pos: proc.tag,
                    scope: scope.as_local(),
                    pointer: TypePointer::Ref(know.process_type_ref()),
                },
            })
        }

        Proc::Null => Ok(Tag {
            item: Proc::Null,
            tag: TypeInfo {
                pos: proc.tag,
                scope: scope.as_local(),
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
                    pos: proc.tag,
                    scope: scope.as_local(),
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
                    pos: proc.tag,
                    scope: scope.as_local(),
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
                    pos: proc.tag,
                    scope: scope.as_local(),
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
                    pos: val.tag,
                    scope: scope.as_local(),
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
                    pos: val.tag,
                    scope: scope.as_local(),
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
                    pos: val.tag,
                    scope: scope.as_local(),
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
                tag: TypeInfo {
                    pos: val.tag,
                    scope: scope.as_local(),
                    pointer,
                },
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
                tag: TypeInfo {
                    pos: val.tag,
                    scope: scope.as_local(),
                    pointer,
                },
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
        tag: TypeInfo {
            pos: abs.tag,
            scope: scope.as_local(),
            pointer,
        },
    })
}

fn infer_pattern<S: Scope>(
    know: &mut Knowledge,
    scope: &S,
    pat: Tag<Pat<Pos>, Pos>,
) -> eyre::Result<Tag<Pat<TypeInfo>, TypeInfo>> {
    match pat.item {
        Pat::Var(var) => {
            let projected_ref = match know.project_type_pointer(scope, &var.var.r#type) {
                Ok(p) => p,
                Err(n) => type_not_found(pat.tag, n.as_str())?,
            };

            let pat_ref = know.declare(
                scope,
                &var.var.id,
                know.project_type_pointer_dict(&projected_ref),
            );

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
                    pos: pat.tag,
                    scope: scope.as_local(),
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
                    pos: pat.tag,
                    scope: scope.as_local(),
                    pointer: TypePointer::Rec(Record { props: types }),
                },
            })
        }

        Pat::Wildcard(t) => {
            let pointer = match know.project_type_pointer(scope, &t) {
                Ok(p) => p,
                Err(n) => type_not_found(pat.tag, n.as_str())?,
            };

            Ok(Tag {
                item: Pat::Wildcard(t),
                tag: TypeInfo {
                    pos: pat.tag,
                    scope: scope.as_local(),
                    pointer,
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
    let pos = decl.tag;
    let decl = match decl.item {
        Decl::Channels(cs) => {
            let mut new_cs = Vec::new();
            for dec in cs {
                let projected_pointer = match know.project_type_pointer(scope, &dec.item.1) {
                    Ok(p) => p,
                    Err(n) => type_not_found(dec.tag, n.as_str())?,
                };

                let type_ref = know.declare(
                    scope,
                    &dec.item.0,
                    know.project_type_pointer_dict(&projected_pointer),
                );

                new_cs.push(Tag {
                    item: dec.item,
                    tag: TypeInfo {
                        pos: decl.tag,
                        scope: scope.as_local(),
                        pointer: TypePointer::Ref(type_ref),
                    },
                })
            }

            Decl::Channels(new_cs)
        }

        Decl::Def(defs) => {
            let mut new_defs = Vec::new();

            for def in defs {
                let abs = infer_abs(know, scope, def.item.abs)?;
                let pointer = TypePointer::App(
                    Box::new(TypePointer::Ref(know.client_type_ref())),
                    Box::new(abs.tag.pointer.clone()),
                );

                know.declare(
                    scope,
                    &def.item.name,
                    know.project_type_pointer_dict(&pointer),
                );
            }

            Decl::Def(new_defs)
        }

        Decl::Type(name, r#type) => {
            let pointer = match know.project_type_pointer(scope, &r#type) {
                Ok(p) => p,
                Err(n) => type_not_found(decl.tag, n.as_str())?,
            };

            know.declare(scope, &name, know.project_type_pointer_dict(&pointer));
            Decl::Type(name, r#type)
        }
    };

    Ok(Tag {
        item: decl,
        tag: TypeInfo {
            pos,
            scope: scope.as_local(),
            pointer: TypePointer::Ref(know.process_type_ref()),
        },
    })
}
