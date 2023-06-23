use crate::ast::{Abs, Decl, Def, Pat, PatVar, Prop, Record, Var};
use crate::ast::{Proc, Program, Tag, Val};
use crate::context::LocalScope;
use crate::typing::{Knowledge, Type, TypeInfo, TypePointer};
use crate::{not_a_function, not_implement, record_label_not_found, type_error, Pos};

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum ValCtx {
    Input,
    Output,
    Regular,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Ann {
    pub pos: Pos,
    pub scope: LocalScope,
    pub r#type: TypePointer,
}

impl Ann {
    pub fn with_type(r#type: TypePointer, tag: TypeInfo) -> Self {
        Self {
            pos: tag.pos,
            r#type,
            scope: tag.scope,
        }
    }
}

pub fn annotate_program(
    ctx: &mut Knowledge,
    prog: Program<TypeInfo>,
) -> eyre::Result<Program<Ann>> {
    let mut annotated = Vec::new();

    for proc in prog.procs {
        annotated.push(annotate_proc(ctx, proc)?);
    }

    Ok(Program { procs: annotated })
}

fn annotate_proc(
    ctx: &mut Knowledge,
    proc: Tag<Proc<TypeInfo>, TypeInfo>,
) -> eyre::Result<Tag<Proc<Ann>, Ann>> {
    match proc.item {
        Proc::Output(l, r) => {
            let l = annotate_val(ctx, l)?;
            let r = annotate_val(ctx, r)?;

            if !ctx.implements(&l.tag.r#type, "Send") {
                return not_implement(
                    proc.tag.pos,
                    &ctx.project_type(&l.tag.r#type).r#type,
                    "Send",
                );
            }

            if let TypePointer::App(_, inner) = ctx.follow_link(&l.tag.r#type) {
                if !ctx.param_matches(inner.as_ref(), &r.tag.r#type) {
                    let inner = ctx.project_type(inner.as_ref());
                    return type_error(
                        proc.tag.pos,
                        &inner.r#type,
                        &ctx.project_type(&r.tag.r#type).r#type,
                    );
                }
            } else {
                unreachable!()
            }

            Ok(Tag {
                tag: Ann::with_type(ctx.process_pointer(), proc.tag),
                item: Proc::Output(l, r),
            })
        }

        Proc::Input(l, r) => {
            let l = annotate_val(ctx, l)?;
            let r = annotate_abs(ctx, r)?;

            if !ctx.implements(&l.tag.r#type, "Receive") {
                return not_implement(
                    proc.tag.pos,
                    &ctx.project_type(&l.tag.r#type).r#type,
                    "Receive",
                );
            }

            if let TypePointer::App(_, inner) = ctx.follow_link(&l.tag.r#type) {
                if !ctx.param_matches(inner.as_ref(), &r.tag.r#type) {
                    let inner = ctx.project_type(inner.as_ref());
                    return type_error(
                        proc.tag.pos,
                        &inner.r#type,
                        &ctx.project_type(&r.tag.r#type).r#type,
                    );
                }
            } else {
                unreachable!()
            }

            Ok(Tag {
                tag: Ann::with_type(ctx.process_pointer(), proc.tag),
                item: Proc::Input(l, r),
            })
        }

        Proc::Null => Ok(Tag {
            item: Proc::Null,
            tag: Ann::with_type(ctx.process_pointer(), proc.tag),
        }),

        Proc::Parallel(ps) => {
            let ann = Ann::with_type(ctx.process_pointer(), proc.tag);
            let mut new_ps = Vec::new();

            for p in ps {
                let proc = annotate_proc(ctx, p)?;

                new_ps.push(proc);
            }

            Ok(Tag {
                item: Proc::Parallel(new_ps),
                tag: ann,
            })
        }

        Proc::Decl(d, p) => {
            let d = annotate_decl(ctx, d)?;
            let p = annotate_proc(ctx, *p)?;

            Ok(Tag {
                item: Proc::Decl(d, Box::new(p)),
                tag: Ann::with_type(ctx.process_pointer(), proc.tag),
            })
        }

        Proc::Cond(val, if_proc, else_proc) => {
            let val = annotate_val(ctx, val)?;

            if !ctx.param_matches(&ctx.bool_pointer(), &val.tag.r#type) {
                return type_error(
                    val.tag.pos,
                    &Type::bool(),
                    &ctx.project_type(&val.tag.r#type).r#type,
                );
            }

            let ann = Ann::with_type(ctx.process_pointer(), proc.tag);
            let if_proc = annotate_proc(ctx, *if_proc)?;
            let else_proc = annotate_proc(ctx, *else_proc)?;

            Ok(Tag {
                item: Proc::Cond(val, Box::new(if_proc), Box::new(else_proc)),
                tag: ann,
            })
        }
    }
}

fn annotate_abs(
    ctx: &mut Knowledge,
    tag: Tag<Abs<TypeInfo>, TypeInfo>,
) -> eyre::Result<Tag<Abs<Ann>, Ann>> {
    let pattern = annotate_pattern(ctx, tag.item.pattern)?;
    let proc = annotate_proc(ctx, *tag.item.proc)?;
    let ann = pattern.tag.clone();

    Ok(Tag {
        item: Abs {
            pattern,
            proc: Box::new(proc),
        },
        tag: ann,
    })
}

pub fn annotate_decl(
    ctx: &mut Knowledge,
    decl: Tag<Decl<TypeInfo>, TypeInfo>,
) -> eyre::Result<Tag<Decl<Ann>, Ann>> {
    let ann = Ann::with_type(ctx.process_pointer(), decl.tag.clone());
    let item = match decl.item {
        Decl::Channels(cs) => {
            let mut chans = Vec::new();

            for decl_tag in cs {
                chans.push(Tag {
                    item: decl_tag.item,
                    tag: Ann::with_type(decl_tag.tag.pointer.clone(), decl_tag.tag),
                });
            }

            Decl::Channels(chans)
        }

        Decl::Type(n, t) => Decl::Type(n, t),

        Decl::Def(defs) => {
            let mut new_defs = Vec::new();
            for def_tag in defs {
                let abs = annotate_abs(ctx, def_tag.item.abs)?;
                new_defs.push(Tag {
                    item: Def {
                        name: def_tag.item.name,
                        abs,
                    },
                    tag: Ann::with_type(def_tag.tag.pointer.clone(), def_tag.tag),
                });
            }

            Decl::Def(new_defs)
        }
    };

    Ok(Tag { item, tag: ann })
}

fn annotate_pattern(
    ctx: &mut Knowledge,
    tag: Tag<Pat<TypeInfo>, TypeInfo>,
) -> eyre::Result<Tag<Pat<Ann>, Ann>> {
    match tag.item {
        Pat::Var(v) => {
            let pat_tag_pointer = v.var.tag.pointer.clone();
            let pat_tag_pat_pointer = v.pattern.clone();
            let pat_tag = annotate_pat_var(ctx, v)?;

            let r#type = if let Some((pat, pat_dict)) = pat_tag_pat_pointer
                .as_ref()
                .zip(pat_tag.item.pattern.as_ref())
            {
                if !ctx.param_matches(&pat_tag_pointer, &pat.tag.pointer) {
                    return type_error(
                        tag.tag.pos,
                        &ctx.project_type(&pat_tag.tag.r#type).r#type,
                        &ctx.project_type(&pat_dict.as_ref().tag.r#type).r#type,
                    );
                }

                pat_dict.tag.r#type.clone()
            } else {
                pat_tag.tag.r#type.clone()
            };

            Ok(Tag {
                item: Pat::Var(pat_tag.item),
                tag: Ann::with_type(r#type, tag.tag),
            })
        }

        Pat::Record(rec) => {
            let mut props = Vec::new();
            let mut props_type = Vec::new();

            for prop in rec.props {
                let val = annotate_pattern(ctx, prop.val)?;

                props_type.push(Prop {
                    label: prop.label.clone(),
                    val: val.tag.r#type.clone(),
                });

                props.push(Prop {
                    label: prop.label,
                    val,
                });
            }

            let r#type = TypePointer::rec(props_type);

            Ok(Tag {
                item: Pat::Record(Record { props }),
                tag: Ann::with_type(r#type, tag.tag),
            })
        }

        Pat::Wildcard(t) => Ok(Tag {
            item: Pat::Wildcard(t.clone()),
            tag: Ann::with_type(tag.tag.pointer.clone(), tag.tag),
        }),
    }
}
fn annotate_pat_var(
    ctx: &mut Knowledge,
    pat_var: PatVar<TypeInfo>,
) -> eyre::Result<Tag<PatVar<Ann>, Ann>> {
    let pattern = if let Some(param) = pat_var.pattern {
        Some(Box::new(annotate_pattern(ctx, *param)?))
    } else {
        None
    };

    let var = Var {
        id: pat_var.var.id,
        r#type: pat_var.var.r#type,
        tag: Ann::with_type(pat_var.var.tag.pointer.clone(), pat_var.var.tag),
    };

    let tag = var.tag.clone();
    Ok(Tag {
        item: PatVar { var, pattern },
        tag,
    })
}

pub fn annotate_val(
    ctx: &mut Knowledge,
    lit: Tag<Val<TypeInfo>, TypeInfo>,
) -> eyre::Result<Tag<Val<Ann>, Ann>> {
    match lit.item {
        Val::Literal(l) => Ok(Tag {
            item: Val::Literal(l),
            tag: Ann::with_type(lit.tag.pointer.clone(), lit.tag),
        }),

        Val::Path(p) => {
            let mut path = p.clone();
            path.reverse();
            let name = path.pop().unwrap();
            ctx.register_used_variable(&lit.tag.scope, &name);
            let dict = if let TypePointer::Rec(mut rec) = lit.tag.pointer.clone() {
                let mut temp = None;

                while let Some(frag) = path.pop() {
                    if let Some(prop) = rec.find_by_prop(&frag) {
                        if let TypePointer::Rec(inner) = prop.val {
                            rec = inner;
                            continue;
                        }

                        temp = Some(prop.val);
                        break;
                    }

                    return record_label_not_found(lit.tag.pos, &name)?;
                }

                if let Some(r#type) = temp {
                    r#type
                } else {
                    TypePointer::Rec(rec)
                }
            } else {
                lit.tag.pointer.clone()
            };

            // FIXME - This code is wrong but doesn't seem to be needed.
            let ann = Ann::with_type(dict, lit.tag);

            Ok(Tag {
                item: Val::Path(p),
                tag: ann,
            })
        }

        Val::Record(xs) => {
            let mut props = Vec::new();
            let mut types = Vec::new();

            for prop in xs.props {
                let val = annotate_val(ctx, prop.val)?;

                types.push(Prop {
                    label: prop.label.clone(),
                    val: val.tag.r#type.clone(),
                });

                props.push(Prop {
                    label: prop.label,
                    val,
                });
            }

            let ann = Ann::with_type(TypePointer::rec(types), lit.tag);
            Ok(Tag {
                item: Val::Record(Record { props }),
                tag: ann,
            })
        }

        Val::AnoClient(abs) => {
            let abs = annotate_abs(ctx, abs)?;
            let r#type = abs.tag.r#type.clone();

            Ok(Tag {
                item: Val::AnoClient(abs),
                tag: Ann::with_type(r#type, lit.tag),
            })
        }

        Val::App(caller, param) => {
            let caller = annotate_val(ctx, *caller)?;
            let param = annotate_val(ctx, *param)?;

            let result_type = if let TypePointer::Fun(expect, result) =
                ctx.follow_link(&caller.tag.r#type)
            {
                if !ctx.param_matches(expect.as_ref(), &param.tag.r#type) {
                    let expectation = ctx.project_type(expect.as_ref());
                    return type_error(
                        lit.tag.pos,
                        &expectation.r#type,
                        &ctx.project_type(&param.tag.r#type).r#type,
                    );
                }

                result.as_ref().clone()
            } else {
                return not_a_function(lit.tag.pos, &ctx.project_type(&caller.tag.r#type).r#type);
            };

            let ann = Ann::with_type(result_type, lit.tag);

            Ok(Tag {
                item: Val::App(Box::new(caller), Box::new(param)),
                tag: ann,
            })
        }
    }
}
