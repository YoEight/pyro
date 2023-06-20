#[cfg(test)]
mod tests;

use std::collections::{HashMap, HashSet};

use crate::ast::{Abs, Decl, Def, Pat, PatVar, Prop, Record, Var};
use crate::ast::{Proc, Program, Tag, Val};
use crate::context::{LocalScope, STDLIB};
use crate::sym::Literal;
use crate::typing::{Knowledge, Type, TypeInfo};
use crate::{not_implement, record_label_not_found, type_error, Ctx, Error, Pos, Result};

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
pub enum ValCtx {
    Input,
    Output,
    Regular,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Ann {
    pub pos: Pos,
    pub r#type: Type,
    pub used: HashSet<String>,
}

impl Ann {
    pub fn with_type(r#type: Type, pos: Pos) -> Self {
        Self {
            pos,
            r#type,
            used: Default::default(),
        }
    }
}

pub fn annotate_program(mut ctx: Knowledge, prog: Program<TypeInfo>) -> Result<Program<Ann>> {
    let mut annotated = Vec::new();

    for proc in prog.procs {
        annotated.push(annotate_proc(&mut ctx, proc)?);
    }

    Ok(Program { procs: annotated })
}

fn annotate_proc(
    ctx: &mut Knowledge,
    proc: Tag<Proc<TypeInfo>, TypeInfo>,
) -> eyre::Result<Tag<Proc<Ann>, Ann>> {
    match proc.item {
        Proc::Output(l, r) => {
            let mut ann = Ann::with_type(Type::process(), proc.tag.pos);
            let mut l = annotate_val(ctx, l)?;
            let mut r = annotate_val(ctx, r)?;

            if !ctx.implements(&proc.tag.scope, &l.tag.r#type, "Send") {
                return not_implement(proc.tag.pos, &l.tag.r#type, "Send");
            }

            if !ctx.param_matches(&proc.tag.scope, l.tag.r#type.inner(), &r.tag.r#type) {
                return type_error(proc.tag.pos, l.tag.r#type.inner(), &r.tag.r#type);
            }

            Ok(Tag {
                tag: ann,
                item: Proc::Output(l, r),
            })
        }

        Proc::Input(l, r) => {
            let ann = Ann::with_type(Type::process(), proc.tag.pos);
            let l = annotate_val(ctx, l)?;
            let r = annotate_abs(ctx, r)?;

            if !ctx.implements(&proc.tag.scope, &l.tag.r#type, "Receive") {
                return not_implement(proc.tag.pos, &l.tag.r#type, "Receive");
            }

            if !ctx.param_matches(&proc.tag.scope, l.tag.r#type.inner(), &r.tag.r#type) {
                return type_error(proc.tag.pos, l.tag.r#type.inner(), &r.tag.r#type);
            }

            Ok(Tag {
                tag: ann,
                item: Proc::Input(l, r),
            })
        }

        Proc::Null => Ok(Tag {
            item: Proc::Null,
            tag: Ann::with_type(Type::process(), proc.tag.pos),
        }),

        Proc::Parallel(ps) => {
            let mut ann = Ann::with_type(Type::process(), proc.tag.pos);
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
            let d = annotate_decl(ctx, &scope, d)?;
            let new_scope = ctx.new_scope(&scope);
            let p = annotate_proc(ctx, new_scope, *p)?;
            let mut tag = p.tag.clone();
            tag.r#type = Type::process();

            Ok(Tag {
                item: Proc::Decl(d, Box::new(p)),
                tag,
            })
        }

        Proc::Cond(val, if_proc, else_proc) => {
            let val = annotate_val(ctx, &scope, ValCtx::Regular, val)?;

            if !Type::bool().parent_type_of(&val.tag.r#type) {
                return Err(Error {
                    pos: val.tag.pos,
                    message: format!("Expected type 'Bool' but got '{}' instead", val.tag.r#type),
                });
            }

            let mut ann = Ann::with_type(Type::process(), proc.tag);
            let new_scope = ctx.new_scope(&scope);
            let if_proc = annotate_proc(ctx, new_scope, *if_proc)?;
            let new_scope = ctx.new_scope(&scope);
            let else_proc = annotate_proc(ctx, new_scope, *else_proc)?;

            ann.used.extend(val.tag.used.clone());
            ann.used.extend(if_proc.tag.used.clone());
            ann.used.extend(else_proc.tag.used.clone());

            Ok(Tag {
                item: Proc::Cond(val, Box::new(if_proc), Box::new(else_proc)),
                tag: ann,
            })
        }
    }
}

fn update_val_type_info(
    ctx: &mut Ctx,
    scope: &LocalScope,
    node: &mut Tag<Val<Ann>, Ann>,
    inferred_type: Type,
) {
    if let Val::Path(p) = &node.item {
        update_path_type_info(ctx, scope, p, inferred_type.clone());
    } else {
        panic!("not implemented for {}", node.item);
    }

    node.tag.r#type = inferred_type;
}

fn update_path_type_info(
    ctx: &mut Ctx,
    scope: &LocalScope,
    path: &Vec<String>,
    inferred_type: Type,
) {
    // TODO - I'm pretty sure this code won't work with record projection, for example x.y.
    match path.as_slice() {
        [ident] => {
            ctx.update(scope, ident, inferred_type);
        }
        _ => todo!(),
    }
}

fn annotate_abs(
    ctx: &mut Knowledge,
    tag: Tag<Abs<TypeInfo>, TypeInfo>,
) -> Result<Tag<Abs<Ann>, Ann>> {
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

fn update_pattern_type_info(ctx: &mut Ctx, scope: &LocalScope, node: &mut Tag<Pat<Ann>, Ann>) {
    match &mut node.item {
        Pat::Var(var) => {
            update_pat_var_type_info(ctx, scope, var);
            // FIXME - The way we propagate type information is horrendous, we need to find a beter
            // way to prevent so weird mutable updates.
            node.tag.r#type = var.var.r#type.clone();
        }

        Pat::Record(rec) => {
            // FIXME - Jeez that function is ugly as hell!!!
            node.tag.r#type = update_pat_rec_type_info(ctx, scope, rec);
        }

        Pat::Wildcard(_) => {}
    }
}

fn update_pat_rec_type_info(
    ctx: &mut Ctx,
    scope: &LocalScope,
    rec: &mut Record<Tag<Pat<Ann>, Ann>>,
) -> Type {
    rec.for_each(|node| update_pattern_type_info(ctx, scope, node));
    let r#type = rec.clone().map(|node| node.tag.r#type);

    Type::Record(r#type)
}

fn update_pat_var_type_info(ctx: &mut Ctx, scope: &LocalScope, pat_var: &mut PatVar<Ann>) {
    let r#type = ctx.look_up(scope, &pat_var.var.id).unwrap();
    pat_var.var.r#type = r#type.clone();
}

pub fn annotate_decl(
    ctx: &mut Ctx,
    scope: &LocalScope,
    decl: Tag<Decl<Pos>, Pos>,
) -> Result<Tag<Decl<Ann>, Ann>> {
    let ann = Ann::with_type(Type::process(), decl.tag);
    let item = match decl.item {
        Decl::Channels(cs) => {
            let mut chans = Vec::new();

            for decl_tag in cs {
                let (n, t) = decl_tag.item;
                let t = resolve_type(&ctx, &scope, decl.tag, t)?;

                if !ctx.declare(scope, &n, t.clone()) {
                    return Err(Error {
                        pos: decl_tag.tag,
                        message: format!("Channel '{}' already exists", n),
                    });
                }

                chans.push(Tag {
                    item: (n, t.clone()),
                    tag: Ann::with_type(t, decl_tag.tag),
                });
            }

            Decl::Channels(chans)
        }

        Decl::Type(n, t) => {
            let resolved_type = resolve_type(&ctx, &scope, decl.tag, t)?;
            ctx.declare(scope, &n, resolved_type.clone());
            Decl::Type(n, resolved_type)
        }

        Decl::Def(defs) => {
            let mut new_defs = Vec::new();
            for def_tag in defs {
                let (r#type, abs) = annotate_abs(
                    ctx,
                    scope,
                    def_tag.item.abs,
                    Some(def_tag.item.name.clone()),
                )?;

                new_defs.push(Tag {
                    item: Def {
                        name: def_tag.item.name,
                        abs,
                    },
                    tag: Ann::with_type(r#type, def_tag.tag),
                });
            }

            Decl::Def(new_defs)
        }
    };

    Ok(Tag { item, tag: ann })
}

fn resolve_type(ctx: &Ctx, scope: &LocalScope, pos: Pos, r#type: Type) -> Result<Type> {
    match r#type {
        Type::Name {
            name,
            generic: false,
            ..
        } => {
            if let Some(rn) = ctx.look_up(scope, &name) {
                Ok(rn.clone())
            } else {
                Err(Error {
                    pos,
                    message: format!("Unknown type '{}'", name),
                })
            }
        }

        Type::Record(rec) => {
            let rec = rec.traverse_result(|t| resolve_type(ctx, scope, pos, t))?;

            Ok(Type::Record(rec))
        }

        Type::App(cons, end) => {
            let cons = resolve_type(ctx, scope, pos, *cons)?;
            let end = resolve_type(ctx, scope, pos, *end)?;

            Ok(Type::App(Box::new(cons), Box::new(end)))
        }

        _ => Ok(r#type),
    }
}

fn literal_type(lit: &Literal) -> Type {
    match lit {
        Literal::Integer(_) => Type::integer(),
        Literal::String(_) => Type::string(),
        Literal::Char(_) => Type::char(),
        Literal::Bool(_) => Type::bool(),
    }
}

fn annotate_pattern(
    ctx: &mut Knowledge,
    tag: Tag<Pat<TypeInfo>, TypeInfo>,
) -> eyre::Result<Tag<Pat<Ann>, Ann>> {
    match tag.item {
        Pat::Var(v) => {
            let pat_tag = annotate_pat_var(ctx, v)?;

            let r#type = if let Some(pat) = &pat_tag.item.pattern {
                if !ctx.param_matches(&tag.tag.scope, &pat_tag.tag.r#type, &pat.tag.r#type) {
                    return type_error(tag.tag.pos, pat_tag.tag.r#type, pat.tag.r#type);
                }

                pat.tag.r#type.clone()
            } else {
                &pat_tag.tag.r#type.clone()
            };

            Ok(Tag {
                item: Pat::Var(pat_tag.item),
                tag: Ann::with_type(r#type, tag.tag.pos),
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

            let r#type = Type::Record(Record { props: props_type });
            Ok(Tag {
                item: Pat::Record(Record { props }),
                tag: Ann::with_type(r#type, tag.tag.pos),
            })
        }

        Pat::Wildcard(t) => Ok(Tag {
            item: Pat::Wildcard(t.clone()),
            tag: Ann::with_type(ctx.project_type(&tag.tag.pointer), tag.tag.pos),
        }),
    }
}
fn annotate_pat_var(
    ctx: &mut Knowledge,
    pat_var: PatVar<TypeInfo>,
) -> Result<Tag<PatVar<Ann>, Ann>> {
    let pattern = if let Some(param) = pat_var.pattern {
        Some(Box::new(annotate_pattern(ctx, *param)?))
    } else {
        None
    };

    let var = Var {
        id: pat_var.var.id,
        r#type: pat_var.var.r#type,
        tag: Ann::with_type(
            ctx.project_type(&pat_var.var.tag.pointer),
            pat_var.var.tag.pos,
        ),
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
) -> Result<Tag<Val<Ann>, Ann>> {
    match lit.item {
        Val::Literal(l) => {
            let r#type = Type::from_literal(&l);

            Ok(Tag {
                item: Val::Literal(l),
                tag: Ann::with_type(r#type, lit.tag.pos),
            })
        }

        Val::Path(p) => {
            let mut path = p.clone();
            path.reverse();
            let name = path.pop().unwrap();
            let r#type = ctx.project_type(&lit.tag.pointer);
            let r#type = if let Type::Record(mut rec) = r#type {
                let mut temp = None;

                while let Some(frag) = path.pop() {
                    if let Some(prop) = rec.find_by_prop(&frag) {
                        if let Type::Record(inner) = prop.val {
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
                    Type::Record(rec)
                }
            } else {
                r#type
            };

            let mut ann = Ann::with_type(r#type, lit.tag.pos);

            Ok(Tag {
                item: Val::Path(p),
                tag: ann,
            })
        }

        Val::Record(xs) => {
            let mut props = Vec::new();
            let mut types = Vec::new();
            let mut used = HashMap::new();

            for prop in xs.props {
                let val = annotate_val(ctx, prop.val)?;

                used.extend(val.tag.used.clone());

                types.push(Prop {
                    label: prop.label.clone(),
                    val: val.tag.r#type.clone(),
                });

                props.push(Prop {
                    label: prop.label,
                    val,
                });
            }

            let mut ann = Ann::with_type(Type::Record(Record { props: types }), lit.tag.pos);
            Ok(Tag {
                item: Val::Record(Record { props }),
                tag: ann,
            })
        }

        Val::AnoClient(abs) => {
            let (r#type, abs) = annotate_abs(ctx, scope, abs, None)?;
            let mut ann = abs.tag.clone();

            ann.r#type = Type::client(r#type);

            Ok(Tag {
                item: Val::AnoClient(abs),
                tag: ann,
            })
        }

        Val::App(caller, param) => {
            let caller = annotate_val(ctx, &scope, val_ctx, *caller)?;
            let param = annotate_val(ctx, &scope, val_ctx, *param)?;

            if !caller.tag.r#type.meets_requirement(&Type::func_type()) {
                return Err(Error {
                    pos: caller.tag.pos,
                    message: format!(
                        "Expected a function but got '{}' instead",
                        caller.tag.r#type,
                    ),
                });
            }

            let result_type = if let Some(result_type) = caller.tag.r#type.apply(&param.tag.r#type)
            {
                result_type
            } else {
                return Err(Error {
                    pos: param.tag.pos,
                    message: format!(
                        "Expected type '{}' but got '{}' instead",
                        caller.tag.r#type, param.tag.r#type
                    ),
                });
            };

            let mut ann = Ann::with_type(result_type, caller.tag.pos);

            ann.used.extend(caller.tag.used.clone());
            ann.used.extend(param.tag.used.clone());

            Ok(Tag {
                item: Val::App(Box::new(caller), Box::new(param)),
                tag: ann,
            })
        }
    }
}
