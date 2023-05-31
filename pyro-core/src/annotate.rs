use std::collections::HashMap;

use crate::ast::{Abs, Decl, Def, Pat, PatVar, Prop, Record, Type};
use crate::ast::{Proc, Program, Tag, Val};
use crate::sym::Literal;
use crate::Result;
use crate::{Error, Pos};

#[derive(Clone, Debug, Copy, PartialEq, Eq)]
enum ValCtx {
    Input,
    Output,
    Regular,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Ann {
    pub pos: Pos,
    pub r#type: Type,
    pub used: HashMap<String, Type>,
}

#[derive(Default, Clone)]
struct Ctx {
    variables: HashMap<String, Type>,
    types: HashMap<String, Type>,
}

impl Ann {
    pub fn with_type(r#type: Type, pos: Pos) -> Self {
        Self {
            pos,
            r#type,
            used: Default::default(),
        }
    }

    pub fn new(pos: Pos) -> Self {
        Ann::with_type(Type::Anonymous, pos)
    }
}

fn configure_context() -> Ctx {
    let mut ctx = Ctx::default();

    ctx.variables
        .insert("print".to_string(), Type::client(Type::string()));

    ctx.types.insert("String".to_string(), Type::string());
    ctx.types.insert("Bool".to_string(), Type::bool());
    ctx.types.insert("Integer".to_string(), Type::integer());
    ctx.types.insert("Char".to_string(), Type::char());
    ctx.types.insert("Client".to_string(), Type::client_type());
    ctx.types.insert("Server".to_string(), Type::server_type());
    ctx.types
        .insert("Channel".to_string(), Type::channel_type());

    ctx
}

pub fn annotate_program(prog: Program<Pos>) -> Result<Program<Ann>> {
    let mut annotated = Vec::new();
    let ctx = configure_context();

    for proc in prog.procs {
        annotated.push(annotate_proc(ctx.clone(), proc)?);
    }

    Ok(Program { procs: annotated })
}

fn annotate_proc(mut ctx: Ctx, proc: Tag<Proc<Pos>, Pos>) -> Result<Tag<Proc<Ann>, Ann>> {
    match proc.item {
        Proc::Output(l, r) => {
            let mut ann = Ann::with_type(Type::process(), proc.tag);
            let l = annotate_val(&mut ctx, ValCtx::Output, l)?;
            let r = annotate_val(&mut ctx, ValCtx::Regular, r)?;

            ann.used.extend(l.tag.used.clone());
            ann.used.extend(r.tag.used.clone());

            if !l.tag.r#type.typecheck_client_call(&r.tag.r#type) {
                return Err(Error {
                    pos: r.tag.pos,
                    message: format!(
                        "Expected type '{}' but got '{}' instead",
                        l.tag.r#type, r.tag.r#type
                    ),
                });
            }

            Ok(Tag {
                tag: ann,
                item: Proc::Output(l, r),
            })
        }

        Proc::Input(l, r) => {
            let mut ann = Ann::with_type(Type::process(), proc.tag);
            let l = annotate_val(&mut ctx, ValCtx::Input, l)?;
            let (pat_type, r) = annotate_abs(&mut ctx, r)?;

            ann.used.extend(l.tag.used.clone());
            ann.used.extend(r.tag.used.clone());

            if !l.tag.r#type.typecheck_server_call(&pat_type) {
                return Err(Error {
                    pos: r.tag.pos,
                    message: format!(
                        "Expected type '{}' but got '{}' instead",
                        l.tag.r#type, pat_type
                    ),
                });
            }

            Ok(Tag {
                tag: ann,
                item: Proc::Input(l, r),
            })
        }

        Proc::Null => Ok(Tag {
            item: Proc::Null,
            tag: Ann::with_type(Type::process(), proc.tag),
        }),

        Proc::Parallel(ps) => {
            let mut ann = Ann::with_type(Type::process(), proc.tag);
            let mut new_ps = Vec::new();

            for p in ps {
                let proc = annotate_proc(ctx.clone(), p)?;

                ann.used.extend(proc.tag.used.clone());
                new_ps.push(proc);
            }

            Ok(Tag {
                item: Proc::Parallel(new_ps),
                tag: ann,
            })
        }

        Proc::Decl(d, p) => {
            let d = annotate_decl(&mut ctx, d)?;
            let p = annotate_proc(ctx.clone(), *p)?;
            let mut tag = p.tag.clone();
            tag.r#type = Type::process();

            Ok(Tag {
                item: Proc::Decl(d, Box::new(p)),
                tag,
            })
        }

        Proc::Cond(val, if_proc, else_proc) => {
            let val = annotate_val(&mut ctx, ValCtx::Regular, val)?;

            if !Type::bool().parent_type_of(&val.tag.r#type) {
                return Err(Error {
                    pos: val.tag.pos,
                    message: format!("Expected type 'Bool' but got '{}' instead", val.tag.r#type),
                });
            }

            let mut ann = Ann::with_type(Type::process(), proc.tag);
            let if_proc = annotate_proc(ctx.clone(), *if_proc)?;
            let else_proc = annotate_proc(ctx.clone(), *else_proc)?;

            ann.used.extend(if_proc.tag.used.clone());
            ann.used.extend(else_proc.tag.used.clone());

            Ok(Tag {
                item: Proc::Cond(val, Box::new(if_proc), Box::new(else_proc)),
                tag: ann,
            })
        }
    }
}

fn annotate_abs(ctx: &mut Ctx, tag: Tag<Abs<Pos>, Pos>) -> Result<(Type, Tag<Abs<Ann>, Ann>)> {
    let pattern = annotate_pattern(ctx, tag.item.pattern)?;
    let proc = annotate_proc(ctx.clone(), *tag.item.proc)?;
    let mut ann = proc.tag.clone();

    ann.pos = tag.tag;
    ann.used.extend(pattern.tag.used.clone());

    Ok((
        pattern.tag.r#type.clone(),
        Tag {
            item: Abs {
                pattern,
                proc: Box::new(proc),
            },
            tag: ann,
        },
    ))
}

fn annotate_decl(ctx: &mut Ctx, decl: Tag<Decl<Pos>, Pos>) -> Result<Tag<Decl<Ann>, Ann>> {
    let ann = Ann::new(decl.tag);
    let item = match decl.item {
        Decl::Channels(cs) => {
            let mut chans = Vec::new();

            for (n, t) in cs {
                let t = resolve_type(&ctx, decl.tag, t)?;
                ctx.variables.insert(n.clone(), t.clone());

                chans.push((n, t));
            }

            Decl::Channels(chans)
        }

        Decl::Type(n, t) => {
            let resolved_type = resolve_type(&ctx, decl.tag, t)?;
            ctx.types.insert(n.clone(), resolved_type.clone());
            Decl::Type(n, resolved_type)
        }

        Decl::Def(defs) => {
            let mut new_defs = Vec::new();
            for def in defs {
                let (r#type, abs) = annotate_abs(ctx, def.abs)?;

                ctx.variables.insert(def.name.clone(), Type::client(r#type));

                new_defs.push(Def {
                    name: def.name,
                    abs,
                });
            }

            Decl::Def(new_defs)
        }
    };

    Ok(Tag { item, tag: ann })
}

fn resolve_type(ctx: &Ctx, pos: Pos, r#type: Type) -> Result<Type> {
    match r#type.name() {
        Some(n) => {
            if let Some(rn) = ctx.types.get(n) {
                Ok(rn.clone())
            } else {
                Err(Error {
                    pos,
                    message: format!("Unknown type '{}'", n),
                })
            }
        }

        _ => {
            if let Type::Record(rec) = r#type {
                let rec = rec.traverse_result(|t| resolve_type(ctx, pos, t))?;

                Ok(Type::Record(rec))
            } else {
                Ok(r#type)
            }
        }
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

fn annotate_pattern(ctx: &mut Ctx, tag: Tag<Pat<Pos>, Pos>) -> Result<Tag<Pat<Ann>, Ann>> {
    match tag.item {
        Pat::Var(v) => {
            let pat_tag = annotate_pat_var(ctx, tag.tag, v)?;

            Ok(Tag {
                item: Pat::Var(pat_tag.item),
                tag: pat_tag.tag,
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
                tag: Ann::with_type(r#type, tag.tag),
            })
        }

        Pat::Wildcard(t) => {
            let t = resolve_type(&ctx, tag.tag, t)?;

            Ok(Tag {
                item: Pat::Wildcard(t.clone()),
                tag: Ann::with_type(t, tag.tag),
            })
        }
    }
}
fn annotate_pat_var(
    ctx: &mut Ctx,
    pos: Pos,
    mut pat_var: PatVar<Pos>,
) -> Result<Tag<PatVar<Ann>, Ann>> {
    let (r#type, pattern) = if let Some(pat) = pat_var.pattern {
        let param = Tag {
            item: *pat,
            tag: pos,
        };

        pat_var.var.r#type = resolve_type(&ctx, pos, pat_var.var.r#type)?;
        let pat = annotate_pattern(ctx, param)?;

        if pat_var.var.r#type != Type::Anonymous
            && !pat_var.var.r#type.parent_type_of(&pat.tag.r#type)
        {
            return Err(Error {
                pos,
                message: format!(
                    "Expected type '{}' but got '{}' instead",
                    pat_var.var.r#type, pat.tag.r#type
                ),
            });
        }

        (pat.tag.r#type, Some(Box::new(pat.item)))
    } else {
        (resolve_type(&ctx, pos, pat_var.var.r#type.clone())?, None)
    };

    ctx.variables.insert(pat_var.var.id.clone(), r#type.clone());

    Ok(Tag {
        item: PatVar {
            var: pat_var.var,
            pattern,
        },
        tag: Ann::with_type(r#type.clone(), pos),
    })
}

fn annotate_val(
    ctx: &mut Ctx,
    val_ctx: ValCtx,
    lit: Tag<Val<Pos>, Pos>,
) -> Result<Tag<Val<Ann>, Ann>> {
    match lit.item {
        Val::Literal(l) => {
            let r#type = literal_type(&l);

            Ok(Tag {
                item: Val::Literal(l),
                tag: Ann::with_type(r#type, lit.tag),
            })
        }

        Val::Path(p) => match val_ctx {
            ValCtx::Input | ValCtx::Output => {
                let name = p.first().unwrap();
                let r#type = if let Some(r#type) = ctx.variables.get(name) {
                    r#type
                } else {
                    return Err(Error {
                        pos: lit.tag,
                        message: format!("Variable '{}' doesn't exist", name),
                    });
                };

                let mut ann = Ann::with_type(r#type.clone(), lit.tag);

                ann.used.insert(name.clone(), r#type.clone());

                Ok(Tag {
                    item: Val::Path(p),
                    tag: ann,
                })
            }

            ValCtx::Regular => {
                let mut path = p.clone();
                path.reverse();
                let name = path.pop().unwrap();
                let r#type = if let Some(r#type) = ctx.variables.get(&name) {
                    r#type
                } else {
                    return Err(Error {
                        pos: lit.tag,
                        message: format!("Variable '{}' doesn't exist", name),
                    });
                };
                let r#type = if let Type::Record(mut rec) = r#type.clone() {
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

                        return Err(Error {
                            pos: lit.tag,
                            message: format!("Record label '{}' doesn't exist", name),
                        });
                    }

                    if let Some(r#type) = temp {
                        r#type
                    } else {
                        Type::Record(rec)
                    }
                } else {
                    r#type.clone()
                };

                let mut ann = Ann::with_type(r#type.clone(), lit.tag);
                ann.used.insert(name.clone(), r#type);

                Ok(Tag {
                    item: Val::Path(p),
                    tag: ann,
                })
            }
        },

        Val::Record(xs) => {
            let mut props = Vec::new();
            let mut types = Vec::new();
            let mut used = HashMap::new();

            for prop in xs.props {
                let val = annotate_val(ctx, ValCtx::Regular, prop.val)?;

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

            let mut ann = Ann::with_type(Type::Record(Record { props: types }), lit.tag);
            ann.used = used;
            Ok(Tag {
                item: Val::Record(Record { props }),
                tag: ann,
            })
        }

        Val::AnoFun(abs) => {
            let (r#type, abs) = annotate_abs(ctx, abs)?;
            let mut ann = abs.tag.clone();

            ann.r#type = r#type;

            Ok(Tag {
                item: Val::AnoFun(abs),
                tag: ann,
            })
        }
    }
}
