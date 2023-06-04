#[cfg(test)]
mod tests;

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

fn generate_generic_type_name(mut n: usize) -> String {
    let mut result = String::new();
    loop {
        let ch = ((n % 26) as u8 + b'a') as char;
        result.push(ch);
        n /= 26;
        if n > 0 {
            n -= 1;
        } else {
            break;
        }
    }

    result.chars().rev().collect::<String>()
}

#[derive(Default)]
struct Ctx {
    scope_id: u32,
    variables_new: HashMap<u32, HashMap<String, Type>>,
}

impl Ctx {
    pub fn new_scope(&mut self, parent: &Scoped) -> Scoped {
        self.scope_id += 1;
        let id = self.scope_id;
        let mut ancestors = parent.ancestors.clone();

        ancestors.push(id);

        Scoped { ancestors }
    }

    pub fn look_up(&self, scope: &Scoped, name: &str) -> Option<Type> {
        let mut ancestors = scope.ancestors.clone();
        while let Some(scope_id) = ancestors.pop() {
            if let Some(variables) = self.variables_new.get(&scope_id) {
                if let Some(r#type) = variables.get(name) {
                    return Some(r#type.clone());
                }
            }
        }

        None
    }

    pub fn declare(&mut self, scope: &Scoped, name: impl AsRef<str>, r#type: Type) -> bool {
        let scope_id = scope.ancestors.last().copied().unwrap();
        let variables = self.variables_new.entry(scope_id).or_default();

        if variables.contains_key(name.as_ref()) {
            return false;
        }

        variables.insert(name.as_ref().to_string(), r#type);

        true
    }

    pub fn update(&mut self, scope: &Scoped, name: impl AsRef<str>, r#type: Type) {
        let mut ancestors = scope.ancestors.clone();
        while let Some(scope_id) = ancestors.pop() {
            if let Some(variables) = self.variables_new.get_mut(&scope_id) {
                variables.insert(name.as_ref().to_string(), r#type);
                return;
            }
        }
    }
}

#[derive(Default, Clone)]
struct Scoped {
    ancestors: Vec<u32>,
}

impl Scoped {
    pub fn std() -> Self {
        Self { ancestors: vec![0] }
    }
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

fn configure_context(ctx: &mut Ctx, pi_std: &Scoped) {
    ctx.declare(&pi_std, "print", Type::client(Type::show()));
    ctx.declare(
        pi_std,
        "+",
        Type::func(
            Type::integer(),
            Type::func(Type::integer(), Type::integer()),
        ),
    );
    ctx.declare(
        pi_std,
        "<=",
        Type::func(Type::integer(), Type::func(Type::integer(), Type::bool())),
    );

    ctx.declare(pi_std, "String", Type::string());
    ctx.declare(pi_std, "Bool", Type::bool());
    ctx.declare(pi_std, "Integer", Type::integer());
    ctx.declare(pi_std, "Char", Type::char());
    ctx.declare(pi_std, "Client", Type::client_type());
    ctx.declare(pi_std, "Server", Type::server_type());
    ctx.declare(pi_std, "Channel", Type::channel_type());
}

pub fn annotate_program(prog: Program<Pos>) -> Result<Program<Ann>> {
    let mut annotated = Vec::new();
    let mut ctx = Ctx::default();
    let pi_std = Scoped::std();

    configure_context(&mut ctx, &pi_std);

    for proc in prog.procs {
        let new_scope = ctx.new_scope(&pi_std);
        annotated.push(annotate_proc(&mut ctx, new_scope, proc)?);
    }

    Ok(Program { procs: annotated })
}

fn annotate_proc(
    ctx: &mut Ctx,
    scope: Scoped,
    proc: Tag<Proc<Pos>, Pos>,
) -> Result<Tag<Proc<Ann>, Ann>> {
    match proc.item {
        Proc::Output(l, r) => {
            let mut ann = Ann::with_type(Type::process(), proc.tag);
            let mut l = annotate_val(ctx, &scope, ValCtx::Output, l)?;
            let mut r = annotate_val(ctx, &scope, ValCtx::Regular, r)?;

            if !l.tag.r#type.typecheck_client_call(&r.tag.r#type) {
                return Err(Error {
                    pos: r.tag.pos,
                    message: format!(
                        "Expected type '{}' but got '{}' instead",
                        l.tag.r#type.inner_type(),
                        r.tag.r#type
                    ),
                });
            }

            if l.tag.r#type == Type::Anonymous {
                let inferred_type = Type::client(r.tag.r#type.clone());
                l.tag.r#type = inferred_type.clone();
                update_val_type_info(ctx, &scope, &mut l, inferred_type.clone());
            } else if r.tag.r#type == Type::Anonymous {
                let inferred_type = l.tag.r#type.inner_type();
                r.tag.r#type = inferred_type.clone();
                update_val_type_info(ctx, &scope, &mut r, inferred_type.clone());
            }

            ann.used.extend(l.tag.used.clone());
            ann.used.extend(r.tag.used.clone());

            Ok(Tag {
                tag: ann,
                item: Proc::Output(l, r),
            })
        }

        Proc::Input(l, r) => {
            let mut ann = Ann::with_type(Type::process(), proc.tag);
            let mut l = annotate_val(ctx, &scope, ValCtx::Input, l)?;
            let (pat_type, mut r) = annotate_abs(ctx, &scope, r, None)?;

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

            if l.tag.r#type == Type::Anonymous {
                let inferred_type = Type::server(r.tag.r#type.clone());
                update_val_type_info(ctx, &scope, &mut l, inferred_type.clone());
            } else if r.tag.r#type == Type::Anonymous {
                let inferred_type = l.tag.r#type.inner_type();
                r.tag.r#type = inferred_type;
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
                let new_scope = ctx.new_scope(&scope);
                let proc = annotate_proc(ctx, new_scope, p)?;

                ann.used.extend(proc.tag.used.clone());
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
    scope: &Scoped,
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

fn update_path_type_info(ctx: &mut Ctx, scope: &Scoped, path: &Vec<String>, inferred_type: Type) {
    // TODO - I'm pretty sure this code won't work with record projection, for example x.y.
    match path.as_slice() {
        [ident] => {
            ctx.update(scope, ident, inferred_type);
        }
        _ => todo!(),
    }
}

fn annotate_abs(
    ctx: &mut Ctx,
    scope: &Scoped,
    tag: Tag<Abs<Pos>, Pos>,
    named: Option<String>,
) -> Result<(Type, Tag<Abs<Ann>, Ann>)> {
    let mut pattern = annotate_pattern(ctx, &scope, tag.item.pattern)?;

    if let Some(def_name) = named {
        if !ctx.declare(&scope, &def_name, Type::client(pattern.tag.r#type.clone())) {
            return Err(Error {
                pos: tag.tag,
                message: format!("Definition '{}' already exists", def_name),
            });
        }
    }

    let new_scope = ctx.new_scope(&scope);
    let proc = annotate_proc(ctx, new_scope, *tag.item.proc)?;
    let mut ann = proc.tag.clone();

    update_pattern_type_info(ctx, scope, &mut pattern);

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

fn update_pattern_type_info(ctx: &mut Ctx, scope: &Scoped, node: &mut Tag<Pat<Ann>, Ann>) {
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
    scope: &Scoped,
    rec: &mut Record<Tag<Pat<Ann>, Ann>>,
) -> Type {
    rec.for_each(|node| update_pattern_type_info(ctx, scope, node));
    let r#type = rec.clone().map(|node| node.tag.r#type);

    Type::Record(r#type)
}

fn update_pat_var_type_info(ctx: &mut Ctx, scope: &Scoped, pat_var: &mut PatVar<Ann>) {
    let r#type = ctx.look_up(scope, &pat_var.var.id).unwrap();
    pat_var.var.r#type = r#type.clone();
}

fn annotate_decl(
    ctx: &mut Ctx,
    scope: &Scoped,
    decl: Tag<Decl<Pos>, Pos>,
) -> Result<Tag<Decl<Ann>, Ann>> {
    let ann = Ann::new(decl.tag);
    let item = match decl.item {
        Decl::Channels(cs) => {
            let mut chans = Vec::new();

            for decl_tag in cs {
                let (n, t) = decl_tag.item;
                let t = resolve_type(&ctx, &scope, decl.tag, t)?;

                if !ctx.declare(&scope, &n, t.clone()) {
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
            ctx.declare(&scope, &n, resolved_type.clone());
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

fn resolve_type(ctx: &Ctx, scope: &Scoped, pos: Pos, r#type: Type) -> Result<Type> {
    match r#type {
        Type::Name { name, .. } => {
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
    ctx: &mut Ctx,
    scope: &Scoped,
    tag: Tag<Pat<Pos>, Pos>,
) -> Result<Tag<Pat<Ann>, Ann>> {
    match tag.item {
        Pat::Var(v) => {
            let pat_tag = annotate_pat_var(ctx, scope, tag.tag, v)?;

            Ok(Tag {
                item: Pat::Var(pat_tag.item),
                tag: pat_tag.tag,
            })
        }

        Pat::Record(rec) => {
            let mut props = Vec::new();
            let mut props_type = Vec::new();

            for prop in rec.props {
                let val = annotate_pattern(ctx, scope, prop.val)?;

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
            let t = resolve_type(&ctx, scope, tag.tag, t)?;

            Ok(Tag {
                item: Pat::Wildcard(t.clone()),
                tag: Ann::with_type(t, tag.tag),
            })
        }
    }
}
fn annotate_pat_var(
    ctx: &mut Ctx,
    scope: &Scoped,
    pos: Pos,
    mut pat_var: PatVar<Pos>,
) -> Result<Tag<PatVar<Ann>, Ann>> {
    let (r#type, pattern) = if let Some(pat) = pat_var.pattern {
        let param = Tag {
            item: *pat,
            tag: pos,
        };

        pat_var.var.r#type = resolve_type(&ctx, scope, pos, pat_var.var.r#type)?;
        let pat = annotate_pattern(ctx, scope, param)?;

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
        pat_var.var.r#type = resolve_type(&ctx, scope, pos, pat_var.var.r#type.clone())?;
        (pat_var.var.r#type.clone(), None)
    };

    if !ctx.declare(scope, &pat_var.var.id, r#type.clone()) {
        return Err(Error {
            pos,
            message: format!("Variable '{}' already exists", pat_var.var.id),
        });
    }

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
    scope: &Scoped,
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
                let r#type = if let Some(r#type) = ctx.look_up(scope, name) {
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
                let r#type = if let Some(r#type) = ctx.look_up(scope, &name) {
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
                let val = annotate_val(ctx, &scope, ValCtx::Regular, prop.val)?;

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

            if !caller.tag.r#type.ancestor_tree_contains(&Type::func_type()) {
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
