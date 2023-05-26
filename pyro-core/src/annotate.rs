use std::collections::HashMap;

use crate::ast::{Abs, Decl, Def, Pat, Prop, Record, Type};
use crate::sym::Literal;
use crate::{
    ast::{Proc, Program, Tag, Val},
    tokenizer::Pos,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Ann {
    pub pos: Pos,
    pub r#type: Type,
}

#[derive(Default, Clone)]
struct Ctx {
    variables: HashMap<String, Type>,
}

impl Ann {
    pub fn with_type(r#type: Type, pos: Pos) -> Self {
        Self { pos, r#type }
    }

    pub fn new(pos: Pos) -> Self {
        Ann::with_type(Type::Anonymous, pos)
    }
}

pub fn annotate_program(prog: Program<Pos>) -> Program<Ann> {
    let mut annotated = Vec::new();
    let mut ctx = Ctx::default();

    ctx.variables.insert(
        "print".to_string(),
        Type::Channel(Box::new(Type::Name("String".to_string()))),
    );

    for proc in prog.procs {
        annotated.push(annotate_proc(ctx.clone(), proc));
    }

    Program { procs: annotated }
}

fn annotate_proc(mut ctx: Ctx, proc: Tag<Proc<Pos>, Pos>) -> Tag<Proc<Ann>, Ann> {
    match proc.item {
        Proc::Output(l, r) => {
            // TODO - Do an existence checking at that level.
            let l = annotate_val(&mut ctx, l);
            let r = annotate_val(&mut ctx, r);

            Tag {
                tag: Ann {
                    pos: proc.tag,
                    r#type: Type::Process,
                },
                item: Proc::Output(l, r),
            }
        }

        Proc::Input(l, r) => {
            let l = annotate_val_input(&mut ctx, l);
            let r = annotate_abs(&ctx, r);

            Tag {
                tag: Ann {
                    pos: proc.tag,
                    r#type: Type::Process,
                },
                item: Proc::Input(l, r),
            }
        }

        Proc::Null => Tag {
            item: Proc::Null,
            tag: Ann {
                pos: proc.tag,
                r#type: Type::Process,
            },
        },

        Proc::Parallel(ps) => {
            let ann = Ann::with_type(Type::Process, proc.tag);

            let ps = ps
                .into_iter()
                .map(|p| annotate_proc(ctx.clone(), p))
                .collect::<Vec<_>>();

            Tag {
                item: Proc::Parallel(ps),
                tag: ann,
            }
        }

        Proc::Decl(d, p) => {
            let d = annotate_decl(&mut ctx, d);
            let p = annotate_proc(ctx.clone(), p.map_item(|b| *b));
            let p = p.map_item(Box::new);
            let mut tag = p.tag.clone();
            tag.r#type = Type::Process;

            Tag {
                item: Proc::Decl(d, p),
                tag,
            }
        }

        Proc::Cond(_, _, _) => todo!(),
    }
}

fn annotate_abs(ctx: &Ctx, tag: Tag<Abs<Pos>, Pos>) -> Tag<Abs<Ann>, Ann> {
    let proc = annotate_proc(ctx.clone(), *tag.item.proc);
    let mut ann = proc.tag.clone();

    ann.pos = tag.tag;
    ann.r#type = Type::Process;

    Tag {
        item: Abs {
            pattern: tag.item.pattern,
            proc: Box::new(proc),
        },
        tag: ann,
    }
}

fn annotate_decl(ctx: &mut Ctx, decl: Tag<Decl<Pos>, Pos>) -> Tag<Decl<Ann>, Ann> {
    let ann = Ann::new(decl.tag);
    let item = match decl.item {
        Decl::Channel(n, t) => {
            ctx.variables.insert(n.clone(), t.clone());

            Decl::Channel(n, t)
        }

        Decl::Type(n, t) => {
            ctx.variables.insert(n.clone(), t.clone());

            Decl::Type(n, t)
        }

        Decl::Def(defs) => {
            let mut new_defs = Vec::new();
            for def in defs {
                let abs = annotate_abs(&ctx, def.abs);

                let r#type = pattern_type(&ctx, &abs.item.pattern);

                ctx.variables
                    .insert(def.name.clone(), Type::Channel(Box::new(r#type)));

                new_defs.push(Def {
                    name: def.name,
                    abs,
                });
            }

            Decl::Def(new_defs)
        }
    };

    Tag { item, tag: ann }
}

fn annotate_val_input(ctx: &mut Ctx, lit: Tag<Val, Pos>) -> Tag<Val, Ann> {
    match &lit.item {
        Val::Path(p) => {
            let name = p.first().unwrap();
            let r#type = ctx.variables.get(name).expect("variable doesn't exist");

            Tag {
                item: lit.item,
                tag: Ann {
                    pos: lit.tag,
                    r#type: Type::Channel(Box::new(r#type.clone())),
                },
            }
        }

        _ => annotate_val(ctx, lit),
    }
}

fn literal_type(lit: &Literal) -> Type {
    match lit {
        Literal::Number(_) => Type::Name("Number".to_string()),
        Literal::String(_) => Type::Name("String".to_string()),
        Literal::Char(_) => Type::Name("Char".to_string()),
        Literal::Bool(_) => Type::Name("Bool".to_string()),
    }
}

fn pattern_type(ctx: &Ctx, pat: &Pat) -> Type {
    match pat {
        Pat::Var(v) => {
            if let Type::Anonymous = &v.var.r#type {
                if let Some(inner) = &v.pattern {
                    return pattern_type(ctx, inner);
                }
            }

            v.var.r#type.clone()
        }

        Pat::Record(rec) => {
            let rec = rec.clone().map(|p| pattern_type(ctx, &p));

            Type::Record(rec)
        }

        Pat::Wildcard(t) => t.clone(),
    }
}

fn annotate_val(ctx: &mut Ctx, lit: Tag<Val, Pos>) -> Tag<Val, Ann> {
    match lit.item {
        Val::Literal(l) => {
            let r#type = literal_type(&l);

            Tag {
                item: Val::Literal(l),
                tag: Ann::with_type(r#type, lit.tag),
            }
        }

        Val::Path(p) => {
            let mut path = p.clone();
            path.reverse();
            let name = path.pop().unwrap();
            let r#type = ctx.variables.get(&name).expect("variable doesn't exist");
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

                    panic!("Label {} doesn't exist in the record", frag);
                }

                if let Some(r#type) = temp {
                    r#type
                } else {
                    Type::Record(rec)
                }
            } else {
                r#type.clone()
            };

            let ann = Ann::with_type(r#type, lit.tag);

            Tag {
                item: Val::Path(p),
                tag: ann,
            }
        }

        // TODO - We should probably have tag at the record level too so if
        // variable that doesn't exist is used, we can have good error message.
        Val::Record(xs) => {
            let mut props = Vec::new();

            for prop in &xs.props {
                // FIXME - See TODO above.
                let tag = Tag {
                    item: prop.val.clone(),
                    tag: Pos { line: 1, column: 1 },
                };

                let ann = annotate_val(ctx, tag);

                props.push(Prop {
                    label: prop.label.clone(),
                    val: ann.tag.r#type,
                });
            }

            let ann = Ann::with_type(Type::Record(Record { props }), lit.tag);
            Tag {
                item: Val::Record(xs),
                tag: ann,
            }
        }
    }
}
