use std::collections::{HashSet, VecDeque};

use crate::ast::{Abs, Decl, Def};
use crate::{
    ast::{Proc, Program, Tag, Val},
    tokenizer::Pos,
};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Ann {
    pub pos: Pos,
    pub free_variables: HashSet<String>,
}

impl Ann {
    pub fn new(pos: Pos) -> Self {
        Self {
            pos,
            free_variables: Default::default(),
        }
    }
}

pub fn annotate(prog: Program<Pos>) -> Program<Ann> {
    let mut annotated = VecDeque::new();

    for proc in prog.procs {
        annotated.push_back(annotate_proc(proc));
    }

    Program { procs: annotated }
}

fn annotate_proc(proc: Tag<Proc<Pos>, Pos>) -> Tag<Proc<Ann>, Ann> {
    match proc.item {
        Proc::Output(l, r) => {
            let l = annotate_val(l);
            let r = annotate_val(r);
            let mut merged_vars = l
                .tag
                .free_variables
                .union(&r.tag.free_variables)
                .cloned()
                .collect::<HashSet<_>>();

            Tag {
                tag: Ann {
                    pos: proc.tag,
                    free_variables: merged_vars,
                },
                item: Proc::Output(l, r),
            }
        }

        Proc::Input(l, r) => {
            let l = annotate_val(l);
            let r = annotate_abs(r);

            let mut merged_vars = l
                .tag
                .free_variables
                .union(&r.tag.free_variables)
                .cloned()
                .collect::<HashSet<_>>();

            Tag {
                tag: Ann {
                    pos: proc.tag,
                    free_variables: merged_vars,
                },
                item: Proc::Input(l, r),
            }
        }

        Proc::Null => Tag {
            item: Proc::Null,
            tag: Ann {
                pos: proc.tag,
                free_variables: Default::default(),
            },
        },

        Proc::Parallel(ps) => {
            let mut ann = Ann::new(proc.tag);
            let ps = ps
                .into_iter()
                .map(|p| {
                    let proc = annotate_proc(p);
                    for var in &proc.tag.free_variables {
                        ann.free_variables.insert(var.clone());
                    }
                    proc
                })
                .collect::<Vec<_>>();

            Tag {
                item: Proc::Parallel(ps),
                tag: ann,
            }
        }

        Proc::Decl(d, p) => {
            let p = annotate_proc(p.map_item(|b| *b));
            let p = p.map_item(Box::new);
            let d = annotate_decl(d);
            let tag = p.tag.clone();

            Tag {
                item: Proc::Decl(d, p),
                tag,
            }
        }

        Proc::Cond(_, _, _) => todo!(),
    }
}

fn annotate_abs(tag: Tag<Abs<Pos>, Pos>) -> Tag<Abs<Ann>, Ann> {
    let proc = annotate_proc(*tag.item.proc);
    let mut ann = proc.tag.clone();

    ann.pos = tag.tag;

    Tag {
        item: Abs {
            pattern: tag.item.pattern,
            proc: Box::new(proc),
        },
        tag: ann,
    }
}

fn annotate_decl(decl: Tag<Decl<Pos>, Pos>) -> Tag<Decl<Ann>, Ann> {
    let mut ann = Ann::new(decl.tag);
    let item = match decl.item {
        Decl::Channel(n, t) => Decl::Channel(n, t),
        Decl::Type(n, t) => Decl::Type(n, t),

        Decl::Def(defs) => {
            let mut new_defs = Vec::new();
            for def in defs {
                let abs = annotate_abs(def.abs);

                for var in &abs.tag.free_variables {
                    ann.free_variables.insert(var.clone());
                }

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

fn annotate_val(lit: Tag<Val, Pos>) -> Tag<Val, Ann> {
    match lit.item {
        a @ Val::Literal(_) => Tag {
            item: a,
            tag: Ann::new(lit.tag),
        },

        Val::Path(p) => {
            let mut ann = Ann::new(lit.tag);
            ann.free_variables.insert(p.first().cloned().unwrap());

            Tag {
                item: Val::Path(p),
                tag: ann,
            }
        }

        Val::Record(xs) => {
            let mut ann = Ann::new(lit.tag);
            let mut stack = vec![xs.props.iter()];

            while let Some(mut props) = stack.pop() {
                for prop in props {
                    match &prop.val {
                        Val::Literal(_) => {}

                        Val::Path(p) => {
                            ann.free_variables.insert(p.first().cloned().unwrap());
                        }

                        Val::Record(xs) => {
                            stack.push(xs.props.iter());
                        }
                    }
                }
            }

            Tag {
                item: Val::Record(xs),
                tag: ann,
            }
        }
    }
}
