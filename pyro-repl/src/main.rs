use clap::{Parser, Subcommand};
use directories::UserDirs;
use glyph::{Input, Options};
use pyro_core::annotate::{annotate_decl, annotate_val, ValCtx};
use pyro_core::ast::{Decl, Tag, Type};
use pyro_core::parser::ParserState;
use pyro_core::sym::Sym;
use pyro_core::tokenizer::Tokenizer;
use pyro_core::STDLIB;
use pyro_runtime::{Engine, EngineBuilder};
use std::path::PathBuf;

#[derive(Parser, Debug)]
#[command(name = ":")]
struct Shell {
    #[command(subcommand)]
    cmd: Cmd,
}

#[derive(Subcommand, Debug)]
enum Cmd {
    /// Add a pyro module to the scope.
    AddModule { file: PathBuf },

    /// Give the type of an expression.
    Type { expr: String },

    /// Exit the application.
    Exit,
}

#[tokio::main]
async fn main() -> eyre::Result<()> {
    let user_dirs = UserDirs::new();
    let options = Options::default()
        .prompt("π")
        .header(include_str!("header.txt"))
        .author("Yo Eight")
        .version("master");

    let mut inputs = glyph::file_backed_inputs(options, ".pyro-repl")?;
    let mut engine = EngineBuilder::default().build();

    while let Some(input) = inputs.next_input()? {
        match input {
            Input::Exit => {
                break;
            }

            Input::Command { name, params } => {
                let mut cmd = vec![":".to_string(), name];
                cmd.extend(params);

                match Shell::try_parse_from(&cmd) {
                    Err(e) => {
                        println!("{}", e);
                    }

                    Ok(shell) => match shell.cmd {
                        Cmd::AddModule { file } => {
                            let path = if let Some(user_dirs) = user_dirs.as_ref() {
                                extrapolate_path(user_dirs, file)
                            } else {
                                PathBuf::from(file)
                            };

                            if let Err(e) = add_module(&mut engine, path) {
                                println!("ERR: {}", e);
                            }
                        }

                        Cmd::Type { expr } => {
                            if let Err(e) = type_expr(&mut engine, expr) {
                                println!("Err: {}", e);
                            }
                        }

                        Cmd::Exit => {
                            break;
                        }
                    },
                }
            }

            Input::String(s) => {
                let source = format!("run {}", s);

                if let Err(e) = engine.clone().run(source.as_str()).await {
                    println!("ERR: {}", e);
                }
            }
        }
    }

    Ok(())
}

fn extrapolate_path(dirs: &UserDirs, path: PathBuf) -> PathBuf {
    let mut buf = PathBuf::new();

    for (idx, comp) in path.components().enumerate() {
        if idx == 0 && comp.as_os_str() == "~" {
            buf.push(dirs.home_dir());
            continue;
        }

        buf.push(comp);
    }

    buf
}

fn add_module(engine: &mut Engine, path: PathBuf) -> eyre::Result<()> {
    let source_code = std::fs::read_to_string(path)?;
    let tokens = Tokenizer::new(source_code.as_str()).tokenize()?;
    let mut parser = ParserState::new(tokens.as_slice());
    let mut decls = Vec::new();

    loop {
        parser.skip_spaces();
        let pos = parser.pos();
        let decl = parser.parse_decl()?;
        let node = Tag {
            item: decl,
            tag: pos,
        };

        let scope = engine.context().new_scope(&STDLIB);
        let decl = annotate_decl(engine.context(), &scope, node)?;
        decls.push(decl.item);
        parser.skip_spaces();

        if parser.look_ahead().item == Sym::EOF {
            break;
        }
    }

    for decl in decls {
        engine.runtime().register(decl.clone());
        match decl {
            Decl::Channels(cs) => {
                for c in cs {
                    let (name, r#type) = c.item;
                    engine
                        .context()
                        .declare_or_replace(&STDLIB, name, Type::channel(r#type));
                }
            }

            Decl::Def(defs) => {
                for def in defs {
                    let r#type = def.tag.r#type;
                    let def = def.item;
                    engine
                        .context()
                        .declare_or_replace(&STDLIB, def.name, Type::client(r#type));
                }
            }

            Decl::Type(name, r#type) => {
                engine.context().declare_or_replace(&STDLIB, name, r#type);
            }
        }
    }

    Ok(())
}

fn type_expr(engine: &mut Engine, expr: String) -> eyre::Result<()> {
    let tokens = Tokenizer::new(expr.as_str()).tokenize()?;
    let mut parser = ParserState::new(tokens.as_slice());
    let value = parser.parse_value()?;
    let scope = engine.context().new_scope(&STDLIB);
    let value = annotate_val(engine.context(), &scope, ValCtx::Regular, value)?;

    println!(">>> {}", value.tag.r#type);

    Ok(())
}

fn show_type(r#type: &Type) -> String {
    match r#type {
        Type::Name { name, .. } => name.to_string(),
        Type::Record(rec) => rec.to_string(),
        Type::App(_, _) => todo!(),
    }
}
