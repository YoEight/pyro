use clap::{Parser, Subcommand};
use directories::UserDirs;
use glyph::{Input, Options};
use pyro_core::annotate::annotate_decl;
use pyro_core::ast::Tag;
use pyro_core::parser::ParserState;
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

    parser.skip_spaces();
    let pos = parser.pos();
    let decl = parser.parse_decl()?;
    let ctx = engine.context();
    let node = Tag {
        item: decl,
        tag: pos,
    };

    let decl = annotate_decl(ctx, &STDLIB.as_local_scope(), node)?;
    engine.runtime().register(decl.item);

    Ok(())
}
