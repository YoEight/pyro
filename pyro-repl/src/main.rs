use clap::{Parser, Subcommand};
use directories::UserDirs;
use glyph::{FileBackedInputs, Input, Options, PromptOptions};
use pyro_core::annotate::{annotate_decl, annotate_val};
use pyro_core::ast::Tag;
use pyro_core::parser::ParserState;
use pyro_core::sym::Sym;
use pyro_core::tokenizer::Tokenizer;
use pyro_core::{infer_decl, infer_val, Machine};
use pyro_runtime::{Engine, Env};
use std::io;
use std::path::PathBuf;

#[derive(Parser, Debug)]
struct Shell {
    #[command(subcommand)]
    cmd: Cmd,
}

#[derive(Subcommand, Debug)]
enum Cmd {
    /// Add a pyro module to the scope.
    AddModule { file: PathBuf },

    /// Give the type of an expression.
    Type { expr: Vec<String> },

    /// Exit the application.
    Exit,
}

#[derive(Default)]
struct State {
    modules: Vec<String>,
}

impl State {
    fn next_input(&self, inputs: &mut FileBackedInputs) -> io::Result<Option<Input<Shell>>> {
        let prompt = self.modules.join(" ");
        let options = if prompt.is_empty() {
            PromptOptions::default()
        } else {
            PromptOptions::default().prompt(prompt)
        };

        inputs.next_input_with_parser_and_options::<Shell>(&options)
    }
}

#[tokio::main]
async fn main() -> eyre::Result<()> {
    let user_dirs = UserDirs::new();
    let options = Options::default()
        .prompt("π>")
        .header(include_str!("header.txt"))
        .author("Yo Eight")
        .version("master");

    let mut inputs = glyph::file_backed_inputs(options, ".pyro-repl")?;
    let mut engine = Engine::with_nominal_typing().stdlib(Env::stdio()).build()?;
    let mut state = State::default();

    while let Some(input) = state.next_input(&mut inputs)? {
        match input {
            Input::Exit => {
                break;
            }

            Input::Command(shell) => match shell.cmd {
                Cmd::AddModule { file } => {
                    let path = if let Some(user_dirs) = user_dirs.as_ref() {
                        extrapolate_path(user_dirs, file)
                    } else {
                        PathBuf::from(file)
                    };

                    if let Err(e) = add_module(&mut state, &mut engine, path) {
                        println!("ERR: {}", e);
                    }
                }

                Cmd::Type { expr } => {
                    if let Err(e) = type_expr(&mut engine, expr) {
                        println!("ERR: {}", e);
                    }
                }

                Cmd::Exit => {
                    break;
                }
            },

            Input::String(s) => {
                let source = format!("run {}", s);

                match engine.compile(source.as_str()) {
                    Err(e) => println!("Compilation error: {}", e),
                    Ok(process) => {
                        if let Err(e) = process.run().await {
                            println!("ERR: {}", e);
                        }
                    }
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

fn add_module<M: Machine>(
    state: &mut State,
    engine: &mut Engine<M>,
    path: PathBuf,
) -> eyre::Result<()> {
    let source_code = std::fs::read_to_string(path.as_path())?;
    let tokens = Tokenizer::new(source_code.as_str()).tokenize()?;
    let mut parser = ParserState::new(tokens.as_slice());

    loop {
        parser.skip_spaces();
        let pos = parser.pos();
        let decl = parser.parse_decl()?;
        let node = Tag {
            item: decl,
            tag: pos,
        };

        let decl = infer_decl(engine.context(), node)?;
        let decl = annotate_decl(engine.context(), decl)?;

        engine.runtime().register(decl.item);
        parser.skip_spaces();

        if parser.look_ahead().item == Sym::EOF {
            break;
        }
    }

    state.modules.push(format!("{}", path.to_string_lossy()));

    Ok(())
}

fn type_expr<M: Machine>(engine: &mut Engine<M>, expr: Vec<String>) -> eyre::Result<()> {
    let expr = expr.join(" ");
    let tokens = Tokenizer::new(expr.as_str()).tokenize()?;
    let mut parser = ParserState::new(tokens.as_slice());
    let value = parser.parse_value()?;
    let value = infer_val(engine.context(), value)?;
    let value = annotate_val(engine.context(), value)?;

    println!(">>> {}", engine.context().project_type(&value.tag.r#type));

    Ok(())
}
