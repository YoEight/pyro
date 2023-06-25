use std::fs;
use std::path::PathBuf;

use clap::Parser;
use pyro_runtime::{Engine, Env};

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    #[arg(long)]
    tokens: bool,

    file: PathBuf,
}

#[tokio::main]
async fn main() -> eyre::Result<()> {
    let args = Args::parse();
    let source = fs::read_to_string(args.file.as_path())?;

    if args.tokens {
        println!("Tokens: {:?}", pyro_core::tokenize(source.as_str()));
    }

    let engine = Engine::builder().stdlib(Env::stdio()).build()?;
    engine.compile(source.as_str())?.run().await
}
