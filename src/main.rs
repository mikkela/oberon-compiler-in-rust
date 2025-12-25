use clap::Parser as _;
use std::path::PathBuf;

use oberon_compiler::driver::{compile_file, CompileOptions};

#[derive(clap::Parser, Debug)]
#[command(name = "obc", about = "Oberon compiler (skeleton)")]
struct Args {
    /// Input source file (.oberon / .mod)
    input: PathBuf,

    /// Output file (binary / object placeholder)
    #[arg(short, long)]
    output: Option<PathBuf>,

    /// Dump intermediate representations
    #[arg(long)]
    dump: bool,
}

fn main() {
    let args = Args::parse();

    let opts = CompileOptions {
        output: args.output,
        dump: args.dump,
    };

    match compile_file(&args.input, opts) {
        Ok(()) => {}
        Err(diag) => {
            eprintln!("{}", diag.render_human());
            std::process::exit(1);
        }
    }
}