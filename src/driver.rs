use std::{fs, path::{Path, PathBuf}};

use crate::{
    backend::risc::{emit_risc_placeholder, RiscBinary},
    diagnostics::Diagnostic,
    error::{InternalError, Result},
    ir,
    lexer,
    parser,
    semantics,
    span::SourceFile,
};

#[derive(Clone, Debug)]
pub struct CompileOptions {
    pub output: Option<PathBuf>,
    pub dump: bool,
}

pub fn compile_file(path: &Path, opts: CompileOptions) -> Result<()> {
    let text = fs::read_to_string(path).map_err(InternalError::from)?;
    let src = SourceFile::new(path.display().to_string(), text);

    let toks = lexer::lex(&src)?;
    let ast = parser::parse_module(&src, &toks)?;
    let checked = semantics::check(ast)?;

    // Lower til IR (dummy)
    let ir_mod = ir::lower(checked.module.name.clone());

    // “Serialize” IR til tekst (midlertidigt)
    let ir_text = format!("{:#?}", ir_mod);
    if opts.dump {
        eprintln!("--- IR DUMP ---\n{ir_text}");
    }

    let bin = emit_risc_placeholder(&ir_text)?;
    write_output(path, opts.output.as_deref(), bin).map_err(|e| Diagnostic::from(InternalError::from(e)))?;
    Ok(())
}

fn write_output(input: &Path, out: Option<&Path>, bin: RiscBinary) -> std::io::Result<()> {
    let out_path = match out {
        Some(p) => p.to_path_buf(),
        None => input.with_extension("riscbin"),
    };
    fs::write(out_path, bin.bytes)
}