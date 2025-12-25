use oberon_compiler::driver::{compile_file, CompileOptions};
use std::{fs, path::PathBuf};

#[test]
fn pipeline_smoke() {
    let dir = tempfile::tempdir().unwrap();
    let src_path = dir.path().join("Hello.mod");

    fs::write(&src_path, r#"
MODULE Hello;
BEGIN
END Hello.
"#).unwrap();

    let out_path = dir.path().join("Hello.riscbin");
    compile_file(
        &src_path,
        CompileOptions { output: Some(out_path.clone()), dump: false }
    ).unwrap();

    let bytes = fs::read(out_path).unwrap();
    assert!(bytes.starts_with(b"RISC"));
}