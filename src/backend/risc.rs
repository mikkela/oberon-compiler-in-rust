use crate::{diagnostics::Diagnostic, error::Result};

#[derive(Clone, Debug)]
pub struct RiscBinary {
    pub bytes: Vec<u8>,
}

pub fn emit_risc_placeholder(ir_text: &str) -> Result<RiscBinary> {
    // Placeholder: skriv “magic” + IR-tekst som bytes.
    // Du kan senere erstatte med rigtig encoding til din emulator.
    let mut bytes = Vec::new();
    bytes.extend_from_slice(b"RISC");
    bytes.extend_from_slice(&(ir_text.len() as u32).to_le_bytes());
    bytes.extend_from_slice(ir_text.as_bytes());

    if bytes.len() < 8 {
        return Err(Diagnostic::error("backend produced empty binary"));
    }

    Ok(RiscBinary { bytes })
}