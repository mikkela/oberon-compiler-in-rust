use thiserror::Error;

use crate::diagnostics::Diagnostic;

pub type Result<T> = std::result::Result<T, Diagnostic>;

#[derive(Debug, Error)]
pub enum InternalError {
    #[error("io error: {0}")]
    Io(#[from] std::io::Error),
}

impl From<InternalError> for Diagnostic {
    fn from(e: InternalError) -> Self {
        Diagnostic::error(e.to_string())
    }
}