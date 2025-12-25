use crate::span::{SourceFile, Span};

#[derive(Clone, Debug)]
pub enum Severity {
    Error,
    Warning,
    Note,
}

#[derive(Clone, Debug)]
pub struct Diagnostic {
    pub severity: Severity,
    pub message: String,
    pub span: Option<Span>,
    pub source: Option<SourceFile>,
}

impl Diagnostic {
    pub fn error(msg: impl Into<String>) -> Self {
        Self { severity: Severity::Error, message: msg.into(), span: None, source: None }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn with_source(mut self, src: SourceFile) -> Self {
        self.source = Some(src);
        self
    }

    pub fn render_human(&self) -> String {
        let sev = match self.severity {
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Note => "note",
        };

        let mut out = format!("{sev}: {}", self.message);

        if let (Some(src), Some(sp)) = (&self.source, self.span) {
            // super simpel “line/col” rendering (kan forbedres senere)
            let prefix = &src.text[..sp.start.min(src.text.len())];
            let line = prefix.bytes().filter(|&b| b == b'\n').count() + 1;
            let col = prefix.rsplit('\n').next().map(|s| s.chars().count() + 1).unwrap_or(1);

            out.push_str(&format!("\n --> {}:{}:{}", src.name, line, col));
            out.push_str("\n");
            out.push_str(src.slice(sp));
        }

        out
    }
}