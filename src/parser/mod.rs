mod parser;

use crate::{
    ast::Module,
    diagnostics::Diagnostic,
    error::Result,
    lexer::{Token, TokenKind},
    span::{Span, SourceFile},
};
use crate::ast::Identifier;

pub fn parse_module(src: &SourceFile, tokens: &[Token]) -> Result<Module> {
    // Dummy: forvent "MODULE <Ident> ; ... END <Ident> ."
    // Men vi laver kun navn + span, resten tomt.
    let mut idx = 0;

    let t0 = tokens.get(idx).ok_or_else(|| Diagnostic::error("missing tokens"))?;
    if t0.kind != TokenKind::Module {
        return Err(Diagnostic::error("expected MODULE").with_span(t0.span).with_source(src.clone()));
    }
    idx += 1;

    let name_tok = tokens.get(idx).ok_or_else(|| Diagnostic::error("expected module name"))?;
    let name = match &name_tok.kind {
        TokenKind::Ident(s) => s.clone(),
        _ => {
            return Err(Diagnostic::error("expected module identifier")
                .with_span(name_tok.span)
                .with_source(src.clone()));
        }
    };

    // Find END (meget naïvt)
    let end_span = tokens.last().map(|t| t.span).unwrap_or(Span::new(0, 0));
    Ok(Module {
        name: Identifier { text: name.clone(), span: Span { start: 0, end: 0 } },
        end_name: Identifier { text: name.clone(), span: Span { start: 0, end: 0 } },
        import_list: vec![],
        declarations: vec![],
        stmts: vec![],
        span: Span { start: 0, end: end_span.end },
    })
}