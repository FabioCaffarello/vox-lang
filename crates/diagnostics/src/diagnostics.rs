use std::{cell::RefCell, rc::Rc};

use lexer::token::{Token, TokenKind};
use text::span::TextSpan;

pub enum DiagnosticKind {
    Error,
    Warning,
}

pub struct Diagnostic<'de> {
    pub message: String,
    pub span: TextSpan<'de>,
    pub kind: DiagnosticKind,
}

impl<'de> Diagnostic<'de> {
    pub fn new(message: String, span: TextSpan<'de>, kind: DiagnosticKind) -> Self {
        Self {
            message,
            span,
            kind,
        }
    }
}

pub struct DiagnosticsBag<'de> {
    pub diagnostics: Vec<Diagnostic<'de>>,
}

pub type DiagnosticsBagCell<'de> = Rc<RefCell<DiagnosticsBag<'de>>>;

impl<'de> DiagnosticsBag<'de> {
    pub fn new() -> Self {
        Self {
            diagnostics: Vec::new(),
        }
    }

    pub fn report_error(&mut self, message: String, span: TextSpan<'de>) {
        self.report(message, span, DiagnosticKind::Error);
    }

    pub fn report_warning(&mut self, message: String, span: TextSpan<'de>) {
        self.report(message, span, DiagnosticKind::Warning);
    }

    pub fn report_unexpected_token(
        &mut self,
        expected: &TokenKind,
        token: &Token<'de>,
    ) {
        let message = format!(
            "Unexpected token: expected '{}', found '{}'",
            expected, token.kind
        );
        self.report_error(message, token.span.clone());
    }

    pub fn report_unexpected_expression(&mut self, token: &Token<'de>) {
        let message = format!("Expected expression, found <{}>", token.kind);
        self.report_error(message, token.span.clone());
    }

    fn report(&mut self, message: String, span: TextSpan<'de>, kind: DiagnosticKind) {
        let diagnostic = Diagnostic::new(message, span, kind);
        self.diagnostics.push(diagnostic);
    }
}
