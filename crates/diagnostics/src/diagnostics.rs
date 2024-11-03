use std::{cell::RefCell, rc::Rc};

use lexer::token::{Token, TokenKind};
use text::span::TextSpan;

#[derive(Clone, Copy, Debug)]
pub enum DiagnosticKind {
    Error,
    Warning,
}

#[derive(Clone, Debug)]
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

#[derive(Clone, Debug)]
pub struct DiagnosticsBag<'de> {
    pub diagnostics: Vec<Diagnostic<'de>>,
}

pub type DiagnosticsBagCell<'de> = Rc<RefCell<DiagnosticsBag<'de>>>;

impl Default for DiagnosticsBag<'_> {
    fn default() -> Self {
        Self::new()
    }
}

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

    pub fn report_unexpected_token(&mut self, expected: &TokenKind, token: &Token<'de>) {
        let message = format!(
            "Unexpected token: expected '{}', found '{}'",
            expected, token.kind
        );
        self.report_error(message, token.span);
    }

    pub fn report_expected_expression(&mut self, token: &Token<'de>) {
        let message = format!("Expected expression, found <{}>", token.kind);
        self.report_error(message, token.span);
    }

    pub fn report_undeclared_variable(&mut self, token: &Token<'de>) {
        let message = format!("Undeclared variable: <{}>", token.span.literal);
        self.report_error(message, token.span);
    }

    fn report(&mut self, message: String, span: TextSpan<'de>, kind: DiagnosticKind) {
        let diagnostic = Diagnostic::new(message, span, kind);
        self.diagnostics.push(diagnostic);
    }

    pub fn report_undeclared_function(&mut self, token: &Token<'de>) {
        self.report_error(
            format!("Undeclared function '{}'", token.span.literal),
            token.span,
        );
    }

    pub fn report_invalid_argument_count(
        &mut self,
        token: &Token<'de>,
        expected: usize,
        actual: usize,
    ) {
        self.report_error(
            format!(
                "Function '{}' expects {} arguments, but was given {}",
                token.span.literal, expected, actual
            ),
            token.span,
        );
    }

    pub fn report_function_already_declared(&mut self, token: &Token<'de>) {
        self.report_error(
            format!("Function '{}' already declared", token.span.literal),
            token.span,
        );
    }

    pub fn report_break_outside_loop(&mut self, token: &Token<'de>) {
        self.report_error("Break statement outside of loop".to_string(), token.span);
    }

    pub fn report_undefined_label(&mut self, token: &Token<'de>) {
        self.report_error(
            format!("Undefined label '{}'", token.span.literal),
            token.span,
        );
    }

    pub fn report_unexpected_label(&mut self, token: &Token<'de>) {
        self.report_error(
            format!("Unexpected label '{}'", token.span.literal),
            token.span,
        );
    }

    pub fn report_duplicate_label(&mut self, token: &Token<'de>) {
        self.report_error(
            format!("Duplicate label '{}'", token.span.literal),
            token.span,
        );
    }
}
