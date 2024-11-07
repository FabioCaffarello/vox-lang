use std::{cell::RefCell, rc::Rc};

use lexer::token::{Token, TokenKind};
use text::span::TextSpan;
use typings::types::Type;

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

    pub fn report_cannot_call_no_callable_expression(
        &mut self,
        callee_span: &TextSpan<'de>,
        callee_type: &Type,
    ) {
        self.report_error(
            format!(
                "Cannot call non-callable expression of type '{}'",
                callee_type
            ),
            *callee_span,
        );
    }

    pub fn report_invalid_argument_count(
        &mut self,
        callee_span: &TextSpan<'de>,
        expected: usize,
        actual: usize,
    ) {
        let args = if expected == 1 {
            "argument"
        } else {
            "arguments"
        };
        self.report_error(
            format!(
                "Function '{}' expects {} {}, but was given {}",
                callee_span.literal, expected, args, actual
            ),
            *callee_span,
        );
    }

    pub fn report_function_already_declared(&mut self, token: &Token<'de>) {
        self.report_error(
            format!("Function '{}' already declared", token.span.literal),
            token.span,
        );
    }

    pub fn report_undeclared_function(&mut self, token: &Token<'de>) {
        self.report_error(
            format!("Undeclared function '{}'", token.span.literal),
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

    pub fn report_type_mismatch(&mut self, span: &TextSpan<'de>, expected: &Type, actual: &Type) {
        self.report_error(
            format!("Type mismatch: expected '{}', found '{}'", expected, actual),
            *span,
        );
    }

    pub fn report_undeclared_type(&mut self, token: &Token<'de>) {
        self.report_error(
            format!("Undeclared type '{}'", token.span.literal),
            token.span,
        );
    }

    pub fn report_cannot_return_outside_function(&mut self, token: &Token<'de>) {
        self.report_error(
            "Cannot use 'return' outside of function".to_string(),
            token.span,
        );
    }

    pub fn report_cannot_use_rec_outside_of_function(&mut self, token: &Token<'de>) {
        self.report_error(
            "Cannot use 'rec' outside of function".to_string(),
            token.span,
        );
    }
}
