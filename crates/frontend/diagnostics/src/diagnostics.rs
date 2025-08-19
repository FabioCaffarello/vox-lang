use std::{cell::RefCell, rc::Rc};

use text::span::TextSpan;
use token::{Token, TokenKind};
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

#[cfg(test)]
mod tests {
    use super::*;
    use std::{cell::RefCell, rc::Rc};
    use token::{Token, TokenKind};

    // ---- helpers ----
    fn span(lit: &'static str, start: usize) -> TextSpan<'static> {
        TextSpan::new(start, start + lit.len(), lit)
    }
    fn tok(kind: TokenKind, lit: &'static str, start: usize) -> Token<'static> {
        Token {
            kind,
            span: span(lit, start),
        }
    }

    #[test]
    fn new_is_empty_and_default_works() {
        let b1: DiagnosticsBag<'_> = DiagnosticsBag::new();
        assert!(b1.diagnostics.is_empty());

        let b2: DiagnosticsBag<'_> = Default::default();
        assert!(b2.diagnostics.is_empty());
    }

    #[test]
    fn report_error_and_warning_push_in_order() {
        let mut bag = DiagnosticsBag::new();
        let s1 = span("x", 0);
        let s2 = span("y", 1);

        bag.report_error("boom".into(), s1);
        bag.report_warning("careful".into(), s2);

        assert_eq!(bag.diagnostics.len(), 2);

        let d0 = &bag.diagnostics[0];
        assert_eq!(d0.message, "boom");
        assert_eq!(d0.span, s1);
        assert!(matches!(d0.kind, DiagnosticKind::Error));

        let d1 = &bag.diagnostics[1];
        assert_eq!(d1.message, "careful");
        assert_eq!(d1.span, s2);
        assert!(matches!(d1.kind, DiagnosticKind::Warning));
    }

    #[test]
    fn report_unexpected_token() {
        let mut bag = DiagnosticsBag::new();
        let t = tok(TokenKind::Identifier, "foo", 0);
        bag.report_unexpected_token(&TokenKind::If, &t);

        assert_eq!(bag.diagnostics.len(), 1);
        let msg = &bag.diagnostics[0].message;
        // Uses TokenKind Display implementation
        assert_eq!(msg, "Unexpected token: expected 'IF', found 'IDENTIFIER'");
        assert_eq!(bag.diagnostics[0].span, t.span);
    }

    #[test]
    fn report_expected_expression() {
        let mut bag = DiagnosticsBag::new();
        let semi = tok(TokenKind::SemiColon, ";", 3);
        bag.report_expected_expression(&semi);

        assert_eq!(bag.diagnostics.len(), 1);
        assert_eq!(
            bag.diagnostics[0].message,
            "Expected expression, found <SEMICOLON>"
        );
        assert_eq!(bag.diagnostics[0].span, semi.span);
    }

    #[test]
    fn report_undeclared_variable() {
        let mut bag = DiagnosticsBag::new();
        let v = tok(TokenKind::Identifier, "x", 10);
        bag.report_undeclared_variable(&v);

        assert_eq!(bag.diagnostics.len(), 1);
        assert_eq!(bag.diagnostics[0].message, "Undeclared variable: <x>");
        assert_eq!(bag.diagnostics[0].span, v.span);
    }

    #[test]
    fn report_cannot_call_non_callable_expression() {
        let mut bag = DiagnosticsBag::new();
        let callee_span = span("foo", 0);
        // We don't depend on exact Type display; just that it’s included.
        let ty = Type::Unresolved; // any Type implementing Display is fine
        bag.report_cannot_call_no_callable_expression(&callee_span, &ty);

        assert_eq!(bag.diagnostics.len(), 1);
        let msg = &bag.diagnostics[0].message;
        assert!(
            msg.starts_with("Cannot call non-callable expression of type '"),
            "unexpected message: {msg}"
        );
        assert!(msg.ends_with('\''));
        assert_eq!(bag.diagnostics[0].span, callee_span);
    }

    #[test]
    fn report_invalid_argument_count_pluralization() {
        let mut bag = DiagnosticsBag::new();
        let callee_span = span("f", 5);

        // plural path
        bag.report_invalid_argument_count(&callee_span, 2, 1);
        assert_eq!(bag.diagnostics.len(), 1);
        assert_eq!(
            bag.diagnostics[0].message,
            "Function 'f' expects 2 arguments, but was given 1"
        );

        // singular path
        bag.report_invalid_argument_count(&callee_span, 1, 2);
        assert_eq!(bag.diagnostics.len(), 2);
        assert_eq!(
            bag.diagnostics[1].message,
            "Function 'f' expects 1 argument, but was given 2"
        );
    }

    #[test]
    fn report_function_decl_conflicts() {
        let mut bag = DiagnosticsBag::new();
        let f = tok(TokenKind::Identifier, "f", 0);
        let g = tok(TokenKind::Identifier, "g", 1);

        bag.report_function_already_declared(&f);
        bag.report_undeclared_function(&g);

        assert_eq!(bag.diagnostics.len(), 2);
        assert_eq!(bag.diagnostics[0].message, "Function 'f' already declared");
        assert_eq!(bag.diagnostics[0].span, f.span);
        assert_eq!(bag.diagnostics[1].message, "Undeclared function 'g'");
        assert_eq!(bag.diagnostics[1].span, g.span);
    }

    #[test]
    fn report_break_and_labels() {
        let mut bag = DiagnosticsBag::new();
        let br = tok(TokenKind::Break, "break", 0);
        bag.report_break_outside_loop(&br);

        let lbl_undef = tok(TokenKind::Identifier, "L", 10);
        bag.report_undefined_label(&lbl_undef);

        let lbl_unexp = tok(TokenKind::Identifier, "L", 11);
        bag.report_unexpected_label(&lbl_unexp);

        let lbl_dup = tok(TokenKind::Identifier, "L", 12);
        bag.report_duplicate_label(&lbl_dup);

        assert_eq!(bag.diagnostics.len(), 4);
        assert_eq!(
            bag.diagnostics[0].message,
            "Break statement outside of loop"
        );
        assert_eq!(bag.diagnostics[0].span, br.span);

        assert_eq!(bag.diagnostics[1].message, "Undefined label 'L'");
        assert_eq!(bag.diagnostics[1].span, lbl_undef.span);

        assert_eq!(bag.diagnostics[2].message, "Unexpected label 'L'");
        assert_eq!(bag.diagnostics[2].span, lbl_unexp.span);

        assert_eq!(bag.diagnostics[3].message, "Duplicate label 'L'");
        assert_eq!(bag.diagnostics[3].span, lbl_dup.span);
    }

    #[test]
    fn report_type_related() {
        let mut bag = DiagnosticsBag::new();
        let sp = span("x", 0);
        let t = tok(TokenKind::Identifier, "T", 1);

        let expected = Type::Unresolved;
        let actual = Type::Unresolved;

        bag.report_type_mismatch(&sp, &expected, &actual);
        bag.report_undeclared_type(&t);

        assert_eq!(bag.diagnostics.len(), 2);

        let msg = &bag.diagnostics[0].message;
        assert!(
            msg.starts_with("Type mismatch: expected '")
                && msg.contains("', found '")
                && msg.ends_with('\''),
            "unexpected message: {msg}"
        );
        assert_eq!(bag.diagnostics[0].span, sp);

        assert_eq!(bag.diagnostics[1].message, "Undeclared type 'T'");
        assert_eq!(bag.diagnostics[1].span, t.span);
    }

    #[test]
    fn report_return_and_rec_outside_function() {
        let mut bag = DiagnosticsBag::new();
        let r = tok(TokenKind::Return, "return", 0);
        let rec = tok(TokenKind::Identifier, "rec", 7);

        bag.report_cannot_return_outside_function(&r);
        bag.report_cannot_use_rec_outside_of_function(&rec);

        assert_eq!(bag.diagnostics.len(), 2);
        assert_eq!(
            bag.diagnostics[0].message,
            "Cannot use 'return' outside of function"
        );
        assert_eq!(bag.diagnostics[0].span, r.span);

        assert_eq!(
            bag.diagnostics[1].message,
            "Cannot use 'rec' outside of function"
        );
        assert_eq!(bag.diagnostics[1].span, rec.span);
    }

    #[test]
    fn diagnostics_bag_cell_shares_state() {
        let bag: DiagnosticsBagCell<'static> = Rc::new(RefCell::new(DiagnosticsBag::new()));
        let other = bag.clone();

        other.borrow_mut().report_error("oops".into(), span("x", 0));

        assert_eq!(bag.borrow().diagnostics.len(), 1);
        assert_eq!(bag.borrow().diagnostics[0].message, "oops");
    }
}
