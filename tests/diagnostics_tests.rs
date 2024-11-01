use compiler::compilation_unit::CompilationUnit;
use diagnostics::diagnostics::{Diagnostic, DiagnosticKind};
use std::vec;
use text::TextSpan;

struct DiagnosticsVerifier<'de> {
    actual: Vec<Diagnostic<'de>>,
    expected: Vec<Diagnostic<'de>>,
}

fn assert_diagnostics(input: &str, raw: &str, expected: Vec<&str>) {
    let verifier = DiagnosticsVerifier::new(input, raw, expected);
    verifier.verify();
}

impl<'de> DiagnosticsVerifier<'de> {
    pub fn new(input: &'de str, raw: &'de str, messages: Vec<&str>) -> Self {
        let messages_len = messages.len();
        let expected = Self::parse_input(input, messages);
        assert_eq!(expected.len(), messages_len);
        let actual = Self::compile(raw);
        Self { expected, actual }
    }

    fn compile(input: &'de str) -> Vec<Diagnostic<'de>> {
        let compilation_unit = CompilationUnit::compile(input);
        match compilation_unit {
            Ok(_) => vec![],
            Err(e) => e.diagnostics.clone(),
        }
    }

    fn get_raw_text(input: &'de str) -> String {
        input.replace("«", "").replace("»", "")
    }

    fn parse_input(input: &'de str, messages: Vec<&str>) -> Vec<Diagnostic<'de>> {
        let raw_text = Self::get_raw_text(input);
        let mut start_index_stack = vec![];
        let mut current_position: usize = 0;
        let mut diagnostics = vec![];

        for c in input.chars() {
            match c {
                '«' => {
                    start_index_stack.push(current_position);
                }
                '»' => {
                    let start_index = start_index_stack.pop().unwrap();
                    let end_index = current_position;
                    let literal = raw_text[start_index..end_index].to_string();
                    let span = TextSpan::from_owned_string(start_index, end_index, literal);
                    let message = messages[diagnostics.len()].to_string();
                    let diagnostic = Diagnostic::new(message, span, DiagnosticKind::Error);
                    diagnostics.push(diagnostic);
                }
                _ => {
                    current_position += 1;
                }
            };
        }

        diagnostics
    }

    fn verify(&self) {
        assert_eq!(
            self.actual.len(),
            self.expected.len(),
            "Expected {} diagnostics, found {}",
            self.expected.len(),
            self.actual.len()
        );

        for (actual, expected) in self.actual.iter().zip(self.expected.iter()) {
            assert_eq!(
                actual.message, expected.message,
                "Expected message '{}', found '{}'",
                expected.message, actual.message
            );
            assert_eq!(
                actual.span.start, expected.span.start,
                "Expected start index {}, found {}",
                expected.span.start, actual.span.start
            );
            assert_eq!(
                actual.span.end, expected.span.end,
                "Expected end index {}, found {}",
                expected.span.end, actual.span.end
            );
            assert_eq!(
                actual.span.literal, expected.span.literal,
                "Expected literal '{}', found '{}'",
                expected.span.literal, actual.span.literal
            );
        }
    }
}

#[test]
fn should_report_undeclared_variable() {
    let input = "let a = «b»";
    let raw = "let a = b";
    let expected = vec!["Undeclared variable: <b>"];

    assert_diagnostics(input, raw, expected);
}

#[test]
fn should_report_expected_expression() {
    let input = "let a = «+»";
    let raw = "let a = +";
    let expected = vec!["Expected expression, found <+>"];

    assert_diagnostics(input, raw, expected);

    let input = "let a = «*»";
    let raw = "let a = *";
    let expected = vec!["Expected expression, found <*>"];

    assert_diagnostics(input, raw, expected);

    let input = "let a = «/»";
    let raw = "let a = /";
    let expected = vec!["Expected expression, found </>"];

    assert_diagnostics(input, raw, expected);
}

#[test]
fn should_report_expected_unary_expression() {
    let input = "let a = -«»";
    let raw = "let a = -";
    let expected = vec!["Expected expression, found <EOF>"];

    assert_diagnostics(input, raw, expected);
}

#[test]
fn should_report_unexpected_token() {
    let input = "let a = 10 «@» 1";
    let raw = "let a = 10 @ 1";
    let expected = vec!["Expected expression, found <BAD>"];

    assert_diagnostics(input, raw, expected);
}

#[test]
fn should_report_expected_expression_multiple_unary_operators() {
    let input = "let a = -«-»1";
    let raw = "let a = --1";
    let expected = vec!["Expected expression, found <->"];

    assert_diagnostics(input, raw, expected);
}
