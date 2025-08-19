use std::cmp;

use crate::diagnostics::Diagnostic;
use termion::color::{Fg, Red, Reset};
use text::source::SourceText;

pub struct DiagnosticPrinter<'a, 'de> {
    text: &'a SourceText,
    diagnostics: &'a [Diagnostic<'de>],
}

const PREFIX_LENGTH: usize = 8;

impl<'a, 'de> DiagnosticPrinter<'a, 'de> {
    pub fn new(text: &'a SourceText, diagnostics: &'a [Diagnostic<'de>]) -> Self {
        Self { text, diagnostics }
    }

    /// Stringifies the diagnostic
    ///
    /// It uses the following format:
    ///
    /// let <red>x<reset> = 5;
    ///          ^
    ///          |
    ///          +-- This is the error message (<line>:<column>)
    ///
    pub fn stringify_diagnostic(&self, diagnostic: &Diagnostic) -> String {
        let line_index = self.text.line_index(diagnostic.span.start);
        let line = self.text.get_line(line_index);
        let line_start = self.text.line_start(line_index);

        let column = diagnostic.span.start - line_start;

        let (prefix, span, suffix) = self.get_text_spans(diagnostic, line, column);

        let indent = cmp::min(PREFIX_LENGTH, column);
        let (arrow_pointers, arrow_line) = Self::format_arrow(diagnostic, indent);
        let error_message = Self::format_error_message(diagnostic, indent, column, line_index);
        format!(
            "{}{}{}{}{}\n{}\n{}\n{}",
            prefix,
            Fg(Red),
            span,
            Fg(Reset),
            suffix,
            arrow_pointers,
            arrow_line,
            error_message
        )
    }

    pub fn print(&self) {
        for diagnostic in self.diagnostics {
            println!("{}", self.stringify_diagnostic(diagnostic));
        }
    }

    fn get_text_spans(
        &'a self,
        diagnostic: &Diagnostic,
        line: &'a str,
        column: usize,
    ) -> (&'a str, &'a str, &'a str) {
        let prefix_start = cmp::max(0, column as isize - PREFIX_LENGTH as isize) as usize;
        let prefix_end = column;
        let suffix_start = cmp::min(column + diagnostic.span.length(), line.len());
        let suffix_end = cmp::min(suffix_start + PREFIX_LENGTH, line.len());

        let prefix = &line[prefix_start..prefix_end];
        let span = &line[prefix_end..suffix_start];
        let suffix = &line[suffix_start..suffix_end];
        (prefix, span, suffix)
    }

    fn format_arrow(diagnostic: &Diagnostic, indent: usize) -> (String, String) {
        let arrow_pointers = format!(
            "{:indent$}{}",
            "",
            "^".repeat(diagnostic.span.length()),
            indent = indent
        );
        let arrow_line = format!("{:indent$}|", "", indent = indent);
        (arrow_pointers, arrow_line)
    }

    fn format_error_message(
        diagnostic: &Diagnostic,
        indent: usize,
        column: usize,
        line_index: usize,
    ) -> String {
        format!(
            "{:indent$}+-- {} ({}:{})",
            "",
            diagnostic.message,
            line_index + 1,
            column + 1,
            indent = indent
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::diagnostics::{Diagnostic, DiagnosticKind};
    use termion::color::{Fg, Red, Reset};
    use text::span::TextSpan;

    fn mk_span<'a>(text: &'a str, start: usize, len: usize) -> TextSpan<'a> {
        TextSpan::new(start, start + len, &text[start..start + len])
    }

    fn colored(first: &str, highlight: &str, tail: &str) -> String {
        format!("{first}{}{}{}{}", Fg(Red), highlight, Fg(Reset), tail)
    }

    #[test]
    fn single_line_basic_highlight_and_arrows() {
        //        0123456789
        let src = "let x = 5;";
        let st = SourceText::new(src.to_string());

        // highlight 'x' (start=4, len=1)
        let span = mk_span(src, 4, 1);
        let diag = Diagnostic::new("Oops".into(), span, DiagnosticKind::Error); // <-- use DiagnosticKind

        let printer = DiagnosticPrinter::new(&st, std::slice::from_ref(&diag));
        let out = printer.stringify_diagnostic(&diag);

        // First line: "let " + <red>"x"</red> + " = 5;"
        let line0 = colored("let ", "x", " = 5;");
        let line1 = format!("{:4}^", "");
        let line2 = format!("{:4}|", "");
        let line3 = format!("{:4}+-- Oops (1:5)", "");

        let expected = format!("{line0}\n{line1}\n{line2}\n{line3}");
        assert_eq!(out, expected);
    }

    #[test]
    fn prefix_window_and_suffix_window_and_indent_cap() {
        // indices:  0........9........19....24
        //           0123456789abcdefghijklmno
        let src = "0123456789abcdefghijklmno";
        let st = SourceText::new(src.to_string());

        // highlight single char at column=12 ('c'): start=12, len=1
        let span = mk_span(src, 12, 1);
        let diag = Diagnostic::new("msg".into(), span, DiagnosticKind::Error);

        let printer = DiagnosticPrinter::new(&st, std::slice::from_ref(&diag));
        let out = printer.stringify_diagnostic(&diag);

        // prefix shows last 8 chars before column: "456789ab"
        // suffix shows up to 8 chars after: "defghijk"
        let line0 = colored("456789ab", "c", "defghijk");
        let line1 = format!("{:8}^", "");
        let line2 = format!("{:8}|", "");
        let line3 = format!("{:8}+-- msg (1:13)", "");

        let expected = format!("{line0}\n{line1}\n{line2}\n{line3}");
        assert_eq!(out, expected);
    }

    #[test]
    fn multi_char_span_caret_count_and_message_position() {
        //           01234567890
        let src = "foo bar baz";
        let st = SourceText::new(src.to_string());

        // highlight "bar" (start=4, len=3)
        let span = mk_span(src, 4, 3);
        let diag = Diagnostic::new("bad token".into(), span, DiagnosticKind::Error);

        let printer = DiagnosticPrinter::new(&st, std::slice::from_ref(&diag));
        let out = printer.stringify_diagnostic(&diag);

        // First line: "foo " + <red>"bar"</red> + " baz"
        let line0 = colored("foo ", "bar", " baz");
        let line1 = format!("{:4}{}", "", "^".repeat(3));
        let line2 = format!("{:4}|", "");
        let line3 = format!("{:4}+-- bad token (1:5)", "");

        let expected = format!("{line0}\n{line1}\n{line2}\n{line3}");
        assert_eq!(out, expected);
    }

    #[test]
    fn multi_line_source_correct_line_and_column() {
        // lines: 1:"aaa\n", 2:"bbb\n", 3:"ccc"
        let src = "aaa\nbbb\nccc";
        let st = SourceText::new(src.to_string());

        // Start at the *second* character of line 2 (column=1, 0-based).
        // This avoids the boundary ambiguity where line_index would return the previous line.
        let line2_start = src.find('\n').unwrap() + 1; // first byte of "bbb"
        let start = line2_start + 1; // column 1 in "bbb"
        let span = TextSpan::new(start, start + 2, &src[start..start + 2]); // "bb"
        let diag = crate::diagnostics::Diagnostic::new(
            "second line".into(),
            span,
            crate::diagnostics::DiagnosticKind::Error,
        );

        let printer = DiagnosticPrinter::new(&st, std::slice::from_ref(&diag));
        let out = printer.stringify_diagnostic(&diag);

        // Second line is "bbb"; prefix up to column 1 is "b"; highlight "bb"; no suffix.
        let line0 = format!(
            "b{}bb{}",
            termion::color::Fg(termion::color::Red),
            termion::color::Fg(termion::color::Reset)
        );
        let line1 = format!("{:1}{}", "", "^".repeat(2));
        let line2 = format!("{:1}|", "");
        // (line:column) is (2:2)
        let line3 = format!("{:1}+-- second line (2:2)", "");

        let expected = format!("{line0}\n{line1}\n{line2}\n{line3}");
        assert_eq!(out, expected);
    }
}
