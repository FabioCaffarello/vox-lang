use miette::{Diagnostic, MietteError, SourceSpan};
use thiserror::Error;

#[derive(Diagnostic, Debug, Error)]
#[error("Unterminated string")]
pub struct StringTerminationError {
    #[source_code]
    pub src: String,

    #[label = "this string literal"]
    pub err_span: SourceSpan,
}

impl StringTerminationError {
    pub fn line(&self) -> usize {
        let until_unrecongized = &self.src[..=self.err_span.offset()];
        until_unrecongized.lines().count()
    }
}

#[derive(Debug, Error, Diagnostic)]
#[error("parser errors occurred")]
pub enum ParserError {
    #[diagnostic(
        code(parser::lexer_errors),
        help("Fix the lexing errors before parsing")
    )]
    LexerErrors {
        #[related]
        errors: Vec<MietteError>,
    },
    // Future parser errors can be added here
}

impl From<Vec<MietteError>> for ParserError {
    fn from(errors: Vec<MietteError>) -> Self {
        Self::LexerErrors { errors }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use miette::{Diagnostic, MietteError, SourceSpan};
    use std::io;

    fn mk_err(src: &str, offset: usize, len: usize) -> StringTerminationError {
        StringTerminationError {
            src: src.to_string(),
            err_span: SourceSpan::from((offset, len)),
        }
    }

    #[test]
    fn line_single_line_beginning() {
        let e = mk_err("unterminated", 0, 1);
        assert_eq!(e.line(), 1);
        assert_eq!(e.err_span.offset(), 0);
        assert_eq!(e.err_span.len(), 1);
    }

    #[test]
    fn line_single_line_middle() {
        let e = mk_err("abcde", 3, 1);
        assert_eq!(e.line(), 1);
    }

    #[test]
    fn line_multiline_inside_second_line() {
        let src = "first\nsecond\nthird";
        let offset_in_second = 8;
        let e = mk_err(src, offset_in_second, 1);
        assert_eq!(e.line(), 2, "offset inside the second line should return 2");
    }

    #[test]
    fn line_multiline_at_newline_boundary() {
        let src = "a\nbb\nccc\n";

        let e_second_nl = mk_err(src, 4, 0);
        assert_eq!(e_second_nl.line(), 2);

        let e_last_nl = mk_err(src, 8, 0);
        assert_eq!(e_last_nl.line(), 3);
    }

    #[test]
    fn display_message_is_fixed() {
        let e = mk_err("foo", 0, 1);
        assert_eq!(format!("{}", e), "Unterminated string");
    }

    #[test]
    fn parser_error_from_vec_maps_to_lexererrors_variant() {
        let e1: MietteError = io::Error::new(io::ErrorKind::Other, "lex oops 1").into();
        let e2: MietteError = io::Error::new(io::ErrorKind::Other, "lex oops 2").into();
        let perr: ParserError = vec![e1, e2].into();

        match perr {
            ParserError::LexerErrors { errors } => {
                assert_eq!(errors.len(), 2);
                assert_eq!(format!("{}", errors[0]), "lex oops 1");
                assert_eq!(format!("{}", errors[1]), "lex oops 2");
            }
            _ => panic!("expected LexerErrors variant"),
        }
    }

    #[test]
    fn parser_error_display_message_is_fixed() {
        let perr: ParserError = vec![io::Error::new(io::ErrorKind::Other, "x").into()].into();
        assert_eq!(format!("{}", perr), "parser errors occurred");
    }

    #[test]
    fn parser_error_has_diagnostic_code_and_help() {
        let perr: ParserError = vec![io::Error::new(io::ErrorKind::Other, "x").into()].into();

        let code_str = perr.code().map(|c| c.to_string());
        assert_eq!(code_str.as_deref(), Some("parser::lexer_errors"));

        let help_str = perr.help().map(|h| h.to_string());
        assert_eq!(
            help_str.as_deref(),
            Some("Fix the lexing errors before parsing")
        );
    }
}
