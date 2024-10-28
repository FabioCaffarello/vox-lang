use miette::{Diagnostic, MietteError};
use thiserror::Error;

#[derive(Debug, Error, Diagnostic)]
#[error("parser errors occurred")]
pub enum ParserError {
    #[error("Lexer errors occurred")]
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
