use miette::{Diagnostic, SourceSpan};
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
