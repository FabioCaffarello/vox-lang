use diagnostics::diagnostics::DiagnosticsBagCell;
use text::span::TextSpan;
use typings::types::Type;

pub fn expect_type<'de>(
    diagnostics: &DiagnosticsBagCell<'de>,
    expected: Type,
    actual: &Type,
    span: &TextSpan<'de>,
) -> Type {
    if !actual.is_assignable_to(&expected) {
        diagnostics
            .borrow_mut()
            .report_type_mismatch(span, &expected, actual);
    }
    expected
}
