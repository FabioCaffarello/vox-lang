use diagnostics::diagnostics::DiagnosticsBagCell;
use lexer::token::Token;
use typings::types::Type;

pub fn resolve_type_from_string<'de>(
    diagnostics: &DiagnosticsBagCell<'de>,
    type_name: &Token<'de>,
) -> Type {
    let ty = Type::from_str(type_name.span.literal);
    let ty = match ty {
        Some(ty) => ty,
        None => {
            diagnostics.borrow_mut().report_undeclared_type(type_name);
            Type::Error
        }
    };
    ty
}
