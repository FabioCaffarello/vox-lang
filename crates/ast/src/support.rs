use diagnostics::diagnostics::DiagnosticsBagCell;
use lexer::token::Token;
use text::span::TextSpan;
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

pub fn expect_type<'de>(
    diagnostics: &DiagnosticsBagCell<'de>,
    span: &TextSpan<'de>,
    expected: Type,
    actual: &Type,
) {
    if !actual.is_assignable_to(&expected) {
        diagnostics
            .borrow_mut()
            .report_type_mismatch(span, &expected, actual);
    }
}

// pub fn resolve_let_statement<'de>(
//     ast: &Ast<'de>,
//     diagnostics: &DiagnosticsBagCell<'de>,
//     scopes: &mut Scopes,
//     visitor: &mut dyn ASTVisitor<'de>,
//     let_statement: &ASTLetStatement<'de>,
// ) {
//     let identifier = let_statement.identifier.span.literal;
//     visitor.visit_expression(&let_statement.initializer);
//     let initializer_expression = ast.query_expr(&let_statement.initializer);
//     let ty = match &let_statement.type_annotation {
//         Some(type_annotation) => {
//             let ty = resolve_type_from_string(&diagnostics, &type_annotation.type_name);
//             expect_type(
//                 &diagnostics,
//                 &initializer_expression.span(ast),
//                 ty.clone(),
//                 &initializer_expression.ty,
//             );
//             ty
//         }
//         None => initializer_expression.ty.clone(),
//     };
//     scopes.declare_variable(identifier, ty);
// }
