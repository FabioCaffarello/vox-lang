use crate::{
    ast::{
        ASTBooleanExpression, ASTBreakStatement, ASTFuncDeclStatement, ASTLetStatement,
        ASTNumberExpression, ASTUnaryExpression, ASTVariableExpression, Ast,
    },
    scopes::GlobalScope,
    visitor::ASTVisitor,
};
use diagnostics::diagnostics::DiagnosticsBagCell;
use text::span::TextSpan;

pub struct GlobalSymbolResolver<'a, 'de> {
    pub global_scope: GlobalScope,
    diagnostics: DiagnosticsBagCell<'de>,
    ast: &'a Ast<'de>,
}

impl<'a, 'de> GlobalSymbolResolver<'a, 'de> {
    pub fn new(diagnostics: DiagnosticsBagCell<'de>, ast: &'a Ast<'de>) -> Self {
        Self {
            ast,
            global_scope: GlobalScope::new(),
            diagnostics,
        }
    }
}

impl<'a, 'de> ASTVisitor<'de> for GlobalSymbolResolver<'a, 'de> {
    fn get_ast(&self) -> &Ast<'de> {
        self.ast
    }
    fn visit_func_decl_statement(&mut self, func_decl_statement: &ASTFuncDeclStatement<'de>) {
        let parameters = func_decl_statement
            .parameters
            .iter()
            .map(|parameter| parameter.identifier.span.literal.to_string())
            .collect();
        let literal_span = &func_decl_statement.identifier.span;
        match self.global_scope.declare_function(
            literal_span.literal,
            &func_decl_statement.body,
            parameters,
        ) {
            Ok(_) => {}
            Err(_) => {
                self.diagnostics
                    .borrow_mut()
                    .report_function_already_declared(&func_decl_statement.identifier);
            }
        }
    }

    fn visit_let_statement(&mut self, _let_statement: &ASTLetStatement) {}
    fn visit_variable_expression(&mut self, _variable_expression: &ASTVariableExpression) {}
    fn visit_number_expression(&mut self, _number: &ASTNumberExpression) {}
    fn visit_boolean_expression(&mut self, _boolean: &ASTBooleanExpression) {}
    fn visit_error(&mut self, _span: &TextSpan) {}
    fn visit_unary_expression(&mut self, _unary_expression: &ASTUnaryExpression) {}
    fn visit_break_statement(&mut self, _break_statement: &ASTBreakStatement<'de>) {}
}
