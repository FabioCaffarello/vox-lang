use crate::{
    ast::{
        ASTBooleanExpression, ASTBreakStatement, ASTExpression, ASTFuncDeclStatement,
        ASTLetStatement, ASTNumberExpression, ASTUnaryExpression, ASTVariableExpression, Ast,
    },
    scopes::{GlobalScope, VariableSymbol},
    support::resolve_type_from_string,
    visitor::ASTVisitor,
};
use diagnostics::diagnostics::DiagnosticsBagCell;
use text::span::TextSpan;
use typings::Type;

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
            .map(|parameter| {
                VariableSymbol::new(
                    parameter.identifier.span.literal.to_string(),
                    resolve_type_from_string(
                        &self.diagnostics,
                        &parameter.type_annotation.type_name,
                    ),
                )
            })
            .collect();
        let literal_span = &func_decl_statement.identifier.span;
        let return_type = match &func_decl_statement.return_type {
            Some(return_type) => {
                resolve_type_from_string(&self.diagnostics, &return_type.type_name)
            }
            None => Type::Void,
        };
        match self.global_scope.declare_function(
            literal_span.literal,
            &func_decl_statement.body,
            parameters,
            return_type,
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
    fn visit_variable_expression(
        &mut self,
        _variable_expression: &ASTVariableExpression,
        _expr: &ASTExpression<'de>,
    ) {
    }
    fn visit_number_expression(
        &mut self,
        _number: &ASTNumberExpression,
        _expr: &ASTExpression<'de>,
    ) {
    }
    fn visit_boolean_expression(
        &mut self,
        _boolean: &ASTBooleanExpression,
        _expr: &ASTExpression<'de>,
    ) {
    }
    fn visit_error(&mut self, _span: &TextSpan) {}
    fn visit_unary_expression(
        &mut self,
        _unary_expression: &ASTUnaryExpression,
        _expr: &ASTExpression<'de>,
    ) {
    }
    fn visit_break_statement(&mut self, _break_statement: &ASTBreakStatement<'de>) {}
}
