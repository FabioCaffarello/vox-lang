use crate::{
    ast::{
        Ast, BooleanExpr, BreakStmt, Expression, FunctionDeclaration, LetStmt, NumberExpr,
        Statement, UnaryExpr, VariableExpr,
    },
    scopes::GlobalScope,
    support::resolve_type_from_string,
    visitor::Visitor,
};
use diagnostics::diagnostics::DiagnosticsBagCell;
use text::span::TextSpan;
use typings::Type;

pub struct GlobalSymbolResolver<'de> {
    pub global_scope: GlobalScope,
    diagnostics: DiagnosticsBagCell<'de>,
}

impl<'de> GlobalSymbolResolver<'de> {
    pub fn new(diagnostics: DiagnosticsBagCell<'de>) -> Self {
        Self {
            global_scope: GlobalScope::new(),
            diagnostics,
        }
    }
}

impl<'de> Visitor<'de> for GlobalSymbolResolver<'de> {
    fn visit_function_declaration(
        &mut self,
        _ast: &mut Ast<'de>,
        func_decl: &FunctionDeclaration<'de>,
    ) {
        let parameters = func_decl
            .parameters
            .iter()
            .map(|parameter| {
                self.global_scope.declare_variable(
                    parameter.identifier.span.literal,
                    resolve_type_from_string(
                        &self.diagnostics,
                        &parameter.type_annotation.type_name,
                    ),
                    false,
                )
            })
            .collect();

        let literal_span = &func_decl.identifier.span;
        let return_type = match &func_decl.return_type {
            Some(return_type) => {
                resolve_type_from_string(&self.diagnostics, &return_type.type_name)
            }
            None => Type::Void,
        };
        match self.global_scope.declare_function(
            literal_span.literal,
            &func_decl.body,
            parameters,
            return_type,
        ) {
            Ok(_) => {}
            Err(_) => {
                self.diagnostics
                    .borrow_mut()
                    .report_function_already_declared(&func_decl.identifier);
            }
        }
    }

    fn visit_let_statement(
        &mut self,
        _ast: &mut Ast<'de>,
        _let_statement: &LetStmt,
        _stmt: &Statement<'de>,
    ) {
    }
    fn visit_variable_expression(
        &mut self,
        _ast: &mut Ast<'de>,
        _variable_expression: &VariableExpr,
        _expr: &Expression<'de>,
    ) {
    }
    fn visit_number_expression(
        &mut self,
        _ast: &mut Ast<'de>,
        _number: &NumberExpr,
        _expr: &Expression<'de>,
    ) {
    }
    fn visit_boolean_expression(
        &mut self,
        _ast: &mut Ast<'de>,
        _boolean: &BooleanExpr,
        _expr: &Expression<'de>,
    ) {
    }
    fn visit_error(&mut self, _ast: &mut Ast<'de>, _span: &TextSpan) {}
    fn visit_unary_expression(
        &mut self,
        _ast: &mut Ast<'de>,
        _unary_expression: &UnaryExpr,
        _expr: &Expression<'de>,
    ) {
    }
    fn visit_break_statement(&mut self, _ast: &mut Ast<'de>, _break_statement: &BreakStmt<'de>) {}
}
