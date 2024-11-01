use std::collections::HashMap;

use crate::ast::{ASTLetStatement, ASTNumberExpression, ASTUnaryExpression, ASTVariableExpression};
use crate::visitor::ASTVisitor;
use diagnostics::diagnostics::DiagnosticsBagCell;
use text::span::TextSpan;

pub struct SymbolChecker<'de> {
    symbols: HashMap<String, ()>,
    diagnostics_bag: DiagnosticsBagCell<'de>,
}

impl<'de> SymbolChecker<'de> {
    pub fn new(diagnostics_bag: DiagnosticsBagCell<'de>) -> Self {
        Self {
            symbols: HashMap::new(),
            diagnostics_bag,
        }
    }
}

impl<'de> ASTVisitor<'de> for SymbolChecker<'de> {
    fn visit_let_statement(&mut self, let_statement: &ASTLetStatement<'de>) {
        let identifier = let_statement.identifier.span.literal.to_string().clone();
        self.visit_expression(&let_statement.initializer);
        self.symbols.insert(identifier, ());
    }

    fn visit_variable_expression(&mut self, variable_expression: &ASTVariableExpression<'de>) {
        if !self
            .symbols
            .contains_key(variable_expression.identifier.span.literal)
        {
            self.diagnostics_bag
                .borrow_mut()
                .report_undefined_variable(&variable_expression.identifier);
        }
    }

    fn visit_number_expression(&mut self, _number: &ASTNumberExpression) {
        // Implement as needed
    }

    fn visit_error(&mut self, _span: &TextSpan) {
        // Implement as needed
    }

    fn visit_unary_expression(&mut self, unary_expression: &ASTUnaryExpression<'de>) {
        self.visit_expression(&unary_expression.operand);
    }
}
