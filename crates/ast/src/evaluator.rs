use std::collections::HashMap;

use crate::ast::{
    ASTBinaryExpression, ASTBinaryOperatorKind, ASTLetStatement, ASTNumberExpression,
    ASTUnaryExpression, ASTVariableExpression, ASTVisitor,
};
use text::span::TextSpan;

pub struct ASTEvaluator {
    pub last_value: Option<f64>,
    pub variables: HashMap<String, f64>,
}

impl ASTEvaluator {
    pub fn new() -> Self {
        Self {
            last_value: None,
            variables: HashMap::new(),
        }
    }
}

impl Default for ASTEvaluator {
    fn default() -> Self {
        Self::new()
    }
}

impl<'de> ASTVisitor<'de> for ASTEvaluator {
    fn visit_let_statement(&mut self, let_statement: &ASTLetStatement) {
        self.visit_expression(&let_statement.initializer);
        self.variables.insert(
            let_statement.identifier.span.literal.to_string(),
            self.last_value.unwrap(),
        );
    }

    fn visit_statement(&mut self, statement: &crate::ast::ASTStatement) {
        self.do_visit_statement(statement);
    }

    fn visit_variable_expression(&mut self, variable_expression: &ASTVariableExpression) {
        self.last_value = Some(
            *self
                .variables
                .get(variable_expression.identifier.span.literal)
                .unwrap(),
        );
    }

    fn visit_number_expression(&mut self, number: &ASTNumberExpression) {
        self.last_value = Some(number.number);
    }

    fn visit_unary_expression(&mut self, expr: &ASTUnaryExpression<'de>) {
        self.visit_expression(&expr.operand);
        let operand = self.last_value.unwrap();
        self.last_value = Some(match expr.operator.kind {
            crate::ast::ASTUnaryOperatorKind::Minus => -operand,
        });
    }

    fn visit_binary_expression(&mut self, expr: &ASTBinaryExpression) {
        self.visit_expression(&expr.left);
        let left = self.last_value.unwrap();
        self.visit_expression(&expr.right);
        let right = self.last_value.unwrap();
        self.last_value = Some(match expr.operator.kind {
            ASTBinaryOperatorKind::Plus => left + right,
            ASTBinaryOperatorKind::Subtract => left - right,
            ASTBinaryOperatorKind::Multiply => left * right,
            ASTBinaryOperatorKind::Divide => left / right,
            ASTBinaryOperatorKind::Power => left.powf(right),
        });
    }

    fn visit_parenthesized_expression(
        &mut self,
        parenthesized_expression: &crate::ast::ASTParenthesizedExpression,
    ) {
        self.visit_expression(&parenthesized_expression.expression);
    }

    fn visit_error(&mut self, _span: &TextSpan) {
        panic!("Cannot evaluate error expression")
    }
}
