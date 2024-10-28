use crate::ast::{ASTBinaryExpression, ASTBinaryOperatorKind, ASTNumberExpression, ASTVisitor};

pub struct ASTEvaluator {
    pub last_value: Option<f64>,
}

impl ASTEvaluator {
    pub fn new() -> Self {
        Self { last_value: None }
    }
}

impl ASTVisitor for ASTEvaluator {
    fn visit_number_literal(&mut self, number: &ASTNumberExpression) {
        self.last_value = Some(number.number);
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
        });
    }

    fn visit_parenthesized_expression(
        &mut self,
        parenthesized_expression: &crate::ast::ASTParenthesizedExpression,
    ) {
        self.visit_expression(&parenthesized_expression.expression);
    }
}
