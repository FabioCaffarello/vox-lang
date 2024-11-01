use crate::ast::{
    ASTAssignmentExpression, ASTBinaryExpression, ASTBlockStatement, ASTBooleanExpression,
    ASTCallExpression, ASTExpression, ASTExpressionKind, ASTFuncDeclStatement, ASTIfStatement,
    ASTLetStatement, ASTNumberExpression, ASTParenthesizedExpression, ASTReturnStatement,
    ASTStatement, ASTStatementKind, ASTUnaryExpression, ASTVariableExpression, ASTWhileStatement,
};
use text::span::TextSpan;

pub trait ASTVisitor<'de> {
    fn visit_let_statement(&mut self, let_statement: &ASTLetStatement<'de>);
    fn visit_variable_expression(&mut self, variable_expression: &ASTVariableExpression<'de>);
    fn visit_number_expression(&mut self, number: &ASTNumberExpression);
    fn visit_error(&mut self, span: &TextSpan);
    fn visit_unary_expression(&mut self, unary_expression: &ASTUnaryExpression<'de>);
    fn visit_func_decl_statement(&mut self, func_decl_statement: &ASTFuncDeclStatement<'de>);
    fn visit_boolean_expression(&mut self, boolean: &ASTBooleanExpression<'de>);

    fn do_visit_statement(&mut self, statement: &ASTStatement<'de>) {
        match &statement.kind {
            ASTStatementKind::Expression(expr) => {
                self.visit_expression(expr);
            }
            ASTStatementKind::LetStatement(expr) => {
                self.visit_let_statement(expr);
            }
            ASTStatementKind::IfStatement(stmt) => {
                self.visit_if_statement(stmt);
            }
            ASTStatementKind::BlockStatement(stmt) => {
                self.visit_block_statement(stmt);
            }
            ASTStatementKind::While(stmt) => {
                self.visit_while_statement(stmt);
            }
            ASTStatementKind::FuncDecl(stmt) => {
                self.visit_func_decl_statement(stmt);
            }
            ASTStatementKind::Return(stmt) => {
                self.visit_return_statement(stmt);
            }
        }
    }

    fn visit_statement(&mut self, statement: &ASTStatement<'de>) {
        self.do_visit_statement(statement);
    }

    fn do_visit_expression(&mut self, expression: &ASTExpression<'de>) {
        match &expression.kind {
            ASTExpressionKind::NumberLiteral(number) => {
                self.visit_number_expression(number);
            }
            ASTExpressionKind::BinaryExpression(expr) => {
                self.visit_binary_expression(expr);
            }
            ASTExpressionKind::ParenthesizedExpression(expr) => {
                self.visit_parenthesized_expression(expr);
            }
            ASTExpressionKind::Error(span) => {
                self.visit_error(span);
            }
            ASTExpressionKind::Variable(expr) => {
                self.visit_variable_expression(expr);
            }
            ASTExpressionKind::UnaryExpression(expr) => {
                self.visit_unary_expression(expr);
            }
            ASTExpressionKind::Assignment(expr) => {
                self.visit_assignment_expression(expr);
            }
            ASTExpressionKind::Boolean(expr) => {
                self.visit_boolean_expression(expr);
            }
            ASTExpressionKind::Call(expr) => {
                self.visit_call_expression(expr);
            }
        }
    }

    fn visit_expression(&mut self, expression: &ASTExpression<'de>) {
        self.do_visit_expression(expression);
    }

    fn visit_binary_expression(&mut self, binary_expression: &ASTBinaryExpression<'de>) {
        self.visit_expression(&binary_expression.left);
        self.visit_expression(&binary_expression.right);
    }

    fn visit_parenthesized_expression(
        &mut self,
        parenthesized_expression: &ASTParenthesizedExpression<'de>,
    ) {
        self.visit_expression(&parenthesized_expression.expression);
    }

    fn visit_if_statement(&mut self, if_statement: &ASTIfStatement<'de>) {
        self.visit_expression(&if_statement.condition);
        self.visit_statement(&if_statement.then_branch);
        if let Some(else_branch) = &if_statement.else_branch {
            self.visit_statement(&else_branch.else_statement);
        }
    }

    fn visit_block_statement(&mut self, block_statement: &ASTBlockStatement<'de>) {
        for statement in &block_statement.statements {
            self.visit_statement(statement);
        }
    }

    fn visit_assignment_expression(
        &mut self,
        assignment_expression: &ASTAssignmentExpression<'de>,
    ) {
        self.visit_expression(&assignment_expression.expression);
    }

    fn visit_return_statement(&mut self, return_statement: &ASTReturnStatement<'de>) {
        if let Some(expr) = &return_statement.return_value {
            self.visit_expression(expr);
        }
    }

    fn visit_while_statement(&mut self, while_statement: &ASTWhileStatement<'de>) {
        self.visit_expression(&while_statement.condition);
        self.visit_statement(&while_statement.body);
    }

    fn visit_call_expression(&mut self, call_expression: &ASTCallExpression<'de>) {
        for argument in &call_expression.arguments {
            self.visit_expression(argument);
        }
    }
}
