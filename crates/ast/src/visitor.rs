use crate::ast::{
    ASTAssignmentExpression, ASTBinaryExpression, ASTBlockStatement, ASTBooleanExpression,
    ASTBreakStatement, ASTCallExpression, ASTExprID, ASTExpression, ASTExpressionKind,
    ASTFuncDeclStatement, ASTIfStatement, ASTLetStatement, ASTNumberExpression,
    ASTParenthesizedExpression, ASTReturnStatement, ASTStatementKind, ASTStmtID,
    ASTUnaryExpression, ASTVariableExpression, ASTWhileStatement, Ast,
};
use text::span::TextSpan;

pub trait ASTVisitor<'de> {
    fn visit_variable_expression(
        &mut self,
        variable_expression: &ASTVariableExpression<'de>,
        expr: &ASTExpression<'de>,
    );
    fn visit_number_expression(&mut self, number: &ASTNumberExpression, expr: &ASTExpression<'de>);
    fn visit_unary_expression(
        &mut self,
        unary_expression: &ASTUnaryExpression<'de>,
        expr: &ASTExpression<'de>,
    );
    fn visit_boolean_expression(
        &mut self,
        boolean: &ASTBooleanExpression<'de>,
        expr: &ASTExpression<'de>,
    );
    fn visit_let_statement(&mut self, let_statement: &ASTLetStatement<'de>);
    fn visit_func_decl_statement(&mut self, func_decl_statement: &ASTFuncDeclStatement<'de>);
    fn visit_break_statement(&mut self, break_statement: &ASTBreakStatement<'de>);
    fn visit_error(&mut self, span: &TextSpan);

    fn get_ast(&self) -> &Ast<'de>;

    fn do_visit_expression(&mut self, expr_id: &ASTExprID) {
        let expression = self.get_ast().query_expr(expr_id).clone();
        match &expression.kind {
            ASTExpressionKind::NumberLiteral(number) => {
                self.visit_number_expression(number, &expression);
            }
            ASTExpressionKind::BinaryExpression(expr) => {
                self.visit_binary_expression(expr, &expression);
            }
            ASTExpressionKind::ParenthesizedExpression(expr) => {
                self.visit_parenthesized_expression(expr, &expression);
            }
            ASTExpressionKind::Error(span) => {
                self.visit_error(span);
            }
            ASTExpressionKind::Variable(expr) => {
                self.visit_variable_expression(expr, &expression);
            }
            ASTExpressionKind::UnaryExpression(expr) => {
                self.visit_unary_expression(expr, &expression);
            }
            ASTExpressionKind::Assignment(expr) => {
                self.visit_assignment_expression(expr, &expression);
            }
            ASTExpressionKind::Boolean(expr) => {
                self.visit_boolean_expression(expr, &expression);
            }
            ASTExpressionKind::Call(expr) => {
                self.visit_call_expression(expr, &expression);
            }
        }
    }

    fn visit_expression(&mut self, expr_id: &ASTExprID) {
        self.do_visit_expression(expr_id);
    }

    fn visit_binary_expression(
        &mut self,
        binary_expression: &ASTBinaryExpression<'de>,
        _expr: &ASTExpression<'de>,
    ) {
        self.visit_expression(&binary_expression.left);
        self.visit_expression(&binary_expression.right);
    }

    fn visit_parenthesized_expression(
        &mut self,
        parenthesized_expression: &ASTParenthesizedExpression,
        _expr: &ASTExpression<'de>,
    ) {
        self.visit_expression(&parenthesized_expression.expression);
    }

    fn visit_assignment_expression(
        &mut self,
        assignment_expression: &ASTAssignmentExpression<'de>,
        _expr: &ASTExpression<'de>,
    ) {
        self.visit_expression(&assignment_expression.expression);
    }

    fn visit_call_expression(
        &mut self,
        call_expression: &ASTCallExpression<'de>,
        _expr: &ASTExpression<'de>,
    ) {
        for argument in &call_expression.arguments {
            self.visit_expression(argument);
        }
    }

    fn do_visit_statement(&mut self, stmt_id: &ASTStmtID) {
        let statement = self.get_ast().query_stmt(stmt_id).clone();
        match &statement.kind {
            ASTStatementKind::Expression(expr_id) => {
                self.visit_expression(expr_id);
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
            ASTStatementKind::Break(break_stmt) => {
                self.visit_break_statement(break_stmt);
            }
        }
    }

    fn visit_statement(&mut self, statement: &ASTStmtID) {
        self.do_visit_statement(statement);
    }

    fn visit_if_statement(&mut self, if_statement: &ASTIfStatement<'de>) {
        self.visit_expression(&if_statement.condition);
        self.visit_statement(&if_statement.then_branch);
        if let Some(else_branch) = &if_statement.else_branch {
            self.visit_statement(&else_branch.else_statement);
        }
    }

    fn visit_block_statement(&mut self, block_statement: &ASTBlockStatement) {
        for statement in &block_statement.statements {
            self.visit_statement(statement);
        }
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
}
