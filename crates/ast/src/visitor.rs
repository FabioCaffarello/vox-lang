use crate::ast::{
    AssignmentExpr, Ast, BinaryExpr, BlockExpr, BooleanExpr, BreakStmt, CallExpr, ExprKind,
    Expression, FuncExpr, IfExpr, ItemKind, LetStmt, NumberExpr, ParenthesizedExpr, ReturnStmt,
    Statement, StmtKind, UnaryExpr, VariableExpr, WhileStmt,
};
use text::span::TextSpan;
use typings::types::{ExprID, ItemID, StmtID};

pub trait Visitor<'de> {
    fn visit_item(&mut self, ast: &mut Ast<'de>, item: ItemID) {
        self.visit_item_default(ast, item);
    }

    fn visit_item_default(&mut self, ast: &mut Ast<'de>, item: ItemID) {
        let item = ast.query_item(item).clone();
        match &item.kind {
            ItemKind::Stmt(stmt) => {
                self.visit_statement(ast, *stmt);
            }
        }
    }

    fn visit_variable_expression(
        &mut self,
        ast: &mut Ast<'de>,
        variable_expression: &VariableExpr<'de>,
        expr: &Expression<'de>,
    );

    fn visit_number_expression(
        &mut self,
        ast: &mut Ast<'de>,
        number: &NumberExpr,
        expr: &Expression<'de>,
    );

    fn visit_unary_expression(
        &mut self,
        ast: &mut Ast<'de>,
        unary_expression: &UnaryExpr<'de>,
        expr: &Expression<'de>,
    );

    fn visit_boolean_expression(
        &mut self,
        ast: &mut Ast<'de>,
        boolean: &BooleanExpr<'de>,
        expr: &Expression<'de>,
    );

    fn visit_let_statement(
        &mut self,
        ast: &mut Ast<'de>,
        let_statement: &LetStmt<'de>,
        stmt: &Statement<'de>,
    );

    fn visit_func_expression(
        &mut self,
        ast: &mut Ast<'de>,
        func_expr: &FuncExpr<'de>,
        expr_id: ExprID,
    );
    fn visit_break_statement(&mut self, ast: &mut Ast<'de>, break_statement: &BreakStmt<'de>);
    fn visit_error(&mut self, ast: &mut Ast<'de>, span: &TextSpan);

    fn do_visit_expression(&mut self, ast: &mut Ast<'de>, expr_id: ExprID) {
        let expression = ast.query_expr(expr_id).clone();
        match &expression.kind {
            ExprKind::Number(number) => {
                self.visit_number_expression(ast, number, &expression);
            }
            ExprKind::Binary(expr) => {
                self.visit_binary_expression(ast, expr, &expression);
            }
            ExprKind::Parenthesized(expr) => {
                self.visit_parenthesized_expression(ast, expr, &expression);
            }
            ExprKind::Error(span) => {
                self.visit_error(ast, span);
            }
            ExprKind::Variable(expr) => {
                self.visit_variable_expression(ast, expr, &expression);
            }
            ExprKind::Unary(expr) => {
                self.visit_unary_expression(ast, expr, &expression);
            }
            ExprKind::Assignment(expr) => {
                self.visit_assignment_expression(ast, expr, &expression);
            }
            ExprKind::Boolean(expr) => {
                self.visit_boolean_expression(ast, expr, &expression);
            }
            ExprKind::Call(expr) => {
                self.visit_call_expression(ast, expr, &expression);
            }
            ExprKind::If(expr) => {
                self.visit_if_expression(ast, expr, &expression);
            }
            ExprKind::Block(expr) => {
                self.visit_block_expression(ast, expr, &expression);
            }
            ExprKind::Func(func_expr) => {
                self.visit_func_expression(ast, func_expr, expr_id);
            }
        }
    }

    fn visit_expression(&mut self, ast: &mut Ast<'de>, expr_id: &ExprID) {
        self.do_visit_expression(ast, *expr_id);
    }

    fn visit_binary_expression(
        &mut self,
        ast: &mut Ast<'de>,
        binary_expression: &BinaryExpr<'de>,
        _expr: &Expression<'de>,
    ) {
        self.visit_expression(ast, &binary_expression.left);
        self.visit_expression(ast, &binary_expression.right);
    }

    fn visit_parenthesized_expression(
        &mut self,
        ast: &mut Ast<'de>,
        parenthesized_expression: &ParenthesizedExpr,
        _expr: &Expression<'de>,
    ) {
        self.visit_expression(ast, &parenthesized_expression.expression);
    }

    fn visit_assignment_expression(
        &mut self,
        ast: &mut Ast<'de>,
        assignment_expression: &AssignmentExpr<'de>,
        _expr: &Expression<'de>,
    ) {
        self.visit_expression(ast, &assignment_expression.expression);
    }

    fn visit_call_expression(
        &mut self,
        ast: &mut Ast<'de>,
        call_expression: &CallExpr<'de>,
        _expr: &Expression<'de>,
    ) {
        for argument in &call_expression.arguments {
            self.visit_expression(ast, argument);
        }
    }

    fn do_visit_statement(&mut self, ast: &mut Ast<'de>, stmt_id: StmtID) {
        let statement = ast.query_stmt(stmt_id).clone();
        match &statement.kind {
            StmtKind::Expr(expr_id) => {
                self.visit_expression(ast, expr_id);
            }
            StmtKind::Let(expr) => {
                self.visit_let_statement(ast, expr, &statement);
            }
            StmtKind::While(stmt) => {
                self.visit_while_statement(ast, stmt);
            }
            StmtKind::Return(stmt) => {
                self.visit_return_statement(ast, stmt);
            }
            StmtKind::Break(break_stmt) => {
                self.visit_break_statement(ast, break_stmt);
            }
        }
    }

    fn visit_statement(&mut self, ast: &mut Ast<'de>, statement: StmtID) {
        self.do_visit_statement(ast, statement);
    }

    fn visit_if_expression(
        &mut self,
        ast: &mut Ast<'de>,
        if_expression: &IfExpr<'de>,
        _expr: &Expression<'de>,
    ) {
        self.visit_expression(ast, &if_expression.condition);
        self.visit_expression(ast, &if_expression.then_branch);
        if let Some(else_branch) = &if_expression.else_branch {
            self.visit_expression(ast, &else_branch.expr);
        }
    }

    fn visit_block_expression(
        &mut self,
        ast: &mut Ast<'de>,
        block_expr: &BlockExpr,
        _expr: &Expression<'de>,
    ) {
        for expr in &block_expr.statements {
            self.visit_statement(ast, *expr);
        }
    }

    fn visit_return_statement(&mut self, ast: &mut Ast<'de>, return_statement: &ReturnStmt<'de>) {
        if let Some(expr) = &return_statement.return_value {
            self.visit_expression(ast, expr);
        }
    }

    fn visit_while_statement(&mut self, ast: &mut Ast<'de>, while_statement: &WhileStmt<'de>) {
        self.visit_expression(ast, &while_statement.condition);
        self.visit_expression(ast, &while_statement.body);
    }
}
