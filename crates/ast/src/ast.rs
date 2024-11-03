use std::collections::HashMap;

use crate::printer::ASTPrinter;
use crate::visitor::ASTVisitor;
use lexer::Token;
use support::counter::Counter;
use text::span::TextSpan;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ASTStmtID {
    pub id: usize,
}

impl ASTStmtID {
    pub fn new(id: usize) -> Self {
        Self { id }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ASTExprID {
    pub id: usize,
}

impl ASTExprID {
    pub fn new(id: usize) -> Self {
        Self { id }
    }
}

#[derive(Debug, Clone)]
pub struct ASTNodeIDGen {
    pub next_stmt_id: Counter,
    pub next_expr_id: Counter,
}

impl Default for ASTNodeIDGen {
    fn default() -> Self {
        Self::new()
    }
}

impl ASTNodeIDGen {
    pub fn new() -> Self {
        Self {
            next_stmt_id: Counter::new(),
            next_expr_id: Counter::new(),
        }
    }

    pub fn next_stmt_id(&self) -> ASTStmtID {
        let id = self.next_stmt_id.get_value();
        self.next_stmt_id.increment();
        ASTStmtID::new(id)
    }

    pub fn next_expr_id(&self) -> ASTExprID {
        let id = self.next_expr_id.get_value();
        self.next_expr_id.increment();
        ASTExprID::new(id)
    }
}

#[derive(Debug, Clone)]
pub struct Ast<'de> {
    pub statements: HashMap<ASTStmtID, ASTStatement<'de>>,
    pub expressions: HashMap<ASTExprID, ASTExpression<'de>>,
    pub top_level_statement_ids: Vec<ASTStmtID>,
    pub node_id_gen: ASTNodeIDGen,
}

impl<'de> Default for Ast<'de> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'de> Ast<'de> {
    pub fn new() -> Self {
        Self {
            statements: HashMap::new(),
            expressions: HashMap::new(),
            top_level_statement_ids: Vec::new(),
            node_id_gen: ASTNodeIDGen::new(),
        }
    }

    pub fn visit(&self, visitor: &mut dyn ASTVisitor<'de>) {
        for stmt_id in &self.top_level_statement_ids {
            visitor.visit_statement(stmt_id);
        }
    }

    pub fn visualize(&self) {
        let mut printer = ASTPrinter::new(self);
        self.visit(&mut printer);
        println!("{}", printer.result);
    }

    pub fn mark_top_level_stmt(&mut self, stmt_id: ASTStmtID) {
        self.top_level_statement_ids.push(stmt_id);
    }

    pub fn query_expr(&self, expr_id: &ASTExprID) -> &ASTExpression<'de> {
        &self.expressions[expr_id]
    }

    pub fn query_stmt(&self, stmt_id: &ASTStmtID) -> &ASTStatement<'de> {
        &self.statements[stmt_id]
    }

    fn stmt_from_kind(&mut self, kind: ASTStatementKind<'de>) -> &ASTStatement<'de> {
        let stmt = ASTStatement::new(kind, self.node_id_gen.next_stmt_id());
        let stmt_id = stmt.id;
        self.statements.insert(stmt_id, stmt);
        &self.statements[&stmt_id]
    }

    pub fn expression_statement(&mut self, expr_id: ASTExprID) -> &ASTStatement<'de> {
        self.stmt_from_kind(ASTStatementKind::Expression(expr_id))
    }

    pub fn let_statement(
        &mut self,
        identifier: Token<'de>,
        initializer: ASTExprID,
    ) -> &ASTStatement<'de> {
        self.stmt_from_kind(ASTStatementKind::LetStatement(ASTLetStatement {
            identifier,
            initializer,
        }))
    }

    pub fn if_statement(
        &mut self,
        if_keyword: Token<'de>,
        condition: ASTExprID,
        then: ASTStmtID,
        else_statement: Option<ASTElseStatement<'de>>,
    ) -> &ASTStatement<'de> {
        self.stmt_from_kind(ASTStatementKind::IfStatement(ASTIfStatement {
            if_keyword,
            condition,
            then_branch: then,
            else_branch: else_statement,
        }))
    }

    pub fn block_statement(&mut self, statements: Vec<ASTStmtID>) -> &ASTStatement<'de> {
        self.stmt_from_kind(ASTStatementKind::BlockStatement(ASTBlockStatement {
            statements,
        }))
    }

    pub fn while_statement(
        &mut self,
        label: Option<Token<'de>>,
        while_keyword: Token<'de>,
        condition: ASTExprID,
        body: ASTStmtID,
    ) -> &ASTStatement<'de> {
        self.stmt_from_kind(ASTStatementKind::While(ASTWhileStatement {
            label,
            while_keyword,
            condition,
            body,
        }))
    }

    pub fn break_statement(
        &mut self,
        break_keyword: Token<'de>,
        label: Option<Token<'de>>,
    ) -> &ASTStatement<'de> {
        self.stmt_from_kind(ASTStatementKind::Break(ASTBreakStatement {
            break_keyword,
            label,
        }))
    }

    pub fn return_statement(
        &mut self,
        return_keyword: Token<'de>,
        return_value: Option<ASTExprID>,
    ) -> &ASTStatement<'de> {
        self.stmt_from_kind(ASTStatementKind::Return(ASTReturnStatement {
            return_keyword,
            return_value,
        }))
    }

    pub fn func_decl_statement(
        &mut self,
        identifier: Token<'de>,
        parameters: Vec<FuncDeclParameter<'de>>,
        body: ASTStmtID,
    ) -> &ASTStatement<'de> {
        self.stmt_from_kind(ASTStatementKind::FuncDecl(ASTFuncDeclStatement {
            identifier,
            parameters,
            body,
        }))
    }

    pub fn expression_from_kind(&mut self, kind: ASTExpressionKind<'de>) -> &ASTExpression<'de> {
        let expr = ASTExpression::new(kind, self.node_id_gen.next_expr_id());
        let expr_id = expr.id;
        self.expressions.insert(expr_id, expr);
        &self.expressions[&expr_id]
    }

    pub fn number_literal_expression(&mut self, number: f64) -> &ASTExpression<'de> {
        self.expression_from_kind(ASTExpressionKind::NumberLiteral(ASTNumberExpression {
            number,
        }))
    }

    pub fn binary_expression(
        &mut self,
        left: ASTExprID,
        operator: ASTBinaryOperator<'de>,
        right: ASTExprID,
    ) -> &ASTExpression<'de> {
        self.expression_from_kind(ASTExpressionKind::BinaryExpression(ASTBinaryExpression {
            left,
            operator,
            right,
        }))
    }

    pub fn unary_expression(
        &mut self,
        operator: ASTUnaryOperator<'de>,
        operand: ASTExprID,
    ) -> &ASTExpression<'de> {
        self.expression_from_kind(ASTExpressionKind::UnaryExpression(ASTUnaryExpression {
            operator,
            operand,
        }))
    }

    pub fn parenthesized_expression(&mut self, expression: ASTExprID) -> &ASTExpression<'de> {
        self.expression_from_kind(ASTExpressionKind::ParenthesizedExpression(
            ASTParenthesizedExpression { expression },
        ))
    }

    pub fn identifier_expression(&mut self, identifier: Token<'de>) -> &ASTExpression<'de> {
        self.expression_from_kind(ASTExpressionKind::Variable(ASTVariableExpression {
            identifier,
        }))
    }

    pub fn assignment_expression(
        &mut self,
        identifier: Token<'de>,
        expression: ASTExprID,
    ) -> &ASTExpression<'de> {
        self.expression_from_kind(ASTExpressionKind::Assignment(ASTAssignmentExpression {
            identifier,
            expression,
        }))
    }

    pub fn boolean_expression(&mut self, token: Token<'de>, value: bool) -> &ASTExpression<'de> {
        self.expression_from_kind(ASTExpressionKind::Boolean(ASTBooleanExpression {
            token,
            value,
        }))
    }

    pub fn call_expression(
        &mut self,
        identifier: Token<'de>,
        arguments: Vec<ASTExprID>,
    ) -> &ASTExpression<'de> {
        self.expression_from_kind(ASTExpressionKind::Call(ASTCallExpression {
            identifier,
            arguments,
        }))
    }

    pub fn error_expression(&mut self, span: TextSpan<'de>) -> &ASTExpression<'de> {
        self.expression_from_kind(ASTExpressionKind::Error(span))
    }
}

#[derive(Debug, Clone)]
pub enum ASTStatementKind<'de> {
    Expression(ASTExprID),
    LetStatement(ASTLetStatement<'de>),
    IfStatement(ASTIfStatement<'de>),
    BlockStatement(ASTBlockStatement),
    While(ASTWhileStatement<'de>),
    Break(ASTBreakStatement<'de>),
    FuncDecl(ASTFuncDeclStatement<'de>),
    Return(ASTReturnStatement<'de>),
}

#[derive(Debug, Clone)]
pub struct ASTStatement<'de> {
    pub kind: ASTStatementKind<'de>,
    pub id: ASTStmtID,
}

impl<'de> ASTStatement<'de> {
    pub fn new(kind: ASTStatementKind<'de>, id: ASTStmtID) -> Self {
        Self { kind, id }
    }
}

#[derive(Debug, Clone)]
pub enum ASTExpressionKind<'de> {
    NumberLiteral(ASTNumberExpression),
    BinaryExpression(ASTBinaryExpression<'de>),
    UnaryExpression(ASTUnaryExpression<'de>),
    ParenthesizedExpression(ASTParenthesizedExpression),
    Error(TextSpan<'de>),
    Variable(ASTVariableExpression<'de>),
    Assignment(ASTAssignmentExpression<'de>),
    Boolean(ASTBooleanExpression<'de>),
    Call(ASTCallExpression<'de>),
}

#[derive(Debug, Clone)]
pub struct ASTExpression<'de> {
    pub kind: ASTExpressionKind<'de>,
    pub id: ASTExprID,
}

impl<'de> ASTExpression<'de> {
    pub fn new(kind: ASTExpressionKind<'de>, id: ASTExprID) -> Self {
        Self { kind, id }
    }
}

#[derive(Debug, Clone)]
pub struct ASTBinaryExpression<'de> {
    pub left: ASTExprID,
    pub operator: ASTBinaryOperator<'de>,
    pub right: ASTExprID,
}

#[derive(Debug, Clone)]
pub enum ASTBinaryOperatorKind {
    // Arithmetic
    Plus,
    Subtract,
    Multiply,
    Divide,
    Power,
    // Relational
    Equals,
    NotEquals,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
}

#[derive(Debug, Clone)]
pub struct ASTBinaryOperator<'de> {
    pub kind: ASTBinaryOperatorKind,
    pub token: Token<'de>,
}

impl<'de> ASTBinaryOperator<'de> {
    pub fn new(kind: ASTBinaryOperatorKind, token: Token<'de>) -> Self {
        ASTBinaryOperator { kind, token }
    }

    pub fn precedence(&self) -> u8 {
        match self.kind {
            ASTBinaryOperatorKind::Power => 30,
            ASTBinaryOperatorKind::Multiply => 20,
            ASTBinaryOperatorKind::Divide => 20,
            ASTBinaryOperatorKind::Plus => 10,
            ASTBinaryOperatorKind::Subtract => 10,
            ASTBinaryOperatorKind::Equals => 50,
            ASTBinaryOperatorKind::NotEquals => 50,
            ASTBinaryOperatorKind::LessThan => 45,
            ASTBinaryOperatorKind::LessThanOrEqual => 45,
            ASTBinaryOperatorKind::GreaterThan => 45,
            ASTBinaryOperatorKind::GreaterThanOrEqual => 45,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ASTUnaryExpression<'de> {
    pub operator: ASTUnaryOperator<'de>,
    pub operand: ASTExprID,
}

#[derive(Debug, Clone)]
pub enum ASTUnaryOperatorKind {
    Minus,
}

#[derive(Debug, Clone)]
pub struct ASTUnaryOperator<'de> {
    pub kind: ASTUnaryOperatorKind,
    pub token: Token<'de>,
}

impl<'de> ASTUnaryOperator<'de> {
    pub fn new(kind: ASTUnaryOperatorKind, token: Token<'de>) -> Self {
        ASTUnaryOperator { kind, token }
    }
}

#[derive(Debug, Clone)]
pub struct ASTLetStatement<'de> {
    pub identifier: Token<'de>,
    pub initializer: ASTExprID,
}

#[derive(Debug, Clone)]
pub struct ASTVariableExpression<'de> {
    pub identifier: Token<'de>,
}

impl<'de> ASTVariableExpression<'de> {
    pub fn identifier(&self) -> &str {
        self.identifier.span.literal
    }
}

#[derive(Debug, Clone)]
pub struct ASTIfStatement<'de> {
    pub if_keyword: Token<'de>,
    pub condition: ASTExprID,
    pub then_branch: ASTStmtID,
    pub else_branch: Option<ASTElseStatement<'de>>,
}

#[derive(Debug, Clone)]
pub struct ASTElseStatement<'de> {
    pub else_keyword: Token<'de>,
    pub else_statement: ASTStmtID,
}

impl<'de> ASTElseStatement<'de> {
    pub fn new(else_keyword: Token<'de>, else_statement: ASTStmtID) -> Self {
        ASTElseStatement {
            else_keyword,
            else_statement,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ASTAssignmentExpression<'de> {
    pub identifier: Token<'de>,
    pub expression: ASTExprID,
}

#[derive(Debug, Clone)]
pub struct ASTBlockStatement {
    pub statements: Vec<ASTStmtID>,
}

#[derive(Debug, Clone)]
pub struct ASTFuncDeclStatement<'de> {
    pub identifier: Token<'de>,
    pub parameters: Vec<FuncDeclParameter<'de>>,
    pub body: ASTStmtID,
}

#[derive(Debug, Clone)]
pub struct ASTReturnStatement<'de> {
    pub return_keyword: Token<'de>,
    pub return_value: Option<ASTExprID>,
}

#[derive(Debug, Clone)]
pub struct FuncDeclParameter<'de> {
    pub identifier: Token<'de>,
}

#[derive(Debug, Clone)]
pub struct ASTWhileStatement<'de> {
    pub label: Option<Token<'de>>,
    pub while_keyword: Token<'de>,
    pub condition: ASTExprID,
    pub body: ASTStmtID,
}

#[derive(Debug, Clone)]
pub struct ASTBreakStatement<'de> {
    pub break_keyword: Token<'de>,
    pub label: Option<Token<'de>>,
}

#[derive(Debug, Clone)]
pub struct ASTCallExpression<'de> {
    pub identifier: Token<'de>,
    pub arguments: Vec<ASTExprID>,
}

#[derive(Debug, Clone)]
pub struct ASTBooleanExpression<'de> {
    pub value: bool,
    pub token: Token<'de>,
}

#[derive(Debug, Clone)]
pub struct ASTNumberExpression {
    pub number: f64,
}

#[derive(Debug, Clone)]
pub struct ASTParenthesizedExpression {
    pub expression: ASTExprID,
}
