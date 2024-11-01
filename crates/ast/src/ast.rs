use crate::printer::ASTPrinter;
use lexer::Token;
use text::span::TextSpan;

#[derive(Debug, Clone)]
pub struct Ast<'de> {
    pub statements: Vec<ASTStatement<'de>>,
}

impl Default for Ast<'_> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'de> Ast<'de> {
    pub fn new() -> Self {
        Self {
            statements: Vec::new(),
        }
    }

    pub fn add_statement(&mut self, statement: ASTStatement<'de>) {
        self.statements.push(statement);
    }

    pub fn visit(&self, visitor: &mut dyn ASTVisitor<'de>) {
        for statement in &self.statements {
            visitor.visit_statement(statement);
        }
    }

    pub fn visualize(&self) {
        let mut printer = ASTPrinter::new();
        self.visit(&mut printer);
        println!("{}", printer.result);
    }
}

pub trait ASTVisitor<'de> {
    fn do_visit_statement(&mut self, statement: &ASTStatement<'de>) {
        match &statement.kind {
            ASTStatementKind::Expression(expr) => {
                self.visit_expression(expr);
            }
            ASTStatementKind::LetStatement(expr) => {
                self.visit_let_statement(expr);
            }
        }
    }

    fn visit_let_statement(&mut self, let_statement: &ASTLetStatement<'de>);

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
            ASTExpressionKind::UnaryExpression(expr) => self.visit_unary_expression(expr),
        }
    }

    fn visit_variable_expression(&mut self, variable_expression: &ASTVariableExpression<'de>);

    fn visit_expression(&mut self, expression: &ASTExpression<'de>) {
        self.do_visit_expression(expression);
    }

    fn visit_number_expression(&mut self, number: &ASTNumberExpression);

    fn visit_error(&mut self, span: &TextSpan);

    fn visit_unary_expression(&mut self, unary_expression: &ASTUnaryExpression<'de>);

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
}

#[derive(Debug, Clone)]
pub struct ASTNumberExpression {
    pub number: f64,
}

#[derive(Debug, Clone)]
pub struct ASTParenthesizedExpression<'de> {
    pub expression: Box<ASTExpression<'de>>,
}

#[derive(Debug, Clone)]
pub enum ASTBinaryOperatorKind {
    Plus,
    Subtract,
    Multiply,
    Divide,
    Power,
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
            ASTBinaryOperatorKind::Power => 3,
            ASTBinaryOperatorKind::Multiply => 2,
            ASTBinaryOperatorKind::Divide => 2,
            ASTBinaryOperatorKind::Plus => 1,
            ASTBinaryOperatorKind::Subtract => 1,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ASTBinaryExpression<'de> {
    pub left: Box<ASTExpression<'de>>,
    pub operator: ASTBinaryOperator<'de>,
    pub right: Box<ASTExpression<'de>>,
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
pub struct ASTUnaryExpression<'de> {
    pub operator: ASTUnaryOperator<'de>,
    pub operand: Box<ASTExpression<'de>>,
}

#[derive(Debug, Clone)]
pub enum ASTStatementKind<'de> {
    Expression(ASTExpression<'de>),
    LetStatement(ASTLetStatement<'de>),
}

#[derive(Debug, Clone)]
pub struct ASTLetStatement<'de> {
    pub identifier: Token<'de>,
    pub initializer: ASTExpression<'de>,
}

#[derive(Debug, Clone)]
pub struct ASTStatement<'de> {
    kind: ASTStatementKind<'de>,
}

impl<'de> ASTStatement<'de> {
    pub fn new(kind: ASTStatementKind<'de>) -> Self {
        Self { kind }
    }

    pub fn expression(expr: ASTExpression<'de>) -> Self {
        ASTStatement::new(ASTStatementKind::Expression(expr))
    }

    pub fn let_statement(identifier: Token<'de>, initializer: ASTExpression<'de>) -> Self {
        ASTStatement::new(ASTStatementKind::LetStatement(ASTLetStatement {
            identifier,
            initializer,
        }))
    }
}

#[derive(Debug, Clone)]
pub enum ASTExpressionKind<'de> {
    NumberLiteral(ASTNumberExpression),
    BinaryExpression(ASTBinaryExpression<'de>),
    UnaryExpression(ASTUnaryExpression<'de>),
    ParenthesizedExpression(ASTParenthesizedExpression<'de>),
    Error(TextSpan<'de>),
    Variable(ASTVariableExpression<'de>),
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
pub struct ASTExpression<'de> {
    pub kind: ASTExpressionKind<'de>,
}

impl<'de> ASTExpression<'de> {
    pub fn new(kind: ASTExpressionKind<'de>) -> Self {
        Self { kind }
    }

    pub fn number_literal(number: f64) -> Self {
        ASTExpression::new(ASTExpressionKind::NumberLiteral(ASTNumberExpression {
            number,
        }))
    }

    pub fn unary_expression(operator: ASTUnaryOperator<'de>, operand: ASTExpression<'de>) -> Self {
        ASTExpression::new(ASTExpressionKind::UnaryExpression(ASTUnaryExpression {
            operator,
            operand: Box::new(operand),
        }))
    }

    pub fn binary_expression(
        left: ASTExpression<'de>,
        operator: ASTBinaryOperator<'de>,
        right: ASTExpression<'de>,
    ) -> Self {
        ASTExpression::new(ASTExpressionKind::BinaryExpression(ASTBinaryExpression {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }))
    }

    pub fn parenthesized_expression(expression: ASTExpression<'de>) -> Self {
        ASTExpression::new(ASTExpressionKind::ParenthesizedExpression(
            ASTParenthesizedExpression {
                expression: Box::new(expression),
            },
        ))
    }

    pub fn error(span: TextSpan<'de>) -> Self {
        ASTExpression::new(ASTExpressionKind::Error(span))
    }

    pub fn identifier(identifier: Token<'de>) -> Self {
        ASTExpression::new(ASTExpressionKind::Variable(ASTVariableExpression {
            identifier,
        }))
    }
}
