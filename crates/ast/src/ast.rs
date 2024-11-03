use crate::printer::ASTPrinter;
use crate::visitor::ASTVisitor;
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

#[derive(Debug, Clone)]
pub enum ASTStatementKind<'de> {
    Expression(ASTExpression<'de>),
    LetStatement(ASTLetStatement<'de>),
    IfStatement(ASTIfStatement<'de>),
    BlockStatement(ASTBlockStatement<'de>),
    While(ASTWhileStatement<'de>),
    Break(ASTBreakStatement<'de>),
    FuncDecl(ASTFuncDeclStatement<'de>),
    Return(ASTReturnStatement<'de>),
}

#[derive(Debug, Clone)]
pub struct ASTStatement<'de> {
    pub kind: ASTStatementKind<'de>,
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

    pub fn if_statement(
        if_keyword: Token<'de>,
        condition: ASTExpression<'de>,
        then: ASTStatement<'de>,
        else_statement: Option<ASTElseStatement<'de>>,
    ) -> Self {
        ASTStatement::new(ASTStatementKind::IfStatement(ASTIfStatement {
            if_keyword,
            condition,
            then_branch: Box::new(then),
            else_branch: else_statement,
        }))
    }

    pub fn block_statement(statements: Vec<ASTStatement<'de>>) -> Self {
        ASTStatement::new(ASTStatementKind::BlockStatement(ASTBlockStatement {
            statements,
        }))
    }

    pub fn while_statement(
        label: Option<Token<'de>>,
        while_keyword: Token<'de>,
        condition: ASTExpression<'de>,
        body: ASTStatement<'de>,
    ) -> Self {
        ASTStatement::new(ASTStatementKind::While(ASTWhileStatement {
            label,
            while_keyword,
            condition,
            body: Box::new(body),
        }))
    }

    pub fn break_statement(break_keyword: Token<'de>, label: Option<Token<'de>>) -> Self {
        ASTStatement::new(ASTStatementKind::Break(ASTBreakStatement {
            break_keyword,
            label,
        }))
    }

    pub fn return_statement(
        return_keyword: Token<'de>,
        return_value: Option<ASTExpression<'de>>,
    ) -> Self {
        ASTStatement::new(ASTStatementKind::Return(ASTReturnStatement {
            return_keyword,
            return_value,
        }))
    }

    pub fn func_decl_statement(
        identifier: Token<'de>,
        parameters: Vec<FuncDeclParameter<'de>>,
        body: ASTStatement<'de>,
    ) -> Self {
        ASTStatement::new(ASTStatementKind::FuncDecl(ASTFuncDeclStatement {
            identifier,
            parameters,
            body: Box::new(body),
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
    Assignment(ASTAssignmentExpression<'de>),
    Boolean(ASTBooleanExpression<'de>),
    Call(ASTCallExpression<'de>),
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

    pub fn unary_expression(operator: ASTUnaryOperator<'de>, operand: ASTExpression<'de>) -> Self {
        ASTExpression::new(ASTExpressionKind::UnaryExpression(ASTUnaryExpression {
            operator,
            operand: Box::new(operand),
        }))
    }

    pub fn parenthesized_expression(expression: ASTExpression<'de>) -> Self {
        ASTExpression::new(ASTExpressionKind::ParenthesizedExpression(
            ASTParenthesizedExpression {
                expression: Box::new(expression),
            },
        ))
    }

    pub fn identifier(identifier: Token<'de>) -> Self {
        ASTExpression::new(ASTExpressionKind::Variable(ASTVariableExpression {
            identifier,
        }))
    }

    pub fn assignment(identifier: Token<'de>, expression: ASTExpression<'de>) -> Self {
        ASTExpression::new(ASTExpressionKind::Assignment(ASTAssignmentExpression {
            identifier,
            expression: Box::new(expression),
        }))
    }

    pub fn boolean(token: Token<'de>, value: bool) -> Self {
        ASTExpression::new(ASTExpressionKind::Boolean(ASTBooleanExpression {
            token,
            value,
        }))
    }

    pub fn call(identifier: Token<'de>, arguments: Vec<ASTExpression<'de>>) -> Self {
        ASTExpression::new(ASTExpressionKind::Call(ASTCallExpression {
            identifier,
            arguments,
        }))
    }

    pub fn error(span: TextSpan<'de>) -> Self {
        ASTExpression::new(ASTExpressionKind::Error(span))
    }
}

#[derive(Debug, Clone)]
pub struct ASTBinaryExpression<'de> {
    pub left: Box<ASTExpression<'de>>,
    pub operator: ASTBinaryOperator<'de>,
    pub right: Box<ASTExpression<'de>>,
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
    pub operand: Box<ASTExpression<'de>>,
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
    pub initializer: ASTExpression<'de>,
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
    pub condition: ASTExpression<'de>,
    pub then_branch: Box<ASTStatement<'de>>,
    pub else_branch: Option<ASTElseStatement<'de>>,
}

#[derive(Debug, Clone)]
pub struct ASTElseStatement<'de> {
    pub else_keyword: Token<'de>,
    pub else_statement: Box<ASTStatement<'de>>,
}

impl<'de> ASTElseStatement<'de> {
    pub fn new(else_keyword: Token<'de>, else_statement: ASTStatement<'de>) -> Self {
        ASTElseStatement {
            else_keyword,
            else_statement: Box::new(else_statement),
        }
    }
}

#[derive(Debug, Clone)]
pub struct ASTAssignmentExpression<'de> {
    pub identifier: Token<'de>,
    pub expression: Box<ASTExpression<'de>>,
}

#[derive(Debug, Clone)]
pub struct ASTBlockStatement<'de> {
    pub statements: Vec<ASTStatement<'de>>,
}

#[derive(Debug, Clone)]
pub struct ASTFuncDeclStatement<'de> {
    pub identifier: Token<'de>,
    pub parameters: Vec<FuncDeclParameter<'de>>,
    pub body: Box<ASTStatement<'de>>,
}

#[derive(Debug, Clone)]
pub struct ASTReturnStatement<'de> {
    pub return_keyword: Token<'de>,
    pub return_value: Option<ASTExpression<'de>>,
}

#[derive(Debug, Clone)]
pub struct FuncDeclParameter<'de> {
    pub identifier: Token<'de>,
}

#[derive(Debug, Clone)]
pub struct ASTWhileStatement<'de> {
    pub label: Option<Token<'de>>,
    pub while_keyword: Token<'de>,
    pub condition: ASTExpression<'de>,
    pub body: Box<ASTStatement<'de>>,
}

#[derive(Debug, Clone)]
pub struct ASTBreakStatement<'de> {
    pub break_keyword: Token<'de>,
    pub label: Option<Token<'de>>,
}

#[derive(Debug, Clone)]
pub struct ASTCallExpression<'de> {
    pub identifier: Token<'de>,
    pub arguments: Vec<ASTExpression<'de>>,
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
pub struct ASTParenthesizedExpression<'de> {
    pub expression: Box<ASTExpression<'de>>,
}
