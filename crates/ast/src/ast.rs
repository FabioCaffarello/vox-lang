use lexer::Token;

pub struct Ast<'de> {
    pub statements: Vec<ASTStatement<'de>>,
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

    pub fn visit(&self, visitor: &mut dyn ASTVisitor) {
        for statement in &self.statements {
            visitor.visit_statement(statement);
        }
    }

    pub fn visualize(&self) -> () {
        let mut printer = ASTPrinter { indent: 0 };
        self.visit(&mut printer);
    }
}

pub trait ASTVisitor {
    fn do_visit_statement(&mut self, statement: &ASTStatement) {
        match &statement.kind {
            ASTStatementKind::Expression(expr) => {
                self.visit_expression(expr);
            }
        }
    }

    fn visit_statement(&mut self, statement: &ASTStatement) {
        self.do_visit_statement(statement);
    }

    fn do_visit_expression(&mut self, expression: &ASTExpression) {
        match &expression.kind {
            ASTExpressionKind::NumberLiteral(number) => {
                self.visit_number_literal(number);
            }
            ASTExpressionKind::BinaryExpression(expr) => {
                self.visit_binary_expression(expr);
            }
            ASTExpressionKind::ParenthesizedExpression(expr) => {
                self.visit_parenthesized_expression(expr);
            }
        }
    }

    fn visit_expression(&mut self, expression: &ASTExpression) {
        self.do_visit_expression(expression);
    }

    fn visit_number_literal(&mut self, number: &ASTNumberExpression);

    fn visit_binary_expression(&mut self, binary_expression: &ASTBinaryExpression);

    fn do_visit_binary_expression(&mut self, binary_expression: &ASTBinaryExpression) {
        self.visit_expression(&binary_expression.left);
        self.visit_expression(&binary_expression.right);
    }

    fn visit_parenthesized_expression(&mut self, parenthesized_expression: &ASTParenthesizedExpression);

    fn do_visit_parenthesized_expression(&mut self, parenthesized_expression: &ASTParenthesizedExpression) {
        self.visit_expression(&parenthesized_expression.expression);
    }
}

pub struct ASTPrinter {
    indent: usize,
}

const LEVEL_INDENT: usize = 2;

impl ASTVisitor for ASTPrinter {
    fn visit_number_literal(&mut self, number: &ASTNumberExpression) {
        self.print_with_indent(&format!("NumberLiteral: {}", number.number));
    }

    fn visit_statement(&mut self, statement: &ASTStatement) {
        self.print_with_indent("Statement:");
        self.indent += LEVEL_INDENT;
        ASTVisitor::do_visit_statement(self, statement);
        self.indent -= LEVEL_INDENT;
    }

    fn visit_expression(&mut self, expression: &ASTExpression) {
        self.print_with_indent("Expression:");
        self.indent += LEVEL_INDENT;
        ASTVisitor::do_visit_expression(self, expression);
        self.indent -= LEVEL_INDENT;
    }

    fn visit_binary_expression(&mut self, binary_expression: &ASTBinaryExpression) {
        self.print_with_indent("Binary Expression:");
        self.indent += LEVEL_INDENT;
        self.print_with_indent(&format!("Operator: {:?}", binary_expression.operator.kind));
        ASTVisitor::do_visit_binary_expression(self, binary_expression);
        self.indent -= LEVEL_INDENT;
    }

    fn visit_parenthesized_expression(&mut self, parenthesized_expression: &ASTParenthesizedExpression) {
        self.print_with_indent("Parenthesized Expression:");
        self.indent += LEVEL_INDENT;
        ASTVisitor::do_visit_parenthesized_expression(self, parenthesized_expression);
        self.indent -= LEVEL_INDENT;
    }
}

impl ASTPrinter {
    pub fn print_with_indent(&self, text: &str) {
        println!("{}{}", " ".repeat(self.indent), text);
    }
}

pub struct ASTNumberExpression {
    pub number: f64,
}

pub struct ASTParenthesizedExpression<'de> {
    pub expression: Box<ASTExpression<'de>>,
}

#[derive(Debug)]
pub enum ASTBinaryOperatorKind {
    Plus,
    Subtract,
    Multiply,
    Divide,
}

pub struct ASTBinaryOperator<'de> {
    pub kind: ASTBinaryOperatorKind,
    token: Token<'de>,
}

impl<'de> ASTBinaryOperator<'de> {
    pub fn new(kind: ASTBinaryOperatorKind, token: Token<'de>) -> Self {
        ASTBinaryOperator { kind, token }
    }

    pub fn precedence(&self) -> u8 {
        match self.kind {
            ASTBinaryOperatorKind::Plus => 1,
            ASTBinaryOperatorKind::Subtract => 1,
            ASTBinaryOperatorKind::Multiply => 2,
            ASTBinaryOperatorKind::Divide => 2,
        }
    }
}

pub struct ASTBinaryExpression<'de> {
    pub left: Box<ASTExpression<'de>>,
    pub operator: ASTBinaryOperator<'de>,
    pub right: Box<ASTExpression<'de>>,
}

pub enum ASTStatementKind<'de> {
    Expression(ASTExpression<'de>),
}

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
}

pub enum ASTExpressionKind<'de> {
    NumberLiteral(ASTNumberExpression),
    BinaryExpression(ASTBinaryExpression<'de>),
    ParenthesizedExpression(ASTParenthesizedExpression<'de>),
}

pub struct ASTExpression<'de> {
    kind: ASTExpressionKind<'de>,
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

    pub fn parenthesized_expression(expression: ASTExpression<'de>) -> Self {
        ASTExpression::new(ASTExpressionKind::ParenthesizedExpression(
            ASTParenthesizedExpression {
                expression: Box::new(expression),
            },
        ))
    }
}
