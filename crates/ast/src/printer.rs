use crate::ast::{
    ASTBinaryExpression, ASTLetStatement, ASTNumberExpression, ASTParenthesizedExpression,
    ASTStatement, ASTUnaryExpression, ASTVariableExpression,
};
use crate::visitor::ASTVisitor;
use termion::color::{self, Fg, Reset};
use text::span::TextSpan;

pub struct ASTPrinter {
    _indent: usize,
    pub result: String,
}

impl Default for ASTPrinter {
    fn default() -> Self {
        Self::new()
    }
}

impl ASTPrinter {
    const NUMBER_COLOR: color::Cyan = color::Cyan;
    const TEXT_COLOR: color::LightWhite = color::LightWhite;
    const KEYWORD_COLOR: color::Magenta = color::Magenta;
    const VARIABLE_COLOR: color::Green = color::Green;

    pub fn new() -> Self {
        Self {
            _indent: 0,
            result: String::new(),
        }
    }

    #[allow(clippy::single_char_add_str)]
    fn add_whitespace(&mut self) {
        self.result.push_str(" ");
    }

    #[allow(clippy::single_char_add_str)]
    fn _add_newline(&mut self) {
        self.result.push_str(
            "
",
        );
    }
}

impl<'de> ASTVisitor<'de> for ASTPrinter {
    fn visit_number_expression(&mut self, number: &ASTNumberExpression) {
        self.result
            .push_str(&format!("{}{}", Self::NUMBER_COLOR.fg_str(), number.number));
    }

    fn visit_error(&mut self, span: &TextSpan) {
        self.result
            .push_str(&format!("{}{}", Self::TEXT_COLOR.fg_str(), span.literal));
    }

    fn visit_statement(&mut self, statement: &ASTStatement) {
        ASTVisitor::do_visit_statement(self, statement);
        self.result.push_str(&format!("{}", Fg(Reset),));
    }

    fn visit_unary_expression(&mut self, unary_expression: &ASTUnaryExpression) {
        self.result.push_str(&format!(
            "{}{}",
            Self::TEXT_COLOR.fg_str(),
            unary_expression.operator.token.span.literal,
        ));
        self.visit_expression(&unary_expression.operand);
    }

    fn visit_binary_expression(&mut self, binary_expression: &ASTBinaryExpression) {
        self.visit_expression(&binary_expression.left);
        self.add_whitespace();
        self.result.push_str(&format!(
            "{}{}",
            Self::TEXT_COLOR.fg_str(),
            binary_expression.operator.token.span.literal,
        ));
        self.add_whitespace();
        self.visit_expression(&binary_expression.right);
    }

    fn visit_parenthesized_expression(
        &mut self,
        parenthesized_expression: &ASTParenthesizedExpression,
    ) {
        self.result
            .push_str(&format!("{}{}", Self::TEXT_COLOR.fg_str(), "(",));
        self.visit_expression(&parenthesized_expression.expression);
        self.result
            .push_str(&format!("{}{}", Self::TEXT_COLOR.fg_str(), ")",));
    }

    fn visit_let_statement(&mut self, let_statement: &ASTLetStatement) {
        self.result
            .push_str(&format!("{}{}", Self::KEYWORD_COLOR.fg_str(), "let"));
        self.add_whitespace();
        self.result.push_str(&format!(
            "{}{}",
            Self::TEXT_COLOR.fg_str(),
            let_statement.identifier.span.literal,
        ));
        self.add_whitespace();
        self.result
            .push_str(&format!("{}{}", Self::TEXT_COLOR.fg_str(), "=",));
        self.add_whitespace();
        self.visit_expression(&let_statement.initializer);
        self.add_whitespace(); // FIXME: This is a hack to make the output look better
    }

    fn visit_variable_expression(&mut self, variable_expression: &ASTVariableExpression) {
        self.result.push_str(&format!(
            "{}{}",
            Self::VARIABLE_COLOR.fg_str(),
            variable_expression.identifier.span.literal,
        ));
    }
}
