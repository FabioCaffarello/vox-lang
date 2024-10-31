use std::cell::Cell;

use crate::errors::ParserError;
use ast::{
    ast::ASTBinaryOperator, ASTBinaryOperatorKind, ASTExpression, ASTStatement, ASTUnaryOperator,
    ASTUnaryOperatorKind,
};
use diagnostics::diagnostics::DiagnosticsBagCell;
use lexer::{Lexer, Token, TokenKind};
use miette::MietteError;

pub struct Counter {
    value: Cell<usize>,
}

impl Default for Counter {
    fn default() -> Self {
        Self::new()
    }
}

impl Counter {
    pub fn new() -> Self {
        Self {
            value: Cell::new(0),
        }
    }

    pub fn increment(&self) {
        let current_value = self.value.get();
        self.value.set(current_value + 1);
    }

    pub fn get_value(&self) -> usize {
        self.value.get()
    }
}

pub struct Parser<'de> {
    pub tokens: Vec<Token<'de>>,
    current: Counter,
    pub diagnostics_bag: DiagnosticsBagCell<'de>,
}

impl<'de> Parser<'de> {
    pub fn new(tokens: Vec<Token<'de>>, diagnostics_bag: DiagnosticsBagCell<'de>) -> Self {
        Self {
            tokens: tokens
                .iter()
                .filter(|token| {
                    token.kind != TokenKind::LineComment && token.kind != TokenKind::BlockComment
                })
                .copied()
                .collect(),
            current: Counter::new(),
            diagnostics_bag,
        }
    }

    pub fn from_input(
        input: &'de str,
        diagnostics_bag: DiagnosticsBagCell<'de>,
    ) -> Result<Self, ParserError> {
        let mut lexer = Lexer::new(input);
        let (tokens, errors) = lexer.collect_tokens();

        if !errors.is_empty() {
            let errors: Vec<MietteError> = errors
                .into_iter()
                .map(|e| MietteError::from(std::io::Error::new(std::io::ErrorKind::Other, e)))
                .collect();
            return Err(ParserError::LexerErrors { errors });
        }

        Ok(Self::new(tokens, diagnostics_bag))
    }

    fn peek(&self, offset: isize) -> &Token<'de> {
        let mut index = (self.current.get_value() as isize + offset) as usize;
        if index >= self.tokens.len() {
            index = self.tokens.len() - 1;
        }
        self.tokens.get(index).unwrap()
    }

    fn current(&self) -> &Token<'de> {
        self.peek(0)
    }

    fn consume(&self) -> Token<'de> {
        self.current.increment();
        *self.peek(-1)
    }

    fn consume_and_check(&self, kind: TokenKind) -> Token<'de> {
        let token = self.consume();
        if token.kind != kind {
            self.diagnostics_bag
                .borrow_mut()
                .report_unexpected_token(&kind, &token);
        }
        token
    }

    pub fn next_statement(&mut self) -> Option<ASTStatement<'de>> {
        if self.is_at_end() {
            return None;
        }
        return Some(self.parse_statement());
    }

    fn is_at_end(&self) -> bool {
        self.current().kind == TokenKind::EOF
    }

    fn parse_statement(&mut self) -> ASTStatement<'de> {
        match self.current().kind {
            TokenKind::Let => self.parse_let_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> ASTStatement<'de> {
        self.consume_and_check(TokenKind::Let);
        let identifier = self.consume_and_check(TokenKind::Identifier);
        self.consume_and_check(TokenKind::Equal);
        let expr = self.parse_expression();
        return ASTStatement::let_statement(identifier, expr);
    }

    fn parse_expression_statement(&mut self) -> ASTStatement<'de> {
        let expr = self.parse_expression();
        return ASTStatement::expression(expr);
    }

    fn parse_expression(&mut self) -> ASTExpression<'de> {
        return self.parse_binary_expression(0);
    }

    fn parse_binary_expression(&mut self, precedence: u8) -> ASTExpression<'de> {
        let mut left = self.parse_unary_expression();

        'outer: while {
            let operator = self.parse_binary_operator();
            if let Some(operator) = operator {
                let operator_precedence = operator.precedence();
                if operator_precedence < precedence {
                    break 'outer;
                }
                self.consume();
                let right = self.parse_binary_expression(operator_precedence);
                left = ASTExpression::binary_expression(left, operator, right);
                true
            } else {
                break 'outer;
            }
        } {}
        left
    }

    fn parse_unary_expression(&mut self) -> ASTExpression<'de> {
        if let Some(operator) = self.parse_unary_operator() {
            self.consume();
            let operand = self.parse_expression();
            return ASTExpression::unary_expression(operator, operand);
        }
        return self.parse_primary_expression();
    }

    fn parse_primary_expression(&mut self) -> ASTExpression<'de> {
        let token = self.consume();
        match token.kind {
            TokenKind::Number(number) => ASTExpression::number_literal(number),
            TokenKind::LParen => {
                let expr = self.parse_expression();
                let _token = self.consume_and_check(TokenKind::RParen);
                ASTExpression::parenthesized_expression(expr)
            }
            TokenKind::Identifier => ASTExpression::identifier(token),
            _ => {
                self.diagnostics_bag
                    .borrow_mut()
                    .report_expected_expression(&token);
                ASTExpression::error(token.span)
            }
        }
    }

    fn parse_unary_operator(&mut self) -> Option<ASTUnaryOperator<'de>> {
        let token = *self.current();
        let kind = match token.kind {
            TokenKind::Minus => Some(ASTUnaryOperatorKind::Minus),
            _ => None,
        };

        return kind.map(|kind| ASTUnaryOperator::new(kind, token));
    }

    fn parse_binary_operator(&mut self) -> Option<ASTBinaryOperator<'de>> {
        let token = *self.current();
        let kind = match token.kind {
            TokenKind::Plus => Some(ASTBinaryOperatorKind::Plus),
            TokenKind::Minus => Some(ASTBinaryOperatorKind::Subtract),
            TokenKind::Star => Some(ASTBinaryOperatorKind::Multiply),
            TokenKind::Slash => Some(ASTBinaryOperatorKind::Divide),
            TokenKind::DoubleStar => Some(ASTBinaryOperatorKind::Power),
            _ => None,
        };

        return kind.map(|kind| ASTBinaryOperator::new(kind, token));
    }
}
