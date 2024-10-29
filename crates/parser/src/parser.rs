use core::panic;
use std::cell::Cell;

use crate::errors::ParserError;
use ast::{ast::ASTBinaryOperator, ASTBinaryOperatorKind, ASTExpression, ASTStatement};
use diagnostics::diagnostics::DiagnosticsBagCell;
use lexer::{Lexer, Token, TokenKind};
use miette::MietteError;

pub struct Counter {
    value: Cell<usize>,
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
                .map(|token| token.clone())
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
        self.peek(-1).clone()
    }

    fn consume_and_check(&self, kind: TokenKind) -> Token<'de> {
        // FIXME: &Token<'de>
        let token = self.consume();
        if token.kind != kind {
            self.diagnostics_bag
                .borrow_mut()
                .report_unexpected_token(&kind, &token);
        }
        return token;
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
        let expr = self.parse_expression();
        return ASTStatement::expression(expr);
    }

    fn parse_expression(&mut self) -> ASTExpression<'de> {
        return self.parse_binary_expression(0);
    }

    fn parse_binary_expression(&mut self, precedence: u8) -> ASTExpression<'de> {
        let mut left = self.parse_primary_expression();

        'outer: while {
            let operator = self.parse_binary_operator();
            if let Some(operator) = operator {
                self.consume();
                let operator_precedence = operator.precedence();
                if operator_precedence < precedence {
                    break 'outer;
                }
                let right = self.parse_binary_expression(operator_precedence);
                left = ASTExpression::binary_expression(left, operator, right);
                true
            } else {
                break 'outer;
            }
        } {}

        return left;
    }

    fn parse_primary_expression(&mut self) -> ASTExpression<'de> {
        let token = self.consume();
        match token.kind {
            TokenKind::Number(number) => ASTExpression::number_literal(number),
            TokenKind::LParen => {
                let expr = self.parse_expression();
                let token = self.consume_and_check(TokenKind::RParen);
                ASTExpression::parenthesized_expression(expr)
            }
            _ => {
                self.diagnostics_bag
                    .borrow_mut()
                    .report_unexpected_expression(&token);
                ASTExpression::error(token.span.clone())
            }
        }
    }

    fn parse_binary_operator(&mut self) -> Option<ASTBinaryOperator<'de>> {
        let token = self.current().clone();
        let kind = match token.kind {
            TokenKind::Plus => Some(ASTBinaryOperatorKind::Plus),
            TokenKind::Minus => Some(ASTBinaryOperatorKind::Subtract),
            TokenKind::Star => Some(ASTBinaryOperatorKind::Multiply),
            TokenKind::Slash => Some(ASTBinaryOperatorKind::Divide),
            _ => None,
        };

        return kind.map(|kind| ASTBinaryOperator::new(kind, token));
    }
}
