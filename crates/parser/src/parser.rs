use crate::errors::ParserError;
use ast::{ast::ASTBinaryOperator, ASTBinaryOperatorKind, ASTExpression, ASTStatement};
use lexer::{Lexer, Token, TokenKind};
use miette::MietteError;

pub struct Parser<'de> {
    pub tokens: Vec<Token<'de>>,
    current: usize,
}

impl<'de> Parser<'de> {
    pub fn new(tokens: Vec<Token<'de>>) -> Self {
        Self {
            tokens: tokens
                .iter()
                .filter(|token| {
                    token.kind != TokenKind::LineComment && token.kind != TokenKind::BlockComment
                })
                .map(|token| token.clone())
                .collect(),
            current: 0,
        }
    }

    pub fn from_input(input: &'de str) -> Result<Self, ParserError> {
        let mut lexer = Lexer::new(input);
        let (tokens, errors) = lexer.collect_tokens();

        if !errors.is_empty() {
            let errors: Vec<MietteError> = errors
                .into_iter()
                .map(|e| MietteError::from(std::io::Error::new(std::io::ErrorKind::Other, e)))
                .collect();
            return Err(ParserError::LexerErrors { errors });
        }

        Ok(Self::new(tokens))
    }

    fn peek(&self, offset: isize) -> Option<&Token<'de>> {
        self.tokens.get((self.current as isize + offset) as usize)
    }

    fn current(&self) -> Option<&Token<'de>> {
        self.peek(0)
    }

    fn consume(&mut self) -> Option<Token<'de>> {
        self.current += 1;
        let token = self.peek(-1)?.clone();
        Some(token)
    }

    pub fn next_statement(&mut self) -> Option<ASTStatement<'de>> {
        return self.parse_statement();
    }

    fn parse_statement(&mut self) -> Option<ASTStatement<'de>> {
        let token = self.current()?;
        if token.kind == TokenKind::EOF {
            return None;
        }
        let expr = self.parse_expression()?;
        return Some(ASTStatement::expression(expr));
    }

    fn parse_expression(&mut self) -> Option<ASTExpression<'de>> {
        return self.parse_binary_expression(0);
    }

    fn parse_binary_expression(&mut self, precedence: u8) -> Option<ASTExpression<'de>> {
        let mut left = self.parse_primary_expression()?;

        'outer: while {
            let operator = self.parse_binary_operator();
            if let Some(operator) = operator {
                let operator_precedence = operator.precedence();
                if operator_precedence < precedence {
                    break 'outer;
                }
                let right = self.parse_binary_expression(operator_precedence)?;
                left = ASTExpression::binary_expression(left, operator, right);
                true
            } else {
                break 'outer;
            }
        } {}

        return Some(left);
    }

    fn parse_primary_expression(&mut self) -> Option<ASTExpression<'de>> {
        let token = self.consume()?;
        match token.kind {
            TokenKind::Number(number) => Some(ASTExpression::number_literal(number)),
            _ => None,
        }
    }

    fn parse_binary_operator(&mut self) -> Option<ASTBinaryOperator<'de>> {
        let token = self.consume()?;
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
