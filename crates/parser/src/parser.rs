use crate::errors::ParserError;
use ast::ast::{
    Ast, BinaryOperator, BinaryOperatorAssociativity, BinaryOperatorKind, ElseBranch, ExprID,
    Expression, FuncDeclParameter, FuncReturnTypeSyntax, Item, ItemKind, Statement,
    StaticTypeAnnotation, UnaryOperator, UnaryOperatorKind,
};
use diagnostics::diagnostics::DiagnosticsBagCell;
use lexer::{Lexer, Token, TokenKind};
use miette::MietteError;
use support::counter::Counter;

pub struct Parser<'a, 'de> {
    ast: &'a mut Ast<'de>,
    pub tokens: Vec<Token<'de>>,
    current: Counter,
    pub diagnostics_bag: DiagnosticsBagCell<'de>,
}

impl<'a, 'de> Parser<'a, 'de> {
    pub fn new(
        tokens: Vec<Token<'de>>,
        diagnostics_bag: DiagnosticsBagCell<'de>,
        ast: &'a mut Ast<'de>,
    ) -> Self {
        Self {
            ast,
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

    pub fn parse(&mut self) {
        while self.next_item().map(|stmt| stmt.id).is_some() {}
    }

    pub fn from_input(
        input: &'de str,
        diagnostics_bag: DiagnosticsBagCell<'de>,
        ast: &'a mut Ast<'de>,
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

        Ok(Self::new(tokens, diagnostics_bag, ast))
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

    fn next_item(&mut self) -> Option<&Item<'de>> {
        if self.is_at_end() {
            return None;
        }
        return Some(self.parse_item());
    }

    fn is_at_end(&self) -> bool {
        self.current().kind == TokenKind::EOF
    }

    fn parse_item(&mut self) -> &Item<'de> {
        match self.current().kind {
            TokenKind::Fun => self.parse_function_declaration(),
            _ => {
                let stmt = self.parse_statement();
                let stmt_id = stmt.id;
                self.ast.item_from_kind(ItemKind::Stmt(stmt_id))
            }
        }
    }

    fn parse_statement(&mut self) -> &Statement<'de> {
        if self.current().kind == TokenKind::Identifier && self.peek(1).kind == TokenKind::Colon {
            let label = self.consume_and_check(TokenKind::Identifier);
            self.consume_and_check(TokenKind::Colon);
            return self.parse_labeled_statement(label);
        }
        self.parse_statement_without_label()
    }

    fn parse_labeled_statement(&mut self, label: Token<'de>) -> &Statement<'de> {
        match self.current().kind {
            TokenKind::While => {
                let while_stmt = self.parse_while_statement(Some(label));
                while_stmt
            }
            _ => {
                self.diagnostics_bag
                    .borrow_mut()
                    .report_unexpected_label(&label);
                return self.parse_statement_without_label();
            }
        }
    }

    fn parse_statement_without_label(&mut self) -> &Statement<'de> {
        match self.current().kind {
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::While => self.parse_while_statement(None),
            TokenKind::Return => self.parse_return_statement(),
            TokenKind::Break => self.parse_break_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_let_statement(&mut self) -> &Statement<'de> {
        self.consume_and_check(TokenKind::Let);
        let identifier = self.consume_and_check(TokenKind::Identifier);
        let optional_type_annotation = self.parse_optional_type_annotation();
        self.consume_and_check(TokenKind::Equal);
        let expr = self.parse_expression().id;
        self.ast
            .let_statement(identifier, expr, optional_type_annotation)
    }

    fn parse_expression_statement(&mut self) -> &Statement<'de> {
        let expr = self.parse_expression().id;
        self.ast.expression_statement(expr)
    }

    fn parse_expression(&mut self) -> &Expression<'de> {
        self.parse_assignment_expression()
    }

    fn parse_assignment_expression(&mut self) -> &Expression<'de> {
        if self.current().kind == TokenKind::Identifier && self.peek(1).kind == TokenKind::Equal {
            let identifier = self.consume_and_check(TokenKind::Identifier);
            let equals = self.consume_and_check(TokenKind::Equal);
            let expr = self.parse_expression().id;
            return self.ast.assignment_expression(identifier, equals, expr);
        }
        return self.parse_binary_expression(0);
    }

    fn parse_binary_expression(&mut self, precedence: u8) -> &Expression<'de> {
        let left = self.parse_unary_expression().id;
        self.parse_binary_expression_recurse(left, precedence)
    }

    fn parse_binary_expression_recurse(
        &mut self,
        mut left: ExprID,
        precedence: u8,
    ) -> &Expression<'de> {
        while let Some(operator) = self.parse_binary_operator() {
            let operator_precedence = operator.precedence();
            if operator_precedence < precedence {
                break;
            }
            self.consume();
            let mut right = self.parse_unary_expression().id;

            while let Some(inner_operator) = self.parse_binary_operator() {
                let greater_precedence = inner_operator.precedence() > operator.precedence();
                let equal_precedence = inner_operator.precedence() == operator.precedence();
                if !(greater_precedence
                    || equal_precedence
                        && inner_operator.associativity() == BinaryOperatorAssociativity::Right)
                {
                    break;
                }

                right = self
                    .parse_binary_expression_recurse(
                        right,
                        std::cmp::max(operator.precedence(), inner_operator.precedence()),
                    )
                    .id;
            }
            left = self.ast.binary_expression(left, operator, right).id;
        }
        self.ast.query_expr(left)
    }

    fn parse_unary_expression(&mut self) -> &Expression<'de> {
        if let Some(operator) = self.parse_unary_operator() {
            self.consume();
            let operand = self.parse_primary_expression().id;
            return self.ast.unary_expression(operator, operand);
        }
        return self.parse_primary_expression();
    }

    fn parse_primary_expression(&mut self) -> &Expression<'de> {
        let token = self.consume();
        match token.kind {
            TokenKind::LBrace => self.parse_block_expression(token),
            TokenKind::If => self.parse_if_expression(token),
            TokenKind::Number(number) => self.ast.number_literal_expression(number, token),
            TokenKind::LParen => {
                let expr = self.parse_expression().id;
                let left_paren = token;
                let right_paren = self.consume_and_check(TokenKind::RParen);
                self.ast
                    .parenthesized_expression(left_paren, expr, right_paren)
            }
            TokenKind::Identifier => {
                if self.current().kind == TokenKind::LParen {
                    self.parse_call_expression(token)
                } else {
                    self.ast.identifier_expression(token)
                }
            }
            TokenKind::True | TokenKind::False => {
                let value = token.kind == TokenKind::True;
                self.ast.boolean_expression(token, value)
            }
            _ => {
                self.diagnostics_bag
                    .borrow_mut()
                    .report_expected_expression(&token);
                self.ast.error_expression(token.span)
            }
        }
    }

    fn parse_unary_operator(&mut self) -> Option<UnaryOperator<'de>> {
        let token = *self.current();
        let kind = match token.kind {
            TokenKind::Minus => Some(UnaryOperatorKind::Minus),
            _ => None,
        };

        return kind.map(|kind| UnaryOperator::new(kind, token));
    }

    fn parse_binary_operator(&mut self) -> Option<BinaryOperator<'de>> {
        let token = *self.current();
        let kind = match token.kind {
            TokenKind::Plus => Some(BinaryOperatorKind::Plus),
            TokenKind::Minus => Some(BinaryOperatorKind::Subtract),
            TokenKind::Star => Some(BinaryOperatorKind::Multiply),
            TokenKind::Slash => Some(BinaryOperatorKind::Divide),
            TokenKind::DoubleStar => Some(BinaryOperatorKind::Power),
            TokenKind::EqualEqual => Some(BinaryOperatorKind::Equals),
            TokenKind::BangEqual => Some(BinaryOperatorKind::NotEquals),
            TokenKind::Less => Some(BinaryOperatorKind::LessThan),
            TokenKind::LessEqual => Some(BinaryOperatorKind::LessThanOrEqual),
            TokenKind::Greater => Some(BinaryOperatorKind::GreaterThan),
            TokenKind::GreaterEqual => Some(BinaryOperatorKind::GreaterThanOrEqual),
            _ => None,
        };

        return kind.map(|kind| BinaryOperator::new(kind, token));
    }

    fn parse_block_expression(&mut self, left_brace: Token<'de>) -> &Expression<'de> {
        let mut statements = Vec::new();
        while self.current().kind != TokenKind::RBrace && !self.is_at_end() {
            statements.push(self.parse_statement().id);
        }
        let right_brace = self.consume_and_check(TokenKind::RBrace);
        self.ast
            .block_expression(left_brace, statements, right_brace)
    }

    fn parse_if_expression(&mut self, if_keyword: Token<'de>) -> &Expression<'de> {
        let condition_expr = self.parse_expression().id;
        let then = self.parse_expression().id;
        let else_statement = self.parse_optional_else_statement();
        self.ast
            .if_expression(if_keyword, condition_expr, then, else_statement)
    }

    fn parse_optional_else_statement(&mut self) -> Option<ElseBranch<'de>> {
        if self.current().kind == TokenKind::Else {
            let else_keyword = self.consume_and_check(TokenKind::Else);
            let else_expr = self.parse_expression().id;
            return Some(ElseBranch::new(else_keyword, else_expr));
        }
        None
    }

    fn parse_function_declaration(&mut self) -> &Item<'de> {
        self.consume_and_check(TokenKind::Fun);
        let identifier = self.consume_and_check(TokenKind::Identifier);
        let parameters = self.parse_optional_parameter_list();
        let return_type = self.parse_optional_return_type_annotation();
        let body = self.parse_statement().id;
        self.ast
            .function_declaration(identifier, parameters, body, return_type)
    }

    fn parse_optional_parameter_list(&mut self) -> Vec<FuncDeclParameter<'de>> {
        if self.current().kind != TokenKind::LParen {
            return Vec::new();
        }
        self.consume_and_check(TokenKind::LParen);
        let mut parameters = Vec::new();
        while self.current().kind != TokenKind::RParen && !self.is_at_end() {
            parameters.push(FuncDeclParameter {
                identifier: self.consume_and_check(TokenKind::Identifier),
                type_annotation: self.parse_type_annotation(),
            });
            if self.current().kind == TokenKind::Comma {
                self.consume_and_check(TokenKind::Comma);
            }
        }
        self.consume_and_check(TokenKind::RParen);
        parameters
    }

    fn parse_return_statement(&mut self) -> &Statement<'de> {
        let return_keyword = self.consume_and_check(TokenKind::Return);
        // TODO: allow empty return statements
        let expression = self.parse_expression().id;
        self.ast.return_statement(return_keyword, Some(expression))
    }

    fn parse_while_statement(&mut self, label: Option<Token<'de>>) -> &Statement<'de> {
        let while_keyword = self.consume_and_check(TokenKind::While);
        let condition_expr = self.parse_expression().id;
        let body = self.parse_expression().id;
        self.ast
            .while_statement(label, while_keyword, condition_expr, body)
    }

    fn parse_break_statement(&mut self) -> &Statement<'de> {
        let break_keyword = self.consume_and_check(TokenKind::Break);
        let label = if self.current().kind == TokenKind::Identifier {
            Some(self.consume_and_check(TokenKind::Identifier))
        } else {
            None
        };
        self.ast.break_statement(break_keyword, label)
    }

    fn parse_call_expression(&mut self, identifier: Token<'de>) -> &Expression<'de> {
        let left_paren = self.consume_and_check(TokenKind::LParen);
        let mut arguments = Vec::new();
        while self.current().kind != TokenKind::RParen && !self.is_at_end() {
            arguments.push(self.parse_expression().id);
            if self.current().kind != TokenKind::RParen {
                self.consume_and_check(TokenKind::Comma);
            }
        }
        let right_paren = self.consume_and_check(TokenKind::RParen);
        self.ast
            .call_expression(identifier, left_paren, right_paren, arguments)
    }

    fn parse_optional_type_annotation(&mut self) -> Option<StaticTypeAnnotation<'de>> {
        if self.current().kind == TokenKind::Colon {
            return Some(self.parse_type_annotation());
        }
        None
    }

    fn parse_type_annotation(&mut self) -> StaticTypeAnnotation<'de> {
        let colon = self.consume_and_check(TokenKind::Colon);
        let type_name = self.consume_and_check(TokenKind::Identifier);
        return StaticTypeAnnotation::new(colon, type_name);
    }

    fn parse_optional_return_type_annotation(&mut self) -> Option<FuncReturnTypeSyntax<'de>> {
        if self.current().kind == TokenKind::Arrow {
            let arrow = self.consume_and_check(TokenKind::Arrow);
            let type_name = self.consume_and_check(TokenKind::Identifier);
            return Some(FuncReturnTypeSyntax::new(arrow, type_name));
        }
        None
    }
}
