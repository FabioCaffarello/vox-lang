use crate::{printer::Printer, visitor::Visitor};
use index::{Idx, IdxVec};
use lexer::Token;
use text::span::TextSpan;
use typings::types::{ExprID, FunctionIdx, ItemID, StmtID, Type, VariableIdx};

#[derive(Debug, Clone)]
pub struct Ast<'de> {
    pub statements: IdxVec<StmtID, Statement<'de>>,
    pub expressions: IdxVec<ExprID, Expression<'de>>,
    pub items: IdxVec<ItemID, Item<'de>>,
}

impl<'de> Default for Ast<'de> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'de> Ast<'de> {
    pub fn new() -> Self {
        Self {
            statements: IdxVec::new(),
            expressions: IdxVec::new(),
            items: IdxVec::new(),
        }
    }

    pub fn visit(&mut self, visitor: &mut dyn Visitor<'de>) {
        for item in self.items.clone().iter() {
            visitor.visit_item(self, item.id);
        }
    }

    pub fn visualize(&mut self) {
        let mut printer = Printer::new();
        self.visit(&mut printer);
        println!("{}", printer.result);
    }

    pub fn set_variable(&mut self, expr_id: ExprID, variable_idx: VariableIdx) {
        let expr = self.query_expr_mut(expr_id);
        match &mut expr.kind {
            ExprKind::Variable(variable_expr) => {
                variable_expr.variable_idx = variable_idx;
            }
            ExprKind::Assignment(assignment_expr) => {
                assignment_expr.variable_idx = variable_idx;
            }
            _ => unreachable!("Cannot set variable of non-variable expression"),
        }
    }

    pub fn set_variable_for_stmt(&mut self, stmt_id: StmtID, variable_idx: VariableIdx) {
        let stmt = self.query_stmt_mut(stmt_id);
        match &mut stmt.kind {
            StmtKind::Let(let_stmt) => {
                let_stmt.variable_idx = variable_idx;
            }
            _ => unreachable!("Cannot set variable of non-let statement"),
        }
    }

    pub fn set_type(&mut self, expr_id: ExprID, ty: Type) {
        let expr = &mut self.expressions[expr_id];
        expr.ty = ty;
    }

    pub fn query_item(&self, item_id: ItemID) -> &Item<'de> {
        &self.items[item_id]
    }

    pub fn query_expr(&self, expr_id: ExprID) -> &Expression<'de> {
        &self.expressions[expr_id]
    }

    pub fn query_expr_mut(&mut self, expr_id: ExprID) -> &mut Expression<'de> {
        &mut self.expressions[expr_id]
    }

    pub fn query_stmt(&self, stmt_id: StmtID) -> &Statement<'de> {
        &self.statements[stmt_id]
    }

    pub fn query_stmt_mut(&mut self, stmt_id: StmtID) -> &mut Statement<'de> {
        &mut self.statements[stmt_id]
    }

    fn stmt_from_kind(&mut self, kind: StmtKind<'de>) -> &Statement<'de> {
        let stmt = Statement::new(kind, StmtID::new(0));
        let id = self.statements.push(stmt);
        self.statements[id].id = id;
        &self.statements[id]
    }

    pub fn expression_statement(&mut self, expr_id: ExprID) -> &Statement<'de> {
        self.stmt_from_kind(StmtKind::Expr(expr_id))
    }

    pub fn let_statement(
        &mut self,
        identifier: Token<'de>,
        initializer: ExprID,
        type_annotation: Option<StaticTypeAnnotation<'de>>,
    ) -> &Statement<'de> {
        self.stmt_from_kind(StmtKind::Let(LetStmt {
            identifier,
            initializer,
            type_annotation,
            variable_idx: VariableIdx::new(0),
        }))
    }

    pub fn if_expression(
        &mut self,
        if_keyword: Token<'de>,
        condition: ExprID,
        then: ExprID,
        else_statement: Option<ElseBranch<'de>>,
    ) -> &Expression<'de> {
        self.expr_from_kind(ExprKind::If(IfExpr {
            if_keyword,
            condition,
            then_branch: then,
            else_branch: else_statement,
        }))
    }

    pub fn block_expression(
        &mut self,
        left_brace: Token<'de>,
        statements: Vec<StmtID>,
        right_brace: Token<'de>,
    ) -> &Expression<'de> {
        self.expr_from_kind(ExprKind::Block(BlockExpr {
            left_brace,
            statements,
            right_brace,
        }))
    }

    pub fn while_statement(
        &mut self,
        label: Option<Token<'de>>,
        while_keyword: Token<'de>,
        condition: ExprID,
        body: ExprID,
    ) -> &Statement<'de> {
        self.stmt_from_kind(StmtKind::While(WhileStmt {
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
    ) -> &Statement<'de> {
        self.stmt_from_kind(StmtKind::Break(BreakStmt {
            break_keyword,
            label,
        }))
    }

    pub fn return_statement(
        &mut self,
        return_keyword: Token<'de>,
        return_value: Option<ExprID>,
    ) -> &Statement<'de> {
        self.stmt_from_kind(StmtKind::Return(ReturnStmt {
            return_keyword,
            return_value,
        }))
    }

    pub fn func_item(
        &mut self,
        func_keyword: Token<'de>,
        identifier: Token<'de>,
        parameters: Vec<FuncDeclParameter<'de>>,
        body: ExprID,
        return_type: Option<FuncReturnTypeSyntax<'de>>,
        function_idx: FunctionIdx,
    ) -> &Item<'de> {
        self.item_from_kind(ItemKind::Function(Box::new(FunctionDeclaration {
            func_keyword,
            identifier,
            parameters,
            body,
            return_type,
            function_idx,
        })))
    }

    pub fn item_from_kind(&mut self, kind: ItemKind<'de>) -> &Item<'de> {
        let item = Item::new(kind, ItemID::new(0));
        let id = self.items.push(item);
        self.items[id].id = id;
        &self.items[id]
    }

    pub fn expr_from_kind(&mut self, kind: ExprKind<'de>) -> &Expression<'de> {
        let expr = Expression::new(kind, ExprID::new(0), Type::Unresolved);
        let id = self.expressions.push(expr);
        self.expressions[id].id = id;
        &self.expressions[id]
    }

    pub fn number_literal_expression(
        &mut self,
        number: f64,
        token: Token<'de>,
    ) -> &Expression<'de> {
        self.expr_from_kind(ExprKind::Number(NumberExpr { number, token }))
    }

    pub fn binary_expression(
        &mut self,
        left: ExprID,
        operator: BinaryOperator<'de>,
        right: ExprID,
    ) -> &Expression<'de> {
        self.expr_from_kind(ExprKind::Binary(BinaryExpr {
            left,
            operator,
            right,
        }))
    }

    pub fn unary_expression(
        &mut self,
        operator: UnaryOperator<'de>,
        operand: ExprID,
    ) -> &Expression<'de> {
        self.expr_from_kind(ExprKind::Unary(UnaryExpr { operator, operand }))
    }

    pub fn parenthesized_expression(
        &mut self,
        left_paren: Token<'de>,
        expression: ExprID,
        right_paren: Token<'de>,
    ) -> &Expression<'de> {
        self.expr_from_kind(ExprKind::Parenthesized(ParenthesizedExpr {
            expression,
            left_paren,
            right_paren,
        }))
    }

    pub fn identifier_expression(&mut self, identifier: Token<'de>) -> &Expression<'de> {
        self.expr_from_kind(ExprKind::Variable(VariableExpr {
            identifier,
            variable_idx: VariableIdx::new(0),
        }))
    }

    pub fn assignment_expression(
        &mut self,
        identifier: Token<'de>,
        equals: Token<'de>,
        expression: ExprID,
    ) -> &Expression<'de> {
        self.expr_from_kind(ExprKind::Assignment(AssignmentExpr {
            identifier,
            equals,
            expression,
            variable_idx: VariableIdx::new(0),
        }))
    }

    pub fn boolean_expression(&mut self, token: Token<'de>, value: bool) -> &Expression<'de> {
        self.expr_from_kind(ExprKind::Boolean(BooleanExpr { token, value }))
    }

    pub fn call_expression(
        &mut self,
        callee: Token<'de>,
        left_paren: Token<'de>,
        right_paren: Token<'de>,
        arguments: Vec<ExprID>,
    ) -> &Expression<'de> {
        self.expr_from_kind(ExprKind::Call(CallExpr {
            callee,
            left_paren,
            right_paren,
            arguments,
        }))
    }

    pub fn error_expression(&mut self, span: TextSpan<'de>) -> &Expression<'de> {
        self.expr_from_kind(ExprKind::Error(span))
    }
}

#[derive(Debug, Clone)]
pub struct Item<'de> {
    pub kind: ItemKind<'de>,
    pub id: ItemID,
}

impl<'de> Item<'de> {
    pub fn new(kind: ItemKind<'de>, id: ItemID) -> Self {
        Self { kind, id }
    }
}

#[derive(Debug, Clone)]
pub enum ItemKind<'de> {
    Stmt(StmtID),
    Function(Box<FunctionDeclaration<'de>>),
}

#[derive(Debug, Clone)]
pub enum StmtKind<'de> {
    Expr(ExprID),
    Let(LetStmt<'de>),
    While(WhileStmt<'de>),
    Break(BreakStmt<'de>),
    Return(ReturnStmt<'de>),
}

#[derive(Debug, Clone)]
pub struct Statement<'de> {
    pub kind: StmtKind<'de>,
    pub id: StmtID,
}

impl<'de> Statement<'de> {
    pub fn new(kind: StmtKind<'de>, id: StmtID) -> Self {
        Self { kind, id }
    }

    pub fn span(&self, ast: &Ast<'de>) -> TextSpan<'de> {
        match &self.kind {
            StmtKind::Expr(expr_id) => ast.query_expr(*expr_id).span(ast),
            StmtKind::Let(let_stmt) => {
                let mut spans = vec![let_stmt.identifier.span];
                if let Some(type_annotation) = &let_stmt.type_annotation {
                    spans.push(type_annotation.colon.span);
                    spans.push(type_annotation.type_name.span);
                }
                TextSpan::combine(spans)
            }
            StmtKind::While(while_stmt) => {
                let mut spans = vec![while_stmt.while_keyword.span];
                spans.push(ast.query_expr(while_stmt.condition).span(ast));
                spans.push(ast.query_expr(while_stmt.body).span(ast));
                TextSpan::combine(spans)
            }
            StmtKind::Return(return_stmt) => {
                let mut spans = vec![return_stmt.return_keyword.span];
                if let Some(return_value) = &return_stmt.return_value {
                    spans.push(ast.query_expr(*return_value).span(ast));
                }
                TextSpan::combine(spans)
            }
            StmtKind::Break(break_stmt) => {
                let mut spans = vec![break_stmt.break_keyword.span];
                if let Some(label) = &break_stmt.label {
                    spans.push(label.span);
                }
                TextSpan::combine(spans)
            }
        }
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind<'de> {
    Number(NumberExpr<'de>),
    Binary(BinaryExpr<'de>),
    Unary(UnaryExpr<'de>),
    Parenthesized(ParenthesizedExpr<'de>),
    Error(TextSpan<'de>),
    Variable(VariableExpr<'de>),
    Assignment(AssignmentExpr<'de>),
    Boolean(BooleanExpr<'de>),
    Call(CallExpr<'de>),
    If(IfExpr<'de>),
    Block(BlockExpr<'de>),
}

#[derive(Debug, Clone)]
pub struct Expression<'de> {
    pub kind: ExprKind<'de>,
    pub id: ExprID,
    pub ty: Type,
}

impl<'de> Expression<'de> {
    pub fn new(kind: ExprKind<'de>, id: ExprID, ty: Type) -> Self {
        Self { kind, id, ty }
    }

    pub fn span(&self, ast: &Ast<'de>) -> TextSpan<'de> {
        match &self.kind {
            ExprKind::Block(block_expr) => {
                let mut spans = vec![block_expr.left_brace.span];
                for stmt in &block_expr.statements {
                    spans.push(ast.query_stmt(*stmt).span(ast));
                }
                spans.push(block_expr.right_brace.span);
                TextSpan::combine(spans)
            }
            ExprKind::Number(expr) => expr.token.span,
            ExprKind::Binary(expr) => {
                let left_span = ast.query_expr(expr.left).span(ast);
                let operator = expr.operator.token.span;
                let right_span = ast.query_expr(expr.right).span(ast);
                TextSpan::combine(vec![left_span, operator, right_span])
            }
            ExprKind::Unary(expr) => {
                let operator = expr.operator.token.span;
                let operand = ast.query_expr(expr.operand).span(ast);
                TextSpan::combine(vec![operator, operand])
            }
            ExprKind::Parenthesized(expr) => {
                let open_paren = expr.left_paren.span;
                let expression = ast.query_expr(expr.expression).span(ast);
                let close_paren = expr.right_paren.span;
                TextSpan::combine(vec![open_paren, expression, close_paren])
            }
            ExprKind::Variable(expr) => expr.identifier.span,
            ExprKind::Assignment(expr) => {
                let identifier = expr.identifier.span;
                let equals = expr.equals.span;
                let expression = ast.query_expr(expr.expression).span(ast);
                TextSpan::combine(vec![identifier, equals, expression])
            }
            ExprKind::Boolean(expr) => expr.token.span,
            ExprKind::Call(expr) => {
                let callee_span = expr.callee.span;
                let left_paren = expr.left_paren.span;
                let right_paren = expr.right_paren.span;
                let mut spans = vec![callee_span, left_paren, right_paren];
                for arg in &expr.arguments {
                    spans.push(ast.query_expr(*arg).span(ast));
                }
                TextSpan::combine(spans)
            }
            ExprKind::If(expr) => {
                let if_span = expr.if_keyword.span;
                let condition = ast.query_expr(expr.condition).span(ast);
                let then_branch = ast.query_expr(expr.then_branch).span(ast);
                let mut spans = vec![if_span, condition, then_branch];
                if let Some(else_branch) = &expr.else_branch {
                    let else_span = else_branch.else_keyword.span;
                    spans.push(else_span);
                    spans.push(ast.query_expr(else_branch.expr).span(ast));
                }
                TextSpan::combine(spans)
            }
            ExprKind::Error(span) => *span,
        }
    }
}

#[derive(Debug, Clone)]
pub struct BinaryExpr<'de> {
    pub left: ExprID,
    pub operator: BinaryOperator<'de>,
    pub right: ExprID,
}

#[derive(Debug, Clone)]
pub enum BinaryOperatorKind {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOperatorAssociativity {
    Left,
    Right,
}

#[derive(Debug, Clone)]
pub struct BinaryOperator<'de> {
    pub kind: BinaryOperatorKind,
    pub token: Token<'de>,
}

impl<'de> BinaryOperator<'de> {
    pub fn new(kind: BinaryOperatorKind, token: Token<'de>) -> Self {
        BinaryOperator { kind, token }
    }

    pub fn precedence(&self) -> u8 {
        match self.kind {
            BinaryOperatorKind::Power => 30,
            BinaryOperatorKind::Multiply => 20,
            BinaryOperatorKind::Divide => 20,
            BinaryOperatorKind::Plus => 10,
            BinaryOperatorKind::Subtract => 10,
            BinaryOperatorKind::Equals => 50,
            BinaryOperatorKind::NotEquals => 50,
            BinaryOperatorKind::LessThan => 45,
            BinaryOperatorKind::LessThanOrEqual => 45,
            BinaryOperatorKind::GreaterThan => 45,
            BinaryOperatorKind::GreaterThanOrEqual => 45,
        }
    }

    pub fn associativity(&self) -> BinaryOperatorAssociativity {
        match self.kind {
            BinaryOperatorKind::Power => BinaryOperatorAssociativity::Right,
            _ => BinaryOperatorAssociativity::Left,
        }
    }
}

#[derive(Debug, Clone)]
pub struct UnaryExpr<'de> {
    pub operator: UnaryOperator<'de>,
    pub operand: ExprID,
}

#[derive(Debug, Clone)]
pub enum UnaryOperatorKind {
    Minus,
}

#[derive(Debug, Clone)]
pub struct UnaryOperator<'de> {
    pub kind: UnaryOperatorKind,
    pub token: Token<'de>,
}

impl<'de> UnaryOperator<'de> {
    pub fn new(kind: UnaryOperatorKind, token: Token<'de>) -> Self {
        UnaryOperator { kind, token }
    }
}

#[derive(Debug, Clone)]
pub struct LetStmt<'de> {
    pub identifier: Token<'de>,
    pub initializer: ExprID,
    pub type_annotation: Option<StaticTypeAnnotation<'de>>,
    pub variable_idx: VariableIdx,
}

#[derive(Debug, Clone)]
pub struct VariableExpr<'de> {
    pub identifier: Token<'de>,
    pub variable_idx: VariableIdx,
}

impl<'de> VariableExpr<'de> {
    pub fn identifier(&self) -> &str {
        self.identifier.span.literal
    }
}

#[derive(Debug, Clone)]
pub struct IfExpr<'de> {
    pub if_keyword: Token<'de>,
    pub condition: ExprID,
    pub then_branch: ExprID,
    pub else_branch: Option<ElseBranch<'de>>,
}

#[derive(Debug, Clone)]
pub struct ElseBranch<'de> {
    pub else_keyword: Token<'de>,
    pub expr: ExprID,
}

impl<'de> ElseBranch<'de> {
    pub fn new(else_keyword: Token<'de>, expr: ExprID) -> Self {
        ElseBranch { else_keyword, expr }
    }
}

#[derive(Debug, Clone)]
pub struct AssignmentExpr<'de> {
    pub equals: Token<'de>,
    pub identifier: Token<'de>,
    pub expression: ExprID,
    pub variable_idx: VariableIdx,
}

#[derive(Debug, Clone)]
pub struct BlockExpr<'de> {
    pub left_brace: Token<'de>,
    pub statements: Vec<StmtID>,
    pub right_brace: Token<'de>,
}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration<'de> {
    pub func_keyword: Token<'de>,
    pub identifier: Token<'de>,
    pub parameters: Vec<FuncDeclParameter<'de>>,
    pub body: ExprID,
    pub return_type: Option<FuncReturnTypeSyntax<'de>>,
    pub function_idx: FunctionIdx,
}

#[derive(Debug, Clone)]
pub struct FuncReturnTypeSyntax<'de> {
    pub arrow: Token<'de>,
    pub type_name: Token<'de>,
}

impl<'de> FuncReturnTypeSyntax<'de> {
    pub fn new(arrow: Token<'de>, type_name: Token<'de>) -> Self {
        FuncReturnTypeSyntax { arrow, type_name }
    }
}

#[derive(Debug, Clone)]
pub struct ReturnStmt<'de> {
    pub return_keyword: Token<'de>,
    pub return_value: Option<ExprID>,
}

#[derive(Debug, Clone)]
pub struct StaticTypeAnnotation<'de> {
    pub colon: Token<'de>,
    pub type_name: Token<'de>,
}

impl<'de> StaticTypeAnnotation<'de> {
    pub fn new(colon: Token<'de>, type_name: Token<'de>) -> Self {
        StaticTypeAnnotation { colon, type_name }
    }
}

#[derive(Debug, Clone)]
pub struct FuncDeclParameter<'de> {
    pub identifier: Token<'de>,
    pub type_annotation: StaticTypeAnnotation<'de>,
}

#[derive(Debug, Clone)]
pub struct WhileStmt<'de> {
    pub label: Option<Token<'de>>,
    pub while_keyword: Token<'de>,
    pub condition: ExprID,
    pub body: ExprID,
}

#[derive(Debug, Clone)]
pub struct BreakStmt<'de> {
    pub break_keyword: Token<'de>,
    pub label: Option<Token<'de>>,
}

#[derive(Debug, Clone)]
pub struct CallExpr<'de> {
    pub callee: Token<'de>,
    pub left_paren: Token<'de>,
    pub right_paren: Token<'de>,
    pub arguments: Vec<ExprID>,
}

impl<'de> CallExpr<'de> {
    pub fn function_name(&self) -> &str {
        self.callee.span.literal
    }
}

#[derive(Debug, Clone)]
pub struct BooleanExpr<'de> {
    pub value: bool,
    pub token: Token<'de>,
}

#[derive(Debug, Clone)]
pub struct NumberExpr<'de> {
    pub token: Token<'de>,
    pub number: f64,
}

#[derive(Debug, Clone)]
pub struct ParenthesizedExpr<'de> {
    pub expression: ExprID,
    pub left_paren: Token<'de>,
    pub right_paren: Token<'de>,
}
