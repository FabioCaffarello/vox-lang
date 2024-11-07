use crate::scopes::GlobalScope;
use crate::visitor::Visitor;
use crate::{
    ast::{
        AssignmentExpr, Ast, BinaryExpr, BinaryOperatorKind, BlockExpr, BooleanExpr, BreakStmt,
        CallExpr, Expression, FunctionDeclaration, IfExpr, LetStmt, NumberExpr, ParenthesizedExpr,
        Statement, UnaryExpr, UnaryOperatorKind, VariableExpr, WhileStmt,
    },
    loops::Loops,
};
use std::collections::HashMap;
use text::span::TextSpan;
use typings::types::{FunctionIdx, ItemID, VariableIdx};

#[derive(Debug)]
pub struct Frame {
    variables: HashMap<VariableIdx, Value>,
}

impl Frame {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    fn insert(&mut self, idx: VariableIdx, value: Value) {
        self.variables.insert(idx, value);
    }

    fn get(&self, idx: &VariableIdx) -> Option<&Value> {
        self.variables.get(idx)
    }
}

#[derive(Debug)]
pub struct Frames {
    frames: Vec<Frame>,
}
impl Frames {
    fn new() -> Self {
        Self {
            frames: vec![Frame::new()],
        }
    }

    fn push(&mut self) {
        self.frames.push(Frame::new());
    }

    fn pop(&mut self) {
        self.frames.pop();
    }

    fn update(&mut self, idx: VariableIdx, value: Value) {
        for frame in self.frames.iter_mut().rev() {
            if frame.get(&idx).is_some() {
                frame.insert(idx, value);
                return;
            }
        }
    }

    fn insert(&mut self, idx: VariableIdx, value: Value) {
        self.frames.last_mut().unwrap().insert(idx, value);
    }

    fn get(&self, idx: &VariableIdx) -> Option<&Value> {
        for frame in self.frames.iter().rev() {
            if let Some(value) = frame.get(idx) {
                return Some(value);
            }
        }
        None
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Number(f64),
    Boolean(bool),
    Function(FunctionIdx),
}

impl Value {
    pub fn expect_boolean(&self) -> bool {
        match self {
            Value::Boolean(value) => *value,
            _ => panic!("Expected boolean value"),
        }
    }

    pub fn expect_number(&self) -> f64 {
        match self {
            Value::Number(value) => *value,
            _ => panic!("Expected number value"),
        }
    }

    pub fn expect_function(&self) -> FunctionIdx {
        match self {
            Value::Function(value) => *value,
            _ => panic!("Expected function value"),
        }
    }
}

#[derive(Debug)]
pub struct Evaluator<'a> {
    pub last_value: Option<Value>,
    pub frames: Frames,
    pub global_scope: &'a GlobalScope,
    should_break: bool,
    loops: Loops,
}

impl<'a> Evaluator<'a> {
    pub fn new(global_scope: &'a GlobalScope) -> Self {
        Self {
            last_value: None,
            frames: Frames::new(),
            global_scope,
            should_break: false,
            loops: Loops::new(),
        }
    }

    fn push_frame(&mut self) {
        self.frames.push();
    }

    fn pop_frame(&mut self) {
        self.frames.pop();
    }

    fn expect_last_value(&self) -> Value {
        *self
            .last_value
            .as_ref()
            .expect("Expected last value to be set")
    }
}

impl<'a, 'de> Visitor<'de> for Evaluator<'a> {
    fn visit_let_statement(
        &mut self,
        ast: &mut Ast<'de>,
        let_statement: &LetStmt<'de>,
        _stmt: &Statement<'de>,
    ) {
        self.visit_expression(ast, &let_statement.initializer);
        self.frames
            .insert(let_statement.variable_idx, self.expect_last_value());
    }

    fn visit_variable_expression(
        &mut self,
        _ast: &mut Ast<'de>,
        variable_expression: &VariableExpr,
        _expr: &Expression<'de>,
    ) {
        let identifier = variable_expression.identifier.span.literal;
        self.last_value = Some(
            *self
                .frames
                .get(&variable_expression.variable_idx)
                .unwrap_or_else(|| {
                    panic!("Variable '{}' not found", identifier);
                }),
        );
    }

    fn visit_number_expression(
        &mut self,
        _ast: &mut Ast<'de>,
        number: &NumberExpr,
        _expr: &Expression<'de>,
    ) {
        self.last_value = Some(Value::Number(number.number));
    }

    fn visit_unary_expression(
        &mut self,
        ast: &mut Ast<'de>,
        unary_expr: &UnaryExpr<'de>,
        _expr: &Expression<'de>,
    ) {
        self.visit_expression(ast, &unary_expr.operand);
        let operand = self.expect_last_value().expect_number();
        self.last_value = Some(Value::Number(match unary_expr.operator.kind {
            UnaryOperatorKind::Minus => -operand,
        }));
    }

    fn visit_binary_expression(
        &mut self,
        ast: &mut Ast<'de>,
        binary_expr: &BinaryExpr<'de>,
        _expr: &Expression<'de>,
    ) {
        self.visit_expression(ast, &binary_expr.left);
        let left = self.expect_last_value();
        self.visit_expression(ast, &binary_expr.right);
        let right = self.expect_last_value();
        self.last_value = Some(match binary_expr.operator.kind {
            BinaryOperatorKind::Plus => Value::Number(left.expect_number() + right.expect_number()),
            BinaryOperatorKind::Subtract => {
                Value::Number(left.expect_number() - right.expect_number())
            }
            BinaryOperatorKind::Multiply => {
                Value::Number(left.expect_number() * right.expect_number())
            }
            BinaryOperatorKind::Divide => {
                Value::Number(left.expect_number() / right.expect_number())
            }
            BinaryOperatorKind::Power => {
                Value::Number(left.expect_number().powf(right.expect_number()))
            }
            BinaryOperatorKind::Equals => Value::Boolean(left == right),
            BinaryOperatorKind::NotEquals => Value::Boolean(left != right),
            BinaryOperatorKind::LessThan => {
                Value::Boolean(left.expect_number() < right.expect_number())
            }
            BinaryOperatorKind::LessThanOrEqual => {
                Value::Boolean(left.expect_number() <= right.expect_number())
            }
            BinaryOperatorKind::GreaterThan => {
                Value::Boolean(left.expect_number() > right.expect_number())
            }
            BinaryOperatorKind::GreaterThanOrEqual => {
                Value::Boolean(left.expect_number() >= right.expect_number())
            }
        });
    }

    fn visit_parenthesized_expression(
        &mut self,
        ast: &mut Ast<'de>,
        parenthesized_expression: &ParenthesizedExpr,
        _expr: &Expression<'de>,
    ) {
        self.visit_expression(ast, &parenthesized_expression.expression);
    }

    fn visit_block_expression(
        &mut self,
        ast: &mut Ast<'de>,
        block_expr: &BlockExpr,
        _expr: &Expression<'de>,
    ) {
        self.push_frame();
        for statement in &block_expr.statements {
            self.visit_statement(ast, *statement);
        }
        self.pop_frame();
    }

    fn visit_if_expression(
        &mut self,
        ast: &mut Ast<'de>,
        if_statement: &IfExpr<'de>,
        _expr: &Expression<'de>,
    ) {
        self.push_frame();
        self.visit_expression(ast, &if_statement.condition);
        if self.expect_last_value().expect_boolean() {
            self.push_frame();
            self.visit_expression(ast, &if_statement.then_branch);
            self.pop_frame();
        } else if let Some(else_branch) = &if_statement.else_branch {
            self.push_frame();
            self.visit_expression(ast, &else_branch.expr);
            self.pop_frame();
        }
        self.pop_frame();
    }

    fn visit_assignment_expression(
        &mut self,
        ast: &mut Ast<'de>,
        assignment_expression: &AssignmentExpr<'de>,
        _expr: &Expression<'de>,
    ) {
        self.visit_expression(ast, &assignment_expression.expression);
        self.frames
            .update(assignment_expression.variable_idx, self.last_value.unwrap());
    }

    fn visit_function_declaration(
        &mut self,
        _ast: &mut Ast<'de>,
        _func_decl: &FunctionDeclaration<'de>,
        _item_id: ItemID,
    ) {
    }

    fn visit_break_statement(&mut self, _ast: &mut Ast<'de>, _break_stmt: &BreakStmt<'de>) {
        self.should_break = true;
    }

    fn visit_while_statement(&mut self, ast: &mut Ast<'de>, while_statement: &WhileStmt<'de>) {
        let label = while_statement
            .label
            .as_ref()
            .map(|token| token.span.literal.to_string());

        if self.loops.push(label.clone()).is_err() {
            panic!("Duplicate loop label '{}'", label.unwrap());
        }
        self.push_frame();
        self.visit_expression(ast, &while_statement.condition);
        while self.expect_last_value().expect_boolean() {
            self.visit_expression(ast, &while_statement.body);
            if self.should_break {
                self.should_break = false;
                self.loops.pop();
                break;
            }
            self.visit_expression(ast, &while_statement.condition);
        }

        self.pop_frame();
    }

    fn visit_call_expression(
        &mut self,
        ast: &mut Ast<'de>,
        call_expression: &CallExpr<'de>,
        _expr: &Expression<'de>,
    ) {
        let function_name = call_expression.function_name();
        let function = self
            .global_scope
            .lookup_function(function_name)
            .map(|f| self.global_scope.functions.get(f))
            .unwrap_or_else(|| panic!("Function '{}' not found", function_name));
        let mut arguments = Vec::new();
        for argument in &call_expression.arguments {
            self.visit_expression(ast, argument);
            arguments.push(self.last_value.unwrap());
        }
        self.push_frame();
        for (argument, param) in arguments.iter().zip(function.parameters.iter()) {
            self.frames.insert(*param, *argument);
        }

        self.visit_expression(ast, &function.body);
        self.pop_frame();
    }

    fn visit_boolean_expression(
        &mut self,
        _ast: &mut Ast<'de>,
        boolean: &BooleanExpr,
        _expr: &Expression<'de>,
    ) {
        self.last_value = Some(Value::Boolean(boolean.value));
    }

    fn visit_error(&mut self, _ast: &mut Ast<'de>, _span: &TextSpan) {
        panic!("Cannot evaluate error expression")
    }
}
