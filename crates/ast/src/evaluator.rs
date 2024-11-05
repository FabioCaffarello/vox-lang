use std::collections::HashMap;

use crate::scopes::{GlobalScope, VariableIdx};
use crate::visitor::Visitor;
use crate::{
    ast::{
        AssignmentExpr, Ast, BinaryExpr, BinaryOperatorKind, BlockExpr, BooleanExpr, BreakStmt,
        CallExpr, Expression, FunctionDeclaration, IfExpr, LetStmt, NumberExpr, ParenthesizedExpr,
        Statement, UnaryExpr, UnaryOperatorKind, VariableExpr, WhileStmt,
    },
    loops::Loops,
};
use text::span::TextSpan;

pub struct Frame {
    variables: HashMap<VariableIdx, f64>,
}

impl Frame {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }

    fn insert(&mut self, idx: VariableIdx, value: f64) {
        self.variables.insert(idx, value);
    }

    fn get(&self, idx: &VariableIdx) -> Option<&f64> {
        self.variables.get(idx)
    }
}
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

    fn update(&mut self, idx: VariableIdx, value: f64) {
        for frame in self.frames.iter_mut().rev() {
            if frame.variables.contains_key(&idx) {
                frame.insert(idx, value);
                return;
            }
        }
    }

    fn insert(&mut self, idx: VariableIdx, value: f64) {
        self.frames.last_mut().unwrap().insert(idx, value);
    }

    fn get(&self, idx: &VariableIdx) -> Option<&f64> {
        for frame in self.frames.iter().rev() {
            if let Some(value) = frame.get(idx) {
                return Some(value);
            }
        }
        None
    }
}

pub struct Evaluator<'a> {
    pub last_value: Option<f64>,
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

    fn eval_boolean_instruction<F>(&self, instruction: F) -> f64
    where
        F: FnOnce() -> bool,
    {
        let result = instruction();
        if result {
            1_f64
        } else {
            0_f64
        }
    }

    fn push_frame(&mut self) {
        self.frames.push();
    }

    fn pop_frame(&mut self) {
        self.frames.pop();
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
            .insert(let_statement.variable_idx, self.last_value.unwrap());
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
                .unwrap_or_else(|| panic!("Variable {} not found", identifier)),
        );
    }

    fn visit_number_expression(
        &mut self,
        _ast: &mut Ast<'de>,
        number: &NumberExpr,
        _expr: &Expression<'de>,
    ) {
        self.last_value = Some(number.number);
    }

    fn visit_unary_expression(
        &mut self,
        ast: &mut Ast<'de>,
        unary_expr: &UnaryExpr<'de>,
        _expr: &Expression<'de>,
    ) {
        self.visit_expression(ast, &unary_expr.operand);
        let operand = self.last_value.unwrap();
        self.last_value = Some(match unary_expr.operator.kind {
            UnaryOperatorKind::Minus => -operand,
        });
    }

    fn visit_binary_expression(
        &mut self,
        ast: &mut Ast<'de>,
        binary_expr: &BinaryExpr<'de>,
        _expr: &Expression<'de>,
    ) {
        self.visit_expression(ast, &binary_expr.left);
        let left = self.last_value.unwrap();
        self.visit_expression(ast, &binary_expr.right);
        let right = self.last_value.unwrap();
        self.last_value = Some(match binary_expr.operator.kind {
            BinaryOperatorKind::Plus => left + right,
            BinaryOperatorKind::Subtract => left - right,
            BinaryOperatorKind::Multiply => left * right,
            BinaryOperatorKind::Divide => left / right,
            BinaryOperatorKind::Power => left.powf(right),
            BinaryOperatorKind::Equals => {
                if left == right {
                    1_f64
                } else {
                    0_f64
                }
            }
            BinaryOperatorKind::NotEquals => self.eval_boolean_instruction(|| left != right),
            BinaryOperatorKind::LessThan => self.eval_boolean_instruction(|| left < right),
            BinaryOperatorKind::LessThanOrEqual => self.eval_boolean_instruction(|| left <= right),
            BinaryOperatorKind::GreaterThan => self.eval_boolean_instruction(|| left > right),
            BinaryOperatorKind::GreaterThanOrEqual => {
                self.eval_boolean_instruction(|| left >= right)
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
        if self.last_value.unwrap() != 0 as f64 {
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
        _func_decl: &FunctionDeclaration,
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

        while self.last_value.unwrap() != 0_f64 {
            self.visit_expression(ast, &while_statement.body);
            self.visit_expression(ast, &while_statement.condition);
            if self.should_break {
                self.should_break = false;
                self.loops.pop();
                break;
            }
        }

        self.pop_frame();
    }

    fn visit_call_expression(
        &mut self,
        ast: &mut Ast<'de>,
        call_expression: &CallExpr<'de>,
        _expr: &Expression<'de>,
    ) {
        let function_idx = self
            .global_scope
            .lookup_function(call_expression.identifier.span.literal)
            .unwrap_or_else(|| {
                panic!(
                    "Function {} not found",
                    call_expression.identifier.span.literal
                )
            });
        let function = self.global_scope.functions.get(function_idx);
        let mut arguments = Vec::new();
        for argument in &call_expression.arguments {
            self.visit_expression(ast, argument);
            arguments.push(self.last_value.unwrap());
        }
        self.push_frame();
        for (argument, param) in arguments.iter().zip(function.parameters.iter()) {
            self.frames.insert(*param, *argument);
        }

        self.visit_statement(ast, function.body);
        self.pop_frame();
    }

    fn visit_boolean_expression(
        &mut self,
        _ast: &mut Ast<'de>,
        boolean: &BooleanExpr,
        _expr: &Expression<'de>,
    ) {
        self.last_value = Some(boolean.value as i64 as f64);
    }

    fn visit_error(&mut self, _ast: &mut Ast<'de>, _span: &TextSpan) {
        panic!("Cannot evaluate error expression")
    }
}
