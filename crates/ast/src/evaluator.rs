use std::collections::HashMap;

use crate::scopes::GlobalScope;
use crate::visitor::ASTVisitor;
use crate::{
    ast::{
        ASTAssignmentExpression, ASTBinaryExpression, ASTBinaryOperatorKind, ASTBlockStatement,
        ASTBooleanExpression, ASTBreakStatement, ASTCallExpression, ASTExpression,
        ASTFuncDeclStatement, ASTIfStatement, ASTLetStatement, ASTNumberExpression,
        ASTParenthesizedExpression, ASTUnaryExpression, ASTUnaryOperatorKind,
        ASTVariableExpression, ASTWhileStatement, Ast,
    },
    loops::Loops,
};
use text::span::TextSpan;

pub struct Frame {
    variables: HashMap<String, f64>,
}

impl Frame {
    fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }
    fn insert(&mut self, identifier: String, value: f64) {
        self.variables.insert(identifier, value);
    }
    fn get(&self, identifier: &String) -> Option<&f64> {
        self.variables.get(identifier)
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

    fn update(&mut self, identifier: String, value: f64) {
        for frame in self.frames.iter_mut().rev() {
            if frame.variables.contains_key(&identifier) {
                frame.insert(identifier, value);
                return;
            }
        }
        panic!("Variable {} not found", identifier)
    }

    fn insert(&mut self, identifier: String, value: f64) {
        self.frames.last_mut().unwrap().insert(identifier, value);
    }

    fn get(&self, identifier: &String) -> Option<&f64> {
        for frame in self.frames.iter().rev() {
            if let Some(value) = frame.get(identifier) {
                return Some(value);
            }
        }
        None
    }
}

pub struct ASTEvaluator<'a, 'de> {
    pub last_value: Option<f64>,
    pub frames: Frames,
    pub global_scope: &'de GlobalScope,
    ast: &'a Ast<'de>,
    should_break: bool,
    loops: Loops,
}

impl<'a, 'de> ASTEvaluator<'a, 'de> {
    pub fn new(global_scope: &'de GlobalScope, ast: &'a Ast<'de>) -> Self {
        Self {
            ast,
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

impl<'a, 'de> ASTVisitor<'de> for ASTEvaluator<'a, 'de> {
    fn get_ast(&self) -> &Ast<'de> {
        self.ast
    }

    fn visit_let_statement(&mut self, let_statement: &ASTLetStatement<'de>) {
        self.visit_expression(&let_statement.initializer);
        self.frames.insert(
            let_statement.identifier.span.literal.to_string(),
            self.last_value.unwrap(),
        );
    }

    fn visit_variable_expression(
        &mut self,
        variable_expression: &ASTVariableExpression,
        _expr: &ASTExpression<'de>,
    ) {
        let identifier = variable_expression.identifier.span.literal;
        self.last_value = Some(
            *self
                .frames
                .get(&identifier.to_string())
                .unwrap_or_else(|| panic!("Variable {} not found", identifier)),
        );
    }

    fn visit_number_expression(
        &mut self,
        number: &ASTNumberExpression,
        _expr: &ASTExpression<'de>,
    ) {
        self.last_value = Some(number.number);
    }

    fn visit_unary_expression(
        &mut self,
        unary_expr: &ASTUnaryExpression<'de>,
        _expr: &ASTExpression<'de>,
    ) {
        self.visit_expression(&unary_expr.operand);
        let operand = self.last_value.unwrap();
        self.last_value = Some(match unary_expr.operator.kind {
            ASTUnaryOperatorKind::Minus => -operand,
        });
    }

    fn visit_binary_expression(
        &mut self,
        binary_expr: &ASTBinaryExpression<'de>,
        _expr: &ASTExpression<'de>,
    ) {
        self.visit_expression(&binary_expr.left);
        let left = self.last_value.unwrap();
        self.visit_expression(&binary_expr.right);
        let right = self.last_value.unwrap();
        self.last_value = Some(match binary_expr.operator.kind {
            ASTBinaryOperatorKind::Plus => left + right,
            ASTBinaryOperatorKind::Subtract => left - right,
            ASTBinaryOperatorKind::Multiply => left * right,
            ASTBinaryOperatorKind::Divide => left / right,
            ASTBinaryOperatorKind::Power => left.powf(right),
            ASTBinaryOperatorKind::Equals => {
                if left == right {
                    1_f64
                } else {
                    0_f64
                }
            }
            ASTBinaryOperatorKind::NotEquals => self.eval_boolean_instruction(|| left != right),
            ASTBinaryOperatorKind::LessThan => self.eval_boolean_instruction(|| left < right),
            ASTBinaryOperatorKind::LessThanOrEqual => {
                self.eval_boolean_instruction(|| left <= right)
            }
            ASTBinaryOperatorKind::GreaterThan => self.eval_boolean_instruction(|| left > right),
            ASTBinaryOperatorKind::GreaterThanOrEqual => {
                self.eval_boolean_instruction(|| left >= right)
            }
        });
    }

    fn visit_parenthesized_expression(
        &mut self,
        parenthesized_expression: &ASTParenthesizedExpression,
        _expr: &ASTExpression<'de>,
    ) {
        self.visit_expression(&parenthesized_expression.expression);
    }

    fn visit_block_statement(&mut self, block_statement: &ASTBlockStatement) {
        self.push_frame();
        for statement in &block_statement.statements {
            self.visit_statement(statement);
        }
        self.pop_frame();
    }

    fn visit_if_statement(&mut self, if_statement: &ASTIfStatement<'de>) {
        self.push_frame();
        self.visit_expression(&if_statement.condition);
        if self.last_value.unwrap() != 0 as f64 {
            self.push_frame();
            self.visit_statement(&if_statement.then_branch);
            self.pop_frame();
        } else if let Some(else_branch) = &if_statement.else_branch {
            self.push_frame();
            self.visit_statement(&else_branch.else_statement);
            self.pop_frame();
        }
        self.pop_frame();
    }

    fn visit_assignment_expression(
        &mut self,
        assignment_expression: &ASTAssignmentExpression<'de>,
        _expr: &ASTExpression<'de>,
    ) {
        let identifier = &assignment_expression.identifier.span.literal;
        self.visit_expression(&assignment_expression.expression);
        self.frames
            .update(identifier.to_string(), self.last_value.unwrap());
    }

    fn visit_func_decl_statement(&mut self, _func_decl_statement: &ASTFuncDeclStatement) {}

    fn visit_break_statement(&mut self, _break_stmt: &ASTBreakStatement<'de>) {
        self.should_break = true;
    }

    fn visit_while_statement(&mut self, while_statement: &ASTWhileStatement<'de>) {
        let label = while_statement
            .label
            .as_ref()
            .map(|token| token.span.literal.to_string());

        if self.loops.push(label.clone()).is_err() {
            panic!("Duplicate loop label '{}'", label.unwrap());
        }
        self.push_frame();
        self.visit_expression(&while_statement.condition);

        while self.last_value.unwrap() != 0_f64 {
            self.visit_statement(&while_statement.body);
            self.visit_expression(&while_statement.condition);
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
        call_expression: &ASTCallExpression<'de>,
        _expr: &ASTExpression<'de>,
    ) {
        let function = self
            .global_scope
            .lookup_function(call_expression.identifier.span.literal)
            .cloned()
            .unwrap_or_else(|| {
                panic!(
                    "Function {} not found",
                    call_expression.identifier.span.literal
                )
            });
        let mut arguments = Vec::new();
        for argument in &call_expression.arguments {
            self.visit_expression(argument);
            arguments.push(self.last_value.unwrap());
        }
        self.push_frame();
        for (argument, param) in arguments.iter().zip(function.parameters.iter()) {
            let parameter_name = param.name.clone();
            self.frames.insert(parameter_name, *argument);
        }

        self.visit_statement(&function.body);
        self.pop_frame();
    }

    fn visit_boolean_expression(
        &mut self,
        boolean: &ASTBooleanExpression,
        _expr: &ASTExpression<'de>,
    ) {
        self.last_value = Some(boolean.value as i64 as f64);
    }

    fn visit_error(&mut self, _span: &TextSpan) {
        panic!("Cannot evaluate error expression")
    }
}
