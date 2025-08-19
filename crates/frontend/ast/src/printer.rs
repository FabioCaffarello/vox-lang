use crate::ast::{
    AssignmentExpr, Ast, BinaryExpr, BlockExpr, BooleanExpr, BreakStmt, CallExpr, Expression,
    FuncReturnTypeSyntax, FunctionDeclaration, IfExpr, LetStmt, NumberExpr, ParenthesizedExpr,
    ReturnStmt, Statement, StaticTypeAnnotation, UnaryExpr, VariableExpr, WhileStmt,
};
use crate::visitor::Visitor;
use termion::color::{self, Fg, Reset};
use text::span::TextSpan;
use typings::types::{ItemID, StmtID};

pub struct Printer {
    indent: usize,
    pub result: String,
}

impl Default for Printer {
    fn default() -> Self {
        Self::new()
    }
}

impl Printer {
    const NUMBER_COLOR: color::Cyan = color::Cyan;
    const TEXT_COLOR: color::LightWhite = color::LightWhite;
    const KEYWORD_COLOR: color::Magenta = color::Magenta;
    const VARIABLE_COLOR: color::Green = color::Green;
    const BOOLEAN_COLOR: color::Yellow = color::Yellow;
    const TYPE_COLOR: color::LightBlue = color::LightBlue;

    pub fn new() -> Self {
        Self {
            indent: 0,
            result: String::new(),
        }
    }

    #[allow(clippy::single_char_add_str)]
    fn add_whitespace(&mut self) {
        self.result.push_str(" ");
    }

    #[allow(clippy::single_char_add_str)]
    fn add_newline(&mut self) {
        self.result.push('\n');
    }

    fn add_keyword(&mut self, keyword: &str) {
        self.result
            .push_str(&format!("{}{}", Self::KEYWORD_COLOR.fg_str(), keyword,));
    }

    fn add_text(&mut self, text: &str) {
        self.result
            .push_str(&format!("{}{}", Self::TEXT_COLOR.fg_str(), text,));
    }

    fn add_variable(&mut self, variable: &str) {
        self.result
            .push_str(&format!("{}{}", Self::VARIABLE_COLOR.fg_str(), variable,));
    }

    fn add_padding(&mut self) {
        for _ in 0..self.indent {
            self.result.push_str("  ");
        }
    }

    fn add_boolean(&mut self, boolean: bool) {
        self.result
            .push_str(&format!("{}{}", Self::BOOLEAN_COLOR.fg_str(), boolean,));
    }

    fn add_type(&mut self, type_: &str) {
        self.result
            .push_str(&format!("{}{}", Self::TYPE_COLOR.fg_str(), type_,));
    }

    fn add_type_annotation(&mut self, type_annotation: &StaticTypeAnnotation) {
        self.add_text(":");
        self.add_whitespace();
        self.add_type(type_annotation.type_name.span.literal);
    }

    fn add_return_type(&mut self, return_type: &FuncReturnTypeSyntax) {
        self.add_text("->");
        self.add_whitespace();
        self.add_type(return_type.type_name.span.literal);
        self.add_whitespace();
    }
}

impl<'de> Visitor<'de> for Printer {
    fn visit_number_expression(
        &mut self,
        _ast: &mut Ast<'de>,
        number: &NumberExpr,
        _expr: &Expression<'de>,
    ) {
        self.result
            .push_str(&format!("{}{}", Self::NUMBER_COLOR.fg_str(), number.number));
    }

    fn visit_error(&mut self, _ast: &mut Ast<'de>, span: &TextSpan) {
        self.add_text(span.literal);
    }

    fn visit_statement(&mut self, ast: &mut Ast<'de>, statement: StmtID) {
        self.add_padding();
        Visitor::do_visit_statement(self, ast, statement);
        self.result.push_str(&format!("{}", Fg(Reset),));
    }

    fn visit_unary_expression(
        &mut self,
        ast: &mut Ast<'de>,
        unary_expression: &UnaryExpr,
        _expr: &Expression<'de>,
    ) {
        self.add_text(unary_expression.operator.token.span.literal);
        self.visit_expression(ast, &unary_expression.operand);
    }

    fn visit_binary_expression(
        &mut self,
        ast: &mut Ast<'de>,
        binary_expression: &BinaryExpr,
        _expr: &Expression<'de>,
    ) {
        self.visit_expression(ast, &binary_expression.left);
        self.add_whitespace();
        self.add_text(binary_expression.operator.token.span.literal);
        self.add_whitespace();
        self.visit_expression(ast, &binary_expression.right);
    }

    fn visit_parenthesized_expression(
        &mut self,
        ast: &mut Ast<'de>,
        parenthesized_expression: &ParenthesizedExpr,
        _expr: &Expression<'de>,
    ) {
        self.add_text("(");
        self.visit_expression(ast, &parenthesized_expression.expression);
        self.add_text(")");
    }

    fn visit_let_statement(
        &mut self,
        ast: &mut Ast<'de>,
        let_statement: &LetStmt<'de>,
        _stmt: &Statement<'de>,
    ) {
        self.add_keyword("let");
        self.add_whitespace();
        self.add_text(let_statement.identifier.span.literal);
        if let Some(type_annotation) = &let_statement.type_annotation {
            self.add_type_annotation(type_annotation);
            self.add_whitespace();
        }
        self.add_whitespace();
        self.add_text("=");
        self.add_whitespace();
        self.visit_expression(ast, &let_statement.initializer);
        self.add_newline();
    }

    fn visit_variable_expression(
        &mut self,
        _ast: &mut Ast<'de>,
        variable_expression: &VariableExpr,
        _expr: &Expression<'de>,
    ) {
        self.result.push_str(&format!(
            "{}{}",
            Self::VARIABLE_COLOR.fg_str(),
            variable_expression.identifier.span.literal,
        ));
    }

    fn visit_block_expression(
        &mut self,
        ast: &mut Ast<'de>,
        block_expr: &BlockExpr,
        _expr: &Expression<'de>,
    ) {
        self.add_text("{");
        self.add_newline();
        self.indent += 1;
        for statement in &block_expr.statements {
            self.visit_statement(ast, *statement);
        }
        self.indent -= 1;
        self.add_padding();
        // self.add_newline();
        self.add_text("}");
    }

    fn visit_if_expression(
        &mut self,
        ast: &mut Ast<'de>,
        if_statement: &IfExpr,
        _expr: &Expression<'de>,
    ) {
        self.add_keyword("if");
        self.add_whitespace();
        self.visit_expression(ast, &if_statement.condition);
        self.add_whitespace();
        self.visit_expression(ast, &if_statement.then_branch);
        // self.add_newline();
        if let Some(else_branch) = &if_statement.else_branch {
            self.add_whitespace();
            self.add_keyword("else");
            self.add_whitespace();
            self.visit_expression(ast, &else_branch.expr);
            // self.add_newline();
        }
    }

    fn visit_assignment_expression(
        &mut self,
        ast: &mut Ast<'de>,
        assignment_expression: &AssignmentExpr,
        _expr: &Expression<'de>,
    ) {
        self.add_variable(assignment_expression.identifier.span.literal);
        self.add_whitespace();
        self.add_text("=");
        self.add_whitespace();
        self.visit_expression(ast, &assignment_expression.expression);
        self.add_newline();
    }

    fn visit_function_declaration(
        &mut self,
        ast: &mut Ast<'de>,
        func_decl: &FunctionDeclaration<'de>,
        _item_id: ItemID,
    ) {
        self.add_keyword("func");
        self.add_whitespace();
        self.add_text(func_decl.identifier.span.literal);
        // self.add_whitespace();
        let are_parameters_empty = func_decl.parameters.is_empty();
        if !are_parameters_empty {
            self.add_text("(");
        } else {
            self.add_whitespace();
        }
        for (i, parameter) in func_decl.parameters.iter().enumerate() {
            if i != 0 {
                self.add_text(",");
                self.add_whitespace();
            }
            self.add_text(parameter.identifier.span.literal);
            self.add_type_annotation(&parameter.type_annotation);
        }
        if !are_parameters_empty {
            self.add_text(")");
            self.add_whitespace();
            if let Some(return_type) = &func_decl.return_type {
                self.add_return_type(return_type);
            }
        }
        self.visit_expression(ast, &func_decl.body);
        self.add_newline();
    }

    fn visit_return_statement(&mut self, ast: &mut Ast<'de>, return_statement: &ReturnStmt) {
        self.add_keyword("return");
        if let Some(expression) = &return_statement.return_value {
            self.add_whitespace();
            self.visit_expression(ast, expression);
        }
        self.add_newline();
    }

    fn visit_while_statement(&mut self, ast: &mut Ast<'de>, while_statement: &WhileStmt) {
        self.add_keyword("while");
        self.add_whitespace();
        self.visit_expression(ast, &while_statement.condition);
        self.add_whitespace();
        self.visit_expression(ast, &while_statement.body);
        self.add_newline();
    }

    #[allow(clippy::useless_format)]
    fn visit_break_statement(&mut self, _ast: &mut Ast<'de>, break_stmt: &BreakStmt<'de>) {
        self.add_keyword("break");
        if let Some(label) = &break_stmt.label {
            self.add_whitespace();
            self.add_keyword(format!("{}", label.span.literal).as_str());
        }
    }

    fn visit_call_expression(
        &mut self,
        ast: &mut Ast<'de>,
        call_expression: &CallExpr,
        _expr: &Expression<'de>,
    ) {
        self.add_text(call_expression.function_name());
        self.add_text("(");
        for (i, argument) in call_expression.arguments.iter().enumerate() {
            if i != 0 {
                self.add_text(",");
                self.add_whitespace();
            }
            self.visit_expression(ast, argument);
        }
        self.add_text(")");
    }

    fn visit_boolean_expression(
        &mut self,
        _ast: &mut Ast<'de>,
        boolean: &BooleanExpr,
        _expr: &Expression<'de>,
    ) {
        self.add_boolean(boolean.value);
    }
}
