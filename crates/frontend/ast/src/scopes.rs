// TODO: create tests
use crate::{
    ast::{
        AssignmentExpr, Ast, BinaryOperatorKind, BlockExpr, BooleanExpr, BreakStmt, CallExpr,
        Expression, FunctionDeclaration, IfExpr, LetStmt, NumberExpr, ReturnStmt, Statement,
        StmtKind, UnaryExpr, UnaryOperatorKind, VariableExpr, WhileStmt,
    },
    loops::Loops,
    support::expect_type,
    visitor::Visitor,
};
use diagnostics::diagnostics::DiagnosticsBagCell;
use index::IdxVec;
use resolver::resolve_type_from_string;
use text::span::TextSpan;
use typings::types::{ExprID, FunctionIdx, ItemID, Type, VariableIdx};

#[derive(Debug)]
pub struct GlobalScope {
    pub variables: IdxVec<VariableIdx, VariableSymbol>,
    pub functions: IdxVec<FunctionIdx, Function>,
    global_variables: Vec<VariableIdx>,
}

impl Default for GlobalScope {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub parameters: Vec<VariableIdx>,
    pub name: String,
    pub body: ExprID,
    pub return_type: Type,
}

impl GlobalScope {
    pub fn new() -> Self {
        GlobalScope {
            variables: IdxVec::new(),
            functions: IdxVec::new(),
            global_variables: Vec::new(),
        }
    }

    pub fn declare_variable(&mut self, identifier: &str, ty: Type, is_global: bool) -> VariableIdx {
        let variable = VariableSymbol::new(identifier.to_string(), ty);
        let variable_idx = self.variables.push(variable);
        if is_global {
            self.global_variables.push(variable_idx);
        }
        variable_idx
    }

    pub fn lookup_global_variable(&self, identifier: &str) -> Option<VariableIdx> {
        self.global_variables
            .iter()
            .rev()
            .map(|variable_idx| (*variable_idx, self.variables.get(*variable_idx)))
            .find(|(_, variable)| variable.name == identifier)
            .map(|(variable_idx, _)| variable_idx)
    }

    pub fn create_function(
        &mut self,
        identifier: String,
        function_body_id: ExprID,
        parameters: Vec<VariableIdx>,
        return_type: Type,
    ) -> Result<FunctionIdx, FunctionIdx> {
        if let Some(existing_function_idx) = self.lookup_function(&identifier) {
            return Err(existing_function_idx);
        }
        let function = Function {
            name: identifier,
            parameters,
            body: function_body_id,
            return_type,
        };
        Ok(self.functions.push(function))
    }

    pub fn lookup_function(&self, identifier: &str) -> Option<FunctionIdx> {
        self.functions
            .indexed_iter()
            .find(|(_, function)| function.name == identifier)
            .map(|(idx, _)| idx)
    }
}

#[derive(Debug)]
pub struct LocalScope {
    locals: Vec<VariableIdx>,
    function: Option<FunctionIdx>,
}

impl LocalScope {
    fn new(function: Option<FunctionIdx>) -> Self {
        LocalScope {
            locals: Vec::new(),
            function,
        }
    }

    fn add_local(&mut self, variable: VariableIdx) {
        self.locals.push(variable);
    }
}

#[derive(Debug)]
pub struct Scopes {
    local_scopes: Vec<LocalScope>,
    pub global_scope: GlobalScope,
}

impl Default for Scopes {
    fn default() -> Self {
        Self::new()
    }
}

impl Scopes {
    pub fn new() -> Self {
        Scopes {
            local_scopes: Vec::new(),
            global_scope: GlobalScope::new(),
        }
    }

    pub fn from_global_scope(global_scope: GlobalScope) -> Self {
        Scopes {
            local_scopes: Vec::new(),
            global_scope,
        }
    }

    fn enter_function_scope(&mut self, function_idx: FunctionIdx) {
        self._enter_scope(Some(function_idx));
    }

    fn enter_scope(&mut self) {
        self._enter_scope(None);
    }

    fn _enter_scope(&mut self, function_idx: Option<FunctionIdx>) {
        self.local_scopes.push(LocalScope::new(function_idx));
    }

    fn exit_function_scope(&mut self) {
        assert!(self.local_scopes.last().unwrap().function.is_some());
        self.exit_scope();
    }

    fn exit_scope(&mut self) {
        self.local_scopes.pop();
    }

    pub fn declare_variable(&mut self, identifier: &str, ty: Type) -> VariableIdx {
        let is_inside_global_scope = self.is_inside_local_scope();
        let idx = self._declare_variable(identifier, ty, !is_inside_global_scope);
        if is_inside_global_scope {
            self.current_local_scope_mut().add_local(idx);
        }
        idx
    }

    pub fn _declare_variable(
        &mut self,
        identifier: &str,
        ty: Type,
        is_global: bool,
    ) -> VariableIdx {
        self.global_scope
            .declare_variable(identifier, ty, is_global)
    }

    fn lookup_variable(&self, identifier: &str) -> Option<VariableIdx> {
        for scope in self.local_scopes.iter().rev() {
            if let Some((idx, _)) = scope
                .locals
                .iter()
                .map(|idx| (*idx, self.global_scope.variables.get(*idx)))
                .find(|(_, variable)| variable.name == identifier)
            {
                return Some(idx);
            }
        }
        self.global_scope.lookup_global_variable(identifier)
    }

    fn is_inside_local_scope(&self) -> bool {
        !self.local_scopes.is_empty()
    }

    fn surrounding_function(&self) -> Option<&Function> {
        self.surrounding_function_idx()
            .map(|function_idx| self.global_scope.functions.get(function_idx))
    }

    fn surrounding_function_idx(&self) -> Option<FunctionIdx> {
        self.local_scopes
            .iter()
            .rev()
            .filter_map(|scope| scope.function)
            .next()
    }

    fn current_local_scope_mut(&mut self) -> &mut LocalScope {
        self.local_scopes.last_mut().unwrap()
    }
}

#[derive(Debug)]
pub struct Resolver<'de> {
    pub scopes: Scopes,
    diagnostics: DiagnosticsBagCell<'de>,
    loops: Loops,
}

impl<'de> Resolver<'de> {
    pub fn new(diagnostics: DiagnosticsBagCell<'de>, scopes: Scopes) -> Self {
        Resolver {
            scopes,
            diagnostics,
            loops: Loops::new(),
        }
    }

    pub fn resolve(&mut self, ast: &mut Ast<'de>) {
        for item_id in ast.items.cloned_indices().iter() {
            self.visit_item(ast, *item_id);
        }
    }

    pub fn resolve_binary_expression(
        &self,
        ast: &Ast<'de>,
        left: &Expression<'de>,
        right: &Expression<'de>,
        operator: &BinaryOperatorKind,
    ) -> Type {
        let matrix: (Type, Type, Type) = match operator {
            BinaryOperatorKind::Plus => (Type::Float, Type::Float, Type::Float),
            BinaryOperatorKind::Subtract => (Type::Float, Type::Float, Type::Float),
            BinaryOperatorKind::Multiply => (Type::Float, Type::Float, Type::Float),
            BinaryOperatorKind::Divide => (Type::Float, Type::Float, Type::Float),
            BinaryOperatorKind::Power => (Type::Float, Type::Float, Type::Float),
            BinaryOperatorKind::Equals => (Type::Float, Type::Float, Type::Bool),
            BinaryOperatorKind::NotEquals => (Type::Float, Type::Float, Type::Bool),
            BinaryOperatorKind::LessThan => (Type::Float, Type::Float, Type::Bool),
            BinaryOperatorKind::LessThanOrEqual => (Type::Float, Type::Float, Type::Bool),
            BinaryOperatorKind::GreaterThan => (Type::Float, Type::Float, Type::Bool),
            BinaryOperatorKind::GreaterThanOrEqual => (Type::Float, Type::Float, Type::Bool),
        };
        self.expect_type(&left.span(ast), matrix.0, &left.ty);
        self.expect_type(&right.span(ast), matrix.1, &right.ty);
        matrix.2
    }

    pub fn resolve_unary_expression(
        &self,
        ast: &Ast<'de>,
        operand: &Expression<'de>,
        operator: &UnaryOperatorKind,
    ) -> Type {
        let matrix: (Type, Type) = match operator {
            UnaryOperatorKind::Minus => (Type::Float, Type::Float),
        };
        self.expect_type(&operand.span(ast), matrix.0, &operand.ty);
        matrix.1
    }

    fn expect_type(&self, span: &TextSpan<'de>, expected: Type, actual: &Type) -> Type {
        expect_type(&self.diagnostics, expected, actual, span)
    }
}

#[derive(Debug, Clone)]
pub struct VariableSymbol {
    pub name: String,
    pub ty: Type,
}

impl VariableSymbol {
    pub fn new(name: String, ty: Type) -> Self {
        VariableSymbol { name, ty }
    }
}

impl<'de> Visitor<'de> for Resolver<'de> {
    fn visit_function_declaration(
        &mut self,
        ast: &mut Ast<'de>,
        func_decl: &FunctionDeclaration<'de>,
        _item_id: ItemID,
    ) {
        let function_idx = func_decl.function_idx;
        self.scopes.enter_function_scope(function_idx);
        let function = self.scopes.global_scope.functions.get(function_idx);
        for parameter in function.parameters.clone() {
            self.scopes.current_local_scope_mut().locals.push(parameter);
        }
        self.visit_expression(ast, &func_decl.body);
        self.scopes.exit_function_scope();
    }

    fn visit_return_statement(&mut self, ast: &mut Ast<'de>, return_statement: &ReturnStmt<'de>) {
        match self.scopes.surrounding_function().cloned() {
            None => {
                let mut diagnostics_binding = self.diagnostics.borrow_mut();
                diagnostics_binding
                    .report_cannot_return_outside_function(&return_statement.return_keyword);
            }
            Some(function) => {
                if let Some(return_expression) = &return_statement.return_value {
                    self.visit_expression(ast, return_expression);
                    let return_expression = ast.query_expr(*return_expression);
                    self.expect_type(
                        &return_expression.span(ast),
                        function.return_type.clone(),
                        &return_expression.ty,
                    );
                } else {
                    self.expect_type(
                        &return_statement.return_keyword.span,
                        Type::Void,
                        &Type::Void,
                    );
                }
            }
        }
    }

    fn visit_block_expression(
        &mut self,
        ast: &mut Ast<'de>,
        block_expr: &BlockExpr,
        expr: &Expression<'de>,
    ) {
        self.scopes.enter_scope();
        for statement in &block_expr.statements {
            self.visit_statement(ast, *statement);
        }
        self.scopes.exit_scope();
        let ty = block_expr
            .statements
            .last()
            .map(|stmt| {
                let stmt = ast.query_stmt(*stmt);
                match stmt.kind {
                    StmtKind::Expr(expr_id) => {
                        let expr = ast.query_expr(expr_id);
                        expr.ty.clone()
                    }
                    _ => Type::Void,
                }
            })
            .unwrap_or(Type::Void);
        ast.set_type(expr.id, ty);
    }

    fn visit_if_expression(
        &mut self,
        ast: &mut Ast<'de>,
        if_statement: &IfExpr<'de>,
        expr: &Expression<'de>,
    ) {
        self.scopes.enter_scope();
        self.visit_expression(ast, &if_statement.condition);
        let condition_expression = ast.query_expr(if_statement.condition);
        self.expect_type(
            &condition_expression.span(ast),
            Type::Bool,
            &condition_expression.ty,
        );
        self.visit_expression(ast, &if_statement.then_branch);
        let mut ty = Type::Void;
        self.scopes.exit_scope();
        if let Some(else_branch) = &if_statement.else_branch {
            self.scopes.enter_scope();
            self.visit_expression(ast, &else_branch.expr);
            let then_expression = ast.query_expr(if_statement.then_branch);
            let else_expression = ast.query_expr(else_branch.expr);
            ty = self.expect_type(
                &then_expression.span(ast),
                else_expression.ty.clone(),
                &then_expression.ty,
            );
            self.scopes.exit_scope();
        }
        ast.set_type(expr.id, ty);
    }

    fn visit_let_statement(
        &mut self,
        ast: &mut Ast<'de>,
        let_statement: &LetStmt<'de>,
        stmt: &Statement<'de>,
    ) {
        let identifier = let_statement.identifier.span.literal;
        self.visit_expression(ast, &let_statement.initializer);
        let initializer_expression = ast.query_expr(let_statement.initializer);
        let ty = if let Some(type_annotation) = &let_statement.type_annotation {
            let ty = resolve_type_from_string(&self.diagnostics, &type_annotation.type_name);
            self.expect_type(
                &initializer_expression.span(ast),
                ty.clone(),
                &initializer_expression.ty,
            );
            ty
        } else {
            initializer_expression.ty.clone()
        };
        let variable = self.scopes.declare_variable(identifier, ty);
        ast.set_variable_for_stmt(stmt.id, variable);
    }

    fn visit_call_expression(
        &mut self,
        ast: &mut Ast<'de>,
        call_expression: &CallExpr<'de>,
        expr: &Expression<'de>,
    ) {
        let function = self
            .scopes
            .global_scope
            .lookup_function(call_expression.function_name());
        let ty = match function {
            None => {
                let mut diagnostics_binding = self.diagnostics.borrow_mut();
                diagnostics_binding.report_undeclared_function(&call_expression.callee);
                Type::Error
            }
            Some(function) => {
                let function = self.scopes.global_scope.functions.get(function);
                if function.parameters.len() != call_expression.arguments.len() {
                    let mut diagnostics_binding = self.diagnostics.borrow_mut();
                    diagnostics_binding.report_invalid_argument_count(
                        &call_expression.callee.span,
                        function.parameters.len(),
                        call_expression.arguments.len(),
                    );
                }
                let return_type = function.return_type.clone();
                for (argument, param) in call_expression
                    .arguments
                    .iter()
                    .zip(function.parameters.clone().iter())
                {
                    self.visit_expression(ast, argument);
                    let argument_expression = ast.query_expr(*argument);
                    let param = self.scopes.global_scope.variables.get(*param);
                    self.expect_type(
                        &argument_expression.span(ast),
                        param.ty.clone(),
                        &argument_expression.ty,
                    );
                }
                return_type
            }
        };
        ast.set_type(expr.id, ty);
    }

    fn visit_variable_expression(
        &mut self,
        ast: &mut Ast<'de>,
        variable_expression: &VariableExpr<'de>,
        expr: &Expression<'de>,
    ) {
        match self
            .scopes
            .lookup_variable(variable_expression.identifier.span.literal)
        {
            None => {
                let mut diagnostics_binding = self.diagnostics.borrow_mut();
                diagnostics_binding.report_undeclared_variable(&variable_expression.identifier);
            }
            Some(variable_idx) => {
                let variable = self.scopes.global_scope.variables.get(variable_idx);
                ast.set_type(expr.id, variable.ty.clone());
                ast.set_variable(expr.id, variable_idx);
            }
        }
    }

    fn visit_number_expression(
        &mut self,
        ast: &mut Ast<'de>,
        _number: &NumberExpr,
        expr: &Expression<'de>,
    ) {
        ast.set_type(expr.id, Type::Float);
    }

    fn visit_boolean_expression(
        &mut self,
        ast: &mut Ast<'de>,
        _boolean: &BooleanExpr,
        expr: &Expression<'de>,
    ) {
        ast.set_type(expr.id, Type::Bool);
    }

    fn visit_error(&mut self, _ast: &mut Ast<'de>, _span: &TextSpan) {}

    fn visit_unary_expression(
        &mut self,
        ast: &mut Ast<'de>,
        unary_expression: &UnaryExpr<'de>,
        expr: &Expression<'de>,
    ) {
        self.visit_expression(ast, &unary_expression.operand);
        let operand = ast.query_expr(unary_expression.operand);
        let ty = self.resolve_unary_expression(ast, operand, &unary_expression.operator.kind);
        ast.set_type(expr.id, ty);
    }

    fn visit_binary_expression(
        &mut self,
        ast: &mut Ast<'de>,
        binary_expression: &crate::BinaryExpr<'de>,
        expr: &Expression<'de>,
    ) {
        self.visit_expression(ast, &binary_expression.left);
        self.visit_expression(ast, &binary_expression.right);
        let left = ast.query_expr(binary_expression.left);
        let right = ast.query_expr(binary_expression.right);
        let ty = self.resolve_binary_expression(ast, left, right, &binary_expression.operator.kind);
        ast.set_type(expr.id, ty);
        // TODO: Check if the types are compatible
    }

    fn visit_break_statement(&mut self, _ast: &mut Ast<'de>, break_stmt: &BreakStmt<'de>) {
        if !self.loops.is_inside_loop() {
            // Report 'break' outside of any loop
            self.diagnostics
                .borrow_mut()
                .report_break_outside_loop(&break_stmt.break_keyword);
            return;
        }

        if let Some(label_token) = &break_stmt.label {
            let label = &label_token.span.literal;

            // Check if the label exists in the active loops
            if self.loops.find_label(label).is_none() {
                // Report undefined label error
                self.diagnostics
                    .borrow_mut()
                    .report_undefined_label(label_token);
            }
        }
    }

    fn visit_while_statement(&mut self, ast: &mut Ast<'de>, while_statement: &WhileStmt<'de>) {
        // Extract the label if present
        let label = while_statement
            .label
            .as_ref()
            .map(|token| token.span.literal.to_string());

        if self.loops.push(label.clone()).is_err() {
            // Report duplicate label error
            self.diagnostics
                .borrow_mut()
                .report_duplicate_label(while_statement.label.as_ref().unwrap());
        }

        self.scopes.enter_scope();

        self.visit_expression(ast, &while_statement.condition);
        let condition_expression = ast.query_expr(while_statement.condition);
        self.expect_type(
            &condition_expression.span(ast),
            Type::Bool,
            &condition_expression.ty,
        );
        self.visit_expression(ast, &while_statement.body);

        self.scopes.exit_scope();

        // Pop the loop from the Loops stack
        self.loops.pop();
    }

    fn visit_parenthesized_expression(
        &mut self,
        ast: &mut Ast<'de>,
        parenthesized_expression: &crate::ast::ParenthesizedExpr,
        expr: &Expression<'de>,
    ) {
        self.visit_expression(ast, &parenthesized_expression.expression);
        let expression = ast.query_expr(parenthesized_expression.expression);
        ast.set_type(expr.id, expression.ty.clone());
    }

    fn visit_assignment_expression(
        &mut self,
        ast: &mut Ast<'de>,
        assignment_expression: &AssignmentExpr<'de>,
        expr: &Expression<'de>,
    ) {
        self.visit_expression(ast, &assignment_expression.expression);
        let identifier = &assignment_expression.identifier.span.literal;
        let ty = match self.scopes.lookup_variable(identifier) {
            Some(variable) => {
                ast.set_variable(expr.id, variable);
                let variable = self.scopes.global_scope.variables.get(variable);
                let value_expression = ast.query_expr(assignment_expression.expression);
                self.expect_type(
                    &value_expression.span(ast),
                    variable.ty.clone(),
                    &value_expression.ty,
                );
                variable.ty.clone()
            }
            None => {
                let mut diagnostics_binding = self.diagnostics.borrow_mut();
                diagnostics_binding.report_undeclared_variable(&assignment_expression.identifier);
                Type::Void
            }
        };
        ast.set_type(expr.id, ty);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{BinaryOperator, ElseBranch, ExprKind, ItemKind};
    use diagnostics::diagnostics::{DiagnosticsBag, DiagnosticsBagCell};
    use index::Idx; // <-- brings Idx::as_index/new into scope
    use std::{cell::RefCell, rc::Rc};
    use text::span::TextSpan;
    use token::{Token, TokenKind};

    // ---------- helpers ----------
    fn span(lit: &'static str, start: usize) -> TextSpan<'static> {
        TextSpan::new(start, start + lit.len(), lit)
    }
    fn tok(kind: TokenKind, lit: &'static str, start: usize) -> Token<'static> {
        Token {
            kind,
            span: span(lit, start),
        }
    }
    fn ident(lit: &'static str, start: usize) -> Token<'static> {
        tok(TokenKind::Identifier, lit, start)
    }
    fn new_diag_bag<'de>() -> DiagnosticsBagCell<'de> {
        Rc::new(RefCell::new(DiagnosticsBag::new()))
    }
    fn new_resolver<'de>() -> (Resolver<'de>, DiagnosticsBagCell<'de>) {
        let bag = new_diag_bag();
        (Resolver::new(bag.clone(), Scopes::new()), bag)
    }

    // ---------- typing basics: numbers, unary, binary ----------
    #[test]
    fn types_number_unary_and_binary() {
        let (mut res, bag) = new_resolver();
        let mut ast = Ast::new();

        let n1 = ast
            .number_literal_expression(1.0, tok(TokenKind::Number(1.0), "1", 0))
            .id;
        let n2 = ast
            .number_literal_expression(2.0, tok(TokenKind::Number(2.0), "2", 2))
            .id;

        let plus = BinaryOperator::new(BinaryOperatorKind::Plus, tok(TokenKind::Plus, "+", 1));
        let add = ast.binary_expression(n1, plus, n2).id;
        res.visit_expression(&mut ast, &add);
        assert_eq!(ast.query_expr(add).ty.to_string(), Type::Float.to_string());

        let minus =
            crate::ast::UnaryOperator::new(UnaryOperatorKind::Minus, tok(TokenKind::Minus, "-", 3));
        let unary = ast.unary_expression(minus, n2).id;
        res.visit_expression(&mut ast, &unary);
        assert_eq!(
            ast.query_expr(unary).ty.to_string(),
            Type::Float.to_string()
        );

        assert!(bag.borrow().diagnostics.is_empty());
    }

    // ---------- let/var/assignment ----------
    #[test]
    fn let_declares_global_then_var_resolves_and_assignment_types() {
        let (mut res, bag) = new_resolver();
        let mut ast = Ast::new();

        // let x = 1
        let one = ast
            .number_literal_expression(1.0, tok(TokenKind::Number(1.0), "1", 4))
            .id;
        let let_stmt = ast.let_statement(ident("x", 0), one, None).id;
        res.visit_statement(&mut ast, let_stmt);

        // x
        let var = ast.identifier_expression(ident("x", 10)).id;
        res.visit_expression(&mut ast, &var);
        assert_eq!(ast.query_expr(var).ty.to_string(), Type::Float.to_string());

        // variable index set
        let expected_idx = res.scopes.lookup_variable("x").expect("x declared");
        match &ast.query_expr(var).kind {
            ExprKind::Variable(v) => assert_eq!(v.variable_idx.as_index(), expected_idx.as_index()),
            _ => panic!("expected variable expr"),
        }

        // x = 2
        let two = ast
            .number_literal_expression(2.0, tok(TokenKind::Number(2.0), "2", 14))
            .id;
        let assign = ast
            .assignment_expression(ident("x", 12), tok(TokenKind::Equal, "=", 13), two)
            .id;
        res.visit_expression(&mut ast, &assign);
        assert_eq!(
            ast.query_expr(assign).ty.to_string(),
            Type::Float.to_string()
        );

        assert!(bag.borrow().diagnostics.is_empty());
    }

    #[test]
    fn assignment_to_undeclared_reports_error() {
        let (mut res, bag) = new_resolver();
        let mut ast = Ast::new();

        let two = ast
            .number_literal_expression(2.0, tok(TokenKind::Number(2.0), "2", 3))
            .id;
        let assign = ast
            .assignment_expression(ident("y", 0), tok(TokenKind::Equal, "=", 2), two)
            .id;

        res.visit_expression(&mut ast, &assign);
        let diags = &bag.borrow().diagnostics;
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.starts_with("Undeclared variable"));
    }

    // ---------- if / else ----------
    #[test]
    fn if_else_types_to_branch_type_and_checks_condition() {
        let (mut res, bag) = new_resolver();
        let mut ast = Ast::new();

        let cond = ast
            .boolean_expression(tok(TokenKind::True, "true", 0), true)
            .id;
        let then_e = ast
            .number_literal_expression(1.0, tok(TokenKind::Number(1.0), "1", 6))
            .id;
        let else_e = ast
            .number_literal_expression(2.0, tok(TokenKind::Number(2.0), "2", 9))
            .id;

        let if_expr = ast
            .if_expression(
                tok(TokenKind::If, "if", 3),
                cond,
                then_e,
                Some(ElseBranch::new(tok(TokenKind::Else, "else", 7), else_e)),
            )
            .id;

        res.visit_expression(&mut ast, &if_expr);
        assert_eq!(
            ast.query_expr(if_expr).ty.to_string(),
            Type::Float.to_string()
        );
        assert!(bag.borrow().diagnostics.is_empty());

        // mismatched else to trigger diagnostic
        let cond2 = ast
            .boolean_expression(tok(TokenKind::True, "true", 20), true)
            .id;
        let then2 = ast
            .number_literal_expression(3.0, tok(TokenKind::Number(3.0), "3", 26))
            .id;
        let else2 = ast
            .boolean_expression(tok(TokenKind::False, "false", 29), false)
            .id;
        let if_bad = ast
            .if_expression(
                tok(TokenKind::If, "if", 23),
                cond2,
                then2,
                Some(ElseBranch::new(tok(TokenKind::Else, "else", 30), else2)),
            )
            .id;

        let before = bag.borrow().diagnostics.len();
        res.visit_expression(&mut ast, &if_bad);
        assert!(bag.borrow().diagnostics.len() > before);
    }

    // ---------- block ----------
    #[test]
    fn block_type_is_last_expr_or_void_when_empty() {
        let (mut res, _bag) = new_resolver();
        let mut ast = Ast::new();

        // { let x = 1; (2) }
        let one = ast
            .number_literal_expression(1.0, tok(TokenKind::Number(1.0), "1", 9))
            .id;
        let let_stmt = ast.let_statement(ident("x", 5), one, None).id;

        let two = ast
            .number_literal_expression(2.0, tok(TokenKind::Number(2.0), "2", 14))
            .id;
        let par = ast
            .parenthesized_expression(
                tok(TokenKind::LParen, "(", 13),
                two,
                tok(TokenKind::RParen, ")", 15),
            )
            .id;
        let expr_stmt = ast.expression_statement(par).id;

        let blk = ast
            .block_expression(
                tok(TokenKind::LBrace, "{", 0),
                vec![let_stmt, expr_stmt],
                tok(TokenKind::RBrace, "}", 16),
            )
            .id;

        res.visit_expression(&mut ast, &blk);
        assert_eq!(ast.query_expr(blk).ty.to_string(), Type::Float.to_string());

        // {}
        let empty_blk = ast
            .block_expression(
                tok(TokenKind::LBrace, "{", 100),
                vec![],
                tok(TokenKind::RBrace, "}", 101),
            )
            .id;
        res.visit_expression(&mut ast, &empty_blk);
        assert_eq!(
            ast.query_expr(empty_blk).ty.to_string(),
            Type::Void.to_string()
        );
    }

    // ---------- returns ----------
    #[test]
    fn return_outside_function_reports_error() {
        let (mut res, bag) = new_resolver();
        let mut ast = Ast::new();

        let v = ast
            .number_literal_expression(1.0, tok(TokenKind::Number(1.0), "1", 0))
            .id;
        let ret = ast
            .return_statement(tok(TokenKind::Return, "return", 2), Some(v))
            .id;

        // avoid E0502: clone the stmt before passing &mut ast
        let ret_stmt = ast.query_stmt(ret).clone();
        if let StmtKind::Return(r) = ret_stmt.kind {
            res.visit_return_statement(&mut ast, &r);
        }

        let diags = &bag.borrow().diagnostics;
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].message, "Cannot use 'return' outside of function");
    }

    #[test]
    fn function_scope_binds_params_and_body_resolves() {
        let (mut res, bag) = new_resolver();
        let mut ast = Ast::new();

        // body: identifier "x"
        let x_id_tok = ident("x", 20);
        let body_expr = ast.identifier_expression(x_id_tok).id;

        // declare parameter; resolver will push into local scope
        let param_idx = res
            .scopes
            .global_scope
            .declare_variable("x", Type::Float, false);

        // create function in global scope
        let f_idx = res
            .scopes
            .global_scope
            .create_function("f".to_string(), body_expr, vec![param_idx], Type::Float)
            .expect("create function");

        // create AST item and get its real id
        let item_id = ast
            .func_item(
                tok(TokenKind::Fun, "func", 0),
                ident("f", 5),
                vec![],
                body_expr,
                None,
                f_idx,
            )
            .id;

        // fetch decl by id and visit
        let decl_box = match ast.query_item(item_id).kind.clone() {
            ItemKind::Function(decl) => decl, // Box<FunctionDeclaration<'_>>
            _ => unreachable!("expected function item"),
        };
        let decl_ref: &FunctionDeclaration<'_> = &decl_box; // deref-coerces fine

        res.visit_function_declaration(&mut ast, decl_ref, item_id);

        assert_eq!(
            ast.query_expr(body_expr).ty.to_string(),
            Type::Float.to_string()
        );
        assert!(bag.borrow().diagnostics.is_empty());
    }

    // ---------- calls ----------
    #[test]
    fn call_known_function_sets_return_type_and_checks_args() {
        let (mut res, bag) = new_resolver();
        let mut ast = Ast::new();

        // g(p: Float) -> Bool
        let param_idx = res
            .scopes
            .global_scope
            .declare_variable("p", Type::Float, false);
        let dummy_body = ast
            .number_literal_expression(0.0, tok(TokenKind::Number(0.0), "0", 100))
            .id;
        res.scopes
            .global_scope
            .create_function("g".to_string(), dummy_body, vec![param_idx], Type::Bool)
            .unwrap();

        // g(9)
        let arg = ast
            .number_literal_expression(9.0, tok(TokenKind::Number(9.0), "9", 10))
            .id;
        let call_ok = ast
            .call_expression(
                ident("g", 0),
                tok(TokenKind::LParen, "(", 1),
                tok(TokenKind::RParen, ")", 4),
                vec![arg],
            )
            .id;
        res.visit_expression(&mut ast, &call_ok);
        assert_eq!(
            ast.query_expr(call_ok).ty.to_string(),
            Type::Bool.to_string()
        );
        let base = bag.borrow().diagnostics.len();

        // g() -> arity error
        let call_bad_arity = ast
            .call_expression(
                ident("g", 20),
                tok(TokenKind::LParen, "(", 21),
                tok(TokenKind::RParen, ")", 22),
                vec![],
            )
            .id;
        res.visit_expression(&mut ast, &call_bad_arity);
        assert!(bag.borrow().diagnostics.len() > base);

        // g(true) -> type mismatch
        let tru = ast
            .boolean_expression(tok(TokenKind::True, "true", 30), true)
            .id;
        let call_bad_type = ast
            .call_expression(
                ident("g", 30),
                tok(TokenKind::LParen, "(", 31),
                tok(TokenKind::RParen, ")", 36),
                vec![tru],
            )
            .id;
        let prev = bag.borrow().diagnostics.len();
        res.visit_expression(&mut ast, &call_bad_type);
        assert!(bag.borrow().diagnostics.len() > prev);
    }

    #[test]
    fn call_unknown_function_reports_error() {
        let (mut res, bag) = new_resolver();
        let mut ast = Ast::new();

        let call = ast
            .call_expression(
                ident("nope", 0),
                tok(TokenKind::LParen, "(", 4),
                tok(TokenKind::RParen, ")", 5),
                vec![],
            )
            .id;
        res.visit_expression(&mut ast, &call);

        let diags = &bag.borrow().diagnostics;
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].message, "Undeclared function 'nope'");
    }

    // ---------- loops / breaks / labels ----------
    #[test]
    fn break_outside_loop_reports_error() {
        let (mut res, bag) = new_resolver();
        let mut ast = Ast::new();

        let br = ast
            .break_statement(tok(TokenKind::Break, "break", 0), None)
            .id;

        // avoid E0502: clone stmt first
        let br_stmt = ast.query_stmt(br).clone();
        if let StmtKind::Break(b) = br_stmt.kind {
            res.visit_break_statement(&mut ast, &b);
        }

        let diags = &bag.borrow().diagnostics;
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].message, "Break statement outside of loop");
    }

    #[test]
    fn break_with_undefined_label_inside_loop_reports_error() {
        let (mut res, bag) = new_resolver();
        let mut ast = Ast::new();

        let cond = ast
            .boolean_expression(tok(TokenKind::True, "true", 10), true)
            .id;
        let br_stmt = ast
            .break_statement(tok(TokenKind::Break, "break", 20), Some(ident("X", 26)))
            .id;
        let body_blk = ast
            .block_expression(
                tok(TokenKind::LBrace, "{", 30),
                vec![br_stmt],
                tok(TokenKind::RBrace, "}", 37),
            )
            .id;
        let w_stmt = ast
            .while_statement(
                Some(ident("L", 0)),
                tok(TokenKind::While, "while", 5),
                cond,
                body_blk,
            )
            .id;

        let w_stmt_cloned = ast.query_stmt(w_stmt).clone();
        if let StmtKind::While(w) = w_stmt_cloned.kind {
            res.visit_while_statement(&mut ast, &w);
        }

        let diags = &bag.borrow().diagnostics;
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].message, "Undefined label 'X'");
    }

    #[test]
    fn duplicate_loop_label_is_reported_on_inner_while() {
        let (mut res, bag) = new_resolver();
        let mut ast = Ast::new();

        let cond1 = ast
            .boolean_expression(tok(TokenKind::True, "true", 10), true)
            .id;

        let cond2 = ast
            .boolean_expression(tok(TokenKind::True, "true", 40), true)
            .id;
        let inner_body = ast
            .block_expression(
                tok(TokenKind::LBrace, "{", 50),
                vec![],
                tok(TokenKind::RBrace, "}", 51),
            )
            .id;
        let inner = ast
            .while_statement(
                Some(ident("L", 41)),
                tok(TokenKind::While, "while", 42),
                cond2,
                inner_body,
            )
            .id;

        let outer_body = ast
            .block_expression(
                tok(TokenKind::LBrace, "{", 20),
                vec![inner],
                tok(TokenKind::RBrace, "}", 30),
            )
            .id;
        let outer = ast
            .while_statement(
                Some(ident("L", 0)),
                tok(TokenKind::While, "while", 5),
                cond1,
                outer_body,
            )
            .id;

        let outer_stmt = ast.query_stmt(outer).clone();
        if let StmtKind::While(w) = outer_stmt.kind {
            res.visit_while_statement(&mut ast, &w);
        }

        let diags = &bag.borrow().diagnostics;
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].message, "Duplicate label 'L'");
    }
}
