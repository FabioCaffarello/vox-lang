use crate::{
    ast::{
        AssignmentExpr, Ast, BinaryOperatorKind, BlockExpr, BooleanExpr, BreakStmt, CallExpr,
        Expression, FunctionDeclaration, IfExpr, LetStmt, NumberExpr, ReturnStmt, StmtID,
        UnaryExpr, UnaryOperatorKind, VariableExpr, WhileStmt,
    },
    loops::Loops,
    support::{expect_type, resolve_type_from_string},
    visitor::Visitor,
    Statement, StmtKind,
};
use diagnostics::diagnostics::DiagnosticsBagCell;
use index::{idx, Idx, IdxVec};
use text::span::TextSpan;
use typings::types::Type;

idx!(FunctionIdx);
idx!(VariableIdx);

#[derive(Debug)]
pub struct GlobalScope {
    pub variables: IdxVec<VariableIdx, VariableSymbol>,
    pub functions: IdxVec<FunctionIdx, FunctionSymbol>,
    global_variables: Vec<VariableIdx>,
}

impl Default for GlobalScope {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct FunctionSymbol {
    pub parameters: Vec<VariableIdx>,
    pub body: StmtID,
    pub return_type: Type,
    pub name: String,
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

    fn lookup_global_variable(&self, identifier: &str) -> Option<VariableIdx> {
        self.global_variables
            .iter()
            .map(|variable_idx| (*variable_idx, self.variables.get(*variable_idx)))
            .find(|(_, variable)| variable.name == identifier)
            .map(|(variable_idx, _)| variable_idx)
    }

    #[allow(clippy::result_unit_err)]
    pub fn declare_function(
        &mut self,
        identifier: &str,
        function_body_id: &StmtID,
        parameters: Vec<VariableIdx>,
        return_type: Type,
    ) -> Result<(), ()> {
        if self.lookup_function(identifier).is_some() {
            return Err(());
        }
        let function = FunctionSymbol {
            parameters,
            body: *function_body_id,
            return_type,
            name: identifier.to_string(),
        };
        self.functions.push(function);
        Ok(())
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
}

impl LocalScope {
    fn new() -> Self {
        LocalScope { locals: Vec::new() }
    }

    fn add_local(&mut self, variable: VariableIdx) {
        self.locals.push(variable);
    }
}

#[derive(Debug)]
pub struct Scopes {
    local_scopes: Vec<LocalScope>,
    pub global_scope: GlobalScope,
    surrounding_function: Option<FunctionIdx>,
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
            surrounding_function: None,
        }
    }

    pub fn from_global_scope(global_scope: GlobalScope) -> Self {
        Scopes {
            local_scopes: Vec::new(),
            global_scope,
            surrounding_function: None,
        }
    }

    fn enter_function_scope(&mut self, function: FunctionIdx) {
        self.surrounding_function = Some(function);
        self.enter_scope();
    }

    fn enter_scope(&mut self) {
        self.local_scopes.push(LocalScope::new());
    }

    fn exit_function_scope(&mut self) {
        self.surrounding_function = None;
        self.exit_scope();
    }

    fn exit_scope(&mut self) {
        self.local_scopes.pop();
    }

    pub fn declare_variable(&mut self, identifier: &str, ty: Type) -> VariableIdx {
        let is_inside_global_scope = self.is_inside_local_scope();
        let idx = self
            .global_scope
            .declare_variable(identifier, ty, !is_inside_global_scope);
        if is_inside_global_scope {
            self.current_local_scope_mut().add_local(idx);
        }
        idx
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

    fn lookup_function(&self, identifier: &str) -> Option<FunctionIdx> {
        self.global_scope.lookup_function(identifier)
    }

    fn is_inside_local_scope(&self) -> bool {
        !self.local_scopes.is_empty()
    }

    fn surrounding_function(&self) -> Option<&FunctionSymbol> {
        return self
            .surrounding_function
            .map(|idx| self.global_scope.functions.get(idx));
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
        expect_type(&self.diagnostics, span, expected, actual)
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

#[derive(Debug)]
pub struct GlobalSymbolResolver<'de> {
    diagnostics: DiagnosticsBagCell<'de>,
    global_scope: GlobalScope,
}

impl<'de> GlobalSymbolResolver<'de> {
    pub fn new(diagnostics: DiagnosticsBagCell<'de>) -> Self {
        GlobalSymbolResolver {
            diagnostics,
            global_scope: GlobalScope::new(),
        }
    }
}

impl<'de> Visitor<'de> for GlobalSymbolResolver<'de> {
    fn visit_function_declaration(
        &mut self,
        _ast: &mut Ast<'de>,
        func_decl: &FunctionDeclaration<'de>,
    ) {
        let parameters = func_decl
            .parameters
            .iter()
            .map(|parameter| {
                self.global_scope.declare_variable(
                    parameter.identifier.span.literal,
                    resolve_type_from_string(
                        &self.diagnostics,
                        &parameter.type_annotation.type_name,
                    ),
                    false,
                )
            })
            .collect();
        let literal_span = &func_decl.identifier.span;
        let return_type = match &func_decl.return_type {
            Some(return_type) => {
                resolve_type_from_string(&self.diagnostics, &return_type.type_name)
            }
            None => Type::Void,
        };
        match self.global_scope.declare_function(
            literal_span.literal,
            &func_decl.body,
            parameters,
            return_type,
        ) {
            Ok(_) => {}
            Err(_) => {
                self.diagnostics
                    .borrow_mut()
                    .report_function_already_declared(&func_decl.identifier);
            }
        }
    }

    fn visit_let_statement(
        &mut self,
        _ast: &mut Ast<'de>,
        _let_statement: &LetStmt<'de>,
        _stmt: &Statement<'de>,
    ) {
    }

    fn visit_variable_expression(
        &mut self,
        _ast: &mut Ast<'de>,
        _variable_expression: &VariableExpr,
        _expr: &Expression<'de>,
    ) {
    }

    fn visit_number_expression(
        &mut self,
        _ast: &mut Ast<'de>,
        _number: &NumberExpr,
        _expr: &Expression<'de>,
    ) {
    }

    fn visit_boolean_expression(
        &mut self,
        _ast: &mut Ast<'de>,
        _boolean: &BooleanExpr,
        _expr: &Expression<'de>,
    ) {
    }

    fn visit_error(&mut self, _ast: &mut Ast<'de>, _span: &TextSpan) {}

    fn visit_unary_expression(
        &mut self,
        _ast: &mut Ast<'de>,
        _unary_expression: &UnaryExpr,
        _expr: &Expression<'de>,
    ) {
    }

    fn visit_break_statement(&mut self, _ast: &mut Ast<'de>, _break_statement: &BreakStmt<'de>) {}
}

impl<'de> Visitor<'de> for Resolver<'de> {
    fn visit_function_declaration(
        &mut self,
        ast: &mut Ast<'de>,
        function_declaration: &FunctionDeclaration<'de>,
    ) {
        let function_id = self
            .scopes
            .lookup_function(function_declaration.identifier.span.literal)
            .unwrap();
        self.scopes.enter_function_scope(function_id);
        let function = self.scopes.global_scope.functions.get(function_id);
        for parameter in function.parameters.clone() {
            self.scopes.current_local_scope_mut().locals.push(parameter);
        }
        self.visit_statement(ast, function_declaration.body);
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
            .lookup_function(call_expression.identifier.span.literal);
        let ty = match function {
            None => {
                let mut diagnostics_binding = self.diagnostics.borrow_mut();
                diagnostics_binding.report_undeclared_function(&call_expression.identifier);
                Type::Void
            }
            Some(function) => {
                let function = self.scopes.global_scope.functions.get(function);
                if function.parameters.len() != call_expression.arguments.len() {
                    let mut diagnostics_binding = self.diagnostics.borrow_mut();
                    diagnostics_binding.report_invalid_argument_count(
                        &call_expression.identifier,
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
            Some(variable_idx) => {
                let variable = self.scopes.global_scope.variables.get(variable_idx);
                ast.set_type(expr.id, variable.ty.clone());
                ast.set_variable(expr.id, variable_idx);
            }
            None => {
                let mut diagnostics_binding = self.diagnostics.borrow_mut();
                diagnostics_binding.report_undeclared_variable(&variable_expression.identifier);
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
