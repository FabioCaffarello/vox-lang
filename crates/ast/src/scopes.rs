use crate::{
    ast::{
        ASTBinaryOperatorKind, ASTBlockStatement, ASTBooleanExpression, ASTBreakStatement,
        ASTCallExpression, ASTExpression, ASTFuncDeclStatement, ASTIfStatement, ASTLetStatement,
        ASTNumberExpression, ASTReturnStatement, ASTStmtID, ASTUnaryExpression,
        ASTUnaryOperatorKind, ASTVariableExpression, ASTWhileStatement, Ast,
    },
    loops::Loops,
    support::{expect_type, resolve_type_from_string},
    visitor::ASTVisitor,
};
use diagnostics::diagnostics::DiagnosticsBagCell;
use std::collections::HashMap;
use text::span::TextSpan;
use typings::types::Type;

#[derive(Debug)]
pub struct GlobalScope {
    pub variables: HashMap<String, VariableSymbol>,
    pub functions: HashMap<String, FunctionSymbol>,
}

impl Default for GlobalScope {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct FunctionSymbol {
    pub parameters: Vec<VariableSymbol>,
    pub body: ASTStmtID,
    pub return_type: Type,
}

impl GlobalScope {
    pub fn new() -> Self {
        GlobalScope {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    fn declare_variable(&mut self, identifier: &str, ty: Type) {
        let variable = VariableSymbol::new(identifier.to_string(), ty);
        self.variables.insert(identifier.to_string(), variable);
    }

    fn lookup_variable(&self, identifier: &str) -> Option<&VariableSymbol> {
        self.variables.get(identifier)
    }

    #[allow(clippy::result_unit_err)]
    pub fn declare_function(
        &mut self,
        identifier: &str,
        function_body_id: &ASTStmtID,
        parameters: Vec<VariableSymbol>,
        return_type: Type,
    ) -> Result<(), ()> {
        if self.functions.contains_key(identifier) {
            return Err(());
        }
        let function = FunctionSymbol {
            parameters,
            body: *function_body_id,
            return_type,
        };
        self.functions.insert(identifier.to_string(), function);
        Ok(())
    }

    pub fn lookup_function(&self, identifier: &str) -> Option<&FunctionSymbol> {
        self.functions.get(identifier)
    }
}

#[derive(Debug)]
pub struct LocalScope {
    variables: HashMap<String, VariableSymbol>,
    // TODO:
    function: Option<FunctionSymbol>,
}

impl LocalScope {
    fn new(function: Option<FunctionSymbol>) -> Self {
        LocalScope {
            variables: HashMap::new(),
            function,
        }
    }

    fn declare_variable(&mut self, identifier: &str, ty: Type) {
        let variable = VariableSymbol::new(identifier.to_string(), ty);
        self.variables.insert(identifier.to_string(), variable);
    }

    fn lookup_variable(&self, identifier: &str) -> Option<&VariableSymbol> {
        self.variables.get(identifier)
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

    fn enter_scope(&mut self, function: Option<FunctionSymbol>) {
        self.local_scopes.push(LocalScope::new(function));
    }

    fn exit_scope(&mut self) {
        self.local_scopes.pop();
    }

    pub fn declare_variable(&mut self, identifier: &str, ty: Type) {
        if self.is_inside_local_scope() {
            self.local_scopes
                .last_mut()
                .unwrap()
                .declare_variable(identifier, ty);
        } else {
            self.global_scope.declare_variable(identifier, ty);
        }
    }

    fn lookup_variable(&self, identifier: &str) -> Option<&VariableSymbol> {
        for local_scope in self.local_scopes.iter().rev() {
            if let Some(variable) = local_scope.lookup_variable(identifier) {
                return Some(variable);
            }
        }
        self.global_scope.lookup_variable(identifier)
    }

    fn lookup_function(&self, identifier: &str) -> Option<&FunctionSymbol> {
        self.global_scope.lookup_function(identifier)
    }

    fn is_inside_local_scope(&self) -> bool {
        !self.local_scopes.is_empty()
    }

    fn surrounding_function(&self) -> Option<&FunctionSymbol> {
        for local_scope in self.local_scopes.iter().rev() {
            if let Some(function) = &local_scope.function {
                return Some(function);
            }
        }
        None
    }
}

#[derive(Debug)]
pub struct Resolver<'a, 'de> {
    pub scopes: Scopes,
    diagnostics: DiagnosticsBagCell<'de>,
    loops: Loops,
    ast: &'a mut Ast<'de>,
}

impl<'a, 'de> Resolver<'a, 'de> {
    pub fn new(
        diagnostics: DiagnosticsBagCell<'de>,
        scopes: Scopes,
        ast: &'a mut Ast<'de>,
    ) -> Self {
        Resolver {
            ast,
            scopes,
            diagnostics,
            loops: Loops::new(),
        }
    }

    #[allow(clippy::map_clone)]
    pub fn resolve(&mut self) {
        let stmt_ids: Vec<ASTStmtID> = self
            .ast
            .top_level_statement_ids
            .iter()
            .map(|stmt| *stmt)
            .collect();
        for stmt_id in stmt_ids {
            self.visit_statement(&stmt_id);
        }
    }

    pub fn resolve_binary_expression(
        &self,
        left: &ASTExpression<'de>,
        right: &ASTExpression<'de>,
        operator: &ASTBinaryOperatorKind,
    ) -> Type {
        let matrix: (Type, Type, Type) = match operator {
            ASTBinaryOperatorKind::Plus => (Type::Float, Type::Float, Type::Float),
            ASTBinaryOperatorKind::Subtract => (Type::Float, Type::Float, Type::Float),
            ASTBinaryOperatorKind::Multiply => (Type::Float, Type::Float, Type::Float),
            ASTBinaryOperatorKind::Divide => (Type::Float, Type::Float, Type::Float),
            ASTBinaryOperatorKind::Power => (Type::Float, Type::Float, Type::Float),
            ASTBinaryOperatorKind::Equals => (Type::Float, Type::Float, Type::Bool),
            ASTBinaryOperatorKind::NotEquals => (Type::Float, Type::Float, Type::Bool),
            ASTBinaryOperatorKind::LessThan => (Type::Float, Type::Float, Type::Bool),
            ASTBinaryOperatorKind::LessThanOrEqual => (Type::Float, Type::Float, Type::Bool),
            ASTBinaryOperatorKind::GreaterThan => (Type::Float, Type::Float, Type::Bool),
            ASTBinaryOperatorKind::GreaterThanOrEqual => (Type::Float, Type::Float, Type::Bool),
        };
        self.expect_type(&left.span(self.ast), matrix.0, &left.ty);
        self.expect_type(&right.span(self.ast), matrix.1, &right.ty);
        matrix.2
    }

    pub fn resolve_unary_expression(
        &self,
        operand: &ASTExpression<'de>,
        operator: &ASTUnaryOperatorKind,
    ) -> Type {
        let matrix: (Type, Type) = match operator {
            ASTUnaryOperatorKind::Minus => (Type::Float, Type::Float),
        };
        self.expect_type(&operand.span(self.ast), matrix.0, &operand.ty);
        matrix.1
    }

    fn resolve_let_statement(&mut self, let_statement: &ASTLetStatement<'de>) {
        let identifier = let_statement.identifier.span.literal;
        self.visit_expression(&let_statement.initializer);
        let initializer_expression = self.ast.query_expr(&let_statement.initializer);
        let ty = if let Some(type_annotation) = &let_statement.type_annotation {
            let ty = resolve_type_from_string(&self.diagnostics, &type_annotation.type_name);
            self.expect_type(
                &initializer_expression.span(self.ast),
                ty.clone(),
                &initializer_expression.ty,
            );
            ty
        } else {
            initializer_expression.ty.clone()
        };
        self.scopes.declare_variable(identifier, ty);
    }

    fn expect_type(&self, span: &TextSpan<'de>, expected: Type, actual: &Type) {
        expect_type(&self.diagnostics, span, expected, actual);
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
pub struct GlobalSymbolResolver<'a, 'de> {
    diagnostics: DiagnosticsBagCell<'de>,
    global_scope: GlobalScope,
    ast: &'a Ast<'de>,
}

impl<'a, 'de> GlobalSymbolResolver<'a, 'de> {
    pub fn new(diagnostics: DiagnosticsBagCell<'de>, ast: &'a Ast<'de>) -> Self {
        GlobalSymbolResolver {
            ast,
            diagnostics,
            global_scope: GlobalScope::new(),
        }
    }
}

impl<'a, 'de> ASTVisitor<'de> for GlobalSymbolResolver<'a, 'de> {
    fn get_ast(&self) -> &Ast<'de> {
        self.ast
    }

    fn visit_func_decl_statement(&mut self, func_decl_statement: &ASTFuncDeclStatement<'de>) {
        let parameters = func_decl_statement
            .parameters
            .iter()
            .map(|parameter| {
                VariableSymbol::new(parameter.identifier.span.literal.to_string(), Type::Void)
            })
            .collect();
        let literal_span = &func_decl_statement.identifier.span;
        let return_type = match &func_decl_statement.return_type {
            Some(return_type) => {
                resolve_type_from_string(&self.diagnostics, &return_type.type_name)
            }
            None => Type::Void,
        };
        match self.global_scope.declare_function(
            literal_span.literal,
            &func_decl_statement.body,
            parameters,
            return_type,
        ) {
            Ok(_) => {}
            Err(_) => {
                self.diagnostics
                    .borrow_mut()
                    .report_function_already_declared(&func_decl_statement.identifier);
            }
        }
    }

    fn visit_let_statement(&mut self, _let_statement: &ASTLetStatement<'de>) {}

    fn visit_variable_expression(
        &mut self,
        _variable_expression: &ASTVariableExpression,
        _expr: &ASTExpression<'de>,
    ) {
    }

    fn visit_number_expression(
        &mut self,
        _number: &ASTNumberExpression,
        _expr: &ASTExpression<'de>,
    ) {
    }

    fn visit_boolean_expression(
        &mut self,
        _boolean: &ASTBooleanExpression,
        _expr: &ASTExpression<'de>,
    ) {
    }

    fn visit_error(&mut self, _span: &TextSpan) {}

    fn visit_unary_expression(
        &mut self,
        _unary_expression: &ASTUnaryExpression,
        _expr: &ASTExpression<'de>,
    ) {
    }

    fn visit_break_statement(&mut self, _break_statement: &ASTBreakStatement<'de>) {}
}

impl<'a, 'de> ASTVisitor<'de> for Resolver<'a, 'de> {
    fn get_ast(&self) -> &Ast<'de> {
        self.ast
    }

    fn visit_func_decl_statement(&mut self, func_decl_statement: &ASTFuncDeclStatement<'de>) {
        let function_symbol = self
            .scopes
            .lookup_function(func_decl_statement.identifier.span.literal)
            .unwrap()
            .clone();
        self.scopes.enter_scope(Some(function_symbol.clone()));
        for parameter in &function_symbol.parameters {
            self.scopes
                .declare_variable(&parameter.name, parameter.ty.clone());
        }
        self.visit_statement(&func_decl_statement.body);
        self.scopes.exit_scope();
    }

    fn visit_return_statement(&mut self, return_statement: &ASTReturnStatement<'de>) {
        match self.scopes.surrounding_function().cloned() {
            None => {
                let mut diagnostics_binding = self.diagnostics.borrow_mut();
                diagnostics_binding
                    .report_cannot_return_outside_function(&return_statement.return_keyword);
            }
            Some(function) => {
                if let Some(return_expression) = &return_statement.return_value {
                    self.visit_expression(return_expression);
                    let return_expression = self.ast.query_expr(return_expression);
                    self.expect_type(
                        &return_expression.span(self.ast),
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

    fn visit_block_statement(&mut self, block_statement: &ASTBlockStatement) {
        self.scopes.enter_scope(None);
        for statement in &block_statement.statements {
            self.visit_statement(statement);
        }
        self.scopes.exit_scope();
    }

    fn visit_if_statement(&mut self, if_statement: &ASTIfStatement<'de>) {
        self.scopes.enter_scope(None);
        self.visit_expression(&if_statement.condition);
        let condition_expression = self.ast.query_expr(&if_statement.condition);
        self.expect_type(
            &condition_expression.span(self.ast),
            Type::Bool,
            &condition_expression.ty,
        );
        self.visit_statement(&if_statement.then_branch);
        self.scopes.exit_scope();
        if let Some(else_branch) = &if_statement.else_branch {
            self.scopes.enter_scope(None);
            self.visit_statement(&else_branch.else_statement);
            self.scopes.exit_scope();
        }
    }

    fn visit_let_statement(&mut self, let_statement: &ASTLetStatement<'de>) {
        self.resolve_let_statement(let_statement);
    }

    fn visit_call_expression(
        &mut self,
        call_expression: &ASTCallExpression<'de>,
        expr: &ASTExpression<'de>,
    ) {
        let function = self
            .scopes
            .lookup_function(call_expression.identifier.span.literal)
            .cloned();
        let ty = match function {
            None => {
                let mut diagnostics_binding = self.diagnostics.borrow_mut();
                diagnostics_binding.report_undeclared_function(&call_expression.identifier);
                Type::Void
            }
            Some(function) => {
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
                    .zip(function.parameters.iter())
                {
                    self.visit_expression(argument);
                    let argument_expression = self.ast.query_expr(argument);
                    self.expect_type(
                        &argument_expression.span(self.ast),
                        param.ty.clone(),
                        &argument_expression.ty,
                    );
                }
                return_type
            }
        };
        self.ast.set_type(&expr.id, ty);
    }

    fn visit_variable_expression(
        &mut self,
        variable_expression: &ASTVariableExpression<'de>,
        expr: &ASTExpression<'de>,
    ) {
        match self
            .scopes
            .lookup_variable(variable_expression.identifier.span.literal)
        {
            Some(variable) => {
                self.ast.set_type(&expr.id, variable.ty.clone());
            }
            None => {
                let mut diagnostics_binding = self.diagnostics.borrow_mut();
                diagnostics_binding.report_undeclared_variable(&variable_expression.identifier);
            }
        }
    }

    fn visit_number_expression(
        &mut self,
        _number: &ASTNumberExpression,
        expr: &ASTExpression<'de>,
    ) {
        self.ast.set_type(&expr.id, Type::Float);
    }

    fn visit_boolean_expression(
        &mut self,
        _boolean: &ASTBooleanExpression,
        expr: &ASTExpression<'de>,
    ) {
        self.ast.set_type(&expr.id, Type::Bool);
    }

    fn visit_error(&mut self, _span: &TextSpan) {}

    fn visit_unary_expression(
        &mut self,
        unary_expression: &ASTUnaryExpression<'de>,
        expr: &ASTExpression<'de>,
    ) {
        self.visit_expression(&unary_expression.operand);
        let operand = self.ast.query_expr(&unary_expression.operand);
        let ty = self.resolve_unary_expression(operand, &unary_expression.operator.kind);
        self.ast.set_type(&expr.id, ty);
    }

    fn visit_binary_expression(
        &mut self,
        binary_expression: &crate::ASTBinaryExpression<'de>,
        expr: &ASTExpression<'de>,
    ) {
        self.visit_expression(&binary_expression.left);
        self.visit_expression(&binary_expression.right);
        let left = self.ast.query_expr(&binary_expression.left);
        let right = self.ast.query_expr(&binary_expression.right);
        let ty = self.resolve_binary_expression(left, right, &binary_expression.operator.kind);
        self.ast.set_type(&expr.id, ty);
        // TODO: Check if the types are compatible
    }

    fn visit_break_statement(&mut self, break_stmt: &ASTBreakStatement<'de>) {
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

    fn visit_while_statement(&mut self, while_statement: &ASTWhileStatement<'de>) {
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

        self.scopes.enter_scope(None);

        self.visit_expression(&while_statement.condition);
        let condition_expression = self.ast.query_expr(&while_statement.condition);
        self.expect_type(
            &condition_expression.span(self.ast),
            Type::Bool,
            &condition_expression.ty,
        );
        self.visit_statement(&while_statement.body);

        self.scopes.exit_scope();

        // Pop the loop from the Loops stack
        self.loops.pop();
    }

    fn visit_parenthesized_expression(
        &mut self,
        parenthesized_expression: &crate::ast::ASTParenthesizedExpression,
        expr: &ASTExpression<'de>,
    ) {
        self.visit_expression(&parenthesized_expression.expression);
        let expression = self.ast.query_expr(&parenthesized_expression.expression);
        self.ast.set_type(&expr.id, expression.ty.clone());
    }

    fn visit_assignment_expression(
        &mut self,
        assignment_expression: &crate::ASTAssignmentExpression<'de>,
        expr: &ASTExpression<'de>,
    ) {
        self.visit_expression(&assignment_expression.expression);
        let value_expression = self.ast.query_expr(&assignment_expression.expression);
        let identifier = &assignment_expression.identifier.span.literal;
        let ty = match self.scopes.lookup_variable(identifier) {
            None => {
                let mut diagnostics_binding = self.diagnostics.borrow_mut();
                diagnostics_binding.report_undeclared_variable(&assignment_expression.identifier);
                Type::Void
            }
            Some(variable) => {
                self.expect_type(
                    &value_expression.span(self.ast),
                    variable.ty.clone(),
                    &value_expression.ty,
                );
                variable.ty.clone()
            }
        };
        self.ast.set_type(&expr.id, ty);
    }
}
