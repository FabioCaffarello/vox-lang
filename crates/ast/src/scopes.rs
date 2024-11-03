use crate::{
    ast::{
        ASTBlockStatement, ASTBooleanExpression, ASTBreakStatement, ASTCallExpression,
        ASTFuncDeclStatement, ASTIfStatement, ASTLetStatement, ASTNumberExpression, ASTStatement,
        ASTUnaryExpression, ASTVariableExpression, ASTWhileStatement,
    },
    loops::Loops,
    visitor::ASTVisitor,
};
use diagnostics::diagnostics::DiagnosticsBagCell;
use std::collections::HashMap;
use text::span::TextSpan;

#[derive(Debug)]
pub struct GlobalScope<'de> {
    pub variables: HashMap<String, ()>,
    pub functions: HashMap<String, FunctionSymbol<'de>>,
}

impl<'de> Default for GlobalScope<'de> {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug)]
pub struct FunctionSymbol<'de> {
    pub parameters: Vec<String>,
    pub body: ASTStatement<'de>,
}

impl<'de> GlobalScope<'de> {
    pub fn new() -> Self {
        GlobalScope {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    fn declare_variable(&mut self, identifier: &str) {
        self.variables.insert(identifier.to_string(), ());
    }

    fn lookup_variable(&self, identifier: &str) -> bool {
        self.variables.contains_key(identifier)
    }

    #[allow(clippy::result_unit_err)]
    pub fn declare_function(
        &mut self,
        identifier: &str,
        function: &ASTStatement<'de>,
        parameters: Vec<String>,
    ) -> Result<(), ()> {
        if self.functions.contains_key(identifier) {
            return Err(());
        }
        let function = FunctionSymbol {
            parameters,
            body: function.clone(),
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
    variables: HashMap<String, ()>,
}

impl LocalScope {
    fn new() -> Self {
        LocalScope {
            variables: HashMap::new(),
        }
    }

    fn declare_variable(&mut self, identifier: &str) {
        self.variables.insert(identifier.to_string(), ());
    }

    fn lookup_variable(&self, identifier: &str) -> bool {
        self.variables.contains_key(identifier)
    }
}

#[derive(Debug)]
pub struct Scopes<'de> {
    local_scopes: Vec<LocalScope>,
    pub global_scope: GlobalScope<'de>,
}

impl<'de> Default for Scopes<'de> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'de> Scopes<'de> {
    pub fn new() -> Self {
        Scopes {
            local_scopes: Vec::new(),
            global_scope: GlobalScope::new(),
        }
    }

    pub fn from_global_scope(global_scope: GlobalScope<'de>) -> Self {
        Scopes {
            local_scopes: Vec::new(),
            global_scope,
        }
    }

    fn enter_scope(&mut self) {
        self.local_scopes.push(LocalScope::new());
    }

    fn exit_scope(&mut self) {
        self.local_scopes.pop();
    }

    fn declare_variable(&mut self, identifier: &str) {
        if self.is_inside_local_scope() {
            self.local_scopes
                .last_mut()
                .unwrap()
                .declare_variable(identifier);
        } else {
            self.global_scope.declare_variable(identifier);
        }
    }

    fn lookup_variable(&self, identifier: &str) -> bool {
        let inside_of_local_scope = self
            .local_scopes
            .iter()
            .rev()
            .any(|scope| scope.lookup_variable(identifier));
        if inside_of_local_scope {
            return true;
        }
        self.global_scope.lookup_variable(identifier)
    }

    fn lookup_function(&self, identifier: &str) -> Option<&FunctionSymbol> {
        self.global_scope.lookup_function(identifier)
    }

    fn is_inside_local_scope(&self) -> bool {
        !self.local_scopes.is_empty()
    }
}

#[derive(Debug)]
pub struct Resolver<'de> {
    pub scopes: Scopes<'de>,
    diagnostics: DiagnosticsBagCell<'de>,
    loops: Loops,
}

impl<'de> Resolver<'de> {
    pub fn new(diagnostics: DiagnosticsBagCell<'de>, scopes: Scopes<'de>) -> Self {
        Resolver {
            scopes,
            diagnostics,
            loops: Loops::new(),
        }
    }
}

#[derive(Debug)]
pub struct GlobalSymbolResolver<'de> {
    diagnostics: DiagnosticsBagCell<'de>,
    global_scope: GlobalScope<'de>,
}

impl<'de> GlobalSymbolResolver<'de> {
    pub fn new(diagnostics: DiagnosticsBagCell<'de>) -> Self {
        GlobalSymbolResolver {
            diagnostics,
            global_scope: GlobalScope::new(),
        }
    }
}

impl<'de> ASTVisitor<'de> for GlobalSymbolResolver<'de> {
    fn visit_func_decl_statement(&mut self, func_decl_statement: &ASTFuncDeclStatement<'de>) {
        let parameters = func_decl_statement
            .parameters
            .iter()
            .map(|parameter| parameter.identifier.span.literal.to_string())
            .collect();
        let literal_span = &func_decl_statement.identifier.span;
        match self.global_scope.declare_function(
            literal_span.literal,
            &func_decl_statement.body,
            parameters,
        ) {
            Ok(_) => {}
            Err(_) => {
                self.diagnostics
                    .borrow_mut()
                    .report_function_already_declared(&func_decl_statement.identifier);
            }
        }
    }

    fn visit_let_statement(&mut self, _let_statement: &ASTLetStatement) {}

    fn visit_variable_expression(&mut self, _variable_expression: &ASTVariableExpression) {}

    fn visit_number_expression(&mut self, _number: &ASTNumberExpression) {}

    fn visit_boolean_expression(&mut self, _boolean: &ASTBooleanExpression) {}

    fn visit_error(&mut self, _span: &TextSpan) {}

    fn visit_unary_expression(&mut self, _unary_expression: &ASTUnaryExpression) {}

    fn visit_break_statement(&mut self, _break_statement: &ASTBreakStatement<'de>) {}
}

impl<'de> ASTVisitor<'de> for Resolver<'de> {
    fn visit_func_decl_statement(&mut self, func_decl_statement: &ASTFuncDeclStatement<'de>) {
        self.scopes.enter_scope();
        for parameter in &func_decl_statement.parameters {
            self.scopes
                .declare_variable(parameter.identifier.span.literal);
        }
        self.visit_statement(&func_decl_statement.body);
        self.scopes.exit_scope();
    }

    fn visit_block_statement(&mut self, block_statement: &ASTBlockStatement<'de>) {
        self.scopes.enter_scope();
        for statement in &block_statement.statements {
            self.visit_statement(statement);
        }
        self.scopes.exit_scope();
    }

    fn visit_if_statement(&mut self, if_statement: &ASTIfStatement<'de>) {
        self.scopes.enter_scope();
        self.visit_expression(&if_statement.condition);
        self.visit_statement(&if_statement.then_branch);
        self.scopes.exit_scope();
        if let Some(else_branch) = &if_statement.else_branch {
            self.scopes.enter_scope();
            self.visit_statement(&else_branch.else_statement);
            self.scopes.exit_scope();
        }
    }

    fn visit_let_statement(&mut self, let_statement: &ASTLetStatement<'de>) {
        let identifier = let_statement.identifier.span.literal;
        self.visit_expression(&let_statement.initializer);
        self.scopes.declare_variable(identifier);
    }

    fn visit_call_expression(&mut self, call_expression: &ASTCallExpression<'de>) {
        let function = self
            .scopes
            .lookup_function(call_expression.identifier.span.literal);
        match function {
            None => {
                let mut diagnostics_binding = self.diagnostics.borrow_mut();
                diagnostics_binding.report_undeclared_function(&call_expression.identifier);
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
            }
        }
        for argument in &call_expression.arguments {
            self.visit_expression(argument);
        }
    }

    fn visit_variable_expression(&mut self, variable_expression: &ASTVariableExpression<'de>) {
        if !self
            .scopes
            .lookup_variable(variable_expression.identifier.span.literal)
        {
            let mut diagnostics_binding = self.diagnostics.borrow_mut();
            diagnostics_binding.report_undeclared_variable(&variable_expression.identifier);
        }
    }

    fn visit_number_expression(&mut self, _number: &ASTNumberExpression) {}

    fn visit_boolean_expression(&mut self, _boolean: &ASTBooleanExpression) {}

    fn visit_error(&mut self, _span: &TextSpan) {}

    fn visit_unary_expression(&mut self, unary_expression: &ASTUnaryExpression<'de>) {
        self.visit_expression(&unary_expression.operand);
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

        // Attempt to push the loop onto the Loops stack
        if self.loops.push(label.clone()).is_err() {
            // Report duplicate label error
            self.diagnostics
                .borrow_mut()
                .report_duplicate_label(while_statement.label.as_ref().unwrap());
        }

        // Enter a new scope for the loop body
        self.scopes.enter_scope();

        // Visit the loop condition
        self.visit_expression(&while_statement.condition);

        // Visit the loop body
        self.visit_statement(&while_statement.body);

        // Exit the loop body scope
        self.scopes.exit_scope();

        // Pop the loop from the Loops stack
        self.loops.pop();
    }
}
