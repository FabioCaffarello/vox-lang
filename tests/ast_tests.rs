use ast::ast::{
    ASTAssignmentExpression, ASTBinaryExpression, ASTBlockStatement, ASTBooleanExpression,
    ASTCallExpression, ASTFuncDeclStatement, ASTIfStatement, ASTLetStatement, ASTNumberExpression,
    ASTParenthesizedExpression, ASTReturnStatement, ASTUnaryExpression, ASTVariableExpression,
    ASTWhileStatement, Ast,
};
use ast::visitor::ASTVisitor;
use compiler::compilation_unit::CompilationUnit;
use text::span::TextSpan;

#[derive(Debug, PartialEq)]
enum TestASTNode {
    Number(f64),
    Boolean(bool),
    Binary,
    Unary,
    Parenthesized,
    LetStmt,
    Assignment,
    Block,
    Variable(String),
    If,
    Else,
    Func,
    While,
    Return,
    Call,
    Break,
}

#[derive(Debug)]
struct ASTVerifier<'de> {
    expected: Vec<TestASTNode>,
    actual: Vec<TestASTNode>,
    ast: Ast<'de>,
}

fn assert_tree(input: &str, expected: Vec<TestASTNode>) {
    let verifier = ASTVerifier::new(input, expected);
    verifier.verify();
}

impl<'de> ASTVerifier<'de> {
    pub fn new(input: &'de str, expected: Vec<TestASTNode>) -> Self {
        let compilation_unit = match CompilationUnit::compile(input) {
            Ok(unit) => {
                assert!(
                    unit.diagnostics_bag.borrow().diagnostics.is_empty(),
                    "Compilation failed with diagnostics: {:?}",
                    unit.diagnostics_bag.borrow().diagnostics
                );
                unit
            }
            Err(diagnostics) => {
                panic!("Compilation failed with diagnostics: {:?}", diagnostics);
            }
        };
        let mut verifier = ASTVerifier {
            expected,
            actual: Vec::new(),
            ast: compilation_unit.ast,
        };
        verifier.flatten_ast();
        verifier
    }

    fn flatten_ast(&mut self) {
        self.actual.clear();
        let ast = self.ast.clone();
        ast.visit(&mut *self);
    }

    pub fn verify(&self) {
        assert_eq!(
            self.expected.len(),
            self.actual.len(),
            "Expected {} nodes, but got {}. Actual nodes: {:?}",
            self.expected.len(),
            self.actual.len(),
            self.actual
        );

        for (index, (expected, actual)) in self.expected.iter().zip(self.actual.iter()).enumerate()
        {
            assert_eq!(
                expected, actual,
                "Expected {:?} at index {}, but got {:?}",
                expected, index, actual
            );
        }
    }
}

impl<'de> ASTVisitor<'de> for ASTVerifier<'de> {
    fn visit_number_expression(&mut self, number: &ASTNumberExpression) {
        self.actual.push(TestASTNode::Number(number.number));
    }

    fn visit_let_statement(&mut self, let_statement: &ASTLetStatement<'de>) {
        self.actual.push(TestASTNode::LetStmt);
        self.visit_expression(&let_statement.initializer);
    }

    fn visit_variable_expression(&mut self, _variable_expression: &ASTVariableExpression<'de>) {
        self.actual.push(TestASTNode::Variable(
            _variable_expression.identifier.to_string(),
        ));
    }

    fn visit_parenthesized_expression(
        &mut self,
        parenthesized_expression: &ASTParenthesizedExpression<'de>,
    ) {
        self.actual.push(TestASTNode::Parenthesized);
        self.visit_expression(&parenthesized_expression.expression);
    }

    fn visit_binary_expression(&mut self, binary_expression: &ASTBinaryExpression<'de>) {
        self.actual.push(TestASTNode::Binary);
        self.visit_expression(&binary_expression.left);
        self.visit_expression(&binary_expression.right);
    }

    fn visit_unary_expression(&mut self, unary_expression: &ASTUnaryExpression<'de>) {
        self.actual.push(TestASTNode::Unary);
        self.visit_expression(&unary_expression.operand);
    }

    fn visit_if_statement(&mut self, if_statement: &ASTIfStatement<'de>) {
        self.actual.push(TestASTNode::If);
        self.visit_expression(&if_statement.condition);
        self.visit_statement(&if_statement.then_branch);
        if let Some(else_branch) = &if_statement.else_branch {
            self.actual.push(TestASTNode::Else);
            self.visit_statement(&else_branch.else_statement);
        }
    }

    fn visit_block_statement(&mut self, block_statement: &ASTBlockStatement<'de>) {
        self.actual.push(TestASTNode::Block);
        for statement in &block_statement.statements {
            self.visit_statement(statement);
        }
    }

    fn visit_assignment_expression(
        &mut self,
        assignment_expression: &ASTAssignmentExpression<'de>,
    ) {
        self.actual.push(TestASTNode::Assignment);
        self.visit_expression(&assignment_expression.expression);
    }

    fn visit_func_decl_statement(&mut self, func_decl_statement: &ASTFuncDeclStatement<'de>) {
        self.actual.push(TestASTNode::Func);
        self.visit_statement(&func_decl_statement.body);
    }

    fn visit_return_statement(&mut self, return_statement: &ASTReturnStatement<'de>) {
        self.actual.push(TestASTNode::Return);
        if let Some(expression) = &return_statement.return_value {
            self.visit_expression(expression);
        }
    }

    fn visit_break_statement(&mut self, _break_statement: &ast::ASTBreakStatement<'de>) {
        self.actual.push(TestASTNode::Break);
    }

    fn visit_while_statement(&mut self, while_statement: &ASTWhileStatement<'de>) {
        self.actual.push(TestASTNode::While);
        self.visit_expression(&while_statement.condition);
        self.visit_statement(&while_statement.body);
    }

    fn visit_boolean_expression(&mut self, boolean: &ASTBooleanExpression) {
        self.actual.push(TestASTNode::Boolean(boolean.value));
    }

    fn visit_call_expression(&mut self, call_expression: &ASTCallExpression<'de>) {
        self.actual.push(TestASTNode::Call);
        for argument in &call_expression.arguments {
            self.visit_expression(argument);
        }
    }

    fn visit_error(&mut self, _span: &TextSpan) {
        // do nothing
    }
}

#[test]
fn should_parse_basic_binary_expression() {
    let input = "let x = 1 + 2";
    let expected = vec![
        TestASTNode::LetStmt,
        TestASTNode::Binary,
        TestASTNode::Number(1.0),
        TestASTNode::Number(2.0),
    ];
    assert_tree(input, expected);
}

#[test]
fn should_parse_parenthesized_expression() {
    let input = "let x = (1 + 2) * 3";
    let expected = vec![
        TestASTNode::LetStmt,
        TestASTNode::Binary,
        TestASTNode::Parenthesized,
        TestASTNode::Binary,
        TestASTNode::Number(1.0),
        TestASTNode::Number(2.0),
        TestASTNode::Number(3.0),
    ];
    assert_tree(input, expected);
}

#[test]
fn should_parse_nested_parenthesized_expression() {
    let input = "let x = ((1 + 2) * 3) / 4";
    let expected = vec![
        TestASTNode::LetStmt,
        TestASTNode::Binary,
        TestASTNode::Parenthesized,
        TestASTNode::Binary,
        TestASTNode::Parenthesized,
        TestASTNode::Binary,
        TestASTNode::Number(1.0),
        TestASTNode::Number(2.0),
        TestASTNode::Number(3.0),
        TestASTNode::Number(4.0),
    ];
    assert_tree(input, expected);
}

#[test]
fn should_parse_variable_expression() {
    let input = "let x = 10 let y = 5 let z = x + y";
    let expected = vec![
        TestASTNode::LetStmt,
        TestASTNode::Number(10.0),
        TestASTNode::LetStmt,
        TestASTNode::Number(5.0),
        TestASTNode::LetStmt,
        TestASTNode::Binary,
        TestASTNode::Variable("x".to_string()),
        TestASTNode::Variable("y".to_string()),
    ];
    assert_tree(input, expected);
}

#[test]
fn should_parse_variable_expression_with_parentheses() {
    let input = "let x = 10 let y = 5 let z = (x + y) * 2";
    let expected = vec![
        TestASTNode::LetStmt,
        TestASTNode::Number(10.0),
        TestASTNode::LetStmt,
        TestASTNode::Number(5.0),
        TestASTNode::LetStmt,
        TestASTNode::Binary,
        TestASTNode::Parenthesized,
        TestASTNode::Binary,
        TestASTNode::Variable("x".to_string()),
        TestASTNode::Variable("y".to_string()),
        TestASTNode::Number(2.0),
    ];
    assert_tree(input, expected);
}

#[test]
fn should_parse_variable_expression_with_nested_parentheses() {
    let input = "let x = 10 let y = 5 let z = ((x + y) * 2) / 3";
    let expected = vec![
        TestASTNode::LetStmt,
        TestASTNode::Number(10.0),
        TestASTNode::LetStmt,
        TestASTNode::Number(5.0),
        TestASTNode::LetStmt,
        TestASTNode::Binary,
        TestASTNode::Parenthesized,
        TestASTNode::Binary,
        TestASTNode::Parenthesized,
        TestASTNode::Binary,
        TestASTNode::Variable("x".to_string()),
        TestASTNode::Variable("y".to_string()),
        TestASTNode::Number(2.0),
        TestASTNode::Number(3.0),
    ];
    assert_tree(input, expected);
}

#[test]
fn should_parse_variable_expression_with_multiple_nested_parentheses() {
    let input = "let x = 10 let y = 5 let z = (((x + y) * 2) / 3) + 4";
    let expected = vec![
        TestASTNode::LetStmt,
        TestASTNode::Number(10.0),
        TestASTNode::LetStmt,
        TestASTNode::Number(5.0),
        TestASTNode::LetStmt,
        TestASTNode::Binary,
        TestASTNode::Parenthesized,
        TestASTNode::Binary,
        TestASTNode::Parenthesized,
        TestASTNode::Binary,
        TestASTNode::Parenthesized,
        TestASTNode::Binary,
        TestASTNode::Variable("x".to_string()),
        TestASTNode::Variable("y".to_string()),
        TestASTNode::Number(2.0),
        TestASTNode::Number(3.0),
        TestASTNode::Number(4.0),
    ];
    assert_tree(input, expected);
}

#[test]
fn should_parse_negation() {
    let input = "let x = -1";
    let expected = vec![
        TestASTNode::LetStmt,
        TestASTNode::Unary,
        TestASTNode::Number(1.0),
    ];
    assert_tree(input, expected);
}

#[test]
fn should_parse_negation_in_expression_with_unary() {
    let input = "let z = 2 let x = -1 let w = (2 * z) * -x";
    let expected = vec![
        TestASTNode::LetStmt,
        TestASTNode::Number(2.0),
        TestASTNode::LetStmt,
        TestASTNode::Unary,
        TestASTNode::Number(1.0),
        TestASTNode::LetStmt,
        TestASTNode::Binary,
        TestASTNode::Parenthesized,
        TestASTNode::Binary,
        TestASTNode::Number(2.0),
        TestASTNode::Variable("z".to_string()),
        TestASTNode::Unary,
        TestASTNode::Variable("x".to_string()),
    ];
    assert_tree(input, expected);
}

#[test]
fn should_parse_negation_in_expression() {
    let input = "let x = -1 * 2";
    let expected = vec![
        TestASTNode::LetStmt,
        TestASTNode::Binary,
        TestASTNode::Unary,
        TestASTNode::Number(1.0),
        TestASTNode::Number(2.0),
    ];
    assert_tree(input, expected);
}

#[test]
pub fn should_parse_if_statement() {
    let input = "\
    let a = 1
    if a > 0 {
        a = 20
    }
    ";
    let expected = vec![
        TestASTNode::LetStmt,
        TestASTNode::Number(1.0),
        TestASTNode::If,
        TestASTNode::Binary,
        TestASTNode::Variable("a".to_string()),
        TestASTNode::Number(0.0),
        TestASTNode::Block,
        TestASTNode::Assignment,
        TestASTNode::Number(20.0),
    ];
    assert_tree(input, expected);
}

#[test]
pub fn should_parse_if_statement_with_else() {
    let input = "\
    let a = 1
    if a > 0 {
        a = 20
    } else {
        a = 30
    }
    ";
    let expected = vec![
        TestASTNode::LetStmt,
        TestASTNode::Number(1.0),
        TestASTNode::If,
        TestASTNode::Binary,
        TestASTNode::Variable("a".to_string()),
        TestASTNode::Number(0.0),
        TestASTNode::Block,
        TestASTNode::Assignment,
        TestASTNode::Number(20.0),
        TestASTNode::Else,
        TestASTNode::Block,
        TestASTNode::Assignment,
        TestASTNode::Number(30.0),
    ];
    assert_tree(input, expected);
}

#[test]
pub fn should_parse_while_statement() {
    let input = "\
        let a = 1
        while a < 10 {
            a = a + 1
        }
        ";
    let expected = vec![
        TestASTNode::LetStmt,
        TestASTNode::Number(1.0),
        TestASTNode::While,
        TestASTNode::Binary,
        TestASTNode::Variable("a".to_string()),
        TestASTNode::Number(10.0),
        TestASTNode::Block,
        TestASTNode::Assignment,
        TestASTNode::Binary,
        TestASTNode::Variable("a".to_string()),
        TestASTNode::Number(1.0),
    ];

    assert_tree(input, expected);
}

#[test]
pub fn should_parse_break_statement() {
    let input = "\
        let a = 1
        while a < 10 {
            a = a + 1
            break
        }
        ";
    let expected = vec![
        TestASTNode::LetStmt,
        TestASTNode::Number(1.0),
        TestASTNode::While,
        TestASTNode::Binary,
        TestASTNode::Variable("a".to_string()),
        TestASTNode::Number(10.0),
        TestASTNode::Block,
        TestASTNode::Assignment,
        TestASTNode::Binary,
        TestASTNode::Variable("a".to_string()),
        TestASTNode::Number(1.0),
        TestASTNode::Break,
    ];

    assert_tree(input, expected);
}

#[test]
pub fn should_parse_function_declaration() {
    let input = "\
        func add(a, b) {
            return a + b
        }
        ";
    let expected = vec![
        TestASTNode::Func,
        TestASTNode::Block,
        TestASTNode::Return,
        TestASTNode::Binary,
        TestASTNode::Variable("a".to_string()),
        TestASTNode::Variable("b".to_string()),
    ];

    assert_tree(input, expected);
}

#[test]
pub fn should_parse_call_expression() {
    let input = "\
        func add(a, b) {
            return a + b
        }
        add(2 * 3, 4 + 5)";
    let expected = vec![
        TestASTNode::Func,
        TestASTNode::Block,
        TestASTNode::Return,
        TestASTNode::Binary,
        TestASTNode::Variable("a".to_string()),
        TestASTNode::Variable("b".to_string()),
        TestASTNode::Call,
        TestASTNode::Binary,
        TestASTNode::Number(2.0),
        TestASTNode::Number(3.0),
        TestASTNode::Binary,
        TestASTNode::Number(4.0),
        TestASTNode::Number(5.0),
    ];

    assert_tree(input, expected);
}
