use ast::ast::{
    ASTBinaryExpression, ASTLetStatement, ASTNumberExpression, ASTParenthesizedExpression,
    ASTUnaryExpression, ASTVariableExpression, Ast,
};
use ast::visitor::ASTVisitor;
use compiler::compilation_unit::CompilationUnit;
use text::span::TextSpan;

#[derive(Debug, PartialEq)]
enum TestASTNode {
    Number(f64),
    Binary,
    Unary,
    Parenthesized,
    LetStmt,
    Variable(String),
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
    fn visit_let_statement(&mut self, let_statement: &ASTLetStatement<'de>) {
        self.actual.push(TestASTNode::LetStmt);
        self.visit_expression(&let_statement.initializer);
    }

    fn visit_variable_expression(&mut self, _variable_expression: &ASTVariableExpression<'de>) {
        self.actual.push(TestASTNode::Variable(
            _variable_expression.identifier.to_string(),
        ));
    }

    fn visit_number_expression(&mut self, number: &ASTNumberExpression) {
        self.actual.push(TestASTNode::Number(number.number));
    }

    fn visit_error(&mut self, _span: &TextSpan) {
        // do nothing
    }

    fn visit_parenthesized_expression(
        &mut self,
        parenthesized_expression: &ASTParenthesizedExpression<'de>,
    ) {
        self.actual.push(TestASTNode::Parenthesized);
        self.visit_expression(&parenthesized_expression.expression);
    }

    fn visit_unary_expression(&mut self, unary_expression: &ASTUnaryExpression<'de>) {
        self.actual.push(TestASTNode::Unary);
        self.visit_expression(&unary_expression.operand);
    }

    fn visit_binary_expression(&mut self, binary_expression: &ASTBinaryExpression<'de>) {
        self.actual.push(TestASTNode::Binary);
        self.visit_expression(&binary_expression.left);
        self.visit_expression(&binary_expression.right);
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
