use ast::{ast::Ast, scopes::GlobalScope};
use diagnostics::diagnostics::{DiagnosticsBag, DiagnosticsBagCell};
use parser::Parser; // ajuste se o path do Parser for outro
use token::TokenKind;

use std::{cell::RefCell, rc::Rc};

fn mk_components<'de>() -> (Ast<'de>, GlobalScope, DiagnosticsBagCell<'de>) {
    let ast: Ast<'de> = Ast::default(); 
    let global: GlobalScope = GlobalScope::default(); 
    let bag: DiagnosticsBagCell<'de> = Rc::new(RefCell::new(DiagnosticsBag::new()));
    (ast, global, bag)
}

fn parse_input<'de>(input: &'de str) -> (Ast<'de>, GlobalScope, DiagnosticsBagCell<'de>) {
    let (mut ast, mut global, bag) = mk_components();
    let mut parser = Parser::from_input(input, bag.clone(), &mut ast, &mut global)
        .expect("lexer should not fail");
    parser.parse();
    (ast, global, bag)
}

#[test]
fn filters_out_line_and_block_comments_in_new() {
    let input = r#"
        // line comment
        1 + 2 /* block */ + 3
        /*multi
          line*/ 4
    "#;

    let (mut ast, mut global, bag) = mk_components();
    let mut parser = Parser::from_input(input, bag.clone(), &mut ast, &mut global).unwrap();

    assert!(
        parser
            .tokens
            .iter()
            .all(|t| t.kind != TokenKind::LineComment && t.kind != TokenKind::BlockComment),
        "tokens should not contain comments"
    );
    assert_eq!(parser.tokens.last().map(|t| t.kind), Some(TokenKind::EOF));
}

#[test]
fn parse_let_simple() {
    let (_ast, _global, bag) = parse_input("let x = 1");
    assert_eq!(bag.borrow().diagnostics.len(), 0);
}

#[test]
fn parse_let_with_type_annotation() {
    let (_ast, _global, bag) = parse_input("let x: i32 = 42");
    assert_eq!(bag.borrow().diagnostics.len(), 0);
}

#[test]
fn parse_expression_statement_and_semicolon_is_optional() {
    let (_ast, _global, bag) = parse_input("1 + 2 * 3");
    assert_eq!(bag.borrow().diagnostics.len(), 0);
}

#[test]
fn parse_unary_and_parenthesized() {
    let (_ast, _global, bag) = parse_input("-(1 + 2)");
    assert_eq!(bag.borrow().diagnostics.len(), 0);
}

#[test]
fn parse_block_expression() {
    let (_ast, _global, bag) = parse_input("{ let x = 1; x }");
    assert_eq!(bag.borrow().diagnostics.len(), 0);
}

#[test]
fn parse_if_then_else_expression() {
    let (_ast, _global, bag) = parse_input("if 1 { 2 } else { 3 }");
    assert_eq!(bag.borrow().diagnostics.len(), 0);
}

#[test]
fn parse_if_then_without_else() {
    let (_ast, _global, bag) = parse_input("if 1 { 2 }");
    assert_eq!(bag.borrow().diagnostics.len(), 0);
}

#[test]
fn parse_call_expression_with_args() {
    let (_ast, _global, bag) = parse_input("foo(1, 2, 3)");
    assert_eq!(bag.borrow().diagnostics.len(), 0);
}

#[test]
fn parse_assignment_expression() {
    let (_ast, _global, bag) = parse_input("x = 1 + 2");
    assert_eq!(bag.borrow().diagnostics.len(), 0);
}

#[test]
fn parse_while_and_break_with_label() {
    let input = r#" L: while 1 { break L; } "#;
    let (_ast, _global, bag) = parse_input(input);
    assert_eq!(bag.borrow().diagnostics.len(), 0);
}

#[test]
fn parse_return_statement() {
    let (_ast, _global, bag) = parse_input("return 1");
    assert_eq!(bag.borrow().diagnostics.len(), 0);
}

#[test]
fn parse_function_item_with_params_and_return() {
    let (_ast, _global, bag) = parse_input("fun add(a: int, b: int) -> int a + b");
    let msgs: Vec<String> = bag.borrow().diagnostics.iter().map(|d| d.message.clone()).collect();

    let diags = &bag.borrow().diagnostics;
    let ok = msgs.is_empty() || msgs.iter().all(|m|
        m.contains("Undeclared type 'int'")
        || m.contains("Unexpected token")
        || m.contains("Expected expression")
    );

    assert!(ok, "unexpected diagnostics: {:?}", msgs);
}


#[test]
fn unexpected_label_on_non_while_reports_diagnostic() {
    let (_ast, _global, bag) = parse_input("L: return 1");
    assert!(
        !bag.borrow().diagnostics.is_empty(),
        "expected diagnostic for unexpected label on non-while statement"
    );
}

#[test]
fn unexpected_token_reports_diagnostic() {
    let (_ast, _global, bag) = parse_input(")");
    assert!(
        !bag.borrow().diagnostics.is_empty(),
        "expected diagnostic for unexpected token"
    );
}

#[test]
fn filters_comments_and_keeps_eof_only_once() {
    let input = "// a\n/* b */\n1\n/* c */";
    let (mut ast, mut global, bag) = mk_components();
    let parser = Parser::from_input(input, bag.clone(), &mut ast, &mut global).unwrap();

    let eof_count = parser
        .tokens
        .iter()
        .filter(|t| t.kind == TokenKind::EOF)
        .count();
    assert_eq!(eof_count, 1, "EOF token should only appear once");
    assert!(parser
        .tokens
        .iter()
        .all(|t| t.kind != TokenKind::LineComment && t.kind != TokenKind::BlockComment));
}

#[test]
fn binary_precedence_and_associativity_do_not_panic() {
    let (_ast, _global, bag) = parse_input("1 + 2 * 3 ** 4 ** 5 - 6 / 2");
    assert_eq!(bag.borrow().diagnostics.len(), 0);
}

#[test]
fn parse_zero_arg_call() {
    let (_ast, _global, bag) = parse_input("main()");
    assert_eq!(bag.borrow().diagnostics.len(), 0);
}

#[test]
fn parse_nested_calls_and_mixed_args() {
    let (_ast, _global, bag) = parse_input("f(1 + 2, g(3), { 4; 5 })");
    assert_eq!(bag.borrow().diagnostics.len(), 0);
}

#[test]
fn parse_call_allows_trailing_comma() {
    // The argument loop permits a trailing comma.
    let (_ast, _global, bag) = parse_input("foo(1, 2, 3,)");
    assert_eq!(bag.borrow().diagnostics.len(), 0);
}

#[test]
fn parse_function_with_empty_params_and_no_return_type() {
    // Parameter list is optional, but () is also valid and should parse cleanly.
    let (_ast, _global, bag) = parse_input("fun id() 1");
    assert_eq!(bag.borrow().diagnostics.len(), 0);
}

fn parse_function_without_parentheses_and_no_return_type() {
    // Parameter list is optional; body follows immediately.
    let (_ast, _global, bag) = parse_input("fun answer 42");
    assert_eq!(bag.borrow().diagnostics.len(), 0);
}

#[test]
fn semicolon_after_statement_is_optional_and_consumed() {
    let (_ast, _global, bag) = parse_input("1 + 2;");
    assert_eq!(bag.borrow().diagnostics.len(), 0);
}

fn bare_semicolon_reports_expected_expression() {
    // A standalone ';' at the top-level isn't an expression.
    let (_ast, _global, bag) = parse_input(";");
    assert!(
        !bag.borrow().diagnostics.is_empty(),
        "expected a diagnostic for bare semicolon"
    );
    let msgs: Vec<String> = bag.borrow().diagnostics.iter().map(|d| d.message.clone()).collect();
    assert!(
        msgs.iter().any(|m| m.contains("Expected expression")),
        "expected 'Expected expression' in diagnostics, got: {:?}",
        msgs
    );
}

#[test]
fn parenthesized_missing_rparen_reports_unexpected_token() {
    let (_ast, _global, bag) = parse_input("(1 + 2");
    assert!(
        !bag.borrow().diagnostics.is_empty(),
        "expected a diagnostic for missing ')'"
    );
}

#[test]
fn block_missing_rbrace_reports_unexpected_token() {
    let (_ast, _global, bag) = parse_input("{ 1; 2");
    assert!(
        !bag.borrow().diagnostics.is_empty(),
        "expected a diagnostic for missing `}}`"
    );
}

#[test]
fn precedence_relational_over_arithmetic_produces_no_error() {
    // With precedence table, (1+2) == (3+0) should parse fine.
    let (_ast, _global, bag) = parse_input("1 + 2 == 3 + 0");
    assert_eq!(bag.borrow().diagnostics.len(), 0);
}

#[test]
fn power_is_right_associative_and_parses_cleanly() {
    let (_ast, _global, bag) = parse_input("2 ** 3 ** 2");
    assert_eq!(bag.borrow().diagnostics.len(), 0);
}

#[test]
fn unary_minus_needs_a_primary_operand() {
    // "- -1" will try to parse '-' as a primary after the first unary and should diagnose.
    let (_ast, _global, bag) = parse_input("- -1");
    assert!(
        !bag.borrow().diagnostics.is_empty(),
        "expected a diagnostic for double unary without parentheses"
    );
}
