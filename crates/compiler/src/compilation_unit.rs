use std::{cell::RefCell, rc::Rc};

use ast::{ast::Ast, evaluator::ASTEvaluator, validator::SymbolChecker};
use diagnostics::{
    diagnostics::{DiagnosticsBag, DiagnosticsBagCell},
    printer::DiagnosticPrinter,
};
use parser::Parser;
use text::source::SourceText;

#[derive(Debug)]
pub struct CompilationUnit<'de> {
    pub ast: Ast<'de>,
    _diagnostics_bag: DiagnosticsBagCell<'de>,
}

impl<'de> CompilationUnit<'de> {
    pub fn compile(input: &'de str) -> Result<CompilationUnit<'de>, DiagnosticsBag<'de>> {
        let source_text = SourceText::new(input.to_string());
        let diagnostics_bag = DiagnosticsBagCell::new(RefCell::new(DiagnosticsBag::new()));
        let diagnostics_rc = Rc::clone(&diagnostics_bag);

        let mut parser = Parser::from_input(input, diagnostics_rc.clone())
            .map_err(|_| diagnostics_bag.borrow().clone())?;

        let mut ast = Ast::new();
        while let Some(statement) = parser.next_statement() {
            ast.add_statement(statement);
        }

        ast.visualize();
        Self::check_diagnostics(&source_text, &diagnostics_bag)?;
        let mut symbol_checker = SymbolChecker::new(diagnostics_rc.clone());
        ast.visit(&mut symbol_checker);
        Self::check_diagnostics(&source_text, &diagnostics_bag)?;

        Ok(Self::create_compilation_unit(ast, diagnostics_bag))
    }

    fn create_compilation_unit(
        ast: Ast<'de>,
        diagnostics_bag: DiagnosticsBagCell<'de>,
    ) -> CompilationUnit<'de> {
        CompilationUnit {
            ast,
            _diagnostics_bag: diagnostics_bag,
        }
    }

    fn check_diagnostics(
        source_text: &SourceText,
        diagnostics_bag: &DiagnosticsBagCell<'de>,
    ) -> Result<(), DiagnosticsBag<'de>> {
        let diagnostics = diagnostics_bag.borrow();
        if !diagnostics.diagnostics.is_empty() {
            let printer = DiagnosticPrinter::new(source_text, &diagnostics.diagnostics);
            printer.print();
            Err(diagnostics.clone())
        } else {
            Ok(())
        }
    }

    pub fn run(&mut self) {
        let mut eval = ASTEvaluator::new();
        self.ast.visit(&mut eval);
        println!("Result: {:?}", eval.last_value);
    }
}
