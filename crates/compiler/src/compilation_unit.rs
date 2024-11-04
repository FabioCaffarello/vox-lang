use std::{cell::RefCell, rc::Rc};

use ast::{
    ast::Ast,
    evaluator::ASTEvaluator,
    scopes::{GlobalScope, Resolver, Scopes},
    validator::GlobalSymbolResolver,
    ASTVisitor,
};
use diagnostics::{
    diagnostics::{DiagnosticsBag, DiagnosticsBagCell},
    printer::DiagnosticPrinter,
};
use parser::Parser;
use text::source::SourceText;

#[derive(Debug)]
pub struct CompilationUnit<'de> {
    pub ast: Ast<'de>,
    pub diagnostics_bag: DiagnosticsBagCell<'de>,
    pub global_scope: GlobalScope,
}

impl<'de> CompilationUnit<'de> {
    pub fn compile(input: &'de str) -> Result<CompilationUnit<'de>, DiagnosticsBag<'de>> {
        let source_text = SourceText::new(input.to_string());
        let diagnostics_bag: DiagnosticsBagCell = Rc::new(RefCell::new(DiagnosticsBag::new()));
        let diagnostics_rc = Rc::clone(&diagnostics_bag);

        let mut ast = Ast::new();
        let mut parser = Parser::from_input(input, diagnostics_rc.clone(), &mut ast)
            .map_err(|_| diagnostics_bag.borrow().clone())?;

        parser.parse();
        ast.visualize();
        if Self::check_diagnostics(&source_text, &diagnostics_bag).is_err() {
            return Err(diagnostics_bag.borrow().clone());
        }
        let mut global_symbol_resolver =
            GlobalSymbolResolver::new(Rc::clone(&diagnostics_bag), &ast);
        ast.visit(&mut global_symbol_resolver);
        let global_scope = global_symbol_resolver.global_scope;
        let scopes = Scopes::from_global_scope(global_scope);
        let mut resolver = Resolver::new(Rc::clone(&diagnostics_bag), scopes, &mut ast);
        resolver.resolve();
        if Self::check_diagnostics(&source_text, &diagnostics_bag).is_err() {
            return Err(diagnostics_bag.borrow().clone());
        }

        Ok(CompilationUnit {
            global_scope: resolver.scopes.global_scope,
            ast,
            diagnostics_bag,
        })
    }

    fn check_diagnostics(
        text: &SourceText,
        diagnostics_bag: &DiagnosticsBagCell,
    ) -> Result<(), ()> {
        let diagnostics = diagnostics_bag.borrow();
        if !diagnostics.diagnostics.is_empty() {
            let printer = DiagnosticPrinter::new(text, &diagnostics.diagnostics);
            printer.print();
            Err(())
        } else {
            Ok(())
        }
    }

    pub fn run(&self) {
        let mut eval = ASTEvaluator::new(&self.global_scope, &self.ast);
        let main_function = self.global_scope.lookup_function("main");

        if let Some(function) = main_function {
            eval.visit_statement(&function.body);
        } else {
            self.ast.visit(&mut eval);
        }
        println!("Result: {:?}", eval.last_value);
    }
}
