use ast::ast::Ast;
use ast::evaluator::ASTEvaluator;
use clap::{Parser as CParser, Subcommand};
use diagnostics::diagnostics::{DiagnosticsBag, DiagnosticsBagCell};
use diagnostics::printer::DiagnosticPrinter;
use lexer::Lexer;
use miette::{IntoDiagnostic, WrapErr};
use parser::Parser;
use std::cell::RefCell;
use std::fs;
use std::path::PathBuf;
use std::rc::Rc;
use text::source::SourceText;

#[derive(CParser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
enum Commands {
    Tokenize { filename: PathBuf },
    Parse { filename: PathBuf },
}

fn main() -> miette::Result<()> {
    let args = Args::parse();
    match args.command {
        Commands::Tokenize { filename } => {
            let mut any_cc_err = false;

            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading '{}' failed", filename.display()))?;

            for token in Lexer::new(&file_contents) {
                let token = match token {
                    Ok(t) => t,
                    Err(e) => {
                        eprintln!("{e:?}");
                        if let Some(unterminated) =
                            e.downcast_ref::<lexer::StringTerminationError>()
                        {
                            any_cc_err = true;
                            eprintln!("[line {}] Error: Unterminated string.", unterminated.line(),);
                        }
                        continue;
                    }
                };
                println!("{token}");
            }

            if any_cc_err {
                std::process::exit(65);
            }
        }
        Commands::Parse { filename } => {
            let mut any_cc_err = false;

            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading '{}' failed", filename.display()))?;

            let source_text = SourceText::new(file_contents.clone());

            let diagnostics_bag = DiagnosticsBagCell::new(RefCell::new(DiagnosticsBag::new()));
            match Parser::from_input(&file_contents, Rc::clone(&diagnostics_bag)) {
                Ok(mut parser) => {
                    let mut ast = Ast::new();
                    while let Some(statement) = parser.next_statement() {
                        ast.add_statement(statement);
                    }
                    let diagnostics_binding = diagnostics_bag.borrow();
                    if !diagnostics_binding.diagnostics.is_empty() {
                        let diagnostics_printer =
                            DiagnosticPrinter::new(&source_text, &diagnostics_binding.diagnostics);
                        diagnostics_printer.print();
                        any_cc_err = true;
                    } else {
                        ast.visualize();
                        let mut eval = ASTEvaluator::new();
                        ast.visit(&mut eval);
                        println!("Result: {:?}", eval.last_value);
                    }
                }
                Err(e) => {
                    // Use miette to report the error
                    eprintln!("{}", miette::Report::new(e));
                    any_cc_err = true;
                }
            }
            if any_cc_err {
                std::process::exit(65);
            }
        }
    }
    Ok(())
}
