use ast::ast::Ast;
use clap::{Parser as CParser, Subcommand};
use lexer::Lexer;
use miette::{IntoDiagnostic, WrapErr};
use parser::Parser;
use std::fs;
use std::path::PathBuf;

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
                        if let Some(unrecognized) = e.downcast_ref::<lexer::SingleTokenError>() {
                            any_cc_err = true;
                            eprintln!(
                                "[line {}] Error: Unexpected character: {}",
                                unrecognized.line(),
                                unrecognized.token
                            );
                        } else if let Some(unterminated) =
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

            match Parser::from_input(&file_contents) {
                Ok(mut parser) => {
                    // Proceed with parsing
                    // for token in parser.tokens {
                    //     println!("{token}");
                    // }
                    let mut ast = Ast::new();
                    while let Some(statement) = parser.next_statement() {
                        ast.add_statement(statement);
                    }
                    ast.visualize();
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
