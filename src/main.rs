use ast::scopes::GlobalScope;
use clap::{Parser, Subcommand};
use compiler::compilation_unit::CompilationUnit;
use lexer::Lexer;
use miette::{IntoDiagnostic, WrapErr};
use std::fs;
use std::path::PathBuf;

#[derive(Parser, Debug)]
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
    // let args = Args::parse();
    // match args.command {
    //     Commands::Tokenize { filename } => {
    //         let mut any_cc_err = false;

    //         let file_contents = fs::read_to_string(&filename)
    //             .into_diagnostic()
    //             .wrap_err_with(|| format!("reading '{}' failed", filename.display()))?;

    //         for token in Lexer::new(&file_contents) {
    //             let token = match token {
    //                 Ok(t) => t,
    //                 Err(e) => {
    //                     eprintln!("{e:?}");
    //                     if let Some(unterminated) =
    //                         e.downcast_ref::<lexer::StringTerminationError>()
    //                     {
    //                         any_cc_err = true;
    //                         eprintln!("[line {}] Error: Unterminated string.", unterminated.line(),);
    //                     }
    //                     continue;
    //                 }
    //             };
    //             println!("{token}");
    //         }

    //         match any_cc_err {
    //             true => Err(miette::miette!("Encountered compilation errors")),
    //             false => Ok(()),
    //         }
    //     }
    //     Commands::Parse { filename } => {
    //         let file_contents = fs::read_to_string(&filename)
    //             .into_diagnostic()
    //             .wrap_err_with(|| format!("reading '{}' failed", filename.display()))?;

    //         let global_scope = GlobalScope::new();
    //         match CompilationUnit::compile(&file_contents) {
    //             Ok(mut compilation_unit) => {
    //                 compilation_unit.run(&global_scope);
    //                 Ok(())
    //             }
    //             Err(_) => {
    //                 std::process::exit(65);
    //             }
    //         }
    //     }
    // }
    let input = "
    /*
        func add(a, b) {
            return a + b
        }
        add(2 * 3, 4 + 5)
    */
    let a = add(1, 2)
    func add(a, b) {
        return a + b
        }
    {
        let a = 10
        a
    }
    ";

    let global_scope = GlobalScope::new();
    match CompilationUnit::compile(input) {
        Ok(mut compilation_unit) => {
            compilation_unit.run(&global_scope);
        }
        Err(_) => {
            std::process::exit(65);
        }
    }
    Ok(())
}
