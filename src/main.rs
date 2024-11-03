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

            match any_cc_err {
                true => Err(miette::miette!("Encountered compilation errors")),
                false => Ok(()),
            }
        }
        Commands::Parse { filename } => {
            let file_contents = fs::read_to_string(&filename)
                .into_diagnostic()
                .wrap_err_with(|| format!("reading '{}' failed", filename.display()))?;

            match CompilationUnit::compile(&file_contents) {
                Ok(compilation_unit) => {
                    compilation_unit.run();
                }
                Err(_) => {
                    std::process::exit(65);
                }
            }
            Ok(())
        }
    }
    // let input = "
    // /*
    //     func add(a, b) {
    //         return a + b
    //     }
    //     add(2 * 3, 4 + 5)
    //     {
    //         let a = 10
    //         a
    //     }
    //     let a = add(1, 2)
    //     func add(a, b) {
    //         return a + b
    //         }
    // */
    // /*
    // let x = 0
    // let y = 0
    // let z = 0
    // outer: while true {
    //     y = y + 10
    //     while (true) {
    //         x = x + 1
    //         if x > 3 {
    //             break outer
    //         }
    //         z = z + x
    //     }
    // }
    // z
    // let x = 0
    // let y = 0
    // let z = true
    // while z {
    //     x = x + 1
    //     while (y < 3) {
    //         y = y + 1
    //         if (y == 2) {
    //             break // Exits the inner loop
    //         }
    //     }
    //     z = false
    // }
    // x
    // */
    // let x = 0
    // let y = 0
    // let z = 0
    // outer: while true {
    //     y = y + 10
    //     while true {
    //         x = x + 1
    //         while x < 10 {
    //             x = x + 1
    //             if x == 5 {
    //                 break
    //             }
    //         }
    //         while true {
    //             x = x + 1
    //             if x == 7 {
    //                 break outer
    //             }
    //         }
    //         break
    //     }
    //     z = z + 300
    //     break
    // }
    // z
    // ";

    // match CompilationUnit::compile(input) {
    //     Ok(compilation_unit) => {
    //         compilation_unit.run();
    //     }
    //     Err(_) => {
    //         std::process::exit(65);
    //     }
    // }
    // Ok(())
}
