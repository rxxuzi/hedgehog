//! Hedgehog - A keyword-less shell scripting language

pub mod ast;
pub mod builtin;
pub mod env;
pub mod eval;
pub mod exec;
pub mod lexer;
pub mod parser;

use eval::Evaluator;
use parser::Parser;
use std::env as std_env;
use std::fs;
use std::io::{self, BufRead, Write};

const VERSION: &str = "0.1.0";

fn main() {
    let args: Vec<String> = std_env::args().collect();

    let mut file: Option<String> = None;
    let mut code: Option<String> = None;
    let mut check_only = false;
    let mut interactive = false;

    let mut i = 1;
    while i < args.len() {
        match args[i].as_str() {
            "-h" | "--help" => {
                print_help();
                return;
            }
            "-v" | "--version" => {
                print_version();
                return;
            }
            "-c" => {
                if i + 1 < args.len() {
                    code = Some(args[i + 1].clone());
                    i += 1;
                } else {
                    eprintln!("hog: option -c requires an argument");
                    std::process::exit(1);
                }
            }
            "-n" => {
                check_only = true;
            }
            "-i" => {
                interactive = true;
            }
            arg if arg.starts_with('-') => {
                eprintln!("hog: unknown option: {}", arg);
                std::process::exit(1);
            }
            _ => {
                file = Some(args[i].clone());
            }
        }
        i += 1;
    }

    // Execute based on options
    if let Some(cmd) = code {
        run_code(&cmd);
    } else if let Some(path) = file {
        if check_only {
            check_file(&path);
        } else if interactive {
            run_file(&path);
            repl();
        } else {
            run_file(&path);
        }
    } else {
        repl();
    }
}

fn print_help() {
    println!("Usage: hog [options] [file]");
    println!();
    println!("Options:");
    println!("  -c CMD    Execute CMD as a string");
    println!("  -n        Check syntax only");
    println!("  -i        Run file then enter REPL");
    println!("  -v        Show version");
    println!("  -h        Show help");
    println!();
    println!("Examples:");
    println!("  hog                     Start REPL");
    println!("  hog script.hog          Run script");
    println!("  hog -c '~> \"hello\"'     Run one-liner");
    println!("  hog -n script.hog       Check syntax");
    println!("  hog -i script.hog       Run then REPL");
}

fn print_version() {
    println!("hog {}", VERSION);
}

fn repl() {
    println!("Hedgehog {} REPL", VERSION);
    println!("Type 'exit' to quit");
    println!();

    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut evaluator = Evaluator::new();

    loop {
        print!(">>> ");
        stdout.flush().unwrap();

        let mut line = String::new();
        match stdin.lock().read_line(&mut line) {
            Ok(0) => {
                println!();
                break;
            }
            Ok(_) => {
                let line = line.trim();
                if line == "exit" || line == "quit" {
                    break;
                }
                if line.is_empty() {
                    continue;
                }

                match Parser::parse_source(line) {
                    Ok(program) => {
                        match evaluator.eval_program(&program) {
                            Ok(value) => {
                                if value != eval::Value::Unit {
                                    println!("{}", value);
                                }
                            }
                            Err(e) => eprintln!("Error: {}", e),
                        }
                    }
                    Err(e) => eprintln!("SyntaxError: {}", e),
                }
            }
            Err(e) => {
                eprintln!("Error: {}", e);
                break;
            }
        }
    }
}

fn run_file(path: &str) {
    match fs::read_to_string(path) {
        Ok(content) => run_code(&content),
        Err(e) => {
            eprintln!("hog: cannot open '{}': {}", path, e);
            std::process::exit(1);
        }
    }
}

fn run_code(code: &str) {
    match Parser::parse_source(code) {
        Ok(program) => {
            let mut evaluator = Evaluator::new();
            if let Err(e) = evaluator.eval_program(&program) {
                eprintln!("Error: {}", e);
                std::process::exit(1);
            }
        }
        Err(e) => {
            eprintln!("SyntaxError: {}", e);
            std::process::exit(1);
        }
    }
}

fn check_file(path: &str) {
    match fs::read_to_string(path) {
        Ok(content) => {
            match Parser::parse_source(&content) {
                Ok(_) => {}
                Err(e) => {
                    eprintln!("SyntaxError: {}", e);
                    std::process::exit(1);
                }
            }
        }
        Err(e) => {
            eprintln!("hog: cannot open '{}': {}", path, e);
            std::process::exit(1);
        }
    }
}