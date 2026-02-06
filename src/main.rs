//! Hedgehog - A keyword-less shell scripting language

use hedgehog::runtime::{Evaluator, Value};
use hedgehog::parser::Parser;
use std::env as std_env;
use std::fs;
use std::io::{self, BufRead, Write};

const VERSION: &str = env!("CARGO_PKG_VERSION");

fn main() {
    let args: Vec<String> = std_env::args().collect();

    let mut file: Option<String> = None;
    let mut code: Option<String> = None;
    let mut check_only = false;
    let mut interactive = false;
    let mut script_args: Vec<String> = Vec::new();

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
                if file.is_none() {
                    file = Some(args[i].clone());
                } else {
                    // Additional args are script arguments
                    script_args.push(args[i].clone());
                }
            }
        }
        i += 1;
    }

    // Execute based on options
    if let Some(cmd) = code {
        run_code(&cmd, script_args);
    } else if let Some(path) = file {
        if check_only {
            check_file(&path);
        } else if interactive {
            if let Some(evaluator) = run_file_interactive(&path, script_args) {
                repl_with_evaluator(evaluator);
            }
        } else {
            run_file(&path, script_args);
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
    repl_with_evaluator(Evaluator::new());
}

fn repl_with_evaluator(mut evaluator: Evaluator) {
    println!("Hedgehog {} REPL", VERSION);
    println!("Type 'exit' to quit");
    println!();

    let stdin = io::stdin();
    let mut stdout = io::stdout();

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
                                if value != Value::Unit {
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

fn run_file(path: &str, args: Vec<String>) {
    match fs::read_to_string(path) {
        Ok(content) => run_code(&content, args),
        Err(e) => {
            eprintln!("hog: cannot open '{}': {}", path, e);
            std::process::exit(1);
        }
    }
}

fn run_file_interactive(path: &str, args: Vec<String>) -> Option<Evaluator> {
    match fs::read_to_string(path) {
        Ok(content) => {
            match Parser::parse_source(&content) {
                Ok(program) => {
                    let mut evaluator = Evaluator::with_args(args);
                    if let Err(e) = evaluator.eval_program(&program) {
                        eprintln!("Error: {}", e);
                        std::process::exit(1);
                    }
                    Some(evaluator)
                }
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

fn run_code(code: &str, args: Vec<String>) {
    match Parser::parse_source(code) {
        Ok(program) => {
            let mut evaluator = Evaluator::with_args(args);
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
                Ok(_) => {
                    println!("{}: OK", path);
                }
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
