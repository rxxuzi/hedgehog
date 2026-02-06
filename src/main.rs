//! Hedgehog - A keyword-less shell scripting language

use hedgehog::parser::{ParseError, Parser};
use hedgehog::report::{codes, get_source_line, Diagnostic};
use hedgehog::runtime::{EvalError, Evaluator, Value};
use std::env as std_env;
use std::fs;
use std::io::{self, BufRead, Write};

const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Report a parse error with source context
fn report_parse_error(error: &ParseError, source: &str, file: &str) {
    let source_line = get_source_line(source, error.line);

    let mut diag = Diagnostic::error(&error.message)
        .with_code(codes::UNEXPECTED_TOKEN)
        .at(file, error.line, error.column);

    if let Some(line) = source_line {
        diag = diag.with_source(line).with_length(1);
    }

    diag.emit();
}

/// Report an eval error with source context
fn report_eval_error(error: &EvalError, source: &str, file: &str) {
    let source_line = get_source_line(source, error.line);

    // Determine error code based on message
    let code = if error.message.contains("undefined") {
        codes::UNDEFINED_VARIABLE
    } else if error.message.contains("type") || error.message.contains("expected") {
        codes::TYPE_MISMATCH
    } else if error.message.contains("division by zero") {
        codes::DIVISION_BY_ZERO
    } else if error.message.contains("index") {
        codes::INDEX_OUT_OF_BOUNDS
    } else {
        "E0000"
    };

    let mut diag =
        Diagnostic::error(&error.message)
            .with_code(code)
            .at(file, error.line, error.column);

    if let Some(line) = source_line {
        diag = diag.with_source(line).with_length(1);
    }

    diag.emit();
}

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
                    Ok(program) => match evaluator.eval_program(&program) {
                        Ok(value) => {
                            if value != Value::Unit {
                                println!("{}", value);
                            }
                        }
                        Err(e) => report_eval_error(&e, line, "<repl>"),
                    },
                    Err(e) => report_parse_error(&e, line, "<repl>"),
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
        Ok(content) => run_code_with_file(&content, args, path),
        Err(e) => {
            Diagnostic::error(format!("cannot open '{}': {}", path, e)).emit();
            std::process::exit(1);
        }
    }
}

fn run_file_interactive(path: &str, args: Vec<String>) -> Option<Evaluator> {
    match fs::read_to_string(path) {
        Ok(content) => match Parser::parse_source(&content) {
            Ok(program) => {
                let mut evaluator = Evaluator::with_args(args);
                if let Err(e) = evaluator.eval_program(&program) {
                    report_eval_error(&e, &content, path);
                    std::process::exit(1);
                }
                Some(evaluator)
            }
            Err(e) => {
                report_parse_error(&e, &content, path);
                std::process::exit(1);
            }
        },
        Err(e) => {
            Diagnostic::error(format!("cannot open '{}': {}", path, e)).emit();
            std::process::exit(1);
        }
    }
}

fn run_code(code: &str, args: Vec<String>) {
    run_code_with_file(code, args, "<stdin>");
}

fn run_code_with_file(code: &str, args: Vec<String>, file: &str) {
    match Parser::parse_source(code) {
        Ok(program) => {
            let mut evaluator = Evaluator::with_args(args);
            if let Err(e) = evaluator.eval_program(&program) {
                report_eval_error(&e, code, file);
                std::process::exit(1);
            }
        }
        Err(e) => {
            report_parse_error(&e, code, file);
            std::process::exit(1);
        }
    }
}

fn check_file(path: &str) {
    match fs::read_to_string(path) {
        Ok(content) => match Parser::parse_source(&content) {
            Ok(_) => {
                println!("{}: OK", path);
            }
            Err(e) => {
                report_parse_error(&e, &content, path);
                std::process::exit(1);
            }
        },
        Err(e) => {
            Diagnostic::error(format!("cannot open '{}': {}", path, e)).emit();
            std::process::exit(1);
        }
    }
}
