//! Hedgehog - A keyword-less shell scripting language
//!
//! Hedgehog uses symbolic "Berry" operators instead of traditional keywords.

pub mod ast;
pub mod builtin;
pub mod exec;
pub mod lexer;
pub mod parser;
pub mod report;
pub mod runtime;

// Re-export for public API
pub use runtime::{Env, EvalError, Evaluator, FuncDef, Value};

use parser::Parser;

/// Result type for Hedgehog operations
pub type Result<T> = std::result::Result<T, Error>;

/// Error type for Hedgehog operations
#[derive(Debug, Clone)]
pub enum Error {
    Parse(String),
    Eval(String),
    Io(String),
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::Parse(msg) => write!(f, "SyntaxError: {}", msg),
            Error::Eval(msg) => write!(f, "Error: {}", msg),
            Error::Io(msg) => write!(f, "IOError: {}", msg),
        }
    }
}

impl std::error::Error for Error {}

/// Output captured from running Hedgehog code
#[derive(Debug, Clone, Default)]
pub struct Output {
    pub stdout: String,
    pub stderr: String,
}

/// Run Hedgehog source code and capture output
///
/// Note: Output capture is not yet implemented. Use `run_code_direct` for CLI-like execution.
pub fn run_code(code: &str, args: Vec<String>) -> Result<Output> {
    run_code_with_capture(code, args)
}

/// Run a Hedgehog file and capture output
pub fn run_file(path: &str, args: Vec<String>) -> Result<Output> {
    let content = std::fs::read_to_string(path)
        .map_err(|e| Error::Io(format!("cannot open '{}': {}", path, e)))?;
    run_code(&content, args)
}

/// Check syntax without executing
pub fn check_syntax(code: &str) -> Result<()> {
    Parser::parse_source(code)
        .map(|_| ())
        .map_err(|e| Error::Parse(e.to_string()))
}

/// Internal: Run code and capture stdout/stderr
fn run_code_with_capture(code: &str, args: Vec<String>) -> Result<Output> {
    let program = Parser::parse_source(code).map_err(|e| Error::Parse(e.to_string()))?;

    let mut evaluator = Evaluator::with_args(args);
    evaluator
        .eval_program(&program)
        .map_err(|e| Error::Eval(e.to_string()))?;

    // For now, return empty output since we don't capture stdout yet
    // TODO: Implement proper output capture
    Ok(Output::default())
}

/// Run code without capturing output (for CLI use)
pub fn run_code_direct(code: &str, args: Vec<String>) -> Result<()> {
    let program = Parser::parse_source(code).map_err(|e| Error::Parse(e.to_string()))?;

    let mut evaluator = Evaluator::with_args(args);
    evaluator
        .eval_program(&program)
        .map_err(|e| Error::Eval(e.to_string()))?;

    Ok(())
}
