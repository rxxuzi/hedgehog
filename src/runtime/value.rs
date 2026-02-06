//! Hedgehog Runtime Values
//!
//! Core value types and evaluation errors.

use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::runtime::channel::Channel;
use crate::runtime::env::FuncDef;

/// Runtime values
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// Integer
    Int(i64),

    /// Float
    Float(f64),

    /// String
    String(String),

    /// Boolean
    Bool(bool),

    /// List
    List(Vec<Value>),

    /// Record
    Record(HashMap<String, Value>),

    /// Tuple
    Tuple(Vec<Value>),

    /// Option Some
    Some(Box<Value>),

    /// Option None
    None,

    /// Result Ok
    Ok(Box<Value>),

    /// Result Err
    Err(Box<Value>),

    /// Function
    Func(Rc<FuncDef>),

    /// Built-in function
    Builtin(String),

    /// Channel
    Channel(Channel),

    /// Unit (no value)
    Unit,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(n) => write!(f, "{}", n),
            Value::Float(n) => write!(f, "{}", n),
            Value::String(s) => write!(f, "{}", s),
            Value::Bool(b) => write!(f, "{}", b),
            Value::List(items) => {
                write!(f, "[")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{}", item)?;
                }
                write!(f, "]")
            }
            Value::Record(fields) => {
                write!(f, "{{")?;
                for (i, (k, v)) in fields.iter().enumerate() {
                    if i > 0 { write!(f, ", ")?; }
                    write!(f, "{}: {}", k, v)?;
                }
                write!(f, "}}")
            }
            Value::Tuple(items) => {
                write!(f, "(,")?;
                for item in items {
                    write!(f, " {}", item)?;
                }
                write!(f, ")")
            }
            Value::Some(v) => write!(f, "(some {})", v),
            Value::None => write!(f, "none"),
            Value::Ok(v) => write!(f, "(ok {})", v),
            Value::Err(v) => write!(f, "(err {})", v),
            Value::Func(func) => write!(f, "<func {}>", func.name),
            Value::Builtin(name) => write!(f, "<builtin {}>", name),
            Value::Channel(ch) => write!(f, "<channel len={}>", ch.len()),
            Value::Unit => write!(f, "()"),
        }
    }
}

/// Evaluation error
#[derive(Debug, Clone)]
pub struct EvalError {
    pub message: String,
    pub line: usize,
    pub column: usize,
}

impl fmt::Display for EvalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Error at {}:{}: {}", self.line, self.column, self.message)
    }
}

impl EvalError {
    pub fn new(message: impl Into<String>, line: usize, column: usize) -> Self {
        Self {
            message: message.into(),
            line,
            column,
        }
    }
}
