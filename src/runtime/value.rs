//! Hedgehog Runtime Values
//!
//! Core value types and evaluation errors.

use std::collections::HashMap;
use std::fmt;
use std::sync::Arc;

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
    Func(Arc<FuncDef>),

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

/// Send-safe value for parallel execution
/// Functions are converted to Unit since they can't be sent across threads
#[derive(Debug, Clone)]
pub enum SendableValue {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    List(Vec<SendableValue>),
    Record(std::collections::HashMap<String, SendableValue>),
    Some(Box<SendableValue>),
    None,
    Ok(Box<SendableValue>),
    Err(Box<SendableValue>),
    Unit,
}

impl From<Value> for SendableValue {
    fn from(v: Value) -> Self {
        match v {
            Value::Int(n) => SendableValue::Int(n),
            Value::Float(f) => SendableValue::Float(f),
            Value::String(s) => SendableValue::String(s),
            Value::Bool(b) => SendableValue::Bool(b),
            Value::List(items) => SendableValue::List(items.into_iter().map(Into::into).collect()),
            Value::Record(fields) => SendableValue::Record(fields.into_iter().map(|(k, v)| (k, v.into())).collect()),
            Value::Tuple(items) => SendableValue::List(items.into_iter().map(Into::into).collect()),
            Value::Some(v) => SendableValue::Some(Box::new((*v).into())),
            Value::None => SendableValue::None,
            Value::Ok(v) => SendableValue::Ok(Box::new((*v).into())),
            Value::Err(v) => SendableValue::Err(Box::new((*v).into())),
            Value::Func(_) => SendableValue::Unit, // Functions can't be sent
            Value::Builtin(name) => SendableValue::String(format!("<builtin {}>", name)),
            Value::Channel(_) => SendableValue::Unit, // Channels handled separately
            Value::Unit => SendableValue::Unit,
        }
    }
}

impl From<SendableValue> for Value {
    fn from(v: SendableValue) -> Self {
        match v {
            SendableValue::Int(n) => Value::Int(n),
            SendableValue::Float(f) => Value::Float(f),
            SendableValue::String(s) => Value::String(s),
            SendableValue::Bool(b) => Value::Bool(b),
            SendableValue::List(items) => Value::List(items.into_iter().map(Into::into).collect()),
            SendableValue::Record(fields) => Value::Record(fields.into_iter().map(|(k, v)| (k, v.into())).collect()),
            SendableValue::Some(v) => Value::Some(Box::new((*v).into())),
            SendableValue::None => Value::None,
            SendableValue::Ok(v) => Value::Ok(Box::new((*v).into())),
            SendableValue::Err(v) => Value::Err(Box::new((*v).into())),
            SendableValue::Unit => Value::Unit,
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
