//! Hedgehog Runtime
//!
//! Core runtime components: values, environment, and evaluator.

pub mod channel;
pub mod env;
pub mod eval;
pub mod value;

pub use channel::Channel;
pub use env::{Env, FuncDef};
pub use eval::Evaluator;
pub use value::{EvalError, Value};
