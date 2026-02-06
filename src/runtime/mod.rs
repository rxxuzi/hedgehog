//! Hedgehog Runtime
//!
//! Core runtime components: values, environment, and evaluator.

pub mod channel;
pub mod value;
pub mod env;
pub mod eval;

pub use channel::Channel;
pub use value::{Value, EvalError};
pub use env::{Env, FuncDef};
pub use eval::Evaluator;
