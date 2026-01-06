//! Hedgehog Environment
//!
//! Manages variable scopes and bindings.

use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

use crate::eval::Value;

/// Environment for variable bindings
#[derive(Debug, Clone)]
pub struct Env {
    /// Current scope bindings
    bindings: HashMap<String, Value>,

    /// Parent scope (for nested scopes)
    parent: Option<Rc<RefCell<Env>>>,
}

impl Env {
    /// Create a new global environment
    pub fn new() -> Self {
        Self {
            bindings: HashMap::new(),
            parent: None,
        }
    }

    /// Create a new scope with parent
    pub fn with_parent(parent: Rc<RefCell<Env>>) -> Self {
        Self {
            bindings: HashMap::new(),
            parent: Some(parent),
        }
    }

    /// Create a wrapped environment
    pub fn wrap(self) -> Rc<RefCell<Self>> {
        Rc::new(RefCell::new(self))
    }

    /// Define a new binding in current scope
    pub fn define(&mut self, name: String, value: Value) {
        self.bindings.insert(name, value);
    }

    /// Get a value (searches parent scopes)
    pub fn get(&self, name: &str) -> Option<Value> {
        if let Some(value) = self.bindings.get(name) {
            Some(value.clone())
        } else if let Some(parent) = &self.parent {
            parent.borrow().get(name)
        } else {
            None
        }
    }

    /// Set a value (in the scope where it's defined)
    pub fn set(&mut self, name: &str, value: Value) -> bool {
        if self.bindings.contains_key(name) {
            self.bindings.insert(name.to_string(), value);
            true
        } else if let Some(parent) = &self.parent {
            parent.borrow_mut().set(name, value)
        } else {
            false
        }
    }

    /// Check if a binding exists
    #[allow(dead_code)]
    pub fn contains(&self, name: &str) -> bool {
        if self.bindings.contains_key(name) {
            true
        } else if let Some(parent) = &self.parent {
            parent.borrow().contains(name)
        } else {
            false
        }
    }

    /// Get all bindings in current scope
    #[allow(dead_code)]
    pub fn bindings(&self) -> &HashMap<String, Value> {
        &self.bindings
    }

    /// Get environment variable
    pub fn get_env_var(&self, name: &str) -> Option<String> {
        std::env::var(name).ok()
    }

    /// Set environment variable
    #[allow(dead_code)]
    pub fn set_env_var(&self, name: &str, value: &str) {
        std::env::set_var(name, value);
    }

    /// Get a value from the global (root) scope only
    pub fn get_global(&self, name: &str) -> Option<Value> {
        if let Some(parent) = &self.parent {
            parent.borrow().get_global(name)
        } else {
            // This is the root scope
            self.bindings.get(name).cloned()
        }
    }

    /// Get a value from the parent scope only (not current, not grandparents)
    pub fn get_parent(&self, name: &str) -> Option<Value> {
        if let Some(parent) = &self.parent {
            parent.borrow().bindings.get(name).cloned()
        } else {
            None
        }
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

/// Function definition stored in environment
#[derive(Debug, Clone)]
pub struct FuncDef {
    pub name: String,
    pub params: Vec<String>,
    pub body: crate::ast::Node<crate::ast::Expr>,
    pub closure: Rc<RefCell<Env>>,
}

impl FuncDef {
    pub fn new(
        name: String,
        params: Vec<String>,
        body: crate::ast::Node<crate::ast::Expr>,
        closure: Rc<RefCell<Env>>
    ) -> Self {
        Self { name, params, body, closure }
    }
}

// FuncDefはクロージャを含むため、単純なPartialEqは実装できない
// 代わりに名前で比較する
impl PartialEq for FuncDef {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.params == other.params
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_env_basic() {
        let mut env = Env::new();
        env.define("x".to_string(), Value::Int(42));

        assert_eq!(env.get("x"), Some(Value::Int(42)));
        assert_eq!(env.get("y"), None);
    }

    #[test]
    fn test_env_nested() {
        let parent = Env::new().wrap();
        parent.borrow_mut().define("x".to_string(), Value::Int(1));

        let mut child = Env::with_parent(parent.clone());
        child.define("y".to_string(), Value::Int(2));

        assert_eq!(child.get("x"), Some(Value::Int(1)));
        assert_eq!(child.get("y"), Some(Value::Int(2)));
    }

    #[test]
    fn test_env_shadow() {
        let parent = Env::new().wrap();
        parent.borrow_mut().define("x".to_string(), Value::Int(1));

        let mut child = Env::with_parent(parent.clone());
        child.define("x".to_string(), Value::Int(2));  // Shadow parent's x

        assert_eq!(child.get("x"), Some(Value::Int(2)));
        assert_eq!(parent.borrow().get("x"), Some(Value::Int(1)));
    }

    #[test]
    fn test_env_set() {
        let mut env = Env::new();
        env.define("x".to_string(), Value::Int(1));

        assert!(env.set("x", Value::Int(2)));
        assert_eq!(env.get("x"), Some(Value::Int(2)));
    }

    #[test]
    fn test_env_set_undefined() {
        let mut env = Env::new();
        assert!(!env.set("x", Value::Int(1)));  // x is not defined
    }

    #[test]
    fn test_env_set_in_parent() {
        let parent = Env::new().wrap();
        parent.borrow_mut().define("x".to_string(), Value::Int(1));

        let mut child = Env::with_parent(parent.clone());

        // set() modifies parent scope
        assert!(child.set("x", Value::Int(2)));
        assert_eq!(parent.borrow().get("x"), Some(Value::Int(2)));
    }

    #[test]
    fn test_env_contains() {
        let mut env = Env::new();
        env.define("x".to_string(), Value::Int(1));

        assert!(env.contains("x"));
        assert!(!env.contains("y"));
    }

    #[test]
    fn test_env_contains_parent() {
        let parent = Env::new().wrap();
        parent.borrow_mut().define("x".to_string(), Value::Int(1));

        let child = Env::with_parent(parent.clone());

        assert!(child.contains("x"));
        assert!(!child.contains("y"));
    }

    #[test]
    fn test_env_multiple_values() {
        let mut env = Env::new();
        env.define("a".to_string(), Value::Int(1));
        env.define("b".to_string(), Value::String("hello".to_string()));
        env.define("c".to_string(), Value::Bool(true));
        env.define("d".to_string(), Value::List(vec![Value::Int(1), Value::Int(2)]));

        assert_eq!(env.get("a"), Some(Value::Int(1)));
        assert_eq!(env.get("b"), Some(Value::String("hello".to_string())));
        assert_eq!(env.get("c"), Some(Value::Bool(true)));
        assert_eq!(env.get("d"), Some(Value::List(vec![Value::Int(1), Value::Int(2)])));
    }

    #[test]
    fn test_env_deeply_nested() {
        let grandparent = Env::new().wrap();
        grandparent.borrow_mut().define("a".to_string(), Value::Int(1));

        let parent = Env::with_parent(grandparent.clone()).wrap();
        parent.borrow_mut().define("b".to_string(), Value::Int(2));

        let child = Env::with_parent(parent.clone());

        assert_eq!(child.get("a"), Some(Value::Int(1)));
        assert_eq!(child.get("b"), Some(Value::Int(2)));
    }

    #[test]
    fn test_env_redefine() {
        let mut env = Env::new();
        env.define("x".to_string(), Value::Int(1));
        env.define("x".to_string(), Value::Int(2));  // Redefine

        assert_eq!(env.get("x"), Some(Value::Int(2)));
    }
}