//! Hedgehog Evaluator
//!
//! Evaluates AST nodes and produces values.

use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::ast::{BinOp, Expr, Literal, Loc, Node, Pattern, Program, Stmt, Type, UnaryOp};
use crate::env::{Env, FuncDef};
use crate::exec;

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

/// Evaluator
pub struct Evaluator {
    env: Rc<RefCell<Env>>,
}

impl Evaluator {
    pub fn new() -> Self {
        let env = Env::new().wrap();

        // Register built-in functions
        {
            let mut e = env.borrow_mut();
            e.define("len".to_string(), Value::Builtin("len".to_string()));
            e.define("head".to_string(), Value::Builtin("head".to_string()));
            e.define("tail".to_string(), Value::Builtin("tail".to_string()));
            e.define("last".to_string(), Value::Builtin("last".to_string()));
            e.define("init".to_string(), Value::Builtin("init".to_string()));
            e.define("rev".to_string(), Value::Builtin("rev".to_string()));
            e.define("sort".to_string(), Value::Builtin("sort".to_string()));
            e.define("uniq".to_string(), Value::Builtin("uniq".to_string()));
            e.define("split".to_string(), Value::Builtin("split".to_string()));
            e.define("join".to_string(), Value::Builtin("join".to_string()));
            e.define("trim".to_string(), Value::Builtin("trim".to_string()));
            e.define("upper".to_string(), Value::Builtin("upper".to_string()));
            e.define("lower".to_string(), Value::Builtin("lower".to_string()));
            e.define("replace".to_string(), Value::Builtin("replace".to_string()));
            e.define("int".to_string(), Value::Builtin("int".to_string()));
            e.define("float".to_string(), Value::Builtin("float".to_string()));
            e.define("str".to_string(), Value::Builtin("str".to_string()));
            e.define("some".to_string(), Value::Builtin("some".to_string()));
            e.define("ok".to_string(), Value::Builtin("ok".to_string()));
            e.define("err".to_string(), Value::Builtin("err".to_string()));
            e.define("some?".to_string(), Value::Builtin("some?".to_string()));
            e.define("none?".to_string(), Value::Builtin("none?".to_string()));
            e.define("ok?".to_string(), Value::Builtin("ok?".to_string()));
            e.define("err?".to_string(), Value::Builtin("err?".to_string()));
            e.define("unwrap".to_string(), Value::Builtin("unwrap".to_string()));
            e.define("unwrap-or".to_string(), Value::Builtin("unwrap-or".to_string()));
            e.define("contains".to_string(), Value::Builtin("contains".to_string()));
            e.define("starts".to_string(), Value::Builtin("starts".to_string()));
            e.define("ends".to_string(), Value::Builtin("ends".to_string()));
            e.define("concat".to_string(), Value::Builtin("concat".to_string()));
            e.define("flatten".to_string(), Value::Builtin("flatten".to_string()));
            e.define("bool".to_string(), Value::Builtin("bool".to_string()));
            e.define("abs".to_string(), Value::Builtin("abs".to_string()));
            e.define("min".to_string(), Value::Builtin("min".to_string()));
            e.define("max".to_string(), Value::Builtin("max".to_string()));
        }

        Self { env }
    }

    pub fn with_env(env: Rc<RefCell<Env>>) -> Self {
        Self { env }
    }

    /// Evaluate a program
    pub fn eval_program(&mut self, program: &Program) -> Result<Value, EvalError> {
        let mut result = Value::Unit;

        for stmt in &program.stmts {
            result = self.eval_stmt(stmt)?;
        }

        Ok(result)
    }

    /// Evaluate a statement
    pub fn eval_stmt(&mut self, stmt: &Node<Stmt>) -> Result<Value, EvalError> {
        match &stmt.node {
            Stmt::Expr(expr) => self.eval_expr(expr),

            Stmt::Bind(name, expr) => {
                let value = self.eval_expr(expr)?;
                self.env.borrow_mut().define(name.clone(), value);
                Ok(Value::Unit)
            }

            Stmt::TypedBind(name, _ty, expr) => {
                let value = self.eval_expr(expr)?;
                self.env.borrow_mut().define(name.clone(), value);
                Ok(Value::Unit)
            }

            Stmt::FuncDef(name, params, body) => {
                let func = FuncDef::new(
                    name.clone(),
                    params.clone(),
                    body.clone(),
                    self.env.clone(),
                );
                self.env.borrow_mut().define(name.clone(), Value::Func(Rc::new(func)));
                Ok(Value::Unit)
            }

            Stmt::TypedFuncDef(name, _param_types, _ret_type, params, body) => {
                let func = FuncDef::new(
                    name.clone(),
                    params.clone(),
                    body.clone(),
                    self.env.clone(),
                );
                self.env.borrow_mut().define(name.clone(), Value::Func(Rc::new(func)));
                Ok(Value::Unit)
            }

            Stmt::Output(expr) => {
                let value = self.eval_expr(expr)?;
                println!("{}", value);
                Ok(Value::Unit)
            }

            Stmt::OutputRaw(expr) => {
                let value = self.eval_expr(expr)?;
                print!("{}", value);
                Ok(Value::Unit)
            }

            Stmt::OutputErr(expr) => {
                let value = self.eval_expr(expr)?;
                eprintln!("{}", value);
                Ok(Value::Unit)
            }

            Stmt::FileWrite(path_expr, content_expr) => {
                let path = self.eval_expr(path_expr)?;
                let content = self.eval_expr(content_expr)?;
                let path_str = path.to_string();
                let content_str = content.to_string();
                std::fs::write(&path_str, &content_str).map_err(|e| {
                    EvalError::new(format!("Failed to write file: {}", e), stmt.loc.line, stmt.loc.column)
                })?;
                Ok(Value::Unit)
            }

            Stmt::FileAppend(path_expr, content_expr) => {
                use std::io::Write;
                let path = self.eval_expr(path_expr)?;
                let content = self.eval_expr(content_expr)?;
                let path_str = path.to_string();
                let content_str = content.to_string();
                let mut file = std::fs::OpenOptions::new()
                    .append(true)
                    .create(true)
                    .open(&path_str)
                    .map_err(|e| {
                        EvalError::new(format!("Failed to open file: {}", e), stmt.loc.line, stmt.loc.column)
                    })?;
                file.write_all(content_str.as_bytes()).map_err(|e| {
                    EvalError::new(format!("Failed to write file: {}", e), stmt.loc.line, stmt.loc.column)
                })?;
                Ok(Value::Unit)
            }

            Stmt::BinaryWrite(path_expr, bytes_expr) => {
                let path = self.eval_expr(path_expr)?;
                let bytes = self.eval_expr(bytes_expr)?;
                let path_str = path.to_string();
                // For now, treat as string write
                let bytes_str = bytes.to_string();
                std::fs::write(&path_str, &bytes_str).map_err(|e| {
                    EvalError::new(format!("Failed to write file: {}", e), stmt.loc.line, stmt.loc.column)
                })?;
                Ok(Value::Unit)
            }

            Stmt::StructDef(_name, _fields) => {
                // TODO: Register struct type
                Ok(Value::Unit)
            }

            Stmt::TypeAlias(_name, _ty) => {
                // TODO: Register type alias
                Ok(Value::Unit)
            }

            Stmt::LibImport(_path) => {
                // TODO: Import library
                Ok(Value::Unit)
            }

            Stmt::AliasImport(_alias, _path) => {
                // TODO: Import with alias
                Ok(Value::Unit)
            }

            Stmt::RelImport(_path) => {
                // TODO: Relative import
                Ok(Value::Unit)
            }
        }
    }

    /// Evaluate an expression
    pub fn eval_expr(&mut self, expr: &Node<Expr>) -> Result<Value, EvalError> {
        let loc = expr.loc;

        match &expr.node {
            Expr::Lit(lit) => Ok(self.eval_literal(lit)),

            Expr::Var(name) => {
                self.env.borrow().get(name).ok_or_else(|| {
                    EvalError::new(format!("Undefined variable: {}", name), loc.line, loc.column)
                })
            }

            Expr::EnvVar(name) => {
                self.env.borrow().get_env_var(name)
                    .map(Value::String)
                    .ok_or_else(|| {
                        EvalError::new(format!("Undefined environment variable: {}", name), loc.line, loc.column)
                    })
            }

            Expr::BinOp(op, lhs, rhs) => {
                let lhs_val = self.eval_expr(lhs)?;
                let rhs_val = self.eval_expr(rhs)?;
                self.eval_binop(*op, lhs_val, rhs_val, loc.line, loc.column)
            }

            Expr::UnaryOp(op, operand) => {
                let val = self.eval_expr(operand)?;
                self.eval_unaryop(*op, val, loc.line, loc.column)
            }

            Expr::Call(func_expr, args) => {
                let func = self.eval_expr(func_expr)?;
                let mut arg_values = Vec::new();
                for arg in args {
                    arg_values.push(self.eval_expr(arg)?);
                }
                self.eval_call(func, arg_values, loc.line, loc.column)
            }

            Expr::List(items) => {
                let mut values = Vec::new();
                for item in items {
                    values.push(self.eval_expr(item)?);
                }
                Ok(Value::List(values))
            }

            Expr::Record(fields) => {
                let mut map = HashMap::new();
                for (name, value_expr) in fields {
                    map.insert(name.clone(), self.eval_expr(value_expr)?);
                }
                Ok(Value::Record(map))
            }

            Expr::Tuple(items) => {
                let mut values = Vec::new();
                for item in items {
                    values.push(self.eval_expr(item)?);
                }
                Ok(Value::Tuple(values))
            }

            Expr::Field(obj, field) => {
                let obj_val = self.eval_expr(obj)?;
                match obj_val {
                    Value::Record(fields) => {
                        fields.get(field).cloned().ok_or_else(|| {
                            EvalError::new(format!("Unknown field: {}", field), loc.line, loc.column)
                        })
                    }
                    _ => Err(EvalError::new("Cannot access field on non-record", loc.line, loc.column))
                }
            }

            Expr::Index(list_expr, index_expr) => {
                let list = self.eval_expr(list_expr)?;
                let index = self.eval_expr(index_expr)?;

                match (list, index) {
                    (Value::List(items), Value::Int(i)) => {
                        let idx = if i < 0 {
                            (items.len() as i64 + i) as usize
                        } else {
                            i as usize
                        };
                        items.get(idx).cloned().ok_or_else(|| {
                            EvalError::new(format!("Index out of bounds: {}", i), loc.line, loc.column)
                        })
                    }
                    (Value::String(s), Value::Int(i)) => {
                        let idx = if i < 0 {
                            (s.len() as i64 + i) as usize
                        } else {
                            i as usize
                        };
                        s.chars().nth(idx)
                            .map(|c| Value::String(c.to_string()))
                            .ok_or_else(|| {
                                EvalError::new(format!("Index out of bounds: {}", i), loc.line, loc.column)
                            })
                    }
                    _ => Err(EvalError::new("Invalid index operation", loc.line, loc.column))
                }
            }

            Expr::Lambda(params, body) => {
                let func = FuncDef::new(
                    "<lambda>".to_string(),
                    params.clone(),
                    (**body).clone(),
                    self.env.clone(),
                );
                Ok(Value::Func(Rc::new(func)))
            }

            Expr::Cond(cond, then_expr, else_expr) => {
                let cond_val = self.eval_expr(cond)?;
                if self.is_truthy(&cond_val) {
                    self.eval_expr(then_expr)
                } else {
                    self.eval_expr(else_expr)
                }
            }

            Expr::CondMulti(branches) => {
                for (cond, body) in branches {
                    let cond_val = self.eval_expr(cond)?;
                    if self.is_truthy(&cond_val) {
                        return self.eval_expr(body);
                    }
                }
                Ok(Value::Unit)
            }

            Expr::Match(scrutinee, branches) => {
                let val = self.eval_expr(scrutinee)?;

                for (pattern, body) in branches {
                    if let Some(bindings) = self.match_pattern(pattern, &val) {
                        let new_env = Env::with_parent(self.env.clone()).wrap();
                        for (name, value) in bindings {
                            new_env.borrow_mut().define(name, value);
                        }

                        let mut evaluator = Evaluator::with_env(new_env);
                        return evaluator.eval_expr(body);
                    }
                }

                Err(EvalError::new("No matching pattern", loc.line, loc.column))
            }

            Expr::Loop(cond, body) => {
                let mut result = Value::Unit;

                loop {
                    let cond_val = self.eval_expr(cond)?;
                    if !self.is_truthy(&cond_val) {
                        break;
                    }
                    result = self.eval_expr(body)?;
                }

                Ok(result)
            }

            Expr::Trap(body, err_name, handler) => {
                match self.eval_expr(body) {
                    Ok(val) => Ok(val),
                    Err(e) => {
                        let new_env = Env::with_parent(self.env.clone()).wrap();
                        new_env.borrow_mut().define(err_name.clone(), Value::String(e.message));

                        let mut evaluator = Evaluator::with_env(new_env);
                        evaluator.eval_expr(handler)
                    }
                }
            }

            Expr::Map(list_expr, var_name, body) => {
                let list = self.eval_expr(list_expr)?;

                match list {
                    Value::List(items) => {
                        let mut results = Vec::new();

                        for item in items {
                            let new_env = Env::with_parent(self.env.clone()).wrap();
                            new_env.borrow_mut().define(var_name.clone(), item);

                            let mut evaluator = Evaluator::with_env(new_env);
                            results.push(evaluator.eval_expr(body)?);
                        }

                        Ok(Value::List(results))
                    }
                    _ => Err(EvalError::new("%> requires a list", loc.line, loc.column))
                }
            }

            Expr::Filter(list_expr, var_name, body) => {
                let list = self.eval_expr(list_expr)?;

                match list {
                    Value::List(items) => {
                        let mut results = Vec::new();

                        for item in items {
                            let new_env = Env::with_parent(self.env.clone()).wrap();
                            new_env.borrow_mut().define(var_name.clone(), item.clone());

                            let mut evaluator = Evaluator::with_env(new_env);
                            let cond = evaluator.eval_expr(body)?;

                            if self.is_truthy(&cond) {
                                results.push(item);
                            }
                        }

                        Ok(Value::List(results))
                    }
                    _ => Err(EvalError::new("%< requires a list", loc.line, loc.column))
                }
            }

            Expr::Fold(list_expr, init_expr, acc_name, var_name, body) => {
                let list = self.eval_expr(list_expr)?;
                let init = self.eval_expr(init_expr)?;

                match list {
                    Value::List(items) => {
                        let mut acc = init;

                        for item in items {
                            let new_env = Env::with_parent(self.env.clone()).wrap();
                            new_env.borrow_mut().define(acc_name.clone(), acc);
                            new_env.borrow_mut().define(var_name.clone(), item);

                            let mut evaluator = Evaluator::with_env(new_env);
                            acc = evaluator.eval_expr(body)?;
                        }

                        Ok(acc)
                    }
                    _ => Err(EvalError::new("%/ requires a list", loc.line, loc.column))
                }
            }

            Expr::Times(n_expr, var_name, body) => {
                let n = self.eval_expr(n_expr)?;

                match n {
                    Value::Int(count) => {
                        let mut result = Value::Unit;

                        for i in 0..count {
                            let new_env = Env::with_parent(self.env.clone()).wrap();
                            new_env.borrow_mut().define(var_name.clone(), Value::Int(i));

                            let mut evaluator = Evaluator::with_env(new_env);
                            result = evaluator.eval_expr(body)?;
                        }

                        Ok(result)
                    }
                    _ => Err(EvalError::new("%~ requires an integer", loc.line, loc.column))
                }
            }

            Expr::Range(start_expr, end_expr) => {
                let start = self.eval_expr(start_expr)?;
                let end = self.eval_expr(end_expr)?;

                match (start, end) {
                    (Value::Int(s), Value::Int(e)) => {
                        let result: Vec<Value> = (s..e).map(Value::Int).collect();
                        Ok(Value::List(result))
                    }
                    _ => Err(EvalError::new("%.. requires integers", loc.line, loc.column))
                }
            }

            Expr::Exec(cmd) => {
                // !! takes 1 child: command string
                match exec::exec_shell(cmd) {
                    Ok(output) => Ok(Value::String(output.trim().to_string())),
                    Err(e) => Err(EvalError::new(e, loc.line, loc.column))
                }
            }

            Expr::CmdInterp(cmd) => {
                match exec::exec_shell(cmd) {
                    Ok(output) => Ok(Value::String(output.trim().to_string())),
                    Err(e) => Err(EvalError::new(e, loc.line, loc.column))
                }
            }

            Expr::Some(inner) => {
                let val = self.eval_expr(inner)?;
                Ok(Value::Some(Box::new(val)))
            }

            Expr::Ok(inner) => {
                let val = self.eval_expr(inner)?;
                Ok(Value::Ok(Box::new(val)))
            }

            Expr::Err(inner) => {
                let val = self.eval_expr(inner)?;
                Ok(Value::Err(Box::new(val)))
            }

            Expr::Block(stmts) => {
                let new_env = Env::with_parent(self.env.clone()).wrap();
                let mut evaluator = Evaluator::with_env(new_env);

                let mut result = Value::Unit;
                for stmt in stmts {
                    result = evaluator.eval_stmt(stmt)?;
                }
                Ok(result)
            }

            // Type cast: :> expr type
            Expr::Pipe(expr, type_expr) => {
                let value = self.eval_expr(expr)?;

                // Get type name from the right side
                let type_name = match &type_expr.node {
                    Expr::Var(name) => name.clone(),
                    _ => return Err(EvalError::new("Expected type for :>", loc.line, loc.column))
                };

                // Convert value to target type
                match type_name.as_str() {
                    // String conversion
                    name if name.contains("Str") || name == "t" => {
                        Ok(Value::String(value.to_string()))
                    }
                    // Integer conversion
                    name if name.contains("I32") || name.contains("I64") || name.contains("U") => {
                        match value {
                            Value::Int(n) => Ok(Value::Int(n)),
                            Value::Float(f) => Ok(Value::Int(f as i64)),
                            Value::String(s) => s.parse::<i64>()
                                .map(Value::Int)
                                .map_err(|_| EvalError::new("Cannot convert to integer", loc.line, loc.column)),
                            _ => Err(EvalError::new("Cannot convert to integer", loc.line, loc.column))
                        }
                    }
                    // Float conversion
                    name if name.contains("F32") || name.contains("F64") => {
                        match value {
                            Value::Int(n) => Ok(Value::Float(n as f64)),
                            Value::Float(f) => Ok(Value::Float(f)),
                            Value::String(s) => s.parse::<f64>()
                                .map(Value::Float)
                                .map_err(|_| EvalError::new("Cannot convert to float", loc.line, loc.column)),
                            _ => Err(EvalError::new("Cannot convert to float", loc.line, loc.column))
                        }
                    }
                    // Bool conversion
                    name if name.contains("Bool") || name == "b" => {
                        match value {
                            Value::Bool(b) => Ok(Value::Bool(b)),
                            Value::Int(n) => Ok(Value::Bool(n != 0)),
                            Value::String(s) => Ok(Value::Bool(!s.is_empty())),
                            _ => Err(EvalError::new("Cannot convert to bool", loc.line, loc.column))
                        }
                    }
                    // Default: return as-is
                    _ => Ok(Value::String(value.to_string()))
                }
            }

            // Type cast: :> expr type
            Expr::TypeCast(expr, ty) => {
                let value = self.eval_expr(expr)?;
                self.convert_type(value, ty, loc)
            }

            // Type check: :? expr type
            Expr::TypeCheck(expr, ty) => {
                let value = self.eval_expr(expr)?;
                let matches = self.check_type(&value, ty);
                Ok(Value::Bool(matches))
            }

            // Type of: :@ expr
            Expr::TypeOf(expr) => {
                let value = self.eval_expr(expr)?;
                let type_name = match value {
                    Value::Int(_) => "@i64",
                    Value::Float(_) => "@f64",
                    Value::String(_) => "@t",
                    Value::Bool(_) => "@b",
                    Value::List(_) => "@[]",
                    Value::Record(_) => "@{}",
                    Value::Tuple(_) => "@()",
                    Value::Some(_) => "@?",
                    Value::None => "@?",
                    Value::Ok(_) => "@!",
                    Value::Err(_) => "@!",
                    Value::Func(_) => "@fn",
                    Value::Builtin(_) => "@fn",
                    Value::Unit => "@unit",
                };
                Ok(Value::String(type_name.to_string()))
            }

            // Stdin: <~
            Expr::Stdin => {
                use std::io::{self, BufRead};
                let stdin = io::stdin();
                let line = stdin.lock().lines().next()
                    .ok_or_else(|| EvalError::new("Failed to read from stdin", loc.line, loc.column))?
                    .map_err(|e| EvalError::new(format!("IO error: {}", e), loc.line, loc.column))?;
                Ok(Value::String(line))
            }

            // File read: <+ path
            Expr::FileRead(path_expr) => {
                let path = self.eval_expr(path_expr)?;
                let path_str = path.to_string();
                let content = std::fs::read_to_string(&path_str).map_err(|e| {
                    EvalError::new(format!("Failed to read file: {}", e), loc.line, loc.column)
                })?;
                Ok(Value::String(content))
            }

            // Binary read: <* path
            Expr::BinaryRead(path_expr) => {
                let path = self.eval_expr(path_expr)?;
                let path_str = path.to_string();
                let bytes = std::fs::read(&path_str).map_err(|e| {
                    EvalError::new(format!("Failed to read file: {}", e), loc.line, loc.column)
                })?;
                let list: Vec<Value> = bytes.into_iter().map(|b| Value::Int(b as i64)).collect();
                Ok(Value::List(list))
            }

            // File write: ~+ path content (as expression)
            Expr::FileWrite(path_expr, content_expr) => {
                let path = self.eval_expr(path_expr)?;
                let content = self.eval_expr(content_expr)?;
                std::fs::write(path.to_string(), content.to_string()).map_err(|e| {
                    EvalError::new(format!("Failed to write file: {}", e), loc.line, loc.column)
                })?;
                Ok(Value::Unit)
            }

            // File append: ~^ path content (as expression)
            Expr::FileAppend(path_expr, content_expr) => {
                use std::io::Write;
                let path = self.eval_expr(path_expr)?;
                let content = self.eval_expr(content_expr)?;
                let mut file = std::fs::OpenOptions::new()
                    .append(true)
                    .create(true)
                    .open(path.to_string())
                    .map_err(|e| {
                        EvalError::new(format!("Failed to open file: {}", e), loc.line, loc.column)
                    })?;
                file.write_all(content.to_string().as_bytes()).map_err(|e| {
                    EvalError::new(format!("Failed to write file: {}", e), loc.line, loc.column)
                })?;
                Ok(Value::Unit)
            }

            // Binary write: ~* path bytes (as expression)
            Expr::BinaryWrite(path_expr, bytes_expr) => {
                let path = self.eval_expr(path_expr)?;
                let bytes = self.eval_expr(bytes_expr)?;
                std::fs::write(path.to_string(), bytes.to_string()).map_err(|e| {
                    EvalError::new(format!("Failed to write file: {}", e), loc.line, loc.column)
                })?;
                Ok(Value::Unit)
            }

            // Channel send: -> ch value
            Expr::ChanSend(_ch, _value) => {
                // TODO: Implement channel send
                Err(EvalError::new("Channels not yet implemented", loc.line, loc.column))
            }

            // Channel receive: <- ch
            Expr::ChanRecv(_ch) => {
                // TODO: Implement channel receive
                Err(EvalError::new("Channels not yet implemented", loc.line, loc.column))
            }

            // TODO: Implement remaining expressions
            _ => Err(EvalError::new("Not yet implemented", loc.line, loc.column))
        }
    }

    /// Convert a value to a specific type
    fn convert_type(&self, value: Value, ty: &Type, loc: Loc) -> Result<Value, EvalError> {
        match ty {
            Type::I8 | Type::I16 | Type::I32 | Type::I64 |
            Type::U8 | Type::U16 | Type::U32 | Type::U64 => {
                match value {
                    Value::Int(n) => Ok(Value::Int(n)),
                    Value::Float(f) => Ok(Value::Int(f as i64)),
                    Value::String(s) => s.parse::<i64>()
                        .map(Value::Int)
                        .map_err(|_| EvalError::new("Cannot convert to integer", loc.line, loc.column)),
                    Value::Bool(b) => Ok(Value::Int(if b { 1 } else { 0 })),
                    _ => Err(EvalError::new("Cannot convert to integer", loc.line, loc.column))
                }
            }
            Type::F32 | Type::F64 => {
                match value {
                    Value::Float(f) => Ok(Value::Float(f)),
                    Value::Int(n) => Ok(Value::Float(n as f64)),
                    Value::String(s) => s.parse::<f64>()
                        .map(Value::Float)
                        .map_err(|_| EvalError::new("Cannot convert to float", loc.line, loc.column)),
                    _ => Err(EvalError::new("Cannot convert to float", loc.line, loc.column))
                }
            }
            Type::Bool => {
                match value {
                    Value::Bool(b) => Ok(Value::Bool(b)),
                    Value::Int(n) => Ok(Value::Bool(n != 0)),
                    Value::String(s) => Ok(Value::Bool(!s.is_empty() && s != "false")),
                    _ => Err(EvalError::new("Cannot convert to bool", loc.line, loc.column))
                }
            }
            Type::Str => {
                Ok(Value::String(value.to_string()))
            }
            Type::Char => {
                match value {
                    Value::String(s) => {
                        if s.len() == 1 {
                            Ok(Value::String(s))
                        } else {
                            Err(EvalError::new("String must be single character", loc.line, loc.column))
                        }
                    }
                    Value::Int(n) => {
                        if n >= 0 && n <= 127 {
                            Ok(Value::String((n as u8 as char).to_string()))
                        } else {
                            Err(EvalError::new("Integer out of char range", loc.line, loc.column))
                        }
                    }
                    _ => Err(EvalError::new("Cannot convert to char", loc.line, loc.column))
                }
            }
            _ => Ok(value) // For other types, return as-is
        }
    }

    /// Check if a value matches a type
    fn check_type(&self, value: &Value, ty: &Type) -> bool {
        match (value, ty) {
            (Value::Int(_), Type::I8 | Type::I16 | Type::I32 | Type::I64 |
                           Type::U8 | Type::U16 | Type::U32 | Type::U64) => true,
            (Value::Float(_), Type::F32 | Type::F64) => true,
            (Value::Bool(_), Type::Bool) => true,
            (Value::String(_), Type::Str | Type::Char) => true,
            (Value::List(_), Type::List(_)) => true,
            (Value::Some(_) | Value::None, Type::Option(_)) => true,
            (Value::Ok(_) | Value::Err(_), Type::Result(_, _)) => true,
            _ => false
        }
    }

    fn eval_literal(&self, lit: &Literal) -> Value {
        match lit {
            Literal::Int(n) => Value::Int(*n),
            Literal::Float(n) => Value::Float(*n),
            Literal::String(s) => Value::String(s.clone()),
            Literal::Bool(b) => Value::Bool(*b),
            Literal::None => Value::None,
        }
    }

    fn eval_binop(&self, op: BinOp, lhs: Value, rhs: Value, line: usize, col: usize) -> Result<Value, EvalError> {
        match op {
            BinOp::Add => match (lhs, rhs) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
                (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 + b)),
                (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a + b as f64)),
                (Value::String(a), Value::String(b)) => Ok(Value::String(a + &b)),
                (Value::List(mut a), Value::List(b)) => { a.extend(b); Ok(Value::List(a)) }
                _ => Err(EvalError::new("Type mismatch for .+", line, col))
            },
            BinOp::Sub => match (lhs, rhs) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
                _ => Err(EvalError::new("Type mismatch for .-", line, col))
            },
            BinOp::Mul => match (lhs, rhs) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
                _ => Err(EvalError::new("Type mismatch for .*", line, col))
            },
            BinOp::Div => match (lhs, rhs) {
                (Value::Int(a), Value::Int(b)) => {
                    if b == 0 {
                        Err(EvalError::new("Division by zero", line, col))
                    } else {
                        Ok(Value::Int(a / b))
                    }
                }
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),
                _ => Err(EvalError::new("Type mismatch for ./", line, col))
            },
            BinOp::Mod => match (lhs, rhs) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a % b)),
                _ => Err(EvalError::new("Type mismatch for .%", line, col))
            },
            BinOp::Pow => match (lhs, rhs) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a.pow(b as u32))),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a.powf(b))),
                _ => Err(EvalError::new("Type mismatch for .^", line, col))
            },
            BinOp::Eq => Ok(Value::Bool(lhs == rhs)),
            BinOp::Neq => Ok(Value::Bool(lhs != rhs)),
            BinOp::Lt => match (lhs, rhs) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a < b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a < b)),
                (Value::String(a), Value::String(b)) => Ok(Value::Bool(a < b)),
                _ => Err(EvalError::new("Type mismatch for .<", line, col))
            },
            BinOp::Gt => match (lhs, rhs) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a > b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a > b)),
                (Value::String(a), Value::String(b)) => Ok(Value::Bool(a > b)),
                _ => Err(EvalError::new("Type mismatch for .>", line, col))
            },
            BinOp::Lte => match (lhs, rhs) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a <= b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a <= b)),
                _ => Err(EvalError::new("Type mismatch for .<=", line, col))
            },
            BinOp::Gte => match (lhs, rhs) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a >= b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(a >= b)),
                _ => Err(EvalError::new("Type mismatch for .>=", line, col))
            },
            BinOp::And => Ok(Value::Bool(self.is_truthy(&lhs) && self.is_truthy(&rhs))),
            BinOp::Or => Ok(Value::Bool(self.is_truthy(&lhs) || self.is_truthy(&rhs))),
        }
    }

    fn eval_unaryop(&self, op: UnaryOp, val: Value, line: usize, col: usize) -> Result<Value, EvalError> {
        match op {
            UnaryOp::Not => Ok(Value::Bool(!self.is_truthy(&val))),
            UnaryOp::Neg => match val {
                Value::Int(n) => Ok(Value::Int(-n)),
                Value::Float(n) => Ok(Value::Float(-n)),
                _ => Err(EvalError::new("Cannot negate non-number", line, col))
            }
        }
    }

    fn eval_call(&mut self, func: Value, args: Vec<Value>, line: usize, col: usize) -> Result<Value, EvalError> {
        match func {
            Value::Func(func_def) => {
                if args.len() != func_def.params.len() {
                    return Err(EvalError::new(
                        format!("Expected {} arguments, got {}", func_def.params.len(), args.len()),
                        line, col
                    ));
                }

                let new_env = Env::with_parent(func_def.closure.clone()).wrap();
                for (param, arg) in func_def.params.iter().zip(args) {
                    new_env.borrow_mut().define(param.clone(), arg);
                }

                let mut evaluator = Evaluator::with_env(new_env);
                evaluator.eval_expr(&func_def.body)
            }
            Value::Builtin(name) => {
                crate::builtin::call_builtin(&name, args, line, col)
            }
            _ => Err(EvalError::new("Not a function", line, col))
        }
    }

    fn is_truthy(&self, val: &Value) -> bool {
        match val {
            Value::Bool(b) => *b,
            Value::Int(n) => *n != 0,
            Value::Float(n) => *n != 0.0,
            Value::String(s) => !s.is_empty(),
            Value::List(l) => !l.is_empty(),
            Value::None => false,
            Value::Err(_) => false,
            _ => true,
        }
    }

    fn match_pattern(&self, pattern: &Pattern, value: &Value) -> Option<Vec<(String, Value)>> {
        match pattern {
            Pattern::Wildcard => Some(vec![]),

            Pattern::Literal(lit) => {
                let lit_val = self.eval_literal(lit);
                if lit_val == *value {
                    Some(vec![])
                } else {
                    None
                }
            }

            Pattern::Var(name) => {
                Some(vec![(name.clone(), value.clone())])
            }

            Pattern::None => {
                if *value == Value::None {
                    Some(vec![])
                } else {
                    None
                }
            }

            Pattern::Some(inner_pat) => {
                if let Value::Some(inner_val) = value {
                    self.match_pattern(inner_pat, inner_val)
                } else {
                    None
                }
            }

            Pattern::Ok(inner_pat) => {
                if let Value::Ok(inner_val) = value {
                    self.match_pattern(inner_pat, inner_val)
                } else {
                    None
                }
            }

            Pattern::Err(inner_pat) => {
                if let Value::Err(inner_val) = value {
                    self.match_pattern(inner_pat, inner_val)
                } else {
                    None
                }
            }

            Pattern::List(patterns, rest) => {
                if let Value::List(items) = value {
                    if items.len() < patterns.len() {
                        return None;
                    }

                    let mut bindings = vec![];

                    for (pat, val) in patterns.iter().zip(items.iter()) {
                        if let Some(b) = self.match_pattern(pat, val) {
                            bindings.extend(b);
                        } else {
                            return None;
                        }
                    }

                    if let Some(rest_name) = rest {
                        let rest_items: Vec<Value> = items[patterns.len()..].to_vec();
                        bindings.push((rest_name.clone(), Value::List(rest_items)));
                    } else if items.len() != patterns.len() {
                        return None;
                    }

                    Some(bindings)
                } else {
                    None
                }
            }

            Pattern::Range(start, end) => {
                if let Value::Int(n) = value {
                    if *n >= *start && *n <= *end {
                        Some(vec![])
                    } else {
                        None
                    }
                } else {
                    None
                }
            }

            _ => None,
        }
    }
}

impl Default for Evaluator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    fn eval(input: &str) -> Result<Value, EvalError> {
        let program = Parser::parse_source(input)
            .map_err(|e| EvalError::new(e.message, e.line, e.column))?;
        let mut evaluator = Evaluator::new();
        evaluator.eval_program(&program)
    }

    fn eval_ok(input: &str) -> Value {
        eval(input).expect(&format!("Failed to eval: {}", input))
    }

    // === Literals ===
    #[test]
    fn test_eval_int() {
        assert_eq!(eval_ok("42"), Value::Int(42));
    }

    #[test]
    fn test_eval_negative_int() {
        assert_eq!(eval_ok("-42"), Value::Int(-42));
    }

    #[test]
    fn test_eval_float() {
        assert_eq!(eval_ok("3.14"), Value::Float(3.14));
    }

    #[test]
    fn test_eval_string() {
        assert_eq!(eval_ok("\"hello\""), Value::String("hello".to_string()));
    }

    #[test]
    fn test_eval_bool() {
        assert_eq!(eval_ok("true"), Value::Bool(true));
        assert_eq!(eval_ok("false"), Value::Bool(false));
    }

    #[test]
    fn test_eval_none() {
        assert_eq!(eval_ok("none"), Value::None);
    }

    // === Arithmetic ===
    #[test]
    fn test_eval_add() {
        assert_eq!(eval_ok(".+ 1 2"), Value::Int(3));
    }

    #[test]
    fn test_eval_sub() {
        assert_eq!(eval_ok(".- 5 3"), Value::Int(2));
    }

    #[test]
    fn test_eval_mul() {
        assert_eq!(eval_ok(".* 4 5"), Value::Int(20));
    }

    #[test]
    fn test_eval_div() {
        assert_eq!(eval_ok("./ 10 2"), Value::Int(5));
    }

    #[test]
    fn test_eval_mod() {
        assert_eq!(eval_ok(".% 10 3"), Value::Int(1));
    }

    #[test]
    fn test_eval_pow() {
        assert_eq!(eval_ok(".^ 2 3"), Value::Int(8));
    }

    #[test]
    fn test_eval_nested_arithmetic() {
        assert_eq!(eval_ok(".* (.+ 1 2) (.+ 3 4)"), Value::Int(21));
    }

    #[test]
    fn test_eval_float_arithmetic() {
        assert_eq!(eval_ok(".+ 1.5 2.5"), Value::Float(4.0));
    }

    #[test]
    fn test_eval_string_concat() {
        assert_eq!(eval_ok(".+ \"hello\" \" world\""), Value::String("hello world".to_string()));
    }

    // === Comparison ===
    #[test]
    fn test_eval_eq() {
        assert_eq!(eval_ok(".= 1 1"), Value::Bool(true));
        assert_eq!(eval_ok(".= 1 2"), Value::Bool(false));
    }

    #[test]
    fn test_eval_neq() {
        assert_eq!(eval_ok(".~ 1 2"), Value::Bool(true));
        assert_eq!(eval_ok(".~ 1 1"), Value::Bool(false));
    }

    #[test]
    fn test_eval_lt() {
        assert_eq!(eval_ok(".< 1 2"), Value::Bool(true));
        assert_eq!(eval_ok(".< 2 1"), Value::Bool(false));
    }

    #[test]
    fn test_eval_gt() {
        assert_eq!(eval_ok(".> 2 1"), Value::Bool(true));
        assert_eq!(eval_ok(".> 1 2"), Value::Bool(false));
    }

    #[test]
    fn test_eval_lte() {
        assert_eq!(eval_ok(".<= 1 1"), Value::Bool(true));
        assert_eq!(eval_ok(".<= 1 2"), Value::Bool(true));
        assert_eq!(eval_ok(".<= 2 1"), Value::Bool(false));
    }

    #[test]
    fn test_eval_gte() {
        assert_eq!(eval_ok(".>= 1 1"), Value::Bool(true));
        assert_eq!(eval_ok(".>= 2 1"), Value::Bool(true));
        assert_eq!(eval_ok(".>= 1 2"), Value::Bool(false));
    }

    // === Logical ===
    #[test]
    fn test_eval_and() {
        assert_eq!(eval_ok(".& true true"), Value::Bool(true));
        assert_eq!(eval_ok(".& true false"), Value::Bool(false));
    }

    #[test]
    fn test_eval_or() {
        assert_eq!(eval_ok(".| true false"), Value::Bool(true));
        assert_eq!(eval_ok(".| false false"), Value::Bool(false));
    }

    #[test]
    fn test_eval_not() {
        assert_eq!(eval_ok(".! true"), Value::Bool(false));
        assert_eq!(eval_ok(".! false"), Value::Bool(true));
    }

    // === Variables ===
    #[test]
    fn test_eval_bind_and_use() {
        assert_eq!(eval_ok("^= x 42\n$x"), Value::Int(42));
    }

    #[test]
    fn test_eval_multiple_binds() {
        assert_eq!(eval_ok("^= x 10\n^= y 20\n.+ $x $y"), Value::Int(30));
    }

    // === List ===
    #[test]
    fn test_eval_list() {
        assert_eq!(eval_ok("[1 2 3]"), Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)]));
    }

    #[test]
    fn test_eval_empty_list() {
        assert_eq!(eval_ok("[]"), Value::List(vec![]));
    }

    #[test]
    fn test_eval_nested_list() {
        let result = eval_ok("[[1 2] [3 4]]");
        match result {
            Value::List(items) => {
                assert_eq!(items.len(), 2);
            }
            _ => panic!("Expected List"),
        }
    }

    // === Conditional ===
    #[test]
    fn test_eval_cond_true() {
        assert_eq!(eval_ok("?: true 1 2"), Value::Int(1));
    }

    #[test]
    fn test_eval_cond_false() {
        assert_eq!(eval_ok("?: false 1 2"), Value::Int(2));
    }

    #[test]
    fn test_eval_cond_multi() {
        assert_eq!(eval_ok("?: | false -> 1 | true -> 2 | _ -> 3"), Value::Int(2));
    }

    // === Function ===
    #[test]
    fn test_eval_function() {
        assert_eq!(eval_ok("|= Double [x] .* $x 2\n(Double 5)"), Value::Int(10));
    }

    #[test]
    fn test_eval_function_multiple_args() {
        assert_eq!(eval_ok("|= Add [a b] .+ $a $b\n(Add 3 4)"), Value::Int(7));
    }

    #[test]
    fn test_eval_recursive_function() {
        let code = "|= Factorial [n] ?: (.<= $n 1) 1 (.* $n (Factorial (.- $n 1)))\n(Factorial 5)";
        assert_eq!(eval_ok(code), Value::Int(120));
    }

    #[test]
    fn test_eval_lambda() {
        assert_eq!(eval_ok("^= double |> [x] .* $x 2\n($double 5)"), Value::Int(10));
    }

    // === Iteration ===
    #[test]
    fn test_eval_map() {
        let result = eval_ok("%> [1 2 3] [x] .* $x 2");
        assert_eq!(result, Value::List(vec![Value::Int(2), Value::Int(4), Value::Int(6)]));
    }

    #[test]
    fn test_eval_filter() {
        let result = eval_ok("%< [1 2 3 4 5] [x] .> $x 2");
        assert_eq!(result, Value::List(vec![Value::Int(3), Value::Int(4), Value::Int(5)]));
    }

    #[test]
    fn test_eval_fold() {
        let result = eval_ok("%/ [1 2 3 4 5] 0 [acc x] .+ $acc $x");
        assert_eq!(result, Value::Int(15));
    }

    #[test]
    fn test_eval_range() {
        let result = eval_ok("%.. 1 5");
        assert_eq!(result, Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3), Value::Int(4)]));
    }

    // === Builtin functions ===
    #[test]
    fn test_eval_builtin_len() {
        assert_eq!(eval_ok("(len [1 2 3])"), Value::Int(3));
    }

    #[test]
    fn test_eval_builtin_head() {
        assert_eq!(eval_ok("(head [1 2 3])"), Value::Int(1));
    }

    #[test]
    fn test_eval_builtin_tail() {
        assert_eq!(eval_ok("(tail [1 2 3])"), Value::List(vec![Value::Int(2), Value::Int(3)]));
    }

    #[test]
    fn test_eval_builtin_rev() {
        assert_eq!(eval_ok("(rev [1 2 3])"), Value::List(vec![Value::Int(3), Value::Int(2), Value::Int(1)]));
    }

    // === Option/Result ===
    #[test]
    fn test_eval_some() {
        assert_eq!(eval_ok("(some 42)"), Value::Some(Box::new(Value::Int(42))));
    }

    #[test]
    fn test_eval_ok_result() {
        assert_eq!(eval_ok("(ok 42)"), Value::Ok(Box::new(Value::Int(42))));
    }

    #[test]
    fn test_eval_err_result() {
        let result = eval_ok("(err \"oops\")");
        assert_eq!(result, Value::Err(Box::new(Value::String("oops".to_string()))));
    }

    #[test]
    fn test_eval_unwrap() {
        assert_eq!(eval_ok("(unwrap (some 42))"), Value::Int(42));
    }

    #[test]
    fn test_eval_unwrap_or() {
        assert_eq!(eval_ok("(unwrap-or none 0)"), Value::Int(0));
        assert_eq!(eval_ok("(unwrap-or (some 42) 0)"), Value::Int(42));
    }

    // === Pattern Matching ===
    #[test]
    fn test_eval_match_literal() {
        let code = "?? 2 | 1 -> \"one\" | 2 -> \"two\" | _ -> \"other\"";
        assert_eq!(eval_ok(code), Value::String("two".to_string()));
    }

    #[test]
    fn test_eval_match_wildcard() {
        let code = "?? 99 | 1 -> \"one\" | _ -> \"other\"";
        assert_eq!(eval_ok(code), Value::String("other".to_string()));
    }

    #[test]
    fn test_eval_match_var() {
        let code = "?? 42 | x -> .* $x 2";
        assert_eq!(eval_ok(code), Value::Int(84));
    }

    // === Block ===
    #[test]
    fn test_eval_block() {
        assert_eq!(eval_ok("{ ^= x 1; ^= y 2; .+ $x $y }"), Value::Int(3));
    }

    // === Division by zero ===
    #[test]
    fn test_eval_div_by_zero() {
        assert!(eval("./ 1 0").is_err());
    }

    // === Undefined variable ===
    #[test]
    fn test_eval_undefined_var() {
        assert!(eval("$undefined").is_err());
    }

    // === Type mismatch ===
    #[test]
    fn test_eval_type_mismatch() {
        assert!(eval(".+ 1 \"hello\"").is_err());
    }
}