//! Hedgehog Built-in Functions
//!
//! Core functions available without imports.

use crate::eval::{EvalError, Value};

/// Call a built-in function
pub fn call_builtin(name: &str, args: Vec<Value>, line: usize, col: usize) -> Result<Value, EvalError> {
    match name {
        // === List operations ===
        "len" => {
            check_arity(name, &args, 1, line, col)?;
            match &args[0] {
                Value::List(l) => Ok(Value::Int(l.len() as i64)),
                Value::String(s) => Ok(Value::Int(s.len() as i64)),
                _ => Err(EvalError::new("len requires a list or string", line, col))
            }
        }

        "head" => {
            check_arity(name, &args, 1, line, col)?;
            match &args[0] {
                Value::List(l) => {
                    l.first().cloned().ok_or_else(|| {
                        EvalError::new("head on empty list", line, col)
                    })
                }
                Value::String(s) => {
                    s.chars().next()
                        .map(|c| Value::String(c.to_string()))
                        .ok_or_else(|| EvalError::new("head on empty string", line, col))
                }
                _ => Err(EvalError::new("head requires a list or string", line, col))
            }
        }

        "tail" => {
            check_arity(name, &args, 1, line, col)?;
            match &args[0] {
                Value::List(l) => {
                    if l.is_empty() {
                        Err(EvalError::new("tail on empty list", line, col))
                    } else {
                        Ok(Value::List(l[1..].to_vec()))
                    }
                }
                Value::String(s) => {
                    if s.is_empty() {
                        Err(EvalError::new("tail on empty string", line, col))
                    } else {
                        Ok(Value::String(s[1..].to_string()))
                    }
                }
                _ => Err(EvalError::new("tail requires a list or string", line, col))
            }
        }

        "last" => {
            check_arity(name, &args, 1, line, col)?;
            match &args[0] {
                Value::List(l) => {
                    l.last().cloned().ok_or_else(|| {
                        EvalError::new("last on empty list", line, col)
                    })
                }
                Value::String(s) => {
                    s.chars().last()
                        .map(|c| Value::String(c.to_string()))
                        .ok_or_else(|| EvalError::new("last on empty string", line, col))
                }
                _ => Err(EvalError::new("last requires a list or string", line, col))
            }
        }

        "init" => {
            check_arity(name, &args, 1, line, col)?;
            match &args[0] {
                Value::List(l) => {
                    if l.is_empty() {
                        Err(EvalError::new("init on empty list", line, col))
                    } else {
                        Ok(Value::List(l[..l.len()-1].to_vec()))
                    }
                }
                _ => Err(EvalError::new("init requires a list", line, col))
            }
        }

        "nth" => {
            check_arity(name, &args, 2, line, col)?;
            match (&args[0], &args[1]) {
                (Value::List(l), Value::Int(i)) => {
                    let idx = if *i < 0 {
                        (l.len() as i64 + i) as usize
                    } else {
                        *i as usize
                    };
                    l.get(idx).cloned().ok_or_else(|| {
                        EvalError::new(format!("index {} out of bounds", i), line, col)
                    })
                }
                _ => Err(EvalError::new("nth requires a list and integer", line, col))
            }
        }

        "rev" => {
            check_arity(name, &args, 1, line, col)?;
            match &args[0] {
                Value::List(l) => {
                    let mut reversed = l.clone();
                    reversed.reverse();
                    Ok(Value::List(reversed))
                }
                Value::String(s) => {
                    Ok(Value::String(s.chars().rev().collect()))
                }
                _ => Err(EvalError::new("rev requires a list or string", line, col))
            }
        }

        // range function for step support (since %.. is 2-fixed)
        "range" => {
            check_arity(name, &args, 3, line, col)?;
            match (&args[0], &args[1], &args[2]) {
                (Value::Int(start), Value::Int(end), Value::Int(step)) => {
                    let mut result = Vec::new();
                    let mut i = *start;

                    if *step > 0 {
                        while i < *end {
                            result.push(Value::Int(i));
                            i += step;
                        }
                    } else if *step < 0 {
                        while i > *end {
                            result.push(Value::Int(i));
                            i += step;
                        }
                    }
                    // step == 0 returns empty list

                    Ok(Value::List(result))
                }
                _ => Err(EvalError::new("range requires 3 integers: start, end, step", line, col))
            }
        }

        "sort" => {
            check_arity(name, &args, 1, line, col)?;
            match &args[0] {
                Value::List(l) => {
                    let mut sorted = l.clone();
                    sorted.sort_by(|a, b| {
                        match (a, b) {
                            (Value::Int(x), Value::Int(y)) => x.cmp(y),
                            (Value::Float(x), Value::Float(y)) => x.partial_cmp(y).unwrap_or(std::cmp::Ordering::Equal),
                            (Value::String(x), Value::String(y)) => x.cmp(y),
                            _ => std::cmp::Ordering::Equal,
                        }
                    });
                    Ok(Value::List(sorted))
                }
                _ => Err(EvalError::new("sort requires a list", line, col))
            }
        }

        "uniq" => {
            check_arity(name, &args, 1, line, col)?;
            match &args[0] {
                Value::List(l) => {
                    let mut unique = Vec::new();
                    for item in l {
                        if !unique.contains(item) {
                            unique.push(item.clone());
                        }
                    }
                    Ok(Value::List(unique))
                }
                _ => Err(EvalError::new("uniq requires a list", line, col))
            }
        }

        "concat" => {
            check_arity(name, &args, 2, line, col)?;
            match (&args[0], &args[1]) {
                (Value::List(a), Value::List(b)) => {
                    let mut result = a.clone();
                    result.extend(b.clone());
                    Ok(Value::List(result))
                }
                (Value::String(a), Value::String(b)) => {
                    Ok(Value::String(format!("{}{}", a, b)))
                }
                _ => Err(EvalError::new("concat requires two lists or strings", line, col))
            }
        }

        "flatten" => {
            check_arity(name, &args, 1, line, col)?;
            match &args[0] {
                Value::List(l) => {
                    let mut result = Vec::new();
                    for item in l {
                        match item {
                            Value::List(inner) => result.extend(inner.clone()),
                            _ => result.push(item.clone()),
                        }
                    }
                    Ok(Value::List(result))
                }
                _ => Err(EvalError::new("flatten requires a list", line, col))
            }
        }

        // === String operations ===
        "split" => {
            check_arity(name, &args, 2, line, col)?;
            match (&args[0], &args[1]) {
                (Value::String(s), Value::String(sep)) => {
                    let parts: Vec<Value> = s.split(sep.as_str())
                        .map(|p| Value::String(p.to_string()))
                        .collect();
                    Ok(Value::List(parts))
                }
                _ => Err(EvalError::new("split requires two strings", line, col))
            }
        }

        "join" => {
            check_arity(name, &args, 2, line, col)?;
            match (&args[0], &args[1]) {
                (Value::List(l), Value::String(sep)) => {
                    let strings: Result<Vec<String>, _> = l.iter().map(|v| {
                        match v {
                            Value::String(s) => Ok(s.clone()),
                            _ => Ok(v.to_string()),
                        }
                    }).collect();
                    Ok(Value::String(strings?.join(sep)))
                }
                _ => Err(EvalError::new("join requires a list and string", line, col))
            }
        }

        "trim" => {
            check_arity(name, &args, 1, line, col)?;
            match &args[0] {
                Value::String(s) => Ok(Value::String(s.trim().to_string())),
                _ => Err(EvalError::new("trim requires a string", line, col))
            }
        }

        "upper" => {
            check_arity(name, &args, 1, line, col)?;
            match &args[0] {
                Value::String(s) => Ok(Value::String(s.to_uppercase())),
                _ => Err(EvalError::new("upper requires a string", line, col))
            }
        }

        "lower" => {
            check_arity(name, &args, 1, line, col)?;
            match &args[0] {
                Value::String(s) => Ok(Value::String(s.to_lowercase())),
                _ => Err(EvalError::new("lower requires a string", line, col))
            }
        }

        "replace" => {
            check_arity(name, &args, 3, line, col)?;
            match (&args[0], &args[1], &args[2]) {
                (Value::String(s), Value::String(from), Value::String(to)) => {
                    Ok(Value::String(s.replace(from.as_str(), to.as_str())))
                }
                _ => Err(EvalError::new("replace requires three strings", line, col))
            }
        }

        "contains" => {
            check_arity(name, &args, 2, line, col)?;
            match (&args[0], &args[1]) {
                (Value::String(s), Value::String(sub)) => {
                    Ok(Value::Bool(s.contains(sub.as_str())))
                }
                (Value::List(l), val) => {
                    Ok(Value::Bool(l.contains(val)))
                }
                _ => Err(EvalError::new("contains requires string/string or list/value", line, col))
            }
        }

        "starts" => {
            check_arity(name, &args, 2, line, col)?;
            match (&args[0], &args[1]) {
                (Value::String(s), Value::String(prefix)) => {
                    Ok(Value::Bool(s.starts_with(prefix.as_str())))
                }
                _ => Err(EvalError::new("starts requires two strings", line, col))
            }
        }

        "ends" => {
            check_arity(name, &args, 2, line, col)?;
            match (&args[0], &args[1]) {
                (Value::String(s), Value::String(suffix)) => {
                    Ok(Value::Bool(s.ends_with(suffix.as_str())))
                }
                _ => Err(EvalError::new("ends requires two strings", line, col))
            }
        }

        // === Type conversion ===
        "int" => {
            check_arity(name, &args, 1, line, col)?;
            match &args[0] {
                Value::Int(n) => Ok(Value::Int(*n)),
                Value::Float(n) => Ok(Value::Int(*n as i64)),
                Value::String(s) => {
                    s.parse::<i64>()
                        .map(Value::Int)
                        .map_err(|_| EvalError::new(format!("Cannot parse '{}' as int", s), line, col))
                }
                Value::Bool(b) => Ok(Value::Int(if *b { 1 } else { 0 })),
                _ => Err(EvalError::new("Cannot convert to int", line, col))
            }
        }

        "float" => {
            check_arity(name, &args, 1, line, col)?;
            match &args[0] {
                Value::Int(n) => Ok(Value::Float(*n as f64)),
                Value::Float(n) => Ok(Value::Float(*n)),
                Value::String(s) => {
                    s.parse::<f64>()
                        .map(Value::Float)
                        .map_err(|_| EvalError::new(format!("Cannot parse '{}' as float", s), line, col))
                }
                _ => Err(EvalError::new("Cannot convert to float", line, col))
            }
        }

        "str" => {
            check_arity(name, &args, 1, line, col)?;
            Ok(Value::String(args[0].to_string()))
        }

        "bool" => {
            check_arity(name, &args, 1, line, col)?;
            let truthy = match &args[0] {
                Value::Bool(b) => *b,
                Value::Int(n) => *n != 0,
                Value::Float(n) => *n != 0.0,
                Value::String(s) => !s.is_empty(),
                Value::List(l) => !l.is_empty(),
                Value::None => false,
                _ => true,
            };
            Ok(Value::Bool(truthy))
        }

        // === Option/Result ===
        "some" => {
            check_arity(name, &args, 1, line, col)?;
            Ok(Value::Some(Box::new(args[0].clone())))
        }

        "ok" => {
            check_arity(name, &args, 1, line, col)?;
            Ok(Value::Ok(Box::new(args[0].clone())))
        }

        "err" => {
            check_arity(name, &args, 1, line, col)?;
            Ok(Value::Err(Box::new(args[0].clone())))
        }

        "some?" => {
            check_arity(name, &args, 1, line, col)?;
            Ok(Value::Bool(matches!(&args[0], Value::Some(_))))
        }

        "none?" => {
            check_arity(name, &args, 1, line, col)?;
            Ok(Value::Bool(matches!(&args[0], Value::None)))
        }

        "ok?" => {
            check_arity(name, &args, 1, line, col)?;
            Ok(Value::Bool(matches!(&args[0], Value::Ok(_))))
        }

        "err?" => {
            check_arity(name, &args, 1, line, col)?;
            Ok(Value::Bool(matches!(&args[0], Value::Err(_))))
        }

        "unwrap" => {
            check_arity(name, &args, 1, line, col)?;
            match &args[0] {
                Value::Some(v) => Ok((**v).clone()),
                Value::Ok(v) => Ok((**v).clone()),
                Value::None => Err(EvalError::new("unwrap on None", line, col)),
                Value::Err(e) => Err(EvalError::new(format!("unwrap on Err: {}", e), line, col)),
                _ => Err(EvalError::new("unwrap requires Option or Result", line, col))
            }
        }

        "unwrap-or" => {
            check_arity(name, &args, 2, line, col)?;
            match &args[0] {
                Value::Some(v) => Ok((**v).clone()),
                Value::Ok(v) => Ok((**v).clone()),
                Value::None => Ok(args[1].clone()),
                Value::Err(_) => Ok(args[1].clone()),
                _ => Err(EvalError::new("unwrap-or requires Option or Result", line, col))
            }
        }

        // === Unknown ===
        _ => Err(EvalError::new(format!("Unknown built-in: {}", name), line, col))
    }
}

fn check_arity(name: &str, args: &[Value], expected: usize, line: usize, col: usize) -> Result<(), EvalError> {
    if args.len() != expected {
        Err(EvalError::new(
            format!("{} expects {} argument(s), got {}", name, expected, args.len()),
            line, col
        ))
    } else {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn call(name: &str, args: Vec<Value>) -> Result<Value, EvalError> {
        call_builtin(name, args, 1, 1)
    }

    // === List operations ===
    #[test]
    fn test_len_list() {
        let result = call("len", vec![Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)])]);
        assert_eq!(result.unwrap(), Value::Int(3));
    }

    #[test]
    fn test_len_string() {
        let result = call("len", vec![Value::String("hello".to_string())]);
        assert_eq!(result.unwrap(), Value::Int(5));
    }

    #[test]
    fn test_len_empty() {
        let result = call("len", vec![Value::List(vec![])]);
        assert_eq!(result.unwrap(), Value::Int(0));
    }

    #[test]
    fn test_head() {
        let result = call("head", vec![Value::List(vec![Value::Int(1), Value::Int(2)])]);
        assert_eq!(result.unwrap(), Value::Int(1));
    }

    #[test]
    fn test_head_empty() {
        let result = call("head", vec![Value::List(vec![])]);
        assert!(result.is_err());
    }

    #[test]
    fn test_tail() {
        let result = call("tail", vec![Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)])]);
        assert_eq!(result.unwrap(), Value::List(vec![Value::Int(2), Value::Int(3)]));
    }

    #[test]
    fn test_last() {
        let result = call("last", vec![Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)])]);
        assert_eq!(result.unwrap(), Value::Int(3));
    }

    #[test]
    fn test_init() {
        let result = call("init", vec![Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)])]);
        assert_eq!(result.unwrap(), Value::List(vec![Value::Int(1), Value::Int(2)]));
    }

    #[test]
    fn test_rev() {
        let result = call("rev", vec![Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)])]);
        assert_eq!(result.unwrap(), Value::List(vec![Value::Int(3), Value::Int(2), Value::Int(1)]));
    }

    #[test]
    fn test_rev_string() {
        let result = call("rev", vec![Value::String("hello".to_string())]);
        assert_eq!(result.unwrap(), Value::String("olleh".to_string()));
    }

    #[test]
    fn test_sort() {
        let result = call("sort", vec![Value::List(vec![Value::Int(3), Value::Int(1), Value::Int(2)])]);
        assert_eq!(result.unwrap(), Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)]));
    }

    #[test]
    fn test_uniq() {
        let result = call("uniq", vec![Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(1), Value::Int(3)])]);
        assert_eq!(result.unwrap(), Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)]));
    }

    #[test]
    fn test_concat_lists() {
        let result = call("concat", vec![
            Value::List(vec![Value::Int(1), Value::Int(2)]),
            Value::List(vec![Value::Int(3), Value::Int(4)])
        ]);
        assert_eq!(result.unwrap(), Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3), Value::Int(4)]));
    }

    #[test]
    fn test_concat_strings() {
        let result = call("concat", vec![
            Value::String("hello".to_string()),
            Value::String(" world".to_string())
        ]);
        assert_eq!(result.unwrap(), Value::String("hello world".to_string()));
    }

    #[test]
    fn test_flatten() {
        let result = call("flatten", vec![Value::List(vec![
            Value::List(vec![Value::Int(1), Value::Int(2)]),
            Value::List(vec![Value::Int(3)])
        ])]);
        assert_eq!(result.unwrap(), Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)]));
    }

    // === String operations ===
    #[test]
    fn test_split() {
        let result = call("split", vec![
            Value::String("a,b,c".to_string()),
            Value::String(",".to_string())
        ]);
        assert_eq!(result.unwrap(), Value::List(vec![
            Value::String("a".to_string()),
            Value::String("b".to_string()),
            Value::String("c".to_string())
        ]));
    }

    #[test]
    fn test_join() {
        let result = call("join", vec![
            Value::List(vec![
                Value::String("a".to_string()),
                Value::String("b".to_string())
            ]),
            Value::String(",".to_string())
        ]);
        assert_eq!(result.unwrap(), Value::String("a,b".to_string()));
    }

    #[test]
    fn test_trim() {
        let result = call("trim", vec![Value::String("  hello  ".to_string())]);
        assert_eq!(result.unwrap(), Value::String("hello".to_string()));
    }

    #[test]
    fn test_upper() {
        let result = call("upper", vec![Value::String("hello".to_string())]);
        assert_eq!(result.unwrap(), Value::String("HELLO".to_string()));
    }

    #[test]
    fn test_lower() {
        let result = call("lower", vec![Value::String("HELLO".to_string())]);
        assert_eq!(result.unwrap(), Value::String("hello".to_string()));
    }

    #[test]
    fn test_replace() {
        let result = call("replace", vec![
            Value::String("hello world".to_string()),
            Value::String("world".to_string()),
            Value::String("rust".to_string())
        ]);
        assert_eq!(result.unwrap(), Value::String("hello rust".to_string()));
    }

    #[test]
    fn test_contains_string() {
        let result = call("contains", vec![
            Value::String("hello world".to_string()),
            Value::String("world".to_string())
        ]);
        assert_eq!(result.unwrap(), Value::Bool(true));
    }

    #[test]
    fn test_contains_list() {
        let result = call("contains", vec![
            Value::List(vec![Value::Int(1), Value::Int(2), Value::Int(3)]),
            Value::Int(2)
        ]);
        assert_eq!(result.unwrap(), Value::Bool(true));
    }

    #[test]
    fn test_starts() {
        let result = call("starts", vec![
            Value::String("hello world".to_string()),
            Value::String("hello".to_string())
        ]);
        assert_eq!(result.unwrap(), Value::Bool(true));
    }

    #[test]
    fn test_ends() {
        let result = call("ends", vec![
            Value::String("hello world".to_string()),
            Value::String("world".to_string())
        ]);
        assert_eq!(result.unwrap(), Value::Bool(true));
    }

    // === Type conversion ===
    #[test]
    fn test_int_from_float() {
        let result = call("int", vec![Value::Float(3.7)]);
        assert_eq!(result.unwrap(), Value::Int(3));
    }

    #[test]
    fn test_int_from_string() {
        let result = call("int", vec![Value::String("42".to_string())]);
        assert_eq!(result.unwrap(), Value::Int(42));
    }

    #[test]
    fn test_int_from_bool() {
        assert_eq!(call("int", vec![Value::Bool(true)]).unwrap(), Value::Int(1));
        assert_eq!(call("int", vec![Value::Bool(false)]).unwrap(), Value::Int(0));
    }

    #[test]
    fn test_float_from_int() {
        let result = call("float", vec![Value::Int(42)]);
        assert_eq!(result.unwrap(), Value::Float(42.0));
    }

    #[test]
    fn test_float_from_string() {
        let result = call("float", vec![Value::String("3.14".to_string())]);
        assert_eq!(result.unwrap(), Value::Float(3.14));
    }

    #[test]
    fn test_str() {
        assert_eq!(call("str", vec![Value::Int(42)]).unwrap(), Value::String("42".to_string()));
        assert_eq!(call("str", vec![Value::Bool(true)]).unwrap(), Value::String("true".to_string()));
    }

    #[test]
    fn test_bool() {
        assert_eq!(call("bool", vec![Value::Int(0)]).unwrap(), Value::Bool(false));
        assert_eq!(call("bool", vec![Value::Int(1)]).unwrap(), Value::Bool(true));
        assert_eq!(call("bool", vec![Value::String("".to_string())]).unwrap(), Value::Bool(false));
        assert_eq!(call("bool", vec![Value::String("x".to_string())]).unwrap(), Value::Bool(true));
        assert_eq!(call("bool", vec![Value::None]).unwrap(), Value::Bool(false));
    }

    // === Option/Result ===
    #[test]
    fn test_some() {
        let result = call("some", vec![Value::Int(42)]);
        assert_eq!(result.unwrap(), Value::Some(Box::new(Value::Int(42))));
    }

    #[test]
    fn test_ok() {
        let result = call("ok", vec![Value::Int(42)]);
        assert_eq!(result.unwrap(), Value::Ok(Box::new(Value::Int(42))));
    }

    #[test]
    fn test_err() {
        let result = call("err", vec![Value::String("oops".to_string())]);
        assert_eq!(result.unwrap(), Value::Err(Box::new(Value::String("oops".to_string()))));
    }

    #[test]
    fn test_some_check() {
        assert_eq!(call("some?", vec![Value::Some(Box::new(Value::Int(1)))]).unwrap(), Value::Bool(true));
        assert_eq!(call("some?", vec![Value::None]).unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_none_check() {
        assert_eq!(call("none?", vec![Value::None]).unwrap(), Value::Bool(true));
        assert_eq!(call("none?", vec![Value::Some(Box::new(Value::Int(1)))]).unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_ok_check() {
        assert_eq!(call("ok?", vec![Value::Ok(Box::new(Value::Int(1)))]).unwrap(), Value::Bool(true));
        assert_eq!(call("ok?", vec![Value::Err(Box::new(Value::Int(1)))]).unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_err_check() {
        assert_eq!(call("err?", vec![Value::Err(Box::new(Value::Int(1)))]).unwrap(), Value::Bool(true));
        assert_eq!(call("err?", vec![Value::Ok(Box::new(Value::Int(1)))]).unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_unwrap_some() {
        let result = call("unwrap", vec![Value::Some(Box::new(Value::Int(42)))]);
        assert_eq!(result.unwrap(), Value::Int(42));
    }

    #[test]
    fn test_unwrap_ok() {
        let result = call("unwrap", vec![Value::Ok(Box::new(Value::Int(42)))]);
        assert_eq!(result.unwrap(), Value::Int(42));
    }

    #[test]
    fn test_unwrap_none_error() {
        let result = call("unwrap", vec![Value::None]);
        assert!(result.is_err());
    }

    #[test]
    fn test_unwrap_or_some() {
        let result = call("unwrap-or", vec![Value::Some(Box::new(Value::Int(42))), Value::Int(0)]);
        assert_eq!(result.unwrap(), Value::Int(42));
    }

    #[test]
    fn test_unwrap_or_none() {
        let result = call("unwrap-or", vec![Value::None, Value::Int(0)]);
        assert_eq!(result.unwrap(), Value::Int(0));
    }

    // === Arity errors ===
    #[test]
    fn test_arity_error() {
        let result = call("len", vec![]);
        assert!(result.is_err());
    }

    #[test]
    fn test_unknown_builtin() {
        let result = call("unknown_function", vec![]);
        assert!(result.is_err());
    }
}