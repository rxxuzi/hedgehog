# Hedgehog 🦔

A keyword-less shell scripting language.

## Philosophy

Hedgehog rejects the modern trend of turning shell scripts into full programming languages. Instead, it returns to Unix roots: small, composable, predictable.

- **Berry-based syntax**: No keywords. Control flow, iteration, and binding all use symbolic operators called Berries.
- **Fixed arity**: Every construct takes a known number of arguments. No ambiguity in parsing.
- **Immutable by default**: Bindings cannot be reassigned. State is explicit.
- **Parallel primitives**: Concurrency is built-in, not bolted on.

## Why Not Python?

Python is a general-purpose language pretending to be a scripting tool. It works, but at a cost:

- Verbose syntax for simple file operations
- Implicit behaviors that hide bugs
- No native support for pipelines or parallelism

Hedgehog is shell-first. It does one thing well: orchestrate commands and data with minimal ceremony.

## Features

- No `if`, `else`, `for`, `while` — only Berries
- No `null` — only `Option` (`some` / `none`)
- No mutable state by default
- Built-in parallel iteration
- Cross-platform (Linux, macOS, Windows)


## Quick Start
```sh
# Install
cargo install hedgehog

# Run a script
hog script.hog

# Start REPL
hog
```

## Demo
```hog
# Bind a variable
^= name "world"

# Output
~> "Hello, $name!"

# Input
~| "Enter your name: "
^= input <~

# List operations
^= nums [1 2 3 4 5]

# Map: double each element
%> $nums [x] .* $x 2          # [2 4 6 8 10]

# Filter: keep only > 2
%< $nums [x] .> $x 2          # [3 4 5]

# Fold: sum all
%/ $nums 0 [acc x] .+ $acc $x # 15

# Conditional
?: (.> $x 0) "positive" "negative"

# Shell execution
^= result !! `ls -la`
~> $result.out

# File I/O
^= content <+ "./config.txt"
~+ "./output.txt" $content
```

No `if`. No `for`. No `while`. Just Berries.

## Status

v0 — experimental. Syntax and semantics may change.

## License

This project is open source and available under the [MIT License](LICENSE).
