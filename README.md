# Hedgehog 🦔

A keyword-less shell scripting language.

## Philosophy

- **Berry-based syntax**: No keywords. Control flow, iteration, and binding all use symbolic operators called Berries.
- **Fixed arity**: Every construct takes a known number of arguments. No ambiguity in parsing.
- **Immutable by default**: Bindings cannot be reassigned. State is explicit.
- **Totality**: No infinite loops, no partial functions. Deterministic execution.

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
# Build
cargo build --release

# Run a script
hog script.hog

# Start REPL
hog

# Run one-liner
hog -c "~> 42"
```

## Demo

```hog
# Bind
^= name "Alice"
^= age 30

# Output with interpolation
~> "Hello, $name!"
~> "Age: $age"

# List
^= nums [1 2 3 4 5]

# Map
%> $nums [x] .* $x 2          # [2 4 6 8 10]

# Filter
%< $nums [x] .> $x 2          # [3 4 5]

# Fold
%/ $nums 0 [acc x] .+ $acc $x # 15

# Conditional
?: (.> $age 18) "adult" "minor"

# Function
|= Double [x] .* $x 2
~> (Double 21)                # 42

# Shell execution
^= result !! `ls -la`
~> $result.out
```

No `if`. No `for`. No `while`. Just Berries.

## Status

v0 — experimental. Syntax and semantics may change.

## License

This project is open source and available under the [MIT License](LICENSE).
