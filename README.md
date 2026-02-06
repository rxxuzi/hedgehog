# Hedgehog 🦔

A keyword-less shell scripting language with deterministic semantics.

## Why Not Python?

Python promised simplicity. What it delivered:

- **Verbose for shell tasks** — Running `ls` requires `subprocess.run(['ls', '-la'], capture_output=True)`. In Hedgehog: `!< \`ls -la\``. That's it.
- **Implicit behaviors** — Magic methods, dynamic typing, hidden state mutations. Bugs love hiding there.
- **Non-deterministic execution** — Race conditions, unpredictable GC pauses, async chaos.
- **No native parallelism** — GIL says no. Multiprocessing is a band-aid.
- **Dependency hell** — pip, conda, venv, poetry, pipenv... which one today?

Python is a general-purpose language pretending to be a shell tool. It works, but at a cost.

Hedgehog is shell-first. It does one thing well: orchestrate commands and data with minimal ceremony.

## Philosophy

Hedgehog takes a different path:

**Berry-based syntax** — No keywords. Control flow uses symbolic operators called "Berries" (`?:`, `%>`, `|=`). Every construct has fixed arity—no ambiguity in what goes where.

**Totality** — Programs always terminate. No infinite loops, no partial functions. If it compiles, it runs to completion.

**Immutable by default** — Bindings can't be reassigned. State changes are explicit, tracked, and intentional.

**Shell-first design** — Command execution is a first-class operation, not an afterthought. Pipelines and parallelism are native.

**No FFI** — Intentionally isolated from C legacy. Pure Hedgehog, pure determinism.


## Quick Start

```sh
# Build
cargo build --release

# Run script
hog script.hog

# REPL
hog

# One-liner
hog -c '~> "Hello, World!"'

# Syntax check
hog -n script.hog
```

## Demo

### Basics

```hog
# Bind (no 'let', no 'var', no 'const')
^= name "Alice"
^= age 30

# Output with interpolation
~> "Hello, $name!"

# Conditional (no 'if')
?: (.> $age 18) "adult" "minor"

# Function (no 'def', no 'fn')
|= Double [x] .* $x 2
~> (Double 21)    # 42
```

### Collections

```hog
^= nums [1 2 3 4 5]

# Map
%> $nums [x] .* $x 2          # [2 4 6 8 10]

# Filter
%< $nums [x] .> $x 2          # [3 4 5]

# Fold
%/ $nums 0 [acc x] .+ $acc $x # 15
```

### Parallel Processing

```hog
# Parallel map — each iteration runs in its own thread
^= squares &% [1 2 3 4 5] [n] .* $n $n
~> $squares    # [1 4 9 16 25]

# Join — wait for all tasks
^= results &= {
  .+ 10 20
  .* 3 4
}
~> $results    # [30 12]

# Race — return first completed
^= first &? { (slow-task) (fast-task) }
```

### Channels

```hog
# Create channel
^= ch @ci

# Send / Receive
-> $ch 42
^= val <- $ch
~> $val    # 42

# Select — pick first available
<>
| <- $ch1 -> [msg] ~> "ch1: $msg"
| <- $ch2 -> [msg] ~> "ch2: $msg"

# Broadcast — send to all
>< [$ch1 $ch2 $ch3] "shutdown"
```

### Shell Execution

```hog
# Execute command — returns {out, err, code}
^= result !! `ls -la`
~> :. $result ["out"]     # stdout
~> :. $result ["code"]    # exit code

# Quick output — stdout only
~> !< `echo hello`

# Check if command exists
?: (!? "git") "git found" "git not found"
```

### Comments

```hog
# Line comment

#-
  Block comment
  Multiple lines
-#

^= x 42  #- inline block comment -#
```

## Berry Reference

| Berry | Arity | Description                |
|-------|-------|----------------------------|
| `^=`  | 2     | Bind                       |
| `~>`  | 1     | Output                     |
| `?:`  | 3     | Conditional                |
| `%>`  | 3     | Map                        |
| `%<`  | 3     | Filter                     |
| `%/`  | 4     | Fold                       |
| `\|=` | 3     | Function                   |
| `!!`  | 1     | Execute → {out, err, code} |
| `!<`  | 1     | Execute → stdout only      |
| `:.`  | 2     | Field access               |
| `&%`  | 3     | Parallel each              |
| `&=`  | 1     | Join                       |
| `&?`  | 1     | Race                       |
| `->`  | 2     | Channel send               |
| `<-`  | 1     | Channel receive            |

## One-liner Power

Fixed arity means any program can be written on one line:

```hog
^= nums [1 2 3 4 5] ~> %/ (%> $nums [x] .* $x $x) 0 [a x] .+ $a $x
# Output: 55 (sum of squares)
```

## Status

v0.3 — active development

## License

[MIT License](LICENSE)
