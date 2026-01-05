//! Hedgehog Abstract Syntax Tree
//!
//! Defines the structure of parsed Hedgehog programs.

use std::fmt;

/// Source location for error reporting
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Loc {
    pub line: usize,
    pub column: usize,
}

impl Loc {
    pub fn new(line: usize, column: usize) -> Self {
        Self { line, column }
    }
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

/// AST Node with location
#[derive(Debug, Clone, PartialEq)]
pub struct Node<T> {
    pub node: T,
    pub loc: Loc,
}

impl<T> Node<T> {
    pub fn new(node: T, loc: Loc) -> Self {
        Self { node, loc }
    }
}

/// Type annotation
#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    // Primitives
    Int,      // @i - integer
    Float,    // @f - floating point
    Str,      // @s - string
    Byte,     // @b - byte
    Query,    // @q - boolean (query)
    Nothing,  // @n - unit/void

    // Generics
    List(Box<Type>),              // []T
    Option(Box<Type>),            // @?T
    Result(Box<Type>, Box<Type>), // @!T E
    Channel(Box<Type>),           // @cT

    // Compound
    Record(Vec<(String, Type)>),  // {field @t, ...}
    Tuple(Vec<Type>),
    Func(Vec<Type>, Box<Type>),

    // Named type (user-defined)
    Named(String),

    // Any type (for inference)
    Any,
}

/// Literal values
#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    None,
}

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum BinOp {
    // Arithmetic
    Add,    // .+
    Sub,    // .-
    Mul,    // .*
    Div,    // ./
    Mod,    // .%
    Pow,    // .^

    // Comparison
    Eq,     // .=
    Neq,    // .~
    Lt,     // .<
    Gt,     // .>
    Lte,    // .<=
    Gte,    // .>=

    // Logical
    And,    // .&
    Or,     // .|
}

/// Unary operators
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnaryOp {
    Not,    // .!
    Neg,    // -
}

/// Pattern for pattern matching
#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    /// Wildcard: _
    Wildcard,

    /// Literal: 0, "hello", true
    Literal(Literal),

    /// Variable binding: x
    Var(String),

    /// List: [h t...]
    List(Vec<Pattern>, Option<String>),  // elements, rest

    /// Record: {x y}
    Record(Vec<(String, Option<Pattern>)>),

    /// Range: 1..10
    Range(i64, i64),

    /// Option Some: (some x)
    Some(Box<Pattern>),

    /// Option None: none
    None,

    /// Result Ok: (ok x)
    Ok(Box<Pattern>),

    /// Result Err: (err e)
    Err(Box<Pattern>),
}

/// Expression
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// Literal value
    Lit(Literal),

    /// Variable reference: $x
    Var(String),

    /// Environment variable: $$PATH
    EnvVar(String),

    /// Binary operation: .+ 1 2
    BinOp(BinOp, Box<Node<Expr>>, Box<Node<Expr>>),

    /// Unary operation: .! cond
    UnaryOp(UnaryOp, Box<Node<Expr>>),

    /// Function call: (func arg1 arg2)
    Call(Box<Node<Expr>>, Vec<Node<Expr>>),

    /// Method/field access: $obj.field
    Field(Box<Node<Expr>>, String),

    /// List: [1 2 3]
    List(Vec<Node<Expr>>),

    /// Record: {name: "Alice", age: 30}
    Record(Vec<(String, Node<Expr>)>),

    /// Tuple: (, 1 2 3)
    Tuple(Vec<Node<Expr>>),

    /// Index access: $list[0]
    Index(Box<Node<Expr>>, Box<Node<Expr>>),

    /// Lambda: |> [x] .* $x 2
    Lambda(Vec<String>, Box<Node<Expr>>),

    /// Conditional: ?: cond then else
    Cond(Box<Node<Expr>>, Box<Node<Expr>>, Box<Node<Expr>>),

    /// Multi-way conditional: ?: | cond1 -> expr1 | cond2 -> expr2
    CondMulti(Vec<(Node<Expr>, Node<Expr>)>),

    /// Pattern match: ?? expr | pat1 -> expr1 | pat2 -> expr2
    Match(Box<Node<Expr>>, Vec<(Pattern, Node<Expr>)>),

    /// Loop: ?@ cond body
    Loop(Box<Node<Expr>>, Box<Node<Expr>>),

    /// Try/trap: ?! body [err] handler
    Trap(Box<Node<Expr>>, String, Box<Node<Expr>>),

    /// Map: %> list [x] expr
    Map(Box<Node<Expr>>, String, Box<Node<Expr>>),

    /// Filter: %< list [x] expr
    Filter(Box<Node<Expr>>, String, Box<Node<Expr>>),

    /// Fold: %/ list init [acc x] expr
    Fold(Box<Node<Expr>>, Box<Node<Expr>>, String, String, Box<Node<Expr>>),

    /// Times: %~ n [i] expr
    Times(Box<Node<Expr>>, String, Box<Node<Expr>>),

    /// Range: %.. start end (exactly 2 children, no step)
    Range(Box<Node<Expr>>, Box<Node<Expr>>),

    /// Parallel each: &% list [x] expr
    ParEach(Box<Node<Expr>>, String, Box<Node<Expr>>),

    /// Parallel join: &= tasks
    ParJoin(Vec<Node<Expr>>),

    /// Parallel race: &? { expr1; expr2 }
    ParRace(Vec<Node<Expr>>),

    /// Background: &! expr
    Background(Box<Node<Expr>>),

    /// Command execution: !! `cmd` (exactly 1 child: command string)
    Exec(String),

    /// Command interpolation: `cmd`
    CmdInterp(String),

    /// Pipe: expr1 :> expr2
    Pipe(Box<Node<Expr>>, Box<Node<Expr>>),

    /// File write: ~+ path content (2 children)
    FileWrite(Box<Node<Expr>>, Box<Node<Expr>>),

    /// File append: ~^ path content (2 children)
    FileAppend(Box<Node<Expr>>, Box<Node<Expr>>),

    /// Binary write: ~* path bytes (2 children)
    BinaryWrite(Box<Node<Expr>>, Box<Node<Expr>>),

    /// Stdin read: <~ (0 children)
    Stdin,

    /// File read: <+ path (1 child)
    FileRead(Box<Node<Expr>>),

    /// Binary read: <* path (1 child)
    BinaryRead(Box<Node<Expr>>),

    /// Some: (some expr)
    Some(Box<Node<Expr>>),

    /// Ok: (ok expr)
    Ok(Box<Node<Expr>>),

    /// Err: (err expr)
    Err(Box<Node<Expr>>),

    /// Block: { expr1; expr2; expr3 }
    Block(Vec<Node<Stmt>>),

    /// Type check: :? expr type (2 children)
    TypeCheck(Box<Node<Expr>>, Type),

    /// Type cast: :> expr type (2 children)
    TypeCast(Box<Node<Expr>>, Type),

    /// Type of: :@ expr (1 child)
    TypeOf(Box<Node<Expr>>),

    /// Channel send: -> channel value (2 children)
    ChanSend(Box<Node<Expr>>, Box<Node<Expr>>),

    /// Channel receive: <- channel (1 child)
    ChanRecv(Box<Node<Expr>>),
}

/// Statement
#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    /// Expression statement
    Expr(Node<Expr>),

    /// Binding: ^= name expr
    Bind(String, Node<Expr>),

    /// Typed binding: ^: name type expr
    TypedBind(String, Type, Node<Expr>),

    /// Function definition: |= Name [args] body
    FuncDef(String, Vec<String>, Node<Expr>),

    /// Typed function: |: Name [types -> ret] [args] body
    TypedFuncDef(String, Vec<Type>, Type, Vec<String>, Node<Expr>),

    /// Output: ~> expr
    Output(Node<Expr>),

    /// Output raw: ~| expr
    OutputRaw(Node<Expr>),

    /// Output error: ~! expr
    OutputErr(Node<Expr>),

    /// File write: ~+ path content
    FileWrite(Node<Expr>, Node<Expr>),

    /// File append: ~^ path content
    FileAppend(Node<Expr>, Node<Expr>),

    /// Binary write: ~* path bytes
    BinaryWrite(Node<Expr>, Node<Expr>),

    /// Struct definition: ^- Name { fields }
    StructDef(String, Vec<(String, Type)>),

    /// Type alias: ^- Name type
    TypeAlias(String, Type),

    /// Library import: /+ module-path
    LibImport(String),

    /// Aliased import: /= alias module-path
    AliasImport(String, String),

    /// Relative import: /. relative-path
    RelImport(String),
}

/// A complete Hedgehog program
#[derive(Debug, Clone)]
pub struct Program {
    pub stmts: Vec<Node<Stmt>>,
}

impl Program {
    pub fn new(stmts: Vec<Node<Stmt>>) -> Self {
        Self { stmts }
    }
}

/// Pretty print AST (for debugging)
impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Lit(lit) => write!(f, "{:?}", lit),
            Expr::Var(name) => write!(f, "${}", name),
            Expr::EnvVar(name) => write!(f, "$${}", name),
            Expr::BinOp(op, lhs, rhs) => write!(f, "({:?} {} {})", op, lhs.node, rhs.node),
            Expr::UnaryOp(op, expr) => write!(f, "({:?} {})", op, expr.node),
            Expr::Call(func, args) => {
                write!(f, "({}", func.node)?;
                for arg in args {
                    write!(f, " {}", arg.node)?;
                }
                write!(f, ")")
            }
            Expr::List(items) => {
                write!(f, "[")?;
                for (i, item) in items.iter().enumerate() {
                    if i > 0 { write!(f, " ")?; }
                    write!(f, "{}", item.node)?;
                }
                write!(f, "]")
            }
            Expr::Cond(cond, then, else_) => {
                write!(f, "(?: {} {} {})", cond.node, then.node, else_.node)
            }
            _ => write!(f, "{:?}", self),
        }
    }
}