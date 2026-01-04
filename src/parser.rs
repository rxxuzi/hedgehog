//! Hedgehog Parser
//!
//! Parses tokens into an Abstract Syntax Tree.

use crate::ast::*;
use crate::lexer::{Lexer, SpannedToken, Token};

/// Parser error
#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub line: usize,
    pub column: usize,
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Parse error at {}:{}: {}", self.line, self.column, self.message)
    }
}

impl ParseError {
    fn new(message: impl Into<String>, line: usize, column: usize) -> Self {
        Self {
            message: message.into(),
            line,
            column,
        }
    }
}

/// Parser for Hedgehog
pub struct Parser {
    tokens: Vec<SpannedToken>,
    pos: usize,
}

impl Parser {
    pub fn new(tokens: Vec<SpannedToken>) -> Self {
        Self { tokens, pos: 0 }
    }

    /// Parse from source code
    pub fn parse_source(source: &str) -> Result<Program, ParseError> {
        let mut lexer = Lexer::new(source);
        let tokens = lexer.tokenize().map_err(|e| {
            ParseError::new(e.message, e.span.line, e.span.column)
        })?;

        let mut parser = Parser::new(tokens);
        parser.parse_program()
    }

    /// Get current token (cloned to avoid borrow issues)
    fn current(&self) -> Token {
        self.tokens.get(self.pos)
            .map(|t| t.token.clone())
            .unwrap_or(Token::Eof)
    }

    /// Get current span
    fn current_span(&self) -> Loc {
        self.tokens.get(self.pos)
            .map(|t| Loc::new(t.span.line, t.span.column))
            .unwrap_or(Loc::new(0, 0))
    }

    /// Advance to next token
    fn advance(&mut self) {
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
    }

    /// Check if current token matches
    fn check(&self, token: &Token) -> bool {
        std::mem::discriminant(&self.current()) == std::mem::discriminant(token)
    }

    /// Consume token if it matches, error otherwise
    fn expect(&mut self, expected: &Token) -> Result<(), ParseError> {
        if self.check(expected) {
            self.advance();
            Ok(())
        } else {
            let loc = self.current_span();
            Err(ParseError::new(
                format!("Expected {:?}, got {:?}", expected, self.current()),
                loc.line, loc.column
            ))
        }
    }

    /// Skip newlines (Hoon-style: treat as whitespace)
    fn skip_newlines(&mut self) {
        while self.check(&Token::Newline) {
            self.advance();
        }
    }

    /// Parse a complete program
    /// Hoon-style: newlines are whitespace, only `;` separates statements
    pub fn parse_program(&mut self) -> Result<Program, ParseError> {
        let mut stmts = Vec::new();

        self.skip_whitespace_and_newlines();

        while !self.check(&Token::Eof) {
            stmts.push(self.parse_stmt()?);
            self.skip_whitespace_and_newlines();

            // Optional semicolon between statements
            if self.check(&Token::Semicolon) {
                self.advance();
                self.skip_whitespace_and_newlines();
            }
        }

        Ok(Program::new(stmts))
    }

    /// Skip whitespace and newlines (Hoon-style: newlines are just whitespace)
    fn skip_whitespace_and_newlines(&mut self) {
        while self.check(&Token::Newline) {
            self.advance();
        }
    }

    /// Parse a statement
    fn parse_stmt(&mut self) -> Result<Node<Stmt>, ParseError> {
        let loc = self.current_span();

        match self.current() {
            // Binding: ^= name expr
            Token::Bind => {
                self.advance();
                let name = self.parse_ident()?;
                let expr = self.parse_expr()?;
                Ok(Node::new(Stmt::Bind(name, expr), loc))
            }

            // Typed binding: ^: name type expr
            Token::TypedBind => {
                self.advance();
                let name = self.parse_ident()?;
                let ty = self.parse_type()?;
                let expr = self.parse_expr()?;
                Ok(Node::new(Stmt::TypedBind(name, ty, expr), loc))
            }

            // Function definition: |= Name [args] body
            Token::Func => {
                self.advance();
                let name = self.parse_ident()?;
                let params = self.parse_param_list()?;
                let body = self.parse_expr()?;
                Ok(Node::new(Stmt::FuncDef(name, params, body), loc))
            }

            // Output: ~> expr
            Token::Out => {
                self.advance();
                let expr = self.parse_expr()?;
                Ok(Node::new(Stmt::Output(expr), loc))
            }

            // Output raw: ~| expr
            Token::OutRaw => {
                self.advance();
                let expr = self.parse_expr()?;
                Ok(Node::new(Stmt::OutputRaw(expr), loc))
            }

            // Output error: ~! expr
            Token::OutErr => {
                self.advance();
                let expr = self.parse_expr()?;
                Ok(Node::new(Stmt::OutputErr(expr), loc))
            }

            // File write: ~+ path content
            Token::FileWrite => {
                self.advance();
                let path = self.parse_expr()?;
                let content = self.parse_expr()?;
                Ok(Node::new(Stmt::FileWrite(path, content), loc))
            }

            // File append: ~^ path content
            Token::FileAppend => {
                self.advance();
                let path = self.parse_expr()?;
                let content = self.parse_expr()?;
                Ok(Node::new(Stmt::FileAppend(path, content), loc))
            }

            // Binary write: ~* path bytes
            Token::BinaryWrite => {
                self.advance();
                let path = self.parse_expr()?;
                let bytes = self.parse_expr()?;
                Ok(Node::new(Stmt::BinaryWrite(path, bytes), loc))
            }

            // Struct definition: ^- Name { fields } or type alias: ^- Name type
            Token::StructDef => {
                self.advance();
                let name = self.parse_ident()?;
                self.skip_newlines();

                if self.check(&Token::LBrace) {
                    // Struct definition
                    self.advance();
                    let mut fields = Vec::new();

                    self.skip_newlines();
                    while !self.check(&Token::RBrace) && !self.check(&Token::Eof) {
                        self.skip_newlines();
                        if self.check(&Token::RBrace) { break; }

                        let field_name = self.parse_ident()?;
                        self.expect(&Token::Colon)?;
                        let field_type = self.parse_type()?;
                        fields.push((field_name, field_type));

                        // Optional comma
                        if self.check(&Token::Comma) {
                            self.advance();
                        }
                        self.skip_newlines();
                    }

                    self.expect(&Token::RBrace)?;
                    Ok(Node::new(Stmt::StructDef(name, fields), loc))
                } else {
                    // Type alias
                    let ty = self.parse_type()?;
                    Ok(Node::new(Stmt::TypeAlias(name, ty), loc))
                }
            }

            // Library import: /+ module-path
            Token::LibImport => {
                self.advance();
                let path = self.parse_module_path()?;
                Ok(Node::new(Stmt::LibImport(path), loc))
            }

            // Aliased import: /= alias module-path
            Token::AliasImport => {
                self.advance();
                let alias = self.parse_ident()?;
                let path = self.parse_module_path()?;
                Ok(Node::new(Stmt::AliasImport(alias, path), loc))
            }

            // Relative import: /. relative-path
            Token::RelImport => {
                self.advance();
                let path = self.parse_path()?;
                Ok(Node::new(Stmt::RelImport(path), loc))
            }

            // Expression statement
            _ => {
                let expr = self.parse_expr()?;
                Ok(Node::new(Stmt::Expr(expr), loc))
            }
        }
    }

    /// Parse an expression
    fn parse_expr(&mut self) -> Result<Node<Expr>, ParseError> {
        self.skip_newlines();  // Allow multi-line expressions
        self.parse_primary_expr()
    }

    /// Parse primary expression
    fn parse_primary_expr(&mut self) -> Result<Node<Expr>, ParseError> {
        let loc = self.current_span();

        match self.current() {
            // Literals
            Token::Int(n) => {
                self.advance();
                Ok(Node::new(Expr::Lit(Literal::Int(n)), loc))
            }
            Token::Float(n) => {
                self.advance();
                Ok(Node::new(Expr::Lit(Literal::Float(n)), loc))
            }
            Token::String(s) => {
                self.advance();
                Ok(Node::new(Expr::Lit(Literal::String(s)), loc))
            }
            Token::RawString(s) => {
                self.advance();
                Ok(Node::new(Expr::Lit(Literal::String(s)), loc))
            }
            Token::CommandString(s) => {
                self.advance();
                Ok(Node::new(Expr::CmdInterp(s), loc))
            }
            Token::True => {
                self.advance();
                Ok(Node::new(Expr::Lit(Literal::Bool(true)), loc))
            }
            Token::False => {
                self.advance();
                Ok(Node::new(Expr::Lit(Literal::Bool(false)), loc))
            }
            Token::None => {
                self.advance();
                Ok(Node::new(Expr::Lit(Literal::None), loc))
            }

            // Variable reference: $name
            Token::Dollar => {
                self.advance();
                let name = self.parse_ident()?;
                Ok(Node::new(Expr::Var(name), loc))
            }

            // Environment variable: $$NAME
            Token::EnvRef => {
                self.advance();
                let name = self.parse_ident()?;
                Ok(Node::new(Expr::EnvVar(name), loc))
            }

            // Arithmetic operations
            Token::Add => self.parse_binop(BinOp::Add),
            Token::Sub => self.parse_binop(BinOp::Sub),
            Token::Mul => self.parse_binop(BinOp::Mul),
            Token::Div => self.parse_binop(BinOp::Div),
            Token::Mod => self.parse_binop(BinOp::Mod),
            Token::Pow => self.parse_binop(BinOp::Pow),

            // Comparison
            Token::Eq => self.parse_binop(BinOp::Eq),
            Token::Neq => self.parse_binop(BinOp::Neq),
            Token::Lt => self.parse_binop(BinOp::Lt),
            Token::Gt => self.parse_binop(BinOp::Gt),
            Token::Lte => self.parse_binop(BinOp::Lte),
            Token::Gte => self.parse_binop(BinOp::Gte),

            // Logical
            Token::And => self.parse_binop(BinOp::And),
            Token::Or => self.parse_binop(BinOp::Or),
            Token::Not => {
                self.advance();
                let operand = self.parse_expr()?;
                Ok(Node::new(Expr::UnaryOp(UnaryOp::Not, Box::new(operand)), loc))
            }

            // Conditional: ?: cond then else
            Token::Cond => self.parse_cond(),

            // Match: ?? expr | pat -> expr
            Token::Match => self.parse_match(),

            // Loop: ?@ cond body
            Token::Loop => {
                self.advance();
                let cond = self.parse_expr()?;
                let body = self.parse_expr()?;
                Ok(Node::new(Expr::Loop(Box::new(cond), Box::new(body)), loc))
            }

            // Trap: ?! body [err] handler
            Token::Trap => self.parse_trap(),

            // Map: %> list [x] body
            Token::Map => self.parse_map(),

            // Filter: %< list [x] body
            Token::Filter => self.parse_filter(),

            // Fold: %/ list init [acc x] body
            Token::Fold => self.parse_fold(),

            // Times: %~ n [i] body
            Token::Times => self.parse_times(),

            // Range: %.. start end [step]
            Token::Range => self.parse_range(),

            // Parallel each: &% list [x] body
            Token::ParEach => self.parse_par_each(),

            // Join: <&> { ... }
            Token::Join => self.parse_join(),

            // Race: &? { ... }
            Token::Race => self.parse_race(),

            // Background: &! expr
            Token::Background => {
                self.advance();
                let expr = self.parse_expr()?;
                Ok(Node::new(Expr::Background(Box::new(expr)), loc))
            }

            // Command execution: !! cmd args
            Token::Exec => self.parse_exec(),

            // Stdin: <~ (0 children)
            Token::Stdin => {
                self.advance();
                Ok(Node::new(Expr::Stdin, loc))
            }

            // File read: <+ path (1 child)
            Token::FileRead => {
                self.advance();
                let path = self.parse_expr()?;
                Ok(Node::new(Expr::FileRead(Box::new(path)), loc))
            }

            // Binary read: <* path (1 child)
            Token::BinaryRead => {
                self.advance();
                let path = self.parse_expr()?;
                Ok(Node::new(Expr::BinaryRead(Box::new(path)), loc))
            }

            // Type check: :? expr type (2 children)
            Token::TypeCheck => {
                self.advance();
                let expr = self.parse_expr()?;
                let ty = self.parse_type()?;
                Ok(Node::new(Expr::TypeCheck(Box::new(expr), ty), loc))
            }

            // Type cast: :> expr type (2 children)
            Token::TypeCast => {
                self.advance();
                let expr = self.parse_expr()?;
                let ty = self.parse_type()?;
                Ok(Node::new(Expr::TypeCast(Box::new(expr), ty), loc))
            }

            // Type of: :@ expr (1 child)
            Token::TypeOf => {
                self.advance();
                let expr = self.parse_expr()?;
                Ok(Node::new(Expr::TypeOf(Box::new(expr)), loc))
            }

            // Channel receive: <- channel (1 child)
            Token::ChanRecv => {
                self.advance();
                let channel = self.parse_expr()?;
                Ok(Node::new(Expr::ChanRecv(Box::new(channel)), loc))
            }

            // Channel send: -> channel value (2 children)
            // Note: Arrow is used in pattern matching, so we need context
            // For now, treat -> as ChanSend when used as expression
            Token::Arrow => {
                self.advance();
                let channel = self.parse_expr()?;
                let value = self.parse_expr()?;
                Ok(Node::new(Expr::ChanSend(Box::new(channel), Box::new(value)), loc))
            }

            // Lambda: |> [args] body
            Token::Lambda => {
                self.advance();
                let params = self.parse_param_list()?;
                let body = self.parse_expr()?;
                Ok(Node::new(Expr::Lambda(params, Box::new(body)), loc))
            }

            // List: [...]
            Token::LBracket => self.parse_list(),

            // Record: {...}
            Token::LBrace => self.parse_record_or_block(),

            // Grouping: (...)
            Token::LParen => self.parse_paren_expr(),

            // Identifier
            Token::Ident(name) => {
                self.advance();
                Ok(Node::new(Expr::Var(name), loc))
            }

            // Output expressions (wrap in Block with single statement)
            Token::Out => {
                self.advance();
                let expr = self.parse_expr()?;
                let stmt = Node::new(Stmt::Output(expr), loc);
                Ok(Node::new(Expr::Block(vec![stmt]), loc))
            }
            Token::OutRaw => {
                self.advance();
                let expr = self.parse_expr()?;
                let stmt = Node::new(Stmt::OutputRaw(expr), loc);
                Ok(Node::new(Expr::Block(vec![stmt]), loc))
            }
            Token::OutErr => {
                self.advance();
                let expr = self.parse_expr()?;
                let stmt = Node::new(Stmt::OutputErr(expr), loc);
                Ok(Node::new(Expr::Block(vec![stmt]), loc))
            }

            _ => {
                Err(ParseError::new(
                    format!("Unexpected token: {:?}", self.current()),
                    loc.line, loc.column
                ))
            }
        }
    }

    /// Parse binary operation
    fn parse_binop(&mut self, op: BinOp) -> Result<Node<Expr>, ParseError> {
        let loc = self.current_span();
        self.advance();
        let lhs = self.parse_expr()?;
        let rhs = self.parse_expr()?;
        Ok(Node::new(Expr::BinOp(op, Box::new(lhs), Box::new(rhs)), loc))
    }

    /// Parse conditional: ?: cond then else
    fn parse_cond(&mut self) -> Result<Node<Expr>, ParseError> {
        let loc = self.current_span();
        self.advance(); // consume ?:

        self.skip_newlines();  // Allow newline after ?:

        // Check for multi-way cond: ?: | cond1 -> expr1 | cond2 -> expr2
        if self.check(&Token::Bar) {
            let mut branches = Vec::new();

            while self.check(&Token::Bar) {
                self.advance(); // consume |
                self.skip_newlines();  // Allow newline after |

                // Check for wildcard
                if self.check(&Token::Underscore) {
                    self.advance();
                    self.expect(&Token::Arrow)?;
                    let body = self.parse_expr()?;
                    branches.push((
                        Node::new(Expr::Lit(Literal::Bool(true)), loc),
                        body
                    ));
                    break;
                }

                let cond = self.parse_expr()?;
                self.expect(&Token::Arrow)?;
                let body = self.parse_expr()?;
                branches.push((cond, body));

                self.skip_newlines();
            }

            Ok(Node::new(Expr::CondMulti(branches), loc))
        } else {
            // Standard ternary: ?: cond then else
            let cond = self.parse_expr()?;
            self.skip_newlines();
            let then_expr = self.parse_expr()?;
            self.skip_newlines();
            let else_expr = self.parse_expr()?;

            Ok(Node::new(Expr::Cond(
                Box::new(cond),
                Box::new(then_expr),
                Box::new(else_expr)
            ), loc))
        }
    }

    /// Parse match: ?? expr | pat -> expr
    fn parse_match(&mut self) -> Result<Node<Expr>, ParseError> {
        let loc = self.current_span();
        self.advance(); // consume ??

        let scrutinee = self.parse_expr()?;
        self.skip_newlines();

        let mut branches = Vec::new();

        while self.check(&Token::Bar) {
            self.advance(); // consume |
            self.skip_newlines();  // Allow newline after |
            let pattern = self.parse_pattern()?;
            self.skip_newlines();
            self.expect(&Token::Arrow)?;
            self.skip_newlines();
            let body = self.parse_expr()?;
            branches.push((pattern, body));
            self.skip_newlines();
        }

        Ok(Node::new(Expr::Match(Box::new(scrutinee), branches), loc))
    }

    /// Parse pattern for pattern matching
    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        let loc = self.current_span();

        match self.current() {
            Token::Underscore => {
                self.advance();
                Ok(Pattern::Wildcard)
            }
            Token::Int(n) => {
                self.advance();
                // Check for range pattern: 1..10
                if self.check(&Token::Range) {
                    self.advance();
                    if let Token::Int(end) = self.current() {
                        self.advance();
                        return Ok(Pattern::Range(n, end));
                    }
                }
                Ok(Pattern::Literal(Literal::Int(n)))
            }
            Token::String(s) => {
                self.advance();
                Ok(Pattern::Literal(Literal::String(s)))
            }
            Token::True => {
                self.advance();
                Ok(Pattern::Literal(Literal::Bool(true)))
            }
            Token::False => {
                self.advance();
                Ok(Pattern::Literal(Literal::Bool(false)))
            }
            Token::None => {
                self.advance();
                Ok(Pattern::None)
            }
            Token::LBracket => {
                self.advance();
                let mut patterns = Vec::new();
                let mut rest = None;

                while !self.check(&Token::RBracket) {
                    if let Token::Ident(name) = self.current() {
                        self.advance();
                        // Check for rest pattern: name...
                        if self.check(&Token::Range) {
                            self.advance();
                            rest = Some(name);
                            break;
                        }
                        patterns.push(Pattern::Var(name));
                    } else {
                        patterns.push(self.parse_pattern()?);
                    }
                }

                self.expect(&Token::RBracket)?;
                Ok(Pattern::List(patterns, rest))
            }
            Token::LParen => {
                self.advance();
                // Check for (some x), (ok x), (err x)
                match self.current() {
                    Token::Ident(ref name) if name == "some" => {
                        self.advance();
                        let inner = self.parse_pattern()?;
                        self.expect(&Token::RParen)?;
                        Ok(Pattern::Some(Box::new(inner)))
                    }
                    Token::Ident(ref name) if name == "ok" => {
                        self.advance();
                        let inner = self.parse_pattern()?;
                        self.expect(&Token::RParen)?;
                        Ok(Pattern::Ok(Box::new(inner)))
                    }
                    Token::Ident(ref name) if name == "err" => {
                        self.advance();
                        let inner = self.parse_pattern()?;
                        self.expect(&Token::RParen)?;
                        Ok(Pattern::Err(Box::new(inner)))
                    }
                    _ => {
                        Err(ParseError::new("Invalid pattern", loc.line, loc.column))
                    }
                }
            }
            Token::Ident(name) => {
                self.advance();
                Ok(Pattern::Var(name))
            }
            _ => {
                Err(ParseError::new(
                    format!("Invalid pattern: {:?}", self.current()),
                    loc.line, loc.column
                ))
            }
        }
    }

    /// Parse trap: ?! body [err] handler
    fn parse_trap(&mut self) -> Result<Node<Expr>, ParseError> {
        let loc = self.current_span();
        self.advance(); // consume ?!

        let body = self.parse_expr()?;

        self.skip_newlines();
        self.expect(&Token::LBracket)?;
        self.skip_newlines();
        let err_name = self.parse_ident()?;
        self.skip_newlines();
        self.expect(&Token::RBracket)?;

        let handler = self.parse_expr()?;

        Ok(Node::new(Expr::Trap(Box::new(body), err_name, Box::new(handler)), loc))
    }

    /// Parse map: %> list [x] body
    fn parse_map(&mut self) -> Result<Node<Expr>, ParseError> {
        let loc = self.current_span();
        self.advance(); // consume %>

        let list = self.parse_expr()?;

        self.skip_newlines();
        self.expect(&Token::LBracket)?;
        self.skip_newlines();
        let var = self.parse_ident()?;
        self.skip_newlines();
        self.expect(&Token::RBracket)?;

        let body = self.parse_expr()?;

        Ok(Node::new(Expr::Map(Box::new(list), var, Box::new(body)), loc))
    }

    /// Parse filter: %< list [x] body
    fn parse_filter(&mut self) -> Result<Node<Expr>, ParseError> {
        let loc = self.current_span();
        self.advance(); // consume %<

        let list = self.parse_expr()?;

        self.skip_newlines();
        self.expect(&Token::LBracket)?;
        self.skip_newlines();
        let var = self.parse_ident()?;
        self.skip_newlines();
        self.expect(&Token::RBracket)?;

        let body = self.parse_expr()?;

        Ok(Node::new(Expr::Filter(Box::new(list), var, Box::new(body)), loc))
    }

    /// Parse fold: %/ list init [acc x] body
    fn parse_fold(&mut self) -> Result<Node<Expr>, ParseError> {
        let loc = self.current_span();
        self.advance(); // consume %/

        let list = self.parse_expr()?;
        let init = self.parse_expr()?;

        self.skip_newlines();
        self.expect(&Token::LBracket)?;
        self.skip_newlines();
        let acc = self.parse_ident()?;
        self.skip_newlines();
        let var = self.parse_ident()?;
        self.skip_newlines();
        self.expect(&Token::RBracket)?;

        let body = self.parse_expr()?;

        Ok(Node::new(Expr::Fold(
            Box::new(list),
            Box::new(init),
            acc,
            var,
            Box::new(body)
        ), loc))
    }

    /// Parse times: %~ n [i] body
    fn parse_times(&mut self) -> Result<Node<Expr>, ParseError> {
        let loc = self.current_span();
        self.advance(); // consume %~

        let n = self.parse_expr()?;

        self.skip_newlines();
        self.expect(&Token::LBracket)?;
        self.skip_newlines();
        let var = self.parse_ident()?;
        self.skip_newlines();
        self.expect(&Token::RBracket)?;

        let body = self.parse_expr()?;

        Ok(Node::new(Expr::Times(Box::new(n), var, Box::new(body)), loc))
    }

    /// Parse range: %.. start end [step]
    /// Parse range: %.. start end (exactly 2 children, no step)
    fn parse_range(&mut self) -> Result<Node<Expr>, ParseError> {
        let loc = self.current_span();
        self.advance(); // consume %..

        let start = self.parse_expr()?;
        let end = self.parse_expr()?;

        // No step - use (range start end step) function for step support
        Ok(Node::new(Expr::Range(Box::new(start), Box::new(end)), loc))
    }

    /// Parse parallel each: &% list [x] body
    fn parse_par_each(&mut self) -> Result<Node<Expr>, ParseError> {
        let loc = self.current_span();
        self.advance(); // consume &%

        let list = self.parse_expr()?;

        self.skip_newlines();
        self.expect(&Token::LBracket)?;
        self.skip_newlines();
        let var = self.parse_ident()?;
        self.skip_newlines();
        self.expect(&Token::RBracket)?;

        let body = self.parse_expr()?;

        Ok(Node::new(Expr::ParEach(Box::new(list), var, Box::new(body)), loc))
    }

    /// Parse join: <&> { expr1; expr2 }
    fn parse_join(&mut self) -> Result<Node<Expr>, ParseError> {
        let loc = self.current_span();
        self.advance(); // consume <&>

        self.skip_newlines();
        self.expect(&Token::LBrace)?;
        let exprs = self.parse_expr_list_until(&Token::RBrace)?;
        self.expect(&Token::RBrace)?;

        Ok(Node::new(Expr::ParJoin(exprs), loc))
    }

    /// Parse race: &? { expr1; expr2 }
    fn parse_race(&mut self) -> Result<Node<Expr>, ParseError> {
        let loc = self.current_span();
        self.advance(); // consume &?

        self.expect(&Token::LBrace)?;
        let exprs = self.parse_expr_list_until(&Token::RBrace)?;
        self.expect(&Token::RBrace)?;

        Ok(Node::new(Expr::ParRace(exprs), loc))
    }

    /// Parse exec: !! cmd args...
    /// Parse exec: !! command-string (exactly 1 child)
    fn parse_exec(&mut self) -> Result<Node<Expr>, ParseError> {
        let loc = self.current_span();
        self.advance(); // consume !!

        // !! takes exactly 1 child: a command string in backticks
        let cmd = match self.current() {
            Token::CommandString(s) => {
                self.advance();
                s
            }
            Token::String(s) => {
                self.advance();
                s
            }
            _ => {
                return Err(ParseError::new(
                    "!! requires a command string in backticks, e.g., !! `echo hello`",
                    loc.line, loc.column
                ));
            }
        };

        Ok(Node::new(Expr::Exec(cmd), loc))
    }

    /// Parse list: [...]
    fn parse_list(&mut self) -> Result<Node<Expr>, ParseError> {
        let loc = self.current_span();
        self.advance(); // consume [

        let mut items = Vec::new();

        while !self.check(&Token::RBracket) && !self.check(&Token::Eof) {
            self.skip_newlines();
            if self.check(&Token::RBracket) { break; }

            items.push(self.parse_expr()?);

            // Optional comma
            if self.check(&Token::Comma) {
                self.advance();
            }
        }

        self.expect(&Token::RBracket)?;

        Ok(Node::new(Expr::List(items), loc))
    }

    /// Parse record or block
    fn parse_record_or_block(&mut self) -> Result<Node<Expr>, ParseError> {
        let loc = self.current_span();
        self.advance(); // consume {

        self.skip_newlines();

        // Check if it's a record (key: value) or a block (statements)
        if self.check(&Token::RBrace) {
            self.advance();
            return Ok(Node::new(Expr::Record(vec![]), loc));
        }

        // Try to determine if it's a record or block
        // Record starts with: identifier :
        let is_record = if let Token::Ident(_) = self.current() {
            // Look ahead for colon
            self.tokens.get(self.pos + 1)
                .map(|t| t.token == Token::Colon)
                .unwrap_or(false)
        } else {
            false
        };

        if is_record {
            let mut fields = Vec::new();

            while !self.check(&Token::RBrace) && !self.check(&Token::Eof) {
                self.skip_newlines();
                if self.check(&Token::RBrace) { break; }

                let key = self.parse_ident()?;
                self.expect(&Token::Colon)?;
                let value = self.parse_expr()?;

                fields.push((key, value));

                // Optional comma
                if self.check(&Token::Comma) {
                    self.advance();
                }
                self.skip_newlines();
            }

            self.expect(&Token::RBrace)?;
            Ok(Node::new(Expr::Record(fields), loc))
        } else {
            // Block
            let mut stmts = Vec::new();

            while !self.check(&Token::RBrace) && !self.check(&Token::Eof) {
                self.skip_newlines();
                if self.check(&Token::RBrace) { break; }

                stmts.push(self.parse_stmt()?);

                // Skip separator
                while self.check(&Token::Semicolon) || self.check(&Token::Newline) {
                    self.advance();
                }
            }

            self.expect(&Token::RBrace)?;
            Ok(Node::new(Expr::Block(stmts), loc))
        }
    }

    /// Parse parenthesized expression or function call
    fn parse_paren_expr(&mut self) -> Result<Node<Expr>, ParseError> {
        let loc = self.current_span();
        self.advance(); // consume (

        // Check for tuple: (, ...)
        if self.check(&Token::Comma) {
            self.advance();
            let items = self.parse_expr_list_until(&Token::RParen)?;
            self.expect(&Token::RParen)?;
            return Ok(Node::new(Expr::Tuple(items), loc));
        }

        // Check for some/ok/err
        match self.current() {
            Token::Ident(ref name) if name == "some" => {
                self.advance();
                let inner = self.parse_expr()?;
                self.expect(&Token::RParen)?;
                return Ok(Node::new(Expr::Some(Box::new(inner)), loc));
            }
            Token::Ident(ref name) if name == "ok" => {
                self.advance();
                let inner = self.parse_expr()?;
                self.expect(&Token::RParen)?;
                return Ok(Node::new(Expr::Ok(Box::new(inner)), loc));
            }
            Token::Ident(ref name) if name == "err" => {
                self.advance();
                let inner = self.parse_expr()?;
                self.expect(&Token::RParen)?;
                return Ok(Node::new(Expr::Err(Box::new(inner)), loc));
            }
            _ => {}
        }

        // Function call or grouping
        let first = self.parse_expr()?;

        if self.check(&Token::RParen) {
            // Simple grouping
            self.advance();
            Ok(first)
        } else {
            // Function call: (func arg1 arg2 ...)
            let mut args = Vec::new();
            while !self.check(&Token::RParen) && !self.check(&Token::Eof) {
                args.push(self.parse_expr()?);
            }
            self.expect(&Token::RParen)?;

            Ok(Node::new(Expr::Call(Box::new(first), args), loc))
        }
    }

    /// Parse identifier
    fn parse_ident(&mut self) -> Result<String, ParseError> {
        let loc = self.current_span();
        match self.current() {
            Token::Ident(name) => {
                self.advance();
                Ok(name)
            }
            _ => Err(ParseError::new(
                format!("Expected identifier, got {:?}", self.current()),
                loc.line, loc.column
            ))
        }
    }

    /// Parse parameter list: [a b c]
    fn parse_param_list(&mut self) -> Result<Vec<String>, ParseError> {
        self.skip_newlines();
        self.expect(&Token::LBracket)?;

        let mut params = Vec::new();
        while !self.check(&Token::RBracket) && !self.check(&Token::Eof) {
            self.skip_newlines();
            if self.check(&Token::RBracket) { break; }

            // Check for rest parameter: *name
            if self.check(&Token::Star) {
                self.advance();
            }
            params.push(self.parse_ident()?);
        }

        self.skip_newlines();
        self.expect(&Token::RBracket)?;
        Ok(params)
    }

    /// Parse type annotation
    fn parse_type(&mut self) -> Result<Type, ParseError> {
        let loc = self.current_span();

        match self.current() {
            Token::TypeLit(name) => {
                self.advance();
                self.parse_type_from_name(&name, loc)
            }
            Token::Ident(name) => {
                self.advance();
                // Check for primitive types first
                match name.as_str() {
                    "i8" => Ok(Type::I8),
                    "i16" => Ok(Type::I16),
                    "i32" => Ok(Type::I32),
                    "i64" => Ok(Type::I64),
                    "u8" => Ok(Type::U8),
                    "u16" => Ok(Type::U16),
                    "u32" => Ok(Type::U32),
                    "u64" => Ok(Type::U64),
                    "f32" => Ok(Type::F32),
                    "f64" => Ok(Type::F64),
                    "bool" => Ok(Type::Bool),
                    "str" => Ok(Type::Str),
                    "char" => Ok(Type::Char),
                    _ => Ok(Type::Named(name))
                }
            }
            _ => Err(ParseError::new("Expected type", loc.line, loc.column))
        }
    }

    /// Parse type from @-prefixed type literal
    fn parse_type_from_name(&mut self, name: &str, loc: Loc) -> Result<Type, ParseError> {
        // Handle generic types like @?[T], @![T E], @chan[T], @[T]
        if name.starts_with("?[") && name.ends_with(']') {
            // Option type: @?[T]
            let inner = &name[2..name.len()-1];
            let inner_type = self.type_from_simple_name(inner)?;
            return Ok(Type::Option(Box::new(inner_type)));
        }
        if name.starts_with("![") && name.ends_with(']') {
            // Result type: @![T E] - simplified parsing
            let inner = &name[2..name.len()-1];
            let parts: Vec<&str> = inner.split_whitespace().collect();
            if parts.len() >= 2 {
                let ok_type = self.type_from_simple_name(parts[0])?;
                let err_type = self.type_from_simple_name(parts[1])?;
                return Ok(Type::Result(Box::new(ok_type), Box::new(err_type)));
            }
            return Err(ParseError::new("Invalid Result type", loc.line, loc.column));
        }
        if name.starts_with("chan[") && name.ends_with(']') {
            // Channel type: @chan[T]
            let inner = &name[5..name.len()-1];
            let inner_type = self.type_from_simple_name(inner)?;
            return Ok(Type::Channel(Box::new(inner_type)));
        }
        if name.starts_with('[') && name.ends_with(']') {
            // List type: @[T]
            let inner = &name[1..name.len()-1];
            let inner_type = self.type_from_simple_name(inner)?;
            return Ok(Type::List(Box::new(inner_type)));
        }

        // Simple types
        match name {
            "i8" => Ok(Type::I8),
            "i16" => Ok(Type::I16),
            "i32" => Ok(Type::I32),
            "i64" => Ok(Type::I64),
            "u8" => Ok(Type::U8),
            "u16" => Ok(Type::U16),
            "u32" => Ok(Type::U32),
            "u64" => Ok(Type::U64),
            "f32" => Ok(Type::F32),
            "f64" => Ok(Type::F64),
            "b" => Ok(Type::Bool),
            "t" => Ok(Type::Str),
            "c" => Ok(Type::Char),
            _ => Ok(Type::Named(name.to_string()))
        }
    }

    /// Convert simple type name to Type
    fn type_from_simple_name(&self, name: &str) -> Result<Type, ParseError> {
        match name {
            "i8" | "@i8" => Ok(Type::I8),
            "i16" | "@i16" => Ok(Type::I16),
            "i32" | "@i32" => Ok(Type::I32),
            "i64" | "@i64" => Ok(Type::I64),
            "u8" | "@u8" => Ok(Type::U8),
            "u16" | "@u16" => Ok(Type::U16),
            "u32" | "@u32" => Ok(Type::U32),
            "u64" | "@u64" => Ok(Type::U64),
            "f32" | "@f32" => Ok(Type::F32),
            "f64" | "@f64" => Ok(Type::F64),
            "b" | "@b" => Ok(Type::Bool),
            "t" | "@t" => Ok(Type::Str),
            "c" | "@c" => Ok(Type::Char),
            _ => Ok(Type::Named(name.to_string()))
        }
    }

    /// Parse module path for imports (e.g., std.math, std.io)
    fn parse_module_path(&mut self) -> Result<String, ParseError> {
        let mut path = String::new();

        loop {
            if let Token::Ident(name) = self.current() {
                path.push_str(&name);
                self.advance();
            } else {
                break;
            }

            // Check for path separator (. for module paths)
            if let Token::Ident(ref next) = self.current() {
                if next.starts_with('.') {
                    // Already has dot prefix
                    continue;
                }
            }

            // Check for :: or . separator
            if self.check(&Token::Qualify) {
                path.push_str("::");
                self.advance();
            } else if let Token::Ident(ref name) = self.current() {
                if !name.starts_with('.') {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(path)
    }

    /// Parse path for imports
    fn parse_path(&mut self) -> Result<String, ParseError> {
        let mut path = String::new();

        loop {
            if let Token::Ident(name) = self.current() {
                path.push_str(&name);
                self.advance();
            } else {
                break;
            }

            // Check for path separator
            if self.check(&Token::Qualify) {
                path.push_str("::");
                self.advance();
            } else if let Token::Ident(ref next) = self.current() {
                if next.starts_with('/') {
                    path.push_str(next);
                    self.advance();
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        Ok(path)
    }

    /// Parse string or identifier
    fn parse_string_or_ident(&mut self) -> Result<String, ParseError> {
        let loc = self.current_span();
        match self.current() {
            Token::String(s) => {
                self.advance();
                Ok(s)
            }
            Token::Ident(s) => {
                self.advance();
                Ok(s)
            }
            _ => Err(ParseError::new("Expected string or identifier", loc.line, loc.column))
        }
    }

    /// Parse expression list until terminator
    fn parse_expr_list_until(&mut self, terminator: &Token) -> Result<Vec<Node<Expr>>, ParseError> {
        let mut exprs = Vec::new();

        while !self.check(terminator) && !self.check(&Token::Eof) {
            self.skip_newlines();
            if self.check(terminator) { break; }

            exprs.push(self.parse_expr()?);

            // Skip separator
            while self.check(&Token::Semicolon) || self.check(&Token::Newline) {
                self.advance();
            }
        }

        Ok(exprs)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse(input: &str) -> Result<Program, ParseError> {
        Parser::parse_source(input)
    }

    fn parse_ok(input: &str) -> Program {
        parse(input).expect(&format!("Failed to parse: {}", input))
    }

    fn parse_expr(input: &str) -> Expr {
        let prog = parse_ok(input);
        match &prog.stmts[0].node {
            Stmt::Expr(e) => e.node.clone(),
            _ => panic!("Expected expression statement"),
        }
    }

    // === Binding ===
    #[test]
    fn test_parse_bind() {
        let prog = parse_ok("^= x 42");
        assert_eq!(prog.stmts.len(), 1);
        match &prog.stmts[0].node {
            Stmt::Bind(name, _) => assert_eq!(name, "x"),
            _ => panic!("Expected Bind"),
        }
    }

    #[test]
    fn test_parse_bind_string() {
        let prog = parse_ok("^= msg \"hello\"");
        assert_eq!(prog.stmts.len(), 1);
    }

    #[test]
    fn test_parse_typed_bind() {
        let prog = parse_ok("^: x i32 42");
        assert_eq!(prog.stmts.len(), 1);
        match &prog.stmts[0].node {
            Stmt::TypedBind(name, ty, _) => {
                assert_eq!(name, "x");
                assert_eq!(*ty, Type::I32);
            }
            _ => panic!("Expected TypedBind"),
        }
    }

    // === Output ===
    #[test]
    fn test_parse_output() {
        let prog = parse_ok("~> \"hello\"");
        assert_eq!(prog.stmts.len(), 1);
        match &prog.stmts[0].node {
            Stmt::Output(_) => {}
            _ => panic!("Expected Output"),
        }
    }

    #[test]
    fn test_parse_output_raw() {
        let prog = parse_ok("~| \"test\"");
        match &prog.stmts[0].node {
            Stmt::OutputRaw(_) => {}
            _ => panic!("Expected OutputRaw"),
        }
    }

    #[test]
    fn test_parse_output_err() {
        let prog = parse_ok("~! \"error\"");
        match &prog.stmts[0].node {
            Stmt::OutputErr(_) => {}
            _ => panic!("Expected OutputErr"),
        }
    }

    // === Functions ===
    #[test]
    fn test_parse_function() {
        let prog = parse_ok("|= Add [a b] .+ $a $b");
        assert_eq!(prog.stmts.len(), 1);
        match &prog.stmts[0].node {
            Stmt::FuncDef(name, params, _) => {
                assert_eq!(name, "Add");
                assert_eq!(params, &vec!["a".to_string(), "b".to_string()]);
            }
            _ => panic!("Expected FuncDef"),
        }
    }

    #[test]
    fn test_parse_function_no_params() {
        let prog = parse_ok("|= GetPi [] 3.14");
        match &prog.stmts[0].node {
            Stmt::FuncDef(name, params, _) => {
                assert_eq!(name, "GetPi");
                assert!(params.is_empty());
            }
            _ => panic!("Expected FuncDef"),
        }
    }

    #[test]
    fn test_parse_lambda() {
        let expr = parse_expr("|> [x] .* $x 2");
        match expr {
            Expr::Lambda(params, _) => {
                assert_eq!(params, vec!["x".to_string()]);
            }
            _ => panic!("Expected Lambda"),
        }
    }

    // === Literals ===
    #[test]
    fn test_parse_int() {
        let expr = parse_expr("42");
        assert_eq!(expr, Expr::Lit(Literal::Int(42)));
    }

    #[test]
    fn test_parse_negative_int() {
        let expr = parse_expr("-42");
        assert_eq!(expr, Expr::Lit(Literal::Int(-42)));
    }

    #[test]
    fn test_parse_float() {
        let expr = parse_expr("3.14");
        assert_eq!(expr, Expr::Lit(Literal::Float(3.14)));
    }

    #[test]
    fn test_parse_string() {
        let expr = parse_expr("\"hello\"");
        assert_eq!(expr, Expr::Lit(Literal::String("hello".to_string())));
    }

    #[test]
    fn test_parse_bool_true() {
        let expr = parse_expr("true");
        assert_eq!(expr, Expr::Lit(Literal::Bool(true)));
    }

    #[test]
    fn test_parse_bool_false() {
        let expr = parse_expr("false");
        assert_eq!(expr, Expr::Lit(Literal::Bool(false)));
    }

    #[test]
    fn test_parse_none() {
        let expr = parse_expr("none");
        assert_eq!(expr, Expr::Lit(Literal::None));
    }

    // === Variables ===
    #[test]
    fn test_parse_var() {
        let expr = parse_expr("$x");
        assert_eq!(expr, Expr::Var("x".to_string()));
    }

    #[test]
    fn test_parse_env_var() {
        let expr = parse_expr("$$PATH");
        assert_eq!(expr, Expr::EnvVar("PATH".to_string()));
    }

    // === List ===
    #[test]
    fn test_parse_list() {
        let prog = parse_ok("^= xs [1 2 3]");
        assert_eq!(prog.stmts.len(), 1);
    }

    #[test]
    fn test_parse_empty_list() {
        let expr = parse_expr("[]");
        match expr {
            Expr::List(items) => assert!(items.is_empty()),
            _ => panic!("Expected List"),
        }
    }

    #[test]
    fn test_parse_nested_list() {
        let expr = parse_expr("[[1 2] [3 4]]");
        match expr {
            Expr::List(items) => assert_eq!(items.len(), 2),
            _ => panic!("Expected List"),
        }
    }

    // === Record ===
    #[test]
    fn test_parse_record() {
        let expr = parse_expr("{name: \"Alice\", age: 30}");
        match expr {
            Expr::Record(fields) => {
                assert_eq!(fields.len(), 2);
                assert_eq!(fields[0].0, "name");
                assert_eq!(fields[1].0, "age");
            }
            _ => panic!("Expected Record"),
        }
    }

    #[test]
    fn test_parse_empty_record() {
        let expr = parse_expr("{}");
        match expr {
            Expr::Record(fields) => assert!(fields.is_empty()),
            _ => panic!("Expected Record"),
        }
    }

    // === Conditional ===
    #[test]
    fn test_parse_cond() {
        let prog = parse_ok("?: true \"yes\" \"no\"");
        assert_eq!(prog.stmts.len(), 1);
    }

    #[test]
    fn test_parse_cond_multi() {
        let expr = parse_expr("?: | true -> 1 | false -> 0");
        match expr {
            Expr::CondMulti(branches) => {
                assert_eq!(branches.len(), 2);
            }
            _ => panic!("Expected CondMulti"),
        }
    }

    // === Arithmetic ===
    #[test]
    fn test_parse_add() {
        let expr = parse_expr(".+ 1 2");
        match expr {
            Expr::BinOp(BinOp::Add, _, _) => {}
            _ => panic!("Expected BinOp Add"),
        }
    }

    #[test]
    fn test_parse_nested_arithmetic() {
        let expr = parse_expr(".* (.+ 1 2) 3");
        match expr {
            Expr::BinOp(BinOp::Mul, lhs, _) => {
                match &lhs.node {
                    Expr::BinOp(BinOp::Add, _, _) => {}
                    _ => panic!("Expected nested Add"),
                }
            }
            _ => panic!("Expected BinOp Mul"),
        }
    }

    // === Comparison ===
    #[test]
    fn test_parse_eq() {
        let expr = parse_expr(".= 1 1");
        match expr {
            Expr::BinOp(BinOp::Eq, _, _) => {}
            _ => panic!("Expected BinOp Eq"),
        }
    }

    #[test]
    fn test_parse_lt() {
        let expr = parse_expr(".< 1 2");
        match expr {
            Expr::BinOp(BinOp::Lt, _, _) => {}
            _ => panic!("Expected BinOp Lt"),
        }
    }

    // === Logical ===
    #[test]
    fn test_parse_and() {
        let expr = parse_expr(".& true false");
        match expr {
            Expr::BinOp(BinOp::And, _, _) => {}
            _ => panic!("Expected BinOp And"),
        }
    }

    #[test]
    fn test_parse_not() {
        let expr = parse_expr(".! true");
        match expr {
            Expr::UnaryOp(UnaryOp::Not, _) => {}
            _ => panic!("Expected UnaryOp Not"),
        }
    }

    // === Iteration ===
    #[test]
    fn test_parse_map() {
        let expr = parse_expr("%> $xs [x] .* $x 2");
        match expr {
            Expr::Map(_, var, _) => assert_eq!(var, "x"),
            _ => panic!("Expected Map"),
        }
    }

    #[test]
    fn test_parse_filter() {
        let expr = parse_expr("%< $xs [x] .> $x 0");
        match expr {
            Expr::Filter(_, var, _) => assert_eq!(var, "x"),
            _ => panic!("Expected Filter"),
        }
    }

    #[test]
    fn test_parse_fold() {
        let expr = parse_expr("%/ $xs 0 [acc x] .+ $acc $x");
        match expr {
            Expr::Fold(_, _, acc, var, _) => {
                assert_eq!(acc, "acc");
                assert_eq!(var, "x");
            }
            _ => panic!("Expected Fold"),
        }
    }

    #[test]
    fn test_parse_times() {
        let expr = parse_expr("%~ 5 [i] $i");
        match expr {
            Expr::Times(_, var, _) => assert_eq!(var, "i"),
            _ => panic!("Expected Times"),
        }
    }

    #[test]
    fn test_parse_range() {
        let expr = parse_expr("%.. 1 10");
        match expr {
            Expr::Range(_, _) => {}
            _ => panic!("Expected Range"),
        }
    }

    // === Loop ===
    #[test]
    fn test_parse_loop() {
        let expr = parse_expr("?@ true 1");
        match expr {
            Expr::Loop(_, _) => {}
            _ => panic!("Expected Loop"),
        }
    }

    // === Function Call ===
    #[test]
    fn test_parse_call() {
        let expr = parse_expr("(Add 1 2)");
        match expr {
            Expr::Call(func, args) => {
                match &func.node {
                    Expr::Var(name) => assert_eq!(name, "Add"),
                    _ => panic!("Expected Var"),
                }
                assert_eq!(args.len(), 2);
            }
            _ => panic!("Expected Call"),
        }
    }

    #[test]
    fn test_parse_call_no_args() {
        let expr = parse_expr("(GetPi)");
        match expr {
            Expr::Var(name) => assert_eq!(name, "GetPi"),
            _ => panic!("Expected Var (single element paren is grouping)"),
        }
    }

    // === Option/Result ===
    #[test]
    fn test_parse_some() {
        let expr = parse_expr("(some 42)");
        match expr {
            Expr::Some(_) => {}
            _ => panic!("Expected Some"),
        }
    }

    #[test]
    fn test_parse_ok() {
        let expr = parse_expr("(ok 42)");
        match expr {
            Expr::Ok(_) => {}
            _ => panic!("Expected Ok"),
        }
    }

    #[test]
    fn test_parse_err() {
        let expr = parse_expr("(err \"oops\")");
        match expr {
            Expr::Err(_) => {}
            _ => panic!("Expected Err"),
        }
    }

    // === Block ===
    #[test]
    fn test_parse_block() {
        let expr = parse_expr("{ ^= x 1; ^= y 2; .+ $x $y }");
        match expr {
            Expr::Block(stmts) => assert_eq!(stmts.len(), 3),
            _ => panic!("Expected Block"),
        }
    }

    // === Exec ===
    #[test]
    fn test_parse_exec() {
        let expr = parse_expr("!! `echo hello`");
        match expr {
            Expr::Exec(cmd) => {
                assert_eq!(cmd, "echo hello");
            }
            _ => panic!("Expected Exec"),
        }
    }

    // === Multiple statements ===
    #[test]
    fn test_parse_multiple_statements() {
        let prog = parse_ok("^= x 1\n^= y 2\n~> .+ $x $y");
        assert_eq!(prog.stmts.len(), 3);
    }

    // === Error cases ===
    #[test]
    fn test_parse_error_unclosed_bracket() {
        assert!(parse("[1 2 3").is_err());
    }

    #[test]
    fn test_parse_error_unclosed_paren() {
        assert!(parse("(Add 1 2").is_err());
    }
}