//! Hedgehog Lexer
//!
//! Tokenizes Hedgehog source code into a stream of tokens.

use std::fmt;
use std::iter::Peekable;
use std::str::Chars;

/// Part of an interpolated string
#[derive(Debug, Clone, PartialEq)]
pub enum StringPart {
    /// Literal text
    Lit(String),
    /// Variable reference: $name
    Var(String),
}

/// Token types for Hedgehog
#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    // Berries (symbolic operators)

    // Binding
    Bind,       // ^=
    TypedBind,  // ^:

    // Function
    Func,        // |=
    Lambda,      // |>
    TypedFunc,   // |:
    TypedLambda, // |;

    // Output
    Out,        // ~>
    OutRaw,     // ~|
    OutErr,     // ~!
    FileWrite,  // ~+
    FileAppend, // ~^
    BinaryWrite,// ~*

    // Control
    Cond,       // ?:
    Match,      // ??
    Loop,       // ?@
    Trap,       // ?!

    // Iteration (% prefix)
    Map,        // %>
    Filter,     // %<
    Fold,       // %/
    Times,      // %~
    Range,      // %..

    // Arithmetic
    Add,        // .+
    Sub,        // .-
    Mul,        // .*
    Div,        // ./
    Mod,        // .%
    Pow,        // .^

    // Comparison
    Eq,         // .=
    Neq,        // .~
    Lt,         // .<
    Gt,         // .>
    Lte,        // .<=
    Gte,        // .>=

    // Logical
    And,        // .&
    Or,         // .|
    Not,        // .!

    // Parallel
    ParEach,    // &%
    ParPipe,    // &>
    Background, // &!
    ParJoin,    // &=
    Race,       // &?

    // Command
    Exec,       // !!

    // Input
    Stdin,      // <~
    FileRead,   // <+
    BinaryRead, // <*

    // Channel
    ChanSend,   // ->
    ChanRecv,   // <-

    // Module
    LibImport,  // /+
    AliasImport,// /=
    RelImport,  // /.

    // Data Operations
    Pipeline,   // ::
    VarApply,   // :*
    NestAccess, // :.
    Terminator, // ==

    // Type Operations
    TypeCheck,  // :?
    TypeCast,   // :>
    TypeOf,     // :@

    // Other Berries
    EnvRef,         // $$
    GlobalRef,      // $>
    ParentRef,      // $<
    Args,           // $@
    ArgIndex(u32),  // $@.n
    ArgCount,       // $@#

    // Definition
    StructDef,  // ^-

    // Single-char tokens
    Dollar,     // $
    Star,       // *
    Percent,    // %
    Hash,       // #
    Semicolon,  // ;
    Comma,      // ,
    Colon,      // :
    Underscore, // _
    Bar,        // |
    Arrow,      // -> (used in pattern matching)

    // Brackets
    LParen,     // (
    RParen,     // )
    LBracket,   // [
    RBracket,   // ]
    LBrace,     // {
    RBrace,     // }

    // Literals
    Int(i64),
    Float(f64),
    String(String),
    InterpString(Vec<StringPart>),
    RawString(String),
    CommandString(String),

    // Keywords (reserved identifiers)
    True,
    False,
    None,

    // Identifier
    Ident(String),

    // Type literal (@u8, @t, @b, etc.)
    TypeLit(String),

    // Special
    Newline,
    Eof,
}

/// Span information for error reporting
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Span {
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

impl Span {
    pub fn new(line: usize, column: usize, offset: usize) -> Self {
        Self { line, column, offset }
    }
}

/// Token with span information
#[derive(Debug, Clone)]
pub struct SpannedToken {
    pub token: Token,
    pub span: Span,
}

/// Lexer error
#[derive(Debug, Clone)]
pub struct LexerError {
    pub message: String,
    pub span: Span,
}

impl fmt::Display for LexerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Lexer error at {}:{}: {}", self.span.line, self.span.column, self.message)
    }
}

/// Lexer for Hedgehog
pub struct Lexer<'a> {
    input: Peekable<Chars<'a>>,
    line: usize,
    column: usize,
    offset: usize,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            input: input.chars().peekable(),
            line: 1,
            column: 1,
            offset: 0,
        }
    }

    fn current_span(&self) -> Span {
        Span::new(self.line, self.column, self.offset)
    }

    fn peek(&mut self) -> Option<&char> {
        self.input.peek()
    }

    fn advance(&mut self) -> Option<char> {
        let c = self.input.next()?;
        self.offset += 1;
        if c == '\n' {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
        Some(c)
    }

    fn skip_whitespace(&mut self) {
        while let Some(&c) = self.peek() {
            if c == ' ' || c == '\t' || c == '\r' {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn skip_comment(&mut self) {
        while let Some(&c) = self.peek() {
            if c == '\n' {
                break;
            }
            self.advance();
        }
    }

    fn read_string(&mut self, quote: char) -> Result<String, LexerError> {
        let start_span = self.current_span();
        // 注意: 開始クォートはすでに next_token で消費済み

        let mut result = String::new();

        loop {
            match self.advance() {
                Some(c) if c == quote => break,
                Some('\\') => {
                    match self.advance() {
                        Some('n') => result.push('\n'),
                        Some('t') => result.push('\t'),
                        Some('r') => result.push('\r'),
                        Some('\\') => result.push('\\'),
                        Some('"') => result.push('"'),
                        Some('\'') => result.push('\''),
                        Some('$') => result.push('$'),
                        Some('0') => result.push('\0'),
                        Some(c) => {
                            return Err(LexerError {
                                message: format!("Invalid escape sequence: \\{}", c),
                                span: self.current_span(),
                            });
                        }
                        None => {
                            return Err(LexerError {
                                message: "Unterminated string".to_string(),
                                span: start_span,
                            });
                        }
                    }
                }
                Some(c) => result.push(c),
                None => {
                    return Err(LexerError {
                        message: "Unterminated string".to_string(),
                        span: start_span,
                    });
                }
            }
        }

        Ok(result)
    }

    /// Read an interpolated string (double-quoted)
    /// Handles $var interpolation
    fn read_interp_string(&mut self) -> Result<Vec<StringPart>, LexerError> {
        let start_span = self.current_span();
        let mut parts: Vec<StringPart> = Vec::new();
        let mut current_lit = String::new();

        loop {
            match self.peek() {
                Some(&'"') => {
                    self.advance();
                    break;
                }
                Some(&'\\') => {
                    self.advance();
                    match self.advance() {
                        Some('n') => current_lit.push('\n'),
                        Some('t') => current_lit.push('\t'),
                        Some('r') => current_lit.push('\r'),
                        Some('\\') => current_lit.push('\\'),
                        Some('"') => current_lit.push('"'),
                        Some('\'') => current_lit.push('\''),
                        Some('$') => current_lit.push('$'),
                        Some('0') => current_lit.push('\0'),
                        Some(c) => {
                            return Err(LexerError {
                                message: format!("Invalid escape sequence: \\{}", c),
                                span: self.current_span(),
                            });
                        }
                        None => {
                            return Err(LexerError {
                                message: "Unterminated string".to_string(),
                                span: start_span,
                            });
                        }
                    }
                }
                Some(&'$') => {
                    self.advance();
                    // Check what follows $
                    match self.peek() {
                        Some(&c) if c.is_alphabetic() || c == '_' => {
                            // $var form
                            if !current_lit.is_empty() {
                                parts.push(StringPart::Lit(std::mem::take(&mut current_lit)));
                            }
                            let var = self.read_interp_var();
                            parts.push(StringPart::Var(var));
                        }
                        _ => {
                            // Just a literal $
                            current_lit.push('$');
                        }
                    }
                }
                Some(_) => {
                    current_lit.push(self.advance().unwrap());
                }
                None => {
                    return Err(LexerError {
                        message: "Unterminated string".to_string(),
                        span: start_span,
                    });
                }
            }
        }

        // Push remaining literal if any
        if !current_lit.is_empty() {
            parts.push(StringPart::Lit(current_lit));
        }

        Ok(parts)
    }

    /// Read a variable name for interpolation ($var)
    fn read_interp_var(&mut self) -> String {
        let mut name = String::new();
        while let Some(&c) = self.peek() {
            if c.is_alphanumeric() || c == '_' || c == '-' {
                name.push(self.advance().unwrap());
            } else {
                break;
            }
        }
        name
    }

    fn read_command_string(&mut self) -> Result<String, LexerError> {
        let start_span = self.current_span();
        // 注意: 開始バッククォートはすでに消費済み

        let mut result = String::new();

        loop {
            match self.advance() {
                Some('`') => break,
                Some('\\') => {
                    match self.advance() {
                        Some('`') => result.push('`'),
                        Some('\\') => result.push('\\'),
                        Some(c) => {
                            result.push('\\');
                            result.push(c);
                        }
                        None => {
                            return Err(LexerError {
                                message: "Unterminated command string".to_string(),
                                span: start_span,
                            });
                        }
                    }
                }
                Some(c) => result.push(c),
                None => {
                    return Err(LexerError {
                        message: "Unterminated command string".to_string(),
                        span: start_span,
                    });
                }
            }
        }

        Ok(result)
    }

    fn read_number(&mut self, first: char) -> Result<Token, LexerError> {
        let mut num_str = String::new();
        num_str.push(first);

        // Check for hex, octal, binary
        if first == '0' {
            match self.peek() {
                Some('x') | Some('X') => {
                    self.advance();
                    num_str.clear();
                    while let Some(&c) = self.peek() {
                        if c.is_ascii_hexdigit() || c == '_' {
                            if c != '_' {
                                num_str.push(c);
                            }
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    if num_str.is_empty() {
                        return Err(LexerError {
                            message: "Invalid hex number".to_string(),
                            span: self.current_span(),
                        });
                    }
                    let value = i64::from_str_radix(&num_str, 16)
                        .map_err(|_| LexerError {
                            message: "Invalid hex number".to_string(),
                            span: self.current_span(),
                        })?;
                    return Ok(Token::Int(value));
                }
                Some('o') | Some('O') => {
                    self.advance();
                    num_str.clear();
                    while let Some(&c) = self.peek() {
                        if ('0'..='7').contains(&c) || c == '_' {
                            if c != '_' {
                                num_str.push(c);
                            }
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    if num_str.is_empty() {
                        return Err(LexerError {
                            message: "Invalid octal number".to_string(),
                            span: self.current_span(),
                        });
                    }
                    let value = i64::from_str_radix(&num_str, 8)
                        .map_err(|_| LexerError {
                            message: "Invalid octal number".to_string(),
                            span: self.current_span(),
                        })?;
                    return Ok(Token::Int(value));
                }
                Some('b') | Some('B') => {
                    self.advance();
                    num_str.clear();
                    while let Some(&c) = self.peek() {
                        if c == '0' || c == '1' || c == '_' {
                            if c != '_' {
                                num_str.push(c);
                            }
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    if num_str.is_empty() {
                        return Err(LexerError {
                            message: "Invalid binary number".to_string(),
                            span: self.current_span(),
                        });
                    }
                    let value = i64::from_str_radix(&num_str, 2)
                        .map_err(|_| LexerError {
                            message: "Invalid binary number".to_string(),
                            span: self.current_span(),
                        })?;
                    return Ok(Token::Int(value));
                }
                _ => {}
            }
        }

        // Regular decimal number
        let mut is_float = false;

        while let Some(&c) = self.peek() {
            if c.is_ascii_digit() || c == '_' {
                if c != '_' {
                    num_str.push(c);
                }
                self.advance();
            } else if c == '.' {
                // Check if this is a decimal point or a berry
                // Look ahead to see if next char is a digit
                self.advance();
                if let Some(&next) = self.peek() {
                    if next.is_ascii_digit() {
                        is_float = true;
                        num_str.push('.');
                        num_str.push(next);
                        self.advance();
                    } else {
                        // It's a berry like .+ - put '.' back by parsing what we have
                        // This is tricky - we've consumed the '.'
                        // For now, just return the integer
                        // The '.' will be handled on next call
                        // Actually we can't "unread", so let's handle this differently
                        // We need to not consume '.' if it's not followed by digit
                        break;
                    }
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        // Continue reading float digits after decimal
        if is_float {
            while let Some(&c) = self.peek() {
                if c.is_ascii_digit() || c == '_' {
                    if c != '_' {
                        num_str.push(c);
                    }
                    self.advance();
                } else {
                    break;
                }
            }
        }

        // Check for exponent
        if let Some(&c) = self.peek() {
            if c == 'e' || c == 'E' {
                is_float = true;
                num_str.push(c);
                self.advance();

                if let Some(&sign) = self.peek() {
                    if sign == '+' || sign == '-' {
                        num_str.push(sign);
                        self.advance();
                    }
                }

                while let Some(&c) = self.peek() {
                    if c.is_ascii_digit() {
                        num_str.push(c);
                        self.advance();
                    } else {
                        break;
                    }
                }
            }
        }

        // Check for type suffix like :u8, :i32
        if let Some(&':') = self.peek() {
            self.advance();
            let mut suffix = String::new();
            while let Some(&c) = self.peek() {
                if c.is_alphanumeric() {
                    suffix.push(c);
                    self.advance();
                } else {
                    break;
                }
            }
            // For now, ignore the suffix and just parse the number
        }

        if is_float {
            let value: f64 = num_str.parse()
                .map_err(|_| LexerError {
                    message: "Invalid float number".to_string(),
                    span: self.current_span(),
                })?;
            Ok(Token::Float(value))
        } else {
            let value: i64 = num_str.parse()
                .map_err(|_| LexerError {
                    message: "Invalid integer".to_string(),
                    span: self.current_span(),
                })?;
            Ok(Token::Int(value))
        }
    }

    fn read_identifier(&mut self, first: char) -> Token {
        let mut ident = String::new();
        ident.push(first);

        while let Some(&c) = self.peek() {
            // 識別子に ?, !, - を含められる (some?, ok?, unwrap-or など)
            if c.is_alphanumeric() || c == '_' || c == '-' || c == '?' || c == '!' {
                ident.push(c);
                self.advance();
            } else {
                break;
            }
        }

        // Check for reserved words
        match ident.as_str() {
            "true" => Token::True,
            "false" => Token::False,
            "none" => Token::None,
            _ => Token::Ident(ident),
        }
    }

    /// Get the next token
    pub fn next_token(&mut self) -> Result<SpannedToken, LexerError> {
        self.skip_whitespace();

        let span = self.current_span();

        let c = match self.advance() {
            Some(c) => c,
            None => return Ok(SpannedToken { token: Token::Eof, span }),
        };

        let token = match c {
            // Comment
            '#' => {
                self.skip_comment();
                return self.next_token();
            }

            // Newline
            '\n' => Token::Newline,

            // Multi-char berries (check longest first)

            // ^ berries (definition)
            '^' => match self.peek() {
                Some('=') => { self.advance(); Token::Bind }
                Some(':') => { self.advance(); Token::TypedBind }
                Some('-') => { self.advance(); Token::StructDef }
                _ => {
                    return Err(LexerError {
                        message: format!("Unexpected character after '^': {:?}", self.peek()),
                        span,
                    });
                }
            },

            // | berries
            '|' => match self.peek() {
                Some('=') => { self.advance(); Token::Func }
                Some('>') => { self.advance(); Token::Lambda }
                Some(':') => { self.advance(); Token::TypedFunc }
                Some(';') => { self.advance(); Token::TypedLambda }
                _ => Token::Bar,
            },

            // ~ berries (output)
            '~' => match self.peek() {
                Some('>') => { self.advance(); Token::Out }
                Some('|') => { self.advance(); Token::OutRaw }
                Some('!') => { self.advance(); Token::OutErr }
                Some('+') => { self.advance(); Token::FileWrite }
                Some('^') => { self.advance(); Token::FileAppend }
                Some('*') => { self.advance(); Token::BinaryWrite }
                _ => {
                    return Err(LexerError {
                        message: format!("Unexpected character after '~': {:?}", self.peek()),
                        span,
                    });
                }
            },

            // ? berries
            '?' => match self.peek() {
                Some(':') => { self.advance(); Token::Cond }
                Some('?') => { self.advance(); Token::Match }
                Some('@') => { self.advance(); Token::Loop }
                Some('!') => { self.advance(); Token::Trap }
                _ => {
                    return Err(LexerError {
                        message: format!("Unexpected character after '?': {:?}", self.peek()),
                        span,
                    });
                }
            },

            // % berries (iteration)
            '%' => match self.peek() {
                Some('>') => { self.advance(); Token::Map }
                Some('<') => { self.advance(); Token::Filter }
                Some('/') => { self.advance(); Token::Fold }
                Some('~') => { self.advance(); Token::Times }
                Some('.') => {
                    self.advance();
                    if self.peek() == Some(&'.') {
                        self.advance();
                        Token::Range
                    } else {
                        return Err(LexerError {
                            message: "Expected '..' after '%.'".to_string(),
                            span,
                        });
                    }
                }
                _ => Token::Percent  // % alone for modulo (handled by .%)
            },

            // . berries (operators)
            '.' => match self.peek() {
                Some('+') => { self.advance(); Token::Add }
                Some('-') => { self.advance(); Token::Sub }
                Some('*') => { self.advance(); Token::Mul }
                Some('/') => { self.advance(); Token::Div }
                Some('%') => { self.advance(); Token::Mod }
                Some('^') => { self.advance(); Token::Pow }
                Some('=') => { self.advance(); Token::Eq }
                Some('~') => { self.advance(); Token::Neq }
                Some('<') => {
                    self.advance();
                    if self.peek() == Some(&'=') {
                        self.advance();
                        Token::Lte
                    } else {
                        Token::Lt
                    }
                }
                Some('>') => {
                    self.advance();
                    if self.peek() == Some(&'=') {
                        self.advance();
                        Token::Gte
                    } else {
                        Token::Gt
                    }
                }
                Some('&') => { self.advance(); Token::And }
                Some('|') => { self.advance(); Token::Or }
                Some('!') => { self.advance(); Token::Not }
                _ => {
                    return Err(LexerError {
                        message: format!("Unexpected character after '.': {:?}", self.peek()),
                        span,
                    });
                }
            },

            // & berries
            '&' => match self.peek() {
                Some('%') => { self.advance(); Token::ParEach }
                Some('>') => { self.advance(); Token::ParPipe }
                Some('!') => { self.advance(); Token::Background }
                Some('?') => { self.advance(); Token::Race }
                Some('=') => { self.advance(); Token::ParJoin }
                _ => {
                    return Err(LexerError {
                        message: format!("Unexpected character after '&': {:?}", self.peek()),
                        span,
                    });
                }
            },

            // ! berries
            '!' => match self.peek() {
                Some('!') => { self.advance(); Token::Exec }
                _ => {
                    return Err(LexerError {
                        message: format!("Unexpected character after '!': {:?}", self.peek()),
                        span,
                    });
                }
            },

            // : berries (type operations & data operations)
            ':' => match self.peek() {
                Some('?') => { self.advance(); Token::TypeCheck }
                Some('>') => { self.advance(); Token::TypeCast }
                Some('@') => { self.advance(); Token::TypeOf }
                Some(':') => { self.advance(); Token::Pipeline }
                Some('*') => { self.advance(); Token::VarApply }
                Some('.') => { self.advance(); Token::NestAccess }
                _ => Token::Colon,
            },

            // = berries (terminator)
            '=' => match self.peek() {
                Some('=') => { self.advance(); Token::Terminator }
                _ => {
                    return Err(LexerError {
                        message: format!("Unexpected character after '=': {:?}", self.peek()),
                        span,
                    });
                }
            },

            // < berries (input & channel)
            '<' => match self.peek() {
                Some('~') => { self.advance(); Token::Stdin }
                Some('+') => { self.advance(); Token::FileRead }
                Some('*') => { self.advance(); Token::BinaryRead }
                Some('-') => { self.advance(); Token::ChanRecv }
                _ => {
                    return Err(LexerError {
                        message: format!("Unexpected character after '<': {:?}", self.peek()),
                        span,
                    });
                }
            },

            // - berries
            '-' => match self.peek() {
                Some('>') => { self.advance(); Token::Arrow }
                Some(c) if c.is_ascii_digit() => {
                    let digit = self.advance().unwrap();
                    let mut token = self.read_number(digit)?;
                    match &mut token {
                        Token::Int(n) => *n = -*n,
                        Token::Float(n) => *n = -*n,
                        _ => {}
                    }
                    token
                }
                _ => {
                    // Could be a command option like -la
                    self.read_identifier('-')
                }
            },

            // * (just star)
            '*' => Token::Star,

            // / berries (imports)
            '/' => match self.peek() {
                Some('+') => { self.advance(); Token::LibImport }
                Some('=') => { self.advance(); Token::AliasImport }
                Some('.') => { self.advance(); Token::RelImport }
                _ => {
                    // Could be part of a path
                    let mut path = String::new();
                    path.push('/');
                    while let Some(&c) = self.peek() {
                        if c.is_alphanumeric() || c == '/' || c == '_' || c == '-' || c == '.' {
                            path.push(c);
                            self.advance();
                        } else {
                            break;
                        }
                    }
                    if path.len() > 1 {
                        Token::Ident(path)
                    } else {
                        return Err(LexerError {
                            message: "Unexpected '/'".to_string(),
                            span,
                        });
                    }
                }
            },

            // $ berries
            '$' => match self.peek() {
                Some('$') => { self.advance(); Token::EnvRef }
                Some('>') => { self.advance(); Token::GlobalRef }
                Some('<') => { self.advance(); Token::ParentRef }
                Some('@') => {
                    self.advance();
                    match self.peek() {
                        Some('#') => { self.advance(); Token::ArgCount }
                        Some('.') => {
                            self.advance();
                            // Read the index number
                            let mut num = String::new();
                            while let Some(&c) = self.peek() {
                                if c.is_ascii_digit() {
                                    num.push(c);
                                    self.advance();
                                } else {
                                    break;
                                }
                            }
                            if num.is_empty() {
                                return Err(LexerError {
                                    message: "Expected number after '$@.'".to_string(),
                                    span,
                                });
                            }
                            let idx = num.parse::<u32>().unwrap_or(0);
                            Token::ArgIndex(idx)
                        }
                        _ => Token::Args
                    }
                }
                _ => Token::Dollar,
            },

            // @ type literals
            '@' => {
                // Read type name (like @i, @s, @?i, @!i, []@i, etc.)
                let mut type_name = String::new();
                let mut bracket_depth = 0;
                while let Some(&c) = self.peek() {
                    if c == '[' {
                        bracket_depth += 1;
                        type_name.push(c);
                        self.advance();
                    } else if c == ']' && bracket_depth > 0 {
                        bracket_depth -= 1;
                        type_name.push(c);
                        self.advance();
                    } else if c.is_alphanumeric() || c == '?' || c == '!' {
                        type_name.push(c);
                        self.advance();
                    } else {
                        break;
                    }
                }
                if type_name.is_empty() {
                    return Err(LexerError {
                        message: "Expected type name after '@'".to_string(),
                        span,
                    });
                }
                Token::TypeLit(type_name)
            },

            // Single char tokens
            ';' => Token::Semicolon,
            ',' => Token::Comma,
            '_' => Token::Underscore,
            '(' => Token::LParen,
            ')' => Token::RParen,
            '[' => Token::LBracket,
            ']' => Token::RBracket,
            '{' => Token::LBrace,
            '}' => Token::RBrace,

            // Strings
            '"' => {
                let parts = self.read_interp_string()?;
                // Optimize: if only one literal part, return plain String
                if parts.len() == 1 {
                    if let StringPart::Lit(s) = &parts[0] {
                        return Ok(SpannedToken { token: Token::String(s.clone()), span });
                    }
                }
                if parts.is_empty() {
                    Token::String(String::new())
                } else {
                    Token::InterpString(parts)
                }
            }
            '\'' => {
                let s = self.read_string('\'')?;
                Token::RawString(s)
            }
            '`' => {
                let s = self.read_command_string()?;
                Token::CommandString(s)
            }

            // Numbers
            c if c.is_ascii_digit() => self.read_number(c)?,

            // Identifiers
            c if c.is_alphabetic() || c == '_' => self.read_identifier(c),

            // Unknown
            c => {
                return Err(LexerError {
                    message: format!("Unexpected character: '{}'", c),
                    span,
                });
            }
        };

        Ok(SpannedToken { token, span })
    }

    /// Tokenize entire input
    pub fn tokenize(&mut self) -> Result<Vec<SpannedToken>, LexerError> {
        let mut tokens = Vec::new();

        loop {
            let spanned = self.next_token()?;
            let is_eof = spanned.token == Token::Eof;
            tokens.push(spanned);

            if is_eof {
                break;
            }
        }

        Ok(tokens)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tokenize(input: &str) -> Vec<Token> {
        let mut lexer = Lexer::new(input);
        lexer.tokenize()
            .unwrap()
            .into_iter()
            .map(|st| st.token)
            .filter(|t| *t != Token::Newline && *t != Token::Eof)
            .collect()
    }

    fn tokenize_all(input: &str) -> Vec<Token> {
        let mut lexer = Lexer::new(input);
        lexer.tokenize()
            .unwrap()
            .into_iter()
            .map(|st| st.token)
            .collect()
    }

    // Binding
    #[test]
    fn test_bind() {
        assert_eq!(tokenize("^= x 42"), vec![
            Token::Bind,
            Token::Ident("x".to_string()),
            Token::Int(42),
        ]);
    }

    #[test]
    fn test_typed_bind() {
        assert_eq!(tokenize("^: x i32 42"), vec![
            Token::TypedBind,
            Token::Ident("x".to_string()),
            Token::Ident("i32".to_string()),
            Token::Int(42),
        ]);
    }

    // Functions
    #[test]
    fn test_function() {
        assert_eq!(tokenize("|= Add [a b] .+ $a $b"), vec![
            Token::Func,
            Token::Ident("Add".to_string()),
            Token::LBracket,
            Token::Ident("a".to_string()),
            Token::Ident("b".to_string()),
            Token::RBracket,
            Token::Add,
            Token::Dollar,
            Token::Ident("a".to_string()),
            Token::Dollar,
            Token::Ident("b".to_string()),
        ]);
    }

    #[test]
    fn test_lambda() {
        assert_eq!(tokenize("|> [x] .* $x 2"), vec![
            Token::Lambda,
            Token::LBracket,
            Token::Ident("x".to_string()),
            Token::RBracket,
            Token::Mul,
            Token::Dollar,
            Token::Ident("x".to_string()),
            Token::Int(2),
        ]);
    }

    #[test]
    fn test_typed_func() {
        assert_eq!(tokenize("|: Add [a @i b @i] @i .+ $a $b"), vec![
            Token::TypedFunc,
            Token::Ident("Add".to_string()),
            Token::LBracket,
            Token::Ident("a".to_string()),
            Token::TypeLit("i".to_string()),
            Token::Ident("b".to_string()),
            Token::TypeLit("i".to_string()),
            Token::RBracket,
            Token::TypeLit("i".to_string()),
            Token::Add,
            Token::Dollar,
            Token::Ident("a".to_string()),
            Token::Dollar,
            Token::Ident("b".to_string()),
        ]);
    }

    #[test]
    fn test_typed_lambda() {
        assert_eq!(tokenize("|; [x @i] @i .* $x 2"), vec![
            Token::TypedLambda,
            Token::LBracket,
            Token::Ident("x".to_string()),
            Token::TypeLit("i".to_string()),
            Token::RBracket,
            Token::TypeLit("i".to_string()),
            Token::Mul,
            Token::Dollar,
            Token::Ident("x".to_string()),
            Token::Int(2),
        ]);
    }

    // Output
    #[test]
    fn test_output() {
        assert_eq!(tokenize("~> \"hello\""), vec![
            Token::Out,
            Token::String("hello".to_string()),
        ]);
    }

    #[test]
    fn test_output_raw() {
        assert_eq!(tokenize("~| \"test\""), vec![
            Token::OutRaw,
            Token::String("test".to_string()),
        ]);
    }

    #[test]
    fn test_output_err() {
        assert_eq!(tokenize("~! \"error\""), vec![
            Token::OutErr,
            Token::String("error".to_string()),
        ]);
    }

    // Control
    #[test]
    fn test_cond() {
        assert_eq!(tokenize("?: true 1 0"), vec![
            Token::Cond,
            Token::True,
            Token::Int(1),
            Token::Int(0),
        ]);
    }

    #[test]
    fn test_match() {
        assert_eq!(tokenize("?? $x | 0 -> \"zero\""), vec![
            Token::Match,
            Token::Dollar,
            Token::Ident("x".to_string()),
            Token::Bar,
            Token::Int(0),
            Token::Arrow,
            Token::String("zero".to_string()),
        ]);
    }

    #[test]
    fn test_loop() {
        assert_eq!(tokenize("?@ true ~> 1"), vec![
            Token::Loop,
            Token::True,
            Token::Out,
            Token::Int(1),
        ]);
    }

    #[test]
    fn test_trap() {
        assert_eq!(tokenize("?! expr [e] handler"), vec![
            Token::Trap,
            Token::Ident("expr".to_string()),
            Token::LBracket,
            Token::Ident("e".to_string()),
            Token::RBracket,
            Token::Ident("handler".to_string()),
        ]);
    }

    // Iteration
    #[test]
    fn test_map() {
        assert_eq!(tokenize("%> $xs [x] .* $x 2"), vec![
            Token::Map,
            Token::Dollar,
            Token::Ident("xs".to_string()),
            Token::LBracket,
            Token::Ident("x".to_string()),
            Token::RBracket,
            Token::Mul,
            Token::Dollar,
            Token::Ident("x".to_string()),
            Token::Int(2),
        ]);
    }

    #[test]
    fn test_filter() {
        assert_eq!(tokenize("%< $xs [x] .> $x 0"), vec![
            Token::Filter,
            Token::Dollar,
            Token::Ident("xs".to_string()),
            Token::LBracket,
            Token::Ident("x".to_string()),
            Token::RBracket,
            Token::Gt,
            Token::Dollar,
            Token::Ident("x".to_string()),
            Token::Int(0),
        ]);
    }

    #[test]
    fn test_fold() {
        assert_eq!(tokenize("%/ $xs 0 [a x] .+ $a $x"), vec![
            Token::Fold,
            Token::Dollar,
            Token::Ident("xs".to_string()),
            Token::Int(0),
            Token::LBracket,
            Token::Ident("a".to_string()),
            Token::Ident("x".to_string()),
            Token::RBracket,
            Token::Add,
            Token::Dollar,
            Token::Ident("a".to_string()),
            Token::Dollar,
            Token::Ident("x".to_string()),
        ]);
    }

    #[test]
    fn test_times() {
        assert_eq!(tokenize("%~ 5 [i] ~> $i"), vec![
            Token::Times,
            Token::Int(5),
            Token::LBracket,
            Token::Ident("i".to_string()),
            Token::RBracket,
            Token::Out,
            Token::Dollar,
            Token::Ident("i".to_string()),
        ]);
    }

    #[test]
    fn test_range() {
        assert_eq!(tokenize("%.. 1 10"), vec![
            Token::Range,
            Token::Int(1),
            Token::Int(10),
        ]);
    }

    // Arithmetic
    #[test]
    fn test_arithmetic() {
        assert_eq!(tokenize(".+ 1 2"), vec![Token::Add, Token::Int(1), Token::Int(2)]);
        assert_eq!(tokenize(".- 5 3"), vec![Token::Sub, Token::Int(5), Token::Int(3)]);
        assert_eq!(tokenize(".* 4 5"), vec![Token::Mul, Token::Int(4), Token::Int(5)]);
        assert_eq!(tokenize("./ 10 2"), vec![Token::Div, Token::Int(10), Token::Int(2)]);
        assert_eq!(tokenize(".% 10 3"), vec![Token::Mod, Token::Int(10), Token::Int(3)]);
        assert_eq!(tokenize(".^ 2 8"), vec![Token::Pow, Token::Int(2), Token::Int(8)]);
    }

    // Comparison
    #[test]
    fn test_comparison() {
        assert_eq!(tokenize(".= 1 1"), vec![Token::Eq, Token::Int(1), Token::Int(1)]);
        assert_eq!(tokenize(".~ 1 2"), vec![Token::Neq, Token::Int(1), Token::Int(2)]);
        assert_eq!(tokenize(".< 1 2"), vec![Token::Lt, Token::Int(1), Token::Int(2)]);
        assert_eq!(tokenize(".> 2 1"), vec![Token::Gt, Token::Int(2), Token::Int(1)]);
        assert_eq!(tokenize(".<= 1 1"), vec![Token::Lte, Token::Int(1), Token::Int(1)]);
        assert_eq!(tokenize(".>= 2 1"), vec![Token::Gte, Token::Int(2), Token::Int(1)]);
    }

    // Logical
    #[test]
    fn test_logical() {
        assert_eq!(tokenize(".& true false"), vec![Token::And, Token::True, Token::False]);
        assert_eq!(tokenize(".| true false"), vec![Token::Or, Token::True, Token::False]);
        assert_eq!(tokenize(".! true"), vec![Token::Not, Token::True]);
    }

    // Parallel
    #[test]
    fn test_parallel() {
        assert_eq!(tokenize("&% $xs [x] body"), vec![
            Token::ParEach,
            Token::Dollar, Token::Ident("xs".to_string()),
            Token::LBracket, Token::Ident("x".to_string()), Token::RBracket,
            Token::Ident("body".to_string()),
        ]);
        assert_eq!(tokenize("&!"), vec![Token::Background]);
        assert_eq!(tokenize("&="), vec![Token::ParJoin]);
        assert_eq!(tokenize("&?"), vec![Token::Race]);
    }

    // Command
    #[test]
    fn test_exec() {
        assert_eq!(tokenize("!! ls -la"), vec![
            Token::Exec,
            Token::Ident("ls".to_string()),
            Token::Ident("-la".to_string()),
        ]);
    }

    // Strings
    #[test]
    fn test_string() {
        assert_eq!(tokenize("\"hello world\""), vec![
            Token::String("hello world".to_string()),
        ]);
    }

    #[test]
    fn test_string_escape() {
        assert_eq!(tokenize("\"hello\\nworld\""), vec![
            Token::String("hello\nworld".to_string()),
        ]);
    }

    #[test]
    fn test_raw_string() {
        assert_eq!(tokenize("'hello'"), vec![
            Token::RawString("hello".to_string()),
        ]);
    }

    #[test]
    fn test_command_string() {
        assert_eq!(tokenize("`echo hello`"), vec![
            Token::CommandString("echo hello".to_string()),
        ]);
    }

    // Numbers
    #[test]
    fn test_int() {
        assert_eq!(tokenize("42"), vec![Token::Int(42)]);
        assert_eq!(tokenize("-42"), vec![Token::Int(-42)]);
        assert_eq!(tokenize("1_000_000"), vec![Token::Int(1000000)]);
    }

    #[test]
    fn test_float() {
        assert_eq!(tokenize("3.14"), vec![Token::Float(3.14)]);
        assert_eq!(tokenize("-2.5"), vec![Token::Float(-2.5)]);
        assert_eq!(tokenize("1e10"), vec![Token::Float(1e10)]);
        assert_eq!(tokenize("1.5e-3"), vec![Token::Float(1.5e-3)]);
    }

    #[test]
    fn test_hex_number() {
        assert_eq!(tokenize("0xFF"), vec![Token::Int(255)]);
        assert_eq!(tokenize("0x10"), vec![Token::Int(16)]);
    }

    #[test]
    fn test_binary_number() {
        assert_eq!(tokenize("0b1010"), vec![Token::Int(10)]);
        assert_eq!(tokenize("0b1111"), vec![Token::Int(15)]);
    }

    #[test]
    fn test_octal_number() {
        assert_eq!(tokenize("0o777"), vec![Token::Int(511)]);
        assert_eq!(tokenize("0o10"), vec![Token::Int(8)]);
    }

    // Identifiers
    #[test]
    fn test_identifier() {
        assert_eq!(tokenize("foo"), vec![Token::Ident("foo".to_string())]);
        assert_eq!(tokenize("foo-bar"), vec![Token::Ident("foo-bar".to_string())]);
        assert_eq!(tokenize("foo_bar"), vec![Token::Ident("foo_bar".to_string())]);
    }

    #[test]
    fn test_identifier_with_question() {
        assert_eq!(tokenize("some?"), vec![Token::Ident("some?".to_string())]);
        assert_eq!(tokenize("ok?"), vec![Token::Ident("ok?".to_string())]);
        assert_eq!(tokenize("none?"), vec![Token::Ident("none?".to_string())]);
    }

    #[test]
    fn test_identifier_with_bang() {
        assert_eq!(tokenize("unwrap!"), vec![Token::Ident("unwrap!".to_string())]);
    }

    // Keywords
    #[test]
    fn test_keywords() {
        assert_eq!(tokenize("true"), vec![Token::True]);
        assert_eq!(tokenize("false"), vec![Token::False]);
        assert_eq!(tokenize("none"), vec![Token::None]);
    }

    // Comments
    #[test]
    fn test_comment() {
        assert_eq!(tokenize("42 # this is a comment"), vec![Token::Int(42)]);
    }

    #[test]
    fn test_comment_line() {
        assert_eq!(tokenize("# comment\n42"), vec![Token::Int(42)]);
    }

    // List
    #[test]
    fn test_list() {
        assert_eq!(tokenize("[1 2 3]"), vec![
            Token::LBracket,
            Token::Int(1),
            Token::Int(2),
            Token::Int(3),
            Token::RBracket,
        ]);
    }

    // Record
    #[test]
    fn test_record() {
        assert_eq!(tokenize("{a: 1, b: 2}"), vec![
            Token::LBrace,
            Token::Ident("a".to_string()),
            Token::Colon,
            Token::Int(1),
            Token::Comma,
            Token::Ident("b".to_string()),
            Token::Colon,
            Token::Int(2),
            Token::RBrace,
        ]);
    }

    // Type Cast
    #[test]
    fn test_type_cast() {
        assert_eq!(tokenize(":> 42 @t"), vec![
            Token::TypeCast,
            Token::Int(42),
            Token::TypeLit("t".to_string()),
        ]);
    }

    // Type Check
    #[test]
    fn test_type_check() {
        assert_eq!(tokenize(":? $x @i"), vec![
            Token::TypeCheck,
            Token::Dollar, Token::Ident("x".to_string()),
            Token::TypeLit("i".to_string()),
        ]);
    }

    // Module
    #[test]
    fn test_lib_import() {
        assert_eq!(tokenize("/+ std"), vec![
            Token::LibImport,
            Token::Ident("std".to_string()),
        ]);
    }

    #[test]
    fn test_alias_import() {
        assert_eq!(tokenize("/= m std"), vec![
            Token::AliasImport,
            Token::Ident("m".to_string()),
            Token::Ident("std".to_string()),
        ]);
    }

    #[test]
    fn test_rel_import() {
        // /. starts relative import
        assert_eq!(tokenize("/."), vec![Token::RelImport]);
    }

    // Input
    #[test]
    fn test_stdin() {
        assert_eq!(tokenize("<~"), vec![Token::Stdin]);
    }

    #[test]
    fn test_file_read() {
        assert_eq!(tokenize("<+ \"file.txt\""), vec![
            Token::FileRead,
            Token::String("file.txt".to_string()),
        ]);
    }

    #[test]
    fn test_binary_read() {
        assert_eq!(tokenize("<* \"data.bin\""), vec![
            Token::BinaryRead,
            Token::String("data.bin".to_string()),
        ]);
    }

    // Output
    #[test]
    fn test_file_write() {
        assert_eq!(tokenize("~+ \"out.txt\" \"hello\""), vec![
            Token::FileWrite,
            Token::String("out.txt".to_string()),
            Token::String("hello".to_string()),
        ]);
    }

    #[test]
    fn test_file_append() {
        assert_eq!(tokenize("~^ \"log.txt\" \"line\""), vec![
            Token::FileAppend,
            Token::String("log.txt".to_string()),
            Token::String("line".to_string()),
        ]);
    }

    // Struct Definition
    #[test]
    fn test_struct_def() {
        assert_eq!(tokenize("^- Point"), vec![
            Token::StructDef,
            Token::Ident("Point".to_string()),
        ]);
    }

    // Channel
    #[test]
    fn test_channel_recv() {
        assert_eq!(tokenize("<- $ch"), vec![
            Token::ChanRecv,
            Token::Dollar, Token::Ident("ch".to_string()),
        ]);
    }

    // Option/Result
    #[test]
    fn test_option_functions() {
        // some?, none?, ok?, err? は識別子として認識される
        assert_eq!(tokenize("(some? $x)"), vec![
            Token::LParen,
            Token::Ident("some?".to_string()),
            Token::Dollar,
            Token::Ident("x".to_string()),
            Token::RParen,
        ]);
    }

    // Newline
    #[test]
    fn test_newline() {
        let tokens = tokenize_all("a\nb");
        assert_eq!(tokens, vec![
            Token::Ident("a".to_string()),
            Token::Newline,
            Token::Ident("b".to_string()),
            Token::Eof,
        ]);
    }

    // Data Operations (v0.2.2)
    #[test]
    fn test_pipeline() {
        assert_eq!(tokenize(":: $nums"), vec![
            Token::Pipeline,
            Token::Dollar,
            Token::Ident("nums".to_string()),
        ]);
    }

    #[test]
    fn test_var_apply() {
        assert_eq!(tokenize(":* (.+) 1 2 3"), vec![
            Token::VarApply,
            Token::LParen,
            Token::Add,
            Token::RParen,
            Token::Int(1),
            Token::Int(2),
            Token::Int(3),
        ]);
    }

    #[test]
    fn test_nest_access() {
        assert_eq!(tokenize(":. $user [name]"), vec![
            Token::NestAccess,
            Token::Dollar,
            Token::Ident("user".to_string()),
            Token::LBracket,
            Token::Ident("name".to_string()),
            Token::RBracket,
        ]);
    }

    #[test]
    fn test_terminator() {
        assert_eq!(tokenize("=="), vec![Token::Terminator]);
    }

    #[test]
    fn test_pipeline_with_terminator() {
        assert_eq!(tokenize(":: $x %> [n] $n =="), vec![
            Token::Pipeline,
            Token::Dollar,
            Token::Ident("x".to_string()),
            Token::Map,
            Token::LBracket,
            Token::Ident("n".to_string()),
            Token::RBracket,
            Token::Dollar,
            Token::Ident("n".to_string()),
            Token::Terminator,
        ]);
    }

    // v0.2.4: Reference Extensions
    #[test]
    fn test_global_ref() {
        assert_eq!(tokenize("$>config"), vec![
            Token::GlobalRef,
            Token::Ident("config".to_string()),
        ]);
    }

    #[test]
    fn test_parent_ref() {
        assert_eq!(tokenize("$<x"), vec![
            Token::ParentRef,
            Token::Ident("x".to_string()),
        ]);
    }

    #[test]
    fn test_args() {
        assert_eq!(tokenize("$@"), vec![Token::Args]);
    }

    #[test]
    fn test_arg_index() {
        assert_eq!(tokenize("$@.0"), vec![Token::ArgIndex(0)]);
        assert_eq!(tokenize("$@.5"), vec![Token::ArgIndex(5)]);
        assert_eq!(tokenize("$@.123"), vec![Token::ArgIndex(123)]);
    }

    #[test]
    fn test_arg_count() {
        assert_eq!(tokenize("$@#"), vec![Token::ArgCount]);
    }

    // String Interpolation
    #[test]
    fn test_plain_string() {
        assert_eq!(tokenize("\"hello\""), vec![Token::String("hello".to_string())]);
    }

    #[test]
    fn test_interp_var() {
        assert_eq!(tokenize("\"Hello, $name!\""), vec![
            Token::InterpString(vec![
                StringPart::Lit("Hello, ".to_string()),
                StringPart::Var("name".to_string()),
                StringPart::Lit("!".to_string()),
            ])
        ]);
    }

    #[test]
    fn test_interp_escape() {
        assert_eq!(tokenize("\"\\$100\""), vec![Token::String("$100".to_string())]);
    }

    #[test]
    fn test_interp_dollar_number() {
        // $1 should not be a variable (variable must start with letter/underscore)
        assert_eq!(tokenize("\"$100\""), vec![Token::String("$100".to_string())]);
    }
}