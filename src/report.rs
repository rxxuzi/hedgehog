//! Error Reporting
//!
//! Rust-style error display with colors and source context.

use colored::*;

/// Error severity level
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Level {
    Error,
    Warning,
    Help,
}

/// A diagnostic message with source location
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub level: Level,
    pub code: Option<String>,
    pub message: String,
    pub file: String,
    pub line: usize,
    pub column: usize,
    pub length: usize,
    pub source_line: Option<String>,
    pub help: Option<String>,
}

impl Diagnostic {
    pub fn error(message: impl Into<String>) -> Self {
        Self {
            level: Level::Error,
            code: None,
            message: message.into(),
            file: String::new(),
            line: 0,
            column: 0,
            length: 1,
            source_line: None,
            help: None,
        }
    }

    pub fn with_code(mut self, code: impl Into<String>) -> Self {
        self.code = Some(code.into());
        self
    }

    pub fn at(mut self, file: impl Into<String>, line: usize, column: usize) -> Self {
        self.file = file.into();
        self.line = line;
        self.column = column;
        self
    }

    pub fn with_length(mut self, length: usize) -> Self {
        self.length = length;
        self
    }

    pub fn with_source(mut self, source: impl Into<String>) -> Self {
        self.source_line = Some(source.into());
        self
    }

    pub fn with_help(mut self, help: impl Into<String>) -> Self {
        self.help = Some(help.into());
        self
    }

    /// Format the diagnostic for display
    pub fn format(&self) -> String {
        let mut output = String::new();

        // Header: error[E0001]: message
        let level_str = match self.level {
            Level::Error => "error".red().bold(),
            Level::Warning => "warning".yellow().bold(),
            Level::Help => "help".cyan().bold(),
        };

        if let Some(code) = &self.code {
            output.push_str(&format!(
                "{}[{}]: {}\n",
                level_str,
                code.bold(),
                self.message.bold()
            ));
        } else {
            output.push_str(&format!("{}: {}\n", level_str, self.message.bold()));
        }

        // Location: --> file:line:column
        if !self.file.is_empty() {
            let arrow = "-->".blue().bold();
            output.push_str(&format!(
                " {} {}:{}:{}\n",
                arrow, self.file, self.line, self.column
            ));
        }

        // Source context
        if let Some(source) = &self.source_line {
            let line_num = format!("{}", self.line);
            let padding = " ".repeat(line_num.len());
            let pipe = "|".blue().bold();

            // Empty line with pipe
            output.push_str(&format!("{} {}\n", padding, pipe));

            // Source line
            output.push_str(&format!("{} {} {}\n", line_num.blue().bold(), pipe, source));

            // Underline
            let spaces = " ".repeat(self.column.saturating_sub(1));
            let underline = "^".repeat(self.length).red().bold();
            output.push_str(&format!("{} {} {}{}\n", padding, pipe, spaces, underline));
        }

        // Help message
        if let Some(help) = &self.help {
            output.push_str(&format!("{}: {}\n", "help".cyan().bold(), help));
        }

        output
    }

    /// Print the diagnostic to stderr
    pub fn emit(&self) {
        eprint!("{}", self.format());
    }
}

/// Error codes
pub mod codes {
    // Syntax errors (E01xx)
    pub const UNEXPECTED_TOKEN: &str = "E0101";
    pub const UNCLOSED_PAREN: &str = "E0102";
    pub const UNCLOSED_BRACKET: &str = "E0103";
    pub const UNCLOSED_BRACE: &str = "E0104";
    pub const UNCLOSED_STRING: &str = "E0105";
    pub const INVALID_NUMBER: &str = "E0106";

    // Name errors (E02xx)
    pub const UNDEFINED_VARIABLE: &str = "E0201";
    pub const UNDEFINED_FUNCTION: &str = "E0202";

    // Type errors (E03xx)
    pub const TYPE_MISMATCH: &str = "E0301";
    pub const INVALID_OPERAND: &str = "E0302";
    pub const ARITY_MISMATCH: &str = "E0303";

    // Runtime errors (E04xx)
    pub const DIVISION_BY_ZERO: &str = "E0401";
    pub const INDEX_OUT_OF_BOUNDS: &str = "E0402";
    pub const CHANNEL_CLOSED: &str = "E0403";
}

/// Helper to extract a line from source code
pub fn get_source_line(source: &str, line: usize) -> Option<String> {
    source
        .lines()
        .nth(line.saturating_sub(1))
        .map(|s| s.to_string())
}

/// Calculate the display length of text (for underlines)
pub fn display_len(text: &str) -> usize {
    text.chars().count()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_diagnostic_format() {
        let diag = Diagnostic::error("undefined variable")
            .with_code(codes::UNDEFINED_VARIABLE)
            .at("test.hog", 5, 10)
            .with_length(4)
            .with_source("~> $foo")
            .with_help("did you mean `$food`?");

        let output = diag.format();
        assert!(output.contains("undefined variable"));
        assert!(output.contains("E0201"));
        assert!(output.contains("test.hog:5:10"));
    }

    #[test]
    fn test_get_source_line() {
        let source = "line 1\nline 2\nline 3";
        assert_eq!(get_source_line(source, 1), Some("line 1".to_string()));
        assert_eq!(get_source_line(source, 2), Some("line 2".to_string()));
        assert_eq!(get_source_line(source, 3), Some("line 3".to_string()));
        assert_eq!(get_source_line(source, 4), None);
    }
}
