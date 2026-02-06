//! Hedgehog External Command Execution
//!
//! Handles execution of shell commands.

use std::process::{Command, Stdio};

/// Execute an external command and return its output
pub fn exec_command(args: &[&str]) -> Result<String, String> {
    if args.is_empty() {
        return Err("No command specified".to_string());
    }

    let cmd = args[0];
    let cmd_args = &args[1..];

    let output = Command::new(cmd)
        .args(cmd_args)
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
        .map_err(|e| format!("Failed to execute '{}': {}", cmd, e))?;

    if output.status.success() {
        Ok(String::from_utf8_lossy(&output.stdout).into_owned())
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        Err(format!("Command '{}' failed: {}", cmd, stderr.trim()))
    }
}

/// Execute a command with string arguments
#[allow(dead_code)]
pub fn exec_command_strings(args: &[String]) -> Result<String, String> {
    let refs: Vec<&str> = args.iter().map(|s| s.as_str()).collect();
    exec_command(&refs)
}

/// Execute a shell command string (via sh -c on Unix, cmd /c on Windows)
#[allow(dead_code)]
pub fn exec_shell(cmd: &str) -> Result<String, String> {
    #[cfg(windows)]
    {
        exec_command(&["cmd", "/c", cmd])
    }
    #[cfg(not(windows))]
    {
        exec_command(&["sh", "-c", cmd])
    }
}

/// Command execution result with stdout, stderr, and exit code
pub struct ExecResult {
    pub out: String,
    pub err: String,
    pub code: i32,
}

/// Execute a shell command and return full result (stdout, stderr, code)
pub fn exec_shell_full(cmd: &str) -> ExecResult {
    #[cfg(windows)]
    let args = ["cmd", "/c", cmd];
    #[cfg(not(windows))]
    let args = ["sh", "-c", cmd];

    match Command::new(args[0])
        .args(&args[1..])
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .output()
    {
        Ok(output) => ExecResult {
            out: String::from_utf8_lossy(&output.stdout).trim().to_string(),
            err: String::from_utf8_lossy(&output.stderr).trim().to_string(),
            code: output.status.code().unwrap_or(-1),
        },
        Err(e) => ExecResult {
            out: String::new(),
            err: format!("Failed to execute: {}", e),
            code: -1,
        },
    }
}

/// Execute a command and stream output in real-time
#[allow(dead_code)]
pub fn exec_command_streaming(args: &[&str]) -> Result<i32, String> {
    if args.is_empty() {
        return Err("No command specified".to_string());
    }

    let cmd = args[0];
    let cmd_args = &args[1..];

    let mut child = Command::new(cmd)
        .args(cmd_args)
        .stdout(Stdio::inherit())
        .stderr(Stdio::inherit())
        .spawn()
        .map_err(|e| format!("Failed to execute '{}': {}", cmd, e))?;

    let status = child
        .wait()
        .map_err(|e| format!("Failed to wait for '{}': {}", cmd, e))?;

    Ok(status.code().unwrap_or(-1))
}

/// Execute a command with input piped to stdin
#[allow(dead_code)]
pub fn exec_with_stdin(args: &[&str], input: &str) -> Result<String, String> {
    if args.is_empty() {
        return Err("No command specified".to_string());
    }

    let cmd = args[0];
    let cmd_args = &args[1..];

    use std::io::Write;

    let mut child = Command::new(cmd)
        .args(cmd_args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .map_err(|e| format!("Failed to execute '{}': {}", cmd, e))?;

    // Write input to stdin
    if let Some(mut stdin) = child.stdin.take() {
        stdin
            .write_all(input.as_bytes())
            .map_err(|e| format!("Failed to write to stdin: {}", e))?;
    }

    let output = child
        .wait_with_output()
        .map_err(|e| format!("Failed to read output: {}", e))?;

    if output.status.success() {
        Ok(String::from_utf8_lossy(&output.stdout).into_owned())
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr);
        Err(format!("Command '{}' failed: {}", cmd, stderr.trim()))
    }
}

/// Pipe output from one command to another
#[allow(dead_code)]
pub fn exec_pipe(cmd1: &[&str], cmd2: &[&str]) -> Result<String, String> {
    if cmd1.is_empty() || cmd2.is_empty() {
        return Err("No command specified".to_string());
    }

    // Execute first command
    let output1 = exec_command(cmd1)?;

    // Pipe to second command
    exec_with_stdin(cmd2, &output1)
}

/// Check if a command exists
#[allow(dead_code)]
pub fn command_exists(cmd: &str) -> bool {
    #[cfg(windows)]
    {
        Command::new("where")
            .arg(cmd)
            .output()
            .map(|o| o.status.success())
            .unwrap_or(false)
    }
    #[cfg(not(windows))]
    {
        Command::new("which")
            .arg(cmd)
            .output()
            .map(|o| o.status.success())
            .unwrap_or(false)
    }
}

/// Get environment variable
#[allow(dead_code)]
pub fn get_env(name: &str) -> Option<String> {
    std::env::var(name).ok()
}

/// Set environment variable
#[allow(dead_code)]
pub fn set_env(name: &str, value: &str) {
    std::env::set_var(name, value);
}

/// Expand glob pattern
#[allow(dead_code)]
pub fn glob_expand(pattern: &str) -> Result<Vec<String>, String> {
    let entries =
        glob::glob(pattern).map_err(|e| format!("Invalid glob pattern '{}': {}", pattern, e))?;

    let mut results = Vec::new();
    for entry in entries {
        match entry {
            Ok(path) => results.push(path.display().to_string()),
            Err(e) => return Err(format!("Glob error: {}", e)),
        }
    }

    Ok(results)
}

/// Read file contents
#[allow(dead_code)]
pub fn read_file(path: &str) -> Result<String, String> {
    std::fs::read_to_string(path).map_err(|e| format!("Failed to read '{}': {}", path, e))
}

/// Write file contents
#[allow(dead_code)]
pub fn write_file(path: &str, content: &str) -> Result<(), String> {
    std::fs::write(path, content).map_err(|e| format!("Failed to write '{}': {}", path, e))
}

/// Append to file
#[allow(dead_code)]
pub fn append_file(path: &str, content: &str) -> Result<(), String> {
    use std::fs::OpenOptions;
    use std::io::Write;

    let mut file = OpenOptions::new()
        .append(true)
        .create(true)
        .open(path)
        .map_err(|e| format!("Failed to open '{}': {}", path, e))?;

    file.write_all(content.as_bytes())
        .map_err(|e| format!("Failed to append to '{}': {}", path, e))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    // Unix only tests
    #[test]
    #[cfg(not(windows))]
    fn test_exec_echo() {
        let result = exec_command(&["echo", "hello"]);
        assert_eq!(result, Ok("hello\n".to_string()));
    }

    #[test]
    #[cfg(not(windows))]
    fn test_exec_shell() {
        let result = exec_shell("echo hello");
        assert_eq!(result, Ok("hello\n".to_string()));
    }

    #[test]
    #[cfg(not(windows))]
    fn test_command_exists() {
        assert!(command_exists("echo"));
        assert!(!command_exists("nonexistent_command_12345"));
    }

    #[test]
    #[cfg(not(windows))]
    fn test_exec_with_args() {
        let result = exec_command(&["echo", "-n", "test"]);
        assert_eq!(result, Ok("test".to_string()));
    }

    #[test]
    #[cfg(not(windows))]
    fn test_exec_pipe() {
        let result = exec_pipe(&["echo", "hello world"], &["wc", "-w"]);
        assert!(result.is_ok());
        let count = result.unwrap().trim().parse::<i32>().unwrap();
        assert_eq!(count, 2);
    }

    #[test]
    #[cfg(not(windows))]
    fn test_exec_with_stdin() {
        let result = exec_with_stdin(&["cat"], "hello");
        assert_eq!(result, Ok("hello".to_string()));
    }

    // Windows only tests
    #[test]
    #[cfg(windows)]
    fn test_exec_echo_windows() {
        let result = exec_command(&["cmd", "/c", "echo", "hello"]);
        assert!(result.is_ok());
        assert!(result.unwrap().contains("hello"));
    }

    #[test]
    #[cfg(windows)]
    fn test_exec_shell_windows() {
        let result = exec_shell("echo hello");
        assert!(result.is_ok());
        assert!(result.unwrap().contains("hello"));
    }

    #[test]
    #[cfg(windows)]
    fn test_command_exists_windows() {
        assert!(command_exists("cmd"));
        assert!(!command_exists("nonexistent_command_12345"));
    }

    // Cross-platform tests
    #[test]
    fn test_exec_nonexistent_command() {
        let result = exec_command(&["nonexistent_command_xyz"]);
        assert!(result.is_err());
    }

    #[test]
    fn test_exec_empty_args() {
        let result = exec_command(&[]);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "No command specified");
    }

    #[test]
    fn test_exec_command_strings() {
        let args = vec!["echo".to_string(), "test".to_string()];
        // Just check it doesn't panic, result depends on OS
        let _ = exec_command_strings(&args);
    }

    #[test]
    fn test_get_env() {
        std::env::set_var("HEDGEHOG_TEST_VAR", "test_value");
        assert_eq!(get_env("HEDGEHOG_TEST_VAR"), Some("test_value".to_string()));
        std::env::remove_var("HEDGEHOG_TEST_VAR");
    }

    #[test]
    fn test_get_env_nonexistent() {
        assert_eq!(get_env("HEDGEHOG_NONEXISTENT_VAR_12345"), None);
    }

    #[test]
    fn test_set_env() {
        set_env("HEDGEHOG_TEST_SET", "value123");
        assert_eq!(std::env::var("HEDGEHOG_TEST_SET").unwrap(), "value123");
        std::env::remove_var("HEDGEHOG_TEST_SET");
    }

    // File I/O tests
    #[test]
    fn test_write_and_read_file() {
        let mut test_path = std::env::temp_dir();
        test_path.push("hedgehog_test_file.txt");
        let test_file = test_path.to_str().unwrap();

        // Write
        let write_result = write_file(test_file, "hello world");
        assert!(write_result.is_ok(), "Write failed: {:?}", write_result);

        // Read
        let read_result = read_file(test_file);
        assert_eq!(read_result, Ok("hello world".to_string()));

        // Cleanup
        let _ = fs::remove_file(test_file);
    }

    #[test]
    fn test_append_file() {
        let mut test_path = std::env::temp_dir();
        test_path.push("hedgehog_test_append.txt");
        let test_file = test_path.to_str().unwrap();

        // Write initial content
        let _ = write_file(test_file, "line1\n");

        // Append
        let append_result = append_file(test_file, "line2\n");
        assert!(append_result.is_ok(), "Append failed: {:?}", append_result);

        // Read and verify
        let read_result = read_file(test_file);
        assert_eq!(read_result, Ok("line1\nline2\n".to_string()));

        // Cleanup
        let _ = fs::remove_file(test_file);
    }

    #[test]
    fn test_read_nonexistent_file() {
        let mut test_path = std::env::temp_dir();
        test_path.push("nonexistent_file_xyz_12345.txt");
        let result = read_file(test_path.to_str().unwrap());
        assert!(result.is_err());
    }

    // Glob tests
    #[test]
    fn test_glob_invalid_pattern() {
        let result = glob_expand("[invalid");
        assert!(result.is_err());
    }

    #[test]
    fn test_glob_no_match() {
        let mut test_path = std::env::temp_dir();
        test_path.push("hedgehog_nonexistent_pattern_*_xyz");
        let result = glob_expand(test_path.to_str().unwrap());
        assert!(result.is_ok());
        assert!(result.unwrap().is_empty());
    }
}
