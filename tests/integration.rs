//! Integration tests for Hedgehog
//!
//! Runs all `.hog` files in the `examples/` directory.

use std::fs;
use std::path::Path;
use std::process::Command;

fn run_hog_file(path: &Path) -> (String, String, i32) {
    let output = Command::new("cargo")
        .args(["run", "--quiet", "--", path.to_str().unwrap()])
        .output()
        .expect("Failed to execute hog");

    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    let code = output.status.code().unwrap_or(-1);

    (stdout, stderr, code)
}

fn discover_hog_files(dir: &str) -> Vec<std::path::PathBuf> {
    let path = Path::new(dir);
    if !path.exists() {
        return vec![];
    }

    fs::read_dir(path)
        .unwrap()
        .filter_map(|entry| {
            let entry = entry.ok()?;
            let path = entry.path();
            if path.extension()?.to_str()? == "hog" {
                Some(path)
            } else {
                None
            }
        })
        .collect()
}

#[test]
fn test_examples_syntax() {
    let files = discover_hog_files("examples");

    assert!(!files.is_empty(), "No example files found in examples/");

    for file in &files {
        let filename = file.file_name().unwrap().to_str().unwrap();
        let content = fs::read_to_string(file).unwrap();

        let result = hedgehog::check_syntax(&content);
        assert!(
            result.is_ok(),
            "Syntax error in {}: {:?}",
            filename,
            result.err()
        );
    }
}

#[test]
fn test_examples_run() {
    let files = discover_hog_files("examples");

    for file in &files {
        let filename = file.file_name().unwrap().to_str().unwrap();
        let (stdout, stderr, code) = run_hog_file(file);

        assert_eq!(
            code, 0,
            "File {} failed with code {}.\nstdout: {}\nstderr: {}",
            filename, code, stdout, stderr
        );
    }
}

#[test]
fn test_hello() {
    let path = Path::new("examples/hello.hog");
    let (stdout, _stderr, code) = run_hog_file(path);

    assert_eq!(code, 0);
    assert!(stdout.contains("Hello, World!"));

    insta::assert_snapshot!("hello", stdout);
}

#[test]
fn test_fizzbuzz() {
    let path = Path::new("examples/fizzbuzz.hog");
    let (stdout, _stderr, code) = run_hog_file(path);

    assert_eq!(code, 0);
    assert!(stdout.contains("FizzBuzz"));

    insta::assert_snapshot!("fizzbuzz", stdout);
}

#[test]
fn test_basics() {
    let path = Path::new("examples/basics.hog");
    let (stdout, _stderr, code) = run_hog_file(path);

    assert_eq!(code, 0);

    insta::assert_snapshot!("basics", stdout);
}
