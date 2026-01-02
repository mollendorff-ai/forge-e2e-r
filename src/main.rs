//! forge-e2e-r: CLI entry point.
//!
//! Validates forge analytics against R.

use std::fs;
use std::path::PathBuf;
use std::time::Instant;

use clap::Parser;
use colored::Colorize;

use forge_e2e_r::cli_runner::find_forge_binary;
use forge_e2e_r::r_validator::{check_r_available, RConfig};
use forge_e2e_r::types::{load_analytics_tests, TestResult};

#[derive(Parser)]
#[command(name = "forge-e2e-r")]
#[command(about = "E2E validation of forge analytics against R")]
#[command(version)]
struct Cli {
    /// Run all tests (headless mode with colored output).
    #[arg(long)]
    all: bool,

    /// Path to test specs directory.
    #[arg(short, long, default_value = "tests/analytics")]
    tests: PathBuf,

    /// Path to forge binary (or set `FORGE_BIN` env var).
    #[arg(short, long)]
    binary: Option<PathBuf>,

    /// Path to R validators directory.
    #[arg(long, default_value = "validators/r")]
    validators: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let cli = Cli::parse();

    // Find forge binary
    let forge_binary = cli
        .binary
        .or_else(find_forge_binary)
        .ok_or_else(|| {
            anyhow::anyhow!(
                "Forge binary not found. Set FORGE_BIN or use --binary"
            )
        })?;

    if !forge_binary.exists() {
        anyhow::bail!("Forge binary not found: {}", forge_binary.display());
    }

    // Check R availability
    let r_config = RConfig {
        validators_dir: cli.validators.clone(),
        ..Default::default()
    };

    let r_version = check_r_available(&r_config).map_err(|_| {
        anyhow::anyhow!(
            "R (Rscript) not found. Install with:\n  macOS: brew install r\n  Ubuntu: apt install r-base"
        )
    })?;

    println!("{}", "forge-e2e-r".bold());
    println!("  Forge: {}", forge_binary.display());
    println!("  R: {r_version}");
    println!("  Tests: {}", cli.tests.display());
    println!("  Validators: {}", cli.validators.display());
    println!();

    // Load tests
    let tests = load_tests(&cli.tests)?;
    println!("Loaded {} tests", tests.len());
    println!();

    if cli.all {
        run_all_mode(&tests, &forge_binary, &r_config)?;
    } else {
        println!("Use --all to run all tests");
    }

    Ok(())
}

fn load_tests(tests_dir: &PathBuf) -> anyhow::Result<Vec<forge_e2e_r::types::AnalyticsTestSpec>> {
    let mut all_tests = Vec::new();

    if !tests_dir.exists() {
        anyhow::bail!("Tests directory not found: {}", tests_dir.display());
    }

    for entry in fs::read_dir(tests_dir)? {
        let entry = entry?;
        let path = entry.path();

        if path.extension().is_some_and(|e| e == "yaml") {
            let content = fs::read_to_string(&path)?;
            match load_analytics_tests(&content) {
                Ok(tests) => all_tests.extend(tests),
                Err(e) => {
                    eprintln!("Warning: Failed to parse {}: {e}", path.display());
                }
            }
        }
    }

    Ok(all_tests)
}

#[allow(clippy::unnecessary_wraps)] // Result for consistent main() error handling
fn run_all_mode(
    tests: &[forge_e2e_r::types::AnalyticsTestSpec],
    _forge_binary: &PathBuf,
    _r_config: &RConfig,
) -> anyhow::Result<()> {
    let start = Instant::now();
    let mut results = Vec::new();

    println!("{}", "Running tests...".cyan());

    for test in tests {
        // TODO: Implement actual test execution
        // For now, mark as skipped since we need the full test infrastructure
        let result = TestResult::Skip {
            name: test.name.clone(),
            reason: "R validation not yet implemented in split".to_string(),
        };
        print_result(&result);
        results.push(result);
    }

    let elapsed = start.elapsed();

    // Summary
    println!();
    println!("{}", "═".repeat(60));

    let passed = results.iter().filter(|r| r.is_pass()).count();
    let failed = results.iter().filter(|r| r.is_fail()).count();
    let skipped = results.iter().filter(|r| matches!(r, TestResult::Skip { .. })).count();

    if failed == 0 {
        println!(
            "  {} {} passed, {} skipped in {:.2}s",
            "✓".green(),
            passed.to_string().green(),
            skipped,
            elapsed.as_secs_f64()
        );
    } else {
        println!(
            "  {} {} passed, {} failed, {} skipped in {:.2}s",
            "✗".red(),
            passed,
            failed.to_string().red(),
            skipped,
            elapsed.as_secs_f64()
        );
    }

    println!("{}", "═".repeat(60));

    if failed > 0 {
        std::process::exit(1);
    }

    Ok(())
}

fn print_result(result: &TestResult) {
    match result {
        TestResult::Pass { name, .. } => {
            println!("  {} {}", "✓".green(), name);
        }
        TestResult::Fail { name, reason } => {
            println!("  {} {}", "✗".red(), name.red());
            println!("      {reason}");
        }
        TestResult::Error { name, error } => {
            println!("  {} {} (error)", "✗".red(), name.red());
            println!("      {error}");
        }
        TestResult::Skip { name, reason } => {
            println!("  {} {} ({})", "○".yellow(), name.dimmed(), reason.dimmed());
        }
    }
}
