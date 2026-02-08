use std::collections::HashMap;
use std::path::Path;
use std::process::Command;
#[cfg(feature = "loc-measurement")]
use tokei::{Config, Languages, LanguageType};
use crate::types::{CrateMeasurement, DepGraph, CompileStatus};

/// Print system and compiler information
pub fn print_system_info() {
    println!("\n=== System & Compiler Info ===");

    // Rust compiler version
    if let Ok(output) = Command::new("rustc").arg("--version").output() {
        let version = String::from_utf8_lossy(&output.stdout).trim().to_string();
        let channel = if version.contains("nightly") {
            "nightly"
        } else if version.contains("beta") {
            "beta"
        } else {
            "stable"
        };
        println!("  Rust: {} ({})", version, channel);
    }

    // Target triple
    if let Ok(output) = Command::new("rustc").arg("-vV").output() {
        let info = String::from_utf8_lossy(&output.stdout);
        for line in info.lines() {
            if let Some(host) = line.strip_prefix("host: ") {
                println!("  Target: {}", host.trim());
            }
        }
    }

    // OS and kernel
    if let Ok(output) = Command::new("uname").arg("-srm").output() {
        let uname = String::from_utf8_lossy(&output.stdout).trim().to_string();
        println!("  OS: {}", uname);
    }

    // CPU info (Linux)
    if let Ok(cpuinfo) = std::fs::read_to_string("/proc/cpuinfo") {
        let mut model_name = None;
        let mut cpu_mhz = None;
        let mut core_count = 0usize;
        for line in cpuinfo.lines() {
            if line.starts_with("model name") && model_name.is_none() {
                model_name = line.split(':').nth(1).map(|s| s.trim().to_string());
            }
            if line.starts_with("cpu MHz") && cpu_mhz.is_none() {
                cpu_mhz = line.split(':').nth(1).map(|s| s.trim().to_string());
            }
            if line.starts_with("processor") {
                core_count += 1;
            }
        }
        if let Some(model) = model_name {
            let freq = cpu_mhz
                .and_then(|m| m.parse::<f64>().ok())
                .map(|f| format!(" @ {:.0} MHz", f))
                .unwrap_or_default();
            println!("  CPU: {} ({} cores{})", model, core_count, freq);
        }
    }

    // System load (Linux)
    if let Ok(loadavg) = std::fs::read_to_string("/proc/loadavg") {
        let parts: Vec<&str> = loadavg.split_whitespace().collect();
        if parts.len() >= 3 {
            println!("  Load: {} {} {} (1/5/15 min)", parts[0], parts[1], parts[2]);
        }
    }
}

/// Measure LOC for a directory using tokei (when feature enabled)
#[cfg(feature = "loc-measurement")]
pub fn measure_loc(path: &Path) -> usize {
    if !path.exists() {
        return 0;
    }
    let mut languages = Languages::new();
    let config = Config::default();

    // We only care about Rust code
    languages.get_statistics(&[path], &["target", ".git", "tests", "examples", "benches"], &config);

    if let Some(rust) = languages.get(&LanguageType::Rust) {
        // Use total lines (code + comments + blanks) for accurate comparison
        // since prettyplease reformatting affects code vs comment line counts
        rust.lines()
    } else {
        0
    }
}

/// Measure LOC for a directory using simple line counting (fallback when tokei disabled)
/// This counts non-empty, non-comment lines in .rs files
#[cfg(not(feature = "loc-measurement"))]
pub fn measure_loc(path: &Path) -> usize {
    if !path.exists() {
        return 0;
    }
    count_rust_loc_simple(path)
}

/// Measure total bytes for all Rust files in a directory
pub fn measure_bytes(path: &Path) -> usize {
    if !path.exists() {
        return 0;
    }
    count_rust_bytes(path)
}

/// Count total file size in bytes for all Rust files
fn count_rust_bytes(dir: &Path) -> usize {
    use std::fs;

    let mut total = 0;

    let walker = match fs::read_dir(dir) {
        Ok(w) => w,
        Err(_) => return 0,
    };

    for entry in walker.flatten() {
        let path = entry.path();

        // Skip excluded directories
        if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
            if name == "target" || name == ".git" || name == "tests"
               || name == "examples" || name == "benches" || name.starts_with('.') {
                continue;
            }
        }

        if path.is_dir() {
            total += count_rust_bytes(&path);
        } else if path.extension().and_then(|s| s.to_str()) == Some("rs") {
            if let Ok(metadata) = fs::metadata(&path) {
                total += metadata.len() as usize;
            }
        }
    }

    total
}

/// Simple line counter for Rust files (used when tokei is disabled)
#[cfg(not(feature = "loc-measurement"))]
fn count_rust_loc_simple(dir: &Path) -> usize {
    use std::fs;

    let mut total = 0;

    let walker = match fs::read_dir(dir) {
        Ok(w) => w,
        Err(_) => return 0,
    };

    for entry in walker.flatten() {
        let path = entry.path();

        // Skip excluded directories
        if let Some(name) = path.file_name().and_then(|n| n.to_str()) {
            if name == "target" || name == ".git" || name == "tests"
               || name == "examples" || name == "benches" || name.starts_with('.') {
                continue;
            }
        }

        if path.is_dir() {
            total += count_rust_loc_simple(&path);
        } else if path.extension().map(|e| e == "rs").unwrap_or(false) {
            total += count_file_loc(&path);
        }
    }

    total
}

/// Count lines of code in a single file (excludes blank lines and comments)
#[cfg(not(feature = "loc-measurement"))]
fn count_file_loc(path: &Path) -> usize {
    use std::fs;

    let content = match fs::read_to_string(path) {
        Ok(c) => c,
        Err(_) => return 0,
    };

    let mut count = 0;
    let mut in_block_comment = false;

    for line in content.lines() {
        let trimmed = line.trim();

        // Handle block comments
        if in_block_comment {
            if trimmed.contains("*/") {
                in_block_comment = false;
            }
            continue;
        }

        if trimmed.starts_with("/*") {
            in_block_comment = true;
            if trimmed.contains("*/") {
                in_block_comment = false;
            }
            continue;
        }

        // Skip empty lines and line comments
        if trimmed.is_empty() || trimmed.starts_with("//") {
            continue;
        }

        count += 1;
    }

    count
}

/// Check if a crate compiles and extract error information
/// Returns (status, total_errors, error_type_counts)
pub fn check_crate_compiles(crate_path: &Path) -> (CompileStatus, usize, HashMap<String, usize>) {
    if !crate_path.exists() || !crate_path.join("Cargo.toml").exists() {
        return (CompileStatus::NotTested, 0, HashMap::new());
    }

    // Run cargo check with JSON output
    let output = match Command::new("cargo")
        .arg("check")
        .arg("--message-format=json")
        .arg("--quiet")
        .current_dir(crate_path)
        .output()
    {
        Ok(o) => o,
        Err(_) => return (CompileStatus::NotTested, 0, HashMap::new()),
    };

    if output.status.success() {
        return (CompileStatus::Success, 0, HashMap::new());
    }

    // Parse JSON output to extract error codes
    let mut error_types = HashMap::new();
    let mut total_errors = 0;

    for line in String::from_utf8_lossy(&output.stdout).lines() {
        // cargo check --message-format=json outputs to stdout, not stderr
        if let Ok(msg) = serde_json::from_str::<serde_json::Value>(line) {
            if msg["reason"] == "compiler-message" {
                if let Some(code_obj) = msg["message"]["code"].as_object() {
                    if let Some(code) = code_obj.get("code").and_then(|c| c.as_str()) {
                        *error_types.entry(code.to_string()).or_insert(0) += 1;
                        total_errors += 1;
                    }
                }
            }
        }
    }

    // If we got no structured errors but the command failed, it's still a failure
    let status = if total_errors > 0 {
        CompileStatus::Failed
    } else {
        // Check stderr for errors
        if String::from_utf8_lossy(&output.stderr).contains("error") {
            CompileStatus::Failed
        } else {
            CompileStatus::Success
        }
    };

    (status, total_errors, error_types)
}

/// Print measurement report in a flat format (safer than tree for large graphs)
pub fn print_measurement_report(
    _graph: &DepGraph,
    measurements: &HashMap<String, CrateMeasurement>,
    use_unicode: bool,
    output_base: Option<&Path>,
    all_deps_loc: usize,
    all_deps_count: usize,
) {
    print_system_info();

    println!("\n=== LOC Reduction & Compilation Report ===");
    println!("{:>10} {:>10} {:>10} {:>8} {:>8}   {:<40}",
             "Before", "After", "Reduction", "Status", "Errors", "Crate");
    println!("{:-<10} {:-<10} {:-<10} {:-<8} {:-<8}   {:-<40}",
             "", "", "", "", "", "");

    // Print in flat format (safer for large dependency graphs)
    // Sort by crate name for consistent output
    let mut crate_names: Vec<_> = measurements.keys().cloned().collect();
    crate_names.sort();

    let mut skipped_count = 0;
    let mut total_before: usize = 0;
    let mut total_after: usize = 0;
    let mut total_crate_errors: usize = 0;
    let mut total_failed: usize = 0;

    for crate_name in crate_names {
        if let Some(m) = measurements.get(&crate_name) {
            // Accumulate totals for ALL crates (before skip check)
            total_before += m.before_loc;
            total_after += m.after_loc;
            total_crate_errors += m.error_count;
            if m.compile_status == CompileStatus::Failed {
                total_failed += 1;
            }

            // Calculate reduction percentage
            let reduction_pct = if m.before_loc > 0 {
                (m.before_loc as f64 - m.after_loc as f64) / m.before_loc as f64 * 100.0
            } else {
                0.0
            };

            // Skip crates with 0% reduction (no change)
            if reduction_pct.abs() < 0.05 {
                skipped_count += 1;
                continue;
            }

            let reduction = format!("{:.1}%", reduction_pct);

            let status_icon = match m.compile_status {
                CompileStatus::Success => if use_unicode { "    ✅ " } else { "OK  " },
                CompileStatus::Failed => if use_unicode { "    ❌ " } else { "FAIL" },
                CompileStatus::NotTested => if use_unicode { "    ⏭️  " } else { "SKIP" },
            };

            let error_display = if m.error_count > 0 {
                format!("{}", m.error_count)
            } else {
                " -".to_string()
            };

            println!("{:>10} {:>10} {:>10} {:>8} {:>8}   {:<40}",
                     m.before_loc, m.after_loc, reduction, status_icon, error_display, crate_name);
        }
    }

    if skipped_count > 0 {
        println!("\n  ({} crates with 0% change not shown)", skipped_count);
    }

    // Print total summary row
    println!("\n{:-<10} {:-<10} {:-<10} {:-<8} {:-<8}   {:-<40}",
             "", "", "", "", "", "");
    let total_reduction = if total_before > 0 {
        format!("{:.1}%", (total_before as f64 - total_after as f64) / total_before as f64 * 100.0)
    } else {
        "0.0%".to_string()
    };
    let total_status = if total_failed > 0 {
        if use_unicode { "    ❌ " } else { "FAIL" }
    } else {
        if use_unicode { "    ✅ " } else { "OK  " }
    };
    let total_error_display = if total_crate_errors > 0 {
        format!("{}", total_crate_errors)
    } else {
        " -".to_string()
    };
    let dir_label = output_base
        .and_then(|p| p.file_name())
        .and_then(|n| n.to_str())
        .unwrap_or("sliced");
    println!("{:>10} {:>10} {:>10} {:>8} {:>8}   {:<40}",
             total_before, total_after, total_reduction,
             total_status, total_error_display,
             format!("TOTAL ({} crates in {}/)", measurements.len(), dir_label));

    // Print "ORIGINAL" row showing all deps LOC (including non-sliceable crates)
    if all_deps_loc > 0 && all_deps_count > measurements.len() {
        let orig_reduction = format!("{:.1}%",
            (all_deps_loc as f64 - total_after as f64) / all_deps_loc as f64 * 100.0);
        println!("{:>10} {:>10} {:>10} {:>8} {:>8}   {:<40}",
                 all_deps_loc, total_after, orig_reduction,
                 "", "",
                 format!("ORIGINAL ({} crates in dependency tree)", all_deps_count));
    }

    // Print error breakdown summary
    print_error_summary(measurements, use_unicode);
}

fn print_crate_row(
    graph: &DepGraph,
    crate_name: &str,
    measurements: &HashMap<String, CrateMeasurement>,
    depth: usize,
    prefix: &[bool],
    is_last: bool,
    use_unicode: bool,
) {
    // Prevent stack overflow on deep dependency trees
    const MAX_DEPTH: usize = 20;
    if depth > MAX_DEPTH {
        return;  // Don't recurse beyond max depth
    }
    if crate_name != graph.root {
        let mut indent = String::new();
        for (i, &p) in prefix.iter().enumerate() {
            if i < prefix.len() - 1 {
                if p {
                    indent.push_str(if use_unicode { "│   " } else { "|   " });
                } else {
                    indent.push_str("    ");
                }
            }
        }

        if depth > 0 {
            if is_last {
                indent.push_str(if use_unicode { "└── " } else { "`-- " });
            } else {
                indent.push_str(if use_unicode { "├── " } else { "|-- " });
            }
        }

        let display_name = if crate_name.len() + indent.len() > 40 {
            let max_len = 40_usize.saturating_sub(indent.len()).saturating_sub(3);
            format!("{}{}", indent, &crate_name[..max_len]) + "..."
        } else {
            format!("{}{}", indent, crate_name)
        };

        if let Some(m) = measurements.get(crate_name) {
            let reduction = if m.before_loc > 0 {
                let red = (m.before_loc as f64 - m.after_loc as f64) / m.before_loc as f64 * 100.0;
                format!("{:.1}%", red)
            } else {
                "0.0%".to_string()
            };

            // Status icon based on compilation result
            let status_icon = match m.compile_status {
                CompileStatus::Success => if use_unicode { "    ✅ " } else { "OK  " },
                CompileStatus::Failed => if use_unicode { "    ❌ " } else { "FAIL" },
                CompileStatus::NotTested => if use_unicode { "    ⏭️ " } else { "SKIP" },
            };

            let error_display = if m.error_count > 0 {
                format!("{}", m.error_count)
            } else {
                " -".to_string()
            };

            // Use emojis for LOC reduction if unicode is enabled
            let emoji = if use_unicode {
                if m.before_loc > 0 && m.after_loc < m.before_loc / 10 {
                    "🚀 "
                } else if m.after_loc < m.before_loc / 2 {
                    "✅ "
                } else {
                    "📦 "
                }
            } else {
                "  "
            };

            println!("{:>10} {:>10} {:>10} {:>8} {:>8} {} {:<40}",
                     m.before_loc, m.after_loc, reduction, status_icon, error_display, emoji, display_name);
        } else {
            println!("{:>10} {:>10} {:>10} {:>8} {:>8}   {:<40}",
                     "-", "-", "-", "-", "-", display_name);
        }
    }

    if let Some(node) = graph.nodes.get(crate_name) {
        let deps = &node.deps;
        for (i, dep) in deps.iter().enumerate() {
            let mut new_prefix = prefix.to_vec();
            new_prefix.push(i < deps.len() - 1);
            print_crate_row(graph, dep, measurements, depth + 1, &new_prefix, i == deps.len() - 1, use_unicode);
        }
    }
}

/// Print error summary showing top error codes and statistics
fn print_error_summary(measurements: &HashMap<String, CrateMeasurement>, use_unicode: bool) {
    // Aggregate all error types
    let mut all_errors: HashMap<String, usize> = HashMap::new();
    let mut total_errors = 0;
    let mut success_count = 0;
    let mut failed_count = 0;
    let mut not_tested_count = 0;

    for m in measurements.values() {
        match m.compile_status {
            CompileStatus::Success => success_count += 1,
            CompileStatus::Failed => failed_count += 1,
            CompileStatus::NotTested => not_tested_count += 1,
        }

        total_errors += m.error_count;
        for (code, count) in &m.error_types {
            *all_errors.entry(code.clone()).or_insert(0) += count;
        }
    }

    let total_tested = success_count + failed_count;
    let success_rate = if total_tested > 0 {
        (success_count as f64 / total_tested as f64) * 100.0
    } else {
        0.0
    };

    println!("\n=== Compilation Summary ===");
    if use_unicode {
        println!("  ✅ Success: {}/{} ({:.1}%)", success_count, total_tested, success_rate);
        println!("  ❌ Failed:  {}/{}", failed_count, total_tested);
    } else {
        println!("  Success: {}/{} ({:.1}%)", success_count, total_tested, success_rate);
        println!("  Failed:  {}/{}", failed_count, total_tested);
    }
    if not_tested_count > 0 {
        println!("  Not Tested: {}", not_tested_count);
    }
    println!("  Total Errors: {}", total_errors);

    if !all_errors.is_empty() {
        println!("\n=== Top Error Codes ===");
        let mut sorted_errors: Vec<_> = all_errors.iter().collect();
        sorted_errors.sort_by(|a, b| b.1.cmp(a.1));  // Sort by count descending

        for (code, count) in sorted_errors.iter().take(15) {
            println!("  {:>5}  {}", count, code);
        }
    }
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_display_name_no_panic() {
        // This used to panic
        let crate_name = "unicode-ident";
        let indent = " ".repeat(40);
        let _display_name = if crate_name.len() + indent.len() > 40 {
            let max_len = 40_usize.saturating_sub(indent.len()).saturating_sub(3);
            format!("{}{}", indent, &crate_name[..max_len]) + "..."
        } else {
            format!("{}{}", indent, crate_name)
        };
    }
}
