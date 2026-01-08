use std::collections::HashMap;
use std::path::Path;
use tokei::{Config, Languages, LanguageType};
use crate::types::{CrateMeasurement, DepGraph};

/// Measure LOC for a directory using tokei
pub fn measure_loc(path: &Path) -> usize {
    if !path.exists() {
        return 0;
    }
    let mut languages = Languages::new();
    let config = Config::default();
    
    // We only care about Rust code
    languages.get_statistics(&[path], &["target", ".git", "tests", "examples", "benches"], &config);
    
    if let Some(rust) = languages.get(&LanguageType::Rust) {
        rust.code
    } else {
        0
    }
}

/// Print measurement report in a format similar to cargo-geiger
pub fn print_measurement_report(
    graph: &DepGraph,
    measurements: &HashMap<String, CrateMeasurement>,
    use_unicode: bool,
) {
    println!("\n=== LOC Reduction Report ===");
    println!("{:>10} {:>10} {:>10}   {:<40}", "Before", "After", "Reduction", "Crate");
    println!("{:-<10} {:-<10} {:-<10}   {:-<40}", "", "", "", "");

    let root = &graph.root;
    print_crate_row(graph, root, measurements, 0, &[], true, use_unicode);
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

            // Use emojis if unicode is enabled
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

            println!("{:>10} {:>10} {:>10} {} {:<40}", m.before_loc, m.after_loc, reduction, emoji, display_name);
        } else {
            println!("{:>10} {:>10} {:>10}   {:<40}", "-", "-", "-", display_name);
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

#[cfg(test)]
mod tests {
    use super::*;

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
