//! Architecture detection and filtering.

use std::collections::HashSet;
use std::path::Path;

use crate::constants::RUST_KEYWORDS;

/// Get the target architecture for filtering architecture-specific code
pub fn get_target_arch() -> &'static str {
    #[cfg(target_arch = "x86_64")]
    { "x86_64" }
    #[cfg(target_arch = "x86")]
    { "x86" }
    #[cfg(target_arch = "aarch64")]
    { "aarch64" }
    #[cfg(target_arch = "arm")]
    { "arm" }
    #[cfg(target_arch = "wasm32")]
    { "wasm32" }
    #[cfg(target_arch = "wasm64")]
    { "wasm64" }
    #[cfg(target_arch = "riscv64")]
    { "riscv64" }
    #[cfg(target_arch = "riscv32")]
    { "riscv32" }
    #[cfg(not(any(
        target_arch = "x86_64",
        target_arch = "x86",
        target_arch = "aarch64",
        target_arch = "arm",
        target_arch = "wasm32",
        target_arch = "wasm64",
        target_arch = "riscv64",
        target_arch = "riscv32"
    )))]
    { "unknown" }
}

/// Check if a file path is for a non-matching architecture and should be skipped
pub fn should_skip_arch_file(file_path: &Path) -> bool {
    let target_arch = get_target_arch();
    let path_str = file_path.to_string_lossy();

    let arch_dirs = [
        ("x86_64", &["x86_64", "x86-64"][..]),
        ("x86", &["x86", "i686", "i386"][..]),
        ("aarch64", &["aarch64", "arm64"][..]),
        ("arm", &["arm", "armv7"][..]),
        ("wasm32", &["wasm32", "wasm"][..]),
        ("wasm64", &["wasm64"][..]),
        ("riscv64", &["riscv64", "riscv"][..]),
        ("riscv32", &["riscv32"][..]),
        ("powerpc64", &["powerpc64", "ppc64"][..]),
        ("powerpc", &["powerpc", "ppc"][..]),
        ("s390x", &["s390x"][..]),
        ("sparc64", &["sparc64"][..]),
        ("sparc", &["sparc"][..]),
        ("mips64", &["mips64"][..]),
        ("mips", &["mips"][..]),
        ("loongarch64", &["loongarch64"][..]),
        ("csky", &["csky"][..]),
        ("x32", &["x32"][..]),
    ];

    for (arch, patterns) in &arch_dirs {
        for pattern in *patterns {
            // Check for /arch/{pattern}/ pattern (e.g., /arch/x86_64/)
            let arch_pattern = format!("/arch/{}/", pattern);

            // Check for /arch nested with pattern (e.g., /arch/generic/x86_64/)
            let nested_arch_pattern = format!("/arch/{}/", pattern);

            // Check for top-level architecture directories (e.g., /src/x86_64/ in linux-raw-sys)
            let toplevel_pattern = format!("/src/{}/", pattern);

            // Check for architecture-specific subdirectories (e.g., /x86_64/)
            let direct_pattern = format!("/{}/", pattern);

            if path_str.contains(&arch_pattern) ||
               path_str.contains(&nested_arch_pattern) ||
               path_str.contains(&toplevel_pattern) {
                // These patterns are architecture-specific, skip if not matching
                if *arch != target_arch {
                    return true;
                }
            } else if path_str.contains(&direct_pattern) {
                // For bare /{pattern}/ directories, check if they're in architecture-specific contexts
                // This catches cases like /arch/x86_64/ or /platform/aarch64/
                if (path_str.contains("/arch/") ||
                    path_str.contains("/platform/") ||
                    path_str.contains("/sys/")) && *arch != target_arch {
                    return true;
                }
            }
        }
    }

    // Check for arch_ prefixed patterns (e.g., arch_x86_64)
    for (arch, patterns) in &arch_dirs {
        for pattern in *patterns {
            let cfg_pattern = format!("arch_{}", pattern);
            if path_str.contains(&cfg_pattern) && *arch != target_arch {
                return true;
            }
        }
    }

    false
}

/// Check if a module name is architecture-specific and should be skipped
pub fn should_skip_arch_module(module_name: &str) -> bool {
    let target_arch = get_target_arch();

    let arch_modules = [
        ("x86_64", &["x86_64", "x86-64"][..]),
        ("x86", &["x86", "i686", "i386"][..]),
        ("aarch64", &["aarch64", "arm64"][..]),
        ("arm", &["arm", "armv7"][..]),
        ("wasm32", &["wasm32", "wasm"][..]),
        ("wasm64", &["wasm64"][..]),
        ("riscv64", &["riscv64", "riscv"][..]),
        ("riscv32", &["riscv32"][..]),
        ("powerpc64", &["powerpc64", "ppc64"][..]),
        ("powerpc", &["powerpc", "ppc"][..]),
        ("s390x", &["s390x"][..]),
        ("sparc64", &["sparc64"][..]),
        ("sparc", &["sparc"][..]),
        ("mips64", &["mips64"][..]),
        ("mips", &["mips"][..]),
        ("mips64r6", &["mips64r6"][..]),
        ("mips32r6", &["mips32r6"][..]),
        ("loongarch64", &["loongarch64"][..]),
        ("csky", &["csky"][..]),
        ("x32", &["x32"][..]),
    ];

    for (arch, patterns) in &arch_modules {
        for pattern in *patterns {
            if module_name == *pattern {
                if *arch != target_arch {
                    return true;
                }
            }
        }
    }

    false
}

/// Rewrite relative module paths to absolute paths to avoid shadowing issues
pub fn rewrite_relative_module_paths(content: &str, current_module: &str, all_modules: &HashSet<String>) -> String {
    let mut result = content.to_string();

    let mut all_paths_with_intermediates: HashSet<String> = all_modules.iter().cloned().collect();
    for module_path in all_modules {
        let parts: Vec<&str> = module_path.split("::").collect();
        for i in 1..parts.len() {
            let intermediate = parts[..i].join("::");
            all_paths_with_intermediates.insert(intermediate);
        }
    }

    let mut module_name_to_paths: std::collections::HashMap<String, Vec<String>> = std::collections::HashMap::new();
    for module_path in &all_paths_with_intermediates {
        if let Some(last_part) = module_path.split("::").last() {
            module_name_to_paths.entry(last_part.to_string()).or_default().push(module_path.clone());
        }
    }

    for (module_name, paths) in &module_name_to_paths {
        if ["crate", "self", "super", "std", "core", "alloc", "Self"].contains(&module_name.as_str()) {
            continue;
        }

        let candidates: Vec<&String> = paths.iter()
            .filter(|p| *p != current_module)
            .collect();

        if candidates.is_empty() {
            continue;
        }

        let best_path = if candidates.len() == 1 {
            candidates[0]
        } else {
            let current_parts: Vec<&str> = current_module.split("::").collect();
            candidates.iter()
                .max_by_key(|path| {
                    let path_parts: Vec<&str> = path.split("::").collect();
                    let depth = path_parts.len();
                    let is_generic = path.contains("::generic");
                    let common_prefix = path_parts.iter().zip(current_parts.iter())
                        .take_while(|(a, b)| a == b)
                        .count();

                    (if is_generic { 1000 } else { 0 }) - (depth as i32 * 10) + (common_prefix as i32)
                })
                .unwrap_or(&candidates[0])
        };

        let pattern = format!("{}::", module_name);
        let replacement = format!("crate::{}::", best_path);

        if result.contains(&pattern) {
            let mut new_result = String::with_capacity(result.len());
            let mut last_end = 0;

            for (start, _) in result.match_indices(&pattern) {
                let before = &result[..start];
                let is_already_absolute =
                    before.ends_with("crate::") ||
                    before.ends_with("super::") ||
                    before.ends_with("self::") ||
                    before.ends_with("::");

                if is_already_absolute {
                    new_result.push_str(&result[last_end..start + pattern.len()]);
                } else {
                    new_result.push_str(&result[last_end..start]);
                    new_result.push_str(&replacement);
                }
                last_end = start + pattern.len();
            }
            new_result.push_str(&result[last_end..]);
            result = new_result;
        }
    }

    result
}

/// Escape a module/identifier name if it's a reserved keyword
pub fn escape_keyword(name: &str) -> String {
    if RUST_KEYWORDS.contains(&name) {
        format!("r#{}", name)
    } else {
        name.to_string()
    }
}
