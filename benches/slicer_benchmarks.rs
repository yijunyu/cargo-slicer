//! Criterion benchmarks for cargo-slicer
//!
//! Run with: cargo bench
//! View HTML report: target/criterion/report/index.html

use criterion::{black_box, criterion_group, criterion_main, Criterion, BenchmarkId};
use std::collections::HashSet;
use std::path::PathBuf;

// Import slicer library
use cargo_slicer::slicer::features::SlicerFeatures;
use cargo_slicer::slicer::cache::SlicerCache;

/// Benchmark content hash generation for cache keys
fn bench_content_hash(c: &mut Criterion) {
    let mut group = c.benchmark_group("content_hash");

    let features = SlicerFeatures::new();

    // Test with different sizes of used_items sets
    for size in [10, 100, 1000, 5000] {
        let used_items: HashSet<String> = (0..size)
            .map(|i| format!("item_name_{}", i))
            .collect();

        group.bench_with_input(
            BenchmarkId::from_parameter(size),
            &used_items,
            |b, items| {
                b.iter(|| {
                    SlicerCache::content_hash(black_box(items), black_box(&features))
                });
            },
        );
    }

    group.finish();
}

/// Benchmark cache key generation
fn bench_cache_key(c: &mut Criterion) {
    c.bench_function("cache_key_generation", |b| {
        b.iter(|| {
            SlicerCache::cache_key(
                black_box("some-crate-name"),
                black_box("1.2.3"),
            )
        });
    });
}

/// Benchmark SlicerFeatures creation at different optimization levels
fn bench_features_from_level(c: &mut Criterion) {
    let mut group = c.benchmark_group("features_from_level");

    for level in [0, 1, 2, 3, 4] {
        group.bench_with_input(
            BenchmarkId::from_parameter(level),
            &level,
            |b, &level| {
                b.iter(|| {
                    SlicerFeatures::from_level(black_box(level))
                });
            },
        );
    }

    group.finish();
}

/// Benchmark feature flag parsing
fn bench_feature_parsing(c: &mut Criterion) {
    let mut group = c.benchmark_group("feature_parsing");

    let test_cases = vec![
        ("private-fn", "single flag"),
        ("private-fn,private-const,private-type", "multiple flags"),
        ("private-fn,cfg-eval,arch-filter,auto-fixes,verify", "many flags"),
    ];

    for (flags, name) in test_cases {
        group.bench_with_input(
            BenchmarkId::new("parse", name),
            &flags,
            |b, flags| {
                b.iter(|| {
                    let mut features = SlicerFeatures::new();
                    for flag in flags.split(',') {
                        let _ = features.enable(black_box(flag));
                    }
                    features
                });
            },
        );
    }

    group.finish();
}

/// Benchmark HashSet operations (common in marking phase)
fn bench_hashset_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("hashset_operations");

    // Pre-populate a large set
    let large_set: HashSet<String> = (0..10000)
        .map(|i| format!("module::submodule::item_{}", i))
        .collect();

    // Benchmark contains check (very common in marker)
    group.bench_function("contains_existing", |b| {
        b.iter(|| {
            large_set.contains(black_box("module::submodule::item_5000"))
        });
    });

    group.bench_function("contains_missing", |b| {
        b.iter(|| {
            large_set.contains(black_box("module::submodule::item_99999"))
        });
    });

    // Benchmark insert
    group.bench_function("insert_new", |b| {
        b.iter_batched(
            || large_set.clone(),
            |mut set| {
                set.insert(black_box("new_item".to_string()));
                set
            },
            criterion::BatchSize::SmallInput,
        );
    });

    group.finish();
}

/// Benchmark string operations common in the slicer
fn bench_string_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("string_operations");

    // Module path construction (common in graph building)
    group.bench_function("module_path_join", |b| {
        let parts = vec!["crate", "module", "submodule", "item"];
        b.iter(|| {
            parts.iter()
                .map(|s| *s)
                .collect::<Vec<_>>()
                .join("::")
        });
    });

    // Check if identifier is a Rust keyword (common in deleter)
    let keywords: HashSet<&str> = [
        "as", "break", "const", "continue", "crate", "else", "enum", "extern",
        "false", "fn", "for", "if", "impl", "in", "let", "loop", "match", "mod",
        "move", "mut", "pub", "ref", "return", "self", "Self", "static", "struct",
        "super", "trait", "true", "type", "unsafe", "use", "where", "while",
    ].iter().cloned().collect();

    group.bench_function("keyword_check", |b| {
        b.iter(|| {
            keywords.contains(black_box("struct"))
        });
    });

    group.finish();
}

/// Benchmark path operations
fn bench_path_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("path_operations");

    let base = PathBuf::from("/home/user/project/crates_to_slice");

    group.bench_function("path_join", |b| {
        b.iter(|| {
            base.join(black_box("some-crate-1.0.0")).join("src").join("lib.rs")
        });
    });

    group.bench_function("path_display", |b| {
        let path = base.join("some-crate-1.0.0").join("src").join("lib.rs");
        b.iter(|| {
            format!("{}", black_box(&path).display())
        });
    });

    group.finish();
}

/// Benchmark ctags-based parsing vs syn-based parsing
fn bench_parsing_methods(c: &mut Criterion) {
    use std::fs;
    use std::path::Path;
    use cargo_slicer::slicer::dependency_graph::build_graph_full;
    use cargo_slicer::ctags_rs::parse_rust_file;

    // Create a temporary test crate with substantial code
    let test_crate_dir = PathBuf::from("/tmp/bench_test_crate");
    if !test_crate_dir.exists() {
        fs::create_dir_all(test_crate_dir.join("src")).unwrap();

        // Write a moderately complex Rust file with multiple items
        let test_code = r#"
pub struct Point {
    pub x: f64,
    pub y: f64,
}

impl Point {
    pub fn new(x: f64, y: f64) -> Self {
        Point { x, y }
    }

    pub fn distance(&self, other: &Point) -> f64 {
        let dx = self.x - other.x;
        let dy = self.y - other.y;
        (dx * dx + dy * dy).sqrt()
    }

    pub fn midpoint(&self, other: &Point) -> Point {
        Point {
            x: (self.x + other.x) / 2.0,
            y: (self.y + other.y) / 2.0,
        }
    }
}

pub enum Shape {
    Circle { radius: f64 },
    Rectangle { width: f64, height: f64 },
    Triangle { a: f64, b: f64, c: f64 },
}

impl Shape {
    pub fn area(&self) -> f64 {
        match self {
            Shape::Circle { radius } => std::f64::consts::PI * radius * radius,
            Shape::Rectangle { width, height } => width * height,
            Shape::Triangle { a, b, c } => {
                let s = (a + b + c) / 2.0;
                (s * (s - a) * (s - b) * (s - c)).sqrt()
            }
        }
    }
}

pub fn add(a: i32, b: i32) -> i32 {
    a + b
}

pub fn subtract(a: i32, b: i32) -> i32 {
    a - b
}

pub fn multiply(a: i32, b: i32) -> i32 {
    a * b
}

const MAX_VALUE: i32 = 100;
const MIN_VALUE: i32 = -100;

pub type Result<T> = std::result::Result<T, String>;
pub type Point2D = Point;
"#;
        fs::write(test_crate_dir.join("src/lib.rs"), test_code).unwrap();

        // Write Cargo.toml
        let cargo_toml = r#"[package]
name = "bench_test_crate"
version = "0.1.0"
edition = "2021"
"#;
        fs::write(test_crate_dir.join("Cargo.toml"), cargo_toml).unwrap();
    }

    let mut group = c.benchmark_group("parsing_methods");
    let test_file = test_crate_dir.join("src/lib.rs");

    // Benchmark syn-based parsing (current default - full AST)
    group.bench_function("syn_full_ast", |b| {
        b.iter(|| {
            let content = fs::read_to_string(black_box(&test_file)).unwrap();
            let _ = syn::parse_file(black_box(&content));
        });
    });

    // Benchmark pure-Rust ctags parser (lightweight tag extraction)
    group.bench_function("pure_rust_ctags", |b| {
        b.iter(|| {
            let _ = parse_rust_file(black_box(test_file.as_path()));
        });
    });

    // Also benchmark full graph building (with dependency analysis)
    group.bench_function("syn_with_graph_building", |b| {
        b.iter(|| {
            let _ = build_graph_full(
                black_box(test_crate_dir.as_path()),
                black_box("bench_test_crate")
            );
        });
    });

    group.finish();
}

criterion_group!(
    benches,
    bench_content_hash,
    bench_cache_key,
    bench_features_from_level,
    bench_feature_parsing,
    bench_hashset_operations,
    bench_string_operations,
    bench_path_operations,
    bench_parsing_methods,
);

criterion_main!(benches);
