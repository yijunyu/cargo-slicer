# Symbol Resolution Architecture

## Goal
Bridge the gap between SCIP semantic information and Rust compiler requirements for compilation.

## Core Principle
**Every symbol used in sliced code must be resolvable** - either defined locally, imported, or in prelude.

## Architecture Phases

```
Phase 1: EXTRACT        → Items to include (existing)
Phase 2: COLLECT        → All symbols referenced
Phase 3: RESOLVE        → Symbol → Source mapping
Phase 4: GENERATE       → Import statements
Phase 5: OUTPUT         → Final sliced code
```

## Key Data Structures

### SymbolTable
```rust
struct SymbolTable {
    symbols: HashMap<String, SymbolInfo>,
}

struct SymbolInfo {
    kind: SymbolKind,
    usages: Vec<UsageLocation>,
    definition: Option<DefinitionLocation>,
}

enum SymbolKind {
    Type,      // struct, enum, type alias
    Trait,     // trait definitions
    Function,  // fn, methods
    Constant,  // const, static
    Module,    // mod declarations
    Macro,     // macro_rules!, proc-macro
}
```

### SymbolResolver
```rust
struct SymbolResolver {
    local_definitions: HashSet<String>,      // Defined in current module
    crate_definitions: HashMap<String, String>, // symbol → crate::path
    std_types: HashMap<String, String>,      // symbol → std::path
    external_imports: HashMap<String, String>, // symbol → external::path
}

enum Resolution {
    Local,                    // No import needed
    Crate { path: String },   // use crate::path::Symbol
    Std { path: String },     // use std::path::Symbol
    External { path: String }, // use external::Symbol
    Unknown,                  // Conservative: try to import
}
```

## Monotonic Workflow

### Invariant
Each step must maintain: `errors_after <= errors_before`

### Testing Protocol
```bash
# Before any change
cargo build --release
./target/release/cargo-slicer --slice-all -o /tmp/baseline
cd /tmp/baseline && cargo check 2>&1 | grep "^error\[E" | wc -l
# Record: BASELINE_ERRORS = N

# After change
cargo build --release
./target/release/cargo-slicer --slice-all -o /tmp/test
cd /tmp/test && cargo check 2>&1 | grep "^error\[E" | wc -l
# Assert: TEST_ERRORS <= BASELINE_ERRORS
```

### Step Sequence

1. **Add SymbolTable** (additive, no behavior change)
2. **Collect symbols** (additive, populates table)
3. **Add SymbolResolver** (additive, no behavior change)
4. **Generate imports from resolver** (parallel path, compare results)
5. **Switch to new import generation** (only if errors decrease)
6. **Remove old heuristics** (only after validation)

## Migration Map

| Old Code | New Location | Priority |
|----------|--------------|----------|
| `generate_internal_imports` | `SymbolResolver::resolve` + `ImportGenerator` | High |
| `filter_preserved_imports` | `SymbolResolver::resolve_external` | High |
| `detect_and_generate_reexports` | `ImportGenerator::generate_reexports` | Medium |
| `apply_module_reexports` | `ImportGenerator::generate_reexports` | Medium |
| `get_crate_local_types_via_scip` | `SymbolTable::collect_definitions` | High |

## Success Metrics

- E0412 (cannot find type): Target 0
- E0433 (failed to resolve): Target 0
- E0405 (cannot find trait): Target 0
- E0425 (cannot find function): Target 0

## Baseline (2024-01-03)

Total errors: 624
- E0425 (cannot find function/value): 163
- E0433 (failed to resolve): 124
- E0405 (cannot find trait): 121
- E0412 (cannot find type): 83
- E0220 (associated type not found): 37
- E0422 (cannot find struct/variant): 33
- Others: 63

## Conservative Defaults

1. **When uncertain about symbol source**: Generate import anyway
2. **When multiple sources possible**: Prefer more specific (local > crate > std)
3. **Never filter imports**: Only add, conversion between sources is OK

## Implementation Status

### Phase 1: Foundation (Completed)
- Created `src/symbol.rs` module with:
  - `SymbolKind`, `SymbolInfo`, `UsageLocation`, `DefinitionLocation` types
  - `SymbolTable` for collecting symbols from code
  - `SymbolResolver` for resolving symbols to sources
  - `ImportGenerator` for generating import statements
  - `SymbolWorkflow` as unified API

### Phase 2: Integration (In Progress)
- Added workflow initialization in `slice_crate_semantic`
- Added parallel import generation path
- **Issue Found**: Conservative merge causing E0252 (duplicate imports)
  - When symbol system generates imports that overlap with old system
  - Merge function needs improvement to handle import variations

### Current Approach (Working)
- Using gap-filling strategy: symbol system adds only missing imports
- `fill_import_gaps()` function checks if symbol is already imported before adding
- Avoids duplicates by comparing symbol names, not full import paths

## Results (2026-01-03)

| Metric | Baseline | Previous | Current | Total Change |
|--------|----------|----------|---------|--------------|
| Total Errors | 624 | 177 | 163 | -74% |
| E0412 (type) | 83 | 21 | 17 | -80% |
| E0405 (trait) | 121 | 4 | 0 | -100% |
| E0425 (fn/val) | 163 | 33 | 28 | -83% |
| E0433 (resolve) | 124 | 30 | 21 | -83% |
| E0432 (import) | - | 38 | 19 | -50% |
| E0220 (assoc type) | - | 20 | 20 | - |
| E0053 (incompatible) | - | 0 | 12 | - |

### Key Fixes Applied
- **Inline module detection**: Skip `pub mod X;` declarations for modules defined inline
- **Duplicate module prevention**: Both lib.rs and mod.rs generation now detect inline modules
- **SCIP symbol_paths for import resolution**: Use cached SCIP data to resolve correct import paths
- **SCIP type imports fallback**: When type not in parsed index but known from SCIP, use SCIP path

### Latest Fix (2026-01-03): SCIP-Based Import Resolution

**Problem**: Import paths like `use crate::plumbing::Consumer` were incorrect because:
1. Regular resolution looks up paths in `all_paths` (extracted modules only)
2. If `iter::plumbing` wasn't extracted, resolution fails
3. Resulted in 15 E0432 errors for `crate::plumbing`

**Solution**: Use SCIP `symbol_paths` HashMap from cached analysis:
1. `analyze_crate_via_scip()` now loads from `~/.cache/cargo-slicer/scip/` cache
2. Extracts `symbol_paths: HashMap<String, Vec<String>>` mapping types to their module paths
3. When regular resolution fails, look up type in `symbol_paths` for correct path
4. E.g., `Consumer -> ["iter::plumbing"]` enables correct import generation

## Next Steps: Slicing Coverage Improvement

### Problem Analysis

Remaining 163 errors are primarily **slicing coverage issues** - items referenced by extracted code but not themselves extracted:

| Error | Count | Pattern | Root Cause |
|-------|-------|---------|------------|
| E0220 | 20 | `Result`, `Output` | Associated type bounds incomplete |
| E0432 | 19 | `crate::range::private` | Module not extracted |
| E0425 | 28 | `bridge_unindexed` | Helper function not extracted |
| E0412 | 17 | `PathBuf` | Missing std imports |
| E0053 | 12 | `method find` | Incompatible trait impl |

### Monotonic Improvement Strategy

**Key Principle**: Only ADD items to extraction, never REMOVE

#### Phase 1: Import-Driven Extraction (Safest)

After initial extraction, scan generated code for `use crate::*` imports.
If referenced item is missing from output but exists in index, add to extraction.

```rust
fn complete_imports(extracted: &BTreeMap<String, Vec<Item>>, index: &CrateIndex) -> HashSet<String> {
    let mut additional = HashSet::new();
    for (_, items) in extracted {
        for item in items {
            for import in extract_crate_imports(&item.source) {
                if !is_extracted(&import, extracted) && index.contains(&import) {
                    additional.insert(import);
                }
            }
        }
    }
    additional
}
```

**Why monotonic**: We only add items that are already referenced via imports.

#### Phase 2: Local Dependency Completion

For each extracted function, ensure local function calls are also extracted.

```rust
fn complete_local_deps(extracted: &HashSet<String>, index: &CrateIndex) -> HashSet<String> {
    let mut additional = HashSet::new();
    for name in extracted {
        for item in index.get_all(name) {
            for dep in &item.local_function_calls {  // NEW: track local calls
                if !extracted.contains(dep) && index.contains(dep) {
                    additional.insert(dep.clone());
                }
            }
        }
    }
    additional
}
```

**Why monotonic**: If A calls B and A is extracted, B must exist for A to compile.

#### Phase 3: Visibility Upgrade

Items used across module boundaries need appropriate visibility.

```rust
fn fix_visibility(item: &mut ParsedItem, usage_context: &str) {
    if item.module_path != usage_context && !item.is_pub {
        // Upgrade to pub(crate) - never restrict
        item.visibility = Visibility::PubCrate;
    }
}
```

**Why monotonic**: Only increasing visibility, never restricting.

### Implementation Order

1. **Add import scanning** to `expand_needed_transitively()` in slicing.rs
2. **Test on glob** (simplest crate) - verify error count decreases
3. **Test on --slice-all** - verify global error count decreases
4. **Add local call tracking** if Phase 1 shows improvement
5. **Add visibility fixing** for remaining edge cases

### Verification Protocol

```bash
# Before change
BASELINE=$(cargo +stable check --manifest-path /tmp/test_slice/Cargo.toml 2>&1 | grep "^error\[E" | wc -l)

# After change
CURRENT=$(cargo +stable check --manifest-path /tmp/test_slice/Cargo.toml 2>&1 | grep "^error\[E" | wc -l)

# Must satisfy
[ "$CURRENT" -le "$BASELINE" ] && echo "PASS: $BASELINE -> $CURRENT"
```

### Risk Mitigation

1. **Over-extraction**: Set maximum items per pass to avoid pulling entire crate
2. **Circular dependencies**: Track extraction path to detect cycles
3. **Feature-gated items**: Respect `#[cfg(...)]` attributes
4. **External dependencies**: Don't extract from non-local crates

## Implementation Log (2026-01-03)

### Attempted Fixes

1. **Import-driven extraction** (slicing.rs)
   - Added `extract_crate_imports()` to detect `use crate::*` in source
   - Integrated into `expand_needed_transitively()`
   - Result: No change (imports not present in original source)

2. **Type-usage extraction** (slicing.rs)
   - Added `extract_type_usages()` to detect bare type names
   - Checks if types exist in index before adding
   - Result: No change (types like `Consumer` not in parsed index)

3. **Import path validation** (semantic.rs)
   - Added check to verify first path component exists in `all_paths`
   - Skips imports for non-existent modules
   - Result: No change (imports coming from multiple sources)

### Root Cause Analysis

The remaining 177 workspace errors stem from:

1. **Incomplete module parsing**: `iter::plumbing` module not fully indexed
   - Types like `Consumer`, `Producer` aren't in the CrateIndex
   - SCIP analysis doesn't capture macro body dependencies

2. **Import path resolution mismatch**: Generated paths don't match actual structure
   - Detects `plumbing::Consumer` but can't resolve to `iter::plumbing::Consumer`
   - Pattern matching extracts leaf module names but loses parent context

3. **Multiple import generation paths**:
   - Aho-Corasick crate module matcher (lines 3675-3780)
   - Type scanning loop (lines 3783-3920)
   - Symbol workflow system (symbol.rs)
   - Each needs validation fixes

### Verified Behavior

- **All 7 individual crates compile**: No regression
- **Workspace errors are cross-crate**: From dependency references
- **Monotonic property maintained**: Error count didn't increase

### Next Steps (Prioritized)

1. **Improve module parsing coverage**: Parse ALL source files, not just used ones
2. **Fix all import generation paths**: Validate paths before generating
3. **Add SCIP macro body analysis**: Track dependencies in macro expansions
4. **Consider post-generation import cleanup**: Remove broken imports after generation
