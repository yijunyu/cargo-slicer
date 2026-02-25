# Speeding Up Rust Builds: Part III — Closing the Gap

*Part III of III. [Part I: The Waiting Game](blog-part1-en.md) | [Part II: The Gap](blog-part2-en.md)*

---

## Previously

In [Part I](blog-part1-en.md), we saw that building Zed takes 17 minutes and no existing optimization really helps. In [Part II](blog-part2-en.md), we discovered why: 37% of the compiler's work is spent on unreachable code — the "separate compilation gap."

Now let's close it.

## The Approach: Four Steps

The idea is simple in principle: figure out what's reachable from `main()` across all crates, then tell the compiler to skip everything else. In practice, there are a few details to get right.

We call this approach **PRECC** (Predictive Precompilation Cutting), and it works in four phases:

### Step 1: Extract

Before compilation starts, we scan all workspace crate sources and build a unified cross-crate call graph. For Rust, we use a `syn`-based parser that extracts function definitions, call sites, and public API surfaces from every `.rs` file. This takes a few seconds, even for large projects.

```bash
cargo-slicer pre-analyze    # builds the cross-crate call graph
```

For Zed, this produces a graph covering all 198 workspace crates: which functions exist, which functions call which, and which are publicly exported.

### Step 2: Analyze

Starting from `main()`, we run a BFS (breadth-first search) through the call graph. Every function reachable from main is marked as "needed." Everything else is marked as "unreachable."

We're careful about special cases. `Drop` implementations? Always needed (the compiler inserts drop calls implicitly). Trait implementations? Always needed (dynamic dispatch via `dyn Trait` can call them). `#[no_mangle]` FFI functions? Always needed. Closures, async functions, unsafe functions? Always needed. We maintain 9 categories of exclusions to be safe.

The result: a precise set of functions that can be safely eliminated from each crate.

### Step 3: Predict

Here's where it gets interesting. Naively, you'd think "just cut everything unreachable." But our analysis has overhead — loading the driver, traversing MIR, doing cache I/O. For crates with very few unreachable functions, this overhead exceeds the savings.

We learned this the hard way. Applying cutting to every crate in bevy *slows the build by 4.4%*. The gap is only 0.4%, and the analysis overhead eats the tiny savings.

So for each crate, we predict: will cutting save more time than the analysis costs? If yes, cut. If no, skip — compile it normally.

Our baseline heuristic is simple:
- If the predicted number of stubbable functions is 0: skip.
- If it's less than 5 AND the stub ratio is under 2%: skip.
- Otherwise: cut.

This gets us 92-100% precision on projects that benefit, and correctly skips projects where cutting would hurt.

### Step 4: Cut

For crates marked "cut," we intercept the compiler. Operating as a `RUSTC_WRAPPER`, we hook into rustc after type checking and replace unreachable function bodies with MIR-level abort stubs. The function signature remains (so downstream crates can still reference it), but the body is replaced with a single `abort()` instruction.

When rustc's monomorphization collector encounters a stubbed function, it finds no callees — no downstream functions, no generic instantiations, nothing to compile. The entire subtree of the mono graph is pruned. LLVM never sees it.

No source code is modified. No Cargo.toml changes. No feature flags. The compiler simply does less work.

```bash
# The full command
cargo-slicer pre-analyze
CARGO_SLICER_VIRTUAL=1 CARGO_SLICER_CODEGEN_FILTER=1 \
  RUSTC_WRAPPER=$(which cargo_slicer_dispatch) \
  cargo +nightly build --release
```

Or, even simpler:

```bash
cargo-slicer.sh /path/to/your/project
```

## The Results

So how much faster is Zed?

| Project | Baseline | With PRECC | Wall-clock | Instructions | Peak Memory |
|---------|----------|------------|------------|--------------|-------------|
| **zed** | 1,012s | **719s** | **-29%** | -37% | -45% |
| **rustc** | 135.8s | **112.4s** | **-17%** | -26% | -7% |
| **zeroclaw** | 192.9s | **170.4s** | **-12%** | -13% | -11% |
| **helix** | 71.2s | **66.6s** | **-6%** | -11% | -18% |
| **ripgrep** | 11.1s | **10.7s** | **-4%** | -5% | -6% |
| nushell | 106.5s | 108.9s | +2.3% | -0.4% | — |
| bevy | 81.8s | 85.4s | +4.4% | -0.4% | — |

Zed's build drops from **17 minutes to 12 minutes**. That's 5 minutes saved on every clean build. 45% less memory. 37% fewer CPU instructions.

The Rust compiler itself builds 17% faster. Helix, 6%. Ripgrep, 4%.

And look at the last two rows. Nushell and bevy have tiny gaps (0.4%), so the prediction step correctly identifies them as not worth cutting. Without prediction, bevy would be 4.4% slower — the overhead exceeds the savings. With prediction, we avoid that regression entirely.

## The Honest Part

Let me be upfront about what this tool *doesn't* do:

- **Incremental builds**: cargo-slicer targets fresh/clean builds. For incremental `cargo check` and small changes, rustc's built-in incremental compilation is already fast. We're solving the CI/fresh-build problem.
- **Small projects**: if your project is 5,000 lines with 3 dependencies, the gap is tiny and the overhead isn't worth it. This tool shines on larger codebases (50K+ LOC).
- **Correctness guarantee**: we replace function bodies with abort stubs. If our reachability analysis is wrong and a "stubbed" function gets called at runtime, the program will abort. In practice, our 9 safety exclusion categories prevent this — we've tested on all benchmark projects — but it's worth knowing.
- **Nightly only**: the MIR-level hooks require unstable rustc APIs, so a nightly toolchain is required.

## Try It

Install with one command:

```bash
curl -fsSL https://raw.githubusercontent.com/yijunyu/cargo-slicer/main/install.sh | bash
```

Then build any Rust project:

```bash
cargo-slicer.sh /path/to/your/project
```

That's it. No config files, no source changes, no Cargo.toml edits. Point it at any Rust project with a `Cargo.toml` and see what happens.

## What We'd Love to Hear

We're researchers, not fortune tellers. The benchmark numbers above are from our test machine (48-core, 128 GB RAM, Linux). Your mileage will vary depending on your project's dependency structure, your hardware, and the phase of the moon.

**We genuinely want to know how this works on your project.** Does it speed things up? Does it break something? Is the gap large or small? Every data point helps us improve.

Reach out:

- **GitHub Issues**: [github.com/yijunyu/cargo-slicer/issues](https://github.com/yijunyu/cargo-slicer/issues) — bug reports, benchmark results, feature requests
- **Email**: yijun.yu@open.ac.uk — for detailed results, collaboration, or just to say hello

We're particularly interested in projects with 10+ workspace crates and heavy dependency usage — that's where the gap tends to be largest.

## The Bigger Picture

The separate compilation gap isn't unique to Rust. We've also applied the same principle to C projects — splitting SQLite's monolithic 256K-line `sqlite3.c` into 2,503 independent compilation units, achieving a 5.8x speedup via parallelism.

The gap is a property of *separate compilation itself*, not of any particular language or compiler. Wherever a compiler processes code in isolation without knowing what's actually needed, there's potential waste.

And wherever there's waste, there's opportunity.

---

*This concludes our three-part series on speeding up Rust builds with cargo-slicer.*

*Thanks for reading. Now go build something — a little faster.*

---

**Links:**
- [GitHub: cargo-slicer](https://github.com/yijunyu/cargo-slicer)
- [Part I: The Waiting Game](blog-part1-en.md)
- [Part II: The Gap](blog-part2-en.md)
- [Full documentation](USAGE.md)
