# Speeding Up Rust Builds: Part II — The Gap

*Part II of III. [Part I: The Waiting Game](part1.md) | [Part III: Closing the Gap](part3.md)*

---

## Previously

In [Part I](part1.md), we tried every known trick to speed up building Zed — parallel frontend, fast linker, caching, alternative backends. Nothing made a dent on that 17-minute clean release build. We ended with a question: what if the problem isn't how the compiler works, but what we're asking it to compile?

Let's find out.

## A Library's Dilemma

Consider a library crate — say, `serde_json`. It exposes a rich API: `from_str()`, `from_slice()`, `from_reader()`, `to_string()`, `to_string_pretty()`, `to_vec()`, `to_writer()`, and dozens more.

Your project calls `serde_json::from_str()` and `serde_json::to_string()`. That's it. Two functions.

But when `rustc` compiles `serde_json`, it doesn't know you only need two functions. It can't. The crate boundary is opaque — rustc compiles each crate independently, treating every public function as a potential entry point. It must generate optimized machine code for *all* of them.

This isn't a bug. It's how separate compilation works. It's a fundamental architectural decision that enables crates to be compiled independently, cached, and reused. It's the right design.

But it has a cost.

## The Separate Compilation Gap

We call this cost the **separate compilation gap**: the difference between what the compiler must compile (everything visible) and what the program actually needs (everything reachable from `main`).

Formally, for a compilation unit *u*:

```
Gap(u) = (|Visible(u)| - |Reachable(u)|) / |Visible(u)|
```

Where:
- **Visible(u)** = all symbols the compiler processes (every public function, every impl, every trait method)
- **Reachable(u)** = the subset actually reachable from `main()` via whole-program call graph analysis

If Gap = 0%, the compiler is doing exactly the right amount of work. If Gap = 50%, half the compiler's effort is wasted.

## Measuring Zed's Gap

So what's Zed's gap?

We built a tool that does whole-program reachability analysis across all 198 workspace crates. Starting from `main()`, it traces every function call, every trait method invocation, every generic instantiation, and marks what's actually needed.

Then we count: how many CPU instructions does the compiler execute with everything vs. only the reachable code?

| | Total | Reachable | Gap |
|---|---|---|---|
| CPU instructions | 28,559 Ginstr | 18,067 Ginstr | **37%** |
| Functions analyzed | 32,579 | 23,095 | **29%** |

**37% of the CPU instructions the compiler executes when building Zed are spent compiling code that no one will ever call.**

Let that sink in. More than a third of the compiler's work is wasted. That's not a rounding error. That's not a micro-optimization waiting to happen. That's 10 *trillion* CPU instructions, burned for nothing, on every clean build.

And this isn't just Zed:

| Project | LOC | Instructions (Base) | Instructions (Reachable) | Gap |
|---------|-----|-------|-----------|------|
| **zed** | 500K | 28,559 Ginstr | 18,067 Ginstr | **37%** |
| **rustc** | 600K | 5,746 Ginstr | 4,268 Ginstr | **26%** |
| **zeroclaw** | 86K | 1,507 Ginstr | 1,314 Ginstr | **13%** |
| **helix** | 100K | 2,256 Ginstr | 2,004 Ginstr | **11%** |
| **ripgrep** | 50K | 314 Ginstr | 298 Ginstr | **5%** |
| **nushell** | 200K | 3,695 Ginstr | 3,682 Ginstr | **0.4%** |
| **bevy** | 300K | 3,807 Ginstr | 3,791 Ginstr | **0.4%** |

Some projects have tiny gaps. Bevy and nushell use almost everything they import — good for them. But Zed has a 37% gap, rustc has 26%, and even zeroclaw (a smaller project) wastes 13%.

## Why Some Gaps Are Bigger

The gap depends on how a project uses its dependencies.

**Large gap** projects like Zed have many library crates with broad APIs, but the binary only touches a fraction. Zed pulls in hundreds of crates for its editor, terminal, collaboration, and AI features. Each crate is compiled in full, even though Zed's binary only uses specific code paths.

**Small gap** projects like bevy use their dependencies more thoroughly. A game engine that imports a math library probably uses most of the math functions. There's less waste.

There's also an interesting amplification effect. In Rust, generics are monomorphized — each generic function gets compiled once per concrete type it's used with. When you stub an unreachable function, you also eliminate all its downstream monomorphizations. That's why Zed's *instruction* gap (37%) is larger than its *function* gap (29%) — each stubbed function cascades into many eliminated monomorphizations.

## The Honest Assessment

Here's the uncomfortable truth: **the Rust compiler isn't slow. It's doing too much work.**

And it's doing too much work because separate compilation — the very architecture that makes Cargo fast for incremental builds and enables the crates ecosystem — prevents the compiler from knowing what's actually needed.

Link-Time Optimization (LTO) can eliminate dead code *after* compilation, but it doesn't reduce the compilation phase itself. The work has already been done.

What we need is something that works *before* compilation. Something that tells the compiler, at the crate boundary, "here's exactly which public functions are actually called from downstream — you can skip the rest."

## So How Do We Close the Gap?

We know the gap exists. We can measure it precisely. For Zed, 37% of the compiler's work is provably unnecessary.

The question is: can we build a tool that, operating purely as a `RUSTC_WRAPPER` with no compiler modifications, identifies unreachable functions and eliminates them *before* LLVM codegen?

And can we do it without breaking anything?

*To be continued in [Part III: Closing the Gap](part3.md)...*

---

*This is Part II of a three-part series on cargo-slicer. [Part I](part1.md) set up the problem. [Part III](part3.md) provides the solution.*
