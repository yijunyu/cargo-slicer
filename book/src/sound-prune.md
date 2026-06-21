# Sound Source-Prune

`cargo-slicer sound-prune` produces a copy of your project with unused function
bodies replaced by `unimplemented!()`, so a plain `cargo build` of the copy skips
the work of compiling code your binary never calls — **without ever producing a
crashing binary**.

The "without crashing" part is the whole point. Earlier reachability-by-guess
approaches could mis-judge dynamic dispatch and stub a function that *was* reached,
yielding a binary that crashed at runtime. `sound-prune` instead uses the linker's
own reachability as ground truth.

## How it works

```
pass 1  cargo build (debug, no inlining)   →  an oracle binary
pass 2  nm -C <binary>                      →  the exact set of functions linked
pass 3  mirror the workspace, replace every unused fn body with unimplemented!()
```

A function is stubbed only if its symbol is **absent** from the linked binary.
Because the seed is link-truth, a wrong decision cannot miscompile: it can only
fail the build *loudly* with `unimplemented!()` at a named line.

## Usage

```bash
cargo-slicer sound-prune --output ../myproject-pruned
cd ../myproject-pruned
cargo build --release          # plain cargo; no slicer needed to build the copy
```

Options:

| Flag | Meaning |
|------|---------|
| `-o, --output DIR` | Output directory (default: `<project>-pruned`) |
| `--delete` | Remove private unused fns entirely (default: stub the body) |
| `--vendor` | **Experimental.** Also `cargo vendor` and prune dependency sources |

The default is **workspace-only** — it rewrites only the crates in your workspace,
never your dependencies. That keeps it simple and side-effect free. `--vendor`
reaches further but pulls in vendoring (checksums, build-dependency handling) and
is opt-in.

## Soundness guarantees

`sound-prune` never stubs:

- functions whose symbol is in the linked binary (i.e. anything actually used);
- trait-impl methods (`impl Trait for T`) — reached through dynamic/generic dispatch;
- functions in a crate that ships a `build.rs` (and, with `--vendor`, any transitive
  build-dependency — build scripts run code your *runtime* binary never links);
- files containing macro output the formatter cannot render.

The oracle build uses no inlining so that every used function keeps a real symbol;
an optimized build would inline callees away and make them look unused.

## When it helps

The speedup tracks how much un-called code sits on the compile critical path. CLI
tools with a focused dependency set see the clearest win (ripgrep: ~1.2–1.3× faster
clean build, identical behaviour). Generics-heavy projects see little from
source-pruning alone, because most of their dead codegen is in monomorphized generic
instances and trait-impl methods that source-pruning keeps for soundness — that case
is what the in-tree `-Zdead-fn-elimination` work targets instead.
