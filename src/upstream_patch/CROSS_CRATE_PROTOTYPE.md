# Cross-crate feedback edge — prototype, not the merge proposal

This branch (`dfe-cross-crate-reference`) adds `dead_fn_elim_xcrate.rs`: a working
reference implementation of the **cross-crate feedback edge** for
`-Z dead-fn-elimination` — the dashed arrow in the design dataflow diagram, where a
binary's reachability result lets *library* crates drop their dead functions.

## Status: it works, and it is deliberately NOT the upstream pitch

Verified end-to-end against a stage1 rustc built from the patch (rust @ 1.93):

- **Single-pass binary kernel** (no env var, one `cargo build`): a binary with an
  unused fn emits
  `note: 1 unreachable functions excluded from codegen by -Z dead-fn-elimination`.
  This is the mergeable proposal.
- **Cross-crate sidecar** (two passes): the binary pass walks extern-crate MIR and
  writes a 48-byte sidecar naming exactly the reached upstream fns (`b_used`, not
  `b_dead_*`). A library pass reads it back keyed by `DefPathHash`.

Both halves compile and run. The data flow is real.

## Why it is a prototype and not the merge proposal

The sidecar needs **two `cargo build`s** with a `RUSTFLAGS` nonce between them to
force cargo to recompile the (otherwise unchanged) libraries. That reintroduces
exactly the out-of-band orchestration that going in-tree was meant to remove: a
transparent `-Z` flag should be one build, not two-plus-an-env-var. Reviewers will
(correctly) ask why a `-Z` flag needs a second pass.

It does NOT make the cross-crate elimination transparent; it makes it *possible
without a cargo patch*. Those are different claims.

Separately: any patched rustc — sidecar or not — only helps people running our
build, so it must be rebuilt on every nightly bump until merged upstream. The
sidecar neither adds to nor removes that cost. The entire value of going in-tree is
**getting merged**, and the two-pass design weakens that pitch.

## The honest conclusion for reviewers

- **Ship the single-process kernel** (`dead_fn_elim.rs`, binary crates only) as the
  upstream `-Z dead-fn-elimination` proposal. It is transparent, one build, sound.
- **Cross-crate library elimination has no transparent path without cargo**
  deferring library codegen until after the binary's analysis. That is a Cargo-team
  decision, not a rustc patch.
- This `dead_fn_elim_xcrate.rs` is kept as proof the feedback edge is implementable
  and as the basis for a future cargo-orchestrated version — not as something to
  merge as-is.

## Files

- `dead_fn_elim.rs` — the single-process kernel (unchanged design; this branch also
  carries rustc-1.93 API-drift fixes: `trait_item_def_id()`, `symbol_name`).
- `dead_fn_elim_xcrate.rs` — the sidecar prototype (binary writes, library reads).
- `dead_fn_elim.rs::run_analysis` — gains a library path, gated entirely on the
  `DFE_SIDECAR` env var so the single-pass binary behaviour is byte-for-byte
  unchanged when the var is unset.

## Reproduce

```
# in a rust checkout @ ~1.93
cp dead_fn_elim*.rs compiler/rustc_mir_transform/src/
git apply rustc_other_files.patch
# register both modules in compiler/rustc_mir_transform/src/lib.rs:
#   pub mod dead_fn_elim;
#   pub mod dead_fn_elim_xcrate;
./x.py build --stage 1 compiler/rustc library

R="build/host/stage1/bin/rustc --sysroot build/host/stage1 -O -Zdead-fn-elimination"
# single-pass kernel (mergeable): binary-only elimination, no env var
$R main.rs   # -> note: N unreachable functions excluded

# sidecar prototype (two passes):
DFE_SIDECAR=/tmp/dfe.bin $R b.rs && DFE_SIDECAR=/tmp/dfe.bin $R a.rs && DFE_SIDECAR=/tmp/dfe.bin $R main.rs  # writes sidecar
DFE_SIDECAR=/tmp/dfe.bin $R b.rs   # library now uses sidecar seeds
```
