# Minimal cargo change for single-pass cross-crate dead-fn-elimination

Branch: `dfe-cargo-minimal-singlepass`. Goal: the *transparent* version the sidecar
prototype could not deliver — **one `cargo build`**, libraries automatically sliced
using the binary's reachability, no env var, no second pass.

The constraint is endorsement: cargo changes need the Cargo team, so this finds the
**smallest** change that reuses cargo's existing pipelining machinery rather than
inventing a new orchestration layer.

Line numbers are against `rust/src/tools/cargo` at rust @ 1.93.

## The key reuse: pipelining already orders *phases* of a job

Cargo's pipelining already lets a dependent start on a *phase* of its dependency,
not only on full completion. `job_queue/mod.rs`:

```rust
enum Artifact {
    All,       // depend on the dependency's full rlib (codegen done)
    Metadata,  // depend only on its .rmeta (available mid-job)
}
```

When a lib's rmeta is ready, `job_state.rs:144` pushes
`Message::Finish(id, Artifact::Metadata, Ok(()))`, unblocking `Metadata` dependents
while the lib's own codegen continues. That is exactly the shape we need, one phase
earlier and in the other direction.

## The ordering we need (a valid DAG, no cycle)

```
libA(rmeta) ─► bin(analysis) ─► libA(codegen) ─► bin(codegen/link)
            └► libB(rmeta) ──┘  libB(codegen) ─┘
```

- `bin` already depends on `libA`/`libB` by `Metadata` (it needs their rmeta to
  type-check). Unchanged.
- **New edge:** `libA(codegen)` and `libB(codegen)` depend on `bin`'s *analysis*
  phase — the point right after `after_analysis`, where `-Z dead-fn-elimination`
  has computed the reachable set, *before* the binary does its own codegen.

No cycle: the binary's **analysis** is an earlier phase than its **codegen**, just
as a lib's **rmeta** is earlier than its **rlib**. The same phase-ordering trick
pipelining already relies on.

## The minimal diff — four touch points

### 1. A third artifact phase — `job_queue/mod.rs`

```rust
enum Artifact {
    All,
    Metadata,
    Analysis,   // NEW: the binary's reachability analysis is complete
                // (emitted after after_analysis, before the binary's codegen)
}
```

### 2. Emit the analysis-complete signal — `job_queue/job_state.rs`

Mirror the existing `rmeta_produced` path (line 142–145). The binary's rustc
already prints an artifact notification when it finishes analysis under the flag
(see the rustc side below); the job intercepts it and pushes:

```rust
// alongside the existing rmeta_produced():
pub fn analysis_complete(&self) {
    self.messages
        .push(Message::Finish(self.id, Artifact::Analysis, Ok(())));
}
```

### 3. Add the reverse edge for library codegen — `unit_dependencies.rs`

Where `compute_deps` builds a lib unit's dependencies, when
`bcx.gctx … dead_fn_elimination` is set and this unit is a library that the binary
depends on, add a dependency on the binary unit carrying the new `Analysis` phase.
This is the only new edge in the graph; everything else is unchanged.

(The lib→bin reverse edge is unusual but representable: cargo's unit graph is keyed
by `(Unit, …)`; the new edge points a library *codegen* requirement at the
binary's *analysis* phase, which completes first.)

### 4. Consume the phase in the queue — `job_queue/mod.rs`

In the `drain_the_queue` / dependency-decrement logic that already special-cases
`Artifact::Metadata`, treat `Artifact::Analysis` the same way: a unit waiting on
`Analysis` unblocks when `Message::Finish(bin_id, Artifact::Analysis, _)` arrives,
independent of the binary's later `Artifact::All`.

## The rustc side (already prototyped, branch `dfe-cross-crate-reference`)

Cargo's part only schedules; rustc does the work, and most of it exists:

- **Binary**: after `run_analysis`, instead of writing a sidecar file, emit an
  artifact notification (`--json=artifacts` channel) so the cargo job can push
  `Artifact::Analysis`. The reachable set is handed to library invocations via a
  rustc arg (`-Z dead-fn-elimination-reachable=<path>` to a cargo-owned temp under
  `target/`, which cargo cleans up — not a user-visible sidecar).
- **Library**: identical to the prototype's library path — seed BFS from the passed
  reachable set, eliminate the rest. Already verified to compile and run.

The difference from the sidecar prototype is purely *who drives it*: cargo, in one
build, via the analysis phase — instead of the user, in two builds, via a
`RUSTFLAGS` nonce.

## Why this is the minimal endorseable change

- **One new enum variant, one new edge, one new signal, one new queue case.** No
  new build phase concept beyond what pipelining already established; `Analysis` is
  to `bin` what `Metadata` is to a lib.
- **Opt-in and inert by default.** With `-Z dead-fn-elimination` unset, no
  `Analysis` edges are added and the unit graph is byte-for-byte today's.
- **No new on-disk format the user sees.** The reachable-set temp lives under
  `target/` and is cargo-managed, not a sidecar the user must name or sequence.
- **Reuses cleanup, fingerprinting, and parallelism** cargo already has for
  pipelined units — no parallel scheduler of our own.

## What is NOT minimal (and is therefore excluded)

- Splitting the binary into two *separate units* (analysis-unit + codegen-unit). It
  would also work but doubles the binary's fingerprint/scheduling surface; the
  phase-signal approach above avoids it by reusing the single-job, multi-`Finish`
  pattern pipelining already uses.
- Any change to registry-dependency handling. This applies to path/workspace libs
  the binary can see MIR for; registry crates ship without the needed MIR and are
  left full (the same boundary as the rustc-side analysis).

## Status

This branch carries the **design and the exact diff points**, not a built cargo:
building a working cargo requires co-building cargo + the patched rustc and a
matching `-Z` artifact channel, which is a larger lift than the four hunks above
warrant before the Cargo team weighs in on the `Analysis` phase concept. The
rustc-side library elimination it depends on is already built and verified on
`dfe-cross-crate-reference`. The intent here is to give the Cargo team a concrete,
line-anchored proposal to react to.
