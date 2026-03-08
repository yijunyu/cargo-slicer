# Speeding Up Rust Builds: Part I — The Waiting Game

*Part I of III. [Part II: The Gap](blog-part2-en.md) | [Part III: Closing the Gap](blog-part3-en.md)*

---

## 17 Minutes of Your Life, Gone

Let's talk about Zed.

Zed is a gorgeous code editor written in Rust. Fast. Sleek. Modern. The kind of project that makes you proud to be a Rust developer.

Now try building it from source:

```
$ time cargo build --release
...
Finished `release` profile in 16m 52s
```

Seventeen minutes.

You start the build. You check your email. You make coffee. You drink the coffee. You check Reddit. You wonder if you chose the wrong career. The build finishes. You realize you had a typo. You start again.

This isn't a Zed problem. This is a Rust problem. Or rather, a *big Rust project* problem. Zed has over 500,000 lines of code across 198 workspace crates. That's a lot of Rust for the compiler to chew through.

But surely we can do better, right? The Rust community has been optimizing the compiler for years. Let's try everything.

## Attempt 1: Parallel Frontend

Rust nightly has a parallel frontend. More threads, more speed. Simple.

```bash
RUSTFLAGS="-Z threads=8" cargo +nightly build --release
```

Result: the build gets maybe 5-10% faster. Nice, but we're still waiting 15 minutes. The parallel frontend helps with parsing and type checking, but the real time sink is LLVM codegen — and that's already parallelized per codegen unit.

## Attempt 2: Faster Linker

Linking takes time. Let's use `wild`, a fast linker written in Rust:

```bash
RUSTFLAGS="-C link-arg=-fuse-ld=wild" cargo +nightly build --release
```

Result: linking goes from ~8 seconds to ~3 seconds. Great for linking. But linking is less than 1% of the total build time. We've saved 5 seconds out of 1,012. The bottleneck isn't linking.

## Attempt 3: Compilation Caching

`sccache` caches compiled crates, so rebuilds are faster:

```bash
RUSTC_WRAPPER=sccache cargo build --release
```

Result: the *second* build is blazingly fast. But the *first* build — a clean, fresh build — is exactly the same. And in CI, every build is a fresh build. Your new developer's first `git clone && cargo build`? Fresh build. Switching branches with incompatible deps? Fresh build.

Caching doesn't reduce the work. It just remembers it for next time.

## Attempt 4: Cranelift Backend

What if we skip LLVM entirely? The Cranelift backend compiles much faster:

```bash
RUSTFLAGS="-Z codegen-backend=cranelift" cargo +nightly build
```

Result: significantly faster compilation. But the output isn't optimized. Cranelift is great for development builds, but for release builds — the ones your users run, the ones CI produces — you want LLVM's optimizations. We need `--release` to be fast.

## Attempt 5: Profile-Guided Optimization of rustc

The Rust project already ships a PGO-optimized compiler. Years of work have gone into making rustc itself faster. The nightly you're using right now benefits from all of that.

And yet, here we are. Seventeen minutes.

## The Honest Question

So let me ask you something uncomfortable.

We've tried the parallel frontend. We've tried faster linkers. We've tried caching. We've tried alternative backends. We've tried optimizing the compiler itself.

What if the compiler is already doing its job well? What if the problem isn't *how* the compiler compiles, but *what* we're asking it to compile?

Think about it. When you `cargo build --release` on Zed, the compiler dutifully compiles every public function in every library crate. The `regex` crate exposes dozens of functions — your project calls maybe three. The `serde` crate has hundreds of methods — you use a fraction. The compiler doesn't know this. It can't. It's compiling each crate in isolation, and any public function *might* be called from downstream.

What if a significant chunk of the compiler's work is simply... unnecessary?

What if we could tell the compiler, before it even starts, "hey, you don't need to bother with these 9,000 functions"?

That would be interesting.

*To be continued in [Part II: The Gap](blog-part2-en.md)...*

---

*This is Part I of a three-part series on cargo-slicer, a tool for speeding up Rust release builds. Part II introduces the "separate compilation gap" and measures just how much work is wasted. Part III shows how to close the gap.*
