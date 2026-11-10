# Rays development guide

Rays is a Rust creative-coding framework and native `.rays` Lisp workspace editor.
The Cargo workspace is the repository root. `crates/` owns libraries, native bindings,
the `rays_bin` application, Lisp nodes and runtime assets. `sketches/` owns runnable
workspaces. `fixtures/` owns shared goldens; `specification/` owns design contracts.
Historical port evidence lives in `docs/history/` and records known unfinished work.

## Fast iteration

- Read the actual flow and callers first. Reuse existing code, stdlib/native features and installed
  dependencies. Write the smallest correct change, delete obsolete code, avoid new abstractions.
- Fast compilation AND fast debug execution are requirements. Measure both before changing profiles,
  dependencies, crate boundaries or hot paths. Rust should beat archived native OCaml in both modes.
- Keep incremental compilation and the warm shared `target/` cache. No routine clean, private target
  directories or RUSTFLAGS/profile overrides. Dependencies are std + cc; justify additions with evidence.
- Inner loop: `cargo check -p <crate>`. Focused regression: `cargo t -p <crate> [filter]`.
  Never run raw `cargo test` or test binaries: cargo t kills cases at 5 seconds and test runs at
  60 seconds, serializes GPU/window cases, reports slowest tests and records `target/t-times.tsv`.
- Stage tests over time: focused case after an edit; touched crates and affected consumers at a stable
  boundary; affected native/GPU checks before commit. Broad/slow qualification belongs at the end,
  explicitly enabled with `RAYS_SLOW=1`. Do not repeatedly run the workspace suite in the edit loop.
- A focused warm run should finish under 30 seconds. Shrink fixtures/share setup before moving genuinely
  exhaustive work to qualification. Never raise deadlines to hide regressions or weaken assertions.
- Leave one small runnable regression for nontrivial logic. Report commands, wall times and slowest cases.
  Run `git diff --check` and commit completed stages after their checks. Known failures stay visible.

## Contracts

- Lisp is the first-class public surface. OCaml is retired: `ocaml-reference` tags `e92f94f0`, the last
  complete implementation. Compare in a separate worktree, never restore an OCaml build dependency.
  See `docs/development.md` for workflows and performance comparison instructions.
- Preserve Lisp semantics, determinism and exact sequential/parallel equality. Rust owns its goldens;
  OCaml byte parity is not required. Refresh goldens alongside intentional changes.
- Native backends are Metal on Apple Silicon and Vulkan on Windows. No CPU raster, SDL2, OpenGL or
  browser fallback. Missing GPU support is a typed startup error. Web export is a separate 2D target;
  editor chrome stays native.
- Keep foundational crates independent of runtime/UI. Flow syntax/check/eval/IR/shader/web remain UI-free.
  Run `cargo run -p gate -- deps` for boundary edits. Existing violations are in the historical checkpoint.
- SDL/GPU/window/resource operations stay on the initial thread. Use the process-wide parallel worker
  pool for coarse pure work; no per-frame threads. Own resources and bounded caches explicitly.
- Scene/input/UI use logical points; drawable-pixel conversion belongs at the backend boundary.
- PXUI is the only UI engine. Preserve theme tokens, Pragmasevka, 24-point rows/bars, 20-point controls.
  UI returns intents; do not mutate the document during frame construction.
- Rays Flow follows `specification/flow.md`. New Lisp syntax ships with graph projection, direct editing
  gestures and a regression, never text-only syntax.
- Required C/C++/Objective-C++ ABI code lives under its owning sys crate. Metal bindings are checked in
  under `crates/metal_sys/src/`; no OCaml generator runs at build time.

Mark deliberate shortcuts with known ceilings using `ponytail:` and an upgrade trigger.
Never shortcut correctness, input validation, lifetime safety, security or accessibility.

Lisp authoring: [generated guide](docs/lisp.md); lookup any name with `rays doc NAME`.
