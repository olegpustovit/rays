# Rays development guide

Rays is a Rust creative-coding framework and native `.rays` Lisp workspace editor.
The Cargo workspace is the repository root. `crates/` owns libraries, the `rays_bin` application, Lisp
nodes and runtime assets. PXUI, the `ogpu_*` GPU layer, the window hosts (`pxui_sdl`, `pxui_wayland`),
`input`, `sdl3`, the `*_sys` bindings, `mem_sys`, `cfmt`, the `cargo t` runner, the fonts and the PXUI
goldens live in the `pxui/` submodule (its own repository, with its own AGENTS.md, gate and
`tools/validate.sh`); Rays depends on its crates by path and is not a workspace member there.
`sketches/` owns runnable workspaces. `fixtures/` owns shared goldens; `specification/` owns design
contracts. Historical port evidence lives in `docs/history/` and records known unfinished work.

## The pxui submodule

- Clone with `git clone --recurse-submodules` (or `git submodule update --init`). The pinned commit is
  the one Rays was tested with; `pxui/` is a normal checkout, so edit engine code there and `cargo
  check` here sees it through the shared `target/`.
- To change PXUI: commit and push in `pxui/` (main), then commit the new pointer here (`git add pxui`).
  `cargo run -p gate -- pxui` (tools/check-pxui-remote.sh, also part of `gate -- all`) refuses a pointer
  that is not on the PXUI remote; `RAYS_OFFLINE=1` skips it.
- Goldens of the PXUI crates are refreshed in `pxui/`; Rays refreshes its own. PXUI crates must build
  and test alone (`pxui/tools/validate.sh`) before their pointer moves.
- There is no hosted CI and no GitHub Actions, in either repository: validate locally with
  `pxui/tools/validate.sh` (macOS; `--windows` and `--linux` print the commands for the Windows box
  and WSL) plus the Rays commands below and in `docs/development.md`.

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
  Run `cargo run -p gate -- deps` for boundary edits (PXUI's own rules are in `pxui/tools/gate`). Existing violations are in the historical checkpoint.
- SDL/GPU/window/resource operations stay on the initial thread. Use the process-wide parallel worker
  pool for coarse pure work; no per-frame threads. Own resources and bounded caches explicitly.
- Scene/input/UI use logical points; drawable-pixel conversion belongs at the backend boundary.
- PXUI is the only UI engine. Preserve theme tokens, Pragmasevka, 24-point rows/bars, 20-point controls.
  UI returns intents; do not mutate the document during frame construction.
- Rays Flow follows `specification/flow.md`. New Lisp syntax ships with graph projection, direct editing
  gestures and a regression, never text-only syntax.
- Required C/C++/Objective-C++ ABI code lives under its owning sys crate. Metal bindings are checked in
  under `pxui/crates/open/metal_sys/src/`; no OCaml generator runs at build time.

Mark deliberate shortcuts with known ceilings using `ponytail:` and an upgrade trigger.
Never shortcut correctness, input validation, lifetime safety, security or accessibility.

Lisp authoring: [generated guide](docs/lisp.md); lookup any name with `rays doc NAME`.
