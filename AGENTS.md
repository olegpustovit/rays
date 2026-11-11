# Rays development guide

Rays is a Rust creative-coding framework and native `.rays` Lisp workspace editor.
The Cargo workspace is the repository root. `crates/` owns libraries, the `rays_bin` application, Lisp
nodes and runtime assets. PXUI lives in the `pxui/` submodule (its own repository, with its own
AGENTS.md, gate and `tools/validate.sh`) in three tiers, described below; Rays depends on its public and
open crates by path and is not a workspace member there.
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

## How Rays uses PXUI

The `pxui/` submodule has three tiers (see pxui/AGENTS.md):

- **public** (`pxui`, `pxui_types`): the UI. Rays builds every panel, the graph and canvas text through
  `pxui`. Every Rays crate links it with `features = ["static"]`, so the one Cargo build, incremental as
  always, exercises the C ABI; a release build may drop the feature and load `libpxui`. The code is the
  same in both.
- **open** (`ogpu_*`, `input`, `sdl3`, `pxui_sdl`, `pxui_wayland`, `mem_sys`, `cfmt`): the GPU layer and
  window hosts. Rays links them statically and owns the device, the queue, the swapchain and the window.
- **private** (everything under `pxui/crates/private/`): never named by a Rays crate. A path containing
  `crates/private` in a Rays Cargo.toml is always wrong; `cargo run -p gate -- deps` refuses it. To reach
  something private, add an export, the wrapper method and the `pxui.h` line in `pxui/` first.

Rules:
- Rays creates the GPU device and hands its native handles to `pxui::Renderer` once (`rays_shell::host`).
  Each frame with UI layers, the scene renderer calls the host's `begin` before it records its first pass
  (`Renderer::prepare`: glyph uploads into the frame's command buffer); each UI layer is drawn into the
  scene's open pass in its place between Rays' own nodes (`Renderer::draw_layer`), and a layer with a
  backdrop blur splits the pass through `UiPass::split`. After PXUI draws, the host calls
  `ogpu_core::backend::invalidate_state` on the pass. PXUI never submits, waits or presents.
- Textures shown in the UI (images) cross as `native_texture` handles and live as long as the Rust value
  that shows them (`host::HostImage`); the frame serials the scene renderer passes keep them alive until
  their frames complete.
- Keys, buttons and events cross as the numbering of `input::wire` (`Key::code`, `Event::raw`): the host
  encodes a frame once; a call site that compares a key or a button uses `.code()`.
- Text results from the UI are borrowed `&str` valid for the frame: copy what the document keeps.
- A panic in UI-building code surfaces at the `ui.frame` call (`host::frame`). Do not catch it inside the
  closure.
- Drop a UI through `host::destroy` before its owner lets it go: the library refuses to destroy a `Ui`
  while a renderer of it lives, and the UI would stay allocated.

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
