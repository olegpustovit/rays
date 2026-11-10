# PXUI development guide

PXUI is a Rust native UI engine: `pxui` (layout, widgets, draw list), `pxui_text`, `pxui_render`,
window hosts (`pxui_sdl`, `pxui_wayland`), the AT-SPI2 bridge (`pxui_atspi`) and the portable GPU layer under them (`ogpu_*`, `*_sys`).
The Cargo workspace is the repository root; `crates/` holds everything, `tools/` the test runner and
the gate, `fixtures/` the goldens, `specification/` the design contracts. Rays embeds this repository
as its `pxui/` submodule through path dependencies: every crate must build and test here alone, and
nothing may reach outside this repository (no `../../..` paths past the root).

## Fast iteration

- Read the actual flow and callers first. Reuse existing code, std and the installed dependencies.
  Write the smallest correct change, delete obsolete code, add no abstractions.
- Fast compilation AND fast debug execution are requirements. Measure both before changing profiles,
  dependencies, crate boundaries or hot paths. Budgets: clean debug build of the workspace under 30 s
  (cosmic-text is about 13 s of it); warm `cargo check -p pxui` after an edit under 1 s.
- Keep incremental compilation and the warm shared `target/`. No routine clean, private target
  directories or RUSTFLAGS/profile overrides.
- Inner loop: `cargo check -p <crate>`. Focused regression: `cargo t -p <crate> [filter]`.
  Never run raw `cargo test` or test binaries: `cargo t` kills cases at 5 seconds and runs at 60 seconds,
  serializes GPU/window cases, reports the slowest tests and records `target/t-times.tsv`.
- A focused warm run should finish under 30 seconds. Never raise deadlines to hide regressions or
  weaken assertions. Leave one small runnable regression for nontrivial logic.
- Run `git diff --check` and `cargo run -p gate -- deps` before committing; commit completed stages.

## Contracts

- Dependencies are std and `cc`. The one exception is cosmic-text, in `pxui_text` only.
- A warm frame allocates nothing; `mem_sys`'s counting allocator is what the tests assert against.
  A UI draws with a small fixed number of draw calls (one instanced pipeline); a change that adds
  draw calls or per-frame allocations needs a measurement and a test that pins the new number.
- `pxui`, `pxui_text` and `pxui_render` contain no SDL and no window code, and `pxui` knows nothing
  about the GPU. Input is data in (`input::Event`, a logical size, a scale factor, a time); output is
  data out (a draw list plus cursor, IME, clipboard and repaint request). A target is a render pass
  the caller opened, not a swapchain.
- Metal (Apple Silicon) and Vulkan (Windows, Linux) are both first class. No Metal-only or
  unified-memory assumption in a GPU data path. No CPU raster, OpenGL or browser fallback; missing GPU
  support is a typed startup error.
- Hosts expose the same handful of functions; there is no `Host` trait and nothing generic to
  monomorphise. Public functions take concrete types or `&mut dyn FnMut`.
- SDL/GPU/window/resource operations stay on the initial thread. No per-frame threads.
- UI and input use logical points; drawable-pixel conversion belongs at the backend boundary.
  UI returns intents; it does not mutate application state during frame construction.
- Preserve the theme tokens, Pragmasevka, 24-point rows/bars and 20-point controls unless a spec
  change says otherwise. Goldens are owned here (`fixtures/`): refresh them alongside the intentional
  change, at the display density the golden was made at.
- Required C/C++/Objective-C++ ABI code lives under its owning `*_sys` crate; the Metal bindings are
  checked in under `crates/metal_sys/src/`.
- `cargo run -p gate -- deps` enforces crate boundaries and `-- shape` code shape (no generic items, no `unsafe` outside `*_sys`, no `impl Trait`, no panics for control flow); policy is `tools/gate/src/policy.rs`.

Mark deliberate shortcuts with known ceilings using `ponytail:` and an upgrade trigger.
Never shortcut correctness, input validation, lifetime safety, security or accessibility.
