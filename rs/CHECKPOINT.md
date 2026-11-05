# Rust port checkpoint — 2026-11-04

The OCaml Rays codebase (~270k lines, `lib/`) is ported to the Cargo workspace `rs/` (edition 2024,
toolchain 1.99.1, std + `cc` only). Plan and rules: `port/artifact-src/index.html` (published as
microapps artifact `15ksz3bazstq`). This is a live completion ledger; checked items require executable
evidence. The current completion pass has uncommitted Rust changes; untouched historical measurements below are not fresh verification.

## Where it stands

| Area | State |
|---|---|
| Code ported | every OCaml library has its crate, plus the `rays` binary (check, fmt, mem, web, editor); one large missing feature found by audit (eval guards) is now ported |
| Byte parity with OCaml | sketch goldens, shaders (renderer/ogpu_metal/flow_gpu), web export, pxui pixels, `rays check`/`fmt` on all 204 files — exact; Mac gates pass |
| GPU backends | `rays` selects ogpu_vulkan on Windows, ogpu_metal on macOS; release build runs window-free on the Mac mini (basic, sop_gallery, ws_tree, ws_morph, gpu_sea exit 0) |
| Native sketches | all formerly failing sketches (deadlock, recursion, morph, memory aborts) run to the end window-free; gpu_sea root cause fixed at 8b9dd63c (re-measure) |
| Crash containment | node cook panic -> `uncaught_node_exception`; static kernel panic -> `E_PACKED_EXECUTION` refusal |
| Debug speed vs native OCaml | latest debug gate: box 28 ms vs 36, transform 74 ms vs 45, sphere 185 ms vs 205, garden 502 ms vs 260; transform/garden still fail |
| Memory | all seven paired pre-arena samples still exceed OCaml live memory; exact binary hashes, times, footprints and untracked shares are recorded in `port/smoke-native.md`; final packed-arena target remains unproved |
| Tests | exact 432-file evidence ledger: 324 assertion-bearing candidates, 57 source stubs, 19 harnesses, six support modules, 26 Mac-qualified source paths; assertion parity/qualification is tracked separately (`port/test_inventory.md`) |

## How to run

```sh
cd rs
cargo build --release -p rays_bin
./target/release/rays ../sketches/basic.rays             # window
./target/release/rays mem ../sketches/ws_tree.rays --frames 3   # window-free + memory ledger
cargo t -p <crate> [filter]       # the only test runner: 5 s/test, 60 s/run; RAYS_SLOW=1 for slow ones
```

Mac deps: `brew install pkgconf sdl3 sdl3_image sdl3_ttf sdl3_mixer`. Windows: mingw at `C:\msys64\mingw64\bin`
on PATH. A hook blocks plain `cargo test`. Build notes: `port/build-speed.md`.

## Key documents

- `port/parity_backlog.md` — every known divergence from OCaml, by area (FIXED marks inline)
- `port/missing_features.md` — audit of OCaml behaviour absent from Rust
- `port/test_inventory.md` — which OCaml tests are ported
- `port/post_parity_backlog.md` — speedups blocked only by byte-exact goldens
- `port/eval-design.md` — evaluator speed phases
- `port/smoke-native.md` — native and window-free sketch runs vs OCaml

## Active completion pass — 2026-11-05

- Verified: `cargo t -p flow_eval -p flow_syntax -p flow_check`, 149 tests passed, 9.561 s runner wall (including 7.22 s build); slowest sketch oracle 1.561 s. New default-stack subprocess covers deep checked terms/values, callback reentry, oversized arrays, recovery and 64 call levels. Scoped depth checks and fallible user-sized buffer reserves are implemented; recursive owner drops, arbitrary object clones and OS overcommit are not claimed contained.
- Verified: `cargo t -p ogpu_vulkan gpu_conformance_ray_tracing -- --nocapture`, native compacted motion-prototype regression passed, 7.851 s runner wall including build. `RAYS_SLOW=1 cargo t -p rays_pathtracer --test gpu_pathtracer` passed all nine native scenarios, including materials/motion, in 41.621 s including build. Retained foreign-thread buffer-readback conformance passes on Vulkan, Metal and mock.
- Verified: scene-command packed UI batching/clipping/reset oracle regression passed, 1.268 s runner wall including build.
- Qualified: Windows GPU-holder ports and both native backend readback contracts. Still being qualified: restored editor/native ports, fallible deferred topology and broad raw-byte attribute/storage cutover. Do not upgrade remaining coverage counts or close checklist items based only on source inspection.
- Mac mini access and logged-in display session confirmed at `olegs-mac-mini.taild870b7.ts.net`; its checkout is older and its Rust tree is untracked. Validation must use an isolated current-source snapshot, not overwrite that checkout.

## TODO

### Correctness / parity
- [x] Re-measured current Windows release gpu_sea at 1/3/8 frames: `flow_gpu.slots` stays 104.9 MB; payload readback stays zero; total readback is 24.6/73.7/196.6 KB respectively; no "left the device" or memory-error lines. Runs exit 0 in 2.27/2.28/2.51 s. Held buffers plateau at 113.2 MB by frame 3. Host/unattributed growth remains tracked under memory tasks.
- [x] Investigate alleged arena-wide `AddrHasher` collisions: premise disproved. `FnId`/`ResidualId` hash only `.0` (identity), not arena `.1`; 64 sequential IDs per type produce 64 distinct hashes. Actual IdMap consumers use keyed operations; replay ordering uses logs. Kept production hasher unchanged; regression checks distribution, lookup and identity-preserving replacement.
- [ ] Process-wide state slows later lowerings in earlier measurements: garden 0.7 s alone, 1.0-1.5 s after transform (`core_lower::lower` -> `residual_view` 3.3 -> 9.3 µs/call). Latest unchanged-hasher transform→garden run gave garden 653 ms; variability/root cause remains unresolved, not proved to depend on hash iteration order.
- [ ] Gap 3 (`port/missing_features.md`): stack overflow and out-of-memory abort; OCaml `Eval.protect` gives `E_DEPTH` / `E_ARRAY_MEMORY` (depth counter + allocation guard)
- [x] Windows native fatal failures terminate promptly instead of waiting for fault/CRT dialogs: the CLI configures the process policy before GPU/check/eval startup. SDL-live subprocess regressions reach a real worker-stack overflow and native abort, exit nonzero, and leave the supervising SDL process usable. `cargo t -p runtime --test window_runtime_visibility native_ -- --nocapture`: three passed, 1.987 s runner wall; overflow 0.061 s, abort 0.119 s. This is termination safety, not recovery of arbitrary native OOM/overflow as a Diagnostic.
- [x] Swarm's extra CPU sweep recook fixed at the shared interrupt boundary: completed work is retained unless a stop was actually observed; packed workers inherit the caller's cancellation latch, and cancelled chunks never restart through the reference cook. Both focused regressions pass (0.024/0.023 s). Current release swarm, 3 frames, exits 0 in 3.81 s with 783.8 MB live (historical Rust 7.42 GB); the separate OCaml memory target is still open.
- [x] T6 workspace-IR disagreements resolved by installing the same evaluator hooks before baseline/static and lowered evaluations. Full path/tuple/value assertions pass for all 13 `workspace_ir` cases, including ws_kit, voxel_wall, flow_kernel, pathtracer and kit. `cargo t -p rays_editor --test gpu_suite_t6 workspace_ir -- --nocapture`: 27.722 s runner wall including build; all cases below 5 s (kit 4.89 s).
- [ ] Editor suite skips listed in `port/parity_backlog.md` (run_result_view materialise off-thread, run_cameras pick, probe geometry_zone template count, `$card` vs `sop/box` plan kind, materials explosion slow path)
- [x] Rust Vulkan compaction preserves triangle geometry metadata for motion prototypes after original-BLAS destruction; removed tracer skip-compaction/reuse workaround. Native backend conformance and all nine native tracer scenarios pass. The separate dirty OCaml implementation was not modified.
- [x] Mac real-window native smoke: isolated source snapshot `rays-checkpoint-8a8FS2B1`, release build, `RAYS_MAX_FRAMES=30` for basic (4.46 s) and sop_gallery (11.59 s), both exit 0 on the logged-in Cocoa display. Retained shared-buffer readback conformance also passes all four Metal backend cases (7.746 s runner wall including build). Final shipping audit must refresh the snapshot after remaining changes.
- [ ] Full windowed native smoke on Windows again (last: 36/49 before today's fixes)

### Memory (target: at or below OCaml)
- [x] `syntax-share` merged (3992f0af, 065f4239): shared `Arc` children (Syntax::clone O(1)), boxed trivia (Syntax 128 -> 64 B), size asserts on Syntax/Node/Term
- [ ] Measure ws_tree live before/after (381 MB Rust vs 28 MB OCaml); then shrink `Term` (176 B), interned names (`u32`), `u32` positions
- [ ] Arena layout for syntax/terms (packed node tables, `u32` ids)
- [ ] Lowering copies still present: `lisp_factory`, `qualify_workspace` / `static_with`, `inline_policy` (`label_context`, schema per call)
- [ ] Untracked heap in the memory ledger (88-99% "unattributed"): charge the big owners
- [x] Recorded paired three-frame Rust/OCaml baselines for gpu_sea, swarm, procedural_modeling, ws_wave, ws_morph, ws_tree and ws_variations; all exit 0 within 30 s without memory errors. `port/smoke-native.md` identifies exact pre-arena binary SHA-256 hashes, raw logs and reused successful samples. Unmet live-memory targets and tree/variations readback differences remain explicit, not waived.

### Debug speed (target: every gate case below native OCaml)
- [ ] Gate: `ONLY=box,transform,sphere,garden RAYS_SLOW=1 cargo t -p flow_sop debug_lowering_beats_native_ocaml -- --ignored --nocapture`
- Latest focused gate: `ONLY=transform,garden RAYS_SLOW=1 cargo t -p flow_sop debug_lowering_beats_native_ocaml -- --ignored --nocapture` failed the existing speed thresholds (transform 164 ms, garden 653 ms; evaluator 77/130 ms). Runner wall 31.815 s including 29.02 s build; test 2.669 s. Evaluator verification: `cargo t -p flow_eval`, all 86 tests passed, runner wall 1.795 s; slowest sketch-oracle test 1.510 s.
- [ ] transform/garden: non-evaluator lowering (`packed::compile_result` ~0.9 ms each, `core_lower::residual_body`, qualify walker body compare/clone, mesh node compiling two programs, cards)
- [ ] Evaluator: lazy closure context; per-node overhead in compiled code (budget arithmetic, Result copies); operators' bodies looking arguments up by name
- [ ] rays_bin e2e goldens take 7-19 s each in debug (over the 5 s budget without RAYS_SLOW)
- [x] Default test-budget violations repaired without dropping coverage: 129 manifest sections pass (slowest 2.919 s); 21 promotion sections retain cutoff/grain/domain matrix (slowest 0.814 s); 23 region sweeps retain all 40,000 HSL times (slowest 0.308 s). Original full-library manifest generation remains explicit qualification, not a silent-success default test.
- [ ] Unlock `port/post_parity_backlog.md` once Rust outputs become the goldens

### Tests still to port (`port/test_inventory.md`)
- [ ] rays_editor (~280 cases, 13 files) — largest
- [x] flow_gpu missing five suites, helper coverage and remaining resident-run matrix ported. `cargo t -p flow_gpu -p ogpu_mock -p ogpu_vulkan` passes all selected unit/native targets (19 runner groups, 41.880 s including build). Native lifetime tests exposed and fixed real device-buffer clobbering and release closures retaining reader liveness; lazy owned readbacks join off-thread without owner pumping. Metal backend conformance also passes on Mac.
- [ ] flow_sop remaining files (test_concat, test_planes done)
- [ ] scene_execution (~78), renderer (~37)
- [x] flow_web `region_arguments` and `array_composition_web`: 78 enabled tests ported, executing emitted JavaScript with serial/1/8-worker runtimes. Fixed non-UTF-8 authored error operands being folded into quoted diagnostic strings; native Core error dispatch now reuses evaluator semantics. `cargo t -p flow_web`: 119 passed, 4.702 s runner wall, slowest 2.263 s. `cargo t -p flow_ir test_core_errors::`: six passed, 12.360 s including build.
- [x] Metal / ogpu_metal source assertion ports completed and qualified on the Mac mini: `cargo t -p metal_sys -p metal -p ogpu_metal`, all build-config, 19 Metal safe-wrapper, eight native OGPU and four conformance scenarios pass (10.746 s runner wall including build). New checks include render-encoder errors, exact sampler bytes, negative native bit patterns and owner-only handle invariants. Device failures no longer silently pass.
- [ ] Complete assertion-level parity/qualification of the evidence candidates, including the historical 46 PARTIAL files and 48 weak PORTED rows; candidate presence is not blanket completion.
- [ ] Parity-ignored tests (`#[ignore = "PARITY: ..."]`) — each a known difference to resolve
- [x] Audit stale leaf-crate MISSING entries: param already ported; LRU functional/accounting/lifecycle and allocation-free hit paths now qualified. `cargo t -p lru`: all 18 pass; the counting allocator records zero bytes across 1,000 warmed hits and checks exact cached values. The old hit-only PARITY ignore is removed.
- [x] Reconciled inventory by exact current source filenames, not arithmetic: all 432 paths occur once in the evidence ledger, with zero duplicate or unclassified filenames. Retired inconsistent inherited PORTED/STUB subtotals and corrected known renamed/misidentified counterparts. Full assertion-level parity remains the separate task above.

### Process
- [ ] Commit `rs/` on `dev` per green checkpoint; never stage the OCaml working tree's uncommitted files
- [ ] Agents: shared brief rules — `cargo t` only, 5 s tests, window-free sketch repros (`rays mem --frames 3`, 30 s kill), size asserts on hot structs, never revert files wholesale, edit with Edit/Write (PowerShell Set-Content corrupts UTF-8)
- Runner verification: each native case now has an individual process/deadline; GPU/window cases are serialized rather than hidden in summed batch limits. Windows Job ownership survives root exit and kills captured-pipe descendants before joining readers. `cargo t -p rays-t`: 12 passed, 4.584 s runner wall; the orphan-process timeout regression passes.
- [x] Updated and published artifact progress page: https://artifacts.microapps.space/a/15ksz3bazstq, version 14. Previewed the actual HTML surface; current verified gates and remaining work are explicit, original plan retained as historical reference. Refresh again before final shipping.
