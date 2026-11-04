# Rust port checkpoint — 2026-11-04

The OCaml Rays codebase (~270k lines, `lib/`) is ported to the Cargo workspace `rs/` (edition 2024,
toolchain 1.99.1, std + `cc` only). Plan and rules: `port/artifact-src/index.html` (published as
microapps artifact `15ksz3bazstq`). All agents were stopped at this checkpoint; everything finished is
committed on `dev`.

## Where it stands

| Area | State |
|---|---|
| Code ported | every OCaml library has its crate, plus the `rays` binary (check, fmt, mem, web, editor); one large missing feature found by audit (eval guards) is now ported |
| Byte parity with OCaml | sketch goldens, shaders (renderer/ogpu_metal/flow_gpu), web export, pxui pixels, `rays check`/`fmt` on all 204 files — exact; Mac gates pass |
| GPU backends | `rays` selects ogpu_vulkan on Windows, ogpu_metal on macOS; release build runs window-free on the Mac mini (basic, sop_gallery, ws_tree, ws_morph, gpu_sea exit 0) |
| Native sketches | all formerly failing sketches (deadlock, recursion, morph, memory aborts) run to the end window-free; gpu_sea root cause fixed at 8b9dd63c (re-measure) |
| Crash containment | node cook panic -> `uncaught_node_exception`; static kernel panic -> `E_PACKED_EXECUTION` refusal |
| Debug speed vs native OCaml | sphere 165 ms vs 205 (faster); box 53 vs 36; transform 169 vs 45; garden ~1-2.6 s vs 260 |
| Memory | Rust live 1.5-21x OCaml on the window-free table (`port/smoke-native.md`), mostly syntax/term representation |
| Tests | 432 OCaml test files: 233 ported, 55 empty stubs, 45 partial, ~96 missing (`port/test_inventory.md`) |

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

## TODO

### Correctness / parity
- [ ] Re-measure gpu_sea after 8b9dd63c: `rays mem sketches/gpu_sea.rays --frames 1/3/8`; `flow_gpu.slots` must stay flat (OCaml ~105 MB, 74 KB readback), no "left the device" lines
- [ ] eval.rs `AddrHasher::write_i64/write_usize` overwrite instead of mixing: every id in an arena collides. A proper mix made garden-after-transform slower (2.3-3.8 s), so something depends on map layout/iteration order — find it (determinism smell), then fix the hash; test by probe/entry counts
- [ ] Process-wide state slows later lowerings: garden 0.7 s alone, 1.0-1.5 s after transform in one process (final `$card` `core_lower::lower` -> `residual_view` 3.3 -> 9.3 µs/call); suspect inline-memo replay inflating the arena residual map
- [ ] Gap 3 (`port/missing_features.md`): stack overflow and out-of-memory abort; OCaml `Eval.protect` gives `E_DEPTH` / `E_ARRAY_MEMORY` (depth counter + allocation guard)
- [ ] After a stack overflow the process hangs instead of exiting
- [ ] swarm: one extra CPU recook of the tube sweep (lower.rs ~1277, packed.rs) doubles its peak vs OCaml
- [ ] Workspace-IR disagreements (t6, `T6_RUN_SKIPPED=1`): ws_kit, voxel_wall, flow_kernel, pathtracer, pxui kit
- [ ] Editor suite skips listed in `port/parity_backlog.md` (run_result_view materialise off-thread, run_cameras pick, probe geometry_zone template count, `$card` vs `sop/box` plan kind, materials explosion slow path)
- [ ] Vulkan compaction-motion-prototype defect (also present in OCaml)
- [ ] Real-window run on macOS (only window-free so far), then the native smoke on Mac
- [ ] Full windowed native smoke on Windows again (last: 36/49 before today's fixes)

### Memory (target: at or below OCaml)
- [x] `syntax-share` merged (3992f0af, 065f4239): shared `Arc` children (Syntax::clone O(1)), boxed trivia (Syntax 128 -> 64 B), size asserts on Syntax/Node/Term
- [ ] Measure ws_tree live before/after (381 MB Rust vs 28 MB OCaml); then shrink `Term` (176 B), interned names (`u32`), `u32` positions
- [ ] Arena layout for syntax/terms (packed node tables, `u32` ids)
- [ ] Lowering copies still present: `lisp_factory`, `qualify_workspace` / `static_with`, `inline_policy` (`label_context`, schema per call)
- [ ] Untracked heap in the memory ledger (88-99% "unattributed"): charge the big owners
- [ ] gpu_sea, swarm, procedural_modeling, ws_wave, ws_morph, ws_tree, ws_variations baseline vs OCaml (`port/smoke-native.md` table)

### Debug speed (target: every gate case below native OCaml)
- [ ] Gate: `ONLY=box,transform,sphere,garden RAYS_SLOW=1 cargo t -p flow_sop debug_lowering_beats_native_ocaml -- --ignored --nocapture`
- [ ] transform/garden: non-evaluator lowering (`packed::compile_result` ~0.9 ms each, `core_lower::residual_body`, qualify walker body compare/clone, mesh node compiling two programs, cards)
- [ ] Evaluator: lazy closure context; per-node overhead in compiled code (budget arithmetic, Result copies); operators' bodies looking arguments up by name
- [ ] rays_bin e2e goldens take 7-19 s each in debug (over the 5 s budget without RAYS_SLOW)
- [ ] flow_sop `manifest_reproduces_the_checked_in_file` 52 s, `group_query_promotion_matches_native_any_incidence_masks` 18 s, flow_ir `core_regions_sweep_hsl_full_part_*` ~9 s — default-run budget violations
- [ ] Unlock `port/post_parity_backlog.md` once Rust outputs become the goldens

### Tests still to port (`port/test_inventory.md`)
- [ ] rays_editor (~280 cases, 13 files) — largest
- [ ] flow_gpu (~181 cases, 5 files) — was in progress
- [ ] flow_sop remaining files (test_concat, test_planes done)
- [ ] scene_execution (~78), renderer (~37), flow_web (2 node-driven files: region_arguments, array_composition_web)
- [ ] metal / ogpu_metal (~67) — on the Mac
- [ ] 45 PARTIAL files; 48 weak "PORTED" rows to re-verify
- [ ] Parity-ignored tests (`#[ignore = "PARITY: ..."]`) — each a known difference to resolve

### Process
- [ ] Commit `rs/` on `dev` per green checkpoint; never stage the OCaml working tree's uncommitted files
- [ ] Agents: shared brief rules — `cargo t` only, 5 s tests, window-free sketch repros (`rays mem --frames 3`, 30 s kill), size asserts on hot structs, never revert files wholesale, edit with Edit/Write (PowerShell Set-Content corrupts UTF-8)
- [ ] Update the artifact progress page (`port/artifact-src/index.html` -> `15ksz3bazstq`)
