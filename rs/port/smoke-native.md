# Native smoke: Rust `rays` vs OCaml `rays` (real windows, Windows/Vulkan)

First real-window run of the Rust editor. Command per sketch: `RAYS_MAX_FRAMES=60 rays FILE.rays`, one window at a time, hard 120 s kill.
Rust: `cargo build --release -p rays_bin` (target `rs/target/ag-smoke`), 4 m 38 s cold (3 m 55 s incremental after the rays_bin edit). OCaml: `_build/default/bin/rays.exe`.
Times are wall seconds on a machine shared with other agents' builds and tests, so treat them as rough.

Pixel diff: none. Neither side has a last-frame PNG capture in a real window (OCaml has only the window-free `tools/ui_shot.exe`; the smoke skill captures nothing from `rays FILE`), so there is no pixel comparison column data. Comparison here is exit code, time and stderr.

Fix made: the Rust binary never selected an OGPU implementation (`Ogpu.Impl.create_driver: no OGPU implementation selected`, exit 1 on every sketch). `rays_bin` now selects `ogpu_vulkan` (Windows) or `ogpu_metal` (macOS) at the top of `main` (Cargo.toml + main.rs). Nothing else was changed.

| sketch | Rust exit / time | OCaml exit / time | pixel diff | notes |
|---|---|---|---|---|
| audio.rays | 0 3.6s | 0 1.1s | n/a | ok |
| basic.rays | 0 0.9s | 0 0.7s | n/a | ok |
| drawing.rays | 0 3.2s | 0 0.9s | n/a | ok |
| file_dialog.rays | 0 1.2s | 0 0.7s | n/a | ok |
| flow_contours.rays | 0 2.8s | 0 1.5s | n/a | ok |
| flow_field.rays | 0 11.1s | 0 10.5s | n/a | runs, slower than OCaml |
| flow_image.rays | 0 1s | 0 0.9s | n/a | ok |
| flow_image_kernel.rays | 0 1.2s | 0 0.8s | n/a | ok |
| flow_kernel.rays | 0 3s | 0 1.8s | n/a | ok |
| flow_particles_gpu.rays | 0 13.3s | 0 5.2s | n/a | runs, slower than OCaml |
| flow_shader.rays | 0 2.8s | 0 1.4s | n/a | ok |
| flow_survey.rays | 0 9.6s | 0 1.8s | n/a | runs, slower than OCaml |
| flow_vectors.rays | 0 1s | 0 0.8s | n/a | ok |
| generative.rays | 0 1.1s | 0 0.7s | n/a | ok |
| gpu_sea.rays | 0 112.7s | 0 2.4s | n/a | E_MEMORY over budget (4.55 GB vs 4.23 GB, then 6.9 GB); window ran to max frames. OCaml 2.4 s: memory grows without bound in Rust |
| isogrid.rays | 0 6.7s | 0 0.9s | n/a | ok |
| isogrid_city.rays | -1073740791 53.9s | 0 2s | n/a | CRASH: memory allocation of 46 MB failed (abort, 0xC0000409) |
| isogrid_drift.rays | 0 77s | 0 1.6s | n/a | E_MEMORY 13 GB in use over 11.8 GB budget; 77 s |
| million_marks.rays | 0 3s | 0 0.8s | n/a | ok |
| noise.rays | 0 1.4s | 0 0.9s | n/a | ok |
| particles.rays | 0 1.2s | 0 0.9s | n/a | ok |
| pathtracer.rays | 0 6.2s | 0 1.3s | n/a | ok |
| procedural_modeling.rays | TIMEOUT 120.1s | 0 2s | n/a | HANG: main thread overflowed its stack (infinite flow_eval recursion), see below |
| pxui.rays | 0 1.8s | 0 0.7s | n/a | ok |
| recursive_rectangles.rays | 0 2.1s | 0 0.7s | n/a | ok |
| recursive_signals.rays | 0 2.6s | 0 0.7s | n/a | ok |
| shattered_cube.rays | TIMEOUT 120s | 0 18.8s | n/a | HANG: self-deadlock on the host-call session mutex, see below |
| shattered_studio.rays | TIMEOUT 120s | 0 23.3s | n/a | HANG: self-deadlock on the host-call session mutex, see below |
| sop_gallery.rays | 0 78.3s | 0 4.4s | n/a | 78 s vs 4.4 s; runs, slow |
| swarm.rays | -1073740791 62.3s | 0 57.9s | n/a | CRASH: allocation failure (0xC0000409), memory growth |
| swarm_crawling.rays | -1073740791 50.7s | 0 6.8s | n/a | CRASH: allocation failure (0xC0000409), memory growth |
| swarm_floating.rays | -1073740791 78.2s | 0 5.7s | n/a | CRASH: allocation failure (0xC0000409), memory growth |
| swarm_lib.rays | 0 3.2s | 0 0.7s | n/a | ok |
| voxel_wall.rays | 0 14.5s | 0 1.6s | n/a | runs, slower than OCaml |
| ws_bloom.rays | 0 13.1s | 0 1.5s | n/a | runs, slower than OCaml |
| ws_facade.rays | 0 2.6s | 0 0.9s | n/a | ok |
| ws_garland.rays | 0 4.1s | 0 1.2s | n/a | ok |
| ws_kit.rays | 0 1.9s | 0 0.9s | n/a | ok |
| ws_layout.rays | TIMEOUT 120s | 0 1.3s | n/a | HANG: self-deadlock on the host-call session mutex, see below |
| ws_morph.rays | 2 11.5s | 0 1.2s | n/a | CRASH: panic E_LOWER Attribute write without geometry (known: sopcat_morph_sketch) |
| ws_orrery.rays | TIMEOUT 120s | 0 31.2s | n/a | HANG/slow: E_MEMORY 5.87 GB over 5.28 GB budget, killed at 120 s (OCaml 31 s) |
| ws_rosette.rays | 0 12.6s | 0 1.2s | n/a | runs, slower than OCaml |
| ws_spreadsheet.rays | 0 1s | 0 1.2s | n/a | ok |
| ws_sunflower.rays | 0 18.9s | 0 1.6s | n/a | runs, slower than OCaml |
| ws_tiles.rays | 0 6.8s | 0 1.1s | n/a | runs, slower than OCaml |
| ws_tree.rays | TIMEOUT 120s | 0 1s | n/a | HANG: self-deadlock on the host-call session mutex, see below |
| ws_tunnel.rays | 0 2.3s | 0 1s | n/a | ok |
| ws_variations.rays | TIMEOUT 120s | 0 1.5s | n/a | HANG: self-deadlock on the host-call session mutex, see below |
| ws_wave.rays | TIMEOUT 120s (a later rerun aborted: allocation of 65 MB failed) | 0 3.1s | n/a | memory growth, see 4 |

Totals: 49 sketches. Rust: 36 exit 0 (gpu_sea and isogrid_drift print E_MEMORY budget errors; 8 are much slower than OCaml), 5 crashes (isogrid_city, swarm, swarm_crawling, swarm_floating, ws_morph), 8 timeouts killed at 120 s (procedural_modeling, shattered_cube, shattered_studio, ws_layout, ws_orrery, ws_tree, ws_variations, ws_wave). OCaml: 49/49 exit 0.

## Rust crashes and hangs, root causes

1. HANG, self-deadlock (ws_tree, ws_layout, ws_variations, shattered_cube, shattered_studio; hangs even at RAYS_MAX_FRAMES=1). gdb on the live process: main thread waits in `rays_editor::cook::update` (cook.rs:719) polling the async cook; thread `sop-async-cook` is blocked on `Mutex::lock` at `flow_sop/src/lower.rs:3492` (`snapshot`). `snapshot` holds `self.session` locked across `sop::session::cook(...)`, and that cook's node closure (lower.rs:3565) calls `snapshot` again on the same mutex: a same-thread re-lock. OCaml has no lock there. Fix (flow_sop, outside my scope): do not hold the guard across the cook, e.g. `session.take()` into a local (create when None), cook, then put it back and `close` any session found there. Backlog entry added.
2. HANG, stack overflow (procedural_modeling). Prints `thread 'main' has overflowed its stack` (256 MB stack from `.cargo/config.toml`), then the process never exits. Unbounded recursion `eval_raw_body -> eval_at -> eval_raw` in flow_eval/src/eval.rs:495/949/1133 reached from `core_region::lower` via `packed::pv_execute_request` / `pv_force` (a packed kernel compile re-entering the evaluator which forces another kernel). flow_eval is being redesigned elsewhere, so not touched. Also a defect: the overflow handler leaves the process hung instead of exiting.
3. CRASH, panic (ws_morph). Crash report `C:\msys64\tmp\rays-crash\Rays-20261104-172249-24508\report.txt` (also `%TEMP%\rays-crash\...`): `panic: the editor opens: "E_LOWER: Attribute write without geometry"`, raised from `flow_sop/src/lower.rs:3271` (`deps` for `sop/with_attr`/`with_group`: the `geometry` argument is neither `Deferred` nor `No_geo`; the node comes from `sop/blend_shapes` -> `set_color`). The code matches lower.ml:3426, so the evaluator hands lowering a different value kind for the geometry arg; same as the existing backlog item sopcat_morph_sketch.
4. CRASH, memory (isogrid_city, swarm, swarm_crawling, swarm_floating, ws_wave on rerun: `memory allocation of N bytes failed`, exit 0xC0000409). Plus E_MEMORY budget errors on gpu_sea, isogrid_drift, ws_orrery (live bytes 4.5 to 13 GB where OCaml runs the same sketches in 2 s). Owners named in the message: `flow_gpu.slots`, `flow_gpu.free/held`, `renderer.mesh`, `renderer.gpu_item`, `sop.session.payload`, i.e. caches that are not bounded/released per frame (a leak or missing eviction). No backtrace: the abort path does not write a crash report. Not root-caused; backlog entry added. Part may be the box running other agents, but the E_MEMORY ledger figures are the process's own.
5. Slow (not failures): sop_gallery 78 s vs 4.4 s, flow_field, flow_particles_gpu, flow_survey, voxel_wall, ws_bloom, ws_rosette, ws_sunflower 11 to 19 s vs 1 to 10 s.

## window-free mem, 3 frames (2026-11-04)

`rays mem sketches/X.rays --frames 3`, one sketch at a time, hard 60 s kill, machine shared with other agents (about 4 GB free RAM, WSL holds about 18 GB). Rust: `target/shared/release/rays.exe` (HEAD 5901cd8f plus working tree). OCaml: `_build/default/bin/rays.exe` (`dune build bin/rays.exe` reported up to date). "live" is the ledger's live total (Rust prints "live bytes", which includes an unattributed share that is process memory with no owner; OCaml prints "ocaml live", the GC heap), "footprint" is the process footprint. Every run exited 0 inside 60 s; no OCaml run printed an E_ line, including E_MEMORY.

| sketch | Rust time / live / footprint / unattributed | OCaml time / live / footprint | E_ lines | Rust live / OCaml live |
|---|---|---|---|---|
| ws_tree | 2.6 s / 381 MB / 849 MB / 376 MB | 1.5 s / 28 MB / 407 MB | none | 13.6x |
| ws_layout | 2.9 s / 470 MB / 972 MB / 467 MB | 3.0 s / 52 MB / 431 MB | none | 9.1x |
| shattered_cube | 33.4 s / 3.52 GB / 4.12 GB / 3.36 GB | 54.7 s / 2.36 GB / 2.95 GB | none | 1.5x |
| shattered_studio | 36.7 s / 3.77 GB / 4.59 GB / 3.44 GB | 59.6 s / 2.58 GB / 3.40 GB | none | 1.5x |
| ws_wave | 5.6 s / 776 MB / 1.27 GB / 771 MB | 2.6 s / 54 MB / 442 MB | none | 14.5x |
| swarm | 13.1 s / 7.42 GB / 11.00 GB / 7.08 GB | 6.5 s / 476 MB / 4.32 GB | Rust: `E_MEMORY: 11.00 GB in use over the 9.90 GB budget` (gpu.driver 1.96 GB, flow_gpu.held 671 MB, sop.volatile 338 MB, flow_gpu.resident 134 MB); exit code still 0 | 15.6x |
| gpu_sea | 10.6 s / 2.56 GB / 4.33 GB / 2.28 GB | 4.6 s / 593 MB / 1.79 GB | none | 4.3x |
| procedural_modeling | 11.8 s / 1.11 GB / 1.83 GB / 1.10 GB | 6.9 s / 54 MB / 439 MB | none | 20.7x |
| ws_variations | 6.1 s / 703 MB / 1.29 GB / 696 MB | 7.0 s / 77 MB / 461 MB | none | 9.1x |
| ws_morph | 4.9 s / 805 MB / 1.40 GB / 800 MB | 5.5 s / 59 MB / 433 MB | none | 13.7x |

Reading: 3 frames never grow live memory in either (live N -> N), so the gap is resident baseline, not a leak. In Rust 88-99% of live is "unattributed" (memory no ledger owner claims), the lead for the next memory pass; swarm is the only sketch over the Rust budget (9.9 GB, tightened by the low free RAM) and is the one with a large attributed share (gpu.driver, flow_gpu.held). Rust is faster than OCaml only on shattered_cube/studio (33-37 s vs 55-60 s, OCaml near the 60 s kill); it is 1.5-2.5x slower on swarm, gpu_sea, procedural_modeling and ws_wave. Raw outputs: `C:\msys64\tmp\mt_<sketch>_{rs,ml}.out`.
