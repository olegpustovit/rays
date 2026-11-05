# Native smoke: Rust `rays` vs OCaml `rays` (real windows, Windows/Vulkan)

## Unused library retention removed — Mac M1, 2026-11-05

Sequential window-free `ws_tree --frames 3` observations, each exiting 0 inside a 30-second deadline.
Same source checkout, release profile and machine as the packed-arena baseline below; single samples
on a shared machine, not timing medians. Compiled paths now share interned storage, and normal
workspace checking retains only the library declaration index and reached implementation forms.
The explicit full-source/manifest APIs remain available and account retained syntax exactly once.

| Rust stage | SHA-256 | Wall | Live | Footprint | Unattributed |
|---|---|---:|---:|---:|---:|
| Shared compiled paths | `6934a2a190ab43d7401bd85bee72b11f5c678a3a47c435c8218567f37d6fa238` | 1.92 s | 101.0 MB | 153.6 MB | 80.8 MB |
| Library storage accounted | `3a28de5abee278223631f59cca9fe1522d7e84b6fecd31ef9ba7e83996f84928` | 1.97 s | 100.9 MB | 152.5 MB | 61.7 MB |
| Unused library AST not retained | `6e971616a7fd10997a1bde08b28abff8a2703bf56bd602d00fe79a4622b15442` | 1.53 s | 82.4 MB | 132.0 MB | 62.1 MB |

Accounting alone exposed 18.9 MB as `flow.library.syntax`; it did not free it. Avoiding that retained
AST storage reduced actual live memory by roughly 19 MB from the 101.3 MB baseline. Rust still
exceeds the paired OCaml live memory (28.4 MB) by 2.9 times. All three runs retain the same checked
term/syntax ledger (10.8/4.0 MB); the remaining unattributed heap is still unresolved.

Raw logs: `rs/target/memory-mac-20261105/library-retention/`. The temporary arena trace found only
one evaluator arena (97 functions, 1,180 residuals and 2,060 plan entries), disproving the initial
64-arena-ring hypothesis for this sketch; instrumentation was removed. The native malloc call tree
locates about 21 MB under library parsing and about 23 MB under prelude-cache construction; these
allocation stacks overlap other owners and must not be summed as disjoint ledger accounts.
The profiling run used 1,000 frames and stack logging, so it is not a comparable timing/footprint
sample. Payload readback varies across these short async runs (33.8, 4.8 and 15.0 KB); this is not
proof of readback parity or of its resolution.

The final lazy-library release above also passes real-window 30-frame smokes for basic (1.76 s)
and sop_gallery (10.69 s), both exit 0 with no stderr diagnostics, under 30-second deadlines.
Their logs are in `library-retention/` beside the memory samples. The separate debug CLI gallery
golden still exceeds its 5-second default test budget; the diagnostic golden pass is not a waiver.

## Current packed-arena measurement — Mac M1, 2026-11-05

Both binaries rebuilt from the current working tree, window-free, sequential three-frame `ws_tree`
runs with a 30-second deadline. Rust release build: 2 m 20 s. These are single observations on a
shared machine, not a controlled comparison with the historical Windows pre-arena numbers below.

| Binary | SHA-256 | Wall | Live | Device | Footprint | Unattributed | Payload readback |
|---|---|---:|---:|---:|---:|---:|---:|
| Rust `rs/target/release/rays` | `1db2f0a4e61a05c15d16580e06ef8db7dd013a155f98fecf4f0cbefefba94cb2` | 2.32 s | 101.3 MB | 7.3 MB | 155.1 MB | 81.1 MB | 41.8 KB |
| OCaml `_build/default/bin/rays.exe` | `e871cb3d2d7bec7af83a7c9a30821bdf42868f05d92fdff4ed98c2bda0717de2` | 3.71 s | 28.4 MB | 7.4 MB | 97.4 MB | 1.3 MB | 33.8 KB |

Both exit 0 without memory-budget errors; live memory stays flat across the measured frames.
Rust still uses 3.6 times the OCaml live memory. Its ledger charges checked terms 10.8 MB and
syntax 4.0 MB, but leaves 74% of explicit memory unattributed. The readback difference remains
unresolved. Both report the same three device-fallback sites (transform, mesh and polywire).
Raw logs: `rs/target/memory-mac-20261105/ws_tree-{rust,ocaml}.log`.

The same Rust binary also opens real Cocoa/Metal windows for `basic` and `sop_gallery` with
`RAYS_MAX_FRAMES=30`: both exit 0, respectively 7.50 s and 11.09 s, under 30-second deadlines.
No native stderr diagnostics were emitted. Logs: `basic-window.log` and `sop_gallery-window.log`
in the same directory. This refreshes the two-sketch Mac smoke only; no pixel parity or broad
Windows/native qualification is inferred.

```sh
cargo build --release -p rays_bin  # from rs/
dune build bin/rays.exe           # from repository root
rs/target/release/rays mem sketches/ws_tree.rays --frames 3
_build/default/bin/rays.exe mem sketches/ws_tree.rays --frames 3
```

## Historical Windows smoke

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

## Paired pre-arena baseline — 2026-11-05

Window-free, three measured frames, one process at a time, 30-second deadline; all samples exit 0
without memory-budget errors. These identify the **pre-arena binaries**, not the subsequently
edited working tree or a final memory/parity claim:

- Rust `rs/target/ag-gpumem/release/rays.exe`, SHA-256
  `84bb9ade8a969196247e8c92dafefb3ebe068821986ca9af76905c32e1be1eb3`.
- OCaml `_build/default/bin/rays.exe`, SHA-256
  `1c9e130e49e02be294c299608572e4053afa0cf78e8eee0ba5229f8afe640210`.
- Remaining paired raw stdout/stderr:
  `rs/target/memory-baseline-20261105-prearena/<sketch>-<rust|ocaml>.out/.err`.
  Earlier successful Rust gpu_sea/swarm/ws_tree measurements from this pass were reused, not rerun.
- Same 24-core Windows/Vulkan machine while implementation workers were active. Times are single
  observations, not interleaved medians or p50/p95 speed claims. Units/rounding are the binaries' output.

| Sketch | Rust seconds | Rust live | Rust footprint | Rust unattributed | OCaml seconds | OCaml live | OCaml footprint |
|---|---:|---|---|---|---:|---|---|
| gpu_sea | 2.28 | 1.08 GB | 2.02 GB | 835.3 MB | 4.630 | 616.3 MB | 1.81 GB |
| swarm | 3.81 | 783.8 MB | 4.51 GB | 442.1 MB | 5.832 | 466.0 MB | 4.31 GB |
| procedural_modeling | 9.230 | 764.9 MB | 1.42 GB | 760.6 MB | 6.252 | 53.6 MB | 443.6 MB |
| ws_wave | 4.969 | 680.2 MB | 1.16 GB | 676.4 MB | 2.121 | 53.6 MB | 446.6 MB |
| ws_morph | 2.891 | 538.2 MB | 1.09 GB | 533.6 MB | 6.317 | 58.7 MB | 433.2 MB |
| ws_tree | 2.23 | 236.1 MB | 684.5 MB | 231.2 MB | 1.693 | 28.1 MB | 407.1 MB |
| ws_variations | 4.108 | 542.1 MB | 1.10 GB | 535.1 MB | 6.559 | 77.2 MB | 462.2 MB |

gpu_sea readback agrees at 73.7 KB with zero payload and no fallback lines. Swarm readback agrees at
204.9 MB (204.8 MB payload); its GPU driver/held allocations are still substantial. Procedural,
wave and morph readbacks agree. Rust tree reads 43.0 KB versus 29.5 KB; variations reads 6.0 KB
versus 3.0 KB. Those readback differences and all above-OCaml live-memory gaps remain open;
large Rust unattributed shares motivate the owner/packed-arena work.

The full 30-frame real-window Windows pass used the original 120-second per-sketch deadline.
All 49 sketches launched: 48 exit 0; ws_orrery times out before creating its window. GDB identifies
recursive definition/body cloning during lowering, not a renderer/event-loop hang. The sharing fix
is implemented but requires a rebuilt-snapshot rerun. Complete per-sketch stdout/stderr:
`rs/target/native-smoke-verified-20261105/<sketch>.out/.err`.

## Prelude ownership follow-up — 2026-11-05

Release SHA-256: `ab59c774f5d0bac06df2153e7fdb990136d3685817bc86a24c1b64fcb9388c19`.
`rays mem sketches/ws_tree.rays --frames 3` exits 0 in 2.02 s under the 30-second deadline:
82.5 MB live, 7.3 MB device, 133.7 MB footprint, 62.0 MB unattributed.
The new `flow.library.preludes` owner reports 14.8 MB, largely formerly charged to document
syntax/terms through the same shared arenas. No memory saving is claimed. It accounts for the
cache independently of an open document; catalog regression covers that lifecycle and repeat reports.

Raw log: `rs/target/memory-mac-20261105/library-retention/prelude-accounting.log`.
Allocation investigation: `lazy-malloc-call-tree.log` in the same directory, captured from a
500-frame run of the preceding `6e971616…` release with `MallocStackLogging=1`. That instrumented
run is diagnostic, not a three-frame parity measurement: 83.0 MB live, 208.7 MB footprint.
Its stacks put 35.5 MB under initial lowering and 22.6 MB under prelude caching (inclusive
allocation stacks, not additive ledger owners). The shared prelude arenas do not explain the
remaining 62 MB unattributed heap; evaluator/lowering ownership remains open.

## Shared inline-replay captures — 2026-11-05

Release SHA-256: `6278069cc24abe3cba0047a2460fac24208ee4430e78dc071236f01d479cf359`.
The same three-frame `ws_tree` command exits 0 in 1.95 s with 80.5 MB live, 7.2 MB device,
129.8 MB footprint and 60.0 MB unattributed. The immediately preceding release above measured
82.5 MB live: unchanged replay environments/lists save 2.0 MB. This remains above OCaml's
28.4 MB baseline. Readback varies with asynchronous completion and is not a parity claim.
Raw log: `rs/target/memory-mac-20261105/library-retention/shared-replay.log`.

Real-window checks with `RAYS_MAX_FRAMES=30` and individual 30-second deadlines both exit 0:
basic 2.21 s and sop_gallery 13.19 s, with no stderr diagnostics (only `/usr/bin/time` output).
Logs: `shared-replay-basic.log` and `shared-replay-gallery.log` in the same directory.

## Native test helper and vector edits — 2026-11-05

The Rays/editor/PXUI helper had skipped GPU assertions on macOS. It now reuses the
renderer helper's Metal backend and lease cleanup; supported-platform creation failures
fail the test. Windows keeps fatal-validation Vulkan. Case sharding stays unchanged.
These are offscreen native tests, distinct from real-window smoke above.

Verified through `cargo t` with default deadlines:

- `rays --test gpu_rays_all gpu_rays_primitives`: 1.381 s test, 2.852 s runner, exit 0.
- `pxui --test gpu_pxui_parity gpu_parity_overlays`: exact golden pixels, 0.346 s test,
  0.592 s runner, exit 0.
- `rays_editor --test gpu_suite_t1_b run_result_view`: complete original script, 4.399 s
  test, 9.797 s including build, exit 0. The first real run exposed stale checked children
  after a whole-vector edit; the shared literal patch now rebuilds those children.
- `editor_document`: all 23 enabled tests pass, 5.140 s warm; the new direct regression
  compares checked terms and recorded evaluation against full checking (0.305 s).

Logs: `/tmp/rays-metal-shared-primitives.log`, `/tmp/rays-metal-shared-pxui-pixels.log`,
`/tmp/rays-result-view-vector-default.log`, `/tmp/rays-vector-literal-document-gate.log`.
The final workspace test compile exits 0 in 3.23 s. These checks do not qualify all newly
enabled native cases or replace Windows smoke.

## Shared document lowering — 2026-11-05

Release SHA-256: `19569fd1563124e52a1d35f35a23457d5f99e8798b895794a2b00f43b69fdad4`
(release build 2m 03s). `ws_tree --frames 3` exits 0 in 1.66 s:
77.9 MB live, 7.3 MB device, 127.6 MB footprint and 57.4 MB unattributed.
The previous measured `6278069…` release was 80.5 MB live. This version also includes
intervening diagnostic-byte and literal-edit fixes; these are versioned observations,
not an isolated allocation experiment. OCaml's 28.4 MB target remains unmet.

The immutable document now shares its lowering through `Arc`; caches retain only their
existing evaluation identity and frame/scope readers borrow the shared payload. Restore
uses copy-on-write for saved compiled IDs/sites. Document tests (23) and editor unit tests
(58) pass, and workspace test compilation passes in 4.64 s.

Real windows, `RAYS_MAX_FRAMES=30`, separate 30-second deadlines: basic exits 0 in 4.88 s,
sop_gallery exits 0 in 4.74 s. Neither prints diagnostics beyond `/usr/bin/time` output.
Raw logs under `rs/target/memory-mac-20261105/library-retention/`:
`shared-lowering.log`, `shared-lowering-basic.log`, `shared-lowering-gallery.log`.

Debug UI evidence is separate: original `run_cameras` correctness passes in 6.354 s with
the diagnostic slow timeout profile, down from 25.618 s. Full original geometry, four
scene instances and every gesture/assertion remain. Default still times out at 5.024 s;
it is not a green default gate. Creation remains the next profiled bottleneck. The
unchanged default `run_result_view` passes in 3.063 s after sharing (previously 4.399 s).

