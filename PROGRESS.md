# Port simplification execution

Plan source: [artifact 2beyggs594gc](https://artifacts.microapps.space/a/2beyggs594gc) (v6 shows the final status; evidence lives here).

Repo mirror: `docs/history/port-simplification/index.html`

**Order:** A ∥ B ∥ M ∥ I, then C, then D. One commit per bullet in Wave A runbook where practical.

**Current state (2026-11-09): finished.** All 63 plan items (waves A, B, M, I, C, D) are done on `dev`; see the Summary below and the session log for evidence. Artifact v9 is published with the final status, and its source SHA-256 (`70cb9c89…`) matches the repository mirror. Crates went from 67 to 51. Final qualification on the final code: Mac workspace 2,612 cases exit 0 and native editor suite 325/325; Windows (RTX 2080 Ti, Vulkan) workspace 2,585 cases exit 0 and native editor suite 325/325. The follow-ups outside the plan are listed in the last session-log entries and on the artifact.

## Summary

| Wave | Done | Partial | Not done |
|------|------|---------|----------|
| A | A1-A8 | - | - |
| B | B0-B5 | - | - |
| M | M1-M21, empty-frame target (1,206/frame) | - | - |
| I | I1-I6 | - | - |
| C | C1-C21 | - | - |
| D | D1-D6 | - | - |

## Items (commits)

A1-A8, B0-B4: see earlier rows below in git history (`6802a325` ... `96fab43d`); all done.

| ID | Status | Commit / evidence |
|----|--------|-------------------|
| B5 | done | `94aa4546` `35df934e` `7b374363` `6f75543d`; `a6dd5ea8` `rdk` facade deleted, `sop`/`flow_sop` use `rdk_*` directly; gate 30 -> 28 violations |
| M1 | done | `9dae5ebd`; regression in `sub_at` route cache fixed `2ba0d306` (broke test_lisp_fusion) |
| M2, M4, M6-M12, M20 | done | see git log (`bbe7f6ae` `0f3804c3` `69d08a8a` `dca77b6f` `66136b7f` `20c1dfb3` `c425eefa` `49ec71f3` `b5be8904` `f6ca915b`) |
| M5 | done | `ef645352` |
| M13 | done | `6f88bce2` Arc<Vec<T>> attributes; COLUMNS ring + FLOW_COPIES deleted; guard `attribute_round_trip_bytes` |
| M14 | done | `a0e5d08e` + repair `19868fbe` etc. (boxed `GeoPayload`) |
| M15, M16 | done | `38bfb307`, `00c4d596` |
| M17 | done, bounded variable-size buffers | C3 closure below removes the remaining numeric chunk copies and result slots. Shared attribute-map guard measures roughly half the float peak. Variable-length text/intersection results retain documented range buffers because output sizes are unknown before work executes. |
| M18 | done | `ed08c91b` `21da1965` i32 scratch; `INT_FLOATS` charged to ledger in `8bf0c0d0` |
| M19 | done | `13abe74c` `rays_math::text_array::TextArray` (offsets+bytes); 1M rows = 8 allocs; text literals as `Arc<[u8]>` skipped (hundreds of sites) |
| M21 | done, deviates | `1ccd60fc` `Rc<[u8]>` with `unique()` copy-on-write; plan premise "nothing mutates" was false (set_pixel, Image::replace, readback) |
| M3 | done | C6 closure below removes native-loop, frame, inactive-pane and carry model copies. Explicit external Editor snapshots still retain their own state. |
| I1 | done | `a06df67a` plus resumed closure: core.md token-reader/scatter rows now say 64-bit; frozen seed aliasing is described modulo 2^63, matching `wrap_int63`. |
| I2 | done | `c9c2b12a` plus resumed closure: builtin `abs` uses `wrapping_abs`; text token parsing relies on the native `i64` parser. Boundary regression failed on the old walker (`abs(-2^62)` returned negative), then passed for walker, strict Core and executor. Matching Node/runtime cases cover MIN/MAX, old 63-bit edges, signs, whitespace and out-of-range tokens. |
| I3 | done | `74942f50` `ocaml_int` deleted; frozen hashes remain in `rays_math::hash`. |
| I4 | done | `edc27be2` plus resumed native-boundary closure: RDK count helpers/key packing use checked i64 arithmetic, count/generation limits use native constants, SOP context accepts the full nonnegative frame range, reader accounting is checked, and node reservations reject native overflow. Node allocation checks exhaustion before wrapping. Frozen geometry/hash masks unchanged. Six focused boundary tests and 530 RDK/SOP cases pass. |
| I5 | done | `b946570e` `859aff62` MSL/SPIR-V + 42 fixtures |
| I6 | done | `4bcb5ba4` WGSL/JS/web, 185 web fixtures, plus resumed export closure: region `countLimit` is native i64; restored `flow_image`/`flow_particles_gpu` to the normal test suite and regenerated their stale text fixtures. All 19 export cases and 23 region-argument/worker cases pass. Exported binary-data hashes unchanged; generated image module renamed to match its content hash. |
| I fallout | done | `49737e72` scatter `sc_rf`; `2c7ebd25` sort/prelude/operators; `e2e2ed18` gpu_run carried fixture |
| C1 | done | `dc2ee86b` `ef645352` `e6ae19d5` `2612f106` `b4b90916`; Vulkan only type-checked on macOS; source lines grew, gain is allocations/closure types |
| C2 | done | `33e163b0` `faa8090e` |
| C3 | done, bounded variable-size buffers | ~30 earlier commits plus resumed closure: all three proximity-index and curve-join paths, numeric attribute mappers and primitive ordering write final disjoint windows. Text/intersection transforms use disjoint range structs and reserve exact final capacity; their variable payloads still need a merge until output sizes can be declared without running work twice (`ponytail:` comments). Census finds no remaining production per-range owned numeric result slots. 533 RDK/SOP tests and six allocation guards pass. |
| C4 | done | `a08c927c`, `fee04de1` plus resumed closure: triangulation/defaulted gathers use the central selector; instance repetition, fuse reductions, target mapping, Boolean consolidation, mesh merge and tuple mirroring use shared float-plane operations. `of_float_planes` consumes output buffers instead of copying them; all callers migrated. Normals keep their joint normalisation paths. Regression proves buffer identity for widths 1–4, missing/mismatched-plane errors, and tuple instance repetition. |
| C5 | done | `a12b1efb` |
| C6 | done | `4cfcff4a` `7d7241ae` through `0f1033a6`, plus mutable lifecycle closure below: reducers/frame/source/viewport/carry updates mutate the stored model; `ModelView` borrows pane and transient state, including the carry's original document. `panel_map` is plain data. The native sketch loop keeps its model in storage and mutates it through callbacks; successful replacement callbacks are also supported. No per-frame whole-model copies remain; external Editor snapshots copy only when mutated. Panic/report regression, pointer guards, native editor run and consumer checks pass. Basic 2,509 -> 2,002 allocs/frame; empty 5,732 -> 4,335. Empty ~1,200 target remains a separate open regression-guard gap. |
| C7, C8 | done | `8e6e0e51`, `69fe4afd` |
| C9 | done | `e0afebff` |
| C10 | done | `d87c7c95` `625ffd0f` `5b15cb29` `04b5ae16` `9bf5f9be`, plus canonical-name closure below: the merged GPU API/backend package, library, directory, Cargo dependencies and Rust imports are `ogpu_core`. No alias/facade crate retained. The other thin owners remain folded; `ocaml_int` was deleted outright (I3), with only intentional frozen hashes retaining 63-bit mixing. |
| C11 | done | `51ebdf92` `b52600c7` plus shared-cache closure below: `rays::table` and `rays::idlist` both use `renderer::side_lru`, backed by `lru::Cache` for ordering, capacity and ledger accounting. Local non-Send identity keys stay in the adapter; borrowed predicates preserve custom equality, newest-equivalent-key selection and exact MRU promotion. Value keys retain second-chance eviction. Drop releases charges/callbacks; basic frame allocations fall 2,002 -> 1,980. |
| C12, C13 | done | `34ea546d`; `b9816d3a` (7M floats identical) |
| C14 | done | `f3b38af3` |
| C15 | done | `b268ff32` `support::Choice<T>` (net -100, plan -400..500) |
| C16 | done | `8afb4996` |
| C17 | done | `84662760` `Gpu::Evidence` on `Config` (not on `Backend` trait: ~16 implementors) |
| C18, C20 | done | `b5aead5f`; `01b90841` `fold.rs` + `lru::Memo` (+163 net lines, not -140) |
| C19 | done | `dd441536` PartialEq derived (Hash impossible: f64 in ogpu types) |
| C21 | done | `7e9376dc` clippy::all allow removed; ~30 test-target lints remain |
| D1 | done | `959d5dae` `b72ab30d` `b4f74ecf` goldens `9a17f5d7` `1a5de4fe` `8fc3feb4`; 56 prepends removed; `a9213d00` last prepend -> VecDeque, six SPIR-V operand-order sites fixed after audit against spirv.ml (34 .spv fixtures, byte lengths unchanged, all pass spirv-val vulkan1.2) |
| D2 | done | `37691759` .. `2e224106` + `a0809e01` (5 stages): rdk errors `{operation, code: Code, message}`, typed cancellation |
| D3 | done | `4e8e7193` one WGSL printer, goldens byte-identical |
| D4 | done | `8bf0c0d0` `18180e84` packed.rs one CPU engine (historical implementation size 4,714 lines); `1455271c` bench_core example |
| D5 | done | Replaced the four float-array variants with `Value::Floats(width, Arc<Vec<f64>>)` throughout production and tests. Shared slicing, concatenation, reductions, literal conversion, cache/physical equality and GPU readback retain width. Validation rejects widths outside 1–4 and incomplete rows before indexing. `Value` remains 40 bytes; original type-specific keys, Lisp display and numerical data are preserved. 902 selected tests pass; one web FMA failure is reproduced on committed baseline `b52600c7`. |
| D6 | done | `f680001d` `d3b9fa62` `597110e1` `0bc18308` `cb8a0ade` `d3109b85` `c0de6479` `cc0680a2` `6570d563` (+ `da51a0c7` stale C8 test-hooks cases): rust-analyzer rename codemod plus perl for macro args and test-hooks code; `pub mod [A-Z]` 87 -> 0, `t` types 52 -> 0, capitalised re-export aliases 6 -> 0; 15 naming allows removed; 4 dead suite support copies deleted. 653 tests across flow_ir/gpu/eval, rdk_core and rays pass; gate deps 28 |

## Known failures / open risks

Resolved in the finish pass: D1 SPIR-V order, D4 FMA failure and benchmarks, D6 naming, the empty-editor allocation gap, and final qualification. The entries below are historical; current open items are the follow-ups in the newest session-log entries.

- Resumed audit next work: D1/D4 audit, D6, empty-frame allocation target and full qualification; C6/M3, C11 and D5 implementation are closed. User requested a committed checkpoint after D5; the full plan is not complete. `fee04de1` and `23d4bb15` are completed stages, not completion of the full artifact plan.

- `cargo t -p flow_sop` on the D2 tree: 700 tests, 696 pass; the 4 failures are the baseline ones below (`lend_session_tests` confirmed pre-existing by LowerClean at `11988707`). `cargo t -p rdk_core/rdk_geom/rdk_boolean/rdk_ops/sop`: 523 pass; `rdk_rays` 10 pass. Other full suites were **not** run on final HEAD under load (load avg 50-450). Focused runs per item passed (see worker reports in item rows).
- Pre-existing at baseline `11988707` (bisected by LowerClean): `test_node_facts::declared_preserved_topology`, `test_keyword_ports::keyword_manifest_defaults_projection_checked_gestures` (E_SHADOW), `test_settle::in_flight_device_writes_settle_at_a_cooks_end`, `test_projection::snapshots_of_the_scene_fixtures`, `lend_session_tests::escaped_zone_nodes_own_only_their_cook_context`.
- `flow_ir test_static_map::sop_deferral`: root cause `4ed6baf8` (constructor deferral); test rewritten (`a3e8c6df`); `StaticMap.deferred` now effectively dead.
- Studio qualification: `run_studio` and `run_studio_keywords` exceed the unchanged five-second case budget on both pre-text-change HEAD `3e4826d8` and the text-ownership tree; `run_studio_tail` passes on both. The mutable-loop closure still exceeds those limits (5.000/5.003 s); tail passes. These scenarios need reduced shared fixture cost or other measured runtime work; deadlines unchanged.
- Native editor qualification restored to compiling: `placement.rs` still called backend `cost`/`prepare` on `Evidence`, and passed a backend to `Display_sink` evidence APIs. Corrected the stale test calls. Calibration passes; `native_placement_chain_counterexamples` now exposes `placement.rs:206` missing placement-profile entry (`Option::unwrap()`); still open, not disabled.
- Confirmed still open during D5: `flow_web test_fma::checked_arithmetic_regions_match_exact_binary64` fails with `E_NONFINITE` on both committed baseline `b52600c7` and the D5 tree (baseline case 0.111 s). D5 does not introduce this failure. All four `flow_ir test_core_operators` text-array parts pass in D5 (slowest 2.985 s); `rays_editor` 64 unit tests, rays/native window cases and the selected pane/text/undo/carry/source/camera/world/drawing/picking checks pass after mutable lifecycle closure; the full native suite and `pxui_graph --features test-hooks` remain unverified after C6.
- Remesh macOS golden switched to the Windows hash constant (`3107f153`); cause (libm) unverified.
- Per-cook guards (a) walker, (b) aliased 4096-element store, (c) round trip, (d) subdivide peak and 1M-text now exist in `flow_sop/src/test_alloc_budget.rs`. The walker uses `Host::force_reference`, bypassing compiled execution. The store guard was verified with a temporary negative control: disabling alias release produced 8,204 allocations and failed the guard; restored code uses 12. All six pass after C3 adds the shared numeric-mapper guard. Per-frame guards `alloc_budget_basic`/`alloc_budget_empty` exist (`e74d3f1d`); caps are now basic 1,988 / 184,000 B and empty 4,350 / 186,000 B. **The artifact's empty-editor target (~1,200 allocations/frame) remains unmet**: measured 4,335. Verify the intended workload and reduce its remaining allocation sites; do not excuse the gap as a layout difference without evidence or raise limits.
- Not measured: B-wave compile-time gains, bench_core before/after for D4 (PackedMerge reported no regression sign only from one noisy run).
- `gate deps`: 28 pre-existing violations, none new.
- Throwaway worktrees `/tmp/rays-m13b`, `/tmp/rays-base`, `/tmp/rays-snap` can be deleted.

## Historical agent logs

| ID | log | scope |
|----|-----|-------|
| a2 | `.subagent-logs/a2.log` | A2 sop/rdk dead code |
| a5 | `.subagent-logs/a5.log` | A5 test-only gates |
| a6 | `.subagent-logs/a6.log` | A6 Cargo dep hygiene |
| a7-geom | `.subagent-logs/a7-geom.log` | A7 rdk_geom trim/triangle |
| review | `.subagent-logs/review.log` | plan audit (read-only) |
| b3 | `.subagent-logs/b3.log` | B3 gpu test merge |

| m10 | `.subagent-logs/m10.log` | M10 store alias fix |
| b0 | `.subagent-logs/b0.log` | B0 incremental notes |
| b1 | `.subagent-logs/b1.log` | B1 flow_check |
| b5 | `.subagent-logs/b5.log` | B5 flow_sop split (queued) |

These are archived logs from earlier stages, committed at the user-requested D5 checkpoint. They describe historical workers, not active jobs. C11 and D5 were completed by one agent.

## Historical plan audits (2026-11-08)

The snapshot totals in these old reviews are superseded by the current Summary above. Read-only reviews: `.subagent-logs/review.log` (Wave A snapshot); `.subagent-logs/review2.log` (63-ID tally); `.subagent-logs/review3.log` (2026-11-08) — **~27/63** with evidence, **2 partial** (B5, M18); next impact **I**, **M1**, **M14**, **C1**, **C3**; `rdk` facade remains for `sop`/`flow_sop` only.

## B0 — shared `target/` incremental invalidation

Policy (`AGENTS.md`, `docs/history/build-speed.md`, plan B0): one warm workspace-root `target/` for the dev edit loop; 41 s vs 7.6 s on the same touch means something below invalidated Flow crate incremental state.

**Invalidates or bypasses dev incremental cache (shared dir):**

| Cause | Effect |
|-------|--------|
| `cargo clean` (whole tree or `-p` on hot crates) | Removes incremental objects; next touch is cold. |
| Non-`dev` profile in the same dir (`--release`, `--profile e2e\|gpu\|slow`, `Makefile` `build`) | Separate output trees; changing `[profile.*]` or per-package `opt-level` can leave stale mixed-profile objects — recovery: `cargo clean -p <crate>` (`build-speed.md` llvm link failure note). |
| `RUSTFLAGS` / `cargo --config …` overrides for a build that shares `target/` | Re-hashes crate fingerprints; avoid on shared dir (policy #3 in `build-speed.md`). |
| `CARGO_TARGET_DIR` pointing away from root `target/` | Not invalidation, but edits do not warm the shared cache agents expect. |
| Toolchain / `.cargo/config.toml` `rustflags` change | Legitimate fingerprint change; full rebuild once. |

**Repo script audit (2026-11-08):** no checked-in `export RUSTFLAGS`, `CARGO_TARGET_DIR=`, or `--config` profile overrides. `tools/buildtime` / `tools/t` only *read* `CARGO_TARGET_DIR` if the shell sets it. `Makefile` uses `cargo build --release` for install only (intentional). Stray env from agents/CI remains the main unrepo’d risk for the 41 s regression.

**Warm pair** (`catalog.rs` marker `// b0: warm incremental smoke`, `cargo check -p flow_check` ×2, shared `target/`, M1):

| Run | `real` (wall) | Cargo `Finished` |
|-----|---------------|------------------|
| 1 (post-touch) | 0.15 s | 0.08 s |
| 2 (no edit) | 0.07 s | 0.02 s |

Budget reference (`build-speed.md` edit loop): flow_check warm check 0.4–0.5 s under load; this run is healthy (not stale).

## Session log

- 2026-11-09: **both platforms pass the native editor suite under unchanged limits** (`6fd9abe6`).
  - Commits:
    - `0fd78c5f` perf: the checker reuses identical defn and closure call-site bodies (80% of library-check inferences were re-checks). Full library and 49 sketches give byte-identical graphs, defs, paths, diagnostics and row counts. Library check 3.03 -> 1.64 s on the Mac. Regression `repeated_call_sites_check_to_fresh_equal_bodies`. ponytail: reused bodies live as long as the cached prelude.
    - `396cd70c` perf: a Lisp node rebuild reads only the packed sites and roots its body reaches.
    - `8f766a4b` harness: Windows test processes opt out of EcoQoS execution-speed throttling (run_ops 9.0 s on E-cores vs 4.6 s on P-cores).
    - `6fd9abe6` perf: a packed compile that no capture steers skips its fixed recompile. Regression `a_compile_no_capture_steers_is_the_fixed_one`.
  - Mac:
    - flow_check/eval/ir/syntax: 527 pass.
    - flow_sop: 475 pass in 45.8 s.
    - Native editor suite: 325/325, 179 s.
    - Workspace: 2,612 cases, exit 0, 201.9 s.
  - Windows (RTX 2080 Ti, Vulkan):
    - Native editor suite: **325/325**, exit 0, 270.9 s.
    - Workspace: 2,585 cases, exit 0, 277.8 s.
  - sopcat_perturbation (RAYS_SLOW): Mac 47 -> 25.7 s; Windows >60 -> 36.4 s.
  - Margins: Windows native_orrery_frames 4.62 s (peak 4.79), run_panels 4.42 s and run_ops 4.25 s are near the 5 s limit. Their remaining cost is lowering: ~80 ms per edit, ~240 ms per new editor, and orrery's per-frame time-driven rebuild. The follow-up stays parameter-independent lowering.

- 2026-11-09: Windows pass (kyiv-win-1, RTX 2080 Ti, Vulkan), commits `bda9754b`..`2104129f`.
  - `bda9754b`: Windows test harnesses release the shared Vulkan device after a panic and exit 101. Before, a leftover device crashed the process at exit (0xc0000005) and hid the real assertion. Vulkan does not specialize captures (no source to fold), so `specialized_captures` asserts none happen there.
  - `9a70d41c` + `0269f7f4`: Lisp trig, packed ops, core_exec/core_marks, scene_command path arcs and mat4 rotations use `rays_math::libm`. It is vendored msun/musl sin/cos/tan/asin/acos/atan/atan2 from rust-src (MIT, Sun notices kept), std-only, and bit-identical to the `libm` crate on 3M random inputs. macOS 27 and Windows ucrtbase differ on ~5% of sin/cos and 41% of tan. Goldens reviewed: round stroke indices 35/43 (…715); particles data.bin 742 of 30,000 f64 moved 1-2 ulp, other export files byte-identical. ponytail: exp/log/pow and rdk/editor/world trig still use platform libm.
  - `db24d045`: the counting allocator touches its thread-local only after `local_reset` (~15 ns per alloc on windows-gnu); flow_sop cases ran 30-40% faster on Windows.
  - `9a90f1fc` + `dc82511c`: keyword_manifest/optional_rest test one factory instead of checking the whole library (the full manifest stays covered by flow_manifest and the sopcat cases). wave/variations are split into lower and cook cases. combining_general_backbone cooks 1 and 4 domains in separate cases joined by a recorded whole-geometry digest, identical on both machines.
  - `a529b535` + `2104129f`: editor cases close every editor they create, and the scrub child releases the GPU before exit. These were Windows exit crashes: validation-layer code ran after unload, and a silent child crash caused a 60 s parent timeout.
  - Results:
    - Windows `cargo t --workspace`: 2,583 cases, exit 0, 257 s.
    - Mac `cargo t --workspace`: 2,609, exit 0, 219 s.
    - Mac native editor suite: 325/325.
    - Windows native editor suite: 313/325. The 12 others (run_unbound_panels, run_ops, run_panels, image_sop, native_orrery_frames, seven sopcat_*) **all pass functionally** under RAYS_SLOW on Windows (5.4-9.3 s). They exceed the Mac-calibrated 5 s budget because that box is slower at CPU-bound debug work. Processes started over SSH partly run on efficiency cores (optional_rest 5.4 s on P-cores vs 10.8 s on E-cores). run_ops is still 5.08 s at High priority.
    - sopcat_perturbation (RAYS_SLOW only) passes on the Mac in 47 s but exceeds 60 s on Windows.
  - Follow-ups: the flow_check library check (~3.4 s per process on the Mac) and value-dependent Lisp lowering are the shared cost behind these Windows timings.

- 2026-11-09: `55d989a8` qual: flow_sop shares light test processes. 98 test_parallel_exact cases become 39 tests (17 grouped `light_*`); 129 manifest goldens become 5; parity 75 -> 21. Each failure still names its section. Full `cargo t -p flow_sop` with default caps passes in 50.5-57.5 s at load 8-14 (472 tests, ~200 s summed).
- 2026-11-09: **Windows verification** on kyiv-win-1 (RTX 2080 Ti, MSYS2 UCRT64, synced with git bundles and no push).
  - `6a2d6505` fix: the `isogrid_orbit` example named `ogpu_metal` unconditionally since the port, so it did not compile on Windows. All Windows targets now compile.
  - Machine fix, approved by the owner: HKLM `SOFTWARE\Khronos\Vulkan\ExplicitLayers` registered the MINGW64 validation layer, whose libstdc++ conflicts with the UCRT64 test binaries (loader error 127, `VK_ERROR_LAYER_NOT_PRESENT`). Elevated SSH sessions ignore `VK_LAYER_PATH`. The UCRT64 layer is now added with value 0 and the MINGW64 entry set to 1. Undo (PowerShell): `Set-ItemProperty HKLM:\SOFTWARE\Khronos\Vulkan\ExplicitLayers 'C:\msys64\mingw64\bin\VkLayer_khronos_validation.json' 0; Remove-ItemProperty HKLM:\SOFTWARE\Khronos\Vulkan\ExplicitLayers 'C:\msys64\ucrt64\bin\VkLayer_khronos_validation.json'`.
  - Windows `cargo t --workspace` at `55d989a8`: 2,560 of 2,579 ok. Open:
    - crash: `gpu_run::specialized_captures` access violation.
    - libm: `test_shape2` round stroke, `web_export` particles data.bin.
    - over 5 s on this machine: flow_sop keyword_manifest, combining_general_backbone, optional_rest, variations, wave; gpu_pathtracer instanced; gpu_world furnace_mis and mirror; gpu_frame_alloc recursive_signals.

- 2026-11-09: **final qualification on the final code** (`5a575954`, docs-only commits after it):
  - `RAYS_T_TIMEOUT=7200 cargo t --workspace`: 2,843 cases, exit 0, no failures or timeouts, 234.5 s wall (load 5.8).
  - `RAYS_T_TIMEOUT=3600 cargo t -p rays_editor --features test-hooks --test gpu_editor`: 325/325, exit 0, 205.2 s wall (load 8.9).
  - The whole-run cap is lifted only for these broad runs. Every case met the unchanged 5 s per-case limit.
  - Remaining caveats:
    - A focused `cargo t -p flow_sop` sits at its 60 s whole-run cap (59.9 s pass / 60.2 s timeout at load 6.4-6.7).
    - Windows Vulkan and the shared cross-platform goldens are unverified: no Windows machine is reachable from this session.

- 2026-11-08: `5a575954` qual: `operators::parity` runs `LANES = 8193` (above packed's 4,096-lane threading threshold; 9 blocks / 5 chunks at 8 threads, with a partial last block) instead of 100,000/32,769. The 100,000-lane `random_three_outputs_match_native` keeps the 16-blocks-per-chunk cap. Parity: 37.1 -> 15.1 s summed; slowest case 1.90 -> 0.40 s.
  - Full flow_sop with default caps: 709/709 in 59.9 s at load 6.4; timed out at 60.2 s (693/709, no failures) at load 6.7. 709/709 in 59.4 s with the cap lifted.
  - The crate run sits at the 60 s whole-run cap and depends on machine load. Next lever: test_parallel_exact, 95 s summed (98 processes, each paying the library check).

- 2026-11-08: `282695fa` perf: cooked-selection programs are prepared at first cook. Preparing is infallible and forced before the placement timer, so the workspace's discarded inlined copy no longer prepares. `4dd7baca`: regression checks a single body build in `root`/`card_node`, zero rebuilds over new inputs and one shared prepared program; it fails with either fix reverted. subdivide build: cold 1.10 s, warm 0.48 s. The remaining second build is a genuinely different program (workspace constants vs factory inputs). Deferring its fallible first compile would move lowering errors to cook time, so it was not done.
  - Full flow_sop: all 709 pass, 65.4 s wall / 261 s summed. Default-cap runs stop at 60 s after 602-623 cases with no failures.
  - Summed time: workspace checks ~50 s (library re-check per process ~27 s), lowerings ~31 s, `operators::parity` ~22 s. Parity is the current timeboxed target.

- 2026-11-08: `91cda944` perf: the BUILT memo keys Lisp-node stand-in inputs by slot, scoped to one Lisp-node environment (`CallEnvShared::id`, `Options::body_of`). `restore` and editor input edits reuse the body, so a node build does 2 body builds instead of 3. subdivide build: cold 1.47 -> 1.20 s, warm 0.80 -> 0.57 s. Regression `a_lisp_node_rebuilt_over_new_inputs_builds_its_body_once` checks byte-identical cooks.
  - Full flow_sop: 709 pass, 64.7 s wall / 257.9 s summed (cap lifted, load ~8); still over 60 s.
  - Checks: flow_ir/eval/check 483 pass; gpu_editor sopcat_ 8, orrery, run_ops and run_panels pass.
  - Next: reuse the first lowering in `root`/`card_node` (option b) and remove the helpers' wasted workspace lowering.

- 2026-11-08: `228cd80f` perf: residual site lookup without copying the residual; subdivide node build 1.05 -> 0.80 s. `dfe28ee9` qual: 21 more test_parallel_exact fixtures shrunk under the `8f3cedb6` rules (98 cases); the module goes 137.8 -> 97.4 s summed. Full flow_sop: 708 pass, 69.2 s wall / 258.7 s summed with the cap lifted; it still exceeds the 60 s cap (58-60 s at load 8-9). Root cause: each Lisp node body is lowered three times (workspace lowering, `root`/`card_node`, `restore`), and the memo never hits because stand-in input nodes get fresh ids. Fixing that in the product now.

- 2026-11-08: closing the remaining gaps.
  - `the_other_sop_fixtures_lower_and_cook` (5.007 s) is split into tree, tunnel and wave cases (0.9, 0.5, 3.5 s) with the same per-fixture checks.
  - Full `cargo t -p flow_sop` with the cap lifted: 707 pass, 75.2 s wall, 299.8 s summed (4 workers). It exceeds the 60 s whole-run cap. test_parallel_exact takes 138 s, of which 27 heavy cases take 89.6 s; their fixtures are being shrunk under the `8f3cedb6` coverage rules.
  - Windows Vulkan cannot be verified from this session. THEVM does not resolve and THEVM2 is Linux aarch64 with no NVIDIA GPU. ogpu_vulkan type-checks on macOS. The shared remesh/marks/round-stroke goldens rest on the evidence in `d265c57f`, `882e12e1` and `9b48aa1e`.

- 2026-11-08: `8f3cedb6`: six test_parallel_exact fixtures shrunk with multi-chunk coverage kept (>=8 grain-97 chunks per domain on the dominant owner; the 6,145-point curve keeps the >4096 topology path).

  | Case | Before (s) | After (s) |
  |---|---|---|
  | combining_general_backbone | 7.12 | 3.7 |
  | combining_named_count | 5.70 | 0.7 |
  | modeling_curve_samples | 9.25 | 4.5 |
  | subdivision_all_edge_source | 6.62 | 2.8 |
  | subdivision_detail_source | 5.22 | 2.9 |
  | subdivision_hole_indices | 5.72 | 2.4 |

  97 pass; summed time 164 -> 137 s. The doc comment records the reductions.
  - Follow-ups:
    - `test_workspace_cook::the_other_sop_fixtures_lower_and_cook` 5.007 s (pre-existing at baseline).
    - combining_merged 4.5-4.7 s and groups 4.3-4.5 s are near the limit.
    - A full `cargo t -p flow_sop` (714 cases, ~240 s summed) needs `RAYS_T_TIMEOUT` for the whole-run cap, like any broad qualification.

- 2026-11-08: `0413a083`: native_orrery_frames plays 12 animated frames instead of 30, with every assertion kept (animation, no re-lowering during playback, 30 idle frames with no recompiles/re-prepares/new kernels/pipelines); 3.18 s. All five former native timeouts now pass.
  - **Follow-up (design, outside the plan):** value-dependent Lisp lowering costs ~210 ms per animated frame in debug for time-driven nodes (orrery ~5 fps in a debug editor). The same cost drives sopcat_perturbation (47 s, RAYS_SLOW) and the thin sopcat margins (~4.5 s). The fix is to lower driven bodies once and bind parameter values at cook time, or to cache `qualify_workspace` on everything except values (`flow_sop/src/lower.rs` `build_call`, `make_lowerer` -> `qualify_workspace`).

- 2026-11-08: native qualification at `3ca50482`, `RAYS_T_TIMEOUT=3600 cargo t -p rays_editor --features test-hooks --test gpu_editor`: 321 of 325 ok, 221.7 s, no assertion failures. Pass start: 285 of 322 with 37 failures.
  - Five timeouts remained. A prelude pass fixed four:
    - `d15bfd6a`: std-only Fx-style hasher on internal arena maps.
    - `e6459c8a`: `Cx::with_path` avoids path copies.
    - `97cd4849`: scrub fresh-process children run concurrently; step 7 reuses step 5.
  - Results: sopcat graph_edits/morph_sketch/node_inputs 5.0 -> 4.5-4.6 s; native_scrub_gpu 5.0 -> 1.9-2.2 s.
  - native_orrery_frames (~7.4 s) is in a 30-minute pass.
  - The prelude check is still ~3.4 s per process. Next gains are interned checker paths or per-signature closure-check memos, both larger refactors and follow-ups.

- 2026-11-08: final default qualification, `RAYS_T_TIMEOUT=7200 cargo t --workspace` at `1553dc64`: 2,823 of 2,839 pass, 305 s wall.
  - Three assertion failures, all pre-existing:
    - `e2e::lib_flow_gpu_shaders_indices`: fmt golden stale since `4ed6baf8` (pre-plan); refreshed in `9598a3d2`.
    - `scene_command::test_shape2::path_fills_and_strokes_match_ocaml`: five round-stroke values are now correctly rounded on macOS 27 libm (sin(pi) = 1.2246467991473532e-16); code unchanged since the port, updated in `9b48aa1e`.
    - `sdl3::cocoa_layer_ownership_and_trackpad_phases`: this SSH/tmux session's launchctl manager is "Background", so there is no window server ("cocoa not available"). It now skips with a printed reason (`8137a024`) and still runs in a GUI session.
  - Six `test_parallel_exact` cases sit at 5.00-5.03 s (combining_general_backbone, combining_named_count, modeling_curve_samples, subdivision_all_edge_source/detail_source/hole_indices). This is the interpreter-bound debug work recorded in the caching pass.

- 2026-11-08: caching/perf pass (pre-existing slow cases). Per-call region recompile was only 1-8% of profiles; the real costs were O(n²) node work, failed compiles inside an iterate loop and whole-catalog qualification on every lowering. All changes are bit-identical; no goldens or assertions changed.
  - Commits:
    - `0e452885`: group_edges incident rounds only when angle-filtered.
    - `4ca4ca07`: packed bodies fold over a table.
    - `21755fb5`: in-place text-array row store (was O(n²)).
    - `8a896d02`: edge_straighten covariance in one scan.
    - `3f8a06ba`: named-value packed uniforms read directly.
    - `2e087d4f`: term depth check once per arena row.
    - `6364d9c8`: Lisp body lowering qualifies only reachable packed sites (equal sets verified on 1,190 lowerings).
    - `b07759a2`: `specialize` memo.
    - `560bab91`: assembled Lisp body memo.
  - test_parallel_exact (seconds):

    | Case | Before | After |
    |---|---|---|
    | emission_uv_mapped | 5.05 | 1.36 |
    | emission_straightened | 12.5 | 0.78 |
    | generated_spiral | 4.73 | 0.42 |
    | sum of 97 | 153.4 | 142.6 |

  - Editor cases (seconds):

    | Case | Before | After |
    |---|---|---|
    | orrery | 16.4 | 7.4 |
    | workspace_packed | 6.7 | 4.0 |
    | sopcat_perturbation | >60 | 47.4 |
    | sopcat_manifest | 5.7 | 4.75 |

  - flow_sop with RAYS_SLOW: 705 pass / 82.8 s.
  - `2787e873`: sopcat_manifest_exhaustive and sopcat_perturbation (every field of every SOP) run under RAYS_SLOW, like shattered_studio_full and the OCaml exhaustive suite.
  - Remaining >5 s debug cases are real interpreter work (subdivide/sweep/carve kernels), ~1 s one-time lowering per process, value-dependent Lisp lowering, and child processes re-lowering gpu_sea (native_scrub_gpu 5.5 s). These need design changes (value-independent lowering, faster flow_check prelude) and are follow-ups outside the plan.

- 2026-11-08: residency and release pass.
  - `2521c306`: all-constant arrays are filled on the device, not uploaded.
  - `17014952`: input/gather tables are skipped when their bits match the last upload.
  - `12e7f15a`: the evaluator drops its resolver after the outermost call; it held the cook's device buffers.
  - `66fb612f`: resident release closures hold planes weakly, so the holder no longer keeps the old P. The holder test checks topology only when streams differ, because static topology is shared since `4ed6baf8`.
  - native_chain_frames was split into six cases (0.8-2.2 s each). A warm cook uploads 0 B; frame 0 uploads 12 B/pt, as in OCaml.
  - `ea3f6adb`: packed float add/sub/mul/div run blocks of 16 or more lanes over split slices with one nonfinite check. Release `map 1M` takes 2.29 ms (old engine 2.57, before 3.49); whole bench 139.3G (old engine) -> 111.2G instructions. **D4 closed.**
  - Bench `text map` digests: the old-engine/dev mismatch was the debug-format digest of text arrays. bench_core now hashes row bytes and checks the case against the reference interpreter; digest `b7fa12b23ecf8830` equals an independently computed 5,001 x "猫".
  - Tests: flow_gpu 94, flow_ir 342, flow_eval 105, native_chain 7, native_holder pass.
  - Open: native_scrub_gpu 5.46 s (eight child processes each re-lower gpu_sea; caching agent).
  - Follow-up, not in the plan: the path tracer rebuilds meshes whose topology is device-generated every frame. A prototype refit patch was reverted because it needs a liveness check.

- 2026-11-08: editor round 3 closed.
  - `1d40581a`: shattered_studio default case is 10 sheets, >=40 shards, exact 1/4-domain fracture and shading (0.92 s). The full sketch is `shattered_studio_full`, RAYS_SLOW only (33.6 s).
  - `4ec9daa0`: window_runtime_render 600 -> 300 frames (2.97 s). window_scene2_argument runs window sweeps 60/30/20 while the offscreen twin keeps 300/100/100 (3.1 s).
  - All editor-owned cases are now under 4 s. Open with the caching agent: orrery 16.4 s, workspace_packed 6.7 s, sopcat_* 4.8-6.5 s. GPU residency (chain/holder) and the release `map` regression are with a fresh agent.

- 2026-11-08: editor round 3, budget overruns.
  - Commits:
    - `2ac97b4d` perf: flow_sop inlined-node build cache 64 -> 1,024 entries. It thrashed (4,360 builds, 0 hits in run_ops), so every editor edit paid for it.
    - `5d1d6ff0`: run_ops reads the editor's shell instead of re-lowering.
    - `b6ae43d8`: `dump_line` uses `editor_loop::state_text` instead of writing a full crash dump.
    - `28bfa1cb`: shattered_studio fractures once per domain count.
  - Timings (RAYS_SLOW real time):

    | Case | Before | After |
    |---|---|---|
    | run_ops | 9.8 s | 3.8 s |
    | unbound_panels | 5.5 s | 3.5 s |
    | panel_keys | 5.0 s | 2.9 s |
    | run_editor | 5.1 s | 2.7 s |
    | run_panels | 5.6 s | 3.7 s |
    | shattered | 57 s | 22.8 s |
    | sopcat_* | 5.4-9.6 s | 4.8-6.5 s |

    All 51 `run_*` cases pass.
  - Decided:
    - window_runtime_render goes to ~300 frames (95% is `nextDrawable` compositor wait; still crosses the 240-frame uniform shrink).
    - shattered_studio keeps a small-fixture 1/4-domain exact variant in the default suite; the full-sketch >=1000-piece check moves to RAYS_SLOW (exhaustive qualification).
    - orrery, workspace_packed and sopcat_* go to the caching agent: `build_call` re-lowering and the `flow_check` prelude at ~3.5 s per process.

- 2026-11-08: Flow round 2.
  - Fixed:
    - `882e12e1`: marks picture 19 golden was stale. It is a serialized-scene digest, and macOS 27 now matches the Windows value.
    - `61e173bf`: sopcat_manifest compares against all registered factories, as the OCaml test did.
    - `b79de591`: `pipeline_compilations` counts only compiles a dispatch waits for, not background specialisation (regression `specialized_captures`).
    - `7653b55e`: isogrid_deferred deadlock. The test awaited a cook whose worker needed the main-thread GPU service; it now pumps as the editor loop does (1.02 s).
  - Root-caused, open:
    - Per-call region compile in `core_exec` (`compile_impl` on every `prepare_invoke`, ~40% malloc) and packed static evaluation of `fold` bodies element by element: test_parallel_exact 4.7 s, no regressing commit found across 5 builds.
    - Lisp node re-lowering on every field change: sopcat_perturbation 60.8 s; sopcat_manifest 5.7 s from factory registration.
    - chain: `mesh_resident` (`4ed6baf8`) uploads a 65,536-float grid array plus a fused P upload.
    - holder: old P device output held one frame.
    - release `map` +29%: the cost is outside the arithmetic loop; a fused `a+x*y` plan did not help and was reverted.
    - native_scrub_gpu 5.4 s.

- 2026-11-08: editor round 2.
  - Allocations/frame (Rust allocations in the harness), with caps lowered each stage:

    | Case | Before | After | Cap |
    |---|---|---|---|
    | empty | 1,698 / 105,039 B | 1,206 / 81,998 B | 1,210 / 82,500 |
    | bare | 1,918 | 1,417 | 1,420 / 97,000 |
    | basic | 1,521 | 1,384 | 1,390 / 157,500 |

    This meets the plan's ~1,200 empty-editor target (the plan's 1,121 was an injected-malloc measurement on the real app, so the two are close rather than identical). Commits:
    - `170875d3`: single-sample 2D batches share one render pass (was 12 passes/frame).
    - `669001fd`: Metal residency declared once per pass.
    - `73fe6b64`: graph pane edited in place.
    - `d3982e1f`: inspector fields borrowed; panel_intents lays out only when needed.
    - `b5a0df4a`: one layout shared by composition and focus.
    - `4810ffc7`: pxui box entry saves stack depths instead of cloning.
    - `c1f7f4e1`: chrome labels written directly.
    - `6d9f1861`: inspector borrows the scene graph.
  - Fixes:
    - `11d73658`: GPU handle "leak" was lazily created Metal depth-stencil states cached on the shared device. They are now created and released with the device, which fixes placement_retained, isogrid_viewport x8 and images x2.
    - `cab81140`: threading test path.
    - `bbfe3b39`: export skips on SDL "(dummy)" (export needs a window).
    - `deeeeddf`: slider offsets account for the Cull row added in `55482c94`.
  - Tests: rays_editor 64, pxui 28, pxui_shell 34, pxui_graph test-hooks 31; gpu_editor placement 4/4, consistency 33/33, marks_scene 18/18, scene_tree 9/9; workspace check clean.
  - Open: renderer `window_all` x2 time out (pre-existing); run_panel_keys and run_panels sit at 4.4-5.0 s.

- 2026-11-08: native suite triage. All 37 dev failures also fail at the pre-plan baseline `11988707` (full suite built and run at both commits in a throwaway worktree, plus `RAYS_SLOW=1` timing runs), so none is a plan regression. The plan made most slow cases faster (e.g. sopcat_graph_edits 16.0 -> 6.7 s, node_inputs 17.9 -> 6.8 s; orrery no longer hits its 444 MB OOM). The chain failure point moved at `537345e4`, which fixed a pre-baseline fuse bug and exposed hidden assertions: warm re-upload, 16 vs 12 B/pt. Pre-existing bugs to fix:
  - GPU handle leaks after close: images x2, isogrid_viewport x8, placement_retained.
  - chain warm re-upload: chain_gpu, chain_frames.
  - scrub-back recompiles a pipeline.
  - holder frames retain the old P.
  - sopcat manifest omits kinds.
  - marks picture 19 (rounded_rect) digest.
  - inspector drag offsets: run_loop_copies, scene_tree_c_slider.
  - threading test path is `../../../`, stale since `57487bbd`.
  - `export_or_skip` doesn't recognise SDL's "(dummy)" error.
  - 5 s budget overruns: run_ops/unbound_panels/editor/panels/panel_keys, sopcat_*, shattered_studio, isogrid_deferred, sopcat_perturbation, workspace_packed_capture_state, orrery.
  Logs are in the session scratchpad (`base.log`, `dev.log`, `*_slow.log`, `bisect-*`).

- 2026-11-08: Flow finish pass:
  - `bfb2a44a`: packed engine no longer finite-checks constants (fixes `test_fma`; introduced by `8bf0c0d0`).
  - `537345e4`: `core_fuse::merge` handles mixed uniform/constant captures (fixes `test_settle`).
  - `c15b1a31`: Lisp node cook mode was shadowed by a local, and the BUILT cache retained inputs (fixes node_facts and lend_session); two stale tests fixed.
  - `a21a7746`: multi-output kernels mark every int output uint32 (fixes `run_studio`/`run_studio_keywords`; regression `resident_sibling_ints`).
  - `ab43acc0`: `write_planes` no longer reads back held positions eagerly (regression `replacing_p_leaves_held_positions_unread`).
  - `d265c57f`: remesh golden is one value on all platforms; the platform branch was OCaml-inherited, not a libm difference.
  - `bf3d9fae`: D4 span cache made allocation-free.
  - D4 bench, debug min ms: sum 14.55 -> 11.39, fold-add 32.09 -> 24.51, fused maps 7.65 -> 6.37, int->float 5.97 -> 4.15, select 42.34 -> 41.46, map 4.54 -> 4.67. Instructions 182.3G -> 158.6G (debug), 139.3G -> 123.9G (release). Open: release `map` +29% instructions; the removed float runner's fused `a + x*y` output path needs porting.
  - Tests: flow_ir/web/shader/web_export 506 pass, flow_gpu 94 pass. flow_sop: every module passes when run on its own. Pre-existing >5 s cases: `test_parallel_exact` (x10, 4.7 s alone), `test_workspace_cook::the_other_sop_fixtures_lower_and_cook`, `optional_rest`.
  - Open: `placement.rs:33` GPU handle leak (3 vs 2 after owner close; pre-existing, was masked by the earlier :270 failure).

- 2026-11-08: D6 non-Flow pass landed (see D6 row). Tests: rays_math/input/metal* 45, rdk_* 480, runtime/editor_core/pxui* 52, rays/flow_native 58, rays_editor 64, pxui_graph test-hooks 31; all pass; gate deps 28. Found 12 test files under `crates/rays_editor/tests/` (suite_t1-3 support, suite_t6/*) that are never compiled. They need wiring or deleting.

- 2026-11-08: finish pass, editor phase. `b4746ded` `998c2dd4` `66401139` M: allocation guard now measures the real `rays` EMPTY workspace (`crates/rays_bin/src/empty.rays`); the old case is kept as `alloc_budget_bare`. Allocations/frame: empty 4,079 -> 1,698 (cap 1,705), bare 4,335 -> 1,918 (cap 1,925), basic 1,980 -> 1,521 (cap 1,525). Empty was above basic because `panel_key` rebuilt owned keys hundreds of times a frame and layout deep-cloned subtrees with three panels. The ~1,200 target is still unmet; what remains is renderer encoders ~350, chrome `format!` IDs ~313, inspector row clones ~264, scene composition ~220. `60dd0c58` qual: port typo in the `native_placement_retained_generations` sketch. `native_placement_chain_counterexamples` now passes (likely `c15b1a31`, not bisected).
  - Open: `run_studio`/`run_studio_keywords` timeouts are a Flow correctness bug. `scatter_add` gets per-point counts of 0x43100000 (the f32 bit pattern of 144.0), size 45e9; handed to the Flow agent. `placement.rs:270` has an unexpected first-warm readback of 3,145,728 B. Native suite with `RAYS_T_TIMEOUT=600`: 322 cases, 285 pass, 37 fail. The 37 are timeouts (sopcat_* x8, run_lowering, run_ops, run_unbound_panels, isogrid_deferred, shattered_studio, workspace_packed_capture_state, native_scrub_gpu, native_orrery_frames) and assertions (run_loop_copies, scene_tree_c_slider, threading, images_native_*, native_isogrid_viewport_recipe_0-7, native_chain_*, native_holder_frames, marks_scene_picture_digests). The 8 sampled failures are unchanged with the editor commits reverted. Whether they predate the plan still needs triage against `11988707`. Shared `target/` served stale lru/flow_syntax/mem_sys objects twice (B0 symptom).

- 2026-11-08: finish pass started (goal: close the whole artifact plan). Phase 1 in parallel: editor (empty-frame allocation gap, `placement.rs:206` counterexample unwrap, studio 5 s timeouts) and Flow (D1 last prepends + SPIR-V order audit, D4 matched bench, `test_fma` E_NONFINITE, five baseline flow_sop failures, remesh golden cause). Phase 2: D6 naming (87 capitalised modules, 52 `t` types, ~80 files). Phase 3: slow qualification, artifact status update.

- 2026-11-08: D5 checkpoint requested by the user: consolidated `Float_array`/`Vec2_array`/`Vec3_array`/`Vec4_array` into `Value::Floats(usize, Arc<Vec<f64>>)`. Migrated all production and test callers, including shader/raster, CPU interpreters, GPU placeholders/readback, SOP attributes/topology/images, path tracing, web export and editor qualification. Collapsed shared storage branches and width-dependent constructors. Equality and identity checks still require the same width, even when two views share one Arc. Type-specific key prefixes and web key hashes remain unchanged. Width validation precedes division/indexing and finite-value memo lookup. No dependencies, profiles, crate boundaries or goldens changed. Updated the current marks contract; historical OCaml and CSR storage names are intentionally retained.
  - New value regression checks widths 1–4, original exact key strings, row counts/types/display/init/get, Arc identity, width-sensitive equality, invalid widths 0/5/usize::MAX and the unchanged 40-byte `Value` size. Focused value run: four pass / 3.400 s including rebuild, slowest new regression 0.032 s. Existing composition regression now checks actual `core_exec::phys_eq` and rejects cross-width concatenation over shared storage. Temporary removal of its width guard fails (6.883 s including rebuild, case 0.024 s); exact bytes restored in `finally`. Composition suite: 25 pass / 24.503 s including rebuild, slowest exact-copy case 0.986 s.
  - `cargo t -p flow_syntax -p flow_check -p flow_eval`: 182 pass / 16.177 s, slowest threaded determinism 3.408 s. `cargo t -p flow_ir`: 340 pass / 11.510 s, slowest text-array part2 2.985 s. `cargo t -p flow_shader -p flow_graph -p web_export`: 82 pass / 18.238 s, slowest particles export 0.672 s; existing goldens match. `cargo t -p flow_web`: 120 pass and the known FMA failure / 13.897 s, slowest all-byte text 1.523 s. Restoring all changed Rust files to `b52600c7` reproduces the same `E_NONFINITE` FMA failure (17.847 s including rebuild, case 0.111 s); working bytes restored in `finally`.
  - `cargo t -p flow_sop FILTER`: `attribute_kernel::tests` 12 pass / 27.750 s including rebuild (slowest selected numeric writes 0.140 s); `test_alloc_budget` six / 0.482 s (million-text 0.130 s); `topology_kernel_impl::` 13 / 0.508 s (counts/edges 0.052 s); `test_geometry_arrays` 38 / 2.541 s (multiblock planes 1.272 s). These exercise shared buffers, width changes, signed zero/subnormal bits, finite/type/count errors, cancellation and exact one/eight-domain equality. Existing allocation limits unchanged.
  - Native `cargo t -p flow_gpu --test gpu_run --test gpu_resident --test gpu_uint32`: 20 pass / 10.859 s, slowest resident scenarios 0.818 s. `cargo t -p flow_gpu --test gpu_run_lifetime --test gpu_image_sink_native --test gpu_pathtrace --test gpu_sampling --test gpu_trace --test gpu_sop_mesh`: 62 pass / 20.692 s, slowest pathtrace resolve 0.565 s. `cargo t -p flow_native`: 20 / 6.505 s, slowest recursive frame allocation 0.973 s. `cargo t -p rays_pathtracer --lib --test gpu_lisp`: five / 7.290 s, slowest world 0.971 s. Metal exercised on this Mac; Windows Vulkan execution remains unqualified.
  - `cargo t -p rays_editor --features test-hooks --test gpu_editor alloc_budget`: two pass / 17.651 s including rebuild, slowest basic 0.387 s. Counts unchanged: basic 1,980 / 182,989 B, empty 4,335 / 184,728 B per frame; no caps raised. `cargo check --workspace --all-targets`: pass / 3.812 s, existing warnings. Main validation batches: 902 passing cases, one independently reproduced baseline failure. Full slow qualification and the other known failures above remain open.
  - Before/after debug `cargo run -p flow_ir --example bench_core -- '1,024'`: digest `c4bbf006c82014c7` matches reference and both domain counts. Sampled min/median ms: one domain 0.0146/0.0183 -> 0.0129/0.0185; eight 0.0104/0.0132 -> 0.0110/0.0146. No speed gain claim; D4's full before/after benchmark remains open. Baseline `cargo check -p flow_syntax -p flow_eval -p flow_ir` wall 0.165 s; post-stage warm check passes. Live artifact rechecked: v5, source SHA-256 matches the mirror, unchanged. `git diff --check`: clean. Evidence `/tmp/rays-d5-*.log`, `/tmp/rays-d5-{before,validation,native-validation}.json`; prior agent logs are archived under `.subagent-logs/`.

- 2026-11-08: C11 closure: removed the separate local identity-cache queue. `rays::idlist` is an 18-line adapter over generic `renderer::side_lru`; the shared `lru::Cache` owns linked ordering, capacity and memory charges. The core stores integer IDs only; local `Rc` keys/values and custom equality stay on their owning thread. Borrowed predicates support comparator-only keys without unsafe Send or new dependencies. Identity hits promote exactly, peek does not, and mutable keys that become equivalent resolve newest first. Value-keyed tables keep their existing second-chance policy. Identity caches retain their previous no-pressure-eviction behavior (`Shed::Never`); constant-hash scans have a documented O(n) ceiling. Drop settles local releases and ledger charges; take transfers ownership without releasing. Iteration keeps its original ordering; all-kept retention avoids an intermediate allocation. No dependencies, profiles, crate boundaries or goldens changed.
  - One focused regression covers non-Send mutable-key equality, MRU/peek/head/iteration/filter/clear, replacement/eviction, equivalent mutable keys, negative-byte clamping, zero capacity, release callbacks, Drop ledger balance, value-key second chances, pinning and take. Baseline fails on a leaked 9-byte Drop charge (2.272 s including rebuild, case 0.049 s). Temporary promotion disable fails (`find did not promote the hit`, 4.597 s / case 0.025 s); restored in `finally`. The newly equivalent-key case first failed (2.456 s / case 0.030 s), then passed after fixing the shared lookup (5.618 s / case 0.051 s).
  - Final `cargo t -p lru -p renderer -p rays`: 109 pass / 32.724 s including rebuild, 28.081 s summed test time; slowest native runtime render 3.025 s, scene2 argument 2.513 s, offscreen lifecycle 2.112 s. `cargo t -p rays_editor`: 64 pass / 8.884 s including rebuild, slowest token scrubs 0.304 s. Native `cargo t -p rays_editor --features test-hooks --test gpu_editor alloc_budget`: two pass / 5.060 s including rebuild, slowest basic 0.383 s. Basic measures 1,980 allocations / 182,989 B per frame, down 22 / 1,954 B; caps lowered to 1,988 / 184,000 B. Empty stays 4,335 / 184,728 B; its cap and unresolved ~1,200 target remain unchanged. `cargo check --workspace --all-targets`: pass / 4.878 s.
  - Matched `662ddcd9` versus working Rust source with `BUILDTIME_LEAF=input BUILDTIME_MIDDLE=editor_core BUILDTIME_TOP=rays_editor BUILDTIME_EDITOR_TOUCH=rays_editor cargo run -p buildtime -- measure`; exact working bytes restored in `finally`, shared target/profiles preserved. Before/after totals 51.641 / 44.469 s. Editor warm-check/touch-check/touch-build/build+test (s): 5.542 / 0.439 / 24.831 / 38.116 -> 4.171 / 0.402 / 21.027 / 33.459. Both stage swaps require consumer rebuilds and exceed the existing 15 s top build/build+test budgets; the command exits zero despite these `OVER` reports. No speed gain claimed and no limits raised. W1 editor-to-bin warm/touch checks: 3.015 / 0.563 -> 2.972 / 0.592; leaf, middle and W0 budgets pass. A subsequent warm working-tree measure passes every existing budget: editor warm/touch-check/touch-build/build+test 2.255 / 0.465 / 1.361 / 6.006 s, W1 warm/touch 2.919 / 0.533 s. `git diff --check`: clean. Evidence `/tmp/rays-c11-buildtime{.json,-before.log,-after.log,-warm.log}`.
  - Affected cook and executor guards before the final equivalent-key correction: `cargo t -p flow_sop test_alloc_budget`: six pass / 18.548 s including rebuild, slowest million-text 0.110 s. `cargo t -p flow_ir test_core_operators`: nine pass / 9.713 s including rebuild, slowest text array part2 2.926 s. The final correction only changes local identity-cache matching; value-key and executor behavior is unchanged. Live artifact rechecked: still v5, SHA-256 matches the repository mirror. Evidence `/tmp/rays-c11-{before,after,recency-negative,equivalent-red,equivalent-green,final-core,final-editor,final-alloc,final-workspace,cook-guards,core-operators}.log`, `/tmp/rays-c11-final.json`.

- 2026-11-08: C10 closure: renamed the already-merged `ogpu` package/library/directory to `ogpu_core`, migrated Cargo dependencies and Rust callers, and updated dependency rules plus the current backend ownership documentation. No compatibility alias or new facade. All 22 core Rust source files are byte-identical across the directory rename; lockfile changes are exactly the package-name substitution. Native labels, shader sources, goldens, dependencies and profiles unchanged. The 28 dependency violations match the previous gate output exactly after normalizing the renamed package.
  - Baseline `cargo check -p ogpu`: 0.07 s; `cargo t -p ogpu`: 16 pass / 2.672 s including build, slowest surface/discard 0.031 s. First `cargo check -p ogpu_core`: 0.97 s; warm 0.07 s. Warm renamed core test run: 16 pass / 0.255 s, slowest 0.030 s. No build-speed improvement claim.
  - `cargo t -p ogpu_core -p ogpu_mock -p gate`: 28 pass / 4.762 s including rebuild, slowest dependency rule 0.033 s. `cargo check --workspace --all-targets`: pass / 10.478 s. `cargo t -p ogpu_metal -p ogpu_vulkan`: 14 Metal cases pass / 6.300 s including rebuild, slowest pipeline 0.313 s; Vulkan compiles on this Mac, native Windows execution remains unqualified. `cargo t -p rays -p rays_editor`: 101 pass / 27.007 s including the rename rebuild, slowest native sketch 1.147 s, GPU primitives 0.513 s. No checks exceed the existing per-case budget.
  - Native `alloc_budget`: two pass / 12.109 s including feature rebuild, slowest basic 0.368 s; counts/bytes unchanged at basic 2,002 / 184,943 B and empty 4,335 / 184,728 B. Caps unchanged. `cargo run -p gate -- deps`: same 28 existing violations. `git diff --check`: clean. Evidence `/tmp/rays-c10-{check-before,tests-before,core-tests,workspace-check,backends,consumers,alloc,deps,check-warm,tests-warm}.log`, `/tmp/rays-c10-results.json`. Live artifact remains v5 with matching SHA-256. C11, remaining D work, allocation gap and qualification remain open.

- 2026-11-08: C6/M3 closure: the native sketch lifecycle updates its stored model through `&mut dyn Any`, returning `None` for an in-place update or `Some(model)` for a completed replacement. The model never leaves storage while callbacks run. Functional replacements retain their previous state until success; an in-place callback's mutations remain available to the crash dump on panic. Editor loop/frame, viewport, source/reload/save, recovery and host edits now mutate state directly. Reduction metadata no longer carries a model. Before/after comparisons retain the document handle, prior pane level/map mode, held keys and pane keys. Borrowed document context handles carry navigation; settled pieces are passed directly for surface picking. Only explicit external Editor snapshots require COW, and close makes one terminal recovery snapshot. No dependency, profile, crate boundary or golden changes.
  - `rays::sketch::tests::model_updates_retain_state_for_crash_dump`: in-place buffer identity, successful replacement, failed replacement, partial in-place mutation and a real crash-report directory/model file all pass; focused run 6.403 s including rebuild, case 0.048 s. New original-document view regression verifies a carried reference resolves in the original document even when its graph is absent from the preview; pass / 6.532 s including rebuild, case 0.199 s.
  - `cargo t -p rays -p rays_editor`: 101 cases pass / 10.322 s, including 64 editor units, rays API/GPU/resource cases and the native sketch window; slowest native sketch 1.059 s, GPU primitives 0.531 s, editor paste 0.298 s. Pointer guards in both allocation cases check a 100 KB preferences buffer survives update and after-present. Temporary model copy in `update_with` fails (`update copied the editor model`), 7.042 s including rebuild / case 0.292 s; exact working source restored in `finally`.
  - Native `cargo t -p rays_editor --features test-hooks --test gpu_editor FILTER`: allocation caps lowered to basic 2,010 / 186,000 B and empty 4,350 / 186,000 B; tightened two-case run passes / 4.693 s including rebuild, slowest basic 0.364 s. Measured basic 2,046 / 193,118 B -> 2,002 / 184,943 B; empty 4,395 / 194,704 B -> 4,335 / 184,728 B. The harness now reuses its owned Editor as the native loop does instead of retaining extra test-only snapshots.
  - Native scenario filters: text 11 pass / 13.495 s including rebuild, slowest w9 2.030 s; undo two / 2.803 s, slowest 2.152 s; carry three / 2.057 s, slowest 1.109 s; panes / 4.954 s, case 4.754 s; atomic / 0.594 s, case 0.338 s; focus-follow / 0.752 s, case 0.539 s; source/reload/autosave 11 / 3.830 s, slowest editor flow 0.630 s; world commands/drag two / 0.836 s, slowest drag 0.312 s; editor camera/settings/workspace five / 2.655 s, slowest walk 0.689 s; fast cameras / 0.589 s, case 0.369 s; drawing six / 2.632 s, slowest parity 0.908 s; picking four / 2.775 s, slowest sunflower 0.746 s. Studio main and keywords still hit unchanged five-second limits; tail passes at 1.679 s (filter total 12.049 s). No failures hidden or deadlines raised.
  - Three-frame native editor CLI run on a temporary workspace, with isolated preferences/crash output: exit 0 / 8.274 s including 6.05 s build; its unique recovery directory cleaned afterward. `cargo check --workspace --all-targets`: pass / 14.965 s, existing warnings. `cargo run -p gate -- deps`: same 28 pre-existing violations, no new dependencies. `git diff --check`: clean.
  - Matched committed `0f1033a6` versus working source with `BUILDTIME_LEAF=input BUILDTIME_MIDDLE=editor_core BUILDTIME_TOP=rays_editor BUILDTIME_EDITOR_TOUCH=rays_editor cargo run -p buildtime -- measure`; exact Rust bytes restored in `finally`, shared target/profiles preserved. Both exit 0; all budgets pass. Editor warm/touch-check/touch-build/build+test (s): 2.650 / 0.412 / 5.487 / 13.459 -> 2.859 / 0.398 / 5.171 / 13.191. W1 editor-to-bin warm/touch: 2.530/0.511 -> 2.567/0.502. Total 22.881 -> 22.470 s; no build-speed improvement claim. Evidence `/tmp/rays-c6-loop-buildtime{.json,-before.log,-after.log}`. Live artifact still v5 and SHA-256 matches the mirror. C10/C11/D1/D4/D5/D6, the empty allocation target and full qualification remain open.

- 2026-11-08: C6 mutable reducer stage: `reduce`, text intents/installation, navigation, level changes, scope/view synchronization and carry updates take `&mut t`. The owned frame wrapper returns that same model plus reduction metadata. Text validation/merge produces a document candidate, installed only after success; finished scrubs have no candidate and add no history entry. Source navigation restores only the pane and view caches when a nested path is missing; active/inactive text targets clear only after navigation succeeds. Host APIs retain their existing snapshot boundary. C6 remains partial: the sketch loop's retained snapshots and carry-hover original-document lookup still copy models.
  - New regression covers document/graph/binding applies, two-frame merged scrubs and release, valid source/definition navigation, and rollback for missing node/graph paths. A 100 KB preferences buffer keeps its identity throughout. Temporarily reintroduced a model copy in `install`: guard failed (`apply copied the model`), 4.059 s including rebuild / case 0.150 s; source restored in `finally`. Green regression 3.503 s including build / case 0.226 s. `cargo t -p rays_editor`: 63 pass / 4.304 s including rebuild, slowest token scrubs 0.299 s. `cargo check -p rays_editor`: 1.21 s, existing 16 warnings only.
  - Native `cargo t -p rays_editor --features test-hooks --test gpu_editor FILTER`: allocation budgets two pass / 7.770 s including feature rebuild, slowest basic 0.392 s; basic 2,046 allocs / 193,118 B and empty 4,395 / 194,704 B unchanged, caps unchanged. Text 11 pass / 8.327 s, slowest w9 2.105 s; undo two pass / 2.773 s, slowest 2.140 s; carry three pass / 2.072 s, slowest 1.122 s; panels / 4.669 s, case 4.464 s; atomic frame / 0.550 s, case 0.343 s; focus-follow / 0.776 s, case 0.543 s; studio tail / 1.856 s, case 1.650 s. Existing full-studio timeouts remain open; limits unchanged.
  - Matched `ff6df2d1` versus working source with `BUILDTIME_LEAF=input BUILDTIME_MIDDLE=editor_core BUILDTIME_TOP=rays_editor BUILDTIME_EDITOR_TOUCH=rays_editor cargo run -p buildtime -- measure`. Working Rust bytes restored in `finally`; shared target/profiles preserved. Both exit 0; all budgets pass, after run includes all 63 editor cases and consumer checks. Editor warm/touch-check/touch-build/build+test (s): 2.746 / 0.421 / 1.960 / 8.043 -> 2.139 / 0.395 / 2.121 / 8.875. W1 editor-to-bin warm/touch: 2.312/0.503 -> 2.506/0.509. Total 17.872 -> 17.401 s; cache/test variance, no build-speed improvement claim. Evidence `/tmp/rays-c6-mutable-buildtime{.json,-before.log,-after.log}`. `git diff --check` clean; no dependency, boundary, profile or golden changes. Live artifact still v5, SHA-256 matches the mirror.

- 2026-11-08: C6 borrowed frame views: replaced `Cow<editor_model::t>` and frame-local whole-model snapshots with a copyable borrowed `ModelView`. Pane readers retain the chosen graph state through nested helpers; common document/cook resources stay borrowed. Inactive panes synchronize in the owned model before drawing, then restore the active key/leaf and deferred-selection list. List, text, inspector, spreadsheet and graph readers all use those views; transient workspace/focus/selection/status fields are references or scalars. The sketch lifecycle/crash snapshot is unchanged. C6 remains partial: mutable reducer APIs and the loop's retained model snapshots still need work.
  - Regression verifies alternate-pane identity/selection/name, independent focus and hidden-panel overrides, and reuse of a model carrying a 1 MB preferences field. Warmed UI frame peak is 79,311 B (cap 100,000). Temporarily reintroduced `value.clone()` at frame entry: the guard failed on pointer identity (negative control), then restored exact source bytes. `cargo t -p rays_editor`: 62 pass / 4.315 s including rebuild, slowest paste 0.311 s. Focused reducer suite: seven pass / 7.841 s including initial rebuild, slowest gestures/prompts 0.203 s. `cargo check -p rays_bin`: 0.81 s; existing warnings only. `git diff --check`: clean.
  - Native checks through `cargo t -p rays_editor --features test-hooks --test gpu_editor FILTER`: panel instances/tied inspectors (`run_panels`) pass / 4.649 s, case 4.451 s; studio tail / 1.863 s, case 1.639 s; text 11 pass / 13.218 s including feature rebuild, slowest w9 2.051 s; undo two pass / 2.765 s, slowest 2.131 s; carry three pass / 2.090 s, slowest 1.131 s; atomic frame / 0.578 s, case 0.326 s; focus-follow / 0.729 s, case 0.522 s. Full studio filter still hits the same unchanged five-second limits for main/keywords (12.069 s total, tail passes); remains open.
  - Allocation guards: basic 2,074 -> 2,046 allocations / 195,516 -> 193,118 B per frame; empty 4,518 -> 4,395 / 199,236 -> 194,704. Caps lowered to 2,055 / 194,000 and 4,410 / 196,000; tightened rerun two pass / 5.853 s including rebuild, slowest basic 0.400 s. No cap raised.
  - Compared committed HEAD `96ef0ea0` and working source with `BUILDTIME_LEAF=input BUILDTIME_MIDDLE=editor_core BUILDTIME_TOP=rays_editor BUILDTIME_EDITOR_TOUCH=rays_editor cargo run -p buildtime -- measure`, restoring all working Rust files in `finally` before the after run; shared target and profiles preserved. Both exit 0, all budgets pass. Editor warm-check / touch-check / touch-build / build+test (s): 3.621 / 0.406 / 4.193 / 12.418 -> 3.681 / 0.422 / 4.932 / 13.507. W1 editor-to-bin warm/touch: 3.771/0.516 -> 2.566/0.519. Overall 25.804 -> 23.460 s, influenced by warm cache and test variance; no build-speed improvement claim. Evidence `/tmp/rays-c6-view-buildtime{.json,-before.log,-after.log}`. No dependencies, boundaries, profiles or goldens changed. Artifact remains v5 with matching SHA-256.

- 2026-11-08: C6 borrowed header/pane lookup stage: `pane_at` borrows an existing pane, constructs just a new pane when absent, and copies only a stale pane when its level needs resolution. Shared `new_pane` keeps initialization consistent with activation. Panel host classification, keyword start updates, graph-panel summaries and header titles use those borrowed pane fields. Graph-name/route/level labels accept explicit pane state rather than fabricating a whole editor model. Remaining model-copy sites in `editor_frame` and list/text/inspector views are still open; C6 is not complete.
  - Route allocation guard: before 102,630 peak B (red / 3.450 s including build, case 0.151 s); after 108 B, cap 512 (green / 3.845 s, case 0.164 s). The same check verifies active-pane pointer identity and repairs a missing inactive level without mutating stored pane state. `cargo t -p rays_editor`: 61 pass / 1.707 s, slowest paste 0.307 s. `cargo check -p rays_bin`: 1.21 s; no new warnings.
  - Native filters through `cargo t -p rays_editor --features test-hooks --test gpu_editor`: `run_panels` passes / 4.953 s, case 4.615 s (tied inspectors, repeated graph-panel instances, focus, text/list views); `run_idle_follow` / 1.042 s, case 0.820 s; `run_studio_tail` / 1.860 s, case 1.657 s; `text_` 11 pass / 8.359 s, slowest w9 2.075 s. Allocation guards pass / 4.312 s, slowest basic 0.552 s; basic 2,074 / 195,516 B and empty 4,518 / 199,236 B remain within the tightened caps. An eager graph-path lookup briefly tripped the empty guard (4,630 allocations); removed that unused lookup when no authored shell exists, preserving the original lazy lookup behavior. No cap raised. `git diff --check`: clean; no dependencies, profiles or goldens changed.

- 2026-11-08: C6 reducer ownership stage: the main reducer consumes its model rather than cloning it at the old/new boundary. Old comparisons keep document/cook handles, navigation facts and a vector of `(object, network identity, root)`; added a scalar network identity accessor to avoid cloning lowered networks. Framing and carry query the model before their fields are replaced. Previous-model retention in `rays::sketch` is unchanged. The existing edit/undo regression now asserts the preferences buffer survives reduction: red before (3.912 s / case 0.167 s), green after (6.054 s including rebuild / 0.198 s). C6 remains partial: mutable reducer APIs and borrowed inactive panes still required.
  - `cargo check -p rays_editor`: 2.30 s; `cargo check -p rays_bin`: 0.60 s. `cargo t -p rays_editor`: 60 pass / 1.721 s, slowest paste 0.305 s. Native `run_undo`: 2 pass / 3.001 s, slowest undo/input 2.243 s; `carry`: 3 pass / 2.051 s, slowest material carry 1.111 s; `text_`: 11 pass / 8.286 s, slowest w9 2.018 s. All through `cargo t -p rays_editor --features test-hooks --test gpu_editor FILTER`.
  - Native allocation guards: basic 2,089 -> 2,074 allocs, 197,521 -> 195,516 B/frame; empty 4,549 -> 4,518, 201,596 -> 199,236 B/frame. Lowered caps to 2,085 / 197,000 and 4,530 / 200,000. Tightened guard rerun: 2 pass / 4.385 s including rebuild, slowest basic 0.364 s. `git diff --check`: clean. No dependencies, boundaries, profiles or goldens changed. Live artifact rechecked: v5 SHA-256 still matches the mirror.

- 2026-11-08: C6 text ownership stage: `one_intent` returns its owned model for view-state and no-op intents; `apply_text_at` swaps the inactive text state into that model, applies intents once, then restores the active state. Removed its two whole-model copies and the shared `with_text` copy. Source/definition navigation and successful text installation still have separate clone paths; the main reducer/borrowed pane work remains open. A pointer-identity regression failed before the change (3.444 s, case 0.171 s), then passed (3.285 s including build, case 0.155 s); it also checks inactive drafts, rejected applies and discards preserve the active draft and document.
  - `cargo check -p rays_editor`: 0.45 s; `cargo check -p rays_bin`: 1.36 s. `cargo t -p rays_editor`: 60 pass / 1.597 s, slowest paste 0.296 s. `cargo t -p rays_editor --features test-hooks --test gpu_editor text_`: 11 pass / 8.288 s, slowest editor_w9 2.016 s. Native allocation checks: two pass / 7.006 s including rebuild, slowest basic 0.417 s; counts/bytes unchanged (2,089 / 197,521 basic; 4,549 / 201,596 empty), caps unchanged.
  - Native studio subset: tail passes, main/keywords hit the five-second limit (12.112 s total). Temporarily restored only `editor_text.rs` from HEAD `3e4826d8` in the shared target, restored working bytes in `finally`, and reproduced the same two timeouts (17.122 s including rebuild; tail 1.837 s). No new failure attributed to this stage; both timeout scenarios remain open. `git diff --check`: clean. Future frame-ownership edits must preserve the sketch lifecycle's crash report guarantee (`rays::sketch` retains the previous model when a callback panics); simply taking it out of a `RefCell<Option<_>>` would break that guarantee.

- 2026-11-08: C6 qualification baseline: `cargo t -p rays_editor test_editor_reduce`: five pass / 15.816 s including rebuild, slowest idle/undo 0.195 s. `cargo t -p rays_editor --features test-hooks alloc_budget` first failed to compile (six stale C1/C17 API uses in `native_qualification/placement.rs`). Corrected test ownership of backend calls versus placement evidence. The native harness then compiled and both allocation checks passed / 8.227 s, slowest basic 0.405 s: basic 2,089 allocs / 197,521 B per frame; empty 4,549 / 201,596. Limits unchanged. Calibration passes / 0.589 s, case 0.197 s. Chain counterexamples fail / 0.930 s, case 0.532 s: profile lookup at placement.rs:206 finds no non-input entry; needs separate diagnosis. No assertion weakened or test disabled. `git diff --check`: clean.

- 2026-11-08: C3/M17 closure: reused `support::Windows` for three proximity paths, three curve-join paths, numeric attribute mapping and primitive ordering. Removed owned result-slot helpers; variable-length text/intersection outputs keep one-pass range buffers with explicit ceilings, avoiding duplicate callback/intersection work. Added exact one/eight-domain checks for mixed feature kinds, nonuniform primitive sizes, curve plans and mapper values/errors; existing intersection parallel/provenance cases pass. Shared mapper guard failed before refactor (72 integer allocations over 60); tightened after: float 6,297,928 -> 3,155,152 peak B, 166 -> 105 allocations (caps 3,160,000 / 110); int 1,574,880 -> 1,051,712 B, 72 -> 35 (caps 1,055,000 / 40). Single sampled mapper times are noisy (float 1.626 -> 1.126 ms, int 0.349 -> 0.335 ms); no speed claim.
  - `cargo check -p rdk_geom -p rdk_ops`: pass / 0.86 s. Focused mapper/ordering: 8.513 / 0.173 s; proximity/curve windows: 14.368 s including rebuild, slowest 0.024 s. `cargo t -p rdk_core -p rdk_geom -p rdk_ops -p rdk_boolean -p sop`: 533 pass / 24.185 s, slowest interpolation parallel-exactness 1.039 s. `cargo t -p flow_sop test_alloc_budget -- --nocapture`: six pass / 15.477 s including rebuild, slowest text 0.117 s. Existing limits unchanged.
  - Required before/after build measurements: `BUILDTIME_LEAF=rdk_core BUILDTIME_MIDDLE=rdk_geom BUILDTIME_TOP=rdk_ops BUILDTIME_EDITOR_TOUCH=rdk_ops cargo run -p buildtime -- measure`. Warm check / touch check / touch build, seconds: core 0.066/0.199/0.364 -> 0.100/0.188/0.335; geom 0.243/0.220/0.452 -> 0.247/0.220/0.457; ops 0.546/0.468/1.089 -> 0.521/0.453/1.110. Geom build+test 2.227 -> 1.952; ops 6.469 -> 7.451 (new regressions + run variance); W1 editor warm/touch 3.547/1.828 -> 3.039/1.813. All tool budgets pass; no compile-speed improvement claimed.
  - Seven debug `rays check` samples after one warmup (basic, gpu_sea, million_marks): median 0.602402 -> 0.588328 s, `/tmp/rays-c3-perf.json`. `cargo run -p flow_ir --example bench_core -- '1,024'`: min/median and digest recorded in `/tmp/rays-c3-bench-{before,after}.log`; both domain counts match reference and digest `c4bbf006c82014c7`. Full D4 qualification remains open. `git diff --check`: clean. No dependencies, boundaries, goldens or profiles changed.

- 2026-11-08: I6 export closure. Baseline normal `cargo t -p web_export`: 17 pass / 5.137 s. Both formerly ignored cases now terminate (0.166 / 0.496 s); only stale goldens failed. Added a runtime assertion on the actual region `countLimit` passed to `invoke`; it failed with 4611686018427387903 before the fix, then passed with native i64 MAX (2.729 s including build, case 0.289 s). `cargo t -p flow_web test_region_arguments`: 23 pass / 4.301 s, slowest all-byte text 1.896 s. `cargo t -p web_export`: 19 pass / 1.534 s, no ignored exports, slowest particles 0.591 s. Written golden review: five ordinary program fixtures change only `countLimit`; verified by replacing the new literal back and comparing original bytes/digest. Formerly ignored fixtures include already-intended i63 removal, unsigned i64 division, D1 register/scan-result order and updated source spans. The image module content is unchanged; its filename/reference now match MD5. Every generated `data.bin` matches the prior bytes or stored digest. `git diff --check`: clean.

- 2026-11-08: I1/I2 closure: confirmed the builtin abs regression before changing the implementation (red test 12.518 s including build); green boundary test 5.936 s, case 0.051 s. `cargo t -p flow_web builtin_integer_boundaries_match_native_i64`: Node present, pass / 3.663 s, case 0.237 s. `cargo t -p flow_check -p flow_eval -p flow_syntax`: 181 pass / 16.485 s, slowest deterministic threaded evaluation 2.809 s. `cargo t -p flow_ir test_core_operators`: 9 pass / 3.149 s, slowest text-array part 2.923 s. `cargo check -p rays_bin`: pass / 7.38 s with pre-existing warnings. `git diff --check`: clean. Live artifact rechecked: still v5, unchanged SHA-256. I6 count metadata, C3/M17, C6/M3 and remaining C/D work remain open.

- 2026-11-08: I4 closure: native helper/ID/frame boundary checks: `cargo t -p rdk_geom -p rdk_ops -p sop native_i64`, 6 pass / 19.367 s including rebuild, slowest 0.029 s. `cargo t -p rdk_core -p rdk_geom -p rdk_ops -p rdk_boolean -p sop`: 530 pass / 6.632 s, slowest `attribute_interpolate::tests::parallel_exactness_at_scale` 1.127 s. `cargo t -p flow_sop test_alloc_budget -- --nocapture`: 5 pass / 3.565 s, slowest 0.115 s, limits unchanged. Rebuilt HEAD `3b847b50` temporarily in the shared target and restored all working changes in `finally`, then rebuilt the native-boundary stage: `cargo build -p rays_bin` wall 21.467 / 9.782 s (incremental cache states differ; no compile-speed claim). Seven measured runs after one warmup of `rays check` on basic, gpu_sea and million_marks: median 0.620483 / 0.589764 s; no observed debug runtime regression. Evidence `/tmp/rays-i4-perf.json`; `git diff --check` clean. Further audit reopened C3/M17 and I1/I2/I6 with concrete source evidence rather than leaving false completion labels.

- 2026-11-08: C4 closure: `cargo check -p rdk_ops -p rdk_boolean` 1.77 s; focused ownership check 3.946 s and instance payload check 12.531 s including rebuilds. `cargo t -p rdk_core -p rdk_ops -p rdk_boolean -p sop`: 469 pass / 10.741 s, slowest `attribute_interpolate::tests::parallel_exactness_at_scale` 1.111 s. `cargo t -p flow_sop test_alloc_budget -- --nocapture`: 5 pass / 13.315 s, slowest 0.135 s, existing limits unchanged. `git diff --check`: clean. Remaining production `Storage::Float2` matches classify kinds/lengths, decode specialised numerical kernels/constraints/UV inputs, or distinguish normal vectors; the duplicate gather/rebuild algorithms use the shared helpers. No boundaries or goldens changed.

- 2026-11-08: completed stages `fee04de1` (C4 subdivision mapping), `23d4bb15` (missing allocation guards). C4 remains partial while equivalent maps elsewhere remain. I4 reopened after finding native cardinality helpers with 63-bit ceilings; hash masks are intentionally frozen. Full objective remains active.

- 2026-11-08: Wave M missing guards added. Walker: 49,180 allocations / 82,472 peak bytes, limits 49,200 / 83,000; aliased store: 12 / 164,634, limits 16 / 165,000. Subdivide peak tightened from 29,700,000 to 29,150,000 B after C4 (measured 29,101,694 B). `cargo t -p flow_sop test_alloc_budget -- --nocapture`: 5 pass / 6.445 s, slowest 0.112 s. Negative-control mutation was restored before final run; `core_exec.rs` has no diff. `git diff --check`: clean.

- 2026-11-08: C4 follow-up: `cargo t -p rdk_ops subdivide` baseline 14 pass / 22.065 s; after change 15 pass / 11.228 s (slowest 0.031 s). `cargo t -p rdk_ops -p sop`: 319 pass / 13.512 s (slowest `attribute_interpolate::tests::parallel_exactness_at_scale`, 1.199 s). `cargo t -p flow_sop test_alloc_budget`: 3 pass / 40.894 s including 40.07 s rebuild (slowest 0.136 s); existing bounds unchanged. `cargo check -p rdk_ops -p sop -p flow_sop`: pass / 18.47 s including build-lock wait, pre-existing warnings only. `git diff --check`: clean. No goldens or boundaries changed.

- 2026-11-08: resumed from `c5bf57b6`; previous turn classified as progress (committed D2 evidence). Re-read live artifact v5 and verified its source matches the mirror. Corrected contradictory C/D summary and final-state claim. Next: complete C4 shared attribute remapping, then C6 and remaining D work; preserve all open qualification requirements.

- 2026-11-08: **pause** — `9dae5ebd` M1 squash; M14 WIP reverted; **~28/63**; artifact unchanged; next **I**, **M14**, **B5** `rdk`.
- 2026-11-08: M1 landed (site cache); `cargo check -p rays_bin` ~7.4s.
- 2026-11-08: `684b3d58` **I prep** — `rays` drops unused `ocaml_int`; M1 site-cache rolled back; `m1-retry` worker queued.
- 2026-11-08: `ba113180` B1 `ScopeStack`; `ed08c91b` M18 partial; Wave B **6/6** (B5 partial); M **11/21**; ~27/63 IDs.
- 2026-11-08: `6f75543d` B5 `rays_editor`; review3; `rdk` only `sop`/`flow_sop`.
- 2026-11-08: `69d08a8a` M6; `66136b7f` M8; `7b374363` B5 `flow_gpu`; M wave 10/21; ~24/63 IDs.
- 2026-11-08: B5 — `flow_gpu` dropped `rdk` dev-dep (`rdk_core`/`rdk_ops` in gpu_* tests); `cargo check -p flow_gpu -p flow_native`.
- 2026-11-08: B5 — `flow_native` no longer depends on `rdk` facade (`rdk_core` direct); `cargo check -p flow_native -p rays_editor`.
- 2026-11-08: B5 — `flow_sop_catalog` (catalog/manifest/port/network/encoded_parameters); `editor_document` direct dep; `cargo check -p flow_sop_catalog -p flow_sop -p editor_document -p rays_bin`.
- 2026-11-08: `b5be8904` M12; `dca77b6f` M7; `35df934e` B5 rdk in flow_native; `20c1dfb3` M9; `49ec71f3` M11.
- 2026-11-08: review2 — 16 done, 2 partial, 45 pending (PROGRESS ↔ git aligned).
- 2026-11-08: `f6ca915b` M20, `bbe7f6ae` M2, `96fab43d` B2; `cargo check -p rays_bin` ~23s after profile shift.
- 2026-11-08: B0 — invalidation checklist + script audit in PROGRESS §B0; warm pair flow_check 0.15 s / 0.07 s; log `.subagent-logs/b0.log`.
- 2026-11-08: **Wave A closed** `3a293e9b` A6; plan audit appended; m10/b0 workers queued.
- 2026-11-08: omp subagents landed `0cad884b` A5, `7a63b07b`/`30623d6e` A2, `da178e93` A7; Wave A 7/8.
- 2026-11-08: omp subagents A2/A5/A6/A7-geom launched; logs under `.subagent-logs/`.
- 2026-11-08: Resume execution; A1 verified on `dev`.
- 2026-11-08: `3c86b2a1` — gpu_suite sharding removed.
- 2026-11-08: `e10bff22` / `10a405ae` / `1a8ee31b` — A2 edge_transport; A3 rdk array limits.
- 2026-11-08: `5a6e7269` — A2 sop::support dead helpers (−297 lines); `cargo t -p sop` 52 tests.
- 2026-11-08: `1a8ee31b` / `a60fcced` — A3 closed for rdk; A4 MD5 merge.
- 2026-11-08: `09ce8574` — A6 partial dep hygiene.
- 2026-11-08: A6 — `editor_core` trims `rays`/`runtime_resources`; `cargo check -p editor_core -p editor_document -p rays_editor -p rays_bin`; `cargo t -p editor_core` 7 tests ~0.25s.
- 2026-11-08: Wave A runbook: gpu_suite sharding (`3c86b2a1`).
- 2026-11-08: `ec5c1b1b` — rdk re-exports / sop support / flow_ir region_snapshot / flow_check op prune.
- 2026-11-08: `d01750db` — pxui_shell `cap_y_paint` dedupe.
- 2026-11-08: verify — `cargo t -p flow_ir` 337 tests; bg `flow_sop` 133 tests (~66s).
- 2026-11-08: A5 dead window/live_delay; A7 triangle closest-point unify; A8 term_arena/param/renderer trim; A2 sop + rdk re-export fix; `cargo t -p rdk_geom planar` pass.
- 2026-11-08: A5 `test-hooks` gates `Editor::Private`; B0 warm incremental smoke on `flow_check`.
- 2026-11-08: A5 closed — pxui_graph `Inspect` hooks file + `Reduce`/`with_core` gated; `cargo check -p pxui_graph -p rays_editor` with/without `test-hooks`.
- 2026-11-08: `f980cbf9` A7 ocaml_trim; `cargo t -p sop` 52 pass (~20s).
- 2026-11-08: A7 `rdk_core` trim helper; B4 buildtime W1 editor chain; `cargo check -p buildtime`.
- 2026-11-08: A7 surface_index → `point_triangle_barycentric_into`; `rdk_core::ocaml_trim`; rdk_ops + sop OCaml blank-name checks; `cargo t -p rdk_geom surface` 9 pass (~7s).
- 2026-11-08: `30623d6e` A2 closed — dead enums/`NumericKind` in `da178e93`; PROGRESS + `cargo t -p sop` 52 pass (~19s).


## Second simplification Z10 — clone review

This is the active second plan's Z10 ledger; the completed port plan above is unchanged. Baseline source: `ba056fc5910945e6b198be5bcbbf69dd6fab5557` (2026-11-10). Counts are literal `.clone()` occurrences in committed `crates/*/src/**/*.rs`, including inline tests, comments and strings; integration tests, examples and build scripts are excluded. They inventory review scope, not redundant operations. Total: **11,571 across 50 crates**. Reproduce with `git grep -o -F '.clone()' ba056fc5 -- crates`, grouping only those source paths. The artifact's original estimate was 11,707; this different-time count proves no runtime savings.

The exact command `cargo clippy --workspace --lib --message-format=json -- -W clippy::redundant_clone` initially exits 101 in 5.301 s at the existing default-denied `mem::Report::to_string` wrapper, before emitting any redundant-clone diagnostics. Commit `cebc8739` removes that wrapper; standard ToString delegates to the same Display implementation. All callers were traced. Existing `cargo t -p mem owner_bytes_drive_report_and_formatting` passes: 2.50 s wall / 2.135 s runner / 0.022 s case.

The second exact Clippy run exits 101 in 7.915 s at the existing approximate-PI literal in `sop_topology.rs`, after 59 redundant-clone diagnostics. A separate inventory-only command, `cargo clippy --workspace --lib --message-format=json -- --cap-lints warn -W clippy::redundant_clone`, exits 0 in 17.068 s and emits **151 candidates across 20 crates**. This is an inventory, not clean-lint qualification; no repository lint policy changes. Raw results and exact commands are in `/tmp/rays-z10-clippy-{before,after-report,audit}.{jsonl,stderr,result.json}`. Clippy's library build excludes conditional test code, while the literal source census includes inline tests. Zero candidates does not prove that every remaining ownership choice is appropriate.

| Crate | Baseline clones | Current clones | Clippy candidates | Ownership review |
|---|---:|---:|---:|---|
| cfmt | 0 | 0 | 0 | Pending review |
| editor_core | 47 | 47 | 0 | Pending review |
| editor_document | 642 | 642 | 4 | Pending review |
| flow_check | 538 | 538 | 9 | Pending review |
| flow_eval | 646 | 646 | 1 | Pending review |
| flow_gpu | 293 | 293 | 6 | Pending review |
| flow_graph | 389 | 389 | 2 | Pending review |
| flow_ir | 1472 | 1472 | 7 | Pending review |
| flow_native | 311 | 311 | 2 | Pending review |
| flow_shader | 119 | 119 | 0 | Pending review |
| flow_sop | 1348 | 1348 | 16 | Pending review |
| flow_sop_catalog | 147 | 147 | 0 | Pending review |
| flow_syntax | 141 | 141 | 0 | Pending review |
| flow_web | 71 | 71 | 0 | Pending review |
| input | 26 | 26 | 0 | Pending review |
| lru | 52 | 52 | 0 | Pending review |
| mem | 61 | 61 | 0 | Pending review |
| mem_sys | 0 | 0 | 0 | Pending review |
| metal_sys | 3 | 3 | 0 | Pending review |
| native_layer_token_sys | 3 | 3 | 0 | Pending review |
| ogpu_core | 113 | 113 | 0 | Pending review |
| ogpu_metal | 137 | 137 | 1 | Pending review |
| ogpu_mock | 23 | 23 | 0 | Pending review |
| ogpu_vulkan | 102 | 102 | 0 | Pending review |
| parallel | 2 | 2 | 0 | Pending review |
| param | 115 | 115 | 0 | Pending review |
| pxui | 100 | 100 | 0 | Pending review |
| pxui_graph | 357 | 357 | 3 | Pending review |
| pxui_shell | 112 | 112 | 3 | Pending review |
| rays | 325 | 325 | 3 | Pending review |
| rays_bin | 1 | 1 | 0 | Pending review |
| rays_diagnostic | 2 | 2 | 0 | Pending review |
| rays_editor | 1734 | 1734 | 55 | Pending review |
| rays_math | 1 | 1 | 0 | Pending review |
| rays_math_sys | 0 | 0 | 0 | Pending review |
| rays_pathtracer | 235 | 235 | 5 | Pending review |
| rdk_boolean | 93 | 93 | 1 | Pending review |
| rdk_core | 135 | 135 | 0 | Pending review |
| rdk_geom | 116 | 116 | 3 | Pending review |
| rdk_ops | 642 | 642 | 14 | Pending review |
| rdk_rays | 61 | 61 | 0 | Pending review |
| renderer | 289 | 289 | 3 | Pending review |
| runtime | 2 | 2 | 0 | Pending review |
| runtime_resources | 32 | 32 | 0 | Pending review |
| scene_command | 22 | 22 | 0 | Pending review |
| sdl3 | 8 | 8 | 0 | Pending review |
| sdl3_sys | 1 | 1 | 0 | Pending review |
| sop | 428 | 428 | 9 | Pending review |
| vulkan_sys | 18 | 18 | 0 | Pending review |
| web_export | 56 | 56 | 4 | Pending review |

Z10 remains ACTIVE. Crate-by-crate ownership review, necessary fixes and unchanged Z0 runtime guards are required before completion. Changes owned by other active stages are not counted as Z10 savings.
