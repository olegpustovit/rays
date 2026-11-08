# Port simplification execution

Plan source: [artifact 2beyggs594gc](https://artifacts.microapps.space/a/2beyggs594gc) (read-only tracker; progress here only).

Repo mirror: `docs/history/port-simplification/index.html`

**Order:** A ∥ B ∥ M ∥ I, then C, then D. One commit per bullet in Wave A runbook where practical.

**Current state (2026-11-08, resumed):** implementation and qualification remain open. The previous stage reported a clean `cargo check --workspace --all-targets`; that is not completion evidence for the full plan. Artifact v5 source SHA-256 matches the repository mirror (`bd1c9fe2…`). Artifact unchanged. Crates 67 -> 51.

## Summary

| Wave | Done | Partial | Not done |
|------|------|---------|----------|
| A | A1-A8 | - | - |
| B | B0-B5 | - | - |
| M | M1-M2, M4-M20 (M3 via C6), M21 | M3 | - |
| I | I1-I3, I5-I6 | I4 (remaining non-hash count guards) | - |
| C | C1-C3, C5, C7-C9, C12-C21 | C4, C6, C10, C11 | - |
| D | D2-D3 | D1, D4 (benchmark evidence missing) | D5-D6 |

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
| M17 | done | via C3 commits (`25726291` ... `141a4afe`), `bd2a526f`, `8afb4996` |
| M18 | done | `ed08c91b` `21da1965` i32 scratch; `INT_FLOATS` charged to ledger in `8bf0c0d0` |
| M19 | done | `13abe74c` `rays_math::text_array::TextArray` (offsets+bytes); 1M rows = 8 allocs; text literals as `Arc<[u8]>` skipped (hundreds of sites) |
| M21 | done, deviates | `1ccd60fc` `Rc<[u8]>` with `unique()` copy-on-write; plan premise "nothing mutates" was false (set_pixel, Image::replace, readback) |
| M3 | partial | via C6 |
| I1-I3 | done | `a06df67a` spec; `c9c2b12a` i64 core; `74942f50` `ocaml_int` deleted |
| I4 | partial | `edc27be2` migrated ~12 negate/abs/square guards. Resumed audit found non-hash cardinality helpers still capped at 2^62−1 in `subdivide_refine`, `curve_subdivide`, `triangulate2d`, plus other count/ID guards. Audit and migrate to checked native i64 arithmetic; preserve frozen hash masks and key-packing validation. |
| I5 | done | `b946570e` `859aff62` MSL/SPIR-V + 42 fixtures |
| I6 | done | `4bcb5ba4` WGSL/JS/web, 185 web fixtures; `flow_image`/`flow_particles_gpu` web_export fixtures not regenerated (tests `#[ignore]`) |
| I fallout | done | `49737e72` scatter `sc_rf`; `2c7ebd25` sort/prelude/operators; `e2e2ed18` gpu_run carried fixture |
| C1 | done | `dc2ee86b` `ef645352` `e6ae19d5` `2612f106` `b4b90916`; Vulkan only type-checked on macOS; source lines grew, gain is allocations/closure types |
| C2 | done | `33e163b0` `faa8090e` |
| C3 | done | ~30 commits `25726291`..`141a4afe`; only 2 test-only mutex slots remain |
| C4 | partial; named gathers shared | `a08c927c`, `fee04de1`: polygon/curve subdivision use the central plane mapper and selector; three private helpers removed. The four named gathers share `topology_remap::select_storage`. Broader follow-up still needed: duplicated float remapping/defaulted gather in `triangulate2d`, tuple repetition in `instance_copy`, and other storage-kind matches need classification/conversion. New `remapping_keeps_all_storage_kinds_and_rows` checks all eight kinds, empty CSR rows, opaque text and two grains. |
| C5 | done | `a12b1efb` |
| C6 | partial | `4cfcff4a` `7d7241ae`: panel_map plain struct, Cow panes, Rc core; reducers do NOT take `&mut t`, `reduce` still clones once; basic 2509 -> 2089 allocs/frame, empty 5732 -> 4549 |
| C7, C8 | done | `8e6e0e51`, `69fe4afd` |
| C9 | done | `e0afebff` |
| C10 | partial | `d87c7c95` `625ffd0f` (crate kept named `ogpu`, not `ogpu_core`) `5b15cb29` `04b5ae16` `9bf5f9be`; `ocaml_int` deleted outright (I3) rather than moved to `int63` |
| C11 | partial | `51ebdf92` `rays::table` -> `renderer::side_lru`; `rays::idlist` not unified (identity-equality keys, cannot hash) |
| C12, C13 | done | `34ea546d`; `b9816d3a` (7M floats identical) |
| C14 | done | `f3b38af3` |
| C15 | done | `b268ff32` `support::Choice<T>` (net -100, plan -400..500) |
| C16 | done | `8afb4996` |
| C17 | done | `84662760` `Gpu::Evidence` on `Config` (not on `Backend` trait: ~16 implementors) |
| C18, C20 | done | `b5aead5f`; `01b90841` `fold.rs` + `lru::Memo` (+163 net lines, not -140) |
| C19 | done | `dd441536` PartialEq derived (Hash impossible: f64 in ogpu types) |
| C21 | done | `7e9376dc` clippy::all allow removed; ~30 test-target lints remain |
| D1 | partial | `959d5dae` `b72ab30d` `b4f74ecf` goldens `9a17f5d7` `1a5de4fe` `8fc3feb4`; 56 prepends removed; some incidental-order spots in spirv.rs left |
| D2 | done | `37691759` .. `2e224106` + `a0809e01` (5 stages): rdk errors `{operation, code: Code, message}`, typed cancellation |
| D3 | done | `4e8e7193` one WGSL printer, goldens byte-identical |
| D4 | implemented; qualification open | `8bf0c0d0` `18180e84` packed.rs one CPU engine (-1560/+? lines in diff; now 4714 lines); `1455271c` bench_core example |
| D5 | not started | `Value` four float-array variants -> `Floats(width, ..)` (90 sites, 23 files) |
| D6 | not started | OCaml module names (`Gpu/Profile/Cost`, `t`) - ~67 files, deferred |

## Known failures / open risks

- Resumed audit next work: finish C4 remaining equivalent storage maps; complete C6/M3 ownership and borrowed panes; migrate remaining I4 non-hash count guards; then C10/C11/D1/D4 audit, D5/D6 and full qualification. `fee04de1` and `23d4bb15` are completed stages, not completion of the full artifact plan.

- `cargo t -p flow_sop` on the D2 tree: 700 tests, 696 pass; the 4 failures are the baseline ones below (`lend_session_tests` confirmed pre-existing by LowerClean at `11988707`). `cargo t -p rdk_core/rdk_geom/rdk_boolean/rdk_ops/sop`: 523 pass; `rdk_rays` 10 pass. Other full suites were **not** run on final HEAD under load (load avg 50-450). Focused runs per item passed (see worker reports in item rows).
- Pre-existing at baseline `11988707` (bisected by LowerClean): `test_node_facts::declared_preserved_topology`, `test_keyword_ports::keyword_manifest_defaults_projection_checked_gestures` (E_SHADOW), `test_settle::in_flight_device_writes_settle_at_a_cooks_end`, `test_projection::snapshots_of_the_scene_fixtures`, `lend_session_tests::escaped_zone_nodes_own_only_their_cook_context`.
- `flow_ir test_static_map::sop_deferral`: root cause `4ed6baf8` (constructor deferral); test rewritten (`a3e8c6df`); `StaticMap.deferred` now effectively dead.
- Unconfirmed after D4: `flow_web test_fma::checked_arithmetic_regions_match_exact_binary64` (E_NONFINITE) and `flow_ir test_core_operators::text_arrays_part_*` near the 5 s limit; `rays_editor` full suite and `pxui_graph --features test-hooks` unverified after C6.
- Remesh macOS golden switched to the Windows hash constant (`3107f153`); cause (libm) unverified.
- Per-cook guards (a) walker, (b) aliased 4096-element store, (c) round trip, (d) subdivide peak and 1M-text now exist in `flow_sop/src/test_alloc_budget.rs`. The walker uses `Host::force_reference`, bypassing compiled execution. The store guard was verified with a temporary negative control: disabling alias release produced 8,204 allocations and failed the guard; restored code uses 12. All five pass. Per-frame guards `alloc_budget_basic`/`alloc_budget_empty` exist (`e74d3f1d`; empty-editor limit 4650, plan said ~1200 - plan figure does not match full-layout editor).
- Not measured: B-wave compile-time gains, bench_core before/after for D4 (PackedMerge reported no regression sign only from one noisy run).
- `gate deps`: 28 pre-existing violations, none new.
- Throwaway worktrees `/tmp/rays-m13b`, `/tmp/rays-base`, `/tmp/rays-snap` can be deleted.

## Subagents (omp)

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

Cursor `Task` subagents unavailable in this harness; workers are `omp launch -p` (pids in shell history).

## Plan audit (2026-11-08)

Read-only reviews: `.subagent-logs/review.log` (Wave A snapshot); `.subagent-logs/review2.log` (63-ID tally); `.subagent-logs/review3.log` (2026-11-08) — **~27/63** with evidence, **2 partial** (B5, M18); next impact **I**, **M1**, **M14**, **C1**, **C3**; `rdk` facade remains for `sop`/`flow_sop` only.

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
