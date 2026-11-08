# Port simplification execution

Plan source: [artifact 2beyggs594gc](https://artifacts.microapps.space/a/2beyggs594gc) (read-only tracker; progress here only).

Repo mirror: `docs/history/port-simplification/index.html`

**Order:** A ∥ B ∥ M ∥ I, then C, then D. One commit per bullet in Wave A runbook where practical.

## Summary

| Wave | Done | Active | Pending |
|------|------|--------|---------|
| A | 8/8 | — | — |
| B | 5/6 | B1 | 1 |
| M | 6/21 | — | 15 |
| I | 0/1 | — | 1 |
| C | 0/21 | — | 21 |
| D | 0/6 | — | 6 |

## Items

| ID | Status | Commit / evidence |
|----|--------|-------------------|
| A1 | done | `6802a325` — delete `rays_collections` / `rays_hash`; gate bans generics |
| A2 | done | `30623d6e` — `e10bff22` edge_transport; `ec5c1b1b`/`992093ce` rdk re-exports; `5a6e7269`/`da178e93` support (−631 vs OCaml root); `NumericKind` test dropped |
| A3 | done | `1a8ee31b` — rdk `MAX_ARRAY_LENGTH` trimmed; `boolean_materialization` packing kept |
| A4 | done | `a60fcced` — `md5_hex` in `rays_math::hash`; 5 duplicate modules removed |
| A5 | done | `0cad884b` `ec5c1b1b` `fa967abe` `992093ce` — `region_snapshot` `#[cfg(test)]`; dead `window_to_values`/`live_delay()` removed; `Editor::Private`, `Reduce`, `with_core` + `pxui_graph::Inspect` test accessors behind `test-hooks` (prod keeps `box_of`/`grabbed`/`footer_paths`); `rays_editor::test-hooks` enables `pxui_graph/test-hooks` |
| A6 | done | `09ce8574` UI/rays_bin; **this session** — `editor_core` drops `rays` + `runtime_resources` (local frame + atomic write; viewport in `rays_editor`) |
| A7 | done | `992093ce` triangle → `point_grid`; `626a1f70` Filename.concat; `f980cbf9`/`8093ff64` ocaml trim; **this session** `rdk_core::ocaml_trim`, rdk_ops/sop blank-name sweep |
| A8 | done | op/term_arena/param/renderer trims; `Native::runtime_of` kept (in use) |
| B0 | done | §B0 checklist + warm pair `flow_check` 0.15s/0.07s; `catalog.rs` marker |
| B3 | done | `5fdb5fb3` — `gpu_editor` + `tests/support`; 7× `gpu_suite_t*` + qual → 2 test links; `cargo t -p rays_editor run_lowering` ~2.9s |
| B4 | done | W1 `flow_ir` touch → `rays_bin` check in `tools/buildtime` (env overrides) |
| B1 | active | `533489ee` term_arena; `e16247cd` `BUILTIN_DELEGATE`/`GPU_STAGE_ONLY` + `EMPTY_CHECK`; ponytail workspace `PathId` scope stack; op.rs ~138× `Arc::new` (was 141) |
| B2 | done | `96fab43d` — dev `opt-level=0` for rays_editor, pxui_graph, editor_document, flow_graph, flow_native |
| B5 | done (partial) | `flow_sop_catalog` crate; `editor_document` → catalog/port/network/manifest; `flow_sop` re-exports + `generate()` wrapper; **`flow_native` dropped `rdk` → `rdk_core`**; **rdk delete deferred** (sop/flow_sop/flow_gpu/rays_editor) |
| M2 | done | `bbe7f6ae` — `editor_graph_ref`; `panel_key` / layouts borrow graph name |
| M10 | done | `c425eefa` — detach alias slots before store; `cargo t -p flow_ir store` 3 pass |
| M20 | done | `f6ca915b` — remove `mem_sys` TEMP alloc sampler |
| M4 | done | `0f3804c3` — `Profile::execution_count` / `probe_run::report_count`; `sync_scope` defers profile clone until after `!moved && !fresh`; `cargo check -p rays_editor`; `cargo t -p flow_native probe_run` 3 pass |
| M9 | done | `20c1dfb3` — `panels::at_ref`; shell `graph_path` / `panel_key` + `beside` borrow; `cargo check -p editor_core -p rays_editor` |
| M11 | done | `49ec71f3` — lazy `field()` error `form`; `Keys` pattern name once; `cargo t -p flow_eval test_workspace_eval` 2 pass |
| M1,M3,M5–M8,M10,M12–M19,M21 | pending | |
| I | pending | |
| C1–C21 | pending | |
| D1–D6 | pending | |

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

Read-only review in `.subagent-logs/review.log`: Wave A runbook **5/6 bullets done**, bullet 3 partial (`pxui` `ocaml_int` → Wave **I**); A4 MD5 done, optional `guarded`/`ocaml_min` dedup still scattered. **~8/63** plan IDs closed (A1–A8 + B4); M/I/C/D not started. A6 closed at `3a293e9b` after review snapshot.

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

- 2026-11-08: B5 — `flow_native` no longer depends on `rdk` facade (`rdk_core` direct); `cargo check -p flow_native -p rays_editor`.
- 2026-11-08: B5 — `flow_sop_catalog` (catalog/manifest/port/network/encoded_parameters); `editor_document` direct dep; `cargo check -p flow_sop_catalog -p flow_sop -p editor_document -p rays_bin`.
- 2026-11-08: `49ec71f3` M11; `0f3804c3` M4; `e16247cd` B1 op delegates; B5 + `cargo t -p flow_sop catalog` 2 pass.
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
