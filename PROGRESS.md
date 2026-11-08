# Port simplification execution

Plan source: [artifact 2beyggs594gc](https://artifacts.microapps.space/a/2beyggs594gc) (read-only tracker; progress here only).

Repo mirror: `docs/history/port-simplification/index.html`

**Order:** A ∥ B ∥ M ∥ I, then C, then D. One commit per bullet in Wave A runbook where practical.

## Summary

| Wave | Done | Active | Pending |
|------|------|--------|---------|
| A | 7/8 | A6 | — |
| B | 0/6 | B0 | 4 |
| M | 0/21 | — | 21 |
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
| A5 | active | `ec5c1b1b` — `region_snapshot` test-only; **this session** — dead `window_to_values`, `live_delay()`; `Editor::Private` behind `test-hooks` feature (+ `required-features` on GPU/window tests); pxui_graph accessors still open |
| A6 | active | `09ce8574` — unused deps on rays_bin / UI (pxui still has `ocaml_int` until I) |
| A7 | done | `992093ce` triangle → `point_grid`; `626a1f70` Filename.concat; `f980cbf9`/`8093ff64` ocaml trim; **this session** `rdk_core::ocaml_trim`, rdk_ops/sop blank-name sweep |
| A8 | done | op/term_arena/param/renderer trims; `Native::runtime_of` kept (in use) |
| B0 | active | Warm incremental OK; staleness = clean / non-dev profile / RUSTFLAGS (AGENTS.md) |
| B4 | done | W1 `flow_ir` touch → `rays_bin` check in `tools/buildtime` (env overrides) |
| B1–B3,B5 | pending | |
| M1–M21 | pending | |
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

Cursor `Task` subagents unavailable in this harness; workers are `omp launch -p` (pids in shell history).

## Session log

- 2026-11-08: omp subagents A2/A5/A6/A7-geom launched; logs under `.subagent-logs/`.
- 2026-11-08: Resume execution; A1 verified on `dev`.
- 2026-11-08: `3c86b2a1` — gpu_suite sharding removed.
- 2026-11-08: `e10bff22` / `10a405ae` / `1a8ee31b` — A2 edge_transport; A3 rdk array limits.
- 2026-11-08: `5a6e7269` — A2 sop::support dead helpers (−297 lines); `cargo t -p sop` 52 tests.
- 2026-11-08: `1a8ee31b` / `a60fcced` — A3 closed for rdk; A4 MD5 merge.
- 2026-11-08: `09ce8574` — A6 partial dep hygiene.
- 2026-11-08: Wave A runbook: gpu_suite sharding (`3c86b2a1`).
- 2026-11-08: `ec5c1b1b` — rdk re-exports / sop support / flow_ir region_snapshot / flow_check op prune.
- 2026-11-08: `d01750db` — pxui_shell `cap_y_paint` dedupe.
- 2026-11-08: verify — `cargo t -p flow_ir` 337 tests; bg `flow_sop` 133 tests (~66s).
- 2026-11-08: A5 dead window/live_delay; A7 triangle closest-point unify; A8 term_arena/param/renderer trim; A2 sop + rdk re-export fix; `cargo t -p rdk_geom planar` pass.
- 2026-11-08: A5 `test-hooks` gates `Editor::Private`; B0 warm incremental smoke on `flow_check`.
- 2026-11-08: `f980cbf9` A7 ocaml_trim; `cargo t -p sop` 52 pass (~20s).
- 2026-11-08: A7 `rdk_core` trim helper; B4 buildtime W1 editor chain; `cargo check -p buildtime`.
- 2026-11-08: A7 surface_index → `point_triangle_barycentric_into`; `rdk_core::ocaml_trim`; rdk_ops + sop OCaml blank-name checks; `cargo t -p rdk_geom surface` 9 pass (~7s).
- 2026-11-08: `30623d6e` A2 closed — dead enums/`NumericKind` in `da178e93`; PROGRESS + `cargo t -p sop` 52 pass (~19s).
