# Port simplification execution

Plan source: [artifact 2beyggs594gc](https://artifacts.microapps.space/a/2beyggs594gc) (read-only tracker; progress here only).

Repo mirror: `docs/history/port-simplification/index.html`

**Order:** A ∥ B ∥ M ∥ I, then C, then D. One commit per bullet in Wave A runbook where practical.

## Summary

| Wave | Done | Active | Pending |
|------|------|--------|---------|
| A | 5/8 | A2, A6 | A7 |
| B | 0/6 | — | 6 |
| M | 0/21 | — | 21 |
| I | 0/1 | — | 1 |
| C | 0/21 | — | 21 |
| D | 0/6 | — | 6 |

## Items

| ID | Status | Commit / evidence |
|----|--------|-------------------|
| A1 | done | `6802a325` — delete `rays_collections` / `rays_hash`; gate bans generics |
| A2 | active | `e10bff22` edge_transport; `5a6e7269` sop::support; rdk unused re-exports trimmed (pending commit) |
| A3 | done | `1a8ee31b` — rdk `MAX_ARRAY_LENGTH` trimmed; `boolean_materialization` packing kept |
| A4 | done | `a60fcced` — `md5_hex` in `rays_math::hash`; 5 duplicate modules removed |
| A5 | done | `ec5c1b1b` — `region_snapshot` test-only in `flow_ir` |
| A6 | active | `09ce8574` — unused deps on rays_bin / UI (pxui still has `ocaml_int` until I) |
| A7 | pending | |
| A8 | done | `ec5c1b1b` — drop `finder`, `Rows`, `arithmetic` lookup in `flow_check::op` |
| B0–B5 | pending | |
| M1–M21 | pending | |
| I | pending | |
| C1–C21 | pending | |
| D1–D6 | pending | |

## Session log

- 2026-11-08: Resume execution; A1 verified on `dev`.
- 2026-11-08: `3c86b2a1` — gpu_suite sharding removed.
- 2026-11-08: `e10bff22` / `10a405ae` / `1a8ee31b` — A2 edge_transport; A3 rdk array limits.
- 2026-11-08: `5a6e7269` — A2 sop::support dead helpers (−297 lines); `cargo t -p sop` 52 tests.
- 2026-11-08: `1a8ee31b` / `a60fcced` — A3 closed for rdk; A4 MD5 merge.
- 2026-11-08: `09ce8574` — A6 partial dep hygiene.
- 2026-11-08: Wave A runbook: gpu_suite sharding (`3c86b2a1`).
- 2026-11-08: rdk re-exports / sop support / flow_ir region_snapshot / flow_check op prune; `cargo t -p flow_ir` 337 tests; `cargo t -p flow_sop` 133 tests (~66s).
