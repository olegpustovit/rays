# Port simplification execution

Plan source: [artifact 2beyggs594gc](https://artifacts.microapps.space/a/2beyggs594gc) (read-only tracker; progress here only).

Repo mirror: `docs/history/port-simplification/index.html`

**Order:** A ∥ B ∥ M ∥ I, then C, then D. One commit per bullet in Wave A runbook where practical.

## Summary

| Wave | Done | Active | Pending |
|------|------|--------|---------|
| A | 1/8 | A2,A3 | 7 |
| B | 0/6 | — | 6 |
| M | 0/21 | — | 21 |
| I | 0/1 | — | 1 |
| C | 0/21 | — | 21 |
| D | 0/6 | — | 6 |

## Items

| ID | Status | Commit / evidence |
|----|--------|-------------------|
| A1 | done | `6802a325` — delete `rays_collections` / `rays_hash`; gate bans generics |
| A2 | active | `e10bff22` partial — edge_transport + support hooks; 48 uncalled fns remain |
| A3 | active | `e10bff22` partial — bounds2_index, delaunay2, contour_triangulation |
| A4 | pending | |
| A5 | pending | |
| A6 | pending | |
| A7 | pending | |
| A8 | pending | |
| B0–B5 | pending | |
| M1–M21 | pending | |
| I | pending | |
| C1–C21 | pending | |
| D1–D6 | pending | |

## Session log

- 2026-11-08: Resume execution; A1 verified on `dev` (ahead 2).
- 2026-11-08: `3c86b2a1` Wave A gpu_suite sharding removed (renamed t1–t3).
- 2026-11-08: `e10bff22` A2 edge_transport deletion; A3 started (3 geom files).
