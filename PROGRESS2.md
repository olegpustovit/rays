# Second simplification pass — execution ledger

Artifact: [Rays Second Simplification](https://artifacts.microapps.space/a/ap6ev04y67bb)
Baseline audit: `dev` @ `e2e4ab02` (2026-11-09). Follows the [first plan](https://artifacts.microapps.space/a/2beyggs594gc) (63/63 done).

## Rules

- One commit per item ID; message names the ID.
- OCaml compatibility is gone; regenerate goldens in the same commit when output changes on purpose.
- Lisp semantics, determinism, sequential/parallel equality, macOS/Windows parity stay invariant (`specification/`, `AGENTS.md`).
- Close an item only after its regression check; never raise deadlines or weaken assertions.

## Wave totals

| Wave | Items | Done | Active | Pending |
|------|------:|-----:|-------:|--------:|
| X | 6 | 1 | 0 | 5 |
| B | 8 | 0 | 0 | 8 |
| A | 4 | 0 | 0 | 4 |
| D | 12 | 0 | 0 | 12 |
| Z | 11 | 0 | 0 | 11 |
| R | 10 | 0 | 0 | 10 |
| G | 7 | 0 | 0 | 7 |
| **All** | **58** | **1** | **0** | **57** |

## Items

| ID | Wave | Status | Commit / evidence | Regression check |
|----|------|--------|-------------------|------------------|
| X1 | X | done | `753036dd` — `packed_ops::{float_min,float_max,ldexp,binary_apply,unary_apply}`; walker `min`/`max`/`array/min`/`array/max`; `min_max_signed_zero_walker_matches_packed`, `float_min_max_signed_zeros` | New case: walker equals packed for (min -0. 0.) and (max 0. -0.) |
| X2 | X | pending | — | test_store: a 2^62 literal decodes as Int |
| X3 | X | pending | — | A context-dependent failure reports its real code |
| X4 | X | pending | — | Both surfaces give the same step for the same drag |
| X5 | X | pending | — | test_editor_inspect covers vec2 and vec4 operator rows |
| X6 | X | pending | — | ogpu_mock: after a release, the view reads back again |
| B1 | B | pending | — | After cargo t -p rays_editor, cargo build -p rays_bin -v shows flow_sop as Fresh |
| B2 | B | pending | — | Same cases in cargo t -p flow_sop plus -p editor_document |
| B3 | B | pending | — | cargo t -p web_export --test e2e; gate deps |
| B4 | B | pending | — | Each crate lists the same case names before and after |
| B5 | B | pending | — | Gate shape rule |
| B6 | B | pending | — | cargo t -p rays-t, -p gate; gate deps |
| B7 | B | pending | — | cargo-timings critical path; gate deps |
| B8 | B | pending | — | cargo t -p rays_editor |
| A1 | A | pending | — | cargo check -p ogpu_metal |
| A2 | A | pending | — | cargo check --workspace --all-targets |
| A3 | A | pending | — | A cfmt test over hex, subnormal, 1., .5, +inf and underscore literals |
| A4 | A | pending | — | cargo check --workspace --all-targets |
| D1 | D | pending | — | rdk_ops dissolve, rdk_boolean |
| D2 | D | pending | — | rdk_ops fuse, rdk_boolean |
| D3 | D | pending | — | rdk_geom exact, implicit_point |
| D4 | D | pending | — | rdk_geom |
| D5 | D | pending | — | rdk_ops poly_, transform |
| D6 | D | pending | — | The 17 grain-0 error tests |
| D7 | D | pending | — | rdk_boolean cells, parallel-equality signatures |
| D8 | D | pending | — | flow_ir and flow_web goldens byte-identical |
| D9 | D | pending | — | manifest golden byte-identical; sopcat_manifest |
| D10 | D | pending | — | Shares the X2 case |
| D11 | D | pending | — | editor_core, gpu_editor alloc_budget |
| D12 | D | pending | — | test_store, test_text_pane |
| Z0 | Z | pending | — | The guards themselves, with limits only ever lowered |
| Z1 | Z | pending | — | Z0 idle frames at 0 |
| Z2 | Z | pending | — | Z0; flow_ir and flow_eval suites |
| Z3 | Z | pending | — | bench_core instructions; warm rebuild ≤ 7.4 s |
| Z4 | Z | pending | — | rdk/sop suites; parallel equality; memory guard |
| Z5 | Z | pending | — | A cross-platform hash and random vector test |
| Z6 | Z | pending | — | grep -ri ocaml crates finds only docs/history references; suites green with regenerated goldens |
| Z7 | Z | pending | — | Orrery frames lower 0 times after warm-up; flow_sop run well under its cap |
| Z8 | Z | pending | — | An oversized array reports a typed error |
| Z9 | Z | pending | — | Z0 warm-cook guards on subdivide, boolean and scatter |
| Z10 | Z | pending | — | Z0; per-crate clone count tracked in PROGRESS.md |
| R1 | R | pending | — | flow_ir; flow_sop sequential/parallel equality |
| R2 | R | pending | — | bench_core digest c4bbf006c82014c7 unchanged |
| R3 | R | pending | — | Sequential/parallel equality; capture order stays sorted by string |
| R4 | R | pending | — | Library plus 49 sketches byte-identical |
| R5 | R | pending | — | flow_eval state |
| R6 | R | pending | — | rdk_core group; flow_sop parallel_exact |
| R7 | R | pending | — | owned_planes_reuse |
| R8 | R | pending | — | pxui_graph (31 cases); alloc_budget |
| R9 | R | pending | — | Lower the alloc_budget.rs limits |
| R10 | R | pending | — | test_prepared_scene3 |
| G1 | G | pending | — | Destroyed-handle and wrong-thread cases in gpu_ogpu_metal |
| G2 | G | pending | — | Move metal/tests/gpu_metal.rs into ogpu_metal |
| G3 | G | pending | — | ogpu_core/metal gpu tests; Windows ogpu_vulkan |
| G4 | G | pending | — | Destroy-in-flight cases, live_handles delta 0 |
| G5 | G | pending | — | Mock queue tests; a Metal queue case |
| G6 | G | pending | — | cargo t -p web_export |
| G7 | G | pending | — | refits() &gt; 0 over 3 frames |

## Order

1. **X1–X5** first, each with its regression; **X6** with wave G.
2. **B1, B2, B3, B5, B6** — one commit each; measure build vs test switch and flow_sop time.
3. **B4** crate by crate; case names unchanged.
4. **A1–A4**, then **B8**.
5. **D1–D12** in parallel by crate (≤2 building agents).
6. **Z0** right after B.
7. **R1–R10**, then **Z1, Z2, Z9, Z10**.
8. **Z5, Z6, Z8** with spec + golden refresh per commit.
9. **Z3, Z4, Z7** large perf changes; bench before/after.
10. **G1 → G2 → G3 → G4**, then G5, X6, G6, G7; Windows check each step.
11. **B7** after cargo-timings confirms path. **Z7** (ex-R11) has the design note on the artifact page.

## Session log

### 2026-11-09

- Opened ledger; artifact tracker seeded (58 pending); baseline `e2e4ab02`.
- Artifact [v4](https://artifacts.microapps.space/a/ap6ev04y67bb): execution progress panel, `db` capability, runbook, R11→Z7 note; publish source `artifacts/second-simplification/index.html`.
- **WaveX1** subagent + integration: X1 code complete; tests `float_min_max_signed_zeros`, `min_max_signed_zero_walker_matches_packed` pass.
- Spawned **ScoutD** for D-wave parallel map (pending).

## D-wave parallel map

| ID | Primary crate(s) | Batch | Notes |
|----|------------------|-------|-------|
| D1 | rdk_core, rdk_ops, rdk_boolean | 1 | DisjointSet; fuse/dissolve/boolean consumers |
| D2 | rdk_ops, rdk_boolean, rays_math | 2 | Sorts; boolean goldens |
| D3 | rdk_geom | 1 | exact_dyadic; lone heavy crate |
| D4 | rays_math, rdk_* | 2 | float helpers; wide grep surface |
| D5 | rdk_core, rdk_ops, sop | 1 | support helpers on geometry/group |
| D6 | rdk_ops | 2 | grain() only; many call sites |
| D7 | rdk_boolean, rdk_geom | 1 | bounds3_index handoff |
| D8 | flow_syntax, flow_ir, flow_check | 2 | PRIMITIVES table; shader goldens |
| D9 | sop, flow_sop | 1 | manifest/sopcat path |
| D10 | editor_core, rays_editor, sop, param | 2 | int parsers; shares X2 |
| D11 | editor_core, rays, input | 1 | Frame/panel types |
| D12 | editor_core, pxui, rays_editor | 2 | settings + text helpers |
