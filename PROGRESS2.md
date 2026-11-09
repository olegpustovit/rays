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
| X | 6 | 6 | 0 | 0 |
| B | 8 | 5 | 1 | 2 |
| A | 4 | 3 | 1 | 0 |
| D | 12 | 1 | 0 | 11 |
| Z | 11 | 0 | 0 | 11 |
| R | 10 | 0 | 0 | 10 |
| G | 7 | 0 | 0 | 7 |
| **All** | **58** | **15** | **2** | **41** |

## Items

| ID | Wave | Status | Commit / evidence | Regression check |
|----|------|--------|-------------------|------------------|
| X1 | X | done | `098310fa` — `packed_ops::{float_min,float_max,ldexp,binary_apply,unary_apply}`; walker `min`/`max`/`array/min`/`array/max`; `min_max_signed_zero_walker_matches_packed`, `float_min_max_signed_zeros` | New case: walker equals packed for (min -0. 0.) and (max 0. -0.) |
| X2 | X | done | `dd1656a0` `d915cccd` — settings `int_of_string_opt`; resource caps → `i64::MAX`/`MIN` (sentinels like `PROVENANCE_SENTINEL` kept) | test_store: a 2^62 literal decodes as Int |
| X3 | X | done | `63d191b0` — `Build` → `R<node::T>`; `per_context` uses `?`; `authored_body_errors_cook_lazily` | A context-dependent failure reports its real code |
| X4 | X | done | `ee88ca64` — `param::number::int_scrub_delta` shared with `pxui::scrubbed` | Both surfaces give the same step for the same drag |
| X5 | X | done | `cc2c7bcc` — `literal_fields`; `operator_rows_split_vec2_and_vec4_literals` | test_editor_inspect covers vec2 and vec4 operator rows |
| X6 | X | done | `f97f3c30` `93bcb897` — shared MappedHost; native release contract + spec corrected | core/mock 21 passed 2.50s; Metal transfer_and_compute 0.279s; released view invalid, successor re-reads |
| B1 | B | done | `aaaaf431` — `lisp_sop` always on; `#[doc(hidden)]` hooks; features removed | After cargo t -p rays_editor, cargo build -p rays_bin -v shows flow_sop as Fresh |
| B2 | B | done | `c2d1c582` — workspace study tests in `editor_document`; no `flow_sop` dev-dep; `cargo t -p flow_sop` ~81s green | Same cases in cargo t -p flow_sop plus -p editor_document |
| B3 | B | done | `2be4b822` — `check_cli` + `tests/e2e` in `web_export`; `rays_bin` re-exports | cargo t -p web_export --test e2e; gate deps |
| B4 | B | active | `1e8b775e` shares window runner; reopened: one harness=false binary/crate + fixture dedup still required | Case inventory before/after; focused native checks |
| B5 | B | done | `403aaac4` — `[lib] test = false` on 11 libs + `print_shader`; gate rejects `#[test]` in those libs | Gate shape rule |
| B6 | B | done | `c10ccba4` — removed `[profile.e2e|gpu|slow]`; trimmed gate phantom crates; nextest comments | cargo t -p rays-t, -p gate; gate deps |
| B7 | B | pending | — | cargo-timings critical path; gate deps |
| B8 | B | pending | — | cargo t -p rays_editor |
| A1 | A | done | `8dea3150` — metadata/unused wrappers/bodies/shims deleted, ABI names normalized; net −821 lines | ogpu_metal check 11.60s; Metal suite 33/34, corrected X6 case passes (93bcb897) |
| A2 | A | active | Agent tracing external rdk_ops/rays APIs before narrowing visibility | cargo check --workspace --all-targets |
| A3 | A | done | `5c9c9f82` — removed C/build.rs/cc; bounded powi scaling, exact subnormal rounding retained | cargo t -p cfmt -p param -p flow_syntax: 61 passed, 6.31s; slowest 0.150s |
| A4 | A | done | `a013679f` — dead stubs/wrappers/Exp removed; derived Change/Op equality; choice callback removed | all-targets check 6.31s; param/pxui_graph 46 passed; flow_ir/flow_web 465 passed, 30.88s, slowest 1.872s |
| D1 | D | pending | — | rdk_ops dissolve, rdk_boolean |
| D2 | D | pending | — | rdk_ops fuse, rdk_boolean |
| D3 | D | pending | — | rdk_geom exact, implicit_point |
| D4 | D | pending | — | rdk_geom |
| D5 | D | pending | — | rdk_ops poly_, transform |
| D6 | D | done | `7ef8286f` — `support::{resolve,check_positive,grain}`; rdk_core/ops/geom/boolean | grain-filtered rdk_ops + `cargo t -p rdk_core` |
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
- **Wave X** complete (`f97f3c30`); **B1** `aaaaf431`.
- **B4** shared `window.rs` harness via `#[path]` (metal `settle` + per-crate test binaries + `test_program` still open).
- **ScoutD**: D-wave slice schedule below (≤2 agents, distinct primaries).
- **WaveX5**: `literal_fields` in `editor_inspect.rs`; regression `operator_rows_split_vec2_and_vec4_literals`.

## D-wave parallel map

Batch = building-agent slot when two distinct primary crates run together. `editor_core`, `rdk_core`, `rdk_boolean`: one writer per crate at a time. Slices serial top-to-bottom.

| ID | Primary crate | Batch | Notes |
|----|---------------|-------|-------|
| D1 | `rdk_core` | 1 | Slice 3. `support::DisjointSet`; callers in `rdk_ops` / `rdk_boolean`. |
| D2 | `rdk_ops`, `rdk_boolean` | 2 | Slice 4. After D7. Stable sort; boolean golden regen. |
| D3 | `rdk_geom` | 1 | Slice 1. Unified exact engine. |
| D4 | `rays_math` | 1 | Slice 2. Shared float/NaN helpers. |
| D5 | `rdk_core` | 1 | Slice 4. After D1. Methods on geometry/group/attribute. |
| D6 | `rdk_core` | 1 | Slice 5. After D5. `support::grain`. |
| D7 | `rdk_boolean` | 2 | Slice 3. After D3. `bounds3_index`; parallel-equality. |
| D8 | `flow_ir` | 2 | Slice 1. `PRIMITIVES` + `Instruction`; flow_web goldens. |
| D9 | `flow_sop` | 2 | Slice 2. `decl::native`; reads `rdk_ops::Params`. |
| D10 | `rays_editor`, `editor_core` | 2 | Slice 5. After X2 caps. |
| D11 | `editor_core`, `rays` | 1 | Slice 6. After D10. `Frame` → `input`. |
| D12 | `editor_core`, `pxui` | 1 | Slice 7. After D10/D11. `flow_syntax` settings + pxui text. |

| Slice | Batch 1 | Batch 2 |
|------:|---------|---------|
| 1 | D3 `rdk_geom` | D8 `flow_ir` |
| 2 | D4 `rays_math` | D9 `flow_sop` |
| 3 | D1 `rdk_core` | D7 `rdk_boolean` |
| 4 | D5 `rdk_core` | D2 `rdk_ops` + `rdk_boolean` |
| 5 | D6 `rdk_core` | D10 `rays_editor` + `editor_core` |
| 6–7 | D11 → D12 | serial on `editor_core` |

### Continuation — evidence audit and wave A

- Previous turn classified **progress**: B4 shared runner commit exists. Revalidated clean worktree and artifact v15 before edits.
- Reopened **B4**: the prior done marker covered only the shared runner. Consolidated binaries and shared `test_support` are still required by the original artifact.
- **A1** `8dea3150`: metadata and actual unused native wrappers removed; retained both wrappers used internally by metal_sys. `cargo check -p ogpu_metal` passed (11.60s). Native run 33/34 passed (12.53s); stale X6 assertion remained visible.
- **X6 follow-up** `93bcb897`: native test now checks the shared released-view contract, new view identity, invalidation and repeated cached lookup. Metal case passed (16.76s including lock, case 0.279s); core/mock 21/21 passed (2.50s, slowest 0.031s). Spec corrected in the same commit.
- **A3** `5c9c9f82`: C fallback/build/cc removed. Regression covers hex, subnormal, `1.`, `.5`, `+inf`, underscores, signed zero/extreme exponents and all normal binary exponents. Consumer check: 61/61 passed, 6.31s, slowest 0.150s.
- **A4** implementation ready. Workspace all-targets check passed (6.31s), param/pxui_graph 46/46 passed (50.62s including changed-chain rebuild; case time 6.732s, slowest `bench_scope_big` 0.910s). New `change_equality` guard measures 0 allocations. Additional Flow regression checks running before commit.
- Artifact tracker audit found invalid odd-segment DB paths (`plan/items/<id>`) and a nonexistent `exists()` method. Repair uses valid `plan/<id>` documents and one collection query, with the published snapshot rendered immediately. DB writes are verified by CLI reads.
- **A4 complete** `a013679f`: Flow regression 465/465 passed in 30.88s (slowest `flow_web::test_region_arguments::all_byte_text_argument` 1.872s); no assertion/deadline changes.
