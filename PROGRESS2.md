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
| B | 8 | 6 | 1 | 1 |
| A | 4 | 4 | 0 | 0 |
| D | 12 | 5 | 3 | 4 |
| Z | 11 | 0 | 1 | 10 |
| R | 10 | 0 | 0 | 10 |
| G | 7 | 0 | 0 | 7 |
| **All** | **58** | **21** | **5** | **32** |

## Items

| ID | Wave | Status | Commit / evidence | Regression check |
|----|------|--------|-------------------|------------------|
| X1 | X | done | `098310fa` — `packed_ops::{float_min,float_max,ldexp,binary_apply,unary_apply}`; walker `min`/`max`/`array/min`/`array/max`; `min_max_signed_zero_walker_matches_packed`, `float_min_max_signed_zeros` | New case: walker equals packed for (min -0. 0.) and (max 0. -0.) |
| X2 | X | done | `dd1656a0` `d915cccd` — settings `int_of_string_opt`; resource caps → `i64::MAX`/`MIN` (sentinels like `PROVENANCE_SENTINEL` kept) | test_store: a 2^62 literal decodes as Int |
| X3 | X | done | `63d191b0` — `Build` → `R<node::T>`; `per_context` uses `?`; `authored_body_errors_cook_lazily` | A context-dependent failure reports its real code |
| X4 | X | done | `ee88ca64` `3d8a746b` — shared six-point integer scrub; native gesture expectations corrected | pxui28/28 passed4.60s; actual text/inspector drag equality guard |
| X5 | X | done | `cc2c7bcc` — `literal_fields`; `operator_rows_split_vec2_and_vec4_literals` | test_editor_inspect covers vec2 and vec4 operator rows |
| X6 | X | done | `f97f3c30` `93bcb897` `82cc1ddc` — core MappedHost; Metal/spec/recycling guards corrected | core/mock21 pass; Metal release/re-read pass; central host-view Weak drops on recycling (0.194s) |
| B1 | B | done | `aaaaf431` — unconditional doc-hidden hooks; test features removed | After editor/CLI tests, cargo build -p rays_bin -v: flow_sop Fresh; build 1.92s |
| B2 | B | done | `c2d1c582` — workspace study tests in `editor_document`; no `flow_sop` dev-dep; `cargo t -p flow_sop` ~81s green | Same cases in cargo t -p flow_sop plus -p editor_document |
| B3 | B | done | `2be4b822` — CLI/e2e moved into web_export; gate repaired by ae022b58 | web_export e2e 206/206 passed (11.22s, slowest 0.878s); gate deps green 2.19s |
| B4 | B | done | `1e8b775e` `a30f6190` — shared runner, seven consolidated binaries, public shared IR fixtures; net −1054 lines | 246 case leaf names/counts unchanged; all 7 integration targets harness=false; native suites + X4/X6 follow-ups pass |
| B5 | B | done | `403aaac4` — `[lib] test = false` on 11 libs + `print_shader`; gate rejects `#[test]` in those libs | Gate shape rule |
| B6 | B | done | `c10ccba4` `ae022b58` — profiles/comments and retired policies removed; Rust edges/startup roles verified | gate deps green 2.19s; gate 7/7 (2.66s), runner 14/14 (1.11s); production boundary rejection retained |
| B7 | B | pending | — | cargo-timings critical path; gate deps |
| B8 | B | active | `3dcb2c3c` — implementation committed; 122 concrete borrowed parameters; full editor run still exceeds cap | all-targets green; smoke4/4 (1.57s), alloc guards3/3 (1.91s); full run timeout60.007s |
| A1 | A | done | `8dea3150` — metadata/unused wrappers/bodies/shims deleted, ABI names normalized; net −821 lines | ogpu_metal check 11.60s; Metal suite 33/34, corrected X6 case passes (93bcb897) |
| A2 | A | done | `1d13c774` — internal visibility + dead modules/wrappers removed; clippy enabled, net −350 lines | workspace all-targets8.53s; rays/rdk_ops307/307 (16.41s), slowest1.310s; scoped clippy1.61s |
| A3 | A | done | `5c9c9f82` — removed C/build.rs/cc; bounded powi scaling, exact subnormal rounding retained | cargo t -p cfmt -p param -p flow_syntax: 61 passed, 6.31s; slowest 0.150s |
| A4 | A | done | `a013679f` — dead stubs/wrappers/Exp removed; derived Change/Op equality; choice callback removed | all-targets check 6.31s; param/pxui_graph 46 passed; flow_ir/flow_web 465 passed, 30.88s, slowest 1.872s |
| D1 | D | pending | — | rdk_ops dissolve, rdk_boolean |
| D2 | D | pending | — | rdk_ops fuse, rdk_boolean |
| D3 | D | done | `caf6359f` — one 64-bit/u128 exact engine; arena temporaries, persistent outputs only | geom60 and Boolean74 plus frozen snapshot pass; predicates3.96x and circumcenter3.45x faster |
| D4 | D | active | Agent tracing float sort/equivalence consumers before sharing rays_math policy | rdk_geom |
| D5 | D | pending | — | rdk_ops poly_, transform |
| D6 | D | done | `7ef8286f` — `support::{resolve,check_positive,grain}`; rdk_core/ops/geom/boolean | grain-filtered rdk_ops + `cargo t -p rdk_core` |
| D7 | D | pending | — | rdk_boolean cells, parallel-equality signatures |
| D8 | D | done | `095418dc` — shared primitive table and Instruction guard methods; 91 lowering primitives unchanged | Flow573/573 pass27.59s; 114 golden files byte-identical; native Metal fixtures and loops pass |
| D9 | D | active | Agent removing all32 native Parameters adapters and36 static decl wrappers; schema/validation invariants retained | manifest golden byte-identical; sopcat_manifest |
| D10 | D | done | `3457d151` `82ba264f` — std i64/trim/round/min/max; syntax-aware literal-spelling comparison | Boundary/UI/SOP/text checks pass; spelling-focused1/1 and literal qualification1/1 pass |
| D11 | D | done | `92542f6a` — canonical input Frame/panel types; borrowed routing; ec_frame/converters deleted | All-targets check10.25s; input/core17, commands9, reducer10, layout2, alloc3 pass |
| D12 | D | active | Agent replacing custom settings Value with validated Syntax, reading config once; shared UTF8/text helpers | test_store, test_text_pane |
| Z0 | Z | active | `296c35c2` — lowered native caps to measured values; full-million-point sea frame guard added; warm cook guards pending | Existing3 and newsea guard pass; zero-allocation end state remains open |
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
| D9 | D | active | Agent removing all32 native Parameters adapters and36 static decl wrappers; schema/validation invariants retained | manifest golden byte-identical; sopcat_manifest |
| D10 | D | done | `3457d151` `82ba264f` — std i64/trim/round/min/max; syntax-aware literal-spelling comparison | Boundary/UI/SOP/text checks pass; spelling-focused1/1 and literal qualification1/1 pass |
| D11 | D | done | `92542f6a` — canonical input Frame/panel types; borrowed routing; ec_frame/converters deleted | All-targets check10.25s; input/core17, commands9, reducer10, layout2, alloc3 pass |
| D12 | D | active | Agent replacing custom settings Value with validated Syntax, reading config once; shared UTF8/text helpers | test_store, test_text_pane |

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
- **B8**: all 122 generic parameters replaced with borrowed views; concrete callers compile with `cargo check -p rays_editor --all-targets`. Full `cargo t -p rays_editor` remains red: whole-run cap 60.007s after 162 tests (no observed case failure); wall 100.77s including lock/build, slowest `run_ops` 3.046s. Remaining targets will be checked separately; B8 stays active until the required full run is green without a raised limit.
- **Dependency gate**: `cargo run -p gate -- deps` fails (2.65s). Diagnostics include missing current-crate policies (`flow_sop_catalog`, `rays_bin`), stale direct edges, and backend selection treating mock+native or GPU-free unit targets as unresolved. **B3/B6 reopened**: their original gate requirement is not currently proven. Preserve boundary assertions while auditing policy against the real architecture.
- **A2 complete** `1d13c774`: ~365 functions narrowed using actual compiler consumers; six dead parameter-only modules and obsolete wrappers deleted. Workspace all-targets check passed 8.53s; rays/rdk_ops307/307 passed16.41s, slowest1.310s. Scoped clippy passed1.61s with23 existing structural warnings; unscoped dependency `mem` default-deny lint remains visible.
- **B8 implementation committed**: all-targets compiler check green;65 unit cases passed in the full-run prefix (some output lines interleave),100 GPU cases completed in a second capped run. The full GPU target also hits60.007s;394 total editor cases are listed (325GPU,65unit,4smoke), so these prefixes do not prove full completion. Smoke4/4 passed1.57s; allocation guards3/3 passed1.91s (slowest0.468s): basic1384/157167, empty1206/81998, bare1417/96657 allocations/bytes per frame. No guard raised.
- **B4 complete** `a30f6190`: seven integration binaries replace the separate targets; normal doc-hidden `flow_ir::test_support` replaces GPU/Web/shader fixture copies. CLI before/after inventories preserve all246 case leaf names/counts; only colliding names carry their former target prefix. Added runner grouping/list-only regressions (`cargo t -p rays-t`,14 cases,3.27s).
- B4 validation: shader26 passed1.615s; renderer52 passed (20.525s case time,100.31s wall including runner queue, slowest3.430s); pathtracer24 passed14.04s; runtime8/Metal14 passed in small combined run; shared IR fixture passed4.87s (case0.082s). Flow GPU94 ran37.08s (22.120s case time):93 passed plus an original-harness-proven X6 counter failure, corrected below.
- **X6 additional native regression** `82cc1ddc`: the old driver `views`/`releases` fault counters no longer measure shared mapping ownership. Removed those dead counters; a Weak to the central host view now proves recycling releases it, while keeping materialisation and buffer-reuse limits. Focused regression passed3.12s (case0.194s).
- **X4 native regression** `3d8a746b`: original text test still expected20points→+4 from the retired five-point scrub. It now asserts the shared six-point policy (+3), including an actual text/inspector equal-step check. Full pxui28/28 passed4.60s (slowest0.212s); no production scrub behavior changed.

### Stable boundary — 17 done, 1 active, 40 pending

- **B6 follow-up complete** `ae022b58`: current Rust direct edges are explicit, retired crate rules are deleted, and backend selection follows entry-point roles. CPU metadata/code-generation targets need no device. Native product binaries require a nonoptional, normal, platform-correct backend; dev GPU/window/examples may combine mock with one native backend. Production UI/native forbidden reaches remain enforced; catalog boundaries added.
- Regression rejects missing/duplicate/optional/build-only/wrong-platform/unconditional/mock-only production startup and prohibited production UI/native dependencies. Gate tests **7/7** pass (2.66s, slowest 0.056s), runner **14/14** pass (1.11s, slowest process-group timeout 0.548s). **`cargo run -p gate -- deps` is green** (2.19s).
- **B3 reclosed** with current evidence: `cargo t -p web_export --test e2e`, **206/206** pass (11.22s; slowest `sketches_sop_gallery` 0.878s), plus the repaired dependency gate.
- **B1 revalidated**: `cargo build -p rays_bin -v` after test runs shows `Fresh flow_sop`; build 1.92s. No cache clean, profile or RUSTFLAGS changes.
- Final `cargo check --workspace --all-targets` passes (0.88s Cargo / 1.01s wall); `git diff --check` passes. Artifact tracker check covers immediate seed rendering, live collection data, invalid live status and offline fallback. CLI database/source readback verifies the synchronized 58-item snapshot.
- **B8 stays active** despite implementation commit `3dcb2c3c`: full editor and full GPU-target commands hit the unchanged 60-second run cap. Compiler/smoke/allocation checks pass; full editor qualification is still unproven. Keep the timeout visible while continuing the remaining performance work. No deadline or allocation limit was raised.
- Remaining scope is unchanged: B7, the unfinished D wave, all Z/R/G items and B8's full-run requirement; final workspace/native qualification on macOS **and Windows** remains outstanding. Z0 must establish the guards before the later R/Z performance commits. The original artifact remains the requirements source.

### Continuation — D-wave implementation

- Previous turn classified **progress**: committed stages and repaired gate are present in the clean worktree at `38c86945`; artifact v18 read with the CLI. Full goal remains 58 items.
- Two building slots: D3 (`rdk_geom`) and D8 (`flow_syntax`/IR/printers). Root prepares D10 without a third build; D10 borrows D8's slot while that agent edits.
- D10 follows the existing decimal Lisp grammar: signed full-range `i64` parsing in settings, literal edits, inspector fields, timeline and picked primitives. Added min/max boundary coverage and explicit rejection of legacy radix/underscore spellings; standard trim/round/clamp replace compatibility wrappers. Existing canonical float printing remains the separate Z6 change.

- **D8 complete** `095418dc`: one 102-entry primitive/name table; the exact 91-op lowering set is retained. Guarded/unguarded Instruction methods replace printer/layout/packed copies. Flow syntax/check/IR/shader/Web **573/573** pass27.59s (slowest1.981s); all114 tracked IR/Web expected files are byte-identical. Native Metal fixture16.29s (case0.322s) and carried-loop0.66s (case0.300s) pass; dependency gate0.57s green.
- **D10 checks so far**: all-targets check9.54s; param/editor_core23/23 pass4.83s (slowest0.081s), pxui/pxui_shell62/62 pass17.39s (slowest0.481s), SOP54/54 pass16.47s (slowest0.031s). One prior SOP/UI invocation stopped before running cases on a transient D3 compilation mismatch; final separate runs pass. Literal-edit and text-pane consumer checks remain before commit.
- D11 source work begins while D3 owns the test runner. Z0 adds a measured allocation bound to the existing real million-point gpu_sea case; warm cook guards will use actual subdivide/Boolean/scatter kernels. No allocation/deadline limit is raised.

- **D3 complete** `caf6359f`: a single64-bit/u128 exact engine replaces the duplicate heap/arena engines. Persistent constructors export only retained coordinates. Geometry60/60 pass0.88s (slowest0.087s); Boolean74/74 pass6.02s (slowest0.054s). The added Boolean snapshot was captured and confirmed on the original engine, then matched unchanged on the new engine: `b174629eacddb5690a6c56d966aadf2b` covers six solid operations, exact position bits, topology, point/vertex weights and piece groups. No golden changed.
- D3 measured debug medians512 iterations: forced orient3d+incircle37,729→9,517µs (**3.96×**); implicit circumcenter9,509→2,756µs (**3.45×**). Warm check0.78→0.83s (all-targets0.80s). Net+100lines includes155lines of regression/benchmark coverage; the planned −300 estimate was not treated as evidence.
- **D10 implementation committed** `3457d151` (net−194lines). Literal fast-path2/2 pass19.26s including dependency rebuild (slowest0.270s); text-pane11/11 pass1.37s (slowest0.195s), status-rule native gesture1/1 pass4.08s (case0.391s). A first unscoped editor build stopped before cases on three D11 conversion callers mid-edit; those callers are now ported and the D11 all-targets check passes.
- Optional `RAYS_SLOW=1 cargo t -p editor_document part_literals -- --ignored` exposed a pre-existing arena identity bug:0.6→0.60 produced no pane change (case0.446s). Preserve its assertion; replace the local-token-ID comparison with the existing syntax comparator and add an ordinary focused guard before closing D10. Earlier invocations without `--ignored` selected zero cases and are not counted as validation.

- **D10 complete**, follow-up `82ba264f`: arena-local NameIds are no longer compared across documents. Existing `s::equal` compares literal syntax correctly. Ordinary spelling regression1/1 pass3.21s (case0.166s); formerly ignored broad literal case1/1 pass1.06s (case0.633s), preserving all assertions. Its new focused test checks the raw authored token, since the canonical Lisp printer intentionally prints0.60 as0.6.
- **D11 complete** `92542f6a`: canonical decorated input Frame, canonical panel enums, borrowed routing with Cow only for event filtering, and borrowed reducer input remove ec_frame/converters. All-targets editor check10.25s, deps gate0.35s. Input/core17 pass2.68s; native command9 pass3.44s (slowest0.400s); reducer10 pass8.22s; layout2 pass2.35s. Native allocation3 pass5.61s (slowest0.372s) at newly tightened exact limits.
- **Z0 first stage** `296c35c2`: basic1384/157167, empty1206/81998, bare1417/96657 allocations/bytes limits now match measured values; successful context/cook assertions added. Full-million-point sea case now has4 warm+4 measured complete update→render→after_present frames, pipeline/buffer stability, unchanged residency/upload/readback/close checks. New initial ceiling54,450 allocations/2,930,000 bytes bounds measured54,446–54,448/2,904,464–2,922,176. A provisional exact ceiling from the first observation failed by808bytes on another baseline run, so initial guard calibration includes the observed variation; none of the three established limits was raised. Final sea guard passes10.74s including rebuild (case1.342s). Zero idle allocations and unchanged/changed subdivide/Boolean/scatter guards remain required, so Z0 stays active.
- D4 runs the finite-sort baseline before its shared float policy change. D9 has prepared the complete32-adapter removal with one-time decoding/validation retained. D12 removes only the redundant settings representation, preserving generic sketch settings, unrelated config keys and malformed-file overwrite refusal. At most two agents build; source edits by a third wait for a slot and compile-stable boundaries.

- Artifact **v19** published with the CLI and verified by byte-for-byte source readback plus all58 database documents against the ledger. Tracker regression passes immediate snapshot, live collection, invalid status and offline fallback. Snapshot:21done/5active/32pending; all original requirements retained.
