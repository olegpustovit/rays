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
| D | 12 | 12 | 0 | 0 |
| Z | 11 | 0 | 1 | 10 |
| R | 10 | 5 | 2 | 3 |
| G | 7 | 0 | 1 | 6 |
| **All** | **58** | **33** | **5** | **20** |

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
| D1 | D | done | `d42f55e4` — 13 geometry union-find engines use support::DisjointSet; min/rank/tie/mask semantics preserved; net −271 lines | 481 unique touched cases pass; frozen Boolean payload and dissolve 1/4-domain equality unchanged; warm guards pass |
| D2 | D | done | `2795d1b2` `5a2da040` — seven custom sorts removed; explicit source ties; padding-sized path ordinal preserves allocations | Ops/Boolean347 +scene26 +native64 pass; fuse8.3x and arrangement19.7x faster; native allocation3 pass exact caps |
| D3 | D | done | `caf6359f` — one 64-bit/u128 exact engine; arena temporaries, persistent outputs only | geom60 and Boolean74 plus frozen snapshot pass; predicates3.96x and circumcenter3.45x faster |
| D4 | D | done | `029244f0` — canonical rays_math float extrema/total ordering; numeric equivalence preserved | 580 CPU cases covered with2 focused assertion repairs; native/UI pass; sort1.44x faster |
| D5 | D | done | `0b6ee7ef` — canonical owner/count/group/quote/domain helpers; net −193 lines; no dependencies added | Core/parallel/geom/gate163, poly30, transform9, ops/Boolean347, native64, text13, IR346 and warm Boolean pass |
| D6 | D | done | `7ef8286f` `5279bbd3` — shared grain validation; strict canonical grain diagnostics before cancellation | grain-filtered ops/core; normal_ops grain0/-1 InvalidArgument and valid-grain Cancelled pass |
| D7 | D | done | `861fcfef` — shared bounds3 index, sorted candidates, inclusive filtering and bounded stack scratch | Boolean76, shared-index equality, frozen payload/cancellation pass; paired runtime97–100% of old; warm guard passes |
| D8 | D | done | `095418dc` — shared primitive table and Instruction guard methods; 91 lowering primitives unchanged | Flow573/573 pass27.59s; 114 golden files byte-identical; native Metal fixtures and loops pass |
| D9 | D | done | `788ac0ca` `a488c2a2` —32 Parameters/36 static Decl wrappers removed; original Reader retained to avoid new shape violation | SOP54, Flow manifest, native catalog and exhaustive qualification pass; manifest unchanged |
| D10 | D | done | `3457d151` `82ba264f` — std i64/trim/round/min/max; syntax-aware literal-spelling comparison | Boundary/UI/SOP/text checks pass; spelling-focused1/1 and literal qualification1/1 pass |
| D11 | D | done | `92542f6a` — canonical input Frame/panel types; borrowed routing; ec_frame/converters deleted | All-targets check10.25s; input/core17, commands9, reducer10, layout2, alloc3 pass |
| D12 | D | done | `98ee157a` — validated Syntax replaces settings Value/codec; preferences read once; shared text helpers | all-targets5.83s; store5, text12, native guide1 pass; i64 save/Unicode regressions |
| Z0 | Z | active | `296c35c2` `bf116466` `c1617a45` `25c98948` `608c3385` — established guards and measured ratchets | Sea51565/2823000; changed subdiv114105, Boolean26695, scatter1402035 allocations; zero/O(1) targets open |
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
| R1 | R | done | `c1617a45` — shared captures/paths/persistent iteration; consumers borrow; allocation guards lowered | 451 evaluator/IR +40 sequential/parallel +zero-view/native/three-kernel allocation checks pass |
| R2 | R | done | `25c98948` — immutable parent cache layers; snapshot isolation; iterative destruction; scatter guard lowered | IR346, 10000-layer/drop guard, bench digest c4bbf006c82014c7, workspace all-targets, native/three-kernel guards pass |
| R3 | R | done | `608c3385` — immutable lexical free-name ranges; global/per-evaluation caches removed; bypass active-input semantics preserved | Flow490, parallel40, zero-allocation reads, lowered idle/sea/three-kernel caps and workspace check pass |
| R4 | R | active | Owner/lifetime and all operator callers traced; baseline helpers prepared; no production changes yet | Library plus 49 sketches byte-identical |
| R5 | R | active | Shared state maps and integer-pair stamps implemented; initial fork/stamp/transaction allocation guard passes | Full state/rollback/native consumer qualification pending; restore snapshots by move |
| R6 | R | done | `39d6f261` — 243 Result predicates; par_fill and plain zip; ten cancellation atomics and two error Mutex channels removed | CPU558, group12/par_fill3, parallel40, native/sea/warm caps and workspace check pass; serial ABI cost disclosed |
| R7 | R | done | `85e72ed5` — fourteen repoint paths share offsets/kinds; one validated constructor; polygons_shared deleted | Geometry429/native10, shared-storage identity, idle/sea/three warm guards pass; repoint9.8x faster |
| R8 | R | pending | — | pxui_graph (31 cases); alloc_budget |
| R9 | R | pending | — | Lower the alloc_budget.rs limits |
| R10 | R | pending | — | test_prepared_scene3 |
| G1 | G | active | `29c9efbf` — common validation moved to core; entry checks109→27; 98 immediate closures removed | Local56 pass10.61s; exact-commit Windows29 pass7.008s; native-only semantic migration still requires G2 |
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

The D wave is complete. At most two agents build concurrently. Current build slots are explicitly handed off between root shared validation, R6 qualification and G1 Windows qualification. R4 prepares without building.

| ID | Primary crate | State |
|----|---------------|-------|
| D1 | `rdk_core`, geometry consumers | Complete: `d42f55e4` |
| D2 | `rdk_ops`, `rdk_boolean`, path | Complete: `2795d1b2`, `5a2da040` |
| D3 | `rdk_geom` | Complete: `caf6359f` |
| D4 | `rays_math` | Complete: `029244f0` |
| D5 | `rdk_core`, `rdk_ops` | Complete: `0b6ee7ef` |
| D6 | `rdk_core`, consumers | Complete: `7ef8286f`, `5279bbd3` |
| D7 | `rdk_boolean` | Complete: `861fcfef` |
| D8 | `flow_ir` | Complete: `095418dc` |
| D9 | `flow_sop` | Complete: `788ac0ca`, `a488c2a2` |
| D10 | `rays_editor`, `editor_document` | Complete: `3457d151`, `82ba264f` |
| D11 | `input`, `editor_core` | Complete: `92542f6a` |
| D12 | `editor_core`, text | Complete: `98ee157a` |

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

### Stable D-wave boundary —24done/5active/29pending

- **D4 complete** `029244f0`, net−242lines across44files. Shared extrema preserve X1 NaN/signed-zero rules; total ordering replaces copied comparators and inline unordered fallbacks. Geometric duplicate identity and numeric-mode zero equivalence/tie signs are preserved. One pure-math dependency edge is Windows-only Vulkan→rays_math; dependency gate passes. Sorting debug median8×16,384finite values20,397→14,196µs (**1.44×**). Incremental test compiles10.27→11.13s include different rebuild scope, so no compile-speed claim. Scoped check1.01→2.00s, broad all-target consumers3.15s.
- D4 validation: eight-crate580-case run14.47s had578pass and2stale expectations. NaN-radius now produces a typed Shape2 fill error under the requested propagation policy; explicit error plus finite-cap assertions pass1.73s. The unrelated D6 grain classification failed with original support source too; strict follow-up `5279bbd3` verifies grain0/-1 InvalidArgument, exact operation/message, then valid-grain Cancelled (2.16s,case0.026s). Both original failing cases are resolved. Slowest CPU case1.171s; frozen Boolean payload unchanged. Metal world2 pass2.95s (slowest0.261s); scope elbow/hint and status gesture3 pass. Windows Vulkan execution remains unverified on macOS.
- **D9 complete** `788ac0ca`, net−857lines:32Parameters/from_values and9to_values removed;36static factories use decl::native, while Switch retains its dynamic schema. One-time table decoding and validation timing remain. All37specs/META bodies,25validation message sequences and629parameter reads/offsets match the originals. SOP54 pass6.778s (slowest0.031s); Flow native manifest1 pass15.993s including15.21s rebuild (case0.137s); normal editor catalog1 pass7.149s (case2.787s). Explicit RAYS_SLOW=1 catalog qualification runs2actual registered cases and passes8.003s (exhaustive5.169s,normal2.608s); this custom harness does not use --ignored. No manifest/golden changed.
- D9 gate follow-up `a488c2a2`: the opportunistic borrowed Reader introduced2new shape violations. Restored its original owning implementation without a gate exception. Native shape adapters/domain/cache regression passes5.887s (case0.023s); check0.84s. Actual shape gate remains red on497pre-existing violations (499before follow-up); the two newly introduced violations are removed. These historical failures remain visible.
- **D12 complete** `98ee157a`, net−41lines including regressions. Settings store validated Syntax for the original six literal kinds, deleting the duplicate Value/codec; startup reads preferences once. Guide save retains unrelated keys and refuses invalid/unreadable-file overwrite. PXUI line bounds, char_indices and a single plural helper replace copies. All-targets check5.83s. Store5 pass2.50s (slowest i64 case0.132s); text12 pass31.65s with D4 dependency rebuild (case total1.020s,slowest0.224s); native guide1 pass22.28s including20.46s rebuild (case0.460s). i64 MIN/MAX/2^62 read+save, first/wrong-kind lookup, invalid save and1–4byte/combining-character boundaries pass.
- **Z0 warm guards** `bf116466`: persistent sessions warm original and changed inputs, then measure an unchanged cache hit and a genuinely new same-sized input. Initial lowering, explicit node rebuilding and input construction are outside measurement; lazy work performed by cook remains counted. All operations use domains1; misses/output identities are checked, changed output is nonempty and differs, and its full bytes match a fresh session. Changed peak exempts only unique newly owned output components, excluding shared input IDs. Density/group/attribute-transfer scatter is exercised. One initial subdivide fixture selected its default missing hole group; explicitly selecting no holes repaired the fixture without changing kernel/assertions.
- Z0 warm guards pass13.08s including rebuild; cases Boolean0.370s, subdivide2.498s,scatter2.845s. All use one cap across three sizes, and none is zero yet. The end state remains required; these are initial ratchets, not evidence of achieved zero/O(1) scratch.

| Z0 kernel | Fixture scales | Unchanged allocations / peak B | Changed allocations / scratch peak B (small→large) |
|---|---|---|---|
| Subdivide |8/16/32quad cells per side|18 /3752|139383/826195 →143843/1258341 →151233/3453743|
| Boolean intersection |1/4/16overlapping box pairs|25 /4426|2391/69331 →7242/240733 →26707/926345|
| Scatter with density/selection |8/16/32quad cells;64/256/1024outputs|18 /5353–5359|1601563/13704426 →1603061/14578037 →1612486/16451199|

- Final stable-boundary `cargo check --workspace --all-targets` passes11.69s wall (11.57s Cargo), before D1 production replacement. No profile/RUSTFLAGS/cache changes. D1 baseline: min/rank helper medians633959/498167ns over65,536items; dissolve300×220 exact1/4domains passes, and frozen six-operation Boolean digest remains `b174629eacddb5690a6c56d966aadf2b`. D5/D7 prepare in temporary files while D1 owns shared UF source; no third building agent.

- Artifact **v20** published and source/database readback verified:24done/5active/29pending (**41.4% by item count**). Tracker fallback/live/offline regression passes. No scope removed; B8 full-run timeout, pre-existing shape violations, zero-allocation end state and final Windows/native qualification remain open.

### Continuation — D1 and shared residual views

- Previous status-only turn classified **no progress**; revalidated committed D1 and current source, then resumed the unfinished R1 implementation. No blocker prevents progress.
- **D1 complete** `d42f55e4`: all 13 geometry union-find implementations now use `rdk_core::support::DisjointSet`, preserving minimum-root, rank, equal-rank tie, masked and direct-link behavior. Net −271 lines; 481 unique touched cases pass. The Flow IR alias-equivalence set stays local because its dependency boundary excludes geometry.
- D1 measurements: old min/rank at 65,536 items 633959/498167 ns; paired old/shared runs at 262,144 stay within about 5%. Final shared medians 1.945/1.596 ms. Rank storage changes i64 → u8 (7 bytes saved per vertex). Frozen Boolean digest unchanged; exact dissolve 300×220 in 1/4 domains passes. Warm Boolean and subdivide guards retain their established limits.
- **R1 active**: shared Arc captures/paths and the existing persistent iteration chain replace deep copies. Regression includes nested captured data, nonempty paths and iteration, shared-storage identity, expired-evaluation error and retained snapshot validity. The new zero-allocation assertion fails against the previous implementation (15,360 allocations for 1,024 reads) and passes after the change (0 allocations; observed 57.208 µs). Focused command passes in 8.36 s including rebuild; case 0.058 s. A first lifecycle assertion exposed the existing recent-arena retention queue; the test now releases its own queue entry before checking expiration. Production retention is unchanged.
- R1 evaluator/IR regression: `cargo t -p flow_eval -p flow_ir` passes all 451 selected cases in 25.71 s wall; slowest deterministic multi-thread evaluator case 1.935 s, followed by driven bounds 1.041 s and array composition 0.920 s.
- **D7 complete** `861fcfef`: shared bounds3 index replaces the private cells builder and RefCell comparator; sorted candidate IDs preserve deterministic boundary behavior. Linear certified-bounds oracle, cancellation, all 76 Boolean cases and frozen six-operation payload pass (3.90 s wall, slowest 0.080 s). Shared bounds-index equality check passes. Initial separate timing samples suggested a regression; nine alternating paired same-process medians show shared cells at 97–100% of legacy runtime. No speedup claim. A warm guard caught 18 extra allocations; replacing per-query heap traversal scratch with a depth-bounded stack removes that regression.
- **R1 complete** `c1617a45`: full workspace all-target check passes in 2.66 s; dependency gate passes in 0.83 s. Shape remains at **497 preexisting violations**, with no new diagnostic class introduced. FlowSOP sequential/parallel suite: 40 cases pass in 40.34 s wall; slowest `light_03` 2.875 s.
- R1/Z0 ratchet: native basic/empty/bare remain exactly 1384/157167, 1206/81998, 1417/96657 allocations/bytes per frame (3 cases pass, 16.38 s including rebuild; slowest 0.403 s). Sea observed maximum is 51,698 allocations / 2,835,442 B, down from 54,448 / 2,922,176 B; new cap **51,700 / 2,840,000**, confirmed by a second run (6.63 s wall, case 1.000 s). Million-point/1,996,002-triangle resident output, zero payload reads/staging, buffer/pipeline plateau and bounded async close assertions all remain.
- Three warm-kernel guards pass with lowered caps in 9.72 s wall (slowest scatter 2.719 s, subdivide 2.469 s, Boolean 0.375 s). Unchanged hits remain subdivide18/3752, Boolean25/4426, scatter18/5359. Across the three sizes, changed allocations now subdivide103471→107931→115321 (previous139383→143843→151233), Boolean2407→7252→26693 (D1 baseline2391→7242→26707), scatter1436323→1437821→1447246 (previous1601563→1603061→1612486). New changed caps: **115325/3453750**, **26695/926350**, **1447250/16451200** allocations/scratch-peak bytes; scatter hit peak cap lowered5400→5360. Rounding covers observed key-length variation; every previous cap only decreases. Scratch peaks still grow with fixture size, so Z0/Z9 remain open.
- D5 and D2 own the next two build slots; root prepares R2 without building. G1 prepares call-chain/safety evidence without source edits/builds. Full scope, original deadlines and final macOS/Windows qualification remain unchanged.
- Artifact **v21** published with the CLI; source byte readback and all 58 database documents match the ledger. Tracked seed JSON is now synchronized with the embedded seed. Tracker fallback/live/offline regression passes. Snapshot: **27 done / 6 active / 25 pending (46.6% by item count)**.

### D-wave complete and R2 verified

- **D5 complete** `0b6ee7ef`: 37 files, net −193 lines. Canonical attribute/group owner names and geometry counts, two distinct byte/Unicode quote codecs, ordered first-match group/edge-group unions, ASCII blank checks and fourteen domain adapters. Detail refusal and Facet shorthand fallback stay intact. The only generic exception is the exact canonical `parallel::with_domains<R>(domains: i64, work: impl FnOnce() -> R) -> R`; sibling/extra-generic/FnMut/nested variants remain rejected. No new dependency.
- D5 validation: all-target affected check6.26 s; core/parallel/geom/gate163 cases6.204 s (slowest0.089 s); poly30 cases13.040 s including rebuild; transform9 cases0.678 s (slowest0.532 s); shared ops/Boolean347 cases3.98 s; native sop/rdk_rays64 cases9.887 s; text13 cases21.816 s including rebuild (slowest0.207 s); reused full IR346 and warm Boolean checks. Dependency gate green; shape497→495 only from D2 deleting generic legacy sort code, with no new normalized diagnostic.
- **D2 complete** `2795d1b2`, allocation correction `5a2da040`: seven bulk custom sorts and obsolete comparators removed, including the real Fuse-reduce caller. Explicit source primitive/edge tie keys replace unspecified heap ties. Fuse8×16384 median31.990→3.856 ms (8.3×); exact-axis arrangement2048 median9.946→0.504 ms (19.7×). All-target1.81 s; ops/Boolean347 pass3.98 s, slowest1.259 s; scene26 pass3.06 s. Frozen Boolean payload unchanged. Of40 shape constants,38 stay byte-identical; the two shared-edge NonZero vertex/index strings intentionally change, with independent covered-area3 assertions. Odd-sliver tie intentionally retains highest original primitive0→2.
- D2's first explicit edge tie tuple increased path crossing storage24→32 B. The established idle guard correctly failed basic+32 B and bare+64 B while counts stayed fixed. A checked u32 source ordinal now uses Crossing's existing padding; a 24-byte size regression and exact path goldens pass. Native allocation3 cases pass20.77 s including18.58 s rebuild, restoring exact previous budgets (basic1384/157167, empty1206/81998, bare1417/96657). No cap raised.
- **R2 complete** `25c98948`: scope forks freeze/move local cache entries into an immutable parent layer, then share it; later parent/child writes cannot leak into older snapshots. Empty-local forks reuse the inherited layer. Reads and last-owner cleanup are iterative; a10,000-layer chain is checked. The new identity/snapshot regression fails against old whole-map copying and passes after the change (15.92 s including rebuild; case0.051 s). Paired128-fork/1024-capture sample: copied14.677291 ms, shared53.750 µs. This is a cache-fork microbenchmark, not an end-to-end frame speed claim.
- R2 full IR346 cases pass10.56 s wall (runner8.158 s; slowest array composition0.903 s). `bench_core` map1024 digest remains **c4bbf006c82014c7** in1/8 domains; sampled force medians0.0084/0.0079 ms before and0.0105/0.0117 ms after under differing concurrent load, so no force-speed claim. Warm example command after takes0.53 s; full workspace all-target boundary passes19.49 s.
- R2 allocation checks: three warm kernels pass14.75 s wall while the workspace check also compiled (slowest scatter4.952 s, subdivision4.579 s; both below unchanged5 s case cap). Isolated scatter confirmation passes6.14 s wall including2.73 s build (case2.553 s), showing no persistent case slowdown. Changed scatter across sizes now1422093→1423591→1433015 allocations; cap lowered1447250→**1433020**. Subdivision/Boolean and all peak-byte caps stay unchanged. Sea passes1.83 s wall/case1.206 s with unchanged tightened51700/2840000 cap and all resident/lifecycle invariants. Native idle exact-budget proof reused from D2 follow-up on the same R2 source.
- **Windows GPU baseline now verified** at exact committed `76f1a7f5d5c0b588d4d7b204dd3a1e8677d3c7f8`, detached clean checkout on the trusted Windows host, pinned GNU Rust1.99.1; global toolchain unchanged. Scoped core/mock/Vulkan all-target check9.831 s passes. `RAYS_VULKAN_VALIDATION=fatal cargo +1.99.1-x86_64-pc-windows-gnu t -p ogpu_core -p ogpu_mock -p ogpu_vulkan`: **28/28 pass**,14.437 s wall (slowest core ownership2.156 s). Native transfer/compute, ray tracing, render, OOM and low-level texture cases actually execute, with no adapter skips; conformance asserts zero remaining handles/validation messages. This is a baseline, not proof of future G1–G5 changes or final workspace qualification.
- Next source owners: G1 GPU validation/layering; R7 topology layout sharing; R6 prepares group callback migration after R7; root prepares R3 immutable free names. Only G1/R7 build at this boundary. All58 requirements remain; B8 full-run timeout, preexisting shape violations, zero allocation/O(1) scratch and final qualification stay open.
- Artifact **v22** published and verified by byte-identical source readback plus all58 live database documents. Embedded/tracked seeds agree and tracker immediate/live/offline regression passes. Snapshot **30 done / 6 active / 22 pending (51.7% by item count)**.
- R7 source committed `85e72ed5`, still **active** pending idle/sea/scatter ratchet checks on coherent G1 source. Shared-layout regression and actual repoint sample pass (192667→19628 ns for64×4096 triangles,9.8x). Geometry429 cases pass14.59 s, native rdk_rays10 pass6.20 s, warm Boolean/subdivision pass existing caps. R6 production installation stays held through the remaining measurements.


### Continuation — R3/R7 final proof and R6/G1 integration

- Previous status-only turn classified **no progress**. Revalidated the current tree, source commits and exact live test handles, then resumed the shared regression boundary. Full58-item objective unchanged.
- **R7 complete** `85e72ed5`: geometry429 cases pass14.59s (slowest1.207s), native rdk_rays10 pass6.20s; shared offset/kind Arc identity and fresh topology identity retained. Repoint64×4096 triangles192667→19628ns (9.8x). Final idle3 pass27.32s including24.96s compile (slowest0.399s); sea1 pass1.46s (case1.043s); warm kernels3 pass14.87s (scatter2.659s, subdivision2.369s). These runs include R3 immutable names and stable G1 layout correction; newly lowered ceilings are being reconfirmed with R6.
- **R3 implementation**: immutable lexical NameId ranges built once at TermArena freeze replace the global cache/mutex, per-evaluation cache and repeated string interning. Sequential let/loop binders, function captures/parameters, state and call-function names preserve lexical semantics. Review found bypass must select only its active authored first input; new regression fails before the shared bypass-input correction and passes after it, including keyword-before-positional and inactive function cases.
- R3 direct zero-allocation guard: old1024 reads3072 allocations/141.333µs; new0 allocations/3.209µs. Sample32evaluations6197→5619 allocations,320223→306240 peakB; checking25.888→24.265ms. Samples are local debug measurements, not an end-to-end frame claim. Final Flow check/eval/IR490cases pass14.52s wall (slowest deterministic evaluator2.209s). Shared R3/R6 FlowSOP parallel_exact40cases pass43.19s wall (runner42.630s; slowest light_022.603s). No goldens, deadlines or limits raised.
- **G1 source stage** `29c9efbf`:20files net−234lines; common slot/shape/pipeline/target/command validation moved to core; Metal explicit entry/drain109→27,98 immediate closures removed. Local56cases pass10.61s wall (slowest native library0.341s); scoped all-target check1.06s. Trusted Windows checkout is clean at exact29c9efbf; scoped pinnedGNU check0.757s and fatal-validation Vulkan/core/mock29cases pass7.008s (slowest raytrace0.443s). Native transfer/render/raytrace execute without adapter skips; teardown asserts zero handles/messages. G1 remains incomplete because native-only semantic APIs still require the G2 migration; ownership/submission counters remain for G4.
- **R3 complete** `608c3385`: final workspace all-target check9.20s passes; shared parallel40 proof above and latest native/geometry guards cover coherent R3+R6+G1 source. Idle3 pass15.27s then tightened byte limits confirmed3/3 in4.56s (slowest0.378s): basic1382/157055, empty1206/81958, bare1417/96617 allocations/B per frame. Sea passes1.68s wall (case1.171s), maximum51561/2822715 with all residency/zero-upload/bounded-close invariants; cap lowered to51565/2823000. Three warm cooks pass3.03s (scatter2.671s, subdivide2.392s, Boolean0.393s); changed caps lowered to subdivision114105/3452650 and scatter1402035/16429860, Boolean26695/926350 unchanged. No cap raised.
- Source/index boundaries remain separate: R3 commit contains only root's guard hunk in flow_sop/test_alloc_budget; R6's Result predicate callback remains in its own diff. R4 begins exact baseline capture before checker/operator changes; G2 implementation prepares outside the worktree until its build slot is available.
- Artifact **v23** published with the CLI and verified by byte-identical source readback, matching embedded/tracked seeds and all58 live database documents. Tracker immediate/live/offline regression passes. Snapshot **32done/5active/21pending (55.2% by item count)**. All original requirements remain; B8 timeout,495historical shape violations, zero/O(1) end state and final full qualification stay open.

- **R6 complete** `39d6f261`:57files net+5lines including five regression guards and an ignored timing case. Group/edge init now takes Result<bool>; constructors use par_fill, combine uses plain zip. All243actual constructor calls migrated (173group/70edge). Ten cancellation AtomicBools, one misleading incidence validation flag and two Boolean error Mutex channels removed. Invalid mapping flag remains deliberately to preserve cancellation-over-invalid-map priority. Grain/name/topology validation order, ordering/padding, deterministic lowest-range typed errors, callback panics and4096-element polling remain checked. Triangulate2d cancellation retains its existing InvalidTriangulation message.
- R6 validation: focused group12 and par_fill3 pass; all-target workspace9.20s; full affected CPU558cases24.194s (slowest parallel attribute interpolation1.137s); shared FlowSOPparallel40cases43.19s and lowered native idle3/sea/three warm cooks pass as recorded above. Temporary reference constructors were removed and final production source byte-compared before commit; diff--check clean.
- R6 paired same-process four-operation medians: domains4 init1.957→0.834ms(grain97),1.831→0.454ms(grain4096),2.35–4.03x faster; unions15–34x faster. Domains1 init1.785→2.768ms and0.991→1.264ms,28–55% slower from the required Result predicate ABI. This serial cost remains visible; no second API or generic adapter was added to disguise it. Actual warm/native checks stay within their unchanged time and lowered allocation limits.
- **R4 baseline captured** before checker ownership changes: all178 static cases (129actual library sources with dependencies plus49sketches), existing sketch golden and custom-mutated Op guard pass. Full-library monolithic case exceeded the original5s case cap; per-source coverage replaces only that oversized fixture, retaining every source and original deadlines. Prelude clone64times47488allocations/4.334ms; warm-check median1.748ms/12864allocations. Borrowed/cached Op batch compiles; full checker base/PathId changes remain active.
- **R5 active**: new guard fails before changes (fork85allocations,1024stamp reads2048, read-only transaction85), then passes after shared snapshots (1/0/0). Added nested rollback and repeatable checkpoint coverage; packed-scope and native image caches migrate from String to typed stamp fields. An initial scoped check identified a remaining packed String field; full caller audit fixes this and native image sentinel fields before workspace verification. Full qualification remains pending, so R5 is not counted complete.
- Artifact **v24** published and CLI readback verifies source bytes, both seeds and58live records. Snapshot **33done/5active/20pending (56.9% by item count)**. R5 remains active pending full consumer qualification; all original scope remains.
