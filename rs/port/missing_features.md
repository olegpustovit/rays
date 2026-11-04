# Missing features audit: OCaml `lib/` vs the Rust port

Audit date 2026-11-04. No product edits. Method and numbers at the end; the evidence files are in the
audit scratchpad (`scratchpad/audit/*.tsv`), regenerate with the commands under "Method".

Result in one line: the port is faithful almost everywhere. Every OCaml error code but one is present
in Rust, every `.mli` value but a handful (all renamed or moved) is present, and every OCaml commit
since 2026-11-01 predates the Rust port (the port starts 2026-11-04 10:16) and shows no unported
change. The real gaps are whole behaviours, not names: eval-level guards (fixed by its owner), and
exception containment in four places.

## Real gaps

### 1. Evaluator guards (deferred checks of an inlined card). HIGH. Known, handed to the eval.rs owner.

- Module: `lib/flow/eval.ml` -> `flow_eval/src/eval.rs`.
- OCaml names with no Rust twin: `guards` (state field), `guarding`, `guardable`, `add_guard`,
  `refuse_later`, `e_guards` (inline-memo entry field), `card ~guards`, and the `Residual _ as v when
  Option.is_some st.guards && guardable v` branch of the validation loop in `inline_node`.
- Behaviour: a card whose `:validate`, `if` or `cond` reads a live argument (anything containing `t`)
  stays inlined and the check becomes a guard the card cooks with (the `$guards` argument that
  `flow_sop::lower` already consumes through `guards_of`/`FUSED_GUARDS`). Rust turns the validation's
  `$NEEDS_TIME` into `catalog_call`: the card stays a plain node, is rebuilt per frame, and gpu_sea
  leaks device sites and reads back every frame.
- Affects: `sketches/gpu_sea.rays` (the reported leak). Sketches that feed `t` into a SOP card are
  candidates: `flow_field`, `flow_kernel`, `isogrid_city`, `isogrid_drift`, `sop_gallery`,
  `ws_orrery`, `ws_wave`. Tests: OCaml `test_lisp_inline.ml` "a driven transform" (lines 231-267,
  `inlines = true` for a live `:shear_xy`, also with `:recompute_normals`); Rust
  `a_live_validation_keeps_the_plain_node` asserts the opposite (wrong, its comment cites a
  nonexistent OCaml `Needs_t` fallback), and `a_live_validation_inlines_the_card_as_a_guard`
  (ignored, red today) is the right one.

### 2. A node that raises while cooking must become a diagnostic, not end the cook. MEDIUM-HIGH.

- Module: `lib/sop/session.ml` `node_result` -> `sop/src/session.rs`; `lib/sop/async_cook.ml` ->
  `sop/src/async_cook.rs`.
- OCaml names: `uncaught_node_exception` (diagnostic code, message "a procedural node raised an
  exception while cooking", cause = the exception), `Uncaught_exception` (async cook error variant:
  "background cook raised: ...").
- Behaviour: `node_result` wraps every node cook; any exception (other than `Invalid_facts` ->
  `E_NODE_FACTS` and `Mem.Refused` -> `E_MEMORY`) becomes an error diagnostic that the editor shows on
  the node. The Rust port has no `catch_unwind` outside `rays_bin` and `parallel`; `async_cook.rs`
  documents it ("a panic here ends the worker instead ... `Error::UncaughtException` is never
  produced"). A panicking kernel, an `expect` or an index error in any node therefore takes the
  cook worker (or the process) down instead of marking one node.
- Affects: any editor session with a faulty node. No OCaml test asserts the code, so nothing fails today.
  Check the Rust twin keeps the `Mem.Refused` -> `E_MEMORY` path.

### 3. Stack overflow -> `E_DEPTH` in `Eval.protect`. MEDIUM.

- Module: `lib/flow/eval.ml` `protect` (line 3099) -> `eval.rs`.
- OCaml: `Stack_overflow -> Error "Evaluation is nested too deeply." (E_DEPTH)`; `Out_of_memory ->
  E_ARRAY_MEMORY "Frame data exceeds available memory."`.
- Behaviour: the explicit depth counter (`MAX_DEPTH`, 64 nested calls) exists in Rust, but unbounded
  native recursion (deeply nested data or terms, a long `let*` chain) aborts the process in Rust
  (it is the reason the perf gate needs a 256 MB thread stack in debug). OCaml answers with a
  diagnostic. Allocation failure has no Rust twin either (Rust aborts on OOM).
- Affects: robustness on hostile or very large workspaces; `RAYS_SLOW` debug runs.

### 4. Static-kernel failures refuse instead of failing hard. MEDIUM.

- Module: `lib/flow_ir/flow_ir.ml` `static_map` (line 431) -> `flow_ir/src/flow_ir.rs`.
- OCaml: `exception (Invalid_argument _ | Not_found | Failure _)` while executing a static kernel is
  caught and becomes `refuse [E_PACKED_EXECUTION ...]`, so the map falls back to the walker. The code
  `E_PACKED_EXECUTION` is the only error code in OCaml `lib/` (besides a golden helper's `E_LISP`)
  that never appears in Rust.
- Behaviour: Rust panics (index out of bounds, `unwrap`) inside a kernel run propagate. Combined with
  gap 2 this is the same missing "exceptions are data" layer.

### 5. Preconditions that OCaml states with `invalid_arg` and Rust omits. LOW-MEDIUM.

Found by the string scan (messages with no Rust twin). Each is a parameter validation:
`Camera.orthographic: near and far must differ` (`rays/camera`); `Edit_graph.factory arity must be
non-negative` / `names must not be blank` / `factory_slots names must not be blank`
(`sop/edit_graph`); `Display_list.reserve: negative capacity`; `Attribute.create_owned: empty name`,
`with_name: empty name`, `create_deferred: ...` (`rdk/core/attribute`); `Packed.Float3.Builder.*:
frozen` guards, `Float_array.row_range: index out of bounds`; `Rdk_mesh.Poly_bevel`: divisions,
distance, grain, selection-topology and point-fan checks; `Rdk.Plane_clip`, `Transform_ops.soft_transform`
(`radius must be finite and non-negative`, `rolloff radius must be positive`, `matrix must be
finite`), `Attribute_ops.blur_points` (iterations, step sizes, selection), `Spatial_index.View.*`
(`grain must be positive`, `output array is too short`), `Mesh_merge` group-schema equality,
`Deletion.primitive_partitions` length check, `Concat source_base/tag_base must be an integer`
(`E_CONCAT_TYPE`, `flow_sop/topology_kernel_impl`), `key/down takes a key name`, `pointer/down takes a
button name` (`flow/op`). In Rust a bad value reaches the algorithm (a panic, a wrong result or an
infinite loop) where OCaml raises with a message. Most are unreachable from the Lisp surface because
the checker bounds the parameter; the rdk and `Camera`/`Edit_graph` ones are reachable from tests and
integrations only. Not worth porting one by one; port them when the owning module is next touched, or
as a single `debug_assert`-style sweep.

**Lisp-reachability follow-up (2026-11-04):** none of these is reachable from a `.rays` file. Checked one
by one: `Camera.orthographic` near/far are the constants 0.1 and 1000 (the check is dead);
`Mesh_merge ... group schemas must match exactly` needs `~pad_groups:false` and its only caller never
passes it (dead branch); `Concat source_base/tag_base` has its `E_CONCAT_TYPE` twin in
`topology_kernel_impl.rs` (`integer(..)?`); the `Poly_bevel`, `soft_transform`, `blur_points`, `clean`,
`join_curves`, `boolean_*`, `remesh` preconditions are re-stated at node level (`sop_topology.rs`,
`shapes.rs`, `attributes.rs` `refuse(..)` and hard parameter ranges) before the rdk call; `key/down` and
`pointer/down` argument errors are excluded by the checker's types. What remains (rdk `grain must be
positive`, `Edit_graph.factory` blank names, `Display_list.reserve`, `Attribute.create_*: empty name`) is
API-only: reachable from integrations and tests, not from Lisp. Gap 5 is closed as no action.

## Triage of the rest (compressed)

False positives, by cause (no action):

- Case: OCaml `fused_writes`, `max_steps`, `max_depth`, ... are `FUSED_WRITES`, `MAX_STEPS`, `MAX_DEPTH`
  constants in Rust (71 names).
- Renamed, inlined or merged: most `flow/eval` (`ev_at`, `bind_pat`, `renv`, `rterm`, `rids`, ...: the
  evaluator is one dense `eval.rs`), `flow/workspace` `s_*` snapshot fields (Rust keeps the checker
  prelude as a cloned `Checker`), `Prelude`/`start` handled by `prelude.checker.clone()`,
  `kernel_requests` (the Rust context tests `handler.kernel.is_some()`), `Bulk_static`/`Static_mode`
  effects (the Rust `ResidualHandlers` carries `defer`, `trusted`, `replay` instead of effects),
  `packed_block_size` -> `PACKED_BLOCK`, `max_concat` -> `MAX_ITERATIONS`, `tolerant` (local
  closure), `inline_variants`/`inline_sites`/`inline_lock` -> `INLINE_VARIANTS`/`InlineMemo`.
- Moved crate: `editor_core.History` lives in `rays_editor::editor_history` (can_undo, redo_label,
  byte_depth, ...); `Burst`/`Gesture`/`Repair` merge kinds are there.
- Middle-dot strings: UI strings with `\u{b7}` in Rust (`Carry dropped ...`, `function . runs per call`,
  `iterate . feed back`, `time in seconds ...`): present.
- OCaml-only runtime concerns with no Rust meaning: `Mem` GC shedding (`major_cycles`, `ocaml_live`,
  `collect_gap`, `shed_cycle`), `Parallel` heap sizing (`adapt_heap`, `ensure_heap`), `Domain`
  workers, Metal/SDL3 raw-binding strings, "exceeds array limits" cardinality checks (Rust `Vec`
  has no 2^54 limit), `Out_of_memory` arms.
- Removed again later in the same OCaml history: `placeholder_bytes`, `table_floats`,
  `device_layer_capacity`, `text_rows_cache`, `material_count`, `trim_bytes` (added by a commit and
  dropped by a later one).
- Lower-level message text differs but the check exists: `Bulk function columns must match ...`,
  rdk parameter checks reported through `refuse(...)` in `sop/sop_topology.rs` (`sop/clean`,
  `sop/join_curves`, `sop/boolean_*`, `sop/remesh` all present).

Dead or build-time in OCaml: `Print_shader`/`Spirv.relation`, Metal bridge generators, `native_qualification`
benches, `goldens/replay.ml` helpers (`E_LISP`, `windows_digests`).

## OCaml commits since 2026-11-01 (`git log --since=2026-11-01 -- lib`): 189

All 189 are dated 2026-11-01 .. 11-04 01:54 (+0100), i.e. before the first `rs/` commit (2026-11-04
10:16 +0200), so the port was written against a tree that contains them. A scan of each commit's added
definitions and string literals against all of `rs/crates` found only these absent items, each triaged:

| Commit | OCaml change | In Rust? |
|---|---|---|
| 939dbd00 | `Duplicate operator:` `invalid_arg` (`flow/op.ml`), eval record-path caches (`recent_cell`, `new_records`, `def_record_paths`) | the precondition is absent (gap 5, low); the caches are an OCaml perf change with no behaviour |
| 2916eddf | `Parallel: the pool has already been shut down` | Rust pool has no shutdown state; n/a |
| 12889c56 | `stateful_programs`/`static_topologies` locks (flow_sop), `ktime` (flow_gpu host) | `STATEFUL_PROGRAMS`, `static_cached` present (case) |
| bc5346c0, 8307463f, 0b71bc19 and other `mem`/`lru` | OCaml GC-driven shedding | n/a, no GC |
| 545bf475, 48a7a7ce, 4c54160d | OCaml domain minor-heap sizing | n/a |
| ad22d9b0 | `sdl3` window display scale externals | Rust sdl3 binding differs (checked by the lead's Windows work) |
| b9c17192, 53507669, 07cd9dc2 | Windows float printing, SPIR-V sin/cos/idiv rewrites | Rust `cfmt` + `flow_shader::spirv` carry the same transforms (spirv goldens in the working tree) |
| remaining ~170 | Vulkan/Windows bring-up, tests, benches, docs | no Rust twin needed or already in `vulkan_sys`/`ogpu_vulkan` |

No commit showed a missing behavioural change except the ones in "Real gaps".

## Method and numbers

- Twins: library -> crate set (`flow` -> `flow_syntax`/`flow_check`/`flow_eval`/`param`; `rdk` ->
  `rdk*`; `runtime` -> `runtime*`; `scene_execution` -> `renderer`; others one to one). Rust token
  sets per library built with `grep -rhoE` over `*.rs`.
- Scans (bash, grep, awk, comm; no Python):
  1. OCaml `let`/`let rec`/`and` names (>= 4 chars; top-level, or nested and used >= 3 times) absent
     from the twin's tokens: 866 candidates over ~280 modules, of which 71 are upper-case constants and
     the rest triaged above by theme and by reading the OCaml and Rust code for each flagged feature.
  2. `.mli` `val`/`external` names: 51 missing outside Metal/SDL/Vulkan raw bindings; all renamed or
     moved (History -> `editor_history`, `Parallel.both/map_array` -> `parallel`'s run helpers,
     `Lru.create` -> constructor); rdk: 10 `Hashtbl` functor signature items only.
  3. Record fields (`mutable x :`, `x :`) and variant constructors (`| Name`): this is what exposed
     `e_guards`/`guards`/`watching` in `flow/eval`.
  4. String literals >= 16 chars in non-test `.ml`: 815 absent, 650 from Metal/SDL/generators/test
     support/`rdk` cardinality checks, the rest triaged (gaps 3 and 5).
  5. Error codes `"E_..."`: 2 absent (`E_PACKED_EXECUTION`, test helper `E_LISP`).
  6. Per-commit added definitions/strings since 2026-11-01: 204 candidates, all triaged in the table above.
- Limits: a name scan cannot see a missing `match` arm or a missing branch inside a function that
  exists. Guards were found only because the state field name was absent. Gaps of that kind (a branch
  missing in a ported function) need either OCaml-vs-Rust differential tests (the sketch goldens,
  `RAYS_SLOW=1 cargo t -p flow_eval sketch`, `rays mem` parity runs like gpu_sea) or a per-function
  read. The two best next steps: run `rays mem` Rust vs OCaml for all sketches and diff the
  "left the device" lines (the smoke-native work started this: `rs/port/smoke-native.md`), and port the
  OCaml `lib/*/test_*.ml` files that still have no Rust twin (`ls lib/**/test_*.ml` vs
  `rs/crates/**/test_*.rs`).

## Test coverage note

OCaml has 365 `test_*.ml` files under `lib/`; Rust has 136 `test_*.rs`. Behaviour gaps like guards survive
because the OCaml test that would catch them was ported with its expectation inverted, or not ported.
Porting the missing tests (starting with `lib/flow/test_workspace_eval.ml`, `lib/sop/test_*.ml`,
`lib/flow_sop/test_lisp_inline.ml` and the `lib/flow_gpu` residency tests) is the cheapest way to
find the next gap of this kind.
