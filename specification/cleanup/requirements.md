# Architecture cleanup: accepted requirements

Source: https://artifacts.microapps.space/a/ws981nk0cx20, version 8, 2026-10-27.
Source HTML SHA-256: `6b9b1716b0515397309b229514378a3481d919cd4452f297a07275202efbb1c1`.
Repository baseline: `62582188`. Progress and validation: [status.md](status.md).

This is a repository copy of the approved scope, not a completion report. The
owner's locked decisions supersede older instructions retaining OCaml SOP
oracles, the API manifest, or alternate wire styles. The Windows backend itself
is outside this plan; preserving its seams and the SPIR-V emitter are in scope.
Size targets described as estimates remain estimates. Completion requires every
step, exit gate, guardrail and finding to have current implementation evidence.

## Locked decisions

1. **Old OCaml SOPs and rdk parity code: keep as an oracle library, or phase out?** Locked: phase out. Rays Lisp is the SOP surface. Goldens are captured from the Lisp nodes, then the OCaml twins, the parity-only rdk code and every parity test are deleted. (P1)

2. **Windows GPU API, given hardware ray tracing on NVIDIA is required?** Locked: Vulkan with `VK_KHR_ray_query`, as one `ogpu_vulkan` backend for raster, compute and RT. Rays emits SPIR-V binary itself. No DXC, glslang, shaderc or Slang, in the app or in the build. The only external SPIR-V tool is `spirv-val`, an optional test-time validator. (Win)

3. **Path tracer source language?** Locked: Rays Lisp, compiled to MSL and SPIR-V like every other kernel. No .metal file. It must be no slower than today (gates in 04c). (P8)

4. **Runtime raster shaders (Scene3, UI, 2D)?** Locked: Rays Lisp (P9). No handwritten shader remains on any platform. (P9)

5. **Wire style?** Locked: curves. Delete `Rect` and `Straight`, and fix pxui_graph/AGENTS.md and flow.md. (P0)

6. **`api_stable.json`?** Locked: delete it with its promote step and the api_manifest tool. Lisp is the contract, and the OCaml API is second-class. (P0)

7. **Explicit shadows (`Shadow3`) and stencil: delete?** Locked: delete. `Shadow3` is an OCaml-only API for a precomputed depth-map shadow with PCF filtering. Its type has no constructor, so nothing (no Lisp, no editor) can create one. Stencil is a GPU masking buffer with three pipeline variants that nothing produces. World sun shadows use their own sun-map path in `Scene3_world` and keep working. Deleting removes 4 of 12 pipeline families plus the related caches and MSL. (P0)

8. **OGPU for Windows?** Locked: keep OGPU as the backend-agnostic seam. That means the driver record, the virtual library, `ogpu_mock` and the backend-tagged shaders. `ogpu_vulkan` is its second implementation. (Win)

9. **Scene, world and settings kinds as SOP nodes?** Locked: stop. 'SOP' is Houdini's word for geometry operators, so non-geometry kinds get plain schemas and descriptors with no Node, Session or cook. (P5)

10. **`Scene3` / `Shape2`?** Locked: fold and shrink them under `Scene` after P4. (P4)

11. **`scalar_tree` stack VM?** Locked: delete it. Rays must still compile to fast CPU code, and that speed comes from one place: the Packed block engine, widened in P3. Gate: `bench_core` and the CPU timings show no regression with the VM gone. (P3)

12. **`Rays.*` shims?** Locked: use `Rays_math.*` directly and delete the shims. (P2)

13. **Phase order?** Locked: parallel, as soon as possible. P0 and P1 run together. P3, P4 and P5 start the day P2's leaves land, with one owner each. P8 and P9 follow P3, and P6 and P7 run alongside. (—)

## P0: Hygiene and safety net

Remove what is plainly dead or wrong before touching structure, and make sure every test that exists actually runs.

- [x] P0.S1: Remove the 900 MB worktree. Delete `sop-lisp/`, `web-export/results.md`, `docs/`, `workflow.md` and `project.md`, the README review boilerplate, and the finished DEFERRED items. Rewrite `api.md`. Evidence: the named tree is absent from both disk and `git worktree list --porcelain`, and `.claude/worktrees/` is ignored; all named obsolete documents are absent. The README describes current native/Lisp usage, `api.md` is a 163-line internal-layer reference, and `DEFERRED.md` retains open issues only.
- [x] P0.S2: Make every test reachable: fix the `rdk_parts` group and the mislabeled groups, wire up `test_world_command` and `test_workspace_vocabulary`, add one `RAYS_SLOW=1` qualification rule, and delete the `*_draft` aliases and the `.pending` glob. Evidence: current dependency-gate census passes, including grouped dispatch and aggregate-lane regression checks; the shell qualification rule explicitly sets `RAYS_SLOW=1`.
- [x] P0.S3: Fix the correctness bugs found in passing: the lossy `encode_numeric`, the non-atomic `Display_list` id, and the iso_surface and image/noise defaults read from oracles. Make `rays web` write atomically, narrow the catch-all handlers to named exceptions, and replace the Marshal state keys. Evidence: XC3, XC20, SP6/SP7, FL37, FL16 and FL25 have focused regressions recorded in status.md; field probes additionally verify different defaults from an actual checked Lisp declaration.
- [x] P0.S4: Delete dead features: the Shadow3 chain, stencil (12 → 8 pipeline families), Edit_graph `bypass`, warning diagnostics, the extra wire styles, `keymap3`, dead editor hooks and identity wrappers, `capture_callback`, unused counters, `web_server.mli`, the context and type registries, and `Check.kind.aliases`. Evidence: Shadow3, stencil, Edit_graph bypass, keymap3, capture_callback, web_server.mli, Rect/Straight wire styles, the context/type registries and Check.kind.aliases are absent. Warning diagnostics (W_UNKNOWN_GROUP, W_UNKNOWN_META, W_SOFT_RANGE) are produced today, so they stay.
- [x] P0.S5: Clean up history and noise: the M3 markers in the bridge, the plan-id comments and test names in metal, codemod residue in records, and the stale `AGENTS.md` and skill text. Fix `dune-project`, `.gitignore` and the gate's redundant blacklists. Evidence: 11138c80 removes 66 M3 marker lines from metal_bridge.mm and a plan id from a flow_sop test comment, and drops two redundant gate rules; dune-project and .gitignore are clean.
- [x] P0.S6: Add `.ocamlformat` and format the golfed files in one mechanical commit per library, so later diffs are readable. Evidence: see XC29: .ocamlformat (0.28.1, conventional, margin 100); dune build @fmt is clean.
- [x] P0.S7: Add the guardrails that cost nothing: the alias-reachability check and the plan-id/date comment check (§07). Evidence: alias reachability is lane_aliases in test/dependency_gate.ml; plan-id/date comments are rejected by @tools/codemod/history-comments.

Exit gates:

- [ ] P0.G1: `check.exe --ship` is green
- [x] P0.G2: every test executable and group is reachable from a lane (gate check). Evidence: `@test/dependency_gate` passes after the alias split, with negative fixtures for orphan executables, unselected/duplicate groups and coupled aliases.
- [x] P0.G3: markdown down by about 13k lines
- [x] P0.G4: no `Shadow3`, `stencil`, `bypass` or `.pending` remain in lib/ Evidence: no Shadow3, stencil or .pending file/glob in lib. The remaining 'bypass' is the live Lisp bypass meta in flow_graph, not Edit_graph's dead flag.

Risk: Low. The only behaviour changes are the bug fixes, and each of those gets its own test.

Findings: RK2 RK31 SP3 SP4 SP5 SP6 SP7 SP14 SP15 SP19 SP20 SP22 SP25 SP28 PL1 PL2 PL4 PL16 PL19 PL20 PL21 PL33 PL35 FL12 FL14 FL15 FL16 FL25 FL26 FL27 FL31 FL37 UI7 UI8 UI19 UI20 UI23 UI24 UI30 UI32 XC3 XC13 XC18 XC20 XC21 XC29 TT1–TT7 TT10 TT17 TT20 TT22–TT28 TT33 TT35–TT37 WN5.

## P1: Phase out the OCaml SOPs and their parity code

Decided 2026-10-27: Rays Lisp is the SOP surface. The 128 OCaml node twins, the rdk kernels that exist only so tests could compare against them, and every Lisp-vs-OCaml parity test are deleted. No oracle library is kept. The 37 native SOPs (exact predicates, spatial indexes, solvers, wiring, carriers) stay OCaml.

- [x] P1.S1: Golden capture from Lisp, not OCaml: cook today's `nodes/*.rays` for every port fixture × case and write one bytes digest per output to `flow_sop/goldens/*.sexp`. Parity already holds, so no native code is involved and nothing waits on it. The port gate becomes a plain Lisp-node golden test that runs in minutes, not 30. Evidence: dbcf8551: the complete 58,773-row Lisp capture (all 128 nodes) is lib/flow_sop/goldens/data, gzip, 11 MB. @lib/flow_sop/goldens/runtest replays 819 rows (every node/fixture pair, first case) in 58 s; the full corpus is @qualification-scale. Replay lowers each expression once and needs no native factory.
- [x] P1.S2: Delete the four parity duplicates: `test_nodes.ml`, `test_lisp_ports.ml`, the per-node flow_sop tests and `test_2d_ports` with its 8 example oracles. Evidence: b58926a8 deletes lib/sop/test_nodes.ml (7.5k lines), test_lisp_ports, the per-node flow_sop parity tests, the port_tests gate, test_2d_ports' 8 example oracles and the parity setters (−21.5k lines). Non-parity checks stay.
- [x] P1.S3: Delete the 128 OCaml node modules in `lib/sop/{groups,sop_topology,attributes,shapes}.ml` and their `Support` helpers. Have the PPX emit `Param.*` and drop the `Sop` shim. Replace `sop_catalog` with `Sop.Nodes.factories` (37 entries) and move the manifest rule to flow_sop. Evidence: see SP1, SP11 and SP23. The PPX already emits Param.*; the Sop shim in support.ml stays because lib/sop cannot name itself (SP10).
- [x] P1.S4: Run `prune-dead-code` to a fixpoint over rdk. This removes the 52 oracle-only modules and the oracle parts of Group_ops, Curve_ops (keep `curve_join`), Instance_copy, Uv_ops and the rest. Also: sweep_circle `run_legacy`, Voronoi2, the adapters, test-only seam cleanup, and `*_reference` twins moved to test support. Evidence: see RK1; no *_reference twins remain in lib/rdk.
- [x] P1.S5: Delete the tests and benches tied to the deleted code (about 17k lines and 4+ benches). Trim `rdk.md` to the kernels that remain. Rewrite the AGENTS.md line 'the replaced OCaml kernels stay in lib/rdk as test oracles', update `lib/rdk/AGENTS.md` and `lib/sop_catalog/AGENTS.md`, and make the add-sop skill Lisp-first, so no agent rebuilds the parity layer. Evidence: see RK24/RK25; AGENTS.md, lib/rdk/AGENTS.md, lib/sop_catalog/AGENTS.md and the add-sop skill are Lisp-first, with the golden --update promote step.
- [x] P1.S6: Prune the dead Metal bridge: make the helpers static and run `dead-stubs` (XPC, Metal 4 state, mesh/tile). Prune the unused sdl3_ttf and mixer exports. Evidence: four Metal bridge helpers are static and drop-c-unused removed their dead code; dead-stubs reports nothing more, and the remaining Metal 4, mesh and tile surface is still named by metal.mli (PL3). The mixer export is pruned; sdl3_ttf has no dead exports.
- [x] P1.S7: Untangle the test dependencies: `lisp_sop` no longer links editor_document, and flow_sop tests move out of lib/sop. Evidence: lisp_sop has no document/runtime dependency. Lisp ports and Flow declaration/facts checks live in flow_sop; the remaining SOP facts test drops its direct Flow/catalog dependencies. All 14 port recipes pass through the preserved old-path alias (7.78 s), node facts pass at one/eight domains (1.45 s), and the remaining SOP facts and source checks pass (1.83 s / 0.66 s).

Exit gates:

- [x] P1.G1: lib/ under about 230k lines (from about 300k), counted with wc Evidence: owner decision 2026-10-28: accepted at 254.5k lines (from 332.5k, −23%); the plan's estimate predates the emitters, Lisp kernels and tests this cleanup added.
- [x] P1.G2: the `ports` alias runs under 2 minutes Evidence: per the owner, the golden lane is the 819-row corpus (c182d05a) and the full 58k corpus is dropped. @lib/flow_sop/goldens/runtest passes in 1:49 wall through check.exe, including the build, at load 18–21.
- [x] P1.G3: the manifest lists 37 native and 128 Lisp SOPs, with zero OCaml twins Evidence: flow_manifest.sexp lists 37 native and 128 Lisp SOPs, with zero OCaml twins (@lib/sop_catalog/runtest diff passes).
- [x] P1.G4: `prune-dead-code` reports nothing left Evidence: prune-dead-code reports nothing further in lib/rdk; dead-stubs reports nothing in lib/metal.
- [x] P1.G5: web parity and draw snapshots unchanged Evidence: after the merge, @lib/flow_web/runtest and @test/test_draw_snapshots pass alongside rdk, sop, sop_catalog, goldens, gate and function-lengths.

Risk: Medium. Goldens freeze today's Lisp numerics, so an intended change to a node needs a golden promote. That is the point. The risk is deleting something that is live; the compiler-driven prune is the safeguard, and the source-text graph is only a starting list.

Findings: SP1 SP2 SP10 SP11 SP12 SP13 SP23 SP24 SP26 RK1 RK5 RK12 RK13 RK14 RK21 RK24 RK25 PL3 PL26 TT8 TT18 TT21.

## P2: One vocabulary: the shared leaves

Give every concept one type in the lowest layer that needs it, so the stack rewrites in P3–P5 build on the same words.

- [x] P2.S1: Add a `diagnostic` leaf library. The original plan called for `Sop.Diagnostic.error = Diagnostic.t`; this cleanup removes the `Sop.Diagnostic` namespace entirely, so Flow and SOP APIs use one `Diagnostic.t` type and `Diagnostic.Error` exception. Delete the duplicate Flow diagnostic namespace and bridge, and keep Flow `(_, Diagnostic.t) result` boundaries. Evidence: `@check`, `@lib/diagnostic/runtest`, `@lib/flow/runtest`, and the strict SOP refusal test in `@lib/flow_sop/test_lisp_node` pass; APIs preserve the diagnostic record fields and trace prepend behavior.
- [x] P2.S2: `Param` gets one name: delete `Sop.Parameter` and `Editor_core.Param`, and add `Param.float_text` (shortest round-trip) and `Param.group_vec3`. Delete the other float printers and vec3 groupers.
- [x] P2.S3: rays_math: add `Hash`, `Color.{to_hex_string,hsl_channels,hsv,aces,Linear}` and `Rand.float_at` everywhere. Split `parallel` into its own library. Let `flow` depend on rays_math (gate edit), then move noise3 and drawing_color into `Flow.Op` and delete `Flow_ir.Operators`. Evidence: `@check`, `@lib/rays_math/runtest`, `@lib/flow/runtest`, `@lib/rdk/test_gen`, `@lib/rdk/test_mesh`, `@lib/rdk/test_boolean`, and `@lib/pxui/runtest` pass; `@test/dependency_gate` passed with direct library edges documented. Exact prior random/hash outputs are checked; microbench results are in `specification/test-times.md`.
- [x] P2.S4: Delete the 16 `Rays.*` shims, using a codemod to switch callers to `Rays_math.*`.
- [x] P2.S5: lru: add an identity-keyed bounded list, let flow_ir reach lru (gate edit), and replace the 13 hand-rolled caches.
- [x] P2.S6: input (ex frame_input): one key, button and event vocabulary. Rays.Event and Runtime_input translate into it once. `Rays.Frame` embeds it.
- [x] P2.S7: One `ensure_directory` and `write_text` (atomic), and replace the hand-written `take` copies with `List.take`.

Exit gates:

- [x] P2.G1: one diagnostic type, one Param name, one float printer. Evidence: Flow/SOP signatures use `Diagnostic.t`, schema users use `Param`, and serializers use `Param.float_text`; the diagnostic, Param, Flow, catalog and graph checks recorded in status.md pass. Display and binary32 shader formatting retain their distinct contracts.
- [x] P2.G2: no `List.filteri` prefix caches remain. Evidence: every remaining product occurrence was reviewed; they remove selected entries, sample cases or select document ranges rather than truncate caches. The final resident GPU cache uses bounded `Lru.Identity_list` and passes native ownership/eviction checks.
- [x] P2.G3: gate updated and documented in backend.md. The current gate passes
  with 61 libraries, 40 reachability rules and 23 direct-dependency whitelists.
  It validates explicit terminal backend selections after WN3. `backend.md`
  records the diagnostic, input, math, Parallel and Lru edges and the graph's
  lib/ppx/tools/test/bin source roots.
- [x] P2.G4: review the public `.mli` diff against baseline `62582188` once and
  record the review in `status.md`. The API snapshot/tool was deleted by the
  locked TT9 decision; this direct review preserves the API-review intent.

Risk: Low to medium. These are wide mechanical diffs. Do one leaf per PR and use codemods, not hand edits.

Findings: XC1 XC4 XC5 XC8 XC9 XC10 XC11 XC14 XC15 FL22 FL24 PL12 PL24 PL28 PL29.

## P3: One Flow pipeline

Check, then static stage, then lower once to Core, then place, then execute with one interpreter, one block engine and one device back end. The walker becomes a pure oracle.

- [x] P3.S1: Invert the walker dependency: remove `execute`, `core`, `bulk*` and `observe_kernel` from `Eval.st`. Flow_ir walks residuals and calls Eval only as a leaf. Promote the needed part of `Eval.Private` to `Eval.Residual`. Evidence: evaluator state has no downstream executor hooks; scoped typed effects preserve static-map specialization, and compiler-facing residual operations live directly in `Eval.Residual`. Flow and Flow_ir focused suites pass, including callback exception propagation, geometry-array resolution and particle state-map parity.
- [x] P3.S2: One dispatcher: lower each residual once. On refusal, walk exactly once to produce the diagnostic. Delete the 64- and 32-entry physical-key lists. Evidence: one bounded candidate cache plus force-scoped memoization replaces both lists; 129 distinct residuals and a repeated first residual cause exactly 129 lower attempts. An unsupported shared residual lowers once and invokes the actual walker once. Compiled state failures restore the checkpoint and rerun the reference path once, preserving diagnostics, state values and epochs.
- [x] P3.S3: Delete the graph IR (`of_evaluation`…`groups`, `compile_marks`), `counted_kernel` (emit `Invoke` for every pure map), `scalar_tree`, the `Closure` tier and `Target.web`. Evidence: the graph IR, counted_kernel, scalar_tree, the Closure tier and Target.web are absent (tier is Interp, Core, Cpu_kernel, Gpu, Gpu_compile, Gpu_readback). The scalar speed gate stays open in benchmarks/scalar-core.md.
- [x] P3.S4: `Packed.t` becomes a private alias of a validated `Core.Region.t`; delete `of_region` and `to_region`. Evidence: make/rebuild validate the private Region record; Packed is its type alias and both conversion APIs/callers are gone. Derived dependency flags are atomically memoized on first use, before worker dispatch. The full focused Flow IR suite and subsequent shared typecheck/GPU/Web checkpoint pass.
- [x] P3.S5: Shared kernel layout: `Flow_ir.Kernel_layout.plan ?fast`, used by Metal emit and WGSL, each keeping only its instruction printer. JS regions import the prims.js noise. Evidence: `@check`, `@test/dependency_gate`, `@lib/flow_gpu/runtest`, `@lib/flow_web/runtest` and the actual 1/8-worker Node check pass; fast and regular layout tests compare both printers against the shared plan, including image uniform dimensions.
- [x] P3.S6: Add traversals: `Core.reads/writes/children/iter` and `Workspace.children/map_children`, then rewrite the roughly 10 hand-written walks. Evidence: `@check`, `@lib/flow/runtest`, `@lib/flow_ir/runtest`, `@lib/flow_web/runtest`, and `@lib/flow_graph/runtest` pass; `test_core_control` checks ordered nested `Protect`/`If`/`Counted`/`Diagnose_rows`, direct reads/writes, and immediate-only workspace mapping. `tools/web_parity.exe` passes `sketches/flow_vectors.rays` (120 frames). A native benchmark on 1,247 workspace terms (ws_layout plus workspace cases; 100 full passes, median of 9, one domain, Darwin arm64) matched call-name results exactly: legacy 0.039653 s / 12,924,096 B; shared walk 0.036493 s / 12,929,696 B.
- [x] P3.S7: Placement in Flow_ir (device regions and attribute fusion), shared by native `Core_exec.draw` and the web exporter. This closes the native fusion gap listed in DEFERRED.md. Move `tools/lisp/web_export.ml` and the web runtime JS to `lib/web_export`, and the binary to `bin/rays`; the gate covers both. Evidence: Core_draw.device_groups shares scope/rate/precision/count and argument rebinding rules; native multi-output dispatch, original-order refusal, exact pixels and two-drawing lifetimes pass. TT12's product-owner moves and dependency gate pass. Literal Float range reuse is drawing-owned and bounded; paired warm million-mark moving-both frames measure 2.533/2.212 ms, one dispatch and about 27 KB allocation.
- [x] P3.S8: Widen `Core.Region` to cover the language rather than a subset: structured loops with carried registers, uint32 wraparound integers, records and vectors lowered to scalars, and inlined `defn`. CPU blocks support each one first, which removes the 'refused, runs single-threaded' path in DEFERRED.md:89. The MSL and WGSL printers follow through `Kernel_layout`. Evidence: Region covers inlined defn, records and vectors as scalars, carried loops and uint32 state. 65d12fa4 removes the E_PACKED_STATE refusal: kernels reading scalar or array state take it as a uniform or argument and run on Packed at 1 and 8 domains (a 1M value map reading state went from 406.5 to 17.2 ms per frame), and image/map with state qualifies for the GPU. The new test_core_state oracle is bit-identical to the walker. State created inside a map body stays on the walker; no sketch uses it.
- [x] P3.S9: Split the god functions: `Workspace.check`, `core_lower.lower_with`, `packed.run_all` and `flow_sop/lower.lower_checked` (into per-kind lowerings from the Operators table). One node descriptor, with `operation = key`. Evidence: Workspace.check, Core_lower.lower_with, Packed.run_all and Lower.lower_checked are each under 300 lines; SOP lowering dispatches by Operators.lowering tag. SP9's operation=key rename stays open because it changes operation identity and golden bytes.
- [x] P3.S10: Move the GPU mailbox out of flow_ir. Pass curve points as a typed array. Key the lower cache on content and drop the `Obj` key. Evidence: Flow_native.Workspace_gpu owns the mailbox/proxy/pump/cancellation; Flow_ir.Gpu is immutable configuration data passed through qualification, lowering, cooking and attribute execution. Lower uses bounded checked-content keys without Obj; live Curve points use typed Vec3 arrays and binary keys. Native worker/placement, attribute/bulk, content-cache and typed-curve checks pass. Startup is one evaluation at469 B/mark (<500); nondefault config survives real text reload/preset loading. Owner close is protected across exceptions.

Exit gates:

- [x] P3.G1: executors go from 7 to 3 (walker oracle, Core plus Packed, device) Evidence: 90d282eb deletes the Fast candidate and Core_exec.fast; the executors are the walker oracle, Core plus Packed, and the device. The root shortcut is Core_exec.direct into the same Core_exec.execute (scalar 106–115 vs 109–111 ns interleaved).
- [x] P3.G2: the walker runs at most once per residual, checked by a test counter. Evidence: `test_core_errors` counts actual reference-walker entry points, including nested Packed/Core routes; shared refusal reports one walk, compiled failure reports one, and successful compiled state steps report zero.
- [x] P3.G3: `test_workspace_ir` back under 5 minutes (from 20–25 min) Evidence: @lib/rays_editor/test/test_workspace_ir runs in 25 s wall (from 20–25 min).
- [x] P3.G4: web parity is green and native fusion is measured on bench_marks_gpu ('moving both', 14.3 ms → target ≤ 6 ms). Evidence: 91 JS goldens and 82 programs / 364 native/web frames pass; real Chrome fused marks and million particles retain max channel difference one with zero CPU invokes. Frozen paired native moving-both runs measure circles 16.018 → 2.533 ms and rectangles 15.386 → 2.212 ms on M1; all 28 cases and background-load limits are recorded in benchmarks/marks-native.md.
- [x] P3.G5: flow_ir test executables go from 30 to about 6 Evidence: ec2d7266 merges lib/flow_ir from 28 test stanzas to 7: four merged runners plus three that must stay standalone (benchmark exit mode, region snapshot, two RAYS_SLOW tests share one).

Risk: High. This is the core of the product. Mitigation: keep the walker-vs-core sweep as the regression net throughout, and land the work as a sequence of deletions behind it, with no big-bang rewrite.

Findings: FL1–FL11 FL13 FL17–FL21 FL23 FL29 FL30 FL32–FL36 FL38 SP8 SP9 SP17 SP18 XC19 TT12.

## P4: Render stack: one renderer

Separate data, renderer and window: `scene_command` is pure data, `renderer` owns GPU lowering, pipelines and per-backend shaders, and `runtime` is SDL window, surface and events. Every step keeps the OGPU seam intact for the Windows port.

- [x] P4.S1: Leave the OGPU seam as it is (virtual library, driver record, mock); the choice between them waits for the Windows port. Fold only the true duplicates: format and storage enums into `Ogpu_core.Types`, and `Command_buffer` into `Backend`. Put platform C behind per-OS stubs and mark the Metal libraries `enabled_if` macOS (WN2, WN3). Evidence: source audit, focused native/mock conformance, native token ownership, portable SDL branch build, the Metal-disabled configuration check, and a full `@check` checkpoint pass; see status.md. Terminal choices are explicit to avoid configuring Metal for mock targets.
- [x] P4.S2: Split `ogpu_metal/backend.ml` into queue, render, compute and blit encoders over a control record. Shrink `Metal_format` and the duplicate format and storage enums. Evidence: private encoder modules share the original command-session refs and retention operations; queue/submission ownership stays in Backend and Queue. Native/mock conformance and all seven Metal backend suites pass. New helpers stay within the function-length guard; enums were covered by PL11.
- [x] P4.S3: Create the `renderer` library from `scene_execution` + `rays_execution` + the MSL taken out of runtime, as `shaders/metal/*.metal` selected by `Shader.descriptor.backend`, so a Windows shader set is a sibling directory. Keep one draw type, delete the four render entry points, the `Window|Offscreen` re-wrap and the extra presentation-facts records, and use one error type. Evidence: Renderer owns the merged execution module, seven shader assets, pipelines, caches, GPU leases and stats; Runtime retains the SDL shell/facts. One `submit ?prepared` path plus retained replay replaces duplicate submission APIs. Runtime window operations are called directly by internal hosts, presentation facts have one Runtime owner, and errors use `Ogpu.Error.t`. Native 1x/2x pixels, lighting/instances, frames 1/2/60/600, resize and SDL-free offscreen checks pass; gate and shared build checkpoints pass. PL10's private modules separate lowering, retained plans and image leases.
- [x] P4.S4: Draw formats: merge Shape_batch into Ui_batch, give Marks its own token, delete Ink, merge Path, and make Shape2 float-based. Evidence: the four carriers are Render_ir, Ui_batch, Marks and Display_list; Path/Shape2 produce shared geometry. Native 1x/2x pixel, batch-order and 100k-shape checks pass (1.80 s); native/web comparison covers 82 programs and 364 frames using the real JS tessellator. Rounded-rectangle parity covers 86 cases, including fractional radii (web runtime alias, 2.58 s).
- [x] P4.S5: Move the global state in `rays` into `Sketch.run_state` and the renderer: the 8 lowering caches become Lru, input, time and window callbacks move into the loop. Make the canvas and image APIs return results, and add a bulk PNG readback. Evidence: rays has no module-level refs (P4.G4); input, time and save callbacks live in the run context; lowering caches are Lru; Canvas and Image return results; screenshot export reads packed RGBA in bulk.
- [x] P4.S6: Pathtracer: take its mesh through rdk_rays (no direct rdk dependency), and split it into mesh, build and frame modules. Evidence: callers prepare triangles/normals through rdk_rays on mesh-cache misses; the tracer consumes packed Rays.Mesh planes and depends on no RDK library. Private Mesh_data, Mesh_build and Frame_data own packing, GPU build lifecycle and frame/film layout. Native tests (5.10 s), both exact M1 image digests, window-free editor logic and the render_workspace build pass. The MSL SHA-256 is unchanged; the updated gate and integrated CLI/IR/source-guard checkpoint pass.
- [x] P4.S7: Rename Scene3 to Scene_3d, or fold it under Scene, now that Shadow3 is gone. Evidence: see XC28: Scene3 folded under Scene as Scene.D3.

Exit gates:

- [x] P4.G1: 2D formats go from 7 to 4 and pipeline families from 12 to 8. Evidence: S4's four carriers replace Shape_batch and Ink; Shape2/Path are geometry producers. Renderer exposes exactly Scene2, Scene2_textured, Scene3, Scene3_points, Scene3_textured, Scene3_world, Ui and Marks. Focused native batch/pipeline/pixel checks pass.
- [x] P4.G2: no MSL outside lib/metal, lib/ogpu_metal, `renderer/shaders/metal`, flow_gpu's Metal printer and the path tracer Evidence: the image-sink MSL moved into flow_gpu's Metal printer (Emit.image_rgba8_source). The remaining MSL is in lib/metal, lib/ogpu_metal, renderer/shaders/metal, flow_gpu's printer, the path tracer, and Metal-backend tests and benchmarks.
- [x] P4.G3: ogpu_mock conformance still passes, the seam is unchanged, and the tree configures with the Metal libraries disabled. Evidence: an isolated source snapshot selecting non-macOS Dune conditions builds the token, portable SDL stubs and OGPU mock conformance in 2.68 s without Metal build directories. This validates build selection on macOS, not a Windows toolchain. The virtual library, driver record and mock remain; terminals select their implementation explicitly.
- [x] P4.G4: `rays` has no module-level refs. Evidence: `test/dependency_gate.ml` parses each `lib/rays/*.ml` structure and rejects refs allocated in module initializers while allowing function-local refs; its negative self-check covers both cases. The current tree has no hits and `@test/dependency_gate` passes. DLS keys are immutable per-domain binding handles; their mutable values are the explicitly scoped run/cache contexts. Atomic resource IDs remain process-unique because handles can outlive an individual run.
- [ ] P4.G5: the native qualification footprint is unchanged and smoke-all is green

Risk: Medium. Presentation and Retina scaling are subtle. Run `@runtest-native` and ui_shot comparisons once per merged step, not per commit.

Findings: PL5 PL7–PL15 PL17 PL18 PL22 PL23 PL25 PL27 PL31 PL32 PL34 WN1 WN2 WN3 WN9 WN10 XC6 XC7 XC23 XC28 (PL6 deferred to the port).

## P5: Editor and UI: real modules

The editor is composed from libraries with honest boundaries. It does not host the Flow runtime or SOP glue, and it is not one 6k-line include chain.

- [x] P5.S1: Move non-editor code out: `flow_native` (Drawing, Marks_scene, Workspace_*, Live_frame) and `rdk_rays` (Surface, Packed_pieces). Doc goes to `editor_document.Document_edit`, Gate to `input`, and the pure part of lisp_text next to Flow.Syntax (one tolerant lexer). All `copy_files` go away. Evidence: the actual implementations and callers use these owners, Flow_native has no UI dependency, and the copy/production-test-support guards pass. Parser/text-pane, probe/document/editor consistency, native draw snapshots and exact PXUI parity checks pass. The material carry fixture keeps cooked document and real gesture checks while avoiding redundant scene rebuilding and sleep-based simulation (6.1 s total; carry 1.56 s).
- [x] P5.S2: Break the `core_*` include chain into explicit modules over `Core_model.t`: Panes, Inspect, List_view, Text, Status, Reduce, Frame. Fold in Environment_*. Rename Core, Environment, Leader and Contexts. Evidence: no include chain; modules open what they use over the editor model, and the god modules are renamed (Editor_frame, Editor_loop, Editor_commands, Lowering) with Loop_model, Loop_source and Loop_view.
- [x] P5.S3: Split `update_frame` per pane, `reduce` into per-intent reducers that return values, and `scope_pane.update` into gesture handlers. Use one `Panel_map` instead of active and parked pane copies. Evidence: update_frame and reduce are split under 300 lines into per-pane and per-intent functions, scope_pane's gestures live in Scope_frame handlers, and one Panel_map replaces the pane copies (UI26).
- [x] P5.S4: pxui: split into Ui (engine), Ui_text and Ui_kit. One kit for shell, graph and inspector. Move Camera_controls into the viewport. Delete Kit and num_field. Evidence: see UI11 and UI9; Camera_controls moved into the viewport (UI12); Kit.number and the num_field duplicate are gone.
- [x] P5.S5: One input path: World drag, viewport grab and graph hints become `Ui.box`/`capture`, with key edges taken from the Router. Evidence: see UI13: one input path; no raw pointer fold remains for the World drag, viewport grab or graph hints (Focus_lost is still read as a window event).
- [x] P5.S6: editor_document: split Contexts into Kinds, Lowering and Shell_eval; drop digestif; make the workspace a record instead of a tuple; pass caches explicitly; add `.mli` files. Scene, world and settings kinds stop being SOP nodes. Evidence: Kinds, Lowering and Shell_eval are split out of Contexts, the workspace is a record, caches are explicit, and all 16 editor_document modules have an .mli. digestif stays because Lowering.sha256 is the only SHA-256.
- [x] P5.S7: Move the panel kinds and layout gestures out of flow_graph and into editor_document. Route the derived-node inspector through workspace_inspector. Evidence: see FL28; flow_graph knows no panel kinds (Editor_document.Panel_kinds) and no layout ops.

Exit gates:

- [x] P5.G1: rays_editor direct dependencies go from 22 to about 15, with no rdk, flow_gpu or scene_command Evidence: rays_editor declares 17 libraries (from 25), none of them rdk, flow_gpu or scene_command. Probe forcing moved to Flow_native.Probe_run and the GPU policy to Workspace_gpu.policy.
- [x] P5.G2: no file in lib/ includes a sibling to extend it Evidence: no lib file includes a sibling to extend it. The rdk_test_support and test shims open their module and export only their extras (X_front). What remains are re-export facades of disjoint modules (pxui/ui.ml, subdivide.ml, Flow_edit, Scope_pane, Metal_raw) and Edit_tree, whose .mli restricts its surface.
- [x] P5.G3: no `copy_files` of lib sources. Evidence: actual helper ownership replaces the product copies; existing IR fixture modules form a private test-only library. The dependency gate rejects library source copies and production dependencies on that test support, with negative fixtures.
- [x] P5.G4: no function over 300 lines in rays_editor, pxui or pxui_graph Evidence: no function in rays_editor, pxui or pxui_graph, product or test, is over 300 lines; the last four test rows are removed and @tools/codemod/function-lengths is clean.
- [x] P5.G5: ui_parity goldens are pixel-identical. Evidence at the P5.S1 checkpoint: `@lib/pxui/test_ui_parity` reports exact overlays, zones, table and kit-panel parity; subsequent UI refactors must retain this proof.

Risk: Medium. Pure refactors under strong pixel goldens (`test_ui_parity`) and the shell suite, which runs in full now that P0 wired the slow sections into a lane.

Findings: UI1–UI6 UI9–UI18 UI21 UI22 UI25–UI29 UI31 UI33 UI34 UI36 SP16 FL28 XC16 XC25 XC27 TT14 WN4.

## P8: Path tracer as a .rays file, plus a SPIR-V emitter

Delete `pathtrace.metal`. The path tracer is a plain `.rays` program; placement puts its pixel map on the device, and each target emits it. No GPU-only language features.

- [x] P8.S1: Benchmark first: today's kernel, window-free, on `pathtracer.rays` (480 × 840) and shattered-studio at fixed spp and bounces, interleaved A/B. This is the bar. Evidence: benchmarks/pathtracer-lisp.md: pathtracer.rays at 480 x 840, 16 spp, five rounds, median 0.520 s per round, one RGBA8 hash. status.md records the frozen shattered-studio five-round run (1600 x 1600, 363.95 s).
- [x] P8.S2: Add one host op, `scene/trace`, returning a hit record. CPU: `Surface_index.raycast_many_into`. Metal: `intersector`. SPIR-V: `OpRayQuery*`. Spheres are an in-loop procedural test, which drops `Function_tables`. Add the accumulation-buffer sink (reuse `image_sink` for RGBA8). Evidence: scene/trace returns a hit record on CPU (Surface_index host), Metal (intersector over a Kernel_layout scene binding) and SPIR-V (ray query). It covers instancing, procedural spheres and strands as in-loop analytic tests, and vertex and instance motion (536acd94, 64f0b712). Film accumulation is a bound in-place kernel plus resolve and Image_sink. Native tests match the CPU within 1e-5.
- [x] P8.S3: Write `pathtrace.rays` from the MSL. The CPU run is the oracle; the GPU film matches it within tolerance. Port `resolve_preview` first, then `trace_pixel`, each slice A/B-gated. Evidence: pathtrace.rays trace-pixel covers every scene shape: flat, instanced and scene meshes, spheres, strands, instance motion, the World map, rect lights, bevel and preview reprojection with resolve. lib/rays_pathtracer/lisp_kernel.ml compiles one kernel set per scene shape (four cached), with scene facts as literals and arrays and camera bound per frame. On the test scenes the GPU film is byte-identical to the CPU walker.
- [x] P8.S4: Delete `pathtrace.metal`, `embed_metal.ml`, the TRACE variants and the intersection-table path. Evidence: 43da74db deletes pathtrace.metal, embed_metal.ml and the TRACE variants; the path tracer no longer uses intersection function tables (the unused OGPU table API is tracked as WN9).
- [x] P8.S5: SPIR-V emitter: binary words over the same IR and `Kernel_layout`, with `spirv-val` on goldens in tests. It joins MSL, WGSL and JS as just another emitter. Evidence: lib/flow_gpu/spirv.ml writes binary SPIR-V words over the shared IR and Kernel_layout; test_spirv validates 21 deterministic goldens with spirv-val (Vulkan 1.1).

Exit gates:

- [x] P8.G1: no `.metal` file in lib/rays_pathtracer Evidence: lib/rays_pathtracer has no .metal file.
- [x] P8.G2: GPU ms/frame ≤ today on both scenes (A/B, ≥ 5 runs, M1), and pipeline compile time not worse Evidence: GPU ms per 16 spp, interleaved: pathtracer.rays −0.6%, shattered_studio 800² −9.7%. Pipeline compile: with the kernel disk cache (key: executable, kernel text and scene facts) the Lisp front end takes 0.3–0.4 ms per kernel, Metal pipeline creation 0.3–1.2 ms; cold Metal cache first frame 518–550 vs 1,414–1,506 ms. The warm first frame is 208–218 ms (median about 211) vs 198.5–207 ms (about 201) for the old kernel; the roughly 10 ms is a major GC cycle from document loading landing in the World bake, and with a forced GC both are within 1–5 ms.
- [x] P8.G3: film RMSE ≤ 1e-4 against today's kernel after N spp Evidence: owner decision 2026-10-28: judged by self-consistency. pathtracer.rays meets RMSE 1e-4 (6.9e-5 at 64 spp). On shattered_studio the Lisp kernel's difference (4.8e-4 at 512 spp, unbiased, falling with spp) is at the deleted kernel's own regrouping noise (3.8e-4).
- [x] P8.G4: every SPIR-V golden passes `spirv-val` Evidence: @lib/flow_gpu/runtest runs spirv-val on every SPIR-V golden and passes (3.4 s).

Risk: Medium. This is the hottest kernel. The benchmark lands first, the CPU run of the same .rays file is the oracle, and the old kernel stays until the last slice matches it.

Findings: WN1 WN9 WN10 WN11 FL6 FL17 PL22 PL23.

## P9: Raster shaders as .rays files

Delete the last handwritten shaders: the MSL strings in `runtime.ml` and `runtime_shaders.ml` (Scene3 vertex/fragment with textured, argument-buffer and sun variants, the UI pipeline, the marks pipeline) and the `image_sink` kernel. A vertex stage is a map over vertices and a fragment stage is a map over fragments. They are two more placements of the same region IR, emitted by the same MSL and SPIR-V emitters.

- [x] P9.S1: Benchmark first: editor frame time (window-free GPU timing, ui_shot scenes) and a Scene3 scene, interleaved A/B. Evidence: b3e5bf63: UI_SHOT_BENCH=N in tools/ui_shot.exe prints per-render GPU ms (Rays.Canvas.gpu_seconds), with medians and minima; the baseline is in specification/cleanup/benchmarks/raster-lisp.md.
- [x] P9.S2: Region IR: stage entry points (vertex, fragment) and interpolated outputs between them, plus texture and sampler reads (the glyph atlas, material textures). Blending, depth and culling stay pipeline state in OGPU, not shader code. Evidence: 14f96583/923624a0: the region IR has fragment intrinsics (gpu/fwidth, gpu/sample, gpu/texture-size); a function returning a record of numbers gives the varyings; Emit.stage_body and Flow_gpu.Raster print vertex and fragment stages ([[stage_in]], [[position]], [[flat]], discard, textures), and the Vulkan SPIR-V modules pass spirv-val. CPU, JS and WGSL refuse the stage intrinsics with E_GPU_STAGE.
- [x] P9.S3: Port in order of risk: the marks pipeline, then UI (guarded pixel for pixel by `test_ui_parity`), then Scene3 and its variants (folded by static facts like the path tracer's), then sun, then the image sink. Evidence: 80b8dde1 ports every raster pipeline in risk order: marks, UI, Scene3 plain, points and textured, sun depth, Scene3_world, Scene2's argument buffer and the image sink (now one full-screen triangle). The 3D winding bug (counter-clockwise winding was tied to vertex slot 7) is fixed per pipeline family (a68b3e9a).
- [x] P9.S4: Delete `runtime_shaders.ml`, the MSL strings in `runtime.ml` and the `renderer/shaders/metal` directory planned in P4. P4's per-backend directory then only ever holds generated output. Evidence: lib/renderer/shaders/metal holds only generated files; the image-sink MSL in emit.ml and the path tracer's copy are deleted.

Exit gates:

- [x] P9.G1: no handwritten MSL, HLSL or SPIR-V anywhere in lib/ Evidence: f6fde32c moves the Perlin noise3/fbm helpers into lib/flow_gpu/noise.rays (spliced per kernel; GPU output byte-identical on 18 kernels; 32-octave fast 47.8→13.6 ms) and fmod64 into IR integer mod. grep of lib/ finds MSL/SPIR-V text only in the two printers, printer goldens, test-only libraries, tests and generated .metal/.spv files.
- [x] P9.G2: `test_ui_parity` goldens are pixel-identical Evidence: test_ui_parity is exact, and ui_shot frames are identical outside the status bar at 1x and 2x on 18 sketches.
- [x] P9.G3: editor and Scene3 GPU frame time no worse than today (A/B, ≥ 5 runs) Evidence: GPU min ms at 2x, interleaved: shattered_studio 12.12–12.63 Lisp vs 12.85–13.06 handwritten; ws_bloom 5.95–6.02 vs 5.86–6.15; ws_layout 3.83 vs 3.84; drawing 0.741 vs 0.742; flow_image_kernel about 2.29 vs 2.30.
- [x] P9.G4: the Windows backend needs zero shader work beyond the SPIR-V emitter Evidence: SPIR-V comes from the same IR for every compute and raster stage (renderer, presentation blit, compaction, trace and noise helpers); no handwritten shader remains, so a Vulkan backend needs only the SPIR-V emitter.

Risk: Medium. UI is drawn every frame, but it has the strictest net in the repo (pixel goldens).

Findings: WN1 WN11 PL8.

## P6: rdk: shape what is left

After P1, rdk holds only live kernels. Give it one error model, one parallel and cancel helper, one storage map, and a three-library structure.

- [x] P6.S1: Errors: one internal `Rdk_error of Error.t`, converted to `result` once at the boundary. Delete `unguard`, `of_string`, the `_raw` twins, the trailing guard wrappers and the 54 local exceptions. Evidence: see RK3.
- [x] P6.S2: One `Support.parallel_for/parallel_ranges` and `Cancel.poll` with a single cadence, applied by codemod to the 886 sites. Evidence: uv_parameterize uses Support.run_ranges; there is one range helper (Support.run_ranges / parallel_for) and one cancel check (Cancel.check_opt).
- [x] P6.S3: `Attribute.map_float_planes`/`planes` to collapse the per-storage arms. One generic median BVH and one `Point_grid`. Merge the two boolean payload modules. Evidence: see RK8, RK9, RK10 and RK15.
- [x] P6.S4: Kernel parameters become required records with no defaults, so the declaration is the single source of defaults. Evidence: 431 → 121 optional parameters: every public rdk kernel SOPs call takes a required params record, with defaults in the SOP declarations and optional-argument fronts in rdk_test_support. The remaining optionals are boolean stage builders, internal helpers behind converted entries, and general constructors and indexes.
- [x] P6.S5: Restructure into `rdk_core`, `rdk_geom` (exact plus spatial), `rdk_ops` (live attrib, gen, curve and mesh) and `rdk_boolean`, and rename `Private` to `View` for the borrowed-plane API. Evidence: fa4541bc: rdk_exact+rdk_spatial are rdk_geom, and rdk_attrib+rdk_curve+rdk_mesh are rdk_ops (include_subdirs); the gate library count went from 59 to 56. Private is View (RK17).
- [x] P6.S6: Tests: local fixtures, one support library opened everywhere, and `parallel_exact` shared. Evidence: see RK23; rdk tests share one support library.

Exit gates:

- [x] P6.G1: rdk under about 80k lines including tests Evidence: lib/rdk is 79,756 lines including tests (product 57.8k, tests 19.4k, support 2.5k), from about 144k at baseline. Attribute_promote, Attribute_relation, the multi-key Ordering sort and Smooth's unused modes are gone with their test-only callers; the path tracer digests, goldens and draw snapshots are unchanged.
- [x] P6.G2: no `unguard`/`_raw` pairs Evidence: grep for unguard or _raw in lib/rdk finds only local helpers (add_raw, insert_raw, group_of_raw), none of them guarded twins.
- [x] P6.G3: one parallel helper Evidence: see P6.S2.
- [x] P6.G4: rdk test alias under 30 s warm Evidence: after touching an rdk test, a relink and run of every rdk test group took 21.9 s wall on the loaded machine.

Risk: Medium. Exactness and byte-identical parallel results must hold. Every helper swap carries the existing 1-domain vs multi-domain checks.

Findings: RK3 RK4 RK6–RK11 RK15–RK20 RK22 RK23 RK26–RK30 RK32 XC22 XC24.

## P7: Repo shape and API surface

The tree says what is product and what is tooling, and the public API is small on purpose.

- [x] P7.S1: Tests live next to their libraries; test/ keeps only the cross-cutting checks (gate, snapshots). Move lisp_sop, workspace_parity and ogpu_conformance to their owners. One generated rule for the snapshot pairs and the test_main sections. Evidence: bac1955e moves editor tests and workspace_parity into lib/rays_editor/test, lisp_sop into lib/flow_sop/lisp_sop and conformance into lib/ogpu/conformance; test/ keeps the gate and the draw snapshots. The moved executables run (their output lines appear).
- [x] P7.S2: Shrink `Private`: within a library use `private_modules`; across libraries use real names or a gate-listed `Unstable`. The allowlist must shrink every PR. Evidence: the gate resolves module aliases and local submodules before counting a cross-library .Private. use, and rejects any not in test/private_allowlist.ml. Eval.Private→Eval.Host, Packed.Private→Packed.Host, and Noise, Marks, Ui_batch, Render_ir, Op, Value, Sop.Image/Kernel, Metal, Runtime_resources and Rays.Image values got public names.
- [x] P7.S3: Scope `api_stable.json` to Rays and Flow, or delete it. Evidence: locked TT9 deletes the snapshot, generator, Dune stanza and promote skill; follow-up source scans remove the remaining promote instructions from the Metal/OGPU/RDK workflows. Product/build/test/workflow sources retain no API snapshot or promote-step references.
- [x] P7.S4: Config: parse the environment once in `bin/rays` and the runners, and remove the `getenv` reads from lib/ (gate token scan). Evidence: see XC12; test runners read their own variables, and the putenv calls are gone from the tests.
- [x] P7.S5: Tools: one `bench_rdk` with `bench_util`. Delete dedupe after its last use, remove the leftover migration codemods, keep only the SDL lock and generate in packaging, and rename `core_fixtures`. Evidence: see TT11, TT19 and TT30; core_fixtures is prims_fixtures.
- [x] P7.S6: Sketches: move the fixtures to `workspace/cases`, add headers, and fix the library table and nested-file list in AGENTS.md. Evidence: ce7a7602: six fixtures moved to specification/workspace/cases, 18 sketches gained a header comment, and the AGENTS.md library table and nested-file list are fixed.

Exit gates:

- [x] P7.G1: test/ under about 3k lines Evidence: test/*.ml is 1,018 lines (from 18,787).
- [x] P7.G2: `Private` cross-library allowlist under 20 entries (from 2,100 references) Evidence: test/private_allowlist.ml has 15 rows (from 2,100 references at baseline).
- [x] P7.G3: zero `RAYS_*` reads in lib/ Evidence: grep for getenv of RAYS_ under lib matches only test_*.ml runners; the gate enforces it. Focused gate, PXUI, rays, editor-logic and smoke checks pass.
- [x] P7.G4: AGENTS.md matches the tree (link check green) Evidence: codemod.exe spec-links specification AGENTS.md lib test tools passes; AGENTS.md lists editor_document and the test-only helper libraries.

Risk: Low.

Findings: XC2 XC12 XC17 XC26 RK17 SP21 SP27 PL30 UI35 UI37 TT9 WN6 WN7 WN8 TT11 TT13 TT15 TT16 TT19 TT29–TT32 TT34 TT38.

## Source detail: principles

### 02Principles for the cleanup

Delete before you move. A module that will be deleted is never refactored, renamed or relocated first. The phase order follows from this.

One declaration, one executor, one format. This is the AGENTS.md rule ("Lisp decides", "one declaration") applied to the code. A second representation is allowed only as a frozen golden, never as live code.

Lisp is the SOP surface; parity code goes. Once a Lisp node is accepted, its own output, frozen as a golden digest, is the regression net. The OCaml twin, the rdk kernel it called and every parity test are deleted. Only the 37 native SOPs stay OCaml.

Portable seams stay. A Windows port is planned. The OGPU driver record, the virtual `ogpu` library, `ogpu_mock`, the backend-tagged `Shader.descriptor` and `native_layer_token` are kept even where only Metal uses them today. Cleanup simplifies the code above and below these seams, never across them.

Private is not an API. If production code in another library calls `X.Private`, the boundary is wrong. Either make it public under a real name, or use dune `private_modules`.

Docs describe the present. Plans, results and session logs live in git history. A spec that contradicts AGENTS.md is a bug.

No new abstraction without two callers. The target structure introduces exactly three new modules or libraries: `Kernel_layout`, `flow_native` and a `diagnostic` leaf. Each replaces at least two copies.

Every phase ends green and smaller. Each phase has to pass `check.exe --ship`, the parity gates that matter (web parity, draw snapshots, goldens), and show a measured drop in lines and test time.

## Source detail: windows

### 04bWindows readiness

The port itself is out of scope for this plan. The plan's job is to avoid making it harder, and to make it cheaper where that costs nothing extra. The good news: OGPU was designed backend-agnostic, and most of it already is. Everything Metal-specific sits in a few known places.

     |  | Seam | State today | What the cleanup does |

       | GPU driver | Ogpu_core.Backend.driver record + virtual `ogpu` (default_implementation ogpu_metal) + ogpu_mock | Kept as is. PL6 no longer deletes the virtual library. At the start of the port, decide whether each platform picks its implementation at link time (virtual library) or at run time (driver record). Until then, nothing changes. |

       | Capabilities | Caps: Ray_tracing, Ray_tracing_curves, Function_tables, limits | Kept. Metal-only features (the path tracer's RT and function tables) stay gated by `Caps.require`. A backend without them must leave the editor working without the render pane (WN4). |

       | Shaders | Backend-tagged `Shader.descriptor`, but MSL text sits in runtime.ml, runtime_shaders.ml, flow_gpu/emit.ml, image_sink.ml and pathtrace.metal | P4 moves the runtime MSL into `renderer/shaders/metal/`, selected by `descriptor.backend`, so a second language is a sibling directory. P3's shared `Kernel_layout` means a Flow kernel for a new backend is one instruction printer, not a third copy of the layout planner (WN1). |

       | Formats | 4 enums (Metal_format 130 cases, Texture.format, Validation.format, Types.texture_format) | `Ogpu_core.Types` becomes the one backend-neutral enum. Metal_format shrinks inside the Metal backend only (PL11). |

       | Native window handle | native_layer_token links CoreFoundation; sdl3_cocoa.c sits inside the sdl3 binding | Keep the token as the seam, and put its C and the Cocoa titlebar code behind per-OS stubs selected by dune (WN2). |

       | Build | metal/discover.ml and build_bridge.ml call /usr/bin/xcrun; 4 dune files link -framework | Mark `metal` and `ogpu_metal` `enabled_if` macOS, so a Windows build skips them instead of failing (WN3). |

       | Window-free tests | ogpu_mock + test/ogpu_conformance | Conformance moves to lib/ogpu and runs against every backend, so the Windows backend is checked by the same suite on day one (WN7). |

       | Rules | AGENTS.md "ships native Apple-Silicon Metal only"; gate rule "Metal. stays within the backend" | Generalise the gate rule to "a backend's modules stay within that backend", which covers the Windows backend automatically. Update the AGENTS direction text when the port starts (WN8). |

### Backend recommendation: Vulkan with hardware ray queries

The Windows build has to use hardware ray tracing on NVIDIA, so WebGPU is out: it has no ray tracing. One Windows backend, `ogpu_vulkan`, covers raster, compute and RT. The reasons:

- Same model as today. The path tracer is one compute kernel with an inline `intersector` (`pathtrace.metal:792`). `VK_KHR_ray_query` is the same model: rays traced from a compute shader, with no separate RT pipeline or shader binding table.

- Procedural spheres need no function tables. Metal intersects them through `intersection_function_table`. With ray query, an AABB hit comes back to the shader loop as a procedural candidate, which the kernel intersects and commits itself. Function tables become a Metal-backend detail.

- A plain C API. It fits the repo's registry-generated binding approach (`lib/metal/gen`) far better than D3D12's COM. SDL3 already creates Vulkan surfaces, and the same backend would later run on Linux.

- Vendor extensions are available. NVIDIA exposes ray-tracing motion blur on Vulkan, and core DXR has no equivalent. Where an extension is missing, the capability is off and the path tracer degrades instead of failing.

- Shaders are generated, not handwritten. Decided 2026-10-27: the path tracer is written in Rays Lisp and compiled like every other Flow kernel (section 04c). Rays emits SPIR-V words directly from its kernel IR, just as it prints MSL today. Nothing ships a shader compiler, and there is no `.metal` or HLSL source to keep in step.

     |  | Path tracer feature (Metal today) | Vulkan on NVIDIA | OGPU change |

       | Triangle BLAS, instanced TLAS, user ids | Core `VK_KHR_acceleration_structure` | None. `Acceleration.descriptor` is already neutral, and `instance_layout` is supplied by the backend. |

       | Inline `intersector` in a compute kernel | `VK_KHR_ray_query` in a compute shader | None. Add a `Ray_query` cap if useful. |

       | Procedural spheres via intersection function tables | AABB geometry with an in-shader candidate loop | Spheres stop requiring `Function_tables` (rays_pathtracer.ml:803, 1103, 1136). Tables stay inside the Metal backend. |

       | Instance and vertex motion blur | NVIDIA motion-blur extension where present | Add a `Ray_tracing_motion` cap; without it, render at the shutter midpoint. |

       | Native curve primitives | Not in core Vulkan; AABBs per segment plus in-shader intersection (or a vendor extension on new hardware) | `Ray_tracing_curves` already exists. The Vulkan backend reports it false, and strands go through AABBs. |

The Windows GPU work is one SPIR-V emitter over the same kernel IR the MSL printer reads, sharing `Kernel_layout` (FL6). Metal compiles MSL text at run time (`newLibraryWithSource`, metal_bridge.mm:2853). Vulkan accepts only SPIR-V binary, so Rays writes the binary words itself. `spirv-val` runs in tests only. The runtime raster shaders (the MSL strings in runtime.ml) become .rays in P9, so the Windows backend needs no handwritten shaders at all.

## Source detail: pathtracer

### 04cThe path tracer in Rays Lisp

Goal (owner, 2026-10-27): no `pathtrace.metal`. The path tracer is a `.rays` program. It is lowered by the Flow pipeline and emitted as MSL on macOS and SPIR-V on Windows. It must be no slower than today's 908-line kernel, and ideally faster.

### Principle: one Lisp, one IR, emitters

A path tracer is not a special GPU language. It is an ordinary Rays program whose map over pixels is placed on the device. Every target is just an emitter over `Core.Region`: CPU blocks (Packed), MSL, SPIR-V, WGSL and JS. Nothing is added that only one target understands.

- Widen the one IR, not a GPU dialect. `Core.Region` (core.mli:107) is straight-line per-element code today. Loops, carried state and records are already Lisp on the CPU walker; the region IR just cannot hold them. That same gap is why `Packed.compile` refuses state and dynamic loops and sends them to the slow single-threaded path (DEFERRED.md:89). Widening it is P3 work, and the CPU benefits first.

- `scene/trace` is a host op on every target. On CPU it calls rdk's existing `Surface_index.raycast_many_into` (surface_index.mli:115). On Metal it is `intersector`; on SPIR-V it is `OpRayQuery*`. The whole path tracer therefore runs on the CPU too, and that run is the oracle; no separate reference kernel is written.

- The same function constants are just static folding, which already exists.

### What the region IR must gain (all targets)

     |  | pathtrace.metal uses | Where | Region IR today | Added once, emitted by every target |

       | Loops with carried state: samples, bounces, lights, panels, CDF binary search, 3×3 resolve | :446 :472 :483 :167 :331 :886 | none | Structured loops with loop-carried registers (`for` and `while` with merge points). This is the largest piece. |

       | Mutable RNG state (`pcg(thread uint&)`) | :149-155 | `Rand_at` only (stateless) | A uint32 state threaded through loop-carried registers. `Int` registers exist; they need uint32 wraparound semantics on the GPU. |

       | vec3/vec4 math, records (Material, Light, Panel, Uniforms) | :119-147 | scalars plus component inputs | Vectors and records lowered to scalar registers (scalar replacement), so the emitters stay scalar. |

       | Helper functions (GGX, Fresnel, pdfs, basis, sampling) | :157-389 | none | Lisp `defn` fully inlined at lowering. These need no GPU feature, and the walker can run them on the CPU as an oracle. |

       | Typed buffer reads of records | materials, lights, panels, world CDFs | `Gather` (float table, index, width, component) | Gather from device buffers with a record layout. Mostly there. |

       | Ray query with instancing, motion, curves and procedural spheres | :792-870 | none | One host intrinsic, `(scene/trace scene origin dir tmin tmax time)`, returning a hit record. Each emitter expands it to Metal `intersector` or SPIR-V `OpRayQuery*`; spheres are an in-loop procedural test on both. |

       | Function constants (INSTANCED, MOTION, SPHERES, CURVES, SCENE → 6 tables, 25 variants) | :14-31, :838 TRACE macro | static stage folding | Free. The static stage already folds known values, so scene facts specialise the kernel before emission and dead branches never reach the GPU. |

       | Film accumulation, `uchar4` resolve | :389, :874 | display and readback sinks | A read-modify-write accumulation buffer sink and an RGBA8 output (`image_sink.ml` already writes RGBA8). |

### Where the speed comes from

- Specialization beyond function constants. The static stage can fold per-scene facts that Metal function constants cannot: light count, panel count, all materials non-metallic, no world map, bounce count. Loops with known trip counts unroll, and unused BSDF lobes vanish. Today these are uniforms that the kernel branches on every sample.

- The same arithmetic, held in binary32. Kernels stay in GPU binary32 like today's MSL. Device kernels already run in the approximate tier, so double-precision emulation never applies here.

- Cheap experiments. Once the kernel is Lisp, a wavefront split (separate trace and shade kernels) or a different RNG is an edit to a `.rays` file, measured with the same benchmark.

### Gates

- Performance: GPU ms per frame on `sketches/pathtracer.rays` (480 × 840) and the shattered-studio scene, on M1, at fixed spp and bounces. Generated ≤ today's, interleaved A/B over ≥ 5 runs (window-free timing, as in the existing GPU-timing notes). Pipeline compile time must not regress either. Record the numbers in `specification/performance.md`.

- Image: same PCG sequence and sample order. Fast-math may reorder operations, so the bar is a tolerance on accumulated film (RMSE ≤ 1e-4 after N spp), not bit identity. Every helper function is checked CPU (walker) against GPU on random inputs.

- Deletion: `pathtrace.metal`, `embed_metal.ml` and the Metal-only `Function_tables` requirement are gone.

Cost: the IR widening lands in P3, where it is owed anyway. What is left for P8 is porting one `.rays` file, one host op and one emitter.

## Source detail: guards

### 07Guardrails, so it does not grow back

Every guardrail below is a check in an existing tool (the dependency gate, `check.exe` or `tools/codemod`). None of them needs a new framework. They target the specific ways this code base accumulated cruft under fast agent-driven development: 233 commits on 2026-10-11 alone, and +408k / −312k lines in October.

     |  | Check | Where | Fails when |

       | Every test executable is attached to an alias that some lane runs | test/dependency_gate.ml | An executable or test-group label is not reachable from runtest, runtest-native or qualification*. This would have caught `rdk_parts`, `test_world_command` and the RAYS_SLOW sections. |

       | No `copy_files` of library sources | dependency_gate | A dune file copies a `.ml` from another library's directory. |

       | `Private` usage across libraries is listed | dependency_gate | Another library adds a new `X.Private.` reference that is not in an explicit allowlist. The allowlist shrinks each phase. |

       | Gate covers `tools/`, `test/`, `bin/` | dependency_gate | Product code in those directories crosses a "may never reach" rule. |

       | No plan ids or dates in code comments | check.exe --ship | A comment in `lib/**/*.ml{,i}` matches `\bP\d+\.\d+\b`, `\bG\d+\b` or `20\d\d-\d\d-\d\d`. |

       | Function length budget | tools/codemod (compiler-libs, already a dependency) | A new top-level binding is over 300 lines. The existing ones are listed with their current length, and that list may only shrink. |

       | Formatting | .ocamlformat + dune fmt | A file is golfed (`let x=…`). Today 119 files have 20 or more such lines. |

       | No `Sys.getenv "RAYS_…"` in lib/ | dependency_gate token scan | A library reads the environment instead of receiving config. The allowlist starts at 8 reads and goes to 0. |

       | Spec links resolve | check.exe --ship | A markdown path under `specification/` or in `AGENTS.md` points at a missing file. Today 342 such references are dangling. |

       | DEFERRED.md stays a list of open items | review rule | An item says "fixed", "confirmed" or "verified". Those items get deleted. |

## Source detail: appendix

### 10Appendix

### A. Flow execution paths today

     |  | # | Tier | Entry | Called by | Role → target |

       | 1 | Tree walker | Flow.Eval.static / force / Private.force_reference / rerun | everything; every fallback | Oracle and general fallback → pure oracle, runs once, only on failure |

       | 2 | Static bulk | Flow_ir.static → Eval.Private.static_with_kernels | contexts.ml, qualify_workspace | Shortcut for maps over 4,096 elements → keep, called through the new dispatcher |

       | 3 | Executor dispatcher | Flow_ir.Executor.compile / force / force_display | flow_sop, core_scope, drawing.ml | Builds a graph IR nobody reads → single dispatcher, lowers each residual once |

       | 4 | Core statements | Core_lower.lower → Core_exec.prepare / execute | 3, flow_sop/lower.ml:253 | Product → keep as the one interpreter |

       | 4a | Scalar stack VM | Core_exec.scalar_tree / code_value | inside 4 | Micro-optimisation with no benchmark → delete |

       | 4b | Counted block compiler | Core_exec.counted_kernel | maps inside state steps | Second block engine → delete; lower these maps as `Invoke` |

       | 5 | Draw program | Core_draw.lower → Core_exec.draw | rays_editor/drawing.ml | Native 2D → keep, sharing placement with web |

       | 6 | Packed region blocks | Packed.force / force_all / force_into | 3, 4, 5, flow_sop | The CPU block engine → keep as the only one; `Packed.t` = validated Region |

       | 7 | Metal GPU | Flow_gpu.Emit / Run / Host | rays_editor workspace_* | Device back end → keep, sharing Kernel_layout with WGSL |

       | 8 | Web | Flow_web.Js + Wgsl + tools/web/runtime | tools/lisp/web_export.ml | Separate target → keep, moved to `lib/web_export` |

       | — | Graph IR | Flow_ir.of_evaluation / optimize / place / compile_marks | tests, bench_core, workspace_parity | Effectively dead → delete (~500 lines) |

### B. Adding a SOP: touch list

Today, native OCaml

- A PPX module in `lib/sop/<family>.ml`

- An alias in `lib/sop/nodes.ml`

- A signature in `lib/sop/nodes.mli`

- `[@@sop.register]` in `lib/sop_catalog/sop_catalog.ml`

- `dune promote` of `flow_manifest.sexp`

- A case in `test_nodes.ml` (which runs only in qualification-scale)

- Possibly test_sop_catalog, sop.md and the rdk op itself

Today, Lisp
- `nodes/<key>.rays`
- Manifest promote
- A new `test_*.ml` plus a stanza in a 304-line dune file, or a port file with a native oracle

Target

- Lisp (default): `nodes/<key>.rays`, plus a manifest promote, plus a section in the single flow_sop test runner.

- Native (the exception): one module in `lib/sop/nodes/`, plus one entry in `Sop.Nodes.factories`, plus a promote.

The `nodes.mli` aliases, `sop_catalog`, `[@@sop.register]` and the `Sop` shim module all go away. The `add-sop` skill is rewritten Lisp-first.

### C. rdk oracle-only inventory

     |  | Family | Lines | Fully oracle-only modules (.ml lines) |

       | mesh | 11,363 | Edge_transport 953, Edge_ops 763, Point_split 701, Revolve 679, Sweep_circle 674, Polyframe 624, Poly_bridge 609, Poly_loft 547, Group_path 509, Motion 494, Separate_pieces 463, Extract_point_curve 402, Extract_centroid 395, Duplicate 393, Edge_flip 351, Bound 319, Match_size 309, Edge_relax 304, Curvature 303, Crease 286, Rewire_vertices 281, Laplacian 277, Surface_metric 191, Mirror_geometry 170, Blast_by_attribute 139, Edge_constraints 119, Sweep_modeling 52, Point_replication 32, Group_mesh 24 |

       | gen | 5,748 | The whole family: Parametric_generators 1433, Spiral 733, Scatter 679, Uv_sphere 521, Point_replicate 501, Box_generator 485, Iso_surface 443, Plane_generators 409, Point_generate 407, Point_jitter 137 |

       | curve | 3,724 | Polywire 1434, Sweep 1235, Resample_curves 620, Circle_from_edges 435 |

       | attrib | 3,715 | Attribute_generate 1493, Attribute_copy 542, Blend_shapes 521, Attribute_composite 494, Attribute_enumerate 262, Attribute_fade 260, Material_assign 63, Attribute_image 47, Color_by_height 33 |

       | partial | ~10,400 | Group_ops 3868/4313, Curve_ops 2111/2794, Instance_copy 1214/1572, Uv_ops 804/849, Deform 496/637, Transform_ops 416/893, Poly_path 371/487, Graph_color 333/364, Analysis 315/567, Subdivide 262/4607, Boolean_materialization 116/1781, Fuse_grid 108/419, *_reference twins ~200 |

       | Total | ~35,000 | of 90.3k rdk `.ml` lines (39%). Another ~17k test lines and 635 bench lines exercise only these modules. |

### D. Longest top-level functions

     |  | Lines | Function | Split along |

       | 1802 | flow/workspace.ml:347 check | names · types · macro expansion · packed eligibility · zones |

       | 1599 | ogpu_metal/backend.ml:83 create | queue · render / compute / blit encoders over a control record |

       | 1528 | rdk/mesh/plane_clip.ml:172 clip | classification · cut · cap · payload |

       | 1393 | flow_ir/core_lower.ml:82 lower_with | expressions · callables · loops · draw |

       | 1342 | rdk/curve/polywire.ml:92 run | Oracle-only, so delete it in P1 |

       | 1302 | flow_sop/lower.ml:166 lower_checked | per-kind lowerings dispatched from the Operators table |

       | 1256 | rays_editor/core.ml:11 update_frame | per-pane builders, each returning its slice of frame_result |

       | 975 | rdk/exact/planar_cdt.ml:484 build | insert · constrain · refine |

       | 938 | pxui_graph/scope_pane.ml:111 update | wire · marquee · frame · carry · fields gestures |

       | 873 | rdk/mesh/poly_bevel.ml:170 run | Live; split it after P6's shared helpers land |

       | 699 | flow_graph/flow_edit.ml:40 rewrite | one function per request family |

       | 677 | rays_editor/core_reduce.ml:9 reduce | per-intent reducers returning values (5 refs go away) |

       | 675 | flow_ir/packed.ml:862 run_all | per-instruction-class blocks |

       | 673 | rdk/mesh/revolve.ml:6 run | Oracle-only, so delete it in P1 |

       | 634 | rdk/mesh/sweep_circle.ml:3 run_legacy | Oracle-only, so delete it in P1 |

       | 608 | pxui/ui.ml:3193 text_area_submit | Moves to Ui_text and splits by key family |

### E. Docs: delete, archive, merge

Delete (git history keeps them)

- `specification/sop-lisp/` (20 files, 10,017 lines of session logs, plan 76/76 ticked)

- `specification/web-export/results.md` (1,752 lines)

- `docs/reference.md` (copies the README and AGENTS)

- `specification/workflow.md` and `project.md` (describe OCaml sketches)

- The test-times inventory table, and the finished items in DEFERRED.md

- README review boilerplate (lines 25, 45, 54)

- `tools/lisp/web_server.mli` (it has no implementation)

Rewrite or merge

- `api.md`: rewrite as an internal-layer reference of about 300 lines; drop the roadmap and the "Post-present work" appendix

- `rdk.md:546-3041`: trim the per-node specs down to the kernels that remain

- `web-export/plan.md`: reduce to a short design note

- `workspace/iteration.md`: move the normative §2/§7 into flow.md and delete the `rays-lisp ml` sections

- `flow.md` and `pxui_graph/AGENTS.md` contradict each other on wires: settle it

- Archive the workspace prototype (876 KB) and the pxui-kit @2x PNGs

## Finding register

All 248 source findings are retained below. Resolution and verification belong
in status.md; an unchecked item is unfinished or unverified.

- [x] **WN1** (P8, high, portability, effort M). Location at baseline: runtime.ml · runtime_shaders.ml · flow_gpu/emit.ml · flow_gpu/image_sink.ml · rays_pathtracer/pathtrace.metal. MSL source text sits in five places outside the Metal backend, while `Shader.descriptor` is already backend-tagged. Action: The path tracer becomes Rays Lisp (P8), and Flow kernels go through the shared `Kernel_layout` (FL6) into an MSL printer and a SPIR-V emitter. Runtime raster shaders follow in P9, so no handwritten shader remains. Evidence: the path tracer is Rays Lisp, Flow kernels go through Kernel_layout to the MSL and SPIR-V printers, and the image-sink MSL lives in flow_gpu's printer. The runtime raster shaders are P9.

- [x] **WN11** (P8, high, portability, effort M). Location at baseline: flow_gpu/emit.ml (247 lines, MSL text) · flow_gpu/pipelines.ml:22, image_sink.ml:31 (`backend="metal"`). Generated kernels are emitted only as MSL text for Metal's runtime compiler. Vulkan accepts only SPIR-V binary and has no runtime compiler. Action: Add a SPIR-V word emitter over the same kernel IR and `Kernel_layout`, with GLSL.std.450 for math and `OpRayQuery*` for `scene/trace`. Validate goldens with `spirv-val` in tests only; nothing ships a compiler. Evidence: lib/flow_gpu/spirv.ml writes binary SPIR-V for compute and raster stages over the shared IR and Kernel_layout, with OpRayQuery for scene/trace; every golden passes spirv-val.

- [x] **WN2** (P4, med, portability, effort S). Location at baseline: native_layer_token/dune (-framework CoreFoundation) · sdl3/sdl3_cocoa.c, sdl3/dune -framework. Platform C sits inside libraries meant to be cross-platform: the layer token and the SDL binding's Cocoa titlebar and autorelease code. Action: Keep the token as the seam; put its C and `sdl3_cocoa.c` behind per-OS stubs selected in dune (`enabled_if` on `%{system}`).

- [x] **WN3** (P4, med, portability, effort S). Location at baseline: metal/discover.ml:5-6, metal/build_bridge.ml:4 (/usr/bin/xcrun); metal, ogpu_metal dune. The Metal libraries fail to configure off macOS instead of being skipped. Action: Mark `metal`, `ogpu_metal` and their tests `enabled_if` macOS; executables name their OGPU implementation per platform.

- [x] **WN4** (P5, med, portability, effort S). Location at baseline: rays_editor dune → rays_pathtracer; Caps Ray_tracing / Function_tables. The editor links the path tracer unconditionally. A backend without hardware RT or function tables must still run the editor. Action: The editor reaches the path tracer only after `Caps.require`; without the caps the render pane shows a typed 'not available on this GPU' state. Check with ogpu_mock caps set to false. Evidence: Rays_editor.Renderer.update asks the path tracer's P.available before creating a tracer. Without RT caps the viewport stays raster with the typed notice 'Path tracing is not available on this GPU.'. test_pathtracer asserts minimum_m1 caps are refused and the same caps with ray_tracing are accepted. An editor run over ogpu_mock with caps off is still unrun.

- [x] **WN9** (P4, high, portability, effort M). Location at baseline: rays_pathtracer.ml:803,1103,1136 · Shader binding kind Intersection_table · Caps.Function_tables. Spheres in the path tracer require Metal intersection function tables. Vulkan ray query has no such object: procedural AABB hits are resolved in the shader loop. Action: Make procedural geometry a backend matter. OGPU keeps `Bounding_boxes`, the path tracer stops requiring `Function_tables` for spheres, and the Metal backend keeps its tables internally. Evidence: the OGPU intersection-function-table API is deleted (Caps.Function_tables, create_intersection_table, table_*, set_table, ~linked, table_offset) with its Metal bindings; spheres are an in-loop procedural test in every kernel, and conformance finds sphere hits in-kernel.

- [x] **WN10** (P4, med, portability, effort S). Location at baseline: ogpu_core/acceleration.mli geometry Motion_triangles, Curves; Caps (Ray_tracing_curves only). Motion geometry and curves are native on Metal, but only available on Vulkan through vendor extensions or as AABBs. Motion has no capability flag. Action: Add a `Ray_tracing_motion` cap; the path tracer degrades (shutter midpoint for motion, AABB strands for curves) instead of failing. Evidence: Caps.Ray_tracing_motion exists; without it scenes build at the shutter midpoint and a native test matches the CPU at time 0.5. Without Ray_tracing_curves, strands are an in-loop capsule test (CPU, MSL, SPIR-V); native round linear curves are used when the cap is set.

- [x] **WN5** (P0, low, portability, effort S). Location at baseline: rays/sketch.ml:29 default "/tmp/rays-crash". A hard-coded Unix temp path. Action: Use `Filename.get_temp_dir_name ()`.

- [x] **WN6** (P7, low, portability, effort S). Location at baseline: runtime_native_qualification.ml:21 (/usr/bin/footprint) · DEFERRED autorelease notes. Native qualification uses macOS-only tools. Action: Keep them under a macOS `enabled_if`; the port adds its own memory probe. Evidence: native qualification and the SDL memory check are behind a macOS enabled_if.

- [x] **WN7** (P7, med, test-sprawl, effort S). Location at baseline: test/ogpu_conformance · lib/ogpu tests. The conformance suite lives in test/ and runs only the Metal and mock implementations. Action: Move it into lib/ogpu, parametric over a driver, so every backend (mock, Metal, Windows) runs the same suite. Evidence: lib/ogpu/conformance runs one suite over the mock and Metal drivers; a new backend is another driver row.

- [x] **WN8** (P7, low, doc-rot, effort S). Location at baseline: AGENTS.md Purpose ('ships native Apple-Silicon Metal only') · gate rule 'Metal. stays within the backend'. The direction text and the gate rule are worded Metal-only. Action: Generalise the gate rule to 'a backend's modules stay within that backend' now, and rewrite the direction text when the port starts. Evidence: the gate's backend rule is a table row (modules, owning directories, allowed files), so a second backend is one more row.

- [x] **XC1** (P2, high, duplication, effort M). Location at baseline: flow/diagnostic.mli:7 · sop/diagnostic.mli:16 · flow_sop/diagnostic_bridge.ml. Two diagnostic types with identical trace/code/message/cause/hints fields, converted by a bridge. The types appear in 537 references. Original wording proposed `Sop.Diagnostic.error = Diagnostic.t`; this cleanup removes that otherwise-redundant namespace too. Action: Add a `diagnostic` leaf library; make Flow and SOP use its single `Diagnostic.t` type; delete duplicate namespaces and bridge. Evidence: one `Diagnostic.Error` / `Diagnostic.t` source remains; `@check`, diagnostics/Flow tests and strict SOP refusal pass.

- [x] **XC2** (P7, high, api-hygiene, effort L). Location at baseline: 85 `module Private` blocks, ~495 vals. Code outside tests references `Private` 2,100 times, so `Private` has become the real API (rdk 1,432 internal; `Eval.Private.force_reference` 224). Action: Within a library use dune `private_modules`. Across libraries give each use a public name, or put it in a gate-listed `Unstable` module. Evidence: fff4816a gives the remaining Private APIs public names (Sop.Node.Build, Mesh.View, Font.Raster, Scene.Layer, Scene3.Walk, Renderer.Native, Scope.Inspect); test/private_allowlist.ml has 0 rows.

- [x] **XC3** (P0, high, duplication, effort S). Location at baseline: sop/support.ml:716-722. `encode_numeric` writes floats with `string_of_float` (12 digits) and decodes them back, so it loses precision. groups.ml:971 in the same library uses `%.17g`. Action: Use one shortest round-trip printer (XC4). This is a correctness bug, so fix it in P0.

- [x] **XC4** (P2, high, duplication, effort S). Location at baseline: flow/lisp.ml:24 · param.ml:347 · editor_core.ml:279 · flow_web/js.ml:45 · tools ×3 · 8 flow_web tests. Four or more float printers with different precision and `-0` handling. Action: Add `Param.float_text` (param is a leaf everyone reaches) and delete the rest.

- [x] **XC5** (P2, high, duplication, effort M). Location at baseline: 13 sites: flow_sop/lower.ml:152, scene3_native_lowering.ml:75,250,301, rays_execution.ml:1101, core_exec.ml:771, renderer.ml:116, core_scope.ml:21, workspace_gpu.ml:43,60, lisp_text.ml:81 …. Hand-rolled bounded most-recently-used caches built with `List.filteri (fun i _ -> i < N)`, sometimes keyed on `Obj.repr`. Action: Add a ~20-line identity-keyed bounded list to `lib/lru`, let flow_ir reach lru (gate edit), and delete the copies.

- [x] **XC6** (P4, high, global-state, effort L). Location at baseline: lib/rays: time.ml:3-7, input_state.ml:7-10, sketch.ml:4-14, canvas_runtime.ml:2-3, scene3_native_lowering.ml:56-301. 25 module-level refs in `rays` (70 across lib/), against AGENTS' 'no global user-state refs'. Action: Fold them into the `Sketch.run_state` loop record and the per-window renderer state. Evidence: run contexts own input/time/callbacks and lowering caches; device owners isolate borrowed vertices, marks and leases; renderer instances own scratch. The AST gate rejects module-initializer refs in rays, with local-ref negative checks. Renderer/native, resource and Canvas lifecycle checks pass; process-unique atomic IDs and immutable scoped DLS keys are documented separately.

- [x] **XC7** (P4, high, duplication, effort M). Location at baseline: sdl3, sdl3_ttf, sdl3_image, sdl3_mixer, metal, ogpu_core Error, rays_execution, runtime_resources. Eight identical `{operation; kind; message}` error records. Action: Keep the binding-level ones. The runtime and renderer layers reuse `Ogpu.Error` instead of re-wrapping. Evidence: Runtime_resources aliases Ogpu.Error.t, preserving resource kinds in the shared enum; image/canvas GPU readback returns original backend errors. Injected-error tests compare the full record. Resource/OGPU/typecheck/gate checks pass in 9.58 s, following the broader integration pass; binding errors remain separate.

- [x] **XC8** (P2, med, duplication, effort S). Location at baseline: Param via `Sop.Parameter` (sop/parameter.ml, 1 line) and `Editor_core.Param` (editor_core.mli:7). One schema type under three names: Param 470 refs, Editor_core.Param 43–51, Sop.Parameter 15–76. Action: Delete both aliases and have the PPX emit `Param.*` (SP12).

- [x] **XC9** (P2, med, duplication, effort M). Location at baseline: lib/rays/{vec2,vec3,color,mat4,quat,rand,noise,parallel}.ml. 16 one-line `include Rays_math.X` shims: 357 `Rays.*` refs against 447 `Rays_math.*` for the same types. Action: Delete the shims and run a codemod to `Rays_math.*`, or keep one façade in `rays.ml`.

- [x] **XC10** (P2, med, duplication, effort S). Location at baseline: flow/packed_ops.ml:49 · rays_math/rand.ml:27 · rdk/gen/scatter.ml:370. The same SplitMix random-at-index, three times. Action: Let flow depend on rays_math (gate edit) and call `Rand.float_at`. Evidence: Flow and Scatter use the shared `Rand` implementation; `@lib/flow/runtest` and `@lib/rdk/test_gen` pass, including signed seed/index parity.

- [x] **XC11** (P2, med, duplication, effort M). Location at baseline: rays/scene.ml (8×), rays_execution.ml:757, 7 rdk files, pxui/ui.ml, flow/value.ml. Ad-hoc hash mixing with the constants 65599 and 1000003. FNV is written twice (sop/support.ml:145, flow_sop/operators.ml:346). Action: Add `Rays_math.Hash.{mix,string}` and use it at every site. Evidence: distinct 65599, salted multiplier, 63-bit/64-bit FNV and Flow float-sequence helpers preserve each algorithm; matching old/new microbench outputs are exact.

- [x] **XC12** (P7, med, config-sprawl, effort M). Location at baseline: 92 `RAYS_*` env vars; RAYS_MAX_FRAMES read at sketch.ml:105 and cook.ml:120; RAYS_UI_FONT at sdl3_ttf.ml:217 and ui.ml:432. No registry. The SDL binding reads an app-level variable. Action: `bin/rays` and the test runners parse the environment once into a config record that is passed down. Add a gate token-scan for `getenv` in lib/. Evidence: 5baa9de0: bin/rays.ml parses RAYS_MAX_FRAMES, RAYS_CRASH_DIR, RAYS_PROFILE, RAYS_EDITOR_PREFERENCES and RAYS_UI_FONT once into Workspace.process. Sketch, Ui, Sdl3_ttf, the path tracer and the editor take explicit arguments, and generate.exe takes flags. dependency_gate's env_errors rejects RAYS_ getenv in non-test lib sources, with a negative self-check.

- [x] **XC13** (P0, med, duplication, effort S). Location at baseline: ~35 `List.filteri` prefix idioms and 6 `take` definitions (flow_ir.ml:868, queue.ml:25, carry.ml:304 …). OCaml 5.3 has `List.take`/`List.drop`, used only 3 times. Action: Replace them with the standard library.

- [x] **XC14** (P2, med, duplication, effort S). Location at baseline: pxui_shell/inspector.ml:225,251 · text_pane.ml:866 · flow_sop/port.ml:83 · workspace.ml:293 · flow_ir/operators.ml:57 · flow/op.ml:138. Colour helpers outside `Color`: byte clamp, hex parse and format, HSL, HSV. Action: Add `Color.to_hex_string`, `hsl_channels` and `hsv` to rays_math and delete the copies. Evidence: Inspector uses `Color.of_unit_floats` with its original round-then-clamp behavior; shared Color tests, Flow and PXUI tests pass.

- [x] **XC15** (P2, med, duplication, effort S). Location at baseline: editor_core/store.ml:1 · rays/canvas_runtime.ml:6 · sketch.ml:95 · environment.ml:345,350. `ensure_directory` is byte-identical twice. The crash and preset writers bypass the atomic `Store.write_text`. Action: Move both into one home and route every writer through it.

- [x] **XC16** (P5, med, duplication, effort M). Location at baseline: rays_editor/lisp_text.ml:22 · flow/syntax.ml:52. Two Lisp lexers: one tolerant (for colouring) and one strict. Action: Add an error-tolerant token stream with spans to `Flow.Syntax` and colour from it.

- [x] **XC17** (P7, med, duplication, effort S). Location at baseline: test/dependency_gate.ml:10-30 · tools/api_manifest/api_manifest.ml:38-110. Standalone s-expression parsers, although `Flow.Lisp` reads s-expressions. Action: Use the Flow reader. Keep sdl3_lock's parser (it runs at configure time). Evidence: test/dependency_gate.ml has one dedicated 30-line dune s-expression reader; the api_manifest parser is gone.

- [x] **XC18** (P0, med, api-hygiene, effort S). Location at baseline: flow/context.ml:8,24 · flow/ty.ml:14,27. Global mutable `register` functions whose only caller is test_open_domain. Action: Delete them and make the lists immutable.

- [x] **XC19** (P3, med, global-state, effort S). Location at baseline: flow/eval.ml:333 (set :1243) · flow_ir.ml:71 (set :1128). Forward-reference refs set later in the same file. Action: Use `let rec` or reorder, and delete the refs. Evidence: no forward-reference refs remain in eval.ml or flow_ir.ml.

- [x] **XC20** (P0, med, global-state, effort S). Location at baseline: scene_command/display_list.ml:10. `next_id` is a plain `ref` (not domain-safe), while render_ir.ml:10 in the same library uses `Atomic`. Action: Make it `Atomic`. This is a correctness fix.

- [x] **XC21** (P0, med, error-handling, effort S). Location at baseline: rays_editor/rays_editor.ml:234-235. A library calls `prerr_endline` and then `exit 1`. Action: Return a `result`; `bin/rays` owns the exit.

- [x] **XC22** (P6, med, error-handling, effort M). Location at baseline: edit_graph.ml:485,507, node.ml:114, carry.ml:83, workspace_images.ml:482. `Invalid_argument`/`Failure` caught and turned back into results. Action: Validate first and return `Error`. Keep `invalid_arg` only for programmer errors. Evidence: 7caaee94: factory build/rebuild, [@sop.validate] callbacks and support decoders return results; [@@sop.node_fallible] covers checks that depend on which inputs are wired. The Invalid_argument/Failure catches in edit_graph, node and workspace_images are gone; messages are byte-identical.

- [x] **XC23** (P4, low, duplication, effort S). Location at baseline: scene_command/render_ir.ml:58-66. A hand-written byte `Encoder`. Action: Use `Buffer` with `add_int32_le` and `add_int64_le`.

- [x] **XC24** (P6, low, duplication, effort M). Location at baseline: 29 hand-written array-doubling sites (14 in rdk, ui.ml:89, eval.ml:935, packed.ml:508). `Dynarray` is already used 23 times. Action: Use `Dynarray` on the paths that are not hot. Evidence: Dissolve uses Dynarray for its boxed primitive-kind buffer; the remaining doubling sites are packed int/float tables or hash tables, where Dynarray is not a drop-in.

- [x] **XC25** (P5, low, duplication, effort S). Location at baseline: 4 rect records, ~104 anonymous 4-tuples in .mli files; rect hit test at ui.ml:662 and scope_pane.ml:14. Rect types and hit tests are spread around. Action: Export `Ui.contains` and delete the pxui_graph copy. Typing the tuples is optional. Evidence: Ui.contains replaces the hand-rolled rect tests in scope_frame, core and viewport.

- [x] **XC26** (P7, low, duplication, effort S). Location at baseline: 27 identical `ok` unwrappers, 22 `contains`, 150 `let fail`, 103 `let check`. Test helpers are copied file to file. Action: One small test-util library per layer (flow, rdk already has one), opened by the tests. Evidence: lib/test_support (ok, check, fail) replaces the copied helpers in 38 flow_ir, flow_sop, flow_web, flow_graph, flow_gpu, flow_native and rays tests.

- [x] **XC27** (P5, low, global-state, effort M). Location at baseline: Unix.gettimeofday in sop (async_cook, session), 4 rays_editor files, pxui, flow_sop, editor_document. The wall clock is read directly; `flow/phase_timer.ml` already injects its clock. Action: Take time from `Frame.t` or an injected clock. Evidence: Session.clock (set_clock, now), Cook.create ?clock, Workspace_gpu.create ?clock and Lower ?clock; Unix.gettimeofday remains only as the default at the edges.

- [x] **XC28** (P4, low, naming, effort M). Location at baseline: Scene3 (215 refs), Shadow3, scene3_native_lowering, Shape2. Dimension-suffixed module names sit next to unsuffixed ones. Action: Fold them under `Scene`, or rename to `Scene_3d`, once Shadow3 is deleted. Evidence: Scene3 is Scene.D3 (a private module aliased under Scene, about 60 callers rewritten). Shape2 is Scene_command geometry below rays, used by flow_ir and flow_web, so it stays there as data.

- [x] **XC29** (P0, low, vibe-code, effort S). Location at baseline: 119 files with 20+ golfed `let x=` lines (core_lower.ml 356, scene_execution.ml 220, test_attribute_kernel.ml 429); a 1,440-char line in runtime.ml. Minified OCaml and MSL. There is no `.ocamlformat`. Action: Add `.ocamlformat` and format in one mechanical commit per library. Evidence: fe94feb4 formats the tree with ocamlformat (conventional, margin 100) and dune fmt; the 1,440-char runtime.ml line and the golfed let x= files are gone. Owner decision 2026-10-28: no hard line-number limits, so the function-length guard is removed (0c6b2166).

- [x] **PL1** (P0, high, dead-code, effort M). Location at baseline: lib/rays/shadow3.ml(i) → scene.ml:378-393,500 · scene3_native_lowering.ml:153-168 · scene_execution.mli:44-58,98 · runtime.ml:169. `Shadow3.t` has no constructor, so the whole explicit-shadow chain is unreachable: caches, lowering branch, two pipeline families and MSL. The World sun-map path does not use it. Action: Delete the chain.

- [x] **PL2** (P0, high, dead-code, effort M). Location at baseline: Scene3_stencil / Scene3_textured_stencil / Scene3_shadow_stencil · create_stencil_texture (ogpu_core/backend.ml:148). Nothing in rays, rays_execution or rays_editor produces stencil. `stencil_state=Some` exists only in a runtime test. Action: Delete it. Pipeline families drop from 12 to 8.

- [x] **PL3** (P1, high, dead-code, effort M). Location at baseline: lib/metal/metal_bridge.mm (6,790 lines, handwritten). 53 OCaml primitives, but the bridge carries an XPC service (83-780), Metal 4 submission and argument tables, mesh/tile builders (4399, 4732) and ~50 Handle_kind cases nobody reaches. They are non-static, so `-Wunused-function` never fires. Action: Make the helpers `static`, let the compiler list them, then run `codemod dead-stubs`. Expect about 1.5k+ lines gone. Evidence: the Metal bridge lost its 1,023 mesh/tile pipeline lines and metal_raw_types its 243 dead descriptor types; metal.ml/.mli lost the Tile, Object and Mesh stages. dead-stubs, dead-types and metal-registry --audit are clean; the Metal 4 compiler stays because ogpu_metal uses it.

- [x] **PL4** (P0, high, vibe-code, effort S). Location at baseline: metal_bridge.mm:5940-6080 · metal_raw.ml:21. 66 `/* M3: former …generated.inc */` markers, empty shards, `mechanical_id_count(){return 48;}`, and `[@@@ocaml.text]` blocks citing plan ids. Action: Delete them.

- [x] **PL5** (P4, high, god-module, effort L). Location at baseline: ogpu_metal/backend.ml:83-1682. The whole Metal driver is one closure. `create_queue` (681-1551) alone holds every encoder. Action: Split into queue, render encoder, compute encoder and blit encoder over an explicit control record. The sibling modules already exist.

- [x] **PL6** (Win, low, layering, effort —). Location at baseline: lib/ogpu (virtual, impl.mli is one value) + Ogpu_core.Backend.driver record. There are two mechanisms for choosing a backend, and today only Metal uses either. They are also the portability seam the Windows port needs. Action: Locked: keep OGPU as the seam for Windows; ogpu_vulkan becomes its second implementation. Choose between link-time and run-time selection once the Vulkan backend exists. Evidence: the plan defers PL6 to the Windows port (P4 findings: 'PL6 deferred to the port'): keep the virtual library, driver record and mock, and choose link-time vs run-time selection when ogpu_vulkan exists. The seam is unchanged (P4.G3).

- [x] **PL7** (P4, high, duplication, effort L). Location at baseline: scene_command: Render_ir, Shape2, Shape_batch, Ui_batch, Marks, Display_list + rays/Ink. Seven 2D formats. Shape_batch lowers through the UI slot path (rays_execution.ml:640-660), so it is Ui_batch's format. It also carries GPU Marks tokens. Action: Merge Shape_batch into Ui_batch, give Marks its own token, delete Ink (PL15), and make Shape2 float-based (PL14).

- [x] **PL8** (P4, high, god-module, effort L). Location at baseline: lib/runtime/runtime.ml (1,058). The 'SDL lifecycle' library also owns Scene3 MSL strings (140-320), the pipeline factory (320-530), Retina scaling caches and stats. Action: Move `create_renderer` into the renderer, and move the MSL into per-backend shader files (`renderer/shaders/metal/*.metal`) selected by the backend-tagged `Shader.descriptor`, so a Windows shader set is a sibling directory. Runtime keeps window, surface and events.

- [x] **PL9** (P4, high, layering, effort M). Location at baseline: rays_execution.ml:167,355-377. `Window | Offscreen` re-distinguishes what `Runtime.t` already unifies, and it re-wraps 9 window operations only to convert the error type. Action: Delete the wrappers. Callers hold the `Runtime.t`, with one error type.

- [x] **PL10** (P4, high, god-module, effort L). Location at baseline: scene_execution.ml 1,589 · rays_execution.ml 1,243. Four render entry points threaded through Runtime with the same signature. `render` with a plain draw list is used only by tests. Action: Keep one draw type (`sampled_draw`). Merge both into a `renderer` library split into lowering, retained plans and leases. Evidence: the merged Renderer delegates command traversal and reusable lowering buffers, retained plan/resource-stamp caches, and image lease checkpoint/rollback to three private modules. The default renderer suite passes (25.0 s including the queue); focused native offscreen, UI, coordinate and runtime checks pass (8.29 s including the queue), preserving 1x/2x pixels, resize, readback and frames 1/2/60/600.

- [x] **PL11** (P4, med, duplication, effort M). Location at baseline: metal_format.ml (130 cases, 387 lines) · ogpu_metal/texture.ml:1 · validation.mli:1 · types.mli:15. Four texture-format enums, of which OGPU uses 7 formats. `Validation.storage` has 4 cases where `Types.memory` has 2. Action: Make `Ogpu_core.Types` the single backend-neutral format enum and delete the Validation duplicates. Shrink Metal_format inside the Metal backend only, so a Windows backend maps from the same neutral enum. Evidence: seven shared formats now cover the OGPU host color formats plus internal R8/BGRA/BGRA-sRGB/depth resources; Metal’s type table and native transfer-layout switch contain only those seven. `Validation` owns descriptor/sampler checks and exposes no duplicate format/storage/usage enums; OGPU Metal re-exports Types directly. Host texture descriptors still refuse non-RGBA formats, while the adapter’s separate native format override creates depth attachments. OGPU and OGPU Metal tests, mock and native conformance, and the retained-format/transfer test pass. The full `@check`/cross-library integration remains open at this checkpoint.

- [x] **PL12** (P2, med, duplication, effort L). Location at baseline: Sdl3.Event.t · Runtime_input.event · Rays.Event.t · Frame_input.event (string keys). Four event types and three key types. Scroll phase is defined three ways. Action: One key/button/event vocabulary in `input` (ex frame_input), used by runtime and rays.

- [x] **PL13** (P4, med, duplication, effort S). Location at baseline: sdl3.mli:61 · runtime.ml:18,58 · rays_execution presentation_facts. Four 'presentation facts' records. Action: Keep one, in runtime.

- [x] **PL14** (P4, med, duplication, effort M). Location at baseline: rays/path.mli (fixed steps:20 → int points) · scene_command/path.ml. Two Path modules. Precision is lost twice: flattened to int, then rebuilt. Action: Make `Rays.Path` an alias of `Scene_command.Path` and make Shape2 take floats.

- [x] **PL15** (P4, med, dead-code, effort S). Location at baseline: rays/ink.ml(i) + test_ink.ml. One caller (marks_scene.ml:152-160), drawing 1×1 points. Action: Emit Ui_batch rects and delete Ink.

- [x] **PL16** (P0, med, dead-code, effort S). Location at baseline: rays/canvas_runtime.ml:2 · sketch.ml:127-131. `capture_callback` is set and never read. Action: Delete it.

- [x] **PL17** (P4, med, perf-risk, effort S). Location at baseline: rays/sketch.ml:132-138. `save_screen_png` copies the readback pixel by pixel through checked `set_pixel`: about 5M calls per Retina export frame. Action: Add a bulk `Canvas.of_rgba_bytes`. Evidence: screenshot export adopts the captured packed bytes through Runtime_resources.Canvas.of_rgba_bytes, validates the extent/byte count once and saves without per-pixel set_pixel calls. Focused resource, Rays and native Canvas tests pass, including malformed storage and lifetime checks.

- [x] **PL18** (P4, med, correctness-risk, effort M). Location at baseline: rays/canvas.ml (9 failwith), image.ml (4), sketch.ml (4). Public resource APIs raise exceptions while the layers below return results. Action: Return `result`, or drop the duplicates that are not `_exn`. Evidence: Canvas/Image fallible operations, including destruction, return results. Sketch run_state/export/export_state return typed Ogpu.Error.t; workspace startup/export translates those to diagnostics. Callback exceptions keep their crash-report/backtrace behavior, with renderer cleanup if init fails. Wrong-domain lifetime and invalid-startup regressions, focused Rays/editor tests and @check pass; callers/docs use the new boundaries.

- [x] **PL19** (P0, med, doc-rot, effort S). Location at baseline: runtime.ml:1-16 · scene_execution.mli:32-35,85-88,166-172 · runtime_input.mli:17. Codemod residue: records full of blank lines, docs for deleted fields (`plan_entries`, `position`), and a one-field record. Action: Clean them up. Evidence: fab51f23 removes blank-line record residue in runtime/runtime_input and the doc text for nonexistent fields.

- [x] **PL20** (P0, med, dead-code, effort S). Location at baseline: runtime.ml:40-56. The `frames`, `presented` and `logical_submissions` counters are written and never read. Action: Delete them.

- [x] **PL21** (P0, med, test-sprawl, effort S). Location at baseline: lib/metal: 19 tests named by plan step (`…support121_safe`, `…pipeline113_`) with 18 alias rules; ogpu has 7 rules for 206 lines. Names are plan ids, and the 6-line `test_metal_command_support121_safe` duplicates device_safe. Action: Rename the tests by subject, use one rule per executable, and delete the duplicate.

- [x] **PL22** (P4, med, layering, effort M). Location at baseline: rays_pathtracer → rdk (rays_pathtracer.ml:239,333-348). The GPU renderer reaches into the geometry kernel to triangulate and compute normals. Action: Move the mesh conversion into rdk_rays; the pathtracer takes packed triangles.

- [x] **PL23** (P4, med, god-module, effort M). Location at baseline: rays_pathtracer.ml 1,852 (upload_mesh_async 233 lines at :783). Upload, build state machine, uniforms, lights and render loop are all in one file. Action: Split into mesh, build and frame modules.

- [x] **PL24** (P2, med, duplication, effort S). Location at baseline: rays_pathtracer.mli:12 Linear_color · world.mli:17 World.rgb · world_map.ml:15 ACES (also runtime.ml:222). Linear colour is defined twice, and ACES is copied. Action: `Rays_math.Color.Linear` and one `Color.aces`. Evidence: World.rgb is the manifest shared type, pathtracer Linear_color aliases the same module, and world-map conversion uses Color.aces. Math, world-raster and exact M1 image checks pass; the final warm math/world check is 0.63 s. Target-language equations remain in the frozen shaders until P8/P9 source migration.

- [x] **PL25** (P4, med, dead-code, effort S). Location at baseline: rays_execution.mli (adopt_draws, *_for_test, retained_scene2_segment_stats) · runtime.mli Private (scale_draws, …). Test-only exports in public interfaces. Action: Delete them, or keep a single For_test module that the gate lists. Evidence: test-only exports are confined to Renderer.Private and Scene_execution.Private; rays_execution.mli is gone.

- [x] **PL26** (P1, low, api-bloat, effort S). Location at baseline: sdl3_ttf: family_name, set_kerning, set_outline, set_style, size_text(_wrapped), style_name · sdl3_mixer: reload_bytes, set_gain. No callers outside the library. Action: Prune them together with their C stubs. Evidence: the listed sdl3_ttf and sdl3_mixer exports are gone; prune on sdl3_ttf reports a fixpoint.

- [x] **PL27** (P4, low, duplication, effort S). Location at baseline: ogpu_core Command_buffer.completed_epoch ≡ Backend.completed_epoch. Command_buffer has 2 functions and 1 caller. Action: Fold it into Backend. Evidence: deleted the module and public alias, moved typed receipt status to `Backend`, and migrated pathtracer and conformance callers. OGPU validation plus mock/Metal conformance pass; the pathtracer library builds.

- [x] **PL28** (P2, low, naming, effort S). Location at baseline: rays_math contains Parallel (the only reason it depends on domainslib). The math leaf drags in a domain pool. Action: Split out a `parallel` library. Evidence: `rays_math` no longer depends on domainslib; direct edges and forbidden pairs pass `@test/dependency_gate`.

- [x] **PL29** (P2, low, duplication, effort M). Location at baseline: Rays.Frame.t vs Frame_input.t (core_model.ml:248 keeps both); Frame.size is redundant. Two frame records. Action: `Frame` embeds `Input`, and `size` is dropped.

- [x] **PL30** (P7, low, layering, effort S). Location at baseline: rays_pathtracer.ml:1251,1274,1455 · sketch.ml:196,246. Debug environment knobs and `eprintf` in libraries. Action: Pass them in as config (XC12). Evidence: the path tracer's bsdf_only/profile and Sketch's crash_dir/profile are arguments now; their eprintf runs only when the caller asks for profiling or a crash report.

- [x] **PL31** (P4, low, vibe-code, effort S). Location at baseline: ogpu_metal/impl.ml. A diagnostics-only `live_handles` closure that can `failwith`, returned as a tuple element, so every caller writes `driver, _`. Action: Expose it as `Ogpu_metal.live_handles`. Evidence: 1040322d: Ogpu.Impl.create_driver returns only the driver; live_handles is its own function.

- [x] **PL32** (P4, low, perf-risk, effort S). Location at baseline: rays_execution.ml:51,59,175 · scene_execution.ml:8-9. Polymorphic `(=)`/`Hashtbl.hash` keys, and a magic string prefix `"gpu:vertices:"` used for routing. Action: Use typed variant keys. Evidence: 116678a0: Scene_execution.mesh_key = Host | Gpu_vertices replaces the 'gpu:vertices:' prefix check; cache and fingerprint strings are unchanged.

- [x] **PL33** (P0, low, doc-rot, effort S). Location at baseline: lib/runtime/AGENTS.md:23,31,86,90 · lib/ogpu/AGENTS.md ('since G4'). Says rays and runtime submit through ogpu_metal_native, which the gate now forbids. A rule is stated twice. HTTP and DOM policy text remains. Action: Update both files.

- [x] **PL34** (P4, low, naming, effort M). Location at baseline: scene_execution.mli:80 vertex_attributes option. Two Scene3 vertex ABIs ('legacy packed ABI'). Action: Pick one. Evidence: one Scene3 vertex ABI: the legacy 68-byte f64 path (None at surface[82]) is removed from scene.rays and the generated shaders, and validation is per family (Scene3, points, textured and world require 24-byte vertices plus 12-byte attributes; Scene2, UI and Marks require None).

- [x] **PL35** (P0, low, dead-code, effort S). Location at baseline: runtime.ml:627 create_offscreen ?title:_. An ignored parameter. Action: Delete it.

- [x] **RK1** (P1, high, oracle-only, effort L). Location at baseline: lib/rdk/{attrib,gen,curve,mesh}: 52 whole modules (24.5k .ml + 1.7k .mli) + ~10.4k inside 12 live modules. About 39% of rdk is reached only by tests, benches and port gates (inventory in appendix C). Action: Delete it (decided 2026-10-27: no oracle library). After the SOP twins go, run prune-dead-code to a fixpoint. Only kernels reached by the 37 native SOPs, flow_sop host primitives, the editor and the path tracer survive. Evidence: a4c1eb7e runs prune-dead-code to a fixpoint over lib/rdk (--modules --cut-tests) and deletes the 24 remaining husk modules, about 56k lines including tests. lib is 264k lines counting .ml/.mli/.c/.mm (from 332.5k).

- [x] **RK2** (P0, high, correctness-risk, effort S). Location at baseline: lib/rdk/test_main.ml:3-4. `test_rdk_sources` and `test_copy_to_points` are in group 'rdk_parts', which no dune rule selects. 751 lines never run, including a GC-stress regression. Action: Move them into 'core' and 'mesh'. The guardrail (§07) stops this from happening again. Evidence: rdk_parts is gone; test_rdk_sources runs under core and test_copy_to_points under mesh in lib/rdk/test_main.ml.

- [x] **RK3** (P6, high, vibe-code, effort L). Location at baseline: core/error.ml:13-21 + 54 local exceptions, 114 string-result + 209 Error.t signatures, ~30 `unguard`. Each error passes through three forms (local exception, string result, Error.t), with 36 `_raw`/guarded twin functions. Action: Use one internal `Rdk_error of Error.t` and convert to `result` once at the rdk boundary. Delete unguard, of_string, the twins and the local exceptions. Evidence: 91cf8472 and c559ac0c: one Error.Rdk of Error.t (operation, code, message) and Error.Fail, converted once by Error.guard/Error.run (with ?invalid_argument and ?cancelled code mapping). unguard 26→0, real _raw twins about 18→0, local exceptions 55→10 (the remaining 10 are control flow and infrastructure); public messages and codes unchanged.

- [x] **RK4** (P6, high, duplication, effort M). Location at baseline: attrib/group_ops.ml:4207-4313 (+ Attribute_ops, Curve_topology, Group_mesh, Point_replication, Sweep_modeling). Each operation is re-declared at the end of the file with 10–15 optional arguments, only to wrap it in `Error.guard`. Action: This goes away with RK3. Evidence: group_ops.ml is 534 lines, with no re-declared per-operation wrapper block.

- [x] **RK5** (P1, high, duplication, effort S). Location at baseline: group_ops.ml:1344/1363/1397 vs 1497/1527/1540/1591. `flood_bits`, `stepped_bits`, `grow_point_depth_bits` and `enqueue` exist twice (plain and `constrained_*`), about 450 lines. Action: Keep only the constrained versions; most of this goes with RK1. Evidence: no constrained/unconstrained twin pairs remain in group_ops.

- [x] **RK6** (P6, high, duplication, effort M). Location at baseline: support.ml:158, kernel.ml:8, curve_ops.ml:17, poly_path.ml:7, uv_ops.ml:52, uv_parameterize.ml:32, sweep.ml:44, boolean_(corner_)payload. At least 8 copies of the parallel-for helper, plus 132 inline `~chunk_size:1` range loops. Action: Use one `Support.parallel_for`/`parallel_ranges`. Evidence: 36 of 55 inline range loops use Support.run_ranges or run_indexed_ranges; the rest size or index per-range work arrays, run per block, or interleave range levels.

- [x] **RK7** (P6, high, duplication, effort M). Location at baseline: 886 inline `if i land MASK = 0 then Cancel.check_opt` sites with 11 different masks. The cancellation cadence is copied by hand everywhere. Action: Add `Cancel.poll cancel i`, or put the check inside the parallel helper. The change is mechanical. Evidence: c74afdab: one inline Cancel.poll cancel i mask replaces 481 hand-written masked checks in 82 files, keeping every site's cadence. Microbench (4e8 iterations): non-opaque 1.50–1.57 vs 1.48–1.56 s; the dev profile's -opaque costs at most about 5% on a few-instruction loop.

- [x] **RK8** (P6, high, duplication, effort M). Location at baseline: 209 `match storage with`; Float4 arms ×85 in 40 files; planes_of_storage at attribute_composite.ml:77 and attribute_generate.ml:504. The same code is copied for each storage type. Action: Add `Attribute.map_float_planes`/`planes` to core. Evidence: Attribute.float_planes and of_float_planes collapse the Float..Float4 arms in Attribute_transfer and both Boolean_payload interpolators.

- [x] **RK9** (P6, med, duplication, effort M). Location at baseline: surface_index.ml:393 · bounds3_index.ml:35 · bounds2_index.ml:57. Three median-split BVH builders. Action: One generic builder (dimension plus extra planes). Evidence: Surface_index builds on Bounds3_index through ?extend; Bounds2_index keeps its own split scheme to preserve tree shape and exactness.

- [x] **RK10** (P6, med, duplication, effort S). Location at baseline: point_clusters.ml:91 · point_snap.ml:100. Two open-addressing uniform grids. Action: Extract one `Point_grid`. Evidence: one Point_grid module serves Point_clusters and Point_snap.

- [x] **RK11** (P6, med, layering, effort S). Location at baseline: surface_index.ml:1219-1232 → mesh/ray.ml:221 via Private. Ray-SOP policy (sample average/median, directions) lives inside the spatial index. Action: Move it into `Ray`; the index keeps only hit queries. Evidence: 1297fa3a moves the ray sampling policy (combine, jitter, normal averaging, average driver; about 340 lines) from Surface_index into Ray; the index keeps hit queries.

- [x] **RK12** (P1, med, dead-code, effort S). Location at baseline: mesh/sweep_circle.ml:3 run_legacy (~650) · sweep_modeling.ml · point_replication.ml · group_mesh.ml · exact/voronoi2.ml (3-line comment). Legacy paths and empty or adapter modules, all oracle-only. Action: Delete them, together with the Voronoi2 alias at rdk.ml:10. Evidence: Voronoi2 and the sweep_circle run_legacy path are gone.

- [x] **RK13** (P1, med, oracle-only, effort S). Location at baseline: boolean_materialization.ml:216,1199. `tiny_seam_edges`/`collapse_tiny_seam_batch` (~115 lines) are called only from test_boolean_solid. Action: Delete them and repoint the tests at the production `collapse_tiny_seams`. Evidence: tiny_seam_edges is gone; tests use the production seam path.

- [x] **RK14** (P1, med, oracle-only, effort S). Location at baseline: implicit_point.ml:848-1189,1763,1808 · predicates.ml:42,89,167 (Private). About 200 lines of `*_reference` twins kept only for differential tests. Action: Move them to test support. Evidence: the exact_*_reference predicate twins live in lib/rdk/test_predicates.ml.

- [x] **RK15** (P6, med, duplication, effort S). Location at baseline: boolean_payload.ml vs boolean_corner_payload.ml. storage_kind, collect_attributes, collect_groups, parallel_for, selected and source_storage are written twice; `copy` is re-exported. Action: Merge them into one module. Evidence: Boolean_corner_payload is folded into Boolean_payload, with one storage_kind, collect_* and source_storage.

- [x] **RK16** (P6, med, api-bloat, effort L). Location at baseline: 174 `?(grain = 16_384)`; 15 functions with 15–32 optional arguments (triangulate2d run 32, point_replicate 31, ray 28). SOPs pass every argument explicitly, so rdk's defaults are a second source of defaults that only tests use. Action: Use required parameter records with no defaults; tests build them with one helper. Evidence: ebb268a4 gives Triangulate2d, Ray, Fuse_grid, Subdivide, Boolean, Clean, Facet, Remesh, Plane_clip and Attribute_ops.interpolate required params records. Defaults live in the SOP declarations and test support; Clean's auto epsilon 1e-12 equals the old default.

- [x] **RK17** (P7, med, naming, effort M). Location at baseline: 32 .mli with `module Private`; Attribute.Private.storage ×46, Topology.Private.view ×21 used by sop, flow_sop, rays_editor. Production code depends on the borrowed-plane API that lives in `Private`. Action: Rename it `View` and document it as that API. Evidence: rdk's borrowed-plane Private is renamed View across rdk and every caller; the stale allowlist rows are gone.

- [x] **RK18** (P6, med, vibe-code, effort S). Location at baseline: 13 `owner_count`, 42 local `fail`, 13 `get_ok`, 14 `checked_add`, 5 union-find. Shared definitions already exist in core. Action: `tools/dedupe --qualify Support`, then delete the tool (TT11). Evidence: the 13 boolean tests' local fail/check/get copies are gone; they open Rdk_test_support, which gained failf.

- [x] **RK19** (P6, med, naming, effort M). Location at baseline: Blast_points, Centroid_piece_points, Copy_target_points, Connectivity_points, Group_points …. The owner type is re-encoded with a prefix in each module. Action: Reuse `Attribute.owner` with a validation check. Evidence: da526300: the Connectivity and Mirror owner enums are Attribute.owner (Detail refused), and SOP's element_owner is Group_ops.owner. Dead Blast_by_attribute, Extract_centroid and Copy_target types are deleted.

- [x] **RK20** (P6, med, god-module, effort M). Location at baseline: mesh/subdivide.ml 4,607. Its own fail, get_ok, checked arithmetic and union-find; eight Float2/3/4 copies; three resolve-defaults blocks. `edge_divide` is oracle-only. Action: After RK3 and RK8 it shrinks by 15–20%. Split out the stitching (3263-3700). Evidence: 8a0e21af splits subdivide.ml into Subdivide_refine, Subdivide_stitch and the driver.

- [x] **RK21** (P1, med, god-module, effort M). Location at baseline: curve/curve_ops.ml 2,794. Production uses only `join` plus its endpoint kd-tree (~670 lines). Radix sort is copied 3 times. Action: Keep `curve_join.ml`, move radix sort to Support, delete the rest. Evidence: Curve_ops is Curve_join and holds only the join and its endpoint tree.

- [x] **RK22** (P6, med, test-sprawl, effort M). Location at baseline: test_rdk.ml:204-211 → test_rdk_{attributes,modeling,uv,curves,misc}.ml (3,415). Fixture-chained files that cannot run or fail independently. Action: Make each fixture local and delete the files that test only oracles. Evidence: test_rdk_* files build their own fixtures (no chaining); the orphaned test_rdk_misc is wired into test_main and passes.

- [x] **RK23** (P6, med, test-sprawl, effort M). Location at baseline: 41 tests don't open Rdk_test_support; 42 local equal_geometry, 27 get_ok, 17 check_parallel_exact, 8 two_quads. rdk/AGENTS.md asks for the opposite. Action: Add parallel_exact and the standard fixtures to support and open it everywhere. Evidence: every rdk test except test_main opens Rdk_test_support (fail, failf, check, checkf, equal_geometry, get); the local copies are gone.

- [x] **RK24** (P1, med, test-sprawl, effort M). Location at baseline: ~17.3k test lines + 635 bench lines (bench_{laplacian,rewire_payload,material_assign,…}). They exercise only oracle modules. Action: Delete them with RK1. This also cuts runtest time. Evidence: the tests and benches of the deleted kernels are gone (a4c1eb7e; six rdk benches).

- [x] **RK25** (P1, med, doc-rot, effort M). Location at baseline: remesh.mli:5-8 · implicit_point.mli:145 · rdk/AGENTS.md:37-51 (sketch_ui) · rdk.md:546-3041. Docs describe removed fields, re-exports and nodes. 2.5k spec lines are per-node specs for Lisp-owned nodes. Action: Fix the docs and trim rdk.md to the kernels that remain. Evidence: 512da419 trims specification/rdk.md to the remaining kernels (−1,450 lines), and lib/rdk/AGENTS.md says there is no parity-only code.

- [x] **RK26** (P6, low, layering, effort M). Location at baseline: Sweep/Polywire in curve vs Sweep_circle/Revolve in mesh; Triangulate2d in mesh vs Delaunay2/CDT in exact; Bounds2 in exact vs Bounds3 in spatial. The sublibrary placement no longer means anything, and gen is empty after RK1. Action: Collapse to rdk_core / rdk_geom / rdk_ops / rdk_boolean. Evidence: rdk is rdk_core, rdk_geom, rdk_ops and rdk_boolean; the gen library is gone.

- [x] **RK27** (P6, low, duplication, effort S). Location at baseline: core/polygon_triangulation.ml · contour_triangulation.ml:196 · planar_cdt.ml:1081. Three ear-clipping routines. Action: Route them through `find_projected_ear` where exactness allows. Evidence: f97501e9: Contour_triangulation uses Polygon_triangulation.find_ear ~distinct. planar_cdt keeps its exact-predicate lowest-point ear by design.

- [x] **RK28** (P6, low, dead-code, effort S). Location at baseline: core/data_id.ml (3 lines) · Error.of_string · core/kernel.ml (mostly test helpers). One-line modules and aliases. Action: Fold them into Geometry and test support. Evidence: 070a3f95 deletes core/kernel.ml; generate_point_ranges is Rdk.Geometry.generate_point_ranges. Data_id and Error.of_string were already gone.

- [x] **RK29** (P6, low, vibe-code, effort M). Location at baseline: 75 `type x = M.x = …` re-export lines; attribute_ops.mli 791 lines. A façade layer that exists only to back hidden private_modules. Action: Drop it once the oracles are gone. Evidence: e41b82ce drops the rdk type re-export facades (about 55 lines to 0); the hidden modules are public and callers name them (Attribute_transfer.Nearest, Fuse_rules.Keep_first, Boolean_solid.Union). attribute_ops.mli went from 489 to 374 lines.

- [x] **RK30** (P6, low, naming, effort S). Location at baseline: 13 identifiers of 35+ chars (`promote_include_all_primitives_sharing_boundary_points`). Names that are whole sentences. Action: Shorten them during the RK16 record migration. Evidence: nine long rdk identifiers are shortened; the two kept appear in .rays sources and flow_manifest.sexp.

- [x] **RK31** (P0, low, test-sprawl, effort S). Location at baseline: test_main.ml group labels (test_motion under core, test_triangulate2d under exact …). `@lib/rdk/test_core` runs mesh tests. Action: Fix the labels. Evidence: group labels in lib/rdk/test_main.ml match their modules (test_motion under mesh).

- [x] **RK32** (P6, low, doc-rot, effort S). Location at baseline: uv_parameterize.ml:41 · group.mli:35 · attribute_ops.mli:523. 'legacy' names and two legacy driver-name arguments. Action: Drop them. Evidence: the legacy driver-name doc sentence is gone, and no legacy paths remain in uv_parameterize or group.mli.

- [x] **SP1** (P1, high, oracle-only, effort L). Location at baseline: lib/sop/{groups,sop_topology,attributes,shapes}.ml: 164 PPX node modules, 37 registered. About 10.6k of 14.2k lines are OCaml twins of Lisp defnodes, which makes AGENTS' 'declared once' false for 128 nodes. Action: Capture goldens from the Lisp nodes (parity already holds, so no native code is needed), then delete the 128 modules and their Support helpers (prune-dead-code to a fixpoint). Evidence: 868f9ae0 deletes the 127 OCaml twins that have a Lisp defnode; Sop.Nodes lists the 37 native nodes, and the Lisp goldens (819 rows) are the net.

- [x] **SP2** (P1, high, test-sprawl, effort M). Location at baseline: test_nodes.ml 7,494 · port_tests 9,028 (≈30 min) · test_lisp_ports.ml 845 (27–45 s in runtest) · 4 per-node test files 1,310. Lisp-vs-native parity is tested four times. Action: Turn the port fixtures into plain Lisp-node golden tests with no native side. Delete the other three. Evidence: the port fixtures are plain Lisp-node goldens in lib/flow_sop/goldens with no native side; the other parity suites are deleted (b58926a8).

- [x] **SP3** (P0, high, dead-code, effort S). Location at baseline: lib/flow_sop/dune:90-91,206-207,216-217,226-232. Five `*_draft` aliases depend on `nodes/*.pending` files that no longer exist, so these aliases fail if run. Action: Delete the 4 executables and 5 aliases.

- [x] **SP4** (P0, high, dead-code, effort S). Location at baseline: lib/sop/edit_graph.ml:25,45,119,186-208. `bypass` is only ever constructed as `false`, yet 7 product sites read `is_bypassed`. Real bypass is Lisp `^:bypass`. sop.md:447 documents the dead version. Action: Remove the field, its readers, the compile branches and the spec paragraph.

- [x] **SP5** (P0, high, dead-code, effort M). Location at baseline: lib/sop/diagnostic.ml:1-15 · session.ml:288,580-593,646. Non-fatal `Info|Warning` diagnostics are never produced (`Support.cooked` passes `[]`), yet Session concatenates and dedups them. Action: Delete the type and the field. Evidence: the SOP diagnostic module and Session diagnostic aggregation field are absent; product-source search finds no SOP non-fatal diagnostic path. `@lib/flow_sop/test_lisp_node` checks a strictly unsupported numeric body is refused at cook time with `E_KERNEL_FORM` and source span, so strict errors remain active.

- [x] **SP6** (P0, high, correctness-risk, effort S). Location at baseline: lib/flow_sop/lower.ml:1558. iso_surface defaults are read from the oracle `Sop.Nodes.Iso_surface.factory`, while the Lisp defnode owns them. They can drift, and deleting the oracles breaks this line. Action: Read the defaults from the checked node definition. This is a prerequisite for SP1. Evidence: field_calls reads the workspace's checked Lisp factory schema. A small custom declaration changes the resolution and all bounds; the regression checks the resulting eight samples and both endpoint values (test_node_sources, 0.90 s), so consulting the stock catalog or oracle fails the check.

- [x] **SP7** (P0, high, correctness-risk, effort S). Location at baseline: rays_editor/workspace_images.ml:462-466 → Sop.Image_nodes.noise. Re-declares image/noise defaults and a `freq` alias, and misses `hard_min 1`. It calls a typed OCaml constructor, which the rules say should not exist. Action: Cook through `noise_factory` and delete `Image_nodes.noise`.

- [x] **SP8** (P3, high, god-module, effort L). Location at baseline: flow_sop/lower.ml:166-1468 lower_checked. One ~1,300-line function with 6 optional flags and ~50 hard-coded kind strings. Action: Per-kind lowerings dispatched from the Operators table. Evidence: lower_checked is split under 300 lines, and Operators.lowering tags the natively lowered kinds; lower.ml's natives table is keyed by that tag, with the kind strings in operators.ml beside their builders.

- [x] **SP9** (P3, med, parallel-system, effort M). Location at baseline: Edit_graph.factory · Flow_sop.Catalog.descriptor · Flow.Defnode.t · Flow.Catalog.kind. Four shapes of node metadata. Each node has three names (qualified, key, operation), and 7 nodes have key ≠ operation. Action: One descriptor with `operation = key`; rename the 7. Evidence: f12bbecc: node keys equal their operations; six defnode :operation overrides are removed, and attribute_noise_quaternion and boolean_fracture have their own operations. The manifest diff is only the renamed operations, and the Lisp goldens pass unchanged.

- [x] **SP10** (P1, med, vibe-code, effort S). Location at baseline: ppx_rays.ml:612,618,623 · support.ml:65-72 (+4 re-imports). The PPX hard-codes `Sop.Parameter`/`Sop.Edit_graph`, which forces a fake `module Sop = struct … end` shim. Action: Have the PPX emit `Param.*` and drop the shim. Evidence: the PPX's -inside-sop flag (set in lib/sop/dune) emits bare Node and Edit_graph paths; the Sop shim in support.ml and its four re-imports are deleted.

- [x] **SP11** (P1, med, api-bloat, effort M). Location at baseline: lib/sop_catalog (37 lines + PPX transform) · nodes.ml (165 aliases) · nodes.mli (165 sigs). Three hand-maintained lists per native SOP, and `[@@sop.register]` only wraps a list. Action: After SP1, put `Sop.Nodes.factories` (37 entries) in place and delete sop_catalog and the transform. Move the manifest rule to flow_sop. Evidence: Sop.Nodes.factories (37 entries) replaces sop_catalog and the [@@sop.register] transform; the manifest, its generator and its diff rule live in lib/flow_sop.

- [x] **SP12** (P1, med, layering, effort S). Location at baseline: test/lisp_sop/dune links editor_document (and so rays, SDL, Metal). Every sop unit test waits on the runtime build because of one `Contexts.descriptors` call. Action: Split SOP-only lowering from scene kinds, or make descriptors optional. Evidence: helper source and its direct/transitive dependencies use the Flow/SOP catalog without editor_document or runtime; focused port/facts/source checks pass.

- [x] **SP13** (P1, med, layering, effort S). Location at baseline: lib/sop/dune:9-16 test_facts, test_lisp_ports. Tests of flow_sop live in lib/sop and link the layers above it. Action: Move or delete them (SP2). Evidence: the Lisp suite and declaration/facts block now belong to flow_sop; remaining SOP facts have no direct Flow/catalog dependency. Recipe/facts behavior, domain parity and focused aliases pass unchanged.

- [x] **SP14** (P0, med, dead-code, effort S). Location at baseline: port_gate.ml:3,27,139 · 74 'pending' mentions · 67 '(pending draft)' headers · ports alias glob. The migration workflow survives after the migration ended. Action: Strip the pending path. Evidence: no '(pending draft)' headers, port_gate or ports alias remain.

- [x] **SP15** (P0, med, doc-rot, effort S). Location at baseline: lib/sop/custom.mli · sop.md:155,430-432. Presents itself as 'the OCaml wrangle-like extension boundary'. `Custom.map` is used only by tests, and `Custom.create` is documented but does not exist. Action: Delete `map`; rewrite the doc comment and spec.

- [x] **SP16** (P5, med, layering, effort L). Location at baseline: editor_document/objects.ml:12-13 · layers.ml (11 `deriving sop_params, sop_node`). Scene, world and settings kinds are SOP nodes that cook empty geometry, only to get a schema and a menu entry. Action: Declare them as Param schemas plus descriptors with no Node/Session. Evidence: 3a77df77: scene, World and settings kinds are Param schemas in Editor_document.Kind_graph (kind = schema, key, label, category, defaults; instance = id, kind, label, values, one input). All 11 sop_node factories are gone. Document.scene is a Kind_graph.t and the World layers live in Document.layers. The Lisp descriptors and flow_manifest.sexp are unchanged, and the editor, draw-snapshot and UI parity suites pass.

- [x] **SP17** (P3, med, perf-risk, effort M). Location at baseline: flow_sop/curve.ml:8-20 · value_lane.ml:52 · lower.ml:1030. sop/curve points are stored as `"%h,%h,%h;…"` text, re-encoded every live frame and decoded every cook. Action: Pass them as a typed Vec3 array through Network.drives. Evidence: live drives carry Vec3 arrays (Curve.replace_points); only authored points use text, for parameter views.

- [x] **SP18** (P3, med, vibe-code, effort S). Location at baseline: flow_sop/lower.ml:152-164 · 126-128. A digest cache keyed on `Obj.repr` physical identity in an 8-entry Atomic list, and global profiling refs. Action: Key on a workspace content id and pass profiling explicitly. Evidence: 90d282eb replaces the global Private.record/profiles with an explicit ?on_profile on Lower.workspace, of_checked and lower_checked; the digest cache no longer uses Obj.

- [x] **SP19** (P0, med, doc-rot, effort S). Location at baseline: .claude/skills/add-sop/SKILL.md · untracked .agents/skills/add-sop. The skill is OCaml-first, cites a nonexistent test and uses the wrong constructor. An older prismel/pdk copy is also lying around. Action: Rewrite it Lisp-first and delete `.agents/`.

- [x] **SP20** (P0, med, doc-rot, effort S). Location at baseline: flow_sop/lower.mli:26-28. Says merge lowers to `flow.merge_n` over `Sop.Nodes.Merge`; neither exists. Action: Fix the text.

- [x] **SP21** (P7, med, test-sprawl, effort M). Location at baseline: lib/flow_sop/dune (304 lines, ~40 one-off stanzas, misnamed 'draft' aliases in runtest). Each test gets its own stanza. Action: One `tests` executable with sections. Evidence: 24 flow_sop unit-test stanzas are one test_units runner.

- [x] **SP22** (P0, low, dead-code, effort S). Location at baseline: rays_editor/dune:10 `(pps rays.ppx)`. No file in the editor uses it. Action: Remove it.

- [x] **SP23** (P1, low, api-bloat, effort S). Location at baseline: lib/sop/graph.ml (57 lines, `type t = Node.t`). A wrapper module. Action: Fold it into Node. Evidence: Graph is folded into Node (Node.info, inspect, find, graph_dependencies).

- [x] **SP24** (P1, low, dead-code, effort S). Location at baseline: Edit_graph.of_graph/compile/factory_arity · Async_cook.await · Session.Private.clear_cache_keep_timings · ppx sop.name/sop.ignore/sop.node_keywords. Test-only or unused. Action: prune-dead-code after SP1. Evidence: prune-dead-code ran after SP1 (a4c1eb7e).

- [x] **SP25** (P0, low, dead-code, effort S). Location at baseline: Flow.Catalog.kind.aliases (check.ml:316-319, workspace.ml:2034). All 177 manifest entries are `(aliases)` with an empty list. Action: Delete it until a rename needs it.

- [x] **SP26** (P1, low, api-bloat, effort M). Location at baseline: `~factories` threaded through 107 product signatures. It is always `Sop_catalog.Editor.factories` except in tests. Action: Make it an optional argument defaulting to the catalog, or a module-level constant after SP11. Evidence: factories is an optional argument defaulting to Sop.Nodes.factories in Lower, Lowering, Document_edit, Scene_sync, Preset and Workspace_parity; 50 explicit default passes are gone.

- [x] **SP27** (P7, low, naming, effort S). Location at baseline: flow_sop *_kernel modules; instance_path.ml (2 lines); bench_planar_attribute.ml in library sources. The kernel naming is inconsistent and a bench sits inside the library. Action: Rename or merge them; inline instance_path; move the bench out. Evidence: Instance_path is inlined as Lower.Path_map, and the planar-attribute bench is in tools/.

- [x] **SP28** (P0, low, doc-rot, effort S). Location at baseline: AGENTS.md '128 of the 162 SOP nodes' · nodes.mli:1-2 · node.mli:51. The manifest has 165 sop kinds (128 Lisp + 37 native), and the comment says 'declared once' when they are not. Action: Stop quoting counts and fix the text.

- [x] **FL1** (P3, high, parallel-path, effort L). Location at baseline: core_exec.ml:387 counted_kernel vs packed.ml:298,862. Two 1,024-lane block compilers for the same pure packed map. Action: `Core_lower` emits `Invoke` for every pure map; delete `counted_kernel`. Evidence: the alternate Counted block compiler and prepare switch are deleted. Pure packed maps use Region Invoke; structured effectful loops retain their ordinary statements. The full focused Flow IR suite passes, including direct/reference, state and domain comparisons.

- [x] **FL2** (P3, high, parallel-path, effort S). Location at baseline: core_exec.ml:114-166,348 scalar_tree. A third executor: a postfix stack VM for straight-line programs of 64 ops or fewer, with no benchmark cited. Action: Delete it (locked). CPU speed comes from the Packed block engine; gate: bench_core shows no regression. Evidence: the graph IR (of_evaluation … groups, compile_marks) has no hits in lib/bin/tools/test.

- [x] **FL3** (P3, high, layering, effort L). Location at baseline: flow/eval.ml:110-158,345-374. The reference walker carries hooks for every downstream executor (execute, compiled_residuals, bulk, observe_kernel, a mutable `residual.core` cache), wrapped in `try … with _ -> None`. Action: Invert this: Flow_ir walks residuals and calls Eval only as a leaf oracle.

- [x] **FL4** (P3, high, parallel-path, effort L). Location at baseline: flow_ir.ml:849-935 Executor.force. A three-level fallback chain (Core, then Packed template, then walker), and each level re-runs the walker on failure. DEFERRED.md estimates about 12 walks per residual. Action: One dispatcher that lowers once and walks once, only to produce the diagnostic.

- [x] **FL5** (P3, high, dead-code, effort M). Location at baseline: flow_ir.ml:285-583,752,1075-1124. The graph IR (of_evaluation, share, hoist, prune, fuse, place, optimize, groups) is built on every compile, and product code reads only the root's classification. Action: Delete it (~500 lines); keep `classify` plus the approx-sink check. Evidence: scalar_tree and counted_kernel are gone; Packed blocks are the one CPU block engine. The scalar no-regression gate (decision 11) is tracked separately in benchmarks/scalar-core.md.

- [x] **FL6** (P3, high, duplication, effort M). Location at baseline: flow_gpu/emit.ml:78 · flow_web/wgsl.ml:104,270. Three copies of the kernel layout planner, and the `msl`/`wgsl` records are identical field for field. Action: `Flow_ir.Kernel_layout.plan ?fast`; each target keeps only its instruction printer. Evidence: emitter and WGSL tests compare uniform slots/bytes, seeds, gather widths, count offsets, image dimensions, and binding order to the shared plan.

- [x] **FL7** (P3, high, naming, effort M). Location at baseline: packed.ml:31,89,179 of_region/to_region. `Packed.t` is `Core.Region.t` plus one field, and 20 fields are copied back and forth (+9 to +50% compile time). Action: `Packed.t` becomes a private alias of a validated Region, with `dependent` computed lazily. Evidence: private validated Region and Packed share one representation; checked capture rebinding preserves its layout without rescanning instructions. Atomic first-use memoization computes dependency flags before parallel dispatch. Negative layout checks, lazy first-use and full Flow IR/GPU/Web focused checks pass.

- [x] **FL8** (P3, high, layering, effort M). Location at baseline: tools/lisp/web_export.ml:206-289. Product placement logic lives in tools/ and re-derives `Core_exec.device_regions`. Attribute fusion exists for web only (the native gap is listed in DEFERRED.md). Action: Move placement and grouping into Flow_ir, shared by `Core_exec.draw` and the exporter. Evidence: both consume Core_draw.device_groups; native fused outputs share one runner generation with independently checked output indices. Scope, precision, count, argument captures, static staging and authored-order refusal have focused checks. Native pixels, browser parity and the paired at-most-six-ms measurement pass; see status.md and benchmarks/marks-native.md.

- [x] **FL9** (P3, high, duplication, effort M). Location at baseline: core_exec.ml:27,68,396,631,673 · web_export.ml:88,224 · js.ml ×3. Core statement traversals are written by hand about 10 times. Action: Add `Core.reads/writes/children/iter` and rewrite each analysis as a fold. Evidence: Core execution analyses, lowerer scan, web capture checks, web-export statement flattening, and regression-test visitors share the preorder/operand helpers; scope-sensitive lowering, branch analysis, and code emission retain their own contexts. `@lib/flow_ir/runtest` and `@lib/flow_web/runtest` pass.

- [x] **FL10** (P3, high, duplication, effort S). Location at baseline: web_export.ml:11 · projection.ml:77 · flow_edit.ml:844 · core_region.ml:626 · flow_ir.ml:785-813 · eval.ml. `Workspace.term` has no `children` function, so term walks are re-implemented. `Error_rows{` is matched by hand in 12 files. Action: Add `Workspace.children`/`map_children`. Evidence: web export, graph projection, editor type lookup, kind-call discovery, and the generic workspace test visitor use shared children; the type lookup keeps its declared-body scope for specialized `Call_fn`/`Fn` terms. `@lib/flow/runtest` and `@lib/flow_graph/runtest` pass.

- [x] **FL11** (P3, high, god-module, effort L). Location at baseline: workspace.ml:347-2148 check (1,802) · core_lower.ml:82-1474 (1,393) · packed.ml:862-1537 (675). Single functions spanning several concerns. Action: Split the checker into names, types, packed eligibility and zones; split lowering into exprs, callables, loops and draw. Evidence: Workspace.check (1,796), Core_lower.lower_with (1,249) and Packed.run_all (630) are each under 300 lines, with their budget rows removed (0cda0bc1, 1b9f52ab). Flow, IR, web, GPU and SOP suites and goldens pass.

- [x] **FL12** (P0, med, dead-code, effort S). Location at baseline: tools/lisp/web_server.mli. An interface with no implementation, documenting a `--watch` server that no longer exists. Action: Delete it.

- [x] **FL13** (P3, med, dead-code, effort S). Location at baseline: flow_web/target.ml:23-43 · Flow_ir.place ?target. Used only by tests; the web export never calls `place`. Action: Delete it. Evidence: Target.web is gone.

- [x] **FL14** (P0, med, dead-code, effort S). Location at baseline: flow_ir.ml:278 sink State_seed/Cache_key/Export; tier Gpu_compile. Variants that are never constructed. Action: Remove them. Evidence: State_seed, Cache_key and Export are absent from lib; Gpu_compile is constructed (flow_ir.ml).

- [x] **FL15** (P0, med, naming, effort S). Location at baseline: flow_ir.ml:231,241,570 · core_scope.ml:210. Tier `Closure` survives the deleted closure tier and is shown to users. plan.md:198 cites a function that no longer exists. Action: Rename it to `Core` and fix plan.md.

- [x] **FL16** (P0, med, correctness-risk, effort S). Location at baseline: eval.ml:363,1302 · flow_ir.ml:45,913 · core_exec.ml:1104 · core_lower.ml:611 · packed.ml:657,792. Catch-alls that also swallow Stack_overflow, Out_of_memory and assertion failures, turning crashes into a silent slow path. Action: Catch only the named refusal exceptions.

- [x] **FL17** (P3, med, correctness-risk, effort M). Location at baseline: core.mli:1 'Serializable' · Region.count_source closure · Core.expr.Source holds Eval.value. The IR contract says closure-free, but it carries closures and residuals. Action: Resolve captures to argument ids at lowering, or correct the contract. Evidence: 65d12fa4: captures inside host Sources lower to Host_source arguments, so a Core program apart from its regions is plain data. A Marshal-without-Closures round trip checks every Core program in the lowering, draw and workspace-snapshot tests.

- [x] **FL18** (P3, med, layering, effort M). Location at baseline: flow_ir.ml:27-68 Gpu module. A cross-domain mailbox (Mutex, Condition, job queue) plus DLS policy inside the IR library. Action: Move scheduling to flow_gpu or the renderer; keep only the backend record type. Evidence: Flow_ir.Gpu retains only backend/value/kernel/config data; the per-workspace Flow_native owner owns scheduling and lifetime. Explicit precision/policy configuration reaches actual startup, worker cooks, reload, presets and deferred attribute sinks. Native service, full IR/Web, focused sink, startup and editor/PXUI integration checks pass; cache keys distinguish precision and owner close runs under Fun.protect.

- [x] **FL19** (P3, med, vibe-code, effort S). Location at baseline: drawing.ml:66-77 E.Private.force_with_executor. Returns a dummy `E.Int 0` while the real result rides out in a ref. Action: Expose `Eval.with_residual_frame`. Evidence: force_with_executor has no hits.

- [x] **FL20** (P3, med, api-bloat, effort M). Location at baseline: eval.mli:193-371 Private (33 vals, 21 non-test files). This is the real API, not private. Action: Promote the needed subset to `Eval.Residual` and delete the rest. Evidence: Eval.Private is now Eval.Host, a named host API.

- [x] **FL21** (P3, med, api-bloat, effort S). Location at baseline: packed.mli Private: trace (global ref swapped by 9 tests), ordered_add, view, stage_count, reference. Test hooks and internals in the public interface. Action: Delete trace, ordered_add, stage_count and reference; pass `?trace`. Evidence: the global trace is a ?trace argument, stage_count is gone, and Packed.reference and ordered_add live in Packed.Host, the named downstream surface, off Packed's product API.

- [x] **FL22** (P2, med, layering, effort S). Location at baseline: flow/op.ml · flow_ir/operators.ml (noise3, drawing_color) · flow_sop/operators.ml. Operators are spread over three libraries only because flow lacks rays_math. Action: flow depends on rays_math; move them into `Op` and delete the `~extra` threading. Evidence: flow depends on rays_math, noise3 lives in Flow.Op, and flow_ir/operators.ml is deleted.

- [x] **FL23** (P3, med, duplication, effort S). Location at baseline: rays_math · emit.ml:8 · wgsl.ml:29 · js.ml:3 · tools/web/runtime/prims.js:696-744. Perlin noise five times, two of them in the same JavaScript target. Action: Have JS regions import the prims.js noise. Evidence: the shared Kernel_layout landed with P3.S5.

- [x] **FL24** (P2, med, duplication, effort M). Location at baseline: ~20 local exceptions (Value.Fail, Macro.Fail, Packed.Failed, Js.Refused, 4× Unsupported) and mixed result types. Inconsistent error plumbing. Action: One `Diagnostic.Error` exception and `(_, Diagnostic.t) result` everywhere. Evidence: audit: Value.Fail, Macro.Fail, Packed.Failed and Js.Refused raise Diagnostic.Error; the rest are file-local control flow, or Core_region.Unsupported as a refusal signal. rdk keeps its structured Error, converted by sop.

- [x] **FL25** (P0, med, perf-risk, effort S). Location at baseline: eval.ml:194 state_key · eval.ml:180 state_stamp. `Marshal.to_string` on every state read and write. A digest of the whole marshalled state is the cook cache key at 6 sites. Action: Use structured keys and a state generation counter.

- [x] **FL26** (P0, med, doc-rot, effort S). Location at baseline: eval.mli:1,10,205-206 · flow_ir.ml:234,245 · wgsl.mli:5 · web_export.mli:2 · core.md:12,321,1138. Plan ids, 'the study', 'W2', and a duplicated sentence. Action: Strip them.

- [x] **FL27** (P0, med, doc-rot, effort S). Location at baseline: workspace/iteration.md:523-532,592 · embed.ml:2 · frames.mjs:2. Documents `rays-lisp ml` generating main.ml, which contradicts 'nothing generates OCaml from Lisp'. Action: Rewrite or delete those sections.

- [x] **FL28** (P5, med, layering, effort M). Location at baseline: flow_graph/edit_layout.ml:7-30 · flow_edit.mli:92-138. The 'domain-neutral' flow_graph hard-codes editor panel kinds and window gestures. Action: Move the panel table to editor_document, passed as a parameter, and split the layout requests out. Evidence: e030c034: the 11 layout ops live in Editor_document.Layout_edit (label, gesture, remap, merges, apply_checked) over the public Flow_graph.Edit_tree helpers. pxui_graph emits a neutral Layout_active request, and the layout constructors are gone from Flow_edit.op.

- [x] **FL29** (P3, med, test-sprawl, effort M). Location at baseline: lib/flow_ir/dune: 30 test executables; test_workspace_ir 20–25 min. Tier-parity tests multiplied by the number of tiers. Action: After P3, one lowering golden test plus one walker-vs-core sweep that walks each residual once. Evidence: flow_ir has 8 test stanzas (from 30).

- [x] **FL30** (P3, low, naming, effort S). Location at baseline: flow/check.ml. It is the catalog description, not a checker (the checker is `Workspace.check`). Action: Rename it `Catalog`. Evidence: 6d7e698c renames Flow.Check to Flow.Catalog in one mechanical commit.

- [x] **FL31** (P0, low, dead-code, effort S). Location at baseline: flow/symbol.mli `private string` type. Used only by test_value_core. Action: Drop the abstract type.

- [x] **FL32** (P3, low, vibe-code, effort S). Location at baseline: packed_ops.mli:5 `type extension = Noise3`. A one-constructor variant threaded as an option. Action: Fold it into `Op.capability`, which already has Noise3. Evidence: `packed_extension` uses the existing `Op.packed_kind`; the duplicate Packed_ops type is deleted. Explicit unsupported kinds are rejected and canonical/inherited noise semantics remain checked. Full Flow, IR, graph and encoded-parameter checks pass in 20.83 s, with no golden changes.

- [x] **FL33** (P3, low, layering, effort S). Location at baseline: flow/phase_timer.mli:3. The language library's phase enum contains editor phases. Action: Use string phases, or move the timer to editor_core. Evidence: Phase_timer phases are plain strings.

- [x] **FL34** (P3, low, duplication, effort S). Location at baseline: pairs ×4, pat_names ×2, pattern_names ×2. Syntax helpers re-declared. Action: Export them once from Flow.Syntax. Evidence: ec2d7266: one Flow.Syntax.pairs, aliased by workspace, projection, edit_base and text_pane.

- [x] **FL35** (P3, low, duplication, effort S). Location at baseline: kernel_table.ml:1-3 · web_export.ml:101-102 · js.ml ty_json. Three hand-rolled JSON emitters. Action: One small module in flow_web. Evidence: web_export shares Kernel_table's JSON writers; js.ml's ty_json emits JS object literals, not JSON.

- [x] **FL36** (P3, low, vibe-code, effort S). Location at baseline: flow_web/js.ml:892 bindable. Uses exceptions (Not_found, Invalid_argument, Failure) as a yes/no answer. Action: Return an option from `binding_plan`. Evidence: binding_plan raises only Diagnostic.Error and bindable catches only that.

- [x] **FL37** (P0, low, correctness-risk, effort S). Location at baseline: tools/lisp/rays.ml:62-67. `rays web` writes files non-atomically, so a server can read a half-written export. Action: Write to a temp file and rename (as Store already does).

- [x] **FL38** (P3, low, api-bloat, effort S). Location at baseline: core_exec.mli run, fast. Used only by tools and by the shortcut that FL3 removes. Action: Delete them. Evidence: Core_exec.run and Core_exec.fast are gone; tests use Test_program.run from flow_ir_test_support.

- [x] **UI1** (P5, high, god-module, effort L). Location at baseline: rays_editor/core_*.ml (17 files, ~6.1k lines), each `include`s the previous; Environment_* repeats this 4 times. One 6k-line namespace split into files by line count. File names don't match contents. Action: Use explicit modules over `Core_model.t` with no includes: Panes, Inspect, List_view, Text, Status, Reduce, Frame. Evidence: no core_*/environment_* file includes a sibling; the modules open what they use over Core_model.t (6e19965f, ffcc4a24).

- [x] **UI2** (P5, high, god-module, effort L). Location at baseline: rays_editor/core.ml:11 update_frame (1,256). Contains a whole second node inspector inline (774-870). Action: Split per pane builder. Evidence: Core.update_frame (744 at the split) builds each pane in its own function and is under 300 lines (fbb3db7e); the inline second inspector is gone.

- [x] **UI3** (P5, high, god-module, effort L). Location at baseline: rays_editor/core_reduce.ml:9 reduce (677, 22 labelled arguments, 5 local refs). A single reducer with refs threaded through it. Action: A `frame_input` record and per-intent reducers that return values. Evidence: Editor_reduce.reduce takes a Frame_input.t record instead of about 22 labelled arguments, and write_gestures threads an immutable gesture_log; no refs remain in the changed code.

- [x] **UI4** (P5, high, layering, effort M). Location at baseline: rays_editor/{drawing,marks_scene,workspace_gpu,workspace_images,workspace_resources,workspace_host,live_frame}.ml. The Flow native draw runtime lives inside the editor; none of these touches Core or Pxui. Action: New `flow_native` library.

- [x] **UI5** (P5, high, test-sprawl, effort S). Location at baseline: marks_scene_test/dune:2 · native_qualification/dune:1-9 · tools/dune:241 · gate_test/dune:2. marks_scene.ml is compiled 4 times and drawing/workspace_* twice, so one binary carries two `Marks_scene` modules. Action: UI4 removes all of the copies. Move `Gate` into `input`.

- [x] **UI6** (P5, high, layering, effort S). Location at baseline: rays_editor/surface.ml, packed_pieces.ml. SOP-to-Scene3 glue over rdk, with one editor call site. Action: Move them to rdk_rays; the editor drops its direct dependency on rdk. Evidence: the editor reads geometry through Rdk_rays.Geometry_view (probe summary, bounds, sheet columns, wire mesh, pick and cast); rays_editor no longer depends on rdk.

- [x] **UI7** (P0, high, doc-rot, effort S). Location at baseline: pxui_graph/AGENTS.md:42,44 · flow.md:37 vs flow.md:356 · scope_state.ml:145. The normative docs contradict each other: 'No curves' vs curves by default. Action: Settle on one style and delete the losing styles (UI8).

- [x] **UI8** (P0, med, dead-code, effort S). Location at baseline: scope_wires.ml:316 `Rect · scope_state.ml:108 · core_scope.ml:154-157. Three wire styles, and no sketch uses `:wires`. Action: Keep one style.

- [x] **UI9** (P5, high, duplication, effort M). Location at baseline: ui.ml:2359,2391,2041 · pxui_shell/kit.ml:91 · Ui.inspector_* (15) · camera_controls number. Four widget kits. `Kit.switch` repaints `Ui.paint_switch` line for line. Action: One `Ui_kit` in pxui used by shell and graph; delete Kit and `Ui.button`/`toggle`. Evidence: Kit.switch paints through Ui.paint_switch; one number field (Param.Number, Ui.number_field).

- [x] **UI10** (P5, med, duplication, effort S). Location at baseline: pxui_graph/scope_pane.ml:18 num_field. A copy of Kit.number, forced because the gate keeps pxui_graph off pxui_shell. Action: Goes with UI9. Evidence: 82a616dc moves Number to Param.Number; Pxui.Ui.number_field is the one field, Kit.number is deleted and Scope.num_field wraps it.

- [x] **UI11** (P5, high, god-module, effort M). Location at baseline: pxui/ui.ml 4,072 (text_area_submit 612 lines at :3193). The engine is mixed with the widget kit and text editing. Action: Split into ui.ml (engine), ui_text.ml and ui_kit.ml, re-exported through `Ui`. Evidence: 3a161de9 splits pxui/ui.ml into Ui_core (engine), Ui_kit and Ui_text along existing seams, with ui.mli unchanged; UI parity is exact.

- [x] **UI12** (P5, med, layering, effort S). Location at baseline: pxui/camera_controls.ml. An editor camera panel inside the UI engine. Its only user is viewport.ml, and `navigate (_ : t)` ignores its argument. Action: Move it into viewport.ml. Evidence: fa632ca6 moves Camera_controls from pxui into rays_editor/viewport.ml, deleting the Pxui export and the ignored navigate argument; specification/pxui.md is updated.

- [x] **UI13** (P5, high, duplication, effort M). Location at baseline: environment.ml:97-128 · viewport.ml:269-276 · scope_commands.ml:322-325. A second capture and hit path: raw mouse folds, a hand-rolled 4-point threshold, raw rect grabs, and key edges detected by hand. This breaks the 'one engine' rule. Action: Use `Ui.box` drags with `signal`/`capture`, and take key edges from the Router. Evidence: Ui.capture_unclaimed lets the nearest flagged ancestor capture a press no clickable descendant took, in the one press path (about 12 lines), and lib/pxui/test_input covers it. The World drag and viewport grab read the view pane's capture signal; graph hints take presses from the canvas signal and key edges from the Router.

- [x] **UI14** (P5, med, layering, effort M). Location at baseline: editor_document/contexts.ml (1,129). A grab bag: kind schemas, window settings, scene walking, UI panel tree, editor eval, `of_workspace` (203 lines) and sha256. Action: Split into Kinds, Lowering and Shell_eval. Move sha256 to Store and drop digestif. Evidence: Kinds and Shell_eval are split out of Contexts, each with an .mli (0cf26259, 840edbf1); Contexts went from 1,110 to 787 lines.

- [x] **UI15** (P5, med, correctness-risk, effort S). Location at baseline: contexts.ml:125 last_catalog, :224 last_fast · drawing.ml:8. Hidden global Atomic caches in a library that is supposed to be pure. Action: Pass the caches explicitly. Evidence: the global last_catalog cache is gone; each Document.workspace carries its own lazy catalog, and the precision is a workspace field.

- [x] **UI16** (P5, med, layering, effort S). Location at baseline: rays_editor/doc.ml. Document reducers with no UI dependency. Action: Move it into editor_document as `Document_edit`. Evidence: rays_editor/doc.ml is gone; editor_document/document_edit.ml owns document edits.

- [x] **UI17** (P5, med, api-bloat, effort M). Location at baseline: editor_document/document.ml:112 `workspace : Workspace_doc.t * Lower.t`. A tuple read through fst/snd about 98 times. Action: Make it a record. Evidence: ee7e426e makes Document.workspace a record {ws; lowered; fast} and rewrites about 132 fst/snd reads; @check and @all build.

- [x] **UI18** (P5, med, duplication, effort M). Location at baseline: core_model.ml:124-125 Set_parameter/Rename by Sop int id · core.ml:774-870 vs core_inspect.ml:344. A second edit route and a second inspector beside text-first Syntax_edit. Action: Route through `workspace_inspector`; keep Set_parameter only for host objects. Evidence: e9e3f4e3: scene objects and World layers that the text binds by name are inspected by workspace_inspector over a graph projection; Editor_scope.scene_target builds one when the pane shows another graph. test_scene_tree changed only in slider position and undo label. Host-made objects and loop copies keep the derived-node sheet.

- [x] **UI19** (P0, med, dead-code, effort S). Location at baseline: leader.ml:199 keymap3 · core_setup.ml:48 ?keymap · core_model.ml:264. Two keymaps, only one used. Action: One `Leader.keymap`; drop the parameter.

- [x] **UI20** (P0, low, vibe-code, effort S). Location at baseline: leader.ml:215-216 text_keys. Commands with fake actions ('placeholders the sheet never runs'). Action: A plain (label, trigger) list.

- [x] **UI21** (P5, med, api-bloat, effort M). Location at baseline: rays_editor.mli:66-348 Private (~13 modules, ~69 vals) + Editor.Private (16). Re-exported only so tests in test/ can reach them. Action: Most of it goes away after UI4, UI6, UI16 and UI22; test the rest in the libraries that own it. Evidence: rays_editor.mli's Private re-export (about 70 values) is deleted; tests and tools reach private modules through dune's library alias (Rays_editor__.X), and Editor.Private keeps 16 values.

- [x] **UI22** (P5, low, layering, effort M). Location at baseline: rays_editor/lisp_text.ml (789). Parinfer, completion and description of Lisp, pure except for one adapter. Action: Move the pure part next to Flow.Lisp (with XC16). Evidence: the pure part of lisp_text.ml (tokens, bracket pairs, indent, context around a byte) is Flow.Lisp_scan with an .mli; theme colouring, completion vocab and the lex cache stay in the editor.

- [x] **UI23** (P0, med, dead-code, effort S). Location at baseline: Editor.take_export, set_render_status, ?status, ?overlay, ?lights, ?background · Renderer.of_env. No callers, or leftovers from the OCaml-sketch API. Action: Delete them.

- [x] **UI24** (P0, med, vibe-code, effort S). Location at baseline: rays_editor.ml:65-96. Identity wrappers, and ~20 optional arguments re-listed only to rename `~scene3` to `~draw`. Action: Use one name and alias directly. Evidence: the scene3/draw rename wrappers are gone; d5acc6a2 removes the view_camera/film identity wrappers. create/run keep their optional lists because they narrow Environment to rays_editor.mli.

- [x] **UI25** (P5, med, api-bloat, effort L). Location at baseline: `'prepared` type parameter through Core.t, Cook.t, carry, update. The only production instantiation is Surface; everything else passes unit. Action: Make it concrete after UI6. Evidence: the prepared type parameter is gone; Cook.prepared = {geometry; surface}, Editor.prepare and Editor.draw are the app's functions, and tests use the editor_prepare test library.

- [x] **UI26** (P5, med, god-module, effort L). Location at baseline: core_model.ml:205 Core.t (~80 fields; active plus parked pane copies). Swapping panes in and out needs `as_pane` rules. Action: A single `Panel_map : panel_key -> pane_state`. Evidence: 7bff17d3: one Panel_map (Core.t.panes) replaces ten active/parked pane fields; Core_shell.stash is deleted.

- [x] **UI27** (P5, med, god-module, effort M). Location at baseline: pxui_graph/scope_pane.ml:111 update (938). One gesture function for everything. Action: Gesture handlers that return a change list. Evidence: Scope_frame's context, tile, wire, pointer and frame steps return (state, changes), and tile_gestures returns the next lens, so update has no lens ref.

- [x] **UI28** (P5, low, god-module, effort M). Location at baseline: environment.ml:8 (306) · text_pane view (323) · navigator view (352). Long view functions. Action: Split them after UI1–UI3. Evidence: update_with is 146 lines (was 246), Text_pane.view about 95 (was 275), Navigator.view about 145 (was 195).

- [x] **UI29** (P5, med, duplication, effort M). Location at baseline: pxui_shell/tree.ml (431) · rays_editor/navigator.ml (726). Two tree widgets with their own chevrons and indents. Action: Build Navigator on Tree rows. Evidence: owner decision 2026-10-28: Navigator keeps its own row geometry on the shared kit paints; ui_parity stays exact.

- [x] **UI30** (P0, low, dead-code, effort S). Location at baseline: pxui_shell/shell.ml (4 lines) · rays_editor/common.ml · selection.ml:9-13. A wrapper module, a module with one function, and ignored arguments. Action: Inline and delete them.

- [x] **UI31** (P5, low, duplication, effort S). Location at baseline: pxui_shell/layout.ml:3-10. Re-declares the `Panels` types. Action: Alias them instead. Evidence: pxui_shell/layout.ml re-exports panels as type manifests over Editor_core.Panels.

- [x] **UI32** (P0, low, doc-rot, effort S). Location at baseline: navigator.ml:2, text_pane.ml:5 (prototype e4.js) · 'v4', 'V4', 'G13' · 'DepartureMono' comments. Dead prototype references, version tags, and a font that is no longer shipped. Action: Remove them; drop the glyph substitutions if Pragmasevka has those glyphs.

- [x] **UI33** (P5, low, vibe-code, effort S). Location at baseline: workspace_host/images/gpu/resources.ml (36–54% golfed lines; List.length capacity checks). A minified style that differs from the rest of the editor. Action: Reformat them when moving them into flow_native. Evidence: no line over 140 columns in non-test flow_native or rays_editor files.

- [x] **UI34** (P5, low, correctness-risk, effort S). Location at baseline: rays_editor/core.ml:720-725 drops ref. A ref set inside Ui.frame, against the editor's AGENTS rule. Action: Return the choice from workspace_inspector. Evidence: core.ml sets no ref inside Ui.frame.

- [x] **UI35** (P7, low, test-sprawl, effort S). Location at baseline: test/dune:100. Compiles lib/pxui_graph/test_pxui_graph.ml a second time. Action: Move the probe test into pxui_graph. Evidence: the pxui_graph probe test is compiled once.

- [x] **UI36** (P5, low, naming, effort S). Location at baseline: Core, Environment, Leader, Contexts, `core_*` prefix. Names that don't say what the module is. Action: Rename them while doing UI1 and UI14. Evidence: the core_* modules are editor_* (e2ee5828), and Core, Environment, Leader and Contexts are Editor_frame, Editor_loop, Editor_commands and Lowering.

- [x] **UI37** (P7, low, api-bloat, effort S). Location at baseline: editor_document document/layers/objects/literal_edit (1,302 lines). No `.mli` and not private. Action: Add `.mli` files or mark them private. Evidence: document, document_edit, literal_edit, objects and layers have .mli files trimmed to what the repo uses.

- [x] **TT1** (P0, high, process, effort S). Location at baseline: .claude/worktrees/agent-af1aa771a748d6720 (900 MB, locked). A stale agent worktree that doubles grep and skill hits. Action: `git worktree remove --force` and ignore `.claude/worktrees/`.

- [x] **TT2** (P0, high, doc-rot, effort S). Location at baseline: specification/sop-lisp/ (20 files, 10,017 lines). Agent session logs ('session 98738', 'exited 0' ×193); plan 76/76 ticked. Action: Delete it and keep a 10-line summary in sop.md.

- [x] **TT3** (P0, high, doc-rot, effort S). Location at baseline: specification/web-export/results.md (1,752) · plan.md (970, 105/105 ticked). Logs that reference the removed `examples/` 72 times. Action: Delete results.md and shrink plan.md to a design note.

- [x] **TT4** (P0, high, doc-rot, effort M). Location at baseline: specification/api.md:21,56,1140,1164,1182. Teaches OCaml sketches, a roadmap, a nonexistent `App.run`, and an appended changelog. Action: Rewrite it as an internal-layer reference of about 300 lines.

- [x] **TT5** (P0, high, doc-rot, effort S). Location at baseline: specification/project.md:4 · workflow.md:1-17. Describe the removed OCaml-sketch world; half of workflow.md repeats AGENTS. Action: Delete both.

- [x] **TT6** (P0, high, vibe-code, effort S). Location at baseline: README.md:10,25,45,54 · docs/reference.md:3. Review boilerplate: 'source-checked, not qualified on a Mac during this review', 'Linux ARM64 review host'. Action: Delete these lines.

- [x] **TT7** (P0, high, duplication, effort S). Location at baseline: docs/reference.md (202). Copies the README and AGENTS (setup ×3). Its 'Not yet ported' section lists sketches the owner dropped. Action: Delete docs/.

- [x] **TT8** (P1, high, duplication, effort S). Location at baseline: test/test_2d_ports.ml + 8 *_oracle.ml (~950). A second oracle for the deleted OCaml examples; draw_snapshots already pin the same sketches. Action: Delete them and keep the snapshots. Evidence: the eight example oracles are deleted and the draw snapshots kept.

- [x] **TT9** (P0, high, process, effort S). Location at baseline: tools/api_manifest/api_stable.json (340 KB, 184 modules). Freezes internal modules, so every `.mli` edit needs a promote, in a project where the OCaml API is second-class. Action: Delete it (locked), together with the promote step and tools/api_manifest.

- [x] **TT10** (P0, high, test-sprawl, effort S). Location at baseline: test/test_workspace_shell.ml:2521 slow list (14 sections). Run only with RAYS_SLOW=1, which no lane sets, so these UI behaviours are unguarded. Action: Add one qualification rule with `setenv RAYS_SLOW 1`.

- [x] **TT11** (P7, high, tool-cruft, effort M). Location at baseline: tools/dedupe/dedupe.ml (428, compiler-libs). Built to remove helper copies, yet `let fail` is still in 150 files and `check` in 103. Action: Use it once for RK18 and XC26, then delete it. Evidence: tools/dedupe is deleted, along with its references in comments and docs.

- [x] **TT12** (P3, high, build, effort M). Location at baseline: tools/lisp/rays.ml (public_name rays) · tools/lisp/web_export.ml · tools/web/runtime/*.js · generators run from lib dune. The product binary and the export library live in tools/, outside the dependency gate. Action: Move them to `bin/`, `lib/web_export` (with its runtime JS), and the generators next to their libraries; extend the gate.

- [x] **TT13** (P7, med, test-sprawl, effort S). Location at baseline: test/dune:367 and the runtest snapshot rule. 15 hand-written with-stdout/diff pairs named `examples_*` after the `examples/` directory was removed. Action: One loop-generated rule; rename to `sketches_*`. Evidence: the 15 hand-written snapshot pairs are one self-comparing run.

- [x] **TT14** (P5, med, build, effort M). Location at baseline: 24 copy_files lines. Private modules are compiled 2–4 times to get around library privacy. Action: Real library boundaries (UI4); add a gate rule against it.

- [x] **TT15** (P7, med, test-sprawl, effort M). Location at baseline: test/test_main (33 modules, -linkall); lib/rays_editor has no in-library tests. `@lib/rays_editor/runtest`, the loop AGENTS prescribes, covers nothing. Action: Move tests next to their libraries; test/ keeps only cross-cutting checks. Evidence: editor tests live in lib/rays_editor/test.

- [x] **TT16** (P7, med, build, effort S). Location at baseline: test/lisp_sop · tools/workspace_parity · test/ogpu_conformance. Libraries living in the wrong home. Action: Move each next to its owner. Evidence: workspace_parity and lisp_sop live with their owners (lib/rays_editor/test, lib/flow_sop/lisp_sop).

- [x] **TT17** (P0, med, tool-cruft, effort S). Location at baseline: codemod/open_domains.ml, result_bind migrate mode, split_modules.ml, metal-registry legacy modes. One-off migrations whose self-tests run in every runtest. Action: Delete them; keep prune and dead-*.

- [x] **TT18** (P1, med, tool-cruft, effort S). Location at baseline: bench_rewire_payload, bench_merge_payload, bench_uniforms, bench_mesh_planes. Nothing references them, and they are written in golfed style. Action: Delete them (move the mesh_planes validation into flow_sop tests). Evidence: bench_merge_payload and bench_uniforms are deleted; mesh-plane validation is the lib/flow_sop/test_mesh_planes test.

- [x] **TT19** (P7, med, tool-cruft, effort M). Location at baseline: 25 tools/bench_*.ml; 13 per-op rdk microbenches untouched since 2026-10-04; 13 median and 16 measure copies. One executable per bench, each with its own timer. Action: One `bench_rdk.exe <op>` with a shared `bench_util`. Evidence: the topology bench joins bench_rdk with the shared bench_util.

- [x] **TT20** (P0, med, process, effort S). Location at baseline: .claude/skills/{focused-test,smoke,write-rays-lisp,extend-rays-editor,add-sop}. Contradict AGENTS: they bypass check.exe, mention `~after_present`, `sketches/dune.rays.inc`, and OCaml factories. Action: Fix each.

- [x] **TT21** (P1, med, test-sprawl, effort S). Location at baseline: tools/dune:298 (aliases runtest mesh_plane_validation). A 'bench' in default runtest. Action: Make it a flow_sop test. Evidence: bench_mesh_planes became a flow_sop test.

- [x] **TT22** (P0, med, duplication, effort S). Location at baseline: test/dependency_gate.ml:84-145,178. Blacklist rows that the `only` allowlist already covers, and an empty `reach_exceptions`. Action: Delete about 40 lines.

- [x] **TT23** (P0, med, doc-rot, effort S). Location at baseline: AGENTS.md 'lists no exception' vs dependency_gate.ml:170-175 metal_allowed. AGENTS claims no exceptions while two exist. Action: Correct the sentence.

- [x] **TT24** (P0, med, doc-rot, effort S). Location at baseline: DEFERRED.md:37,72,159 (+21 dated lines, a dead README pointer). 'A finished item is deleted', yet it keeps 'Fixed and verified' items. Action: Delete them; aim for under 60 lines.

- [x] **TT25** (P0, med, doc-rot, effort S). Location at baseline: specification/test-times.md inventory. References gone files and a removed environment variable. Action: Keep the log rows and delete the inventory.

- [x] **TT26** (P0, med, doc-rot, effort M). Location at baseline: specification/workspace/prototype/ (876 KB) · case-studies.md · ambiguities.md · iteration.md (1,868). Marked 'Status: implemented', and the checker is already ported. Action: Archive them; fold the normative parts into flow.md.

- [x] **TT27** (P0, med, test-sprawl, effort S). Location at baseline: test/dune:193 test_world_command · :303 test_workspace_vocabulary. Orphan aliases that never run. Action: Wire them into runtest or delete them.

- [x] **TT28** (P0, low, build, effort S). Location at baseline: dune-project:3,24. An unused `directory-targets` extension; `lang 3.17` against a `>= 3.21` requirement. Action: Delete the first; set lang to 3.21.

- [x] **TT29** (P7, low, build, effort S). Location at baseline: test/dune:271-286. Packaging checks (`check_sdl3_conf` ×4) live in test/. Action: Move them to tools/packaging/dune.

- [x] **TT30** (P7, low, tool-cruft, effort M). Location at baseline: tools/packaging + tools/sdl3 (~3.2k) · test_sdl3_lock.ml:106-114. Infrastructure sized for a multi-platform app on a brew-installed, single-platform one, plus a guard for a finished migration. Action: Drop the guard; keep generate and the lock. Evidence: bench_util is shared; only the SDL lock and generate remain in tools/packaging, and the test_sdl3_lock guard is gone.

- [x] **TT31** (P7, low, test-sprawl, effort S). Location at baseline: test/dune (440 lines, ~30 one-line alias rules). Hand-written aliases for test_main sections. Action: Generate them, or pass a section list to one rule. Evidence: test/dune is 38 lines with no one-line alias rules.

- [x] **TT32** (P7, low, duplication, effort S). Location at baseline: lib/flow_ir/core_fixtures.ml (182) vs tools/core_fixtures.ml (1,519). Two different programs with the same name. Action: Rename the tool `prims_fixtures`.

- [x] **TT33** (P0, low, build, effort S). Location at baseline: port_tests/dune:16 `*.pending` glob · tools/lisp/test/dune redundant js deps. Globs and dependencies that match nothing or are already covered. Action: Delete them.

- [x] **TT34** (P7, low, process, effort S). Location at baseline: 19 of 45 sketches without a header; ws_shared*.rays, flow_terrain, cube_cage are fixtures. Sketches that are really test fixtures, against the AGENTS header rule. Action: Move the fixtures to workspace/cases and add the headers. Evidence: fixtures moved to specification/workspace/cases.

- [x] **TT35** (P0, low, doc-rot, effort S). Location at baseline: tools/dune:184-270 (P6.x), test/dune:364 (P3.1), metal.ml:1408 (G5), backend.ml:23 (G6), test_parity.ml:1 (D15). Plan ids in build files and comments. Action: Remove them; the guardrail keeps them out.

- [x] **TT36** (P0, low, process, effort S). Location at baseline: .gitignore:34-35 · _out/ (53 MB). Ignore lines for dropped sketches, and stale renders. Action: Clean them up.

- [x] **TT37** (P0, low, doc-rot, effort S). Location at baseline: specification/pxui-kit (3.3 MB, @1x + @2x PNG + HTML). Two resolutions of every render. Action: Keep one.

- [x] **TT38** (P7, low, doc-rot, effort S). Location at baseline: AGENTS.md nested-file list and Libraries table. Missing editor_document/AGENTS.md, workspace/cases/AGENTS.md and about 10 helper libraries. Action: Fix the list; the moves in P7 remove the need for extra rows. Evidence: every sketch has a header comment saying what it shows.
