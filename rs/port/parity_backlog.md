# Parity backlog (collected from agent reports; fix in one pass when flow_ir/flow_sop land)
- flow_ir: OCaml evaluates `if` branches right-to-left, so text constants are numbered in the other order (flow_web oracle case `js_region carried record`, both precisions).
- flow_ir core_lower refuses the `error_rows` mapper with "refused binding" where OCaml lowers it (flow_web ignored test range_last_diagnosis_is_refused_with_its_span).
- flow_check: `error_rows` rejects `rows mapper :policy text`; OCaml accepts (workspace.ml:1456).
- flow_ir test_inner_sum: bounds_errors_and_guards — packed path message "array/slice needs a non-negative whole length." vs walker "An array slice must be within its source." (both E_ARRAY_RANGE).
- flow_ir test_inner_sum: static_map_profile_and_late_slice_diagnostic — recorded profile differs.
- OCaml bug (both sides, not fixed, user asked): lib/rdk/ops/mesh/ray.ml combine_samples doesn't reset combined_selected/distances when a query has 0 hits.
- OCaml working tree: lib/ogpu/conformance/conformance.ml rt_extra needs `triangle_data` on Metal intersectors.
- Unported web tests needing the node JS runtime: flow_web test_gather/scatter/grade_index/compact_scan/array_max_length, node half of test_js_frame.
- Dependency gate (cargo run -p gate -- deps), 29 lines after rules update 41ac7366: direct edges OCaml lacks — rays->ogpu/ogpu_core/sdl3, renderer->ogpu_core/sdl3, flow_sop->rdk/rdk_core, pxui->scene_command, pxui_graph/pxui_shell->param, ogpu_mock/ogpu_vulkan/ogpu_metal->ogpu (Impl record: likely legitimate, decide), ogpu_metal->native_layer_token; rays & renderer test targets select two implementations (need per-target selection); rdk_rays needs ogpu_mock dev-dep.
- param::number::scrub rounding order (fix agent running).
- attribute_kernel: borrowed_columns copies a geometry plane per cache miss (OCaml zero-copy) — perf pass.
## From flow_ir test port irtest2 (26 failing tests; verify each claim against OCaml before changing)
1. flow_eval eval.rs:1341 raises E_ITER_BOUND for iterate counts outside 0..=4096; OCaml walker reportedly has no cap, negative = no-op. (core_lists_parity_part_0/1/2, iterate_over_a_packed_array, carried_loops part_2, carried_loops_uint32_and_iterate_fixtures, loop_fusion_* )
2. flow_eval eval.rs:1220 "A map exceeds 4,096 elements." for 5001-element plain list; OCaml allows. (known_5001_geometry_map_filter_captures)
3. flow_check op.rs:671 normalize computes x/len; OCaml x*(1/len). (scalar_value_operators part_1)
4. core_lower: state-step map over previous array -> Ok with regions=0 (core_state_kernels_previous, workspace_particles_snapshot).
5. flow_check workspace.rs:2407 error_rows only accepts `rows :policy p mapper`; OCaml both orders. Checker also accepts ill-typed error_rows/with_error forms (mapper returning non-record; state inside with_error). (all test_error_rows; with_error case)
6. strict core_lower refuses with_error residuals ("binding"/"fn") — 9 test_with_error tests.
- flow_ir test_with_error/test_error_rows: flow_graph checked-edit blocks compiled out (#[cfg(any())]) — add flow_graph dev-dep and enable.
- ts::fixtures compile left-to-right; OCaml list literal right-to-left (test_program snapshot) — check test_support.
## From flow_sop operators/kernels (fsopB)
- HIGH: flow_sop parity+integration tests run concurrently in ONE process hang > 60 s; one at a time they pass. Suspect nested-parallel deadlock in `parallel` pool or a shared lock (flow_eval arena?). Reproduce with plain `cargo test -p flow_sop` (threaded) and get a backtrace.
- PERF: rand_at_agrees took 284 s at 100k elements (opt-level 1); cut to 3000/4097. Profile vs OCaml — likely a perf bug.
- flow_check refuses untyped fn param passed to image/map ("expects fn(vec2)->vec4; got fn"); OCaml accepts (ignored test in image_kernel).
- packed normalize differs from reference at time -0 for vector cases (behind RAYS_NORMALIZE_PARITY=1) — likely same as item 3 above.
## From flow_ir test port irtest1 (ignored tests, grep `#[ignore` in rs/crates/flow_ir/src/test_*.rs)
- HANG (intermittent ~5/8, even 1 domain): resolver forcing a state fold inside packed::force (test_uniforms rollback_*, test_gather state_rolls_back_on_gather_errors, late_bounds_versus_earlier_table_cause).
- ORDER-DEPENDENT: Host::force_reference inside 4/8-domain scope -> E_UNBOUND $kernel-input:0 (text_select_reference_inside_domains); text_helper_lookup fails ~half the runs (text_helper_then_select_same_thread repro) -> thread-local scope/arena state leaks between runs.
- captured_helpers: E_FUNCTION_ID — helper fn belongs to another evaluation's arena; get_fn is per arena.
- array_init(limit+1) aborts the process allocating 1.4e17 bytes instead of E_ARRAY_RANGE.
- array/max_length non-text :type -> E_TYPE, OCaml E_ARRAY_TYPE.
- array/max of [-0. 0.] and Vec2 returns -0., OCaml +0. (f64::max sign in reduce_array, flow_check op.rs).
- slice start max_int: packed vs walker E_ARRAY_RANGE message differ.
- static-map profile head_elements 4096 vs OCaml 0 (kernel does not run first): static_map_profile, kernel_first_profile_clock, kernel_first_head_is_zero.
- packed::force / host::prepare don't roll back state after a reduction error (rollback_restores_state; stamp 8 vs 0).
- Host::force_reference evaluates index arg before resolving the table (E_RANGE vs OCaml E_DATA_SOURCE).
- text_error_cause_raw_bytes: Diagnostic.cause is String (raw bytes lost).
- flow_ir lacks flow_graph dev-dep: replay probes (test_static_map), checked-edit blocks (#[cfg(any())] in test_with_error/test_error_rows).
## From flow_sop lower port (fsopD)
- LEAK: lowering closures hold Arc<Lowerer>; lowering and its nodes form a reference cycle (leaks every lowering; the editor re-lowers per edit). Use Weak or restructure.
- operators::parity::rand_at_primitive_agrees_with_native fails.
- 8 flow_sop tests hang in flow_ir::qualify_workspace (static-map deadlock) — flowrt.
- library nodes transform/mountain/normals/curve fail check: "sop/group takes 1 positional inputs; got 3" — likely flow_syntax attribute_args positional bug (sent to flowck).
- test_settle needs device write placement (settle/release/bounds) in attribute_kernel.
- $card marker: Rust flow_eval emits a marker with an attached error where OCaml evaluator makes a catalog call; lower.rs rewrites it (failed_card_as_call) — fix flow_eval to match OCaml and drop the rewrite.
- Unported flow_sop tests: most of test_lisp_node, test_lisp_fusion, test_graph, test_node_sources, test_workspace_cook/edit/zone, test_projection, test_lisp_projection, test_mesh_resident, test_gpu_bulk, test_node_facts, test_metadata_kernel, kernel half of test_trace, test_group_kernel, test_remap.
- flow_sop depends directly on rdk/rdk_core (OCaml reaches rdk only via sop) — gate.
## Remaining after flowrt (2026-11-04 ~14:40)
- flow_check op.rs:467 array/slice limit isize::MAX vs OCaml Sys.max_floatarray_length (pass None to array_count) -> slice_start_max_int.
- flow_syntax value.rs array_init doesn't reject limit+1 before allocating (abort vs E_ARRAY_RANGE).
- flow_check array/max_length non-text :type E_TYPE vs E_ARRAY_TYPE; array/max of [-0. 0.] keeps -0.
- flow_ir debug-build tests over 5 s (iterate/map parity, several) and flow_web test_js 11 tests over 5 s — buildspeed agent deciding opt-level; else shrink.
- frame_kernel_prepared_programs_match_the_reference_a/_b fail with flowck's working-tree core_lower/core_region edits.
- Per-callback arena copy-on-write cost not measured.
## From flow_gpu
- Kept GPU outputs (materialise, Device::add reads) only work on the creating thread (Rc mapped views); OCaml reads from 4 domains. Fix: owner-thread prefetch at settle, or Arc-backed mapped views in OGPU. Matters for flow_sop.
- materialise is infallible (panics on bad/freed uint32 holder) where OCaml raised.
- gpu_run::carried skips iterate-count/iterate-until/iterate-int63 for the (now removed) E_ITER_BOUND cap — un-ignore.
- Mac: harmless thread-local destructor panic message at process exit in flow_gpu tests.
## From editor_document un-ignore pass (edocfix)
- PERF (root of the editor_document slow/hanging tests; in flow_eval, not editor_document): `editor_document::lowering::of_workspace` -> `flow_sop::lower::of_checked` -> `flow_eval::eval::Host::static_with_requests` (eval.rs ~2380, `static_with`) costs ~200 ms for `(workspace w (graph g :context sop (sop/box)))` and 0.63 s for `(sop/transform (sop/box) :translate [1 0 0])` (debug, flow_eval/flow_sop at opt-level 1), and 20-36 s for `(workspace w (graph g :context sop (sop/uv_sphere :radius 0.5)))`. OCaml lowers all three in milliseconds. NOT a deadlock (the static-map deadlock fix 6c7165c3 holds: the three `fast_path_*` tests run, they just never finish within 60 s). gdb samples: one thread, innermost frames are recursive `Vec`/`Box` clone (`to_vec` -> `clone` -> `clone` ...) and `drop_glue` of syntax/Term trees, i.e. deep clones of `Value`/env/arena data per evaluation step, plus `flow_sop::context_kernel::term_once` closures. Repro (test body in editor_document test_scene_sync): `ws_of_text` + `lower(None, &w)` of the three texts above, timed with `Instant`. Expected: all well under 100 ms. Suspect: `static_with` clones the whole `Workspace`/arena (or term tree) per node or per `get_fn`; profile with a sampling run of `sop/uv_sphere` first.
- PERF: `ws_of_text` (Workspace_doc.of_text, check) is 0.18-1.2 s per call in debug for those same one-node texts (flow_check); the 15 scene_sync tests that do 10-30 lowerings each therefore take 5-40 s and are `#[ignore = "slow ..."]` (they pass: ran the whole file under RAYS_SLOW=1 earlier, 20 of 21 ok; `root_of_saved_sketches` > 60 s).
- 3 test_lowering tests (fast_path_literal_scrub_with_closure, fast_path_inline_scrubs_gpu_sea, fast_path_inline_scrubs_orrery) and test_workspace_doc part_view stay ignored for the uv_sphere cost above; part_contexts and part_literals (ported, ignored as slow) were not yet run to completion for the same reason.
- Unported from OCaml test_workspace_doc (need Rays_editor.Editor / Pxui_graph::Scope / Test_support.Workspace_cook): part_editor, part_conditional_history, part_live, part_preset (Editor.create + keys), part_pane_layout, part_editor_contexts, part_projection_reuse (check), the editor half of part_literals, the Pane-update and cooked-bytes checks in part_literals, and the whole of part_files (uses Rays_editor.Source.read_imports / Workspace.load / workspace_catalog). test_scene_tree.ml (657 lines) is entirely Editor-driven: rays_editor port.
- Un-ported scene_sync: qualification_roots, the through-the-editor block (see test_scene_sync.rs header).
## From flow_sop un-ignore pass (fsopF)
- FIXED frame-fold bug: flow_eval Residual::eval_term (called by flow_ir core_region::function_body with input::at_time(0.0)) overwrote the shared arena's bound live frame; the later cook of a stateful node saw frame 0, begin_frame regressed and cleared the fold state. eval_term now restores a previously bound frame. (group_frame_folds_recook_exact_masks passes.)
- FIXED deadlocks (std Mutex re-entered on one thread): inline_node's LIVE_KERNELS hook (frame_dependent/compile on the locked arena) now runs under call_published; nested Host::rewrite_with from a rewrite callback (lower.rs fold_writes) falls back to rewrite_plain via a thread-local (inner copy shares the earlier value's ids).
- FIXED graph_input_fits: a closure from another arena (library defnode) was compared as plain `fn`; it now looks the closure up in its home arena. Remaining: a_map_function_slot... then fails E_RESIDUAL_ID (home arena expired before cook).
- Remaining ignored in flow_sop (reasons on the attributes): lowering that is merely slow (perf agent): cooked_folds_traverse_inventory, fusion x3, inline x3, declaration_cones, node_facts topology, remap strict_public_defnode, zone x5. Not slow: numeric_body_without_packed_kernel (code lost via map_err(|d| d.message)+edit_r), call_site_and_helper_edits (cache misses 4 hits 3), optional_and_rest_callable (E_TYPE Functions cannot be compared), typed/optional image slots (disconnected_input), a_live_curve_is_pending_and_volatile.
- Remaining ~2-4 s alone (flake under the runner's parallelism, ok alone): native_owner_masks_defnode_round_trip, iterate x2, chain/a_map_reading fusion, inline chained_bodies/cooked_data, workspace_cook::skips.
## TOP: flow_eval arena locking redesign (after lowerperf finishes)
Three re-entrant deadlocks so far (registry_find lock, live-kernels hook under lock, nested Host::rewrite_with in fold_writes), each fixed by a local workaround (published COW copy, call_published, thread-local rewrite_plain sharing ids — the last is NOT exact OCaml semantics). OCaml has no locks; callbacks re-enter freely. Redesign so re-entrance is structurally impossible to deadlock (e.g. per-thread arenas with RefCell borrowed only for short non-callback sections, never held across host callbacks), then remove the workarounds and restore exact id semantics in fold_writes.
## flow_sop remaining (8): a_map_function_slot_keeps_its_separate_outer_geometry_capture (E_RESIDUAL_ID expired: library arena dropped before cook); a_numeric_body_without_a_packed_kernel_is_refused_not_interpreted (lower.rs map_err(d.message) loses E_KERNEL_FORM code/span); call_site_and_helper_edits_invalidate_only_what_depends_on_them (cache misses 4 hits 3); optional_and_rest_callable_slots_keep_bytes_and_cache (E_TYPE functions cannot be compared); typed_image_slots_keep_their_cooked_pixels + optional_and_rest_image_slots_keep_bytes (disconnected_input); a_live_curve_is_pending_and_volatile (unknown).
## From rays_pathtracer
- ogpu_vulkan (OCaml AND Rust): compaction drops the geometry record, so a motion instance whose prototype was compacted fails ("motion instance prototype is not a triangle BLAS"). Tracer works around it (skips compaction for motion prototypes, no reuse). Fix in the backend, then remove the workaround.
- Lisp kernel check+compile ~2 s per distinct scene shape in debug (cold runs 7-17 s) — same flow_check/flow_eval slowness as lowerperf.
- furnace_mis: with >1 spp per frame MIS-vs-BSDF variance check barely holds (1.06 vs 1.14 at 8 spp; OCaml 1 spp gives 0.95 vs 3.25) — cause not found.
- Metal: destroy doesn't release buffers until the last Rust clone drops; release_film byte check only on Vulkan; 2 window handles tolerated on Metal.
- gpu_parity runs only under RAYS_SLOW=1 (256 spp instead of 1024); exact_m1 digests, bench_scene, bench_world not ported.
## From flow_native
- flow_sop attribute_kernel::resident_vec3/resident_color return a copied Vec, so flow_gpu::run::pin can't find the placeholder: workspace_gpu::display never succeeds (host copy drawn instead). Return the placeholder's own Arc.
- flow_native frame-alloc budget measures live-byte growth (mem_sys tracks live bytes), not cumulative allocation like OCaml.
- RECHECK after lowerperf2: flow_gpu run.rs materialise panicked (settle diagnostic / read after free) in a run during the Value::List refactor.
