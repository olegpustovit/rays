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
