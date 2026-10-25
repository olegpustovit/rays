# Test times

The log behind "Test time is a budget" in `AGENTS.md`: one row each time a slow test is found, reworked, gated
or deleted. Newest first. Times are wall seconds on the author's machine, warm cache, dune dev profile.

| Date | Test | Before | After | What changed |
|---|---|---|---|---|
| 2026-10-25 | `run_sop`, `run_diagnostics` (qualification-scale), `test_lisp_ports`, `test_workspace_live`, `test_workspace_doc`, `test_workspace_ir`, `test_bloom_studio`, `run_nodes` | 44.6 s, 48.4 s, 151 s, over 300 s (1222 s in the cold trace), over 300 s (1408 s), over 590 s, 72.5 s, over 300 s (41 CPU-min when killed) | 14.9 s, 8.6 s, 34 s, 32-59 s, 84 s, 146 s, 42 s, qualification-scale | `run_diagnostics`: the 160x80 sphere (12 642 points) is 48x24 (1 106 points), same counts rule. `run_sop`: `test_local_subdivide_contract` was 32.8 s of the 44.6; `Lisp_sop.node` of `sop/subdivide` costs 1.1-1.4 s per call (the 1 357-line body is lowered each time) and the test made 30; now 5 (group + bilinear, consistent Stitch Divide, second-input creases, detail overrides, independent curves with normals). `test_lisp_ports`: the rule-table nodes (delete/rename attributes, group delete/rename, swap) ran 74-300 rows on 5-7 geometries, each with 2 lowerings (about 10 ms) and 9 domain/grain cooks, 16 700 lowerings in all; rows on the plain geometries are every 8th, the 64x64 grid and the 32 769-point cloud run only for nodes with no named items, domain/grain are 3 pairs not 9, the reference-input variants run every 4th row. `test_workspace_live`: orrery playback 8 frames not 600 (74 s of CPU at 60), `wave` leaves the live-vs-static loop. `test_workspace_doc`: `part_files` edits 2 literals per file instead of all (each costs 3 lowerings) and skips the seven files that cost 6-30 s (`flow_particles_gpu`, `ws_sunflower`, `ws_tiles`, `sop_gallery`, `flow_field`, `ws_bloom`, `ws_garland`) unless `RAYS_SLOW=1`. `test_workspace_ir`: the default sweep is the examples, the kit, the 13 fixtures and four small sketches (`RAYS_SLOW=1`: every sketch). `test_bloom_studio`: 3 and 2 petals. `run_nodes` (975 `Lisp_sop.node` calls, about 1 s each): alias `test_nodes` moved from `runtest` to `qualification-scale`; Lisp-against-native parity of each node is also the `ports` alias |
| 2026-10-25 | inventory of the whole default loop (table below) | not measured | cold `@runtest` with `-j 3`, cache off, machine load 11-17: 3 581 s, exit 1 | "trace" times are inflated 2-3x by the load; "solo" is the executable alone at load 3-5. Exit 1 is `test_materials` (exits 2) and `dependency_gate` (exits 2), both red before this change |
| 2026-10-25 | `RAYS_SLOW=1` inventory, `RAYS_SLOW=1 test_workspace_cook`, `test_geometry_collections`, `test_attribute_kernel` (field SOP), `test_gpu_bulk`, `test_array_composition_web` | cook about 270 s then 35 s; geometry_collections 75 s; attribute_kernel 50 s (one block 28.9 s); gpu_bulk 32 s; array_composition_web 50 s (region_arguments 13, rand_at 18, packed 11, ir array_composition 15 unchanged) | cook 27 s; geometry_collections 12 s; attribute_kernel 21 s; gpu_bulk 2 s; array_composition_web 23 s | cook: the facade, garland, orrery, rosette, sunflower and tiles fixtures leave the slow loop (the same 1-vs-3-domain, determinism and checked/source properties stay on kit, tree, tunnel, variations, wave), lowerings are cached per name and the determinism check compares the source lowering with the checked lowering already made (no third lowering), bloom runs with 3 and 2 petals, the sunflower source-attribute check is a 6-sphere inline merge, stable ids are checked on kit. geometry_collections: the `map` case is a subset of `filter-map`, domains 1/8, grains 1/4096, times 0/1. attribute_kernel: iso-surface field at 24^3 instead of 64^3. gpu_bulk: 100x100 grid instead of 1000x1000 (`GRID_SIDE` still raises it). array_composition_web: fewer capture counts and frames |
| 2026-10-25 | `test_workspace_shell` (all sections, `RAYS_SUITE_BUDGET=900`) | 166 s (`run_panels` 52.1, `run_ops` 23.1, `run_studio` 13.8, `run_panel_keys` 13.8, `run_unbound_panels` 10.9, the other 38 sections 0-7.7) | 110 s, no section over 14 s (`run_panel_keys` 13.9, `run_studio` 13.8, `run_ops` 13.4) | `run_panels` called `panels ()` (a full dump) 65 536 times in `any_id`; now once. `run_ops` and `run_restore` use `cheap_variations` (one viewport, boxes instead of spheres): their checks are layout edits, not lowering. Sections print their time; `RAYS_SECTION=run_x` runs one |
| 2026-10-25 | qualification-scale `run_sop`, `run_diagnostics` | not timed | 50 s, 42 s | measured only; not part of any default or `RAYS_SLOW` run |
| 2026-10-25 | `Lower.workspace` (cause of the rows below) | `ws_layout` 0.52 s and 152 M words, `ws_rosette` 2.77 s and 822 M words per lowering (temporary bench, 10 and 3 runs) | 0.36 s / 102 M words and 1.70 s / 577 M words | SIGPROF call-stack sampling showed 31% in `Flow.Lisp.print`: every Lisp-node build printed (pretty-printer) the whole authored source and the reachable library to make a cache key. `Context_kernel.forms_digest` serialises compactly instead, and the digests are memoised in a 64-entry cache keyed elementwise on physical forms (a nested lowering rebuilds the lists, so the old one-entry cache missed). Not the polymorphic compares or the `Memo` hash (a better hash was slower). Left: each Lisp-node build lowers the node body again (`qualify_workspace` 40%, `Context_kernel.map` 23%); a cache of that per (definition, arguments) is the next step |
| 2026-10-25 | `@lib/flow_sop/test_workspace_cook` | over 180 s (about 270 s: blend_shapes 7, attribute_composite 10, optional_rest 9, switch 2.5, skips 1.8, then per fixture bloom 35, sunflower 35, garland 18, facade 14, rosette 12, orrery 10, kit 4.6, ...) | 5.5 s | Cost is lowering (`Lower.workspace`/`of_checked`, 0.5-1 s per sphere-like node), done 3 times per fixture plus 3 cooks, not the cook or the 1/3-domain comparison. Default keeps switch, skips and the kit fixture (checked/source lowering equality, 1 vs 3 domains, determinism); `RAYS_SLOW=1` adds the three per-node parity sections, the other eleven fixtures and the bloom/sunflower/wave/ids checks (the rule depends on `RAYS_SLOW`). Product lead as in the `test_text_pane` rows: faster lowering |
| 2026-10-25 | `test_text_pane` (second pass), `editor_text_drop` | about 41 s in the test binary (editor_w9 24.6, editor_text_drop 6.3, editor_text 4.9), alias about 40 s | about 27 s alias (`tools/check.exe`), 10 of 10 consecutive passes | `editor_w9` created the editor and ran frames 1..21 once per scenario; now once, each of its 7 scenarios continues from that value, and the "untouched" scenario is a read of it (no cook). The two macro-dialog scenarios cannot share a UI handle (the dialog and the inspector scroll live in it), so the Enter one has its own prefix and the note one (wheel) runs last. Profile: only the edit frames cook (1.4-2.4 s each in `editor_w9`, 0.6 s each in a 3-node workspace, so the cost is per edit and mostly not the fixture); a `sample` of the binary shows `Flow_sop.Context_kernel.term` (the `Memo` hashed by form id, `==` equality) as the top self-time frame, about a third of the main-thread samples: a product lead, not touched. `editor_text_drop` flaked because a carry shows its preview only if applying and the last cook took under `carry_budget` (0.5 s of the real clock); the test now passes `~carry_budget:Float.infinity` |
| 2026-10-25 | `test_text_pane` | about 240 s (editor_text 124, editor_w9 70, editor_binding 40) | about 37 s in the test binary | The sunflower and rosette fixtures cooked 240 seeds / 12 petals on every edit: each edit re-lowers the workspace and the cost follows the number of `sop/uv_sphere` calls (about 0.07 s a seed, 16 s a cook at 240), not the text pane. The tests now use `small_sunflower` (8 seeds) and `small_rosette` (2 petals, 1 inner), same checks. What is left is a fixed 1.8 s a cook in `editor_w9` (7 scenarios, 22 s); under 30 s needs a faster workspace lowering (`Flow_ir.walk`, `Context_kernel.map`), not a smaller fixture |
| 2026-10-25 | G3 additions (`@lib/pxui_graph/runtest`, `@lib/flow_graph/runtest`) | 8 s | 8 s | about 10 new checks, none over 1 s |
| 2026-10-25 | `test_workspace_shell` (whole) | about 360 s with every section | 45 s default, rest skipped and counted | Sections run under `RAYS_SUITE_BUDGET`; `RAYS_SLOW=1` or `=900` runs all. Each section still takes 25-30 s: root cause not found, next thing to profile |
| 2026-10-25 | 9 tests in `flow_sop`, `flow_web`, `flow_ir` | minutes together | 0 in the default loop | `enabled_if RAYS_SLOW=1` |
| 2026-10-25 | `run_sop`, `run_diagnostics` (`lib/sop`) | minutes | 0 in the default loop | moved to `qualification-scale` |
| 2026-10-25 | whole `@runtest` | about 20 min | not run for an edit | focused aliases through `tools/check.exe`, 180 s hard timeout |
| 2026-10-25 | graph drag in `ui_shot` (recursive_rectangles) | 27 s for 30 frames | 3.3 s | probes are no longer forced on every frame (`core_scope.ml`) |
| 2026-10-25 | `@lib/flow_sop/test_workspace_cook` (first note) | over 180 s (stopped by `check.exe`) | fixed, see the row above | found while checking the Wrap change; not caused by it. `@test/test_text_pane` is 25 s: `Context_kernel.map` memo is not the cost (1000+ big-graph calls, chains up to 160 from identical unrolled subtrees, about 6% of main-thread samples); polymorphic `List.mem`/`List.assoc_opt` compares are about 30% |

Open: the default loop still has eight aliases over 30 s (listed at the end of the inventory below); the cause is
the product lowering, not the fixtures. `core_census.t` fails on crosses and hatch since before these changes.

## Inventory, 2026-10-25

Every test executable behind `dune build @runtest` (the default loop), `RAYS_SLOW=1` and `@qualification-scale`,
measured on this tree. "Trace wall" is the cold `@runtest` (`DUNE_CACHE=disabled`, `-j 3`, machine load 11-17,
per-action time from `--trace-file` + `dune trace cat`), so it overstates by 2-3x; "solo before" and "solo after"
are the executable run alone (load 3-5) from `_build/default/<dir>` before and after this change. A dash means the
trace time is already under budget and the executable was not run again. Budgets: focused alias 30 s, one section
10 s. A `test_main` row is a section alias (`@test/<section>`, or the library's own `test_main`). Slowest first.
Only rows of 1 s and over are listed; the 116 below 1 s are named after the table.

### Default loop (`@runtest`)

| Alias / executable | Loop | Trace wall (s) | Solo before (s) | Solo after (s) | Under budget |
|---|---|---|---|---|---|
| `run_nodes` | default | 2246.9 | >300 (41 CPU-min when killed) | moved to qualification-scale | NO |
| `test_workspace_ir ..` | default | 2175.7 | >590 | 146 (examples, kit, fixtures, 4 small sketches; every sketch under RAYS_SLOW=1) | NO |
| `test_workspace_doc` | default | 1408.1 | >300 (1408 in the trace) | 84 (heavy literal sweep under RAYS_SLOW=1) | NO |
| `test_workspace_live` | default | 1221.6 | >300 (1222 in the trace) | 32-59 (8 orrery frames, 2 fixtures) | NO |
| `test_lisp_ports` | default | 469.1 | 151.5 | 34 (35 at load 5) | NO |
| `test_bloom_studio` | default | 211 | 72.5 | 42 (3 and 2 petals) | NO |
| `test_sop_catalog` | default | 102.5 | 44.0 | 44.0 (not changed) | NO |
| `test_scene_sync` | default | 99.7 | 40.9 | 40.9 (not changed) | NO |
| `test_workspace_shell` | default | 65.5 | 51.2 (sections past RAYS_SUITE_BUDGET skipped) | 51.2 (not changed) | NO |
| `test_set_color` | default | 34.1 | 10.4 | 10.4 | yes |
| `test_viewport_pick` | default | 29.6 | 7.4 | 7.4 | yes |
| `test_workspace_zone` | default | 28.3 | 12.1 | 12.1 | yes |
| `test_text_pane` | default | 27 | 11.6 | 11.6 | yes |
| `test_grade_index` | default | 24.8 | 2.2 (flow_ir), 8.4 (flow_web) | 8.4 | yes |
| `test_probe` | default | 20.5 | 5.2 | 5.2 | yes |
| `test_lisp_node` | default | 18.1 | 6.1 | 6.1 | yes |
| `test_ir` | default | 16.5 | 5.3 | 5.3 | yes |
| `test_materials` | default | 16 | 13.8 (exits 2, fails before and after) | 13.8 | yes |
| `test_uniforms` | default | 15.5 | 7.0 | 7.0 | yes |
| `test_metadata_ports --edge` | default | 13.7 | - | - | yes |
| `mesh` | default | 12.5 | - | - | yes |
| `test_gather` | default | 11.8 | - | - | yes |
| `test_remap` | default | 11.4 | - | - | yes |
| `test_points_port` | default | 11.3 | - | - | yes |
| `test_image_sop` | default | 9.9 | - | - | yes |
| `attrib` | default | 9.8 | - | - | yes |
| `test_workspace_source` | default | 9.7 | - | - | yes |
| `test_keyword_ports` | default | 9.4 | - | - | yes |
| `test_iterate` | default | 8.7 | - | - | yes |
| `test_setters` | default | 8.6 | - | - | yes |
| `test_rays_editor_logic` | default | 8.4 | - | - | yes |
| `test_geometry_arrays` | default | 8.2 | - | - | yes |
| `test_math_kernel` | default | 8.1 | - | - | yes |
| `test_pxui_graph` | default | 7.5 | - | - | yes |
| `test_editor_consistency` | default | 7.5 | - | - | yes |
| `test_js` | default | 7.1 | - | - | yes |
| `test_gather` | default | 6.9 | - | - | yes |
| `boolean` | default | 6.8 | - | - | yes |
| `core` | default | 6.4 | - | - | yes |
| `test_group_kernel` | default | 6.3 | - | - | yes |
| `test_line_port` | default | 6.1 | - | - | yes |
| `spatial` | default | 5.9 | - | - | yes |
| `test_2d_ports` | default | 5.6 | - | - | yes |
| `test_workspace_cook` | default | 5.2 | - | - | yes |
| `test_facts` | default | 5.2 | - | - | yes |
| `test_core_lists` | default | 5.1 | - | - | yes |
| `test_concat` | default | 5 | - | - | yes |
| `test_core_regions` | default | 4.9 | - | - | yes |
| `test_gallery_shapes` | default | 4.8 | - | - | yes |
| `gen` | default | 4.8 | - | - | yes |
| `test_ui_parity` | default | 4.6 | - | - | yes |
| `test_js_frame` | default | 4.6 | - | - | yes |
| `curve` | default | 4.3 | - | - | yes |
| `test_grade_index` | default | 4.2 | 2.2 (flow_ir), 8.4 (flow_web) | 8.4 | yes |
| `main --check-workspace` | default | 4.2 | - | - | yes |
| `test_planes` | default | 3.8 | - | - | yes |
| `test_scene_tree` | default | 3.7 | - | - | yes |
| `test_lisp_fusion` | default | 3.6 | - | - | yes |
| `test_error_rows` | default | 3.6 | - | - | yes |
| `test_encoded_parameters` | default | 3.6 | - | - | yes |
| `test_attr_from_image` | default | 3.4 | - | - | yes |
| `test_workspace_images` | default | 3.3 | - | - | yes |
| `test_static_map` | default | 3.1 | - | - | yes |
| `test_compact_scan` | default | 2.9 | - | - | yes |
| `test_metal` | default | 2.8 | - | - | yes |
| `test_sdl3_image ../../test/sdl3_image_fixtures` | default | 2.7 | - | - | yes |
| `test_frame_kernel` | default | 2.7 | - | - | yes |
| `test_drawing ../examples/particles/sketch.rays` | default | 2.7 | - | - | yes |
| `test_parallel_branches` | default | 2.6 | - | - | yes |
| `run_edit_graph` | default | 2.5 | - | - | yes |
| `test_scatter` | default | 2.3 | - | - | yes |
| `test_workspace_eval` | default | 2.2 | - | - | yes |
| `test_fma` | default | 2.2 | - | - | yes |
| `test_sop_ui` | default | 2.1 | - | - | yes |
| `test_topology_kernel` | default | 2 | - | - | yes |
| `test_sdl3_ttf` | default | 2 | - | - | yes |
| `test_payload` | default | 2 | - | - | yes |
| `test_editor_input` | default | 2 | - | - | yes |
| `exact` | default | 2 | - | - | yes |
| `test_array` | default | 1.9 | - | - | yes |
| `test_noise_kernel` | default | 1.8 | - | - | yes |
| `test_editor_commands` | default | 1.8 | - | - | yes |
| `test_merge_source` | default | 1.7 | - | - | yes |
| `test_workspace_node` | default | 1.6 | - | - | yes |
| `test_projection` | default | 1.6 | - | - | yes |
| `test_open_domain` | default | 1.6 | - | - | yes |
| `test_lisp_projection` | default | 1.6 | - | - | yes |
| `test_metadata_kernel` | default | 1.5 | - | - | yes |
| `test_marks_scene` | default | 1.5 | - | - | yes |
| `test_host_effects` | default | 1.5 | - | - | yes |
| `codemod result-bind,--self-test,../../ppx/result_bind/result_bind_ppx.ex` | default | 1.5 | - | - | yes |
| `test_core_parity` | default | 1.4 | - | - | yes |
| `test_compact_scan` | default | 1.4 | - | - | yes |
| `test_rays` | default | 1.3 | - | - | yes |
| `test_node_sources` | default | 1.2 | - | - | yes |
| `test_flow_shader ../sketches/flow_shader/sketch.rays` | default | 1.2 | - | - | yes |
| `test_packed_pieces` | default | 1.1 | - | - | yes |
| `dependency_gate` | default | 1.1 | - | - | yes |
| `test_reductions` | default | 1 | - | - | yes |
| `test_image_kernel` | default | 1 | - | - | yes |
| `test_editor_cook` | default | 1 | - | - | yes |

Under 1 s each in the trace (116, all under budget): test_adjacency, test_array_composition_graph,
test_array_max_length (2), test_automatic_text_cache, test_check, test_core, test_core_control, test_core_draw,
test_core_errors, test_core_lower, test_core_marks, test_core_operators, test_core_records, test_core_state,
test_custom_sop, test_defnode, test_delimiter, test_device_error, test_display_list, test_domain, test_easy_camera,
test_editor_document, test_editor_transactions, test_emit, test_error_rows, test_frame, test_gate,
test_geometry_comparison, test_gpu_image, test_gpu_vertices, test_graph, test_history, test_image_sink, test_input,
test_iterate_graph, test_js_gpu, test_keyboard, test_light, test_lru, test_macro, test_marks, test_math_helpers,
test_metal_argument_reflection_snapshot, test_metal_build_config, test_mock, test_noise, test_nonfinite,
test_ogpu_acceleration/binding/foundation/pipeline/shader/surface/validation, test_op, test_panels, test_param,
test_phase_timer, test_pipelines, test_png_rows, test_prepared_scene3, test_rays_api, test_region_snapshot,
test_resources_audio, test_resources_font, test_run_lifetime, test_runtime_input, test_runtime_resources,
test_runtime_scaling, test_scatter, test_scene_display_list, test_scene_rounded_cache, test_scene3_float32,
test_scene3_native_lowering, test_sdl3, test_sdl3_events, test_sdl3_failure, test_sdl3_mixer,
test_sdl3_mixer_device_failure, test_sdl3_no_dynamic_ffi, test_sdl3_stress, test_sdl3_ttf_discovery_failure,
test_shape_batch (2), test_shell, test_sop_metadata, test_state, test_store, test_string_bytes,
test_subdivide_cracks, test_syntax, test_table, test_target, test_terminal_boolean_normals, test_text_bytes,
test_threading, test_ty, test_ui, test_ui_batch, test_ui_commands, test_value_core, test_wgsl, test_with_error,
test_workspace, test_workspace_edit, test_world, plus the `codemod`/`generate`/`core_census`/`check_runtime_licenses`
self-tests and the SDL3 config checks.

Red in the cold run, both before this change: `test_materials` (the carry-preview budget is the real clock: 572 ms
against 500 ms under load 11-17; 13.8 s alone and it passes then) and `dependency_gate` (its rule chdirs to the
workspace root and the test reads `../specification/workspace/cases/variations.lisp`, which is not in the rule's
`source_tree` deps).

### `RAYS_SLOW=1` (extra executables; solo, load 3-6)

| Alias / executable | Loop | Solo (s) | Under budget |
|---|---|---|---|
| `test_attribute_kernel` | RAYS_SLOW | 25.9 | yes |
| `test_array_composition_web` | RAYS_SLOW | 22.5 | yes |
| `test_rand_at` | RAYS_SLOW | 20.1 | yes |
| `test_array_composition` (flow_ir) | RAYS_SLOW | 19.3 | yes |
| `test_geometry_collections` | RAYS_SLOW | 14.0 | yes |
| `test_region_arguments` | RAYS_SLOW | 12.5 | yes |
| `test_packed` (flow_ir) | RAYS_SLOW | 12.2 | yes |
| `test_gpu_bulk` | RAYS_SLOW | 1.8 | yes |
| `test_workspace_cook` with `RAYS_SLOW=1` | RAYS_SLOW | 25.4 | yes |
| `test_workspace_doc` heavy files, `test_workspace_ir` sketches | RAYS_SLOW | not re-measured; they are the part removed from the default rows above | - |

### `@qualification-scale`

| Alias / executable | Loop | Solo before (s) | Solo after (s) | Under budget |
|---|---|---|---|---|
| `test_parallel_exact` (`run_parallel_exact`; also `qualification`) | qualification-scale | over 300 | not changed | NO |
| `test_nodes` (`run_nodes`, moved here from `runtest`) | qualification-scale | over 300 (41 CPU-min) | not changed | NO |
| `test_sop_catalog_exhaustive` | qualification-scale | 281.8 | not changed | NO |
| `test_shattered_studio` (also `qualification`) | qualification-scale | 84.4 | not changed | NO |
| `test_sop` (`run_sop`) | qualification-scale | 44.6 | 14.9 | yes |
| `test_diagnostics` (`run_diagnostics`) | qualification-scale | 48.4 | 8.6 | yes |
| `test_workspace_shell` with `RAYS_SUITE_BUDGET=900` (default loop runs it with the 45 s budget: 51 s) | default | 150.5 | not changed | NO |

Left over budget in the default loop: `test_workspace_doc` 84 s (parts `contexts` 25 s, `editor_contexts` 7 s, `editor` 9 s,
`literals` 11 s, `files` about 25 s, the rest under 4 s each), `test_workspace_ir` 146 s (fixtures dominate: each is a
multi-time, multi-domain cook of the whole workspace), `test_workspace_live` 32-59 s, `test_bloom_studio` 42 s,
`test_workspace_shell` 51 s, `test_sop_catalog` 44 s, `test_scene_sync` 41 s, `test_lisp_ports` 34 s. Every one of
them is bounded by the same thing: one workspace or one `Lisp_sop.node` lowering costs 0.4-1.4 s
(`Lower.workspace` / `of_checked`; `sop/subdivide` is the worst at 1.1-1.4 s) and the tests lower per edit, per
case or per file. Pruning further deletes coverage; the next step is the lowering cache per (definition, arguments).
