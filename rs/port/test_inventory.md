# OCaml test inventory vs Rust port

Scope: all 432 `lib/**/test_*.ml` files (365 at depth `lib/<x>/`, 67 nested). Generated from a name and comment scan of `rs/` (`target/` excluded).

**Summary: PORTED 233, STUB 55, PARTIAL 45, MISSING 96, rdk verified 2026-11-04 (432 files).**

Method:
- PORTED: a Rust file with the same basename (or a `tests/` file without the `test_` prefix), or a Rust comment or `##` backlog header naming the file, or a same-named module containing `#[cfg(test)]`. The last kind is a name-only match: 48 PORTED rows, unverified.
- PARTIAL: named as ported, but a Rust header or `port/parity_backlog.md` says parts are not ported, skipped or unported.
- MISSING: none of the above.
- Library: top-level directory under `lib/`.
- Cases: top-level `let test*` functions. Files without that style are counted by assertion-style call lines (`assert`, `check`, `expect*`), so the figures are a size proxy, not an exact case count. Files with zero detected checks show 0.

## Libraries with the most missing cases

| Library | Missing cases | Missing files |
|---|---:|---:|
| rays_editor | 280 | 13 |
| flow_gpu | 181 | 5 |
| flow_sop | 117 | 9 |
| scene_execution | 78 | 6 |
| mem | 46 | 1 |
| param | 46 | 1 |
| metal | 44 | 16 |
| flow | 43 | 2 |
| renderer | 37 | 8 |
| lru | 32 | 1 |
| ogpu_metal | 23 | 6 |
| flow_web | 21 | 2 |
| rays_math | 18 | 2 |
| scene_command | 17 | 3 |

Total MISSING cases: 1043 (rdk row removed, see "rdk verification").

## MISSING (96 files, sorted by library, then cases)

| Library | File | Cases | Basis |
|---|---|---:|---|
| diagnostic | `lib/diagnostic/test_diagnostic.ml` | 6 | checks |
| flow | `lib/flow/test_macro.ml` | 26 | checks |
| flow | `lib/flow/test_value_core.ml` | 17 | checks |
| flow_gpu | `lib/flow_gpu/test_image_sink_native.ml` | 76 | checks |
| flow_gpu | `lib/flow_gpu/test_run_lifetime.ml` | 60 | checks |
| flow_gpu | `lib/flow_gpu/test_image_sink.ml` | 42 | checks |
| flow_gpu | `lib/flow_gpu/test_sampling.ml` | 3 | checks |
| flow_gpu | `lib/flow_gpu/test_raster.ml` | 0 | checks |
| flow_sop | `lib/flow_sop/test_geometry_arrays.ml` | 51 | checks |
| flow_sop | `lib/flow_sop/test_math_helpers.ml` | 33 | checks |
| flow_sop | `lib/flow_sop/test_gpu_bulk.ml` | 13 | checks |
| flow_sop | `lib/flow_sop/test_mesh_planes.ml` | 11 | checks |
| flow_sop | `lib/flow_sop/test_concat.ml` | 5 | checks |
| flow_sop | `lib/flow_sop/goldens/test_fixture_codec.ml` | 3 | checks |
| flow_sop | `lib/flow_sop/test_planes.ml` | 1 | checks |
| flow_sop | `lib/flow_sop/test_units.ml` | 0 | checks |
| flow_sop | `lib/flow_sop/test_units_geometry.ml` | 0 | checks |
| flow_web | `lib/flow_web/test_region_arguments.ml` | 15 | checks |
| flow_web | `lib/flow_web/test_array_composition_web.ml` | 6 | checks |
| input | `lib/input/test_gate.ml` | 0 | checks |
| lru | `lib/lru/test_lru.ml` | 32 | checks |
| mem | `lib/mem/test_mem.ml` | 46 | checks |
| metal | `lib/metal/test_metal_compute_encoder_safe.ml` | 10 | checks |
| metal | `lib/metal/test_metal_render_encoder_safe.ml` | 9 | checks |
| metal | `lib/metal/test_metal_support.ml` | 7 | checks |
| metal | `lib/metal/test_metal_function_constants_safe.ml` | 5 | checks |
| metal | `lib/metal/test_metal_argument_reflection_snapshot.ml` | 4 | checks |
| metal | `lib/metal/test_metal_command_buffer_safe.ml` | 3 | checks |
| metal | `lib/metal/test_metal_resource_bounds.ml` | 3 | checks |
| metal | `lib/metal/test_metal_blit_safe.ml` | 1 | checks |
| metal | `lib/metal/test_metal_device_library_safe.ml` | 1 | checks |
| metal | `lib/metal/test_metal_device_safe.ml` | 1 | checks |
| metal | `lib/metal/test_metal_build_config.ml` | 0 | checks |
| metal | `lib/metal/test_metal_compute_pipeline_safe.ml` | 0 | checks |
| metal | `lib/metal/test_metal_device_queue_safe.ml` | 0 | checks |
| metal | `lib/metal/test_metal_render_icb_argument.ml` | 0 | checks |
| metal | `lib/metal/test_metal_render_pipeline_safe.ml` | 0 | checks |
| metal | `lib/metal/test_metal_sampler_safe.ml` | 0 | checks |
| native_layer_token | `lib/native_layer_token/test_native_layer_token.ml` | 11 | checks |
| ogpu | `lib/ogpu/conformance/test_mock.ml` | 0 | checks |
| ogpu_metal | `lib/ogpu_metal/test_ogpu_metal_acceleration.ml` | 7 | checks |
| ogpu_metal | `lib/ogpu_metal/test_ogpu_metal_foundation.ml` | 7 | checks |
| ogpu_metal | `lib/ogpu_metal/test_ogpu_metal_texture.ml` | 4 | checks |
| ogpu_metal | `lib/ogpu_metal/test_ogpu_metal_pipeline.ml` | 3 | checks |
| ogpu_metal | `lib/ogpu_metal/test_ogpu_metal_sampler_pixels.ml` | 2 | checks |
| ogpu_metal | `lib/ogpu_metal/test_ogpu_metal_capabilities.ml` | 0 | checks |
| parallel | `lib/parallel/test_parallel.ml` | 7 | checks |
| param | `lib/param/test_param.ml` | 46 | checks |
| pxui | `lib/pxui/test_ui_parity.ml` | 8 | checks |
| rays | `lib/rays/test_world_raster.ml` | 6 | checks |
| rays | `lib/rays/test_automatic_text_cache.ml` | 2 | checks |
| rays | `lib/rays/test_canvas_native.ml` | 2 | checks |
| rays | `lib/rays/test_scene_display_list.ml` | 1 | checks |
| rays | `lib/rays/test_retained_view_native.ml` | 0 | checks |
| rays_editor | `lib/rays_editor/native_qualification/test_workspace_gpu_native.ml` | 98 | checks |
| rays_editor | `lib/rays_editor/native_qualification/test_placement_native.ml` | 96 | checks |
| rays_editor | `lib/rays_editor/native_qualification/test_isogrid_viewport.ml` | 55 | checks |
| rays_editor | `lib/rays_editor/marks_scene_test/test_marks_scene.ml` | 18 | checks |
| rays_editor | `lib/rays_editor/native_qualification/test_workspace_gpu_marks.ml` | 8 | checks |
| rays_editor | `lib/rays_editor/native_qualification/test_chain_gpu_native.ml` | 2 | checks |
| rays_editor | `lib/rays_editor/native_qualification/test_grid_gpu_native.ml` | 1 | checks |
| rays_editor | `lib/rays_editor/native_qualification/test_holder_frames_native.ml` | 1 | checks |
| rays_editor | `lib/rays_editor/test/test_rays.ml` | 1 | checks |
| rays_editor | `lib/rays_editor/native_qualification/test_chain_frames_native.ml` | 0 | checks |
| rays_editor | `lib/rays_editor/native_qualification/test_live_mesh_native.ml` | 0 | checks |
| rays_editor | `lib/rays_editor/native_qualification/test_scrub_gpu_native.ml` | 0 | checks |
| rays_editor | `lib/rays_editor/test/test_text.ml` | 0 | checks |
| rays_math | `lib/rays_math/test_color.ml` | 16 | checks |
| rays_math | `lib/rays_math/test_clock.ml` | 2 | checks |
| renderer | `lib/renderer/test_batch_merge.ml` | 9 | checks |
| renderer | `lib/renderer/test_dense_scene2.ml` | 9 | checks |
| renderer | `lib/renderer/test_scene3_instances.ml` | 9 | checks |
| renderer | `lib/renderer/test_scene3_lighting.ml` | 6 | checks |
| renderer | `lib/renderer/test_ui_pipeline.ml` | 2 | checks |
| renderer | `lib/renderer/test_marks_native.ml` | 1 | checks |
| renderer | `lib/renderer/test_shapes_lowering.ml` | 1 | checks |
| renderer | `lib/renderer/test_runtime_render.ml` | 0 | checks |
| scene_command | `lib/scene_command/test_ui_primitives.ml` | 12 | checks |
| scene_command | `lib/scene_command/test_marks.ml` | 5 | checks |
| scene_command | `lib/scene_command/test_display_list.ml` | 0 | checks |
| scene_execution | `lib/scene_execution/test_gpu_vertices.ml` | 45 | checks |
| scene_execution | `lib/scene_execution/test_prepared_scene3.ml` | 20 | checks |
| scene_execution | `lib/scene_execution/test_offscreen_metal.ml` | 8 | checks |
| scene_execution | `lib/scene_execution/test_retained_eviction.ml` | 2 | checks |
| scene_execution | `lib/scene_execution/test_scene_execution_metal.ml` | 2 | checks |
| scene_execution | `lib/scene_execution/test_automatic_scratch.ml` | 1 | checks |
| sdl3 | `lib/sdl3/test_sdl3.ml` | 9 | checks |
| sdl3 | `lib/sdl3/test_sdl3_events.ml` | 4 | checks |
| sdl3 | `lib/sdl3/test_sdl3_metal.ml` | 2 | checks |
| sdl3 | `lib/sdl3/test_sdl3_failure.ml` | 0 | checks |
| sdl3 | `lib/sdl3/test_sdl3_stress.ml` | 0 | checks |
| sdl3_image | `lib/sdl3_image/test_sdl3_image.ml` | 2 | checks |
| sdl3_mixer | `lib/sdl3_mixer/test_sdl3_mixer.ml` | 0 | checks |
| sdl3_mixer | `lib/sdl3_mixer/test_sdl3_mixer_device_failure.ml` | 0 | checks |
| sdl3_ttf | `lib/sdl3_ttf/test_sdl3_ttf.ml` | 0 | checks |
| sdl3_ttf | `lib/sdl3_ttf/test_sdl3_ttf_discovery_failure.ml` | 0 | checks |

## PARTIAL (45 files)

| Library | File | Note |
|---|---|---|
| flow_gpu | `lib/flow_gpu/test_helpers.ml` | header: only the GPU half of test_helpers.ml ported |
| flow_sop | `lib/flow_sop/test_adjacency.ml` | header: only the parts that need no sop/group_edges (or packed-vs-reference) ported |
| flow_sop | `lib/flow_sop/test_graph.ml` | parity_backlog Unported list: most cases of this file not ported (test_trace: kernel half; test_js_frame: node half) |
| flow_sop | `lib/flow_sop/test_group_kernel.ml` | parity_backlog Unported list: most cases of this file not ported (test_trace: kernel half; test_js_frame: node half) |
| flow_sop | `lib/flow_sop/test_keyword_ports.ml` | header: catalog side only |
| flow_sop | `lib/flow_sop/test_lisp_fusion.ml` | parity_backlog Unported list: most cases of this file not ported (test_trace: kernel half; test_js_frame: node half) |
| flow_sop | `lib/flow_sop/test_lisp_node.ml` | parity_backlog Unported list: most cases of this file not ported (test_trace: kernel half; test_js_frame: node half) |
| flow_sop | `lib/flow_sop/test_lisp_projection.ml` | parity_backlog Unported list: most cases of this file not ported (test_trace: kernel half; test_js_frame: node half) |
| flow_sop | `lib/flow_sop/test_math_kernel.ml` | header: only the parts that need no sop/group_edges (or packed-vs-reference) ported |
| flow_sop | `lib/flow_sop/test_merge_source.ml` | header: reduced port (Mesh_merge_front oracle dropped) |
| flow_sop | `lib/flow_sop/test_mesh_resident.ml` | parity_backlog Unported list: most cases of this file not ported (test_trace: kernel half; test_js_frame: node half) |
| flow_sop | `lib/flow_sop/test_metadata_kernel.ml` | parity_backlog Unported list: most cases of this file not ported (test_trace: kernel half; test_js_frame: node half) |
| flow_sop | `lib/flow_sop/test_node_facts.ml` | parity_backlog Unported list: most cases of this file not ported (test_trace: kernel half; test_js_frame: node half) |
| flow_sop | `lib/flow_sop/test_node_sources.ml` | parity_backlog Unported list: most cases of this file not ported (test_trace: kernel half; test_js_frame: node half) |
| flow_sop | `lib/flow_sop/test_projection.ml` | parity_backlog Unported list: most cases of this file not ported (test_trace: kernel half; test_js_frame: node half) |
| flow_sop | `lib/flow_sop/test_rand_at.ml` | header: only the parts that need no sop/group_edges (or packed-vs-reference) ported |
| flow_sop | `lib/flow_sop/test_remap.ml` | parity_backlog Unported list: most cases of this file not ported (test_trace: kernel half; test_js_frame: node half) |
| flow_sop | `lib/flow_sop/test_trace.ml` | parity_backlog Unported list: most cases of this file not ported (test_trace: kernel half; test_js_frame: node half) |
| flow_sop | `lib/flow_sop/test_workspace_cook.ml` | parity_backlog Unported list: most cases of this file not ported (test_trace: kernel half; test_js_frame: node half) |
| flow_sop | `lib/flow_sop/test_workspace_edit.ml` | parity_backlog Unported list: most cases of this file not ported (test_trace: kernel half; test_js_frame: node half) |
| flow_sop | `lib/flow_sop/test_workspace_zone.ml` | parity_backlog Unported list: most cases of this file not ported (test_trace: kernel half; test_js_frame: node half) |
| flow_web | `lib/flow_web/test_array_max_length.ml` | parity_backlog Unported list: most cases of this file not ported (test_trace: kernel half; test_js_frame: node half) |
| flow_web | `lib/flow_web/test_compact_scan.ml` | parity_backlog Unported list: most cases of this file not ported (test_trace: kernel half; test_js_frame: node half) |
| flow_web | `lib/flow_web/test_gather.ml` | parity_backlog Unported list: most cases of this file not ported (test_trace: kernel half; test_js_frame: node half) |
| flow_web | `lib/flow_web/test_grade_index.ml` | parity_backlog Unported list: most cases of this file not ported (test_trace: kernel half; test_js_frame: node half) |
| flow_web | `lib/flow_web/test_js_frame.ml` | parity_backlog Unported list: most cases of this file not ported (test_trace: kernel half; test_js_frame: node half) |
| flow_web | `lib/flow_web/test_scatter.ml` | parity_backlog Unported list: most cases of this file not ported (test_trace: kernel half; test_js_frame: node half) |
| rays_editor | `lib/rays_editor/test/test_drawing.ml` | backlog: only the pure mode ported |
| rays_editor | `lib/rays_editor/test/test_editor_consistency.ml` | backlog: run_native/render_mode_click not ported; consistency_frozen_driven skipped (HANG) |
| rays_editor | `lib/rays_editor/test/test_editor_document.ml` | header: lowering parts only |
| rays_editor | `lib/rays_editor/test/test_editor_input.ml` | header: support module for the suite, not a standalone test target |
| rays_editor | `lib/rays_editor/test/test_fast_path.ml` | header: part of the file ported; the rest drives lowering that is still ignored |
| rays_editor | `lib/rays_editor/test/test_host_effects.ml` | header: errors only ported (start/reload path) |
| rays_editor | `lib/rays_editor/test/test_isogrid_deferred.ml` | backlog: NOT PORTED (stub in suite_t6/isogrid.rs) |
| rays_editor | `lib/rays_editor/test/test_materials.ml` | backlog: carry_tests and viewport_carry_tests not ported |
| rays_editor | `lib/rays_editor/test/test_scene_sync.ml` | backlog: qualification_roots and the through-the-editor block unported |
| rays_editor | `lib/rays_editor/test/test_sop_ui.ml` | backlog: dynamic_schema and the sop_params PPX checks not ported |
| rays_editor | `lib/rays_editor/test/test_text_pane.ml` | backlog: only editor_graph_drafts and editor_fn_frames ported |
| rays_editor | `lib/rays_editor/test/test_threading.ml` | header: errors only ported (start/reload path) |
| rays_editor | `lib/rays_editor/test/test_workspace_doc.ml` | backlog: part_editor, part_live, part_preset and others unported |
| rays_editor | `lib/rays_editor/test/test_workspace_ir.ml` | backlog: static part only |
| rays_editor | `lib/rays_editor/test/test_workspace_shell.ml` | backlog: run_native and window sections not ported; several cases SKIPPED |
| rays_editor | `lib/rays_editor/test/test_workspace_start.ml` | header: errors only ported (start/reload path) |
| sop | `lib/sop/test_parallel_exact.ml` | header: session-level core only |
| sop | `lib/sop/test_sop.ml` | header: only the parts that need no sop/group_edges (or packed-vs-reference) ported |

## Caveats

- Name matching is textual. A module with a different name (e.g. an rdk `group_ops.rs` holding a port) is invisible to the scan and shows as MISSING. Verify the largest MISSING rows before planning work.
- The rdk and rays_editor counts dominate. rdk tests are mostly `let run ()` scripts with `check` helpers, so their cases are a proxy.
- 48 PORTED rows are the name-only `#[cfg(test)]` match. Re-verify those first.

## rdk verification (2026-11-04)

All 64 rdk rows were MISSING by name. Checked against `lib/rdk/dune`'s `test_main` and the Rust `rdk_*` crates:

- 47 files are stubs: `let run () = print_endline "... passed"`, no assertion on product code (attribute_composite, attribute_copy, attribute_fade, blast_by_attribute, blend_shapes, circle, circle_from_edges, crease, distance_along_geometry, distance_from_target, duplicate, edge_divide, edge_equalize, edge_flip, ends, extract_centroid, extract_point_curve, group_{backface,boundary,bounds,edge_angle,edge_depth,find_path,non_planar,normal,promote_boundary,random,range_connectivity,unshared}, groups_from_name, laplacian, match_size, motion, normals, platonic, point_generate, point_split, poly_bridge, poly_loft, polyframe, polywire, resample, reverse, revolve, rewire_vertices, separate_pieces, skin, spiral, sweep, tube). Their cases counted in this table were helper-function checks that never run.
- 7 more are stubs with unused helpers (`_`-prefixed values, a cancel token, no assertion): edge_cusp, edge_relax, edge_straighten, point_jitter (one `Group.init` self-check), plus edge_divide/equalize/flip above. 12 OCaml modules they would exercise (scatter, spiral, polywire, sweep, crease, duplicate, edge_ops, extract_point_curve, laplacian, motion, revolve, sweep_modeling) have no Rust file; accepted Lisp nodes replace them.
- `test_support/test_geometry_comparison.ml` tests the OCaml-only `close_geometry` ulp comparator; Rust compares with `geometry_signature`. No port.
- 9 files assert for real. Already ported under other names: clip (`plane_clip.rs`, 11 of 11 sections), connectivity (`rdk_core/analysis.rs`), fuse (`fuse_grid.rs` and `fuse_*.rs`), group_catalog (`group_ops.rs`), instances (`instance_copy.rs`), soft_transform and transform (`transform_ops.rs`). Gaps found and ported now: connectivity validation and sentinels; fuse one/four-domain target and rule exactness; rdk_sources pattern/rewrite suite (`attribute_pattern.rs`); rdk_misc tuple blur (`attribute_blur.rs`).
- The 3 PARTIAL rdk rows are complete: test_rdk (topology validation and deferred topology in `rdk_core/topology.rs`, group/extrude remap in `rdk_rays/tests.rs`), test_rdk_attributes (surface-index extras in `rdk_geom/surface_index.rs`; the transfer half was already in `attribute_transfer.rs`), test_distance_from_geometry (curve reference and domain exactness in `transform_ops.rs`).
- Not ported on purpose: the million-point range kernel (`Geometry.generate_point_ranges`, sequential in Rust, covered by `point_range_generator_keeps_fixed_plane_lengths`); the OCaml minor-heap class-bit regression; the "released topology still materialised" thunk (a Rust materialiser returns a value, it cannot raise); one/four/eight-domain loops over topology validation (sequential in Rust).
