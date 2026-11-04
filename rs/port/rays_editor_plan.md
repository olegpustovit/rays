# rays_editor port plan (edplan)

Crate skeleton exists: `rs/crates/rays_editor/{Cargo.toml, src/lib.rs, src/*.rs (empty)}`; `cargo check -p rays_editor` passes (30 s cold).
Every dep in Cargo.toml exists today (flow_native, rays_pathtracer, editor_document have lib.rs; ports in progress). No dep was commented out.
Source: lib/rays_editor, 42 `.ml`, 21,437 lines. Dune also lists `unix` (-> std::fs/time/thread, no crate) and `lru`.

## Module naming
One OCaml file = one `src/<name>.rs`, same names. Exceptions:
- `renderer.ml` -> `src/editor_renderer.rs` (clashes with extern crate `renderer`). Public path `Rays_editor.Renderer` is re-exported from `rays_editor.rs`.
- `rays_editor.ml` (public API: Editor/Workspace/Settings/Source/Renderer/Reduce) -> `src/rays_editor.rs` as `pub mod rays_editor` (same pattern as editor_core). The `.mli` is the pub surface.
- No other clashes with keywords/std. Private modules stay `pub mod` so tests reach them (`Rays_editor__.X` -> `rays_editor::x`).

## Dependency DAG (internal edges; comment-only hits removed, e.g. editor_commands->editor_panes is only a comment)
| Module | Lines | Internal deps | External crates (use count) |
|---|---|---|---|
| selection | 16 | - | - |
| panel_map | 23 | - | - |
| spreadsheet | 30 | - | pxui, pxui_shell, rdk_rays |
| timeline | 41 | - | rays |
| echo | 46 | - | flow_*, flow_graph |
| pick | 47 | - | pxui, rdk_rays, flow_sop, sop |
| schedule | 61 | - | input |
| editor_menu | 105 | navigator | pxui_graph, flow_sop |
| world_map | 107 | editor_host, editor_inspect, editor_list | pxui_shell |
| source_file | 115 | - | editor_document, editor_core, std::fs |
| bars | 131 | - | pxui, pxui_shell, editor_core |
| loop_view | 211 | editor_commands, editor_host, editor_list, editor_model, editor_shell, timeline | pxui(_shell), flow_native |
| loop_source | 215 | editor_commands, editor_host, editor_model, editor_setup, editor_text, source_file | editor_document, editor_core |
| editor_actions | 219 | editor_commands, selection, timeline | pxui_shell, editor_core |
| editor_setup | 228 | cook, editor_commands, navigator, panel_map, selection, timeline | sdl3, runtime_input, pxui_*, editor_core, flow_native |
| editor_status | 297 | cook, editor_commands, text_pane, timeline | pxui_shell, pxui_graph, flow_graph |
| editor_viewport_pane | 308 | cook | pxui, pxui_shell, rdk_rays |
| editor_shell | 317 | lisp_text, panel_map, selection, text_pane | pxui(_shell), editor_core |
| editor_carry | 343 | carry, cook, editor_commands, selection | pxui, pxui_graph, flow_sop, input |
| editor_inspector_pane | 375 | carry, editor_scope, selection, text_pane | pxui, pxui_shell, flow_graph |
| carry | 393 | text_pane | editor_document, flow_graph, editor_core |
| editor_commands | 409 | text_pane (+ Pxui_shell.Layout.panel) | pxui_shell, editor_core (Command/Router/Keymap), input |
| editor_scope | 416 | timeline | pxui_graph, flow_native, flow_sop, sop |
| editor_add | 421 | - | pxui_graph, flow_graph, flow_sop, editor_document |
| editor_model | 428 | carry, cook, editor_commands, lisp_text, navigator, panel_map, pick, selection, text_pane, timeline | pxui_shell, pxui_graph, flow_native, flow_sop, sop, editor_core (History) |
| editor_host | 452 | carry, cook, editor_commands, text_pane (+ model) | editor_document, flow_sop, editor_core |
| editor_list | 455 | cook, selection | pxui_shell, editor_document, flow_native, rdk_rays |
| editor_text | 462 | cook, selection, text_pane | editor_document, pxui_graph, editor_core |
| renderer | 498 | - | rays_pathtracer, rdk_rays, lru |
| loop_model | 527 | cook, editor_host, editor_inspect, editor_list, editor_model, editor_setup, editor_shell, editor_status, source_file, timeline | flow_native, rdk_rays, editor_core, std::time |
| rays_editor | 595 | cook, editor_add, editor_inspect, editor_loop, editor_model, editor_panels, editor_panes, editor_reduce, editor_scope, editor_shell, editor_status, lisp_text, loop_model, loop_view, renderer, source_file, viewport | flow_native (31), rays, sop, flow_sop |
| editor_panels | 645 | bars, editor_commands, navigator, selection, text_pane | pxui_shell, pxui_graph, flow_graph, editor_document |
| lisp_text | 710 | - | pxui (Ui.language), flow_*, lru |
| editor_panes | 732 | bars, cook, editor_commands, navigator, panel_map, pick, selection, spreadsheet, text_pane, timeline | pxui_shell, pxui_graph (22), pxui |
| cook | 779 | pick, schedule, timeline | flow_native, rdk_rays (10), flow_sop (8), flow_graph, lru, std::thread/time |
| editor_loop | 1059 | cook, editor_commands, editor_frame, editor_host, editor_inspect, editor_list, editor_model, editor_panes, editor_shell, editor_status, editor_text, source_file, text_pane, timeline | flow_native (30), rays, input, editor_core |
| navigator | 1140 | - | pxui_shell, pxui, flow_* (30), flow_graph |
| viewport | 1154 | cook, editor_commands, editor_host, editor_inspect, editor_list, editor_model, renderer | pxui, editor_core |
| editor_frame | 1544 | carry, editor_commands, editor_reduce, navigator, panel_map, selection, text_pane, timeline | pxui_shell (66), pxui (23), pxui_graph, flow_graph, editor_core |
| editor_reduce | 1589 | carry, cook, echo, editor_commands, navigator, panel_map, selection, text_pane, timeline | editor_document (37), pxui_graph (20), flow_graph, editor_core |
| editor_inspect | 1809 | cook, navigator, text_pane | flow_graph (89), flow_* (84), pxui (44), pxui_shell (25), flow_sop |
| text_pane | 1985 | - | pxui_shell, pxui, flow_graph (17), editor_document |

(editor_frame also reads the model chain via `open`; the AGENTS.md chain order is a valid topological order.)

## Bundles (6, 3.1k to 3.9k lines each)
| Bundle | Modules | Lines | Upstream crates |
|---|---|---|---|
| A base | selection, panel_map, spreadsheet, timeline, echo, pick, schedule, source_file, bars, lisp_text, cook, carry, editor_commands, editor_actions, editor_renderer (renderer.ml), editor_viewport_pane | 3826 | editor_core, pxui, pxui_shell, flow_syntax/check/eval, flow_graph, flow_sop, sop, flow_native (Cook), rdk_rays, rays_pathtracer (renderer), editor_document (carry, source_file), input, lru |
| B text+outline | text_pane, navigator | 3125 | pxui, pxui_shell, flow_graph, flow_*, editor_document, input |
| C panes | editor_inspect, editor_panes, editor_scope, editor_add, editor_menu, editor_inspector_pane | 3858 | pxui, pxui_shell, pxui_graph, flow_graph, flow_sop, sop, flow_native (scope), editor_document |
| D model chain | editor_model, editor_shell, editor_list, editor_setup, editor_status, editor_text, editor_panels, editor_carry, editor_host | 3627 | editor_core (History), pxui_shell, pxui_graph, flow_native, editor_document, sdl3, runtime_input, rdk_rays |
| E reduce+frame | editor_reduce, editor_frame | 3133 | editor_document (Document_edit), pxui_graph, pxui_shell, pxui, editor_core (Router/History) |
| F loop+API | viewport, world_map, loop_view, loop_model, loop_source, editor_loop, rays_editor | 3868 | flow_native (~70 uses: loop/marks/workspace), rays, renderer, rays_pathtracer, runtime_input, sdl3, input, sop |

Ordering: A and B are fully parallel; C needs A+B; D needs A+B (+C for editor_inspect users: loop_model/world_map are in F, so D needs only A+B plus editor_scope not at all); E needs D; F needs E. Agents can write ahead against the OCaml `.mli` files (bars, loop_*, navigator, panel_map, text_pane, timeline, rays_editor) and fix at compile time.
Outside blockers: `flow_native` (Cook in A, Loop_* in F, ~80 uses), `rays_pathtracer` (renderer in A), `editor_document` Document_edit / Workspace_doc (carry, source_file in A; D; E).

## Generics: where editor_core / pxui_shell get specialised
Gate (rs/tools/gate/src/main.rs ~l.185): `adapter_crate = rays_collections | editor_core | pxui_shell` may keep generics until rays_editor exists.
Instances used: `Command.t<S,A>` / `command::make` / `for_guide` / `Router.step<S,A>` and pxui_shell `which_key` (`page<S,A>`, `by_category<S,A>`) and `tree::bindings<S>` with exactly `S = Pxui_shell.Layout.panel`, `A = Editor_commands.action`; and `History.t<A,E>` with `A = Document`.
- Command/Router/which_key: specialised in bundle A (editor_commands, which defines `action`). Because `action` lives in rays_editor, editor_core/pxui_shell cannot name it: either move the action type down, or keep only non-generic pieces (Trigger, Keymap, guide) in editor_core and monomorphise command/router into editor_commands. Callers of Router: editor_frame (E), viewport (F).
- History: specialised in bundle D (editor_model is the first user; also host/text/setup/carry/reduce/loop_model).
- When E compiles and tests pass, drop `editor_core` and `pxui_shell` from `adapter_crate` (only `rays_collections` stays). This touches editor_core/pxui_shell + their tests (test_history, test_layout, test_logic) and the gate: do it as its own step after E, not inside a bundle.

## OCaml tests (lib/rays_editor/test, 26.7k lines) to bundles
Nearly everything goes through the public `Rays_editor.Editor`, so it is runnable only after F. Direct private-module use in the last column.
| Test | Lines | Earliest | Direct private modules |
|---|---|---|---|
| test_text_pane | 2160 | B (+cook from A) | text_pane, cook |
| test_editor_commands | 601 | F | lisp_text |
| test_editor_cook, test_probe, test_workspace_live, test_viewport_pick | 365/530/325/317 | F | cook, timeline, pick |
| test_sop_ui | 405 | F | editor_inspect, editor_model, renderer |
| test_packed_pieces | 287 | F | schedule, timeline |
| test_drawing, test_materials, test_rays_editor, test_workspace_shell (5239; slow sections under RAYS_SLOW=1), test_workspace_source | 525/1026/1086/5239/875 | F | editor_commands, viewport, renderer, bars |
| test_editor_consistency (1649), test_bloom_studio, test_isogrid_deferred, test_scene_tree, test_scene_sync (1748) | | F | cook, text_pane, editor_loop, renderer |
| test_editor_document/input/transactions, test_history_memory, test_host_effects, test_open_domain, test_custom_sop, test_2d_ports, test_flow_shader, test_shattered_studio, test_sop_catalog (1186), test_workspace_doc (1399), test_workspace_ir/images/images_native | | F | none (public API only) |
| test_fast_path, test_gallery_shapes, test_image_sop, test_rays, test_threading, test_workspace_start, test_terminal_boolean_normals, test_text, test_main; helpers test_support, ws_fixture, editor_prepare, workspace_parity, sop_render_parity, sop_params_fixture | | F | harness / fixtures |
| ../marks_scene_test, ../native_qualification (bench_*, test_*_native) | | F, slow | |
Per-bundle checks other than B: each bundle agent adds a small `src/test_<module>.rs` (gate convention for test files) with assertions lifted from the above for its own modules; the whole-suite ports wait for F.

## tests/ layout (window/GPU)
Follow crates/pxui/tests and crates/renderer/tests: `[[test]] harness = false`, own `main`, `#[path = "../../rays/tests/common/mod.rs"] mod common;` and `common::window::run(&cases)`; `cases/*.rs` export `pub const CASES: &[(&str, fn())]`. Set SDL video driver hint `dummy` and init Video+Events before running. gpu_* targets are offscreen and run in parallel; window_* targets run alone.
```
tests/
  cases/support.rs        fixtures: ws_fixture, editor_prepare, test_support, workspace_parity
  cases/text_pane.rs      test_text_pane (B; pure, may instead be src/test_text_pane.rs)
  cases/editor_logic.rs   test_rays_editor_logic, test_editor_{document,input,commands,transactions}, test_history_memory, test_host_effects
  cases/workspace.rs      test_workspace_{doc,source,live,start,ir}, test_scene_sync, test_scene_tree, test_editor_cook, test_probe
  cases/consistency.rs    test_editor_consistency (+ _native)
  cases/shell.rs          test_workspace_shell fast sections; slow ones behind RAYS_SLOW=1
  cases/sop.rs            test_sop_catalog, test_sop_ui, test_custom_sop, test_materials, test_shattered_studio, test_bloom_studio, test_2d_ports
  gpu_editor_render.rs    offscreen: sop_render_parity, test_flow_shader, test_workspace_images, test_gallery_shapes
  window_editor_all.rs    own process: test_rays_editor, test_editor_canvas_native, test_workspace_images_native, test_workspace_pixels, native_qualification/*
  window_editor_shell.rs  test_workspace_shell native sections
```
Add each target to Cargo.toml as `[[test]] name = "..." harness = false` when its first case lands (dev-deps ogpu, ogpu_vulkan, renderer already present). Budgets: 30 s warm per target, 5 s per case.

## Editor API for tests (bundle F, `rays_editor::Editor` and friends)

The public surface of `rays_editor.ml` is `src/rays_editor.rs`, re-exported at the crate root
(`use rays_editor::{Editor, Reduce, Workspace, Measure, Source, Settings, Renderer, Timeline, Workspace_doc}`).
A worked example is `tests/window_editor_smoke.rs` (4 cases, about 8 s for the target in a debug build).

**Harness.** A target that creates an `Editor` is `window_editor_*` (`[[test]] harness = false`, cases on the
main thread through `../../rays/tests/common/window.rs`), with `sdl3::hint::set(VIDEO_DRIVER, "dummy")` and
`sdl3::init::init(Video, Events)` first in `main`, as pxui does. Call `common::gpu()` at the top of each
*case*, never in `main`: the lease it takes aborts the process at exit when `--list` returns before
`window::run` releases it (0xC0000005 with no output, which `cargo t` reports as exit 5). Cases that need no
device (pure `Editor::Private`, `Reduce` on a cooked-free workspace) still need the dummy-driver SDL for the fonts.

**Creating.**
```rust
let ws = Workspace::load(None /*ops*/, imports, None /*factories*/, text)?;          // Vec<Diagnostic> on error
let ed = Editor::create(Editor::Create { await_: Some(true), ui_text_size: Some(11), domains: Some(1), ..Default::default() },
                        &ws, None /*Source::t*/, Editor::prepare_fn(), Editor::draw_fn())?;   // Result<Editor::t, String>
```
`Editor::Create` is `loop_model::create_options` (OCaml's optional arguments: `inputs name presets factories
gpu_owner settings lights camera background seed grain domains max_entries max_payload_bytes await_ carry_budget
preferences ui_text_size platform ui_font opened`). `await_: Some(true)` makes `update` block on its cook, as the
OCaml tests do. Tests that want an isolated state folder pass `presets: Some(dir)`.

**Frames.** `Editor::t` is a plain `Clone` value passed and returned by value, as in OCaml:
`ed = Editor::update(ed, &frame); let nodes = Editor::scene(&ed, &frame); ed = Editor::after_present(ed, &frame);`
(`Editor::update_with(ed, &frame, &|ui| Rc::new(...))` for a sketch inspector, answering `(t, Option<Rc<dyn Any>>)`).
Build the `rays::frame::t` like `Measure::frame_at` / the smoke test's `frame_at` (size, pointer, buttons, keys,
events, `pixel_scale (1,1)`). The 120-frame cadence and `Sketch.Fixed dt` of the OCaml helpers are `input::Frame.dt`
and `tick`/`frame` set by the test. `Editor::close(&ed)` at the end of each case.

**What a test reaches.**
- `ed.core` (`editor_model::t`), `ed.extra` (viewport state), `ed.camera`, `ed.host` (`Rc<RefCell<workspace_host::t>>`), plus the
  accessors of the mli as functions: `Editor::{can_undo, can_redo, undo_label, redo_label, workspace, document, scene_document,
  network, prepared, selected_node, displayed_node, timeline, panes, probe, set_probe, node_box, edit, edit_layout, edit_phases,
  carrying, carry_line, level, objects, lights, world, camera, viewport_camera, view_camera, render_camera, render_settings, film,
  slot_of, flying, look_through, renderer, set_renderer, settings, set_settings}`; `Editor::Private::*` (status_text, live_frame,
  inspector_subject, new_graph, menu_forms, lisp_completions, image_plan, image_payload, image_stats, image_render_stats,
  canvas_scenes, image_gpu_stats, image, with_images, host_stats, resolved_scene, gpu_qualification).
- OCaml's `{ editor with Loop_model.core = ... }` is `Editor::with_core(ed, core)`; use it (not `ed.core = ...`) so the idle-frame
  bookkeeping (`ed.rest`) is dropped: the loop's "nothing changed" shortcut is valid only while the core is what the last full frame
  left. Same for `Editor::set_camera`.
- `Reduce::{open_import, spreadsheet, select_path, view, step}`; `step` returns `Result<Editor::t, String>` (`reduce_idle` does).
- Tests that need the private modules use `rays_editor::<module>` (`editor_commands`, `cook`, `viewport`, `editor_renderer`, ...);
  the OCaml `Rays_editor.Renderer` is `Renderer` (= `editor_renderer`) with `Mode::{Path_traced, Raster, Wireframe}`.
- `Editor::Private::canvas_scenes` and `scene` answer `Vec<rays::scene::node>`.

**Divergences a ported test meets.**
- `Workspace::load(ops, imports, factories, text)`, `Workspace::main(text, process, path)`, `Workspace::export(graph, fps, dir, frames, doc)`,
  `Workspace::sop_graphs(factories, doc)`: OCaml optional arguments are leading `Option` parameters. `Workspace::process.profile` is `bool`.
- `Measure::run(MeasureOptions, step, early, frames, &ws)`; its `outcome.final` is `outcome.fin` (keyword). `track_sites` (Gc.Memprof) has no
  Rust counterpart and is absent; `sample.live` is the allocator's live bytes (no GC), `promoted` is 0.
- The camera panel and the viewport handles are closures that receive the `Ui` the frame holds (`editor_frame::update`'s
  `camera_panel: &dyn Fn(&mut Ui) -> Panel_result`; it took no argument before bundle F because OCaml closes over the handle).
- `draw_timeline_pane` takes `(Panel::Timeline, box)` as OCaml does (bundle C's `Leaf` stand-in is gone).
- Physical equality of OCaml values that have no Rust identity is by value: a hidden scene is reused by `Scene3::same` + camera, the
  textures of a drawn object are never known to be the same ones (a textured piece is composed again each frame), a bake and a
  drawing are `Rc` identity. Tests asserting "the same Scene.t" use `Scene3::same` / `Scene::Layer::same`.
- `Mem.root` heap walks: the ledger owner `editor.document` is the size of the document text; `editor.history` and `editor.lowering`
  are charged nothing, and `memory_boundary` sheds history beyond 16 entries after the first `Over` verdict (the OCaml `later` step).

**Known gaps and findings (bundle F).**
- Lowering `sketches/ws_layout.rays` never returns in the Rust port: `lowering::of_workspace` of a graph with
  `sop/scatter ... :count count :seed seed` (graph inputs) plus `copy_to_points` spins forever in `editor_document`/`flow_sop`/`sop`
  (before any editor code runs; repro: the `garden` graph alone, `diag` text in the smoke test history). The smoke test runs a
  ws_layout look-alike without the scatter (`smoke_studio`); `smoke_ws_layout` is gated on `RAYS_SMOKE_WS_LAYOUT=1` until that is fixed.
- `rays::texture::t` has no identity (`same`); add it to drop the conservative recomposition of textured pieces.
- `flow_sop::lower::ImageMetadata` must be `Send + Sync`, `workspace_images::t` is not: the frame's dimensions callback reads a
  thread-local holding the latest frame's images (`editor_loop::FRAME_IMAGES`); `Editor::Private::with_images` passes a callback that
  answers none.
- The canvas pictures are not wrapped in `refusal_typed` (a refused allocation is whatever `Mem::admit_*` returns inside the kernels).
- Nothing exercises `Editor::run` / `Workspace::main` headless (they open a window): covered by the type check only. The bin crate is not part of this bundle.
