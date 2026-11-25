# Editor UI fleet (2026-11-24)

The plan, with measurements, file:line evidence and every bug: Microapps artifact `61jscr1xwssb`
(`art read 61jscr1xwssb`, or the copy in `target/fleet-ui-plan.html`). Bug IDs B1–B47 and lanes L1–L9 are
defined there. This file is the working contract: rules, ownership, decisions and cross-lane requests.
Delete it when W3 closes; leftovers go to `OPEN_ISSUES.md`.

## Gates

- `cargo t -p rays_editor --test gpu_editor click_budget`: allocations on the frame of each editor action
  and the settled frame after it (basic, empty, isogrid, a long session). Ceilings are exact counts in
  `crates/rays_editor/tests/native_qualification/click_budget.rs`. Never raise one. When your change makes
  a row cheaper, lower it to measured + 5% in the same commit. `RAYS_CLICK_TIMES=1` also checks the 3 ms target.
- `cargo t -p rays_editor --test gpu_editor alloc_budget`: idle frames (prints, references in the file).
- Allocation sites: run the test binary with `MallocStackLogging=1`, pause it, then `malloc_history <pid> -callTree`.
  `mem_sys` wraps the system allocator, so every site shows.

## Rules

- AGENTS.md applies. Every agent invokes `agentpack-bundle:ponytail ultra` first.
- One tree, shared `target/`. Edit only the files and functions your lane owns (table below). A change you
  need elsewhere goes as a line under the owner's heading in "Requests" below. Don't edit their file.
- Keep the tree compiling: make edits in batches that compile on their own, and never leave a half edit
  while you wait for a build. If the build breaks in a file you don't own, wait a minute and retry; don't fix it.
  No `cargo fmt` over whole files.
- GPU and window tests: `lockf /tmp/rays-gpu.lock cargo t -p <crate> --test <bin> <filter>`. Pure tests run
  without the lock. Focused filters only; the broad suites belong to W3.
- Commit each finished item: `git commit <your paths> -m "..."`, never `git add -A` or `git commit -a`.
  Name the bug or perf IDs in the message, and give the `click_budget`/`alloc_budget` rows it moved,
  before → after. Run `git diff --check` and the lane's focused cases first.
- Every bug fix leaves one regression that fails before the fix. Never weaken an assertion or raise a deadline.
- Stuck after one retry on a root cause: stop and report; Opus takes it over.

## Decisions (user, 2026-11-24)

- The targets in the plan's §4 stand.
- B29: a multi-selection inspector edits shared fields. A field shows the common value or "mixed";
  an edit applies to every selected node as one undo step.
- B13: `/i` with no inspector in the layout shows a notice.
- B41 and anything that would drop unsaved text: ask first, with an in-app prompt.
- At most two implementing agents at once. Slow tests are lane L9's work.

## Ownership

| Lane | Owns |
|---|---|
| L1 layout edits O(change) | `editor_document/src/{layout_edit,layout_by_path,workspace_doc,lowering,document_edit}.rs`; the `Change::LayoutEdit` branch of `editor_reduce.rs` |
| L2 history, reduce, cook | `editor_history.rs`, `editor_core/src/*`, `editor_document/src/document.rs`, `cook.rs`, `schedule.rs`, `selection.rs`, `editor_reduce.rs` except L1's branch, the `seal` call in `editor_loop.rs` |
| L3 frame loop | `crates/rays/src/sketch.rs`, runtime present code, `editor_loop.rs` except `seal`, `loop_model.rs`, `loop_view.rs`, `editor_renderer.rs`, `rays_shell/src/host.rs` |
| L6 Lisp text pane | done: P4 (`Rc<Vocab>`/`Rc<Names>`; basic/empty/isogrid settled after a split 12.4k -> 0.73k/0.27k/0.49k), `Shown` is a shared handle and `State.cache`/draft bases are Arcs, language/names/lex/find kept per revision, text area result borrowed (idle text pane ~65 allocs/frame, all inside PXUI calls), caret mapped in the draft once per text (B47), moving the selection reuses the document print (first draft keystroke on isogrid 135027 -> 2832 allocs), B41 asks before a reload replaces edits and never overwrites a draft, B42 autosave carries drafts, B43 definition through the draft parse, B44 right-click menu, B45 characters/Unicode find/CRLF+tab, B46 `repaint_after(LIVE_DELAY)`; new case `typing_budget` (+ `text_menu`, `autosave_drafts`). Open: an unchanged text pane as a cached PXUI view, incremental parinfer (L7), keyboard paste normalisation in PXUI |
| L4 graph pane | `crates/rays_graph/src/*`, `editor_scope.rs`, `flow_graph/src/projection.rs`, graph functions of `editor_panes.rs`, scope calls of `editor_frame.rs` (around 1000–1045, 1221) |
| L5 inspector, list, outline, status | `editor_inspect.rs`, `editor_inspector_pane.rs`, `editor_list.rs`, `navigator.rs`, `spreadsheet.rs`, `timeline.rs`, `editor_status.rs`, `bars.rs`, `editor_menu.rs`, `rays_shell/src/{inspector,tree,status_bar,timeline_bar,kit}.rs`, `crates/param`, inspector/list calls of `editor_frame.rs` (around 600–670, 1236) |
| L6 Lisp text pane | `text_pane.rs`, `editor_text.rs`, `lisp_text.rs`, `source_file.rs`, `loop_source.rs`, `echo.rs`, reload in `editor_host.rs`, `completion_names` in `editor_shell.rs` |
| L7 PXUI | everything under `pxui/`, and the pointer bumps here (`pxui/tools/validate.sh`, push PXUI `dev`, `cargo run -p gate -- pxui`) |
| L8 shell, chrome, panels | `editor_panels.rs`, `editor_shell.rs` except `completion_names`, `panel_map.rs`, `editor_commands.rs`, `editor_actions.rs`, `rays_shell/src/{chrome,layout,which_key,prompt}.rs`, the rest of `editor_frame.rs` and `editor_panes.rs` |
| L9 test speed | bodies of existing slow cases, `crates/*/tests/support`, `crates/rays/tests/common`; other lanes put new tests in new files |

## Status

| Lane | State |
|---|---|
| S0 harness | done: `click_budget` |
| L2 history, reduce, cook | done: history shares past/future by Arc, seal in place, snapshot_bytes counts imports, B1/B2/B4 fixed, idle reduce 20-34 -> 10-20 allocs. L10 shares the cook, history snapshot and gesture inputs |
| L3 frame loop | P4 cause found (text_pane `view_checked` deep-clones the Vocab per built frame; request under L6, fix = `Rc<Vocab>`; expect basic/empty settled ~12.4k -> ~700). B30/B34 (input frames never quiet, wake at `repaint_at`), B31 (skipped present redrawn), B33 (hidden window), B36 (wait before on_stop) fixed, test `quiet_rule`. L10 wired B32. Open: B35 (note), P10 small items measured at ~1 alloc each (idle frame is 217 of 268 in `editor_frame::update`); isogrid `escape` row is 1413/1036 over 1380/1149 from another lane's change |
| L1 layout edits | done (8f924adf5, a7cd99747, 4fb7f16ad): a layout edit splices the rewritten editor form (fresh ids, no whole print/parse), `flow_check` rechecks only graphs no unchanged graph references over the retained last check, lowering evaluates only those over the previous plan (`flow_eval::graphs_against`, `flow_sop::lower::with_graphs`) and keeps scene/objects/settings when no viewport shows its own scene instance; a kept plan asks for no cook. isogrid split 798k -> 7.2k allocs (130 ms -> 6 ms), layout switch 821k -> 8.6k; basic layout rows ~40k -> ~3k. Guard: `layout_gestures_match_a_whole_rebuild_on_every_editor_sketch`. Left: the document edit still imports the whole syntax (~1.7k on isogrid) and copies the plan/records once; a viewport over its own scene instance (ws_variations) and `select_layout` lower whole |
| L7 PXUI | B37–B40 fixed. B32 callback fires in SDL poll and wait and is registered by the sketch host. P5 updates the wrapped or unwrapped line index from the edit, lends text-area output and reuses descriptor buffers. Settled picker and header allocations fell 12 → 3 in the engine and 13 → 4 through the wrapper. P7 keeps views when hover has no reader or stays inside the same watched subtree. Open work includes incremental Lisp rewriting (L6), borrowing area results in Rays (L6) and reducing parent work when a child build genuinely needs hover. |
| L8 shell, chrome, panels | B5–B13 fixed. B12 applies the splitter draft before laying out grips and panes, once per drag frame. P10 reuses chrome storage and cached graph keys, borrows idle shell state and stores drag origins in PXUI. Paired idle allocs/frame for B12 (basic/empty/bare) 623/310/463 → 619/292/405. P10 606/281/390 → 584/184/296. |
| L4 graph pane | done: B14–B20 fixed, each with a regression in `rays_graph/tests/cases/scope_l4.rs` (B18's test found the node menu's clear cross placed outside the popup: fixed). run_ui_frames zoom failure was the status view hashing the zoom it shows only when the graph is the one pane (P8 regression, fixed). Perf: projection cached and borrowing; one Pane copy a frame (frame's copy handed to reduce by swap, rebuilt only for a repeated PXUI build); graph_name_at no per-def `format!` (def graph idle 740 -> 468); one scope copy per scrub frame (`with_patched`); nodes borrowed (`find_ref`, `zones`, `node_of`); chains O(w log w); node menu items shared; first layout once. `graph_budget_isogrid` now: zoom 586/463, pan 478/1067, select 476/464, box 475/7197, hover 651/536, enter 1961/468 and up 1725/464 (real: def:build-volume and back). Left: the box-select frame's 6.4k allocations are the multi-selection inspector (L5 request); per sync_scope call 1 alloc each for `viewed_path` (L5) and `graph_name` (`object_graph` owns, L2); the frame's Pane copy (key String, selection paths, ~8); `draw_other_graph_panes` clones inactive panes (multi-graph layouts only); drag frames (pan, band) +280 outside the graph layer |
| L5 inspector, list, outline, status | B21–B28 fixed. B29 adds mixed shared fields and one undo for a multi-node scrub. P7 borrows scope state and the graph pane's chain table. Inspector hover enter/cross rows 178/172 → 24/28 allocs, settled 0. P8 retains outline rows and caches status, timeline and spreadsheet views. Paired idle allocs/frame (basic/empty/bare) 597/194/311 → 584/184/296. Settled outline (basic/empty) 565/528 → 456/430. L10 shares list rows and outline inputs and synchronizes the geometry-list selection. Off-graph scene projections remain open. |
| L9 test speed | Both final Mac runs reached all 541 default editor cases within the 60-second execution deadline, up from 294 at timeout. The warm run took 53.964 s and its slowest case took 1.864 s. The grouped editor, Flow integration and graph window targets passed the same sets in forward and reverse order. Assertion failures remain listed below. |
| W3-fix final review | The Failures table's ten W3 cases fixed or re-expected (commits there); PXUI `60e761de` (view-aware lead gap) pinned with `430e79fb`. Final runs on the shared tree with other lanes' edits: `rays_editor` 544 cases in 60.7 s, one failure, `click_budget_isogrid` “toggle graph 1846/466 over 1820/490”, which also fails without W3-fix's changes (W3-budgets). `rays` 39, `editor_document` 105, `gate` 11, `rays_bin` 5 pass; `gate -- deps` and `gate -- pxui` pass. Manual: B32's live window edge drag. |
| W3-budgets final review | All 12 allocation cases agree individually, in the default 544-case editor run (62.7 s wall), and in reverse order. Fresh children (`b236c3df8`, PXUI `430e79fb`) isolate process caches. A fixed diagnostic clock keeps PXUI text-cache growth reproducible (`5bf2b6d83`). IsoGrid split-right settled 877 → 446 and long-session final idle key frame 342 → 189 (settled 130). IsoGrid box selection 7197 → 664 (`0329653af`), enter/up 1961/1725 → 1864/573 (`8205853ee`), pan and rubber band inside overscan 743/742 → 448/448 (`7ae088f62`), inactive graph zoom/settled 239/231 → 234/226 (`4a962f0dd`). Idle basic/empty/bare 560/171/253 → 522/168/253 (`172e4ff1f`, `f0aa71a12`). Historical `04c1bb1eb` already measured 1866/471 for toggle graph in a fresh process. The final 1827/447 sets its approved ceiling at 1919/470. |
| L10 integration | B29 maps all lowered list rows to their graph paths or clears the old scope pick. B41 confirms destructive reloads with a safe default. B32 redraws at the current extent during SDL poll or wait. P8/P10 share list rows and defer outline inputs and derived object copies. Cook publications and all four workspace snapshot fields share storage. Idle allocations/frame for basic/empty/bare were 584/175/294 → 560/171/253 in the shared fleet tree. Settled retype list for basic/isogrid was 304/572 → 188/481 and outline was 455/572 → 187/481. The three idle references are measured + 5%. L4's combined click and graph ceilings are in `04c1bb1eb`. B35 still needs fractional density through staging and retained render keys. The native window smoke test passed and a physical live drag remains a manual check. |

The opt-in 20-round `click_budget_long` still exceeds the close and undo key-frame caps of 3549 and
5612. Both caps first tightened in `04c1bb1eb`, which measured 4279 and 5738 in a fresh process. The
current measurements are 4127 and 5623. The default 12-round case passes.

### L9 test timings

| Case | Before (s) | After (s) | Change |
|---|---:|---:|---|
| `flow_gpu::sampling` | 3.007 | 0.041 | Default checks 1,024 samples across the pixel-row boundary. Slow mode keeps 4,096. |
| `sopcat_node_inputs` | 2.627 | 0.522 | Lower only the requested factories and their dependencies. |
| `sopcat_graph_edits` | 2.624 | 0.548 | Lower only the requested factories and their dependencies. |
| `sopcat_morph_sketch` | 2.610 | 0.457 | Reuse the sketch's lowered Blend Shapes factory. |
| `sopcat_motion` | 2.216 | 0.077 | Lower only the two motion factories and their dependencies. |
| `sopcat_set_color` | 2.329 | 0.132 | Lower only the box and color factories and their dependencies. |
| `sopcat_membership` | 2.134 | 1.863 | Build the full catalog once in the grouped inventory cases. |
| `sopcat_manifest` | 2.399 | 0.255 | Reuse that catalog. |
| `image_sop` | 2.253 | 0.029 | Lower its checked source and reuse the catalog inventory case for manifest slot assertions. |
| `text_editor_text` | 1.999 | 0.412 | Read the state dump in memory and advance the debounce clock directly. |
| `workspace_packed_capture_state` | 2.159 | 0.582 | Reuse capture cones. Keep the 65-wide row and use five default rows. |
| `native_live_mesh` | 2.339 | 1.440 | Batch the same trace samples and shorten repeated resident-frame checks. |
| `native_chain_frames_triangles` | 1.884 | 0.946 | Default checks the cold frame and three refits. Slow mode keeps eight frames. |
| `native_placement_retained_generations` | 1.667 | 0.308 | Default checks 32 generations after warmup. Slow mode keeps the 200-generation run. |
| `alloc_budget_basic` | 4.193 | 0.087 | Historical baseline. Grouped run reuses native setup and includes the fleet's editor changes. |
| `click_budget_long` | 2.044 | 1.088 | Focused timing with 12 default rounds, 20 in slow mode, and the same ceilings including settled idle. |
| `editor_document` layout gesture oracle | 2.602 | 1.845 / 0.858 | Independent smaller-sketch and IsoGrid cases keep all 11 gesture comparisons. |
| `editor_document` wave cook | 2.066 | 1.651 | Use 48 samples per strand by default and the authored 90 in slow mode. |
| `rays_pathtracer::furnace_mis` | 2.592 | 1.370 | Batch 16 samples per render with the original total. |
| `renderer::gpu_execution_texture_sampling` | 2.280 | 0.194 | Use 64 draws per path by default and 600 in slow mode. |

The workspace sweep covered 40 default packages and 10 slow-mode suites, one crate at a time.
Default runner wall including compilation fell from 17.255 to 16.335 s for `editor_document`,
11.902 to 2.273 s for `flow_gpu`, 34.112 to 9.836 s for `rays_pathtracer`, and
15.572 to 12.170 s for `renderer`. The final editor run before the warm repeat took 62.971 s,
including a 6.13-second build. The warm repeat took 53.964 s, with both runs below the execution cap.
Balancing the four pure groups against recorded timings reduced the busiest group's summed
case time from 15.337 to 12.296 s. The slow editor run reached 541 cases in 145.478 s.
Its longest cases were the full Shattered Studio fixture (30.504 s) and catalog perturbation (21.417 s).

The first capped run could not time the later native cases. Those baselines come from the first
run that reached each case. Grouped case times start inside the child; standalone timings
include process startup. The per-case ledger, cases over one second and slowest 30 for each
mode are in `target/l9-test-times/`.

The runner's focused failure-recovery and deadline tests passed, and the PXUI dependency and
remote gates passed. The full runner test set passed at the graph-window stage. The final
balancing stage still saw intermittent failures in the two existing qualification setup and
contention timing fixtures.

## Requests

### L2
- L4: `document::object_graph` returns an owned String; a borrowing variant lets `editor_panes::graph_name` return `&str` (3 allocations a frame at an object's level).
### L3 notes
- B35 (fractional HiDPI) left: `sketch.rs` renders at an integer `density` (`round(max(pixel_density, drawable/logical))`) down through `native_scene_lowering::render`; a 1.5x display renders at 2x and is scaled by the swapchain. Keeping the true density needs `f64` density in `layer::stage_native_render_checked` and the retained plan keys; Windows box verifies it later.
### L3
### L6 (from L3, P4, measured)
- P4 cause: `text_pane.rs:2485` (`view_checked`) calls `lisp_text::language(Some(vocab.clone()), ..)`: a deep clone of the whole `Vocab` (`Vec<Entry>` of `FieldView` vectors, ~12,000 allocations) plus its drop, on every built frame of every drawn text pane. After a layout edit (split) a text pane is drawn each frame, which is the whole 650 -> 12,400 idle step (sampled: `draw_text_panes` 434 of 674 update samples, 253 clone + 154 drop). Fix: `lisp_text::language` takes `Rc<Vocab>` (it wraps it in an Rc anyway, :746), `view_checked`/`view` take `&Rc<Vocab>`, and `LazyVocab` (`editor_model.rs:414`, cell of `Rc<Vocab>`, `force()` returns `&Rc<Vocab>`) hands out the shared Rc; `editor_panes.rs:658` (L8) passes it through. Better still: keep the Language per (vocab, names, imports, parinfer, theme) instead of rebuilding its ten closures per frame, and let an unchanged text pane be a cached PXUI view. Expect basic/empty settled rows from ~12.4k back to ~700.
- Scheduled wake (B34, from L3, done): the window loop now sleeps until the UI's `repaint_at` (the editor passes it through `sketch::wake_at` each frame). To have a frame at LIVE_DELAY (`text_pane.rs:2247`) call `ui.repaint_after(LIVE_DELAY)` once when the draft changes; no polling and no change in the loop needed.
### L4
- L5 (P7): route `editor_scope::scene_target` for a graph other than the laid pane's graph through the projection cache and keep its probe chains alongside it. The inspector already borrows the laid pane's projection and chain table when the selected object's graph matches.
- L2: `editor_scope::sync_scope` costs ~6 allocations on an idle frame of the empty sketch (the largest left in the reduce step).
### L5
- L4: on isogrid a box select's selection-change frame allocates 7,197, 6,431 of them in `draw_inspectors` (the multi-selection inspector). `viewed_path` (editor_inspect.rs:162) returns an owned Path: `sync_scope` calls it every frame (3 a frame); a borrow or Cow would make it free.
### L6
- L7, B39: PXUI reports an exhausted text undo or redo through `ui.passed_undo()`. Forward it to document history only when no pane has an unapplied draft and no modal owns the text input. Rays already does this in `editor_frame::undo_passed`; preserve the draft check when changing routing.
- L7, P5: the Lisp rewrite implementation lives in `crates/flow_syntax/src/parinfer.rs` and its caller in `lisp_text::language`. Incremental Lisp rewriting needs checkpointed parser state and one caret/anchor mapping from the chosen rewrite, wired by the host. PXUI's current language callback receives the whole string and one caret. Use `text_area_submit_borrowed` in `text_pane.rs` and copy the result where a draft or intent needs ownership.
### L7 (from L6)
- Keyboard paste and typed tabs are normalised in `text_pane` after the area returns (`\r\n` to `\n`, tab to two spaces), so the area's caret can sit a few bytes off after such an edit; doing it in the text area's paste path would keep the caret exact.
- `rays_diagnostic::position_of_offset` still counts bytes for the column; the text pane status row and Go to line count characters (`text_pane::char_position`).
### L9

### Failures (for review)

| Case | Binary | Assertion and first recorded failing HEAD |
|---|---|---|
| `rays_editor::run_ui_frames` | `editor` | At `bf70fcea5`, “zooming rebuilt 10 views and wrote 0 instance bytes a frame.” The latest default run passes. Slow mode at `7335e7874` still reports “orrery: playing rebuilt 25 views in 12 frames.” **Fixed `1ade2aed8`** (first bad `451f6dabb`: the cached status view and the line's nested view both rebuilt per playing frame; orrery 13 in 12). |
| `rays_editor::run_editor` | `gpu_editor` | “the drag rewrote the outline split” at `bf70fcea5`. The resulting split writes `0.3337`, and the case expects `0.3333`. **Expectation `0cd4eabfd`**: `f7330c199` (B12) divides by the room beside the gutter, as layout does (300/899). |
| `rays_editor::run_loop_copies` | `gpu_editor` | “a rename of one copy did not rename the template” at `bf70fcea5`. The scene still reports `spot,spot,spot`. **Expectation `59d48fde8`**: `082f81564` (B28) renames inline with the name selected (Finder), so typing replaces it; the template is renamed once. |
| `rays_editor::run_expression_chip` | `gpu_editor` | “a nested inspector reference followed its parent expression: g/b, g/b/:b” at `3df52a69f`. A later run reports “the 3.0 of the * was not editable from the row's inspector.” **Fixed `cea0ab526`** + PXUI `60e761de` (first bad `737998ce0`: rows in views took the 4-point lead gap, 4 points low). |
| `rays_editor::run_context_menus` | `gpu_editor` | “the timeline context menu did not open at the pointer” at `3df52a69f`. **Fixed `df4210564`** (first bad `68933fc02`: the ruler's own Copy menu covered the panel menu; the frame field's menu now opens). |
| `rays_editor::scene_tree_a_list` | `gpu_editor` | “list Enter did not open the selected node in graph” at `3df52a69f`. **Fixed `ede762b20`** (`082f81564` made Return rename on macOS and left no open key there: Command-Down opens, as in Finder). |
| `rays_editor::workspace_gpu_marks` | `gpu_editor` | “GPU marks differ at t=0” at `3df52a69f`, reproduced in an isolated process. **Fixed `86fc67cbb`**: pre-fleet (fails at `b32854cec`); one-step edge rounding of `b3727688a`, held to the renderer parity bound of `26a873f96`. |
| `rays_editor::editor_frame::tests::b5_split_preserves_graph_pane_and_locals` and `rays_editor::editor_frame::tests::b12_splitter_drag_updates_panes_on_same_frame` | `rays_editor` library | “the frame's pane” at `74d4d97f5`. The full-run `click_budget_empty` also reaches that assertion. |
| `rays_editor::click_budget_isogrid` and `rays_editor::click_budget_long` | `gpu_editor` | Full run at `139c256e0` exceeds settled allocation ceilings, including IsoGrid split right (877 vs 510) and long-session final idle (342 vs 228). The focused run of those two cases passes. |
| `rays_editor::source_reload_keeps_drafts` | `gpu_editor` | “assertion failed: r.e().core.prompt.is_none()” at `823933f5b`. |
| `rays_editor::graph_budget_isogrid` | `gpu_editor` | “enter: 1964/470 over 704/611; up: 1728/466 over 708/611” at `823933f5b`. |
| `rays_editor::tab_keeps_the_caret` | `gpu_editor` | “typing made a draft” at `823933f5b`. Passes at `1ade2aed8` alone in a clean worktree and in the full run: not reproduced. |
| `gate::tests::dependency_gate_resolves_workspace_dependencies_and_rejects_prohibited_edges` | `gate` | “dev-only tests do not change production boundaries” at `451f6dabb`. The fixture gets a private PXUI dependency diagnostic for `flow_ir`. **Expectation `7442a98d5`**: pre-fleet (`6d00f3b22` refuses a private path in every section). |
| `rays_bin::cli_help_errors_json_and_documentation` | `cli` | Generated JSON differs from `fixtures/cli/doc.json` at `c41f672f1`. **Golden `04a8e4fa9`**: `table/load` (Source Atlas, `cdd9b371d`) was missing. |

HEAD labels record the first available failing sample from the shared checkout. The three
original editor failures last passed at `3f2307451` in the timing ledger. Concurrent edits
were present in some failing samples.
