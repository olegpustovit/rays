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
| L2 history, reduce, cook | done: history shares past/future by Arc, seal in place, snapshot_bytes counts imports, B1/B2/B4 fixed, idle reduce 20-34 -> 10-20 allocs; open: Cook pieces/summaries/sources clone per idle frame (Rc), snapshots still clone `workspace_doc::T` (needs Arc fields, L1), per-gesture clones (document_edit, scene_sync, `kinds`, FrameResult rollback) |
| L3 frame loop | P4 cause found (text_pane `view_checked` deep-clones the Vocab per built frame; request under L6, fix = `Rc<Vocab>`; expect basic/empty settled ~12.4k -> ~700). B30/B34 (input frames never quiet, wake at `repaint_at`), B31 (skipped present redrawn), B33 (hidden window), B36 (wait before on_stop) fixed, test `quiet_rule`. Open: B32 (request L7), B35 (note), P10 small items measured at ~1 alloc each (idle frame is 217 of 268 in `editor_frame::update`); isogrid `escape` row is 1413/1036 over 1380/1149 from another lane's change |
| L1 layout edits | partial: shell Arc reused when unchanged. Layout edits stay O(document): isogrid split = 725k allocs (check 600k: print 132k + check 462k; lowering 116k in `flow_sop::lower::of_checked`, 7k the rest of `of_workspace`). Needs an incremental per-graph check (flow_check) and a plan patch for one graph (flow_sop/flow_eval); not reachable in the lane's files |
| L7 PXUI | B37–B40 fixed. B32 callback fires in SDL poll and wait, ready for L3 host registration. P5 updates the wrapped or unwrapped line index from the edit, lends text-area output and reuses descriptor buffers. Settled picker and header allocations fell 12 → 3 in the engine and 13 → 4 through the wrapper. P7 keeps views when hover has no reader or stays inside the same watched subtree. Open work includes incremental Lisp rewriting (L6), borrowing area results in Rays (L6) and reducing parent work when a child build genuinely needs hover. |

## Requests

### L1
- L2: `workspace_doc::T` fields (`source`, `extra`, `imports`, `import_sources`) as `Arc<..>` so a history snapshot shares them instead of deep-copying imports.
- L2: `document_edit.rs:24` copies the factory list per gesture.
### L2
### L3 notes
- B35 (fractional HiDPI) left: `sketch.rs` renders at an integer `density` (`round(max(pixel_density, drawable/logical))`) down through `native_scene_lowering::render`; a 1.5x display renders at 2x and is scaled by the swapchain. Keeping the true density needs `f64` density in `layer::stage_native_render_checked` and the retained plan keys; Windows box verifies it later.
### L3
### L6 (from L3, P4, measured)
- P4 cause: `text_pane.rs:2485` (`view_checked`) calls `lisp_text::language(Some(vocab.clone()), ..)`: a deep clone of the whole `Vocab` (`Vec<Entry>` of `FieldView` vectors, ~12,000 allocations) plus its drop, on every built frame of every drawn text pane. After a layout edit (split) a text pane is drawn each frame, which is the whole 650 -> 12,400 idle step (sampled: `draw_text_panes` 434 of 674 update samples, 253 clone + 154 drop). Fix: `lisp_text::language` takes `Rc<Vocab>` (it wraps it in an Rc anyway, :746), `view_checked`/`view` take `&Rc<Vocab>`, and `LazyVocab` (`editor_model.rs:414`, cell of `Rc<Vocab>`, `force()` returns `&Rc<Vocab>`) hands out the shared Rc; `editor_panes.rs:658` (L8) passes it through. Better still: keep the Language per (vocab, names, imports, parinfer, theme) instead of rebuilding its ten closures per frame, and let an unchanged text pane be a cached PXUI view. Expect basic/empty settled rows from ~12.4k back to ~700.
- Scheduled wake (B34, from L3, done): the window loop now sleeps until the UI's `repaint_at` (the editor passes it through `sketch::wake_at` each frame). To have a frame at LIVE_DELAY (`text_pane.rs:2247`) call `ui.repaint_after(LIVE_DELAY)` once when the draft changes; no polling and no change in the loop needed.
### L4
- L2: `editor_scope::sync_scope` costs ~6 allocations on an idle frame of the empty sketch (the largest left in the reduce step).
### L5
### L6
- L7, B39: PXUI reports an exhausted text undo or redo through `ui.passed_undo()`. Forward it to document history only when no pane has an unapplied draft and no modal owns the text input. Rays already does this in `editor_frame::undo_passed`; preserve the draft check when changing routing.
- L7, P5: the Lisp rewrite implementation lives in `crates/flow_syntax/src/parinfer.rs` and its caller in `lisp_text::language`. Incremental Lisp rewriting needs checkpointed parser state and one caret/anchor mapping from the chosen rewrite, wired by the host. PXUI's current language callback receives the whole string and one caret. Use `text_area_submit_borrowed` in `text_pane.rs` and copy the result where a draft or intent needs ownership.
### L7
- L3, B32 (live resize redraw): the sdl3 event-watch callback (`pxui/crates/open/sdl3/src/lib.rs` near :550) is never installed, so while the user drags a window edge SDL runs its own modal loop and `poll` is not called: the picture stretches until release. Install an `SDL_AddEventWatch` that, on a `WindowChange::Resized`/`PixelSizeChanged` event, pushes it and calls a registered redraw hook; `rays::sketch` then registers a hook that runs one `one_frame` (it can call `input_state::poll` and `scene::render` from the main thread, which is where SDL calls watches on macOS and Windows). L3 wires the hook as soon as the export exists.
- L7 to L3: `sdl3::event::set_live_resize_callback` now fires during both poll and wait. The resize event is queued after the callback, so redraw using the current window size and keep event draining in the normal loop. The SDL poll and wait regressions pass.
### L8
- L2: `editor_frame.rs:1340` copies the level's objects into every `FrameResult`; reduce now takes that copy, so the frame could build it only when a change is pending.
### L9
