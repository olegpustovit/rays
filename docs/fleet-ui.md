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
| L1 layout edits | partial: shell Arc reused when unchanged. Layout edits stay O(document): isogrid split = 725k allocs (check 600k: print 132k + check 462k; lowering 116k in `flow_sop::lower::of_checked`, 7k the rest of `of_workspace`). Needs an incremental per-graph check (flow_check) and a plan patch for one graph (flow_sop/flow_eval); not reachable in the lane's files |

## Requests

### L1
- L2: `workspace_doc::T` fields (`source`, `extra`, `imports`, `import_sources`) as `Arc<..>` so a history snapshot shares them instead of deep-copying imports.
- L2: `document_edit.rs:24` copies the factory list per gesture.
### L2
### L3
### L4
- L2: `editor_scope::sync_scope` costs ~6 allocations on an idle frame of the empty sketch (the largest left in the reduce step).
### L5
### L6
### L7
### L8
- L2: `editor_frame.rs:1340` copies the level's objects into every `FrameResult`; reduce now takes that copy, so the frame could build it only when a change is pending.
### L9
