# Zero warm allocations fleet (2026-11-25)

Plan with attribution: Microapps artifact `ty4g96mkx792` (`art read ty4g96mkx792`). Raw data, the scratch
probe (`tools/probe.patch`) and per-site stacks: `target/zero-alloc/` (`attribution.md`, `sites/catalog.tsv`).
Lanes Z0–Z7 are defined in the plan. This file is the working contract; delete it when the fleet closes.

## Gates

- Z0 adds the `zero_alloc` suite (scenarios A–F). Until it lands, measure with the existing
  `cargo t -p rays_editor --test gpu_editor budget` (click/graph/typing/alloc budgets) and the site data above.
- Allocation counts are exact; ceilings only go down (measured + 5% after an improvement; the end state is 0
  Rust allocations per warm frame, native ≤ 12 per rendered frame). Never raise one, never weaken an assertion.

## Rules

- AGENTS.md applies (and pxui/AGENTS.md for PXUI). One shared tree; edit only your lane's files (below). Need
  a change elsewhere: add a line under the owner's heading in "Requests". Keep the tree compiling: edits in
  compile-atomic batches; if it breaks in someone else's file, wait and retry.
- GPU/window tests: `lockf /tmp/rays-gpu.lock cargo t ...`. Use the `cargo t` alias, never raw `cargo test`.
- Commit each finished item: `git commit <your paths>`, never `git add -A`/`-a`. Message: lane, sites fixed,
  allocation rows before → after.
- Push validated work regularly: `git fetch origin && git merge origin/dev`, rerun your focused tests, normal
  push (never force). Rays dev must stay green.
- PXUI: commit in `pxui/` on a branch off the latest `origin/dev`, run `pxui/tools/validate.sh` (needs
  /opt/homebrew/bin on PATH; zero-allocation gates, A7 instruction counts and goldens must not regress), push to
  PXUI origin dev (fast-forward; on rejection fetch, rebase, revalidate), then bump the pointer here (`git add
  pxui`, `cargo run -p gate -- pxui`) and push Rays. Fetch PXUI often: other agents push to its dev too.
- Every fix keeps a regression (an allocation assertion counts).

## Ownership

| Lane | Owns |
|---|---|
| Z0 gate | `pxui/crates/open/mem_sys` (trace feature), `crates/mem`, `crates/rays_editor/tests/native_qualification/zero_alloc*.rs` + `tests/suites/natq.rs` hook, the `RAYS_ZERO_ALLOC` dev mode in `crates/rays/src/sketch.rs` |
| Z1 editor model | `editor_document/src/{kind_graph,objects}.rs`, `rays_editor/src/{loop_model,editor_shell,editor_frame,editor_reduce,editor_loop}.rs` (except `canvas_pictures`), `rays_shell/src/layout.rs`, `editor_core/src/*`, `crates/param`, `crates/rays/src/scene3.rs`, `viewport.rs` (except overlays) |
| Z2 graph pane | `crates/rays_graph/src/*`, `rays_editor/src/{editor_panes,editor_scope,editor_status}.rs`, `flow_graph/src/{probe,projection}.rs` |
| Z4 Lisp pane | `rays_editor/src/{lisp_text,text_pane,editor_text}.rs`, `flow_check/src/lisp_scan.rs`, `flow_syntax` lexer |
| Z3 overlays (W2) | `viewport.rs` overlay functions, `crates/scene_command` path code, `crates/rays/src/scene.rs` path |
| Z5 renderer (W2) | `crates/renderer`, `crates/rays` staging, PXUI open tier `ogpu_*`/`metal_sys` |
| Z7 evaluation (W2) | `crates/flow_eval`, `crates/flow_native`, `editor_loop::canvas_pictures` |
| Z6 animated canvas (W3) | renderer plan code, `flow_ir` draw lowering, canvas text (`crates/rays/src/font.rs`, PXUI text) |

## Status

| Lane | State |
|---|---|
| Z2 | done: edit_overlay (mark_input→graph-edit); LitCache Rc; pane.chains; selected `&[Path]`; graph_name_at `&str`. zero_alloc A_ISOSEL 712→51 (matches idle); regression via update_cached |
| Z0 | landed: mem_sys `trace` (PXUI 71931474), zero_alloc A–F (ceilings = measured+5%), RAYS_ZERO_ALLOC; cfg fix ae7f8ed36 |
| Z1 | Landed borrow sites: `find`/`ids`/`read` (cb1a86b52); Leaf/Panel walk, look/focus `&str`, composition_key Rc, Scene3 Rc nodes/lights, Extra render by-value, chord lists without Vec (545d5ffcf). click_budget isogrid idle 536/458→402/324, hover 469/469→334/334 (meas 382/308, 318/318). Remaining: Extra navigate clones, `panel_key` owned Path on rare paths, Finding 2 remainder is Z2 content rebuild + Z3 overlays (~300 settled still theirs). |
| Z4 | specials OnceLock; BoundDoc lazy; complete/rank borrow; colorize SPANS; Word unit; incremental lex (sketch equality); State take in one_intent. typing_budget 1712/782→1163/330 (meas 1107/314, mean 0.36ms). Left: shown() State clone via &ModelView (needs Z1 take), Live.seen String on Intent::Live, lex token Vec clone on return, PXUI cosmic-text (Z6), remaining ~500–1100 whole-step allocs outside lisp_text. |

## Requests

### Z0
### Z1
- Z2: `scope_state::selected` → `&[Path]`; call sites may need `.iter()`/`.to_vec()` (Z2 patched editor_frame/panels/inspect/loop/reduce for compile).
- Z2: `LitCache.scope` is `Rc<Scope>` in `editor_model.rs` (for lit_tags).
### Z2
### Z3
- Finding 2 remainder: viewport overlay re-tessellation on every input event with no scene change.
### Z4
- `text_pane::shown` still clones State on cache hit because `editor_frame::text_state` takes `&ModelView`; take/replace needs a `&mut` path in Z1's frame.
### Z5
### Z6
### Z7
