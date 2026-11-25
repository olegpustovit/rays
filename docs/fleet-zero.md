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

- **Since 2026-11-25 16:30: every lane works in its own worktree** (`git worktree add ../rays-<lane> origin/dev -b <lane>` + `git submodule update --init`; PXUI work on a branch off PXUI origin/dev inside it), rebases on origin/dev and pushes from there. **Never stash, reset, checkout or restore another lane's files**; the shared checkout is the coordinator's.

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
| Z4 | lisp_text/lex BoundDoc+OnceLock+SPANS+Word+incremental+Arc caches; State take (one_intent+shown+frame); Live moves. typing_budget→1103/260 (meas 1050/247, mean 0.60ms). zero_alloc D_typing 401 rust + 37 native/unit (was ~1031 app + ~49 pxui). Left: ~400 rust/key (scene/kind_graph/PXUI cosmic Z6); not yet 0.
| Z11 | in progress (`../rays-z11`). D **366→234** (ceil 246): parinfer Cow+scratch, PXUI rewrite None (cf568c3a), draft reuse, soft complete top-48, static tab labels, borrow draft text. typing_budget 830/128 (ceil 872/135). Left: lex/Token rebuild, editor_area chips/notes, PXUI cosmic reshape, live parse; ~70 frame/update_with → Z10.
| Z3 | done: Scratch tessellate (fef497e23); Wide keys + always-on shape cache + Affine unit segments (ae918f88f); guides cache + Affine lines + stack screen_box (cc870c828). zero_alloc A_isosel 712→48, B 727→196/194, C 467/462→174/170 (Z0 ceilings at meas+5%). click_budget isogrid idle 402/324→252/175, hover 334/334→185/185. Overlay tessellate/format! → 0 warm; pan reuses unit meshes. Left: guides hit clones outer Nodes Vec; camera-key miss still builds Node wrappers (tessellate 0). |
| Z9 | in progress (`../rays-z9`). B_hover **139/137→77/74** (ceil 81/78): cook skip; pointer keymap; view_bodies Rc; of_kind refs; `last_picture` borrow on PointerMoved. Residue: ~77 update (Extra×2, frame rebuild, compose_view on first hover), ~8 native (Z5). |
| Z8 | in progress (`../rays-z8`). Autosave splice+buffer; REST slide proved. A_empty/bare/isogrid/isosel **0** rust. Isogrid was thrashing one `frame_pass` between Scene3+depth and Scene2 — dual `frame_passes` + reused attach scratch. E_toggle 333 (ceil 365). B/C/D/E/A_basic/F → Z9–Z13. |
| Z13 | E settled **0/0** (was click 10 / toggle 24–34). Toggle settle waits out graph `repaint_after(0.1)` (idle 8 on settled only). Key: click **210** (ceil 221), toggle **319** (ceil 335). Residue: inspector/wires rebuild on selection; layout+recovery splice+after_present on `/g`. |
| Z5 | submit retained pool + warm-submit 0-alloc regression (`2ecaaf05`); phase_timer Arc+TLS recycle. SLOW empty/bare **2/8** (ceil 3/9); isogrid **13/12**. Renderer warm = 0 on empty. Left: ~2 editor scatter (follow_press / compose) → Z1; MetalHandle general pool optional. |
| Z6a | done: PXUI rekey-on-text-change + mono caret (85482d18); glyph atlas. Direct atlas quads enabled once Z6b Image-run batching landed (no per-frame blit). |
| Z12 | F **0/0** rust (was 101/102); A_idle_basic **218→208** (ceil 219). Live-stamp canvas reuse + may_rest (dt=0 / size-only). Draw locals+marks pool; Cook Rc reuse. A residue → Z5/Z6b (`ordered_items`, signature collect, prepare_upload for changing circle+frame text). |
| Z6b | done (`../rays-z6b`): finding 5 plan/ICB patch + prepared reuse; **Image-run batching** — consecutive same-texture `Command::Image`/Glyphs → one Scene2Textured draw (`affine[20k..]`, capacity-stable `image-run:{tex}:{cap}`). Canvas text `TextDraw::Atlas` (no per-frame blit). A_idle_basic **240/8** (ceil 252/9); F 122/7 + 128/8 (ceil 129/8, 135/9). Free-list ICB pool deferred. Remaining: Z5 ogpu encode; Z7 eval; Z8 editor; Z6b signature collect / argument-slice / ICB re-encode. |

## Requests

### Z0
### Z1
- Z2: `scope_state::selected` → `&[Path]`; call sites may need `.iter()`/`.to_vec()` (Z2 patched editor_frame/panels/inspect/loop/reduce for compile).
- Z2: `LitCache.scope` is `Rc<Scope>` in `editor_model.rs` (for lit_tags).
### Z2
### Z3
### Z4
- `text_pane::shown` still clones State on cache hit because `editor_frame::text_state` takes `&ModelView`; take/replace needs a `&mut` path in Z1's frame.
### Z5
- Idle empty **2** Rust are editor scatter (follow_press `path.cloned`, compose/host) — not ogpu. MetalHandle general pool still open.
- crates/lru: side_lru reserves HashMap; shared `lru` still node-allocates on cold insert (warm reserve holds).
### Z1
- Z5→Z8: idle empty/bare/isogrid/isosel **0** rust (REST slide + dual frame_passes).
### Z6
### Z6b
- Z12→Z6b: A_basic ~214 rust — `automatic_signature` collect + `prepare_upload` each animated frame (circle radius + frame text). Pool/reuse signature vec; keep upload scratch.
- Z12→Z5: `ordered_items::add` / `stage_native` ~6 allocs/frame on animated basic.
- Owns plan_entry/plan_batches/batch_resources_of/rebind, ICB pool, and Image-run (same-texture Image/Glyphs → one Scene2Textured draw). Coordinate with Z5 on shared helpers via Requests.
- Image-run + Atlas landed: A_idle_basic 240/8; window_resources text/atlas ownership + density checks green.
### Z7
