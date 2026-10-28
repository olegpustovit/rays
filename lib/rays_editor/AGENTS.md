# lib/rays_editor rules

`rays_editor` is Rays Editor: it composes `pxui`, `pxui_shell`, `pxui_graph`, `editor_core`,
`editor_document` and `sop_catalog` into the SOP workspace. Nothing imports it.
`Rays_editor.Editor` is the one editor. Root repository rules apply.

## Where things live

| Module | Owns |
|---|---|
| `Rays_editor` (`rays_editor.mli`) | The public API: `Editor`, `Workspace` (a `.rays` file as a program), `Settings`, `Source`, `Renderer`, and the unstable test hooks `Private` and `Reduce` |
| `Environment_view` | Scene composition for visible, floating and hidden viewports and canvases, World bakes and hidden-scene cache |
| `Environment_model` | `Core` plus viewport state, creation, camera focus and prepared-object composition |
| `Environment_source` | Source opening, polling and saving; autosave and recovery ownership |
| `Environment` | Frame coordination, presentation, crash reports, resource shutdown and `run` |
| `Viewport`, `Renderer` | Camera objects and follow-viewport, guides and handles, raster / wireframe / traced slots |
| `Core_model` | Every type: the model `t`, `frame_result`, `change`, `carry` |
| `Core_shell` … `Core_host` | The model's functions, one module per concern; a module `open`s the ones it uses, nothing is included |
| `Core_reduce` | `reduce`: the one reduction of a frame (no UI) |
| `Core` | `update_frame`: routes keys, builds the panes inside `Ui.frame`, calls `reduce` |
| `Editor_document.Document_edit` | The document reducers: `syntax_edit`, `syntax_batch`, `text_edit`, `layout_edit`, `reconcile` |
| `Carry`, `Core_carry` | What a payload does at a place (`Carry.put`), and the carry's turn of a frame |
| `Cook`, `Schedule`, `Pick` | The cook worker boundary, when to cook, viewport provenance |
| `Text_pane`, `Lisp_text` | The text pane and the Lisp as `Pxui.Ui.language` |
| `Gate` | What the hosted sketch sees of a frame: the pointer only on its picture and only while the window is key (not hovered from another app, not the click that raised it), the keyboard only with a viewer or canvas in focus and no prompt, menu, leader or text field open; a held key stays hidden until released. Pure, table-tested in `gate_test` |
| `Navigator`, `Bars`, `Leader`, `Echo` | The Outline panel, header tools, the one keymap, a gesture in the words of the text |

The chain order is `Core_model`, `Core_shell`, `Core_inspect`, `Core_panes`, `Core_scope`, `Core_add`,
`Core_list`, `Core_setup`, `Core_status`, `Core_actions`, `Core_text`, `Core_panels`, `Core_menu`,
`Core_carry`, `Core_reduce`, `Core_host`, `Core`. A function goes in the first module that has what
it needs; a new module joins the chain and `private_modules` in `dune`.

The private environment chain is `Environment_view`, `Environment_model`,
`Environment_source`, `Environment`; each opens what it uses. All
frame effects and resource shutdown stay in the final coordinator.

## What must stay true

**State.** One immutable `Document` is the only thing `Editor_core.History` (128 entries)
snapshots; `Core.doc` is its present, except while a carry shows a preview. The workspace text is
the single truth: the scene, the networks and the shell tree are its lowering. Selection, the open
level, probes, panel-local state (`locals`, capacity 64), drafts and the viewport are view state:
never in the document, never in history.

**Start.** `Workspace.run` evaluates the workspace once: it builds the document
(`Contexts.of_workspace`), reads the window (`Contexts.document_window`) and the camera from it
and hands it to the editor (`Environment.run ?opened`, down to `Core.create`). `?opened` is only
ever that caller's document of the same workspace and factories, it is taken once, and nothing
reads it afterwards: an edit, a save and a reload lower the new text themselves. Do not add a
second evaluation to a start (`test/test_workspace_start` counts them) and do not turn the
hand-over into a cache.

**One frame.** `Core.update` is `carry_step`, then `update_frame`. Inside `Ui.frame` code only
builds boxes and returns values: every request of a pane is a field of `frame_result` (`changes`,
`scope_changes`, `tree_intents`, `text_intents`, `bar_action`, `view_pick`, `drops`, …). `reduce`
turns the result and the frame's actions into the next model. Never set a ref or the model inside
`Ui.frame`. The gutters' drag targets are built last (`Chrome.splitters`), over the panes.

**Edits.**
- A gesture on the text is a `Flow_graph.Flow_edit.op`: `Syntax_edit` (one op) or `Syntax_batch`
  (several, all or none). `Document_edit.syntax_edit` rewrites, checks and lowers atomically. Add syntax
  edits to `Flow_edit`, never to `Core`.
- A frame's gestures land together: the first one refused leaves none applied.
- The fields of a scene object or World layer the text declares are written text first
  (`Scene_sync.set_fields`: inspector rows, viewport handles, World drags), and so are the list's
  gestures (flags, rename, reparent, delete, restack), World keys and presets, the render camera,
  the root and the settings (`Scene_sync.write` with an `edit`; `Core_list.tree_edit` maps a list
  intent to one). Only an object the host made is edited as a derived node and adopted by
  `Document_edit.reconcile`, and only a camera following the viewport reconciles an object of the text.
- Every history entry has a label. One pointer gesture is one entry: a scrub merges by a
  `Gesture` key, a burst of keys by `Burst`. Viewport navigation with a camera that follows the
  viewport is view state: it amends the present (`Repair`) and leaves no entry.
- While a payload is carried only navigation runs; nothing reaches the history until the put
  (`install ~label:"Put"`), and cancelling restores the original document physically.

**Keys.** Keys become actions only through the one `Editor_core.Command.t` keymap
(`Leader.keymap`, `Pxui_graph.Scope.bindings`, `Pxui_shell.Tree.bindings`). Panes do not match `KeyPressed` for commands. A key that only means
something on one level or view is filtered in `Core.routed`. The text area's own chords (comment, move and duplicate lines, enclosing form, matching bracket) are
handled by `Ui.text_area`; `Leader.text_keys` lists their labels and triggers on the key sheet and is never in the keymap. A focused text field keeps the
keyboard except the global Command/Ctrl chords it has no use for. The graph pane's fields count as
text focus only while the pane is drawn; a pane that is not drawn is `Scope.suspend`ed.

**Panels.** The shell is `Editor_core.Panels.t`, evaluated from the workspace's editor graph into
`Document.shell`; a document without one uses the host's `?layout` until its first panel edit
writes it. Never store layout anywhere else. Every leaf is an instance with its own PXUI state
(keys seeded by its pane root) and its own model state in `locals`, by `Core.panel_key`. The pane
in use (`graph_pane`) follows the focus; the others wait in `graph_panes` and are read with
`Core.as_pane`, which resolves their level again. Never reduce a panel's intents against a pane
that is not in use, and never add a "first leaf of a kind" path. Layout gestures are `Flow_edit`
ops on the editor graph; disclosure and window bounds are `Layout_by_path.panels`.

**Files.** Command-S writes the source file only while its digest is the remembered one, else a
preset. Command-O (`Leader.Open_source`) opens the system dialog over the sketch's folder and
loads the chosen `.rays` into the window through the import-open path, which refuses while the
document has unsaved work. It applies text drafts first and refuses when one does not check. A changed source file
reloads as one entry; while the document has unsaved work or a draft the file's text waits and the
strip says so ("Reload sketch from its file" takes it). Autosave writes one recovery file per
sketch at 2 Hz and on close, never over an earlier session's file while the document is still the
one opened, and never a carry preview.

**Viewports.** Each viewport keeps its own orbit; handles and picks follow the
focused one. The viewport shows each object's graph result, or its transient lexical preview
request at the loop selectors. A preview never reaches the document or history. Viewports asking for the same picture
share a tracer; a traced slot at its cap with nothing changed does not render. Relative mouse and
fly mode end through `Viewport.release` on every path that stops navigating.

**Text.** The printed text and its span map (`Flow.Lisp.print`) are the only readers of the text:
never match strings. `Lisp_text` offers the checker's forms (`Flow.Workspace.special_forms`).
Text entry is `Ui.text_area` with `Lisp_text.language`. A Graph tab keeps one draft per graph. A draft is parsed once it has rested
(`Text_pane.live_delay`); diagnostics are tied to the text they were found in and shown on a later text
only while the text up to their line is unchanged. Command-Z past a draft's own undo steps never reaches
the document history.
Number drags use PXUI's pre-edit token range and the printed span map to emit
the card's `Set_arg`; an unapplied draft or stale source uses the existing
whole-text merge. Active token scrubs patch cached text and spans without
printing and restore canonical line breaks on release.

**Resources.** Every editor a test creates is closed (`Editor.close`): each holds worker domains.
Tests pass `~await:true` and count frames; they never wait on the clock for a cook. Caches have
capacities (64 converted meshes, 16 tracers, 64 panel states, 1 catalog).

## How to add things

- **A command**: one `Leader.command` entry with an id, a label, a trigger and, for a pane's own
  key, a scope; its action is a `Leader.action` handled in `reduce` or the environment. The key
  sheet and the palette list it from the table.
- **A text gesture**: a `Flow_edit.op` with its label and gesture key, emitted by a pane as
  `Syntax_edit`; test it in `test/test_workspace_edit.ml` and through the editor.
- **A pane request**: a field of `frame_result`, set where `build` returns, read in `reduce`.
- **A reducer test**: `Rays_editor.Reduce.step` runs `reduce` with no UI frame.
- **A panel kind**: a `Flow.Workspace` op, a case in `Shell_eval.panel_tree` and in
  `Editor_document.Panel_kinds`, a `Panels.panel`, a root in `build`.
- **Sketch state**: a `Param` schema passed as `~settings`; never a global ref.

Check with `dune build @lib/editor_core/runtest @lib/pxui_shell/runtest`, then the editor tests
(`@test_workspace_shell`, `@test_scene_sync`, `@test_text_pane`, `@test_workspace_source`,
`@test_rays_editor_logic`) under `SDL_VIDEODRIVER=dummy`. `specification/flow.md` is the design of
the graph pane, the layout forms and the carry.
