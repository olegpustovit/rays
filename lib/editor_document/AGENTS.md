# Private editor document boundary

This library owns the immutable document, sketch settings, the scene object and World layer
schemas, validation and presets. It is private to the Rays package and independent of PXUI, the
shell, graph presentation and `rays_editor`; `test/dependency_gate.ml` enforces
it. Selection, navigation, cooking and history policy belong to the host. Root rules apply.

## Where things live

| Module | Owns |
|---|---|
| `Workspace_doc`, `Layout_by_path` | The authored truth: source, checked workspace, layout by path, settings. `edit` applies one `Flow_edit.op` atomically and remaps layout keys |
| `Document` | The document: `workspace` (the text and its `Lower.t`), and what lowering derives (`scene`, `networks`, `shell`, `root`, `homes`, `view_worlds`, `view_roots`) |
| `Contexts` | The scene, world, settings and editor kinds generated from the schemas, and `of_workspace`, which builds a document |
| `Scene_sync` | Derived scene and World edits written back to the text |
| `Objects`, `Layers`, `Settings` | The schemas |
| `Preset` | The one persisted form: an s-expression `.rays` file, no version, no older reader |

## What must stay true

- **The text is the truth.** Never edit `scene`, `networks` or `shell` to change the document:
  edit the text and lower again. `Document.dump` is for crash reports and tests, not loadable.
- **Levels** are `Scene | Inside of int`. `Document.resolve_level` is total: a level that is gone
  is the scene.
- **Kinds come from schemas.** Add a field to a schema and it is a keyword; do not hand-write
  per-kind Lisp glue in `Contexts`. The catalog is kept for the last factories list (capacity 1).
- **Homes.** `Document.homes` says where each derived object is written: `Bound_at` a binding,
  `Inline_in` an argument (unfolded into a binding before an edit), `Copy` of a loop's template,
  `Looped`. Ids are claimed by home, then by operation and label, so a rename keeps the id.
- **A loop's copies are one template.** An edit of a literal field writes the template and every
  copy changes; a field the loop computes is refused with its expression; deleting a copy adds
  its iteration tuple to a `:skip`, so the other copies keep their homes and ids.
- **A scene graph is authoritative** for every object kind, the World included
  (a `scene/world` member):
  what it does not say is not there. Only a workspace with no such graph gets the host's camera,
  lights and one geometry object per `sop` graph; such an object has no text until
  its first explicit edit writes the graph from all derived objects (`adopt`). A camera following
  the viewport is not such an edit (`~adopt:false`).
- **Write-back.** What is asked of an object the text declares is written text first, the derived
  document never edited in between: `Scene_sync.set_fields` (one object's fields) and
  `Scene_sync.write` (an `edit`: flags, rename, delete, restack, preset, render camera, root,
  settings, reparent). `reconcile before after` is only for objects without text (it adopts them
  on the first explicit edit) and for the fields of a camera that follows the viewport
  (`~adopt:false`). A new kind of edit is a case of `edit`, never a derived edit for `reconcile`
  to diff, and never a third write-back.
- **Surgical edits.** A deleted or moved World layer is an edit of its binding: the graph's
  inputs, expressions, comments and names stay. A graph is written whole only when it gains
  layers it did not have. A name two objects would share is refused (`:parent` reads names).
- **Names.** A binding added to an existing graph is named by `Flow_edit.fresh_name`; several at
  once by `Flow_edit.fresh_among`.
- **The root.** `scene/root` is not a node of the scene network: `Contexts` reads it into
  `Document.root` and `homes.root`. The first edit of a render setting writes one. `E_SCENE_ROOT`,
  `E_SCENE_WORLD` and `E_SCENE_CAMERA` are raised while lowering, so a gesture that would cause
  one is refused whole.
- **The World.** `scene/world (ref g)` is an object like the others, its layers the world graph
  `g`.
- **Physical identity.** `of_workspace ~previous` keeps an unchanged scene network, object
  network and settings physically, so an edit elsewhere recomposes nothing. Find an object's
  graph with `Document.object_graph`, which does not rely on identity alone.
- **The shell.** `Contexts.editor` evaluates the editor graph named by `layout.editor` (default:
  the first) into `Document.shell`. Origins come from walking the checked terms, never from
  string search. An evaluation or tree error refuses the whole document. Panel disclosure and
  window bounds are `Layout_by_path.panels`.

## How to add things

- **A field of an object or layer**: add it to the schema in `Objects` or `Layers`; accept the
  `flow_manifest.sexp` diff.
- **A panel kind**: a `Flow.Workspace` op, a case in `Contexts.panel_tree`, a `Panels.panel`.
- **A derived edit**: a case in `Scene_sync` with a test in `lib/rays_editor/test/test_scene_sync.ml` that the
  saved text reloads to the same document.
