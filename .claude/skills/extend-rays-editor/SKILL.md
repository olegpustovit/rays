---
name: extend-rays-editor
description: Extend the native Rays workspace editor with commands, widgets, panels or checked graph gestures.
---

# Extend Rays Editor

Read `lib/rays_editor/AGENTS.md`, `lib/pxui_graph/AGENTS.md`,
`specification/flow.md` and `specification/pxui.md` before editing.

1. Keep authored behavior in the `.rays` workspace. A new geometry node
   follows `add-sop` and starts as a Lisp `defnode`; do not add a typed
   OCaml sketch constructor or duplicate parameter defaults.
2. A command is a `Leader.keymap` entry with an `Editor_core.Command`
   identity, label, trigger and scope. Route its action through the host's
   existing update/reduce pipeline. Text focus and popups retain their
   existing key ownership.
3. A widget is a function over the shared `Pxui.Ui.box`, `signal` and
   `draw` APIs in the lowest library that owns its behavior. Build it in
   the host's one `Ui.frame`, return an intent and apply it afterwards.
   Use the existing theme, type, row sizes and marks.
4. A graph gesture is a `Flow_graph.Flow_edit.op` emitted by
   `Pxui_graph.Scope` and applied by the document reducer. Every language
   capability must project to the graph and be editable there. Do not
   add a second capture, text-entry, hit-test or renderer path.
5. Workspace text is authoritative for scene objects, world, settings and
   layout. Use checked syntax/document edits instead of shadow state.
   One gesture makes one labelled history entry; merge scrubs with
   `Gesture` and bursts with `Burst`.
6. Value dragging between panes uses `Ui.carry` and `Ui.drop_target`.
   Keep selection, focus, hover, pan and other view state outside document
   history. Close every editor and owned UI/resource handle a test creates.

Build `tools/check.exe` once and time focused aliases through
`_build/default/tools/check.exe`. Use the affected library aliases and
`@test/dependency_gate`; editor behavior can be driven without a window
through scripted frames and the existing test hooks. UI/rendering changes
also need the affected native lane and unchanged pixel goldens. For an
editor screenshot use `tools/ui_shot.exe FILE.rays /tmp/rays-shot.png`.
Review internal `.mli` changes directly. Reserve `check.exe --ship`
for the shipping check.
