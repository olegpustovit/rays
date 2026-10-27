# Scene tree

Rays Editor navigates between the scene and each geometry object’s SOP
graph (the World has its layer stack instead). All of them are graphs of one
workspace text, each with its context (`sop`, `scene`, `world`, `material`, `settings`,
`editor`). `i` or a double-click follows a reference into the graph it names; `u` goes
back.

A graph, a list and a lisp panel show the same graph; `/ l` and a letter retypes the
focused panel (`flow.md` §8, §11.11).

## The scene graph: objects, merge, root

Normative. A workspace's scene graph (`:context scene`) has three kinds of node and one direction.

- `scene/geometry`, `scene/light`, `scene/camera` and `scene/world` return an object.
- `scene/merge` takes objects and scenes and returns a scene. Merges nest, and its one variadic
  port, in list order, is the composition of the scene.
- `scene/root scene :camera :renderer :width :height :max_spp :bounces :round_samples` takes one
  scene and returns a render. A render cannot be merged, so a root is always the last node.
  `:camera` is a slot: the camera object that renders (omitted: the first camera in merge order,
  else the viewport's orbit). `:renderer` is `"Raster"`, `"Wireframe"` or `"Path traced"`; the
  size is in pixels; `:bounces` and `:round_samples` are the path tracer's. The printer writes the
  keywords in that order (camera, renderer, size, samples).

A scene graph without a root is a *part*: other scenes take it with `(ref part)`, and a viewport that
shows it uses the default root (the schema defaults in `Objects.Root`). `Document.root` holds the
root's settings, the defaults for a part; `homes.root` is the root call, `None` for a part. The
first edit of a root setting (the Render section of the empty-selection inspector, the renderer
switch of a document with no authored renderer) writes `(scene/root ...)` over the graph's result
(only the settings that differ from the defaults) in one undo entry; later edits rewrite its
keywords, and Save round-trips them. The viewport reads the renderer, the resolution, `max_spp`,
`bounces` and `round_samples` from the root. A root in the text names the renderer and wins over
the viewport's preference and a sketch's `renderer` setting; a document without one keeps both.
`Editor.render_settings` is the root's size and samples.

Old files load unchanged: a graph without a root is a part, and `:width`, `:height` and `:max_spp`
on a `scene/camera` (the camera's former Render folder, no longer a field of it) are read as the
root's until the root says them. They stay in the text until it is edited; a root setting wins.

### The World is a merge member

`scene/world (ref sky) :name :exposure :rotation ...` is an object of the scene, as
`scene/geometry (ref shards)` is, and stands to a `:context world` graph as a geometry object to its
SOP graph. The world graph is the layer stack: its result is the top layer
(`(graph sky :context world (world/sun (world/sky :turbidity 3)))`, or `(world/none)` for no
layers); the World's other keywords (background, time of day, sun, ...) are the keywords of
`scene/world`. Deleting the object, hiding it and looping over it are the gestures of any object;
`/ e` selects the scene's World and enters it, creating one (written as a `scene/world`
member and a `world` graph) when the scene has none. `i` enters a geometry object or the World, `u`
leaves.

### What is refused

| Code | When |
|---|---|
| `E_SCENE_WORLD` | Two Worlds reach one scene. The message names both bindings. |
| `E_SCENE_ROOT` | A root is wired into a merge or into another root. |
| `E_SCENE_CAMERA` | The root's `:camera` is not a camera in the root's scene. |

Each is raised while the workspace is lowered, so a gesture that would cause one (adding a second
World) is refused whole and changes nothing.

### Composition gestures

One gesture is one undo entry (`Core.Syntax_batch`: all the rewrites or none).

| Gesture | Writes |
|---|---|
| `/ a` Geometry | a new SOP graph (a box), a `scene/geometry (ref it)` binding and one more merge input |
| `/ a` Geometry of... graph | the object and its merge input only; two objects share the graph and it cooks once (the second takes a distinct `:name`) |
| `/ a` World | a world graph (a sky and a sun), a `scene/world` binding and its merge input; refused when the scene has one |
| `/ e` World | enters the World; when none exists, writes the same world graph and member as `/ a` World in one `Add World` entry; reopening an existing World writes nothing |
| `/ a` Merge | with two or more objects selected, `Flow_edit.Group_merge`: a new merge between the selection and the old one; with none, an empty merge to wire |
| select wire, delete | the merge loses the input (`Disconnect`); the binding stays as an unwired node and wires back with an ordinary wire (the design drops a "+" stub for it: the prototype draws one, the editor does without) |
| `b` on a scene object | `:visible false` on its call (again, `:visible true`): one key takes an object out of the render without removing it, as `b` bypasses a SOP node. A node that can be bypassed keeps `b` as the bypass (`Scope_pane.hide_row`) |
| `Alt` `Up` / `Down` on a hovered merge input | `Move_item`: the input swaps with its neighbour, so the merge order (the list order) changes. Lists and strings move the same way (`Projection.reorderable`) |
| delete an object | the object and its merge input; its SOP or world graph goes in the same undo entry when nothing else reads it (`(ref g)` or a `(ui/graph "g")` panel) |

## Objects are nodes

The scene is itself an `Edit_graph`: every object is a node, input 0 is its
parent, and parameters hold its transform and kind settings. The graph view,
the list, the inspector, translate handles, copy/paste/duplicate, presets,
and undo therefore work on objects without a second code path. The scene
network is presentation only and never cooks.

| Kind | Operation | Parameters | Network |
|---|---|---|---|
| Geometry | `geometry` | translate, rotate (degrees, applied Z·Y·X), scale, visible, renderable | a SOP network |
| Light | `light` | type (area, point, spot, directional), translate, target, color, intensity, width/height, cone, visible, renderable | none |
| Camera | `camera` | eye, target, up, fov, near/far, follow viewport, aperture, focus distance (0: the target) | none; never parented |
| World | `world` | background, rotation, exposure, time of day, day cycle, latitude, day of year, sun linking | its layer stack |

The root is not a node of the scene network: its settings are `Document.root`. The Camera's render
resolution and samples belong to the root.

`Document.t` holds the scene network, one network per geometry object and
per World (keyed by object id), the active camera object, the root's render
settings, and the sketch `Settings`. History snapshots it; the open level, selection, projection
(graph, list, or text), and the map view are view state. A deleted object's
network goes with it; a pasted object copies its source's network.

An empty scene or SOP graph has no displayed node (a SOP graph's result is `nil`).
Deleting every SOP clears that object's preview, including a late cook from
before deletion. Disconnected nonempty SOPs stay editable; a compile error
retains the last successful preview with an error. Editor preserves an empty
scene without adding a camera.

A preset load parses, checks and lowers the text.
Rejected loads leave the installed document, history, camera and preview
unchanged. The same serializer writes the crash report's `document.rays`. Document owns no
PXUI state; the graph pane's layout is `Editor_document.Layout_by_path`, keyed by path.

A workspace's scene graph is authoritative: it replaces the host's camera and lights, and
an empty one means none. Presets are workspace documents (s-expressions, see `api.md`).

## Levels and keys

The scene and the World open as lists, a SOP network as a graph; a graph panel's
`:view` keyword and `/ l` choose the view, and each panel keeps its own. The Lisp
pane shows and edits the text of any graph, the scene's and the World's included.
Inside the World the view pane
opens on the lat-long map and `/ m` flips it between the map and 3D. `i`, a double-click on a tile or row, or the row
menu's Enter open a geometry object or the World. On a camera, Enter selects it
as the active render camera and enables look-through; entering it again keeps
look-through enabled. `u` goes back up (both
from any pane). `/ e` selects the World and opens it, creating it on first use as a
daylight sky with a sun. `/ a` opens the add menu of the open level
(objects, SOPs, or World layers) in list and graph alike: a search field over the kinds,
the likeliest first; typing searches everything. Leader sequences are
`Editor_core.Keymap.Leader` strings; which-key shows one page per typed
prefix. In the view, `w`/`e`/`r` show translate, rotate, or scale handles on
the selected node's xyz triples and Escape hides them so drags only orbit; a
selected camera also gets a handle sliding its target along the view
direction (its focus distance). Camera guides (frustum, aim line, eye marker)
are drawn over the view for every camera the view is not looking
through; clicking the eye marker selects that camera. `Editor.view_camera`
is what the pane shows, for sketch renderers: the render camera through
look-through, else the free viewport carrying the ACTIVE camera's lens. The
Viewport section pairs the look-through toggle with "Camera follows
viewport" (the ACTIVE camera's parameter, one undo entry) so a fixed camera
is set up by looking through it and orbiting. Looking through, the render
fills `Editor.film`: the root's aspect (its render resolution, the gate) fitted
into the pane; otherwise the whole pane, whatever draws it. Solid, Wire and Traced are the
same camera in the same rectangle: a path-traced free view is traced at the pane's own drawable
pixels and replaces the raster picture in place, and only looking through the camera shows the
gate (the tracer's film is then the root's resolution in the step that fits it). The editor
paints its 3D view, guides and handles inside that rect (the axis gizmo keeps the pane's corner),
and every mark over a view is clipped to its pane. "Render / save PNG" captures the screen.
List rows carry a kind badge (G geometry, L light, C camera, W world, ✦ sun,
· layer, S SOP); the status bar names the open level's keys.

The list (`Pxui_shell.Tree`) follows WAI-ARIA tree keys: arrows move the
focus and select, Shift extends, Left/Right fold or go to parent/child,
Home/End, Tab/Shift-Tab reparent under the row above or up a level (keeping
the world transform), Alt-arrows reorder, F2 renames, `/` filters (keeping
ancestors), `h` hides, `f` reveals. Rows drag with a 4-point dead zone onto
before/inside/after zones; toggle columns (`vis`, `rnd`; `disp` in a SOP
list; `vis` for layers) paint when dragged and apply to the whole selection
when the pressed row is selected. Right-click acts on the row, or the
selection when the row is in it, and never changes the selection. New rows
open in rename mode.

A SOP list follows each node's first input; other inputs nest under their
consumer, and a node used twice repeats as a muted `↳` row. Scene siblings list by their canvas y position.
Reordering swaps the affected siblings' positions and saves both in history.

Inside the World (graph pane focused): `t` flips the selected emitter
between dome and light, `n` reseeds, `[`/`]` step the time of day, `d`
plays the day cycle, and `1`–`4` load the presets. Dragging on the lat-long
map moves the selected layer; with the Sun layer selected it places the sun
(unlinking it from the time of day). Shift-drag in the 3D view turns the
World.

PXUI owns the whole pointer gesture. The World edit latches its operation,
stable node ID, and map bounds at the press; releasing Shift or crossing panes
does not redirect it. Ordered movement and the release position enter the same
undo entry. Map coordinates clamp while captured, and popup, pointer, and
focus cancellation end the edit. World events are consumed before camera
navigation; they do not also orbit or pan the view.

## Cooking and drawing

Every visible geometry object's display node cooks in one bounded job
(`Async_cook.submit_all`); an object whose compiled graph and settings are
physically unchanged keeps its prepared value when the projection of its
declared context dependencies also matches. Time/Frame-dependent outputs
prepare again as the timeline changes. Each piece carries the submission's
settings and context projection; publication does not stamp current settings
onto an older result. Settings changes invalidate every object, including
settings whose schema only declares draw effects. Empty object sets submit
an empty request to supersede previous work. The sketch's `scene3` draws
each object, cached per prepared value, and the editor places the drawings
at their world transforms (`Scene.D3.nodes` under `Scene.D3.transform`), so
moving, parenting, hiding, or re-lighting objects never re-cooks SOPs. Light
objects light the scene when it has any; otherwise the first object's own
Scene3 lights do. While inside an object the others draw ghosted
(screen-blended, no depth writes) and are not editable. `Editor.objects`,
`lights`, and `world` hand the same scene to a sketch's own renderer (the
voxel wall's path tracer).

The World bakes each frame it changes (`World.bake_cached`, capacity 4):
512×256 while a gesture or the day cycle runs, 2048×1024 when idle. The
day cycle advances the time of day by `day cycle` hours per timeline second,
so playback and `Sketch.export` with `Fixed dt` are deterministic. See
`environment.md` for the World itself.

## Deliberate limits

- Two levels only; cameras cannot be parented.
- Ghosting is a blend, not a material override.
- Reparenting drops shear (a non-uniformly scaled parent).
- A scene has one World (`E_SCENE_WORLD`). A viewport over another scene instance renders as that
  instance's own root (`Contexts.instance_root`: renderer, resolution, `max_spp`, bounces, round
  samples, and the camera object its `:camera` names; `Document.view_roots`, `Core.view_root_opt`,
  `Renderer.setting` per viewport slot, so a raster and a path-traced viewport can sit side by side)
  and shows that instance's own World (`Document.view_worlds`: the node and the layers of the
  `scene/world` its merge holds, none when it holds none). Only the document's own scene (the first
  scene graph) has scene objects in the list; the instance of a viewport is shown, not edited, so
  its root and World are edited in their graph. `Viewport` reads everything per key: `film`,
  `view_camera` and `render` take the viewport's key and read that key's root (a viewport over a
  part, or over the document's own scene, reads `Document.root` and the ACTIVE camera, and a root
  that names no camera falls back to the document's). Looking through, each viewport sees through
  its own root's camera (`Editor.viewport_camera`). `Environment` threads the key through
  `camera_of`, `paint_view` and the render call.

### The path tracer's film, budget and sample cap

- A traced viewport that is not looking through the render camera shows its own free view, in
  place of the raster picture: its film is its pane in drawable pixels (step 1), so a resize of
  the pane restarts the accumulation.  The steps below are for a viewport looking through the
  camera, whose rectangle is the gate.
- The film of a traced viewport looking through its camera is the root's resolution divided by 1, 2, 4 or 8 (`Renderer.film`):
  the largest that does not exceed the viewport's gate in drawable pixels (the gate is the root's
  aspect fitted into the pane, in points, times the pixel scale; the eighth when even that is too
  big). A pane resize changes the film, and restarts the accumulation, only when it crosses a
  step; inside a step the same film is scaled to the gate. The header of the viewport shows
  `1600×900 ½  64/256 spp`: the root's resolution, the step and the samples against the cap.
  Before, the film was sized from the logical bounds, so a traced pane on a Retina screen rendered
  at half resolution and was scaled up.
- `max_spp` is read each frame by `Renderer.update` (the view's `cap`) and is not part of
  `Renderer.setting` (renderer, bounces, round samples): raising a cap continues the accumulation
  of the same tracer, while any change of the setting discards the slot's tracer.
- One tracer (slot) serves every viewport that asks for the same picture: the same scene, camera,
  film size and setting. Viewports over the very same instance with the same World compose one
  scene, so they can share. The sample budget: the focused viewport's slot renders every frame; of
  the other slots that want the GPU (their picture differs from what the tracer last had, or they
  are short of their cap) one takes a turn per frame, in rotation (`Renderer.next_turn`); a slot at
  its cap with an unchanged picture costs nothing. `Editor.slot` reports a slot's film, step,
  samples, cap and the viewports it serves.
- Not built: comparing two roots in one pane (a wipe between two slots, `:against`).
  `E_SCENE_ROOT` stays strict: a root is never merged or wired into another.
