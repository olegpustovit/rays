# PXUI interaction and visual contract

PXUI is a wrapped sibling library. It depends on `rays`; the core library
does not depend on PXUI. Its compact creative-tool character is inspired by
[ofxUI](https://github.com/liquidzym/ofxUI), adapted to Rays's functional
scene and event model.

## Architecture: one box, one pass, one draw list

`Pxui.Ui` is the only UI engine. The panel kit, the parameter inspector
(`Pxui_shell.Inspector`), the workspace chrome (`pxui_shell`, `rays_editor`), and the graph canvas
(`pxui_graph`) all build boxes in the same `Ui` frame, share one pointer
capture, one focus, and one hit list, and paint into one instance list.

```ocaml
let update model frame =
  Pxui.Ui.frame model.ui frame @@ fun ui ->
  Pxui.Ui.panel ui "Motion" @@ fun () ->
  let animate = Pxui.Ui.toggle ui "Animate" model.animate in
  let name = Pxui.Ui.text_field ui "Name" model.name in
  if Pxui.Ui.button ui "Quit" then Sketch.quit ();
  { model with animate; name }

let view model _frame = scene_of model @ Pxui.Ui.scene model.ui
```

Widget values live in the sketch model; widgets return them. There are no
named change lists, value getters, or setters to keep in sync. `Ui.t` is a
mutable cache handle threaded through the model like `Assets`, released with
`Ui.destroy` from `Sketch.run_state ~on_stop`.

Each frame runs four steps:

1. **Route.** The frame's ordered events are resolved against the previous
   frame's hit list (paint order, clipped). The topmost box under a press
   becomes the single `active` box and captures the pointer until the
   matching release; clickable presses also update keyboard focus. Wheel
   steps go to the topmost `scroll` box under the pointer unless a
   `blocking` box covers it. Key, text, and IME events go to the focused box.
   One frame of input latency buys a single build pass.
   After the frame, `Ui.input ~owner` gives a viewport only pointer events
   routed to its exact root; children and unowned ground retain their events.
   Without an owner, unconsumed pointer events pass through. Unconsumed keys,
   focus loss and pointer cancellation retain their existing routing.
2. **Build.** User code creates boxes. A box key hashes its label (text after
   `##` is key-only, `###id` replaces the key) with the enclosing box, so
   state follows labels. Duplicate keys in one frame receive order-stable
   distinct keys. Per-key state — rectangles, scroll offsets, section
   state, text-editing buffers, double-click timing — lives
   in a retained cache: an open-addressing integer table into structure-of-
   arrays pools. A key not built for a frame is pruned.
3. **Layout.** A bottom-up pass computes `Px`, `Text`, and `Fit` sizes; a
   top-down pass resolves `Pct`, `Rel`, and `Grow`, reserves a 10-point
   gutter in overflowing scroll boxes, clamps scroll offsets, and positions
   flow and floating (`at`) children in logical screen points.
4. **Paint.** Boxes paint depth-first with nested clip rectangles, culling
   boxes outside their clip. Painters receive the final rectangle and emit
   `Scene.Private.Ui_batch` instances. `Ui.to_front ~order` raises a root in both
   painting and hit testing. The published `Ui.scene` contains native-only UI
   batches plus `Scene.text_input_region` metadata. `Ui.scene ~under` can insert
   a host Scene immediately before a raised root's UI batch, letting floating
   native viewports share the UI's paint order without another hit-test path.

## Renderer

A UI instance is 64 bytes: a quad plus a kind. One Metal pipeline family,
`Ui`, draws them with vertex pulling (four vertices per instance) and one
signed-distance fragment stage:

- **Rect.** Radius-0, non-anti-aliased rects rely on quad rasterization, so
  fills cover exactly the pixels a triangle rectangle covers under the
  top-left rule. A 1-point stroke is a separate band instance whose inner
  test biases the sample point by +10⁻³ toward +x/+y, reproducing the
  top-left rule on half-integer edges at every integer density. Rounded
  rects, circles, and borders on them use an anti-aliased rounded-box SDF.
- **Textured.** Glyphs and images sample one texture with nearest filtering.
  Instances carry texel coordinates, normalized by the texture's size in the
  fragment stage, so the atlas can grow without invalidating earlier
  instances.
- **Wire.** A cubic Bézier becomes 4–48 segment instances by length; the
  vertex stage evaluates the curve and strokes each chord as an anti-aliased
  capsule. Chords outside the batch clip are not emitted.
- **Grid.** One quad draws a dot every `spacing` points from an origin.

PXUI layout, hit bounds, glyph placement and input regions use logical screen
points directly. The kit has no per-box or per-batch scale or translation;
physical-pixel snapping uses only the frame's density.

`Ui_batch` groups instances by `{clip, texture}`; untextured
instances never split a batch. `Rays_execution.Private.lower_ui`
lowers each batch to one indexed draw with a 24-byte affine uniform (logical
screen points to clip space) and a logical scissor, which the runtime scales
to physical pixels exactly once. `Ui` draws use direct texture bindings, so
the executor gives them their own attachment class and they never share an
argument-buffer render pass with Scene2 draws. `Scene.Private.ui` layers,
like `view3d`, ignore enclosing Scene transforms and clips: the batch
carries its own screen coordinates and clip.

## Typography and the design kit

PXUI records the nearest hit ancestor of each box. A pane root's
`Ui.signal.subtree_press` reports the last press on that root or a nested
control, while `pressed` remains exclusive to the topmost box.
`Ui.last_press_within` reads that same hit tree before frame building so a
click and a scoped key in one event batch use the clicked pane.

`Pxui.Theme` is the design kit, revision 3. The reference is the "Rays UI Redesign" canvas
(foundations, widgets, workspace, overlays and one sheet per panel); `sketches/ws_layout` is the
layout it is compared with, and `tools/ui_shot.exe FILE.rays OUT.png` renders any workspace's
editor to a PNG without a window.

Tokens. The six base colours are unchanged (`panel` the ground, `foreground` ink, `control` the
current row, `input` the sheet, `track` the well, `accent` signal orange `#f0481f`); invalid is
magenta `#c2255c` and the port colours are unchanged. Derived: text is ink, `ink_2` (labels and
secondary text, 4.8:1 on the ground; `muted` is its old name) and `ink_3` (disabled, placeholder,
keys); lines are the foreground at 30, 15 and 8 percent (`border`, `edge`, `faint_border`: the one
outlined button and switches, panes and fields, inside a list); fills are `hover_fill`,
`pressed_fill` and `tint` (the accent at 16 percent: a selection in text, a dock target). Type has
four sizes, 40, 20, 13 and 11 (`display_size`, `title_size`, `font_size`, `label_size`); a label is
upper case with a point of tracking (`Ui.Paint.cap`). One unit, 24 points, is a row, a header, a
bar and the graph's grid pitch; a control is 20. Nothing has a radius, a shadow, a gradient or an
ink fill: separation comes from space first, a hairline second, a rectangle last.

Marks (`Ui.Paint`): corner `brackets` in the accent for the selected object (a node, a list row),
an accent underline for keyboard focus, a `cross`ed box for nothing displayed, a `hatch` for
bypassed or stale, a dot every 24 points and a register cross every 480 by 192 on the canvas,
`dashed` lines for scopes and drop targets, and the drawn `chevron`.

Widgets. A field is a value on an `edge` hairline (accent while it holds the keyboard, invalid
when refused); the 2-point `ink_2` line under a number is its position in the soft range. A
button is its text and, in `ink_3`, its key: hover and pressed fills, the `control` fill while
active, and one outlined primary button per panel (`Pxui_shell.Kit.button`). Tabs are text, the
one in use underlined (`Kit.segments`). A switch is 28 by 14 with an 8-point knob, accent when on.
A section is a label in `ink_3` with a chevron under 16 points of space (4 at the top of its
parent, none after a closed section). A list row is 24 points: hover is `faint_border`, the
keyboard cursor the `control` fill, the selection that fill inset by 4 with accent brackets. A
menu or a window is the `input` sheet with one hairline; a docked panel is the ground. The
scrollbar is a 4-point `border` thumb with no track.

Chrome and panels. The status strip reads, left to right: the document's file (its name when it has none), a dot for its state
(checked, cooking, refused), the status line, a hairline, the focused pane's context as a label
and each of its keys before what it does, then the layout in use and the frame rate
(`Pxui_shell.Status_bar`). The keys are the ones the pane's sheet lists, in its words (graph:
`Tab add after`, `o open`, `v view`, `b bypass`, `i enter`, `f frame`, `w hints`, `/ leader`; viewport:
move, rotate, scale, enter object, orbit; Lisp, Timeline and Inspector have none). A graph that is
the only docked pane shows `N nodes · M selected` and `ZOOM P%` on the right in place of the
layout and the frame rate; with floating windows the right side is `N FLOATING` alone. A header's
right end is laid out once (`Chrome.header_slots`): the tabs a pane stands there (a graph panel's
Graph / List / Text, then a text view's Selection / Graph / Document) keep their place, 8 apart,
before the collapse button; the subject is cut to the room that remains, losing its head first
(`… / sop`), then its tail with an ellipsis, then the label and the focus square whole; a group that does not
fit in the header is left out, the last one first, so a header never draws over
itself or past its pane. Text that is cut anywhere in the kit is cut by `Ui.ellipsis ~width ~limit`,
the one text fitter. A floating window's title row ends
with a chevron (dock) and a cross (close); a floating viewport is the white sheet with its picture
inset 12 points in a line-2 frame, the mode and camera as a label inside it. A value with a unit
(`[@sop.unit "deg"]` on the field, carried by `Param.field_view.unit`) shows it after the number
in `ink_3`; a vector row and a colour row are `Pxui_shell.Kit.vector` and `Kit.colour`. A docked timeline is one bar with the ruler at its end; a taller one
puts the ruler under the bar with numbered major ticks and the last frame, a field, at the bar's end. The
leader is a sheet along the bottom with its sections in columns; the key sheet has a filter
field. The Outline lists, under a search field and an add button, the scene with its objects
and their visible and render flags (a press on one writes that argument of the object's binding), geometry and materials with how many graphs use each, the
World, the layouts (a row switches to it), the active graph's inputs and its data flow. Over a
view the host draws, on patches of the ground, the render frame (a hairline with ink corner
brackets and its resolution) where it is not the whole pane, the focused object's accent
brackets and name, the render camera at the upper right (its name and its lens as a focal length on a 24 mm gate), the scene's size and the frame rate
at the lower right, and a traced view's samples with a 4-point progress bar at the upper left.
A window's panes are built on the `input` sheet (`Core.on_sheet`, `Scope.with_theme`).

The kit face is Pragmasevka (or `RAYS_UI_FONT`). Each `Ui.t` loads it once per logical
size, found from the working directory or the executable upward, caches a
failed load (falling back to the system face), and frees its faces in
`Ui.destroy`. Kit text defaults to 13 points and rows are `max 24 (size + 11)`, so 24 up to that
size; `Editor.create ~ui_text_size` overrides the editor's size (the tests set 11, the size their pointer
positions were written for); chrome and inspector text take the kit size, never a literal one.
A line of `Ui.text_area` is `Ui.text_line_height`, one and a half times the text (17 points at 11,
20 at 13), not a 24-point control row; its gutter holds label-size line numbers in `ink_3` behind
a `faint_border` rule, the caret's line is tinted with `faint_border`, an error line with the
invalid colour at 7 percent, and the bracket pair at the caret is outlined in the accent.
Compact string fields (including node names and notes) align left; compact numeric
fields align right.
The editor inspector uses zero outer panel padding. Its shared
`Ui.inspector_header` (the selected thing's name at the display size, once per panel),
`inspector_section`, `inspector_row`, `inspector_toggle`, `inspector_button`,
`inspector_readout`, and `inspector_message` keep the
selected node, empty selection, settings, viewport, camera, and render
contexts on one grid: 12 points, a 6-point slot for the pin dot (filled once the row is on the
card), the label in `ink_2`, the control, 12 points. Parameter rows are 24 points high; a label
too long for its column takes a 48-point row with the control underneath. A switch row gives the
label the row and puts the switch at its end.

Glyphs are rasterized by SDL_ttf exactly as whole strings were: each code
point is rendered at the backing density (`Font.Private.glyph`), packed
white-with-coverage into one shelf atlas, and placed at the font's pen
advance for that density with the run's origin snapped to a physical pixel.
The atlas uses `Image.upload_rgba`; a failed upload stays dirty for retry.
For the kit face this reproduces whole-string rendering pixel for pixel at
1×, 2×, and 3× (only the RGB of fully transparent pixels differs). Pair
kerning of proportional overrides is not applied.

Kit rows: 12 points at each side, a label column of 112 points (half of a narrow row), then
the control at `y + 2`, `row_height - 4` tall; a switch sits at the right edge; text sits at
`y + max 4 ((row_height - font_size - 3) / 2)`. `test_ui_parity`
compares an offscreen 2x render (no window, as `tools/ui_shot` draws) of the kit widgets with
`lib/pxui/fixtures/kit_panel_2x.png` pixel for pixel; the goldens are rendered with
DepartureMono (the test sets `RAYS_UI_FONT`) and are regenerated only when the kit's design
changes, by running the test with `RAYS_UPDATE_FIXTURES=<dir>`.

## Coordinate model

Panel position, width, padding, row height, layout, painting, and hit testing
use Rays logical points. Runtime translates SDL3 logical event
coordinates into that space; PXUI never multiplies positions by
`Frame.pixel_scale`, which only selects the glyph density.

A panel with `height` fills its pane; `max_height` caps a content-sized panel.
Both clip rows to the padded content rectangle.
Scrolling is one mechanism, `apply_scroll` in `Pxui.Ui`, for every `scroll` box
(panels, the inspector body, lists, menus, the text area): no widget has its
own. Its model and constants are NSScrollView's as `../scriptc-ui`
(`packages/appkit/src/scrolling-behavior.ts`) fitted them to a recording.

- An unphased wheel (a mouse, or any platform without gesture phases) scrolls
  by `scroll_step` (one row) per step. Past an edge, `x` points of travel show
  as `0.05 x / (1 + 0.05 |x| / h)` in a box `h` tall, and 80 ms after the last
  step the overscroll decays with tau = 0.084 s.
- A trackpad (`Input.Trackpad_scrolled`, macOS) moves the content one point per
  point while the fingers are down, in the box the gesture began over. Past an
  edge the content follows the fingers at about half their travel at first
  (`0.55 x / (1 + 0.55 |x| / h)`; the reference's 0.05 read as not following), held
  until the fingers lift, then decays with tau = 0.084 s. Fingers that rest more
  than 0.1 s before lifting do not coast. Lifted in motion, the box coasts from the release
  velocity (the last motion over the time since the one before, from the
  events' own clock; under 20 points a second there is no coast):
  `y(t) = y0 + v0 tau (1 - e^(-t/tau))`, tau = 0.26 s, ending when the speed
  falls under 20 points a second. A coast that reaches an edge overshoots it
  as `A (e^(-9.25 t) - e^(-19 t))` with `A = 0.39 v / (19 - 9.25)` for the
  arrival velocity `v`, and ends on the edge. A touch or a wheel step catches a
  coast or bounce where it is.
- The system's own momentum events are not applied (the coast replaces them),
  and the wheel steps SDL makes of a gesture's motion do not move the box in a
  frame that carries the gesture; both still reach `signal.scroll` in steps, so
  a canvas that zooms on the wheel is unchanged.
- Every motion is a closed form of the time since it began or an exponential of
  the frame's own interval, so the frame rate does not change it. An overscroll
  under a quarter point becomes zero and a coast is on whole device pixels:
  a box at rest does not move. Content that fits its box neither scrolls,
  stretches nor coasts.

A positive vertical delta moves the content down, matching a downward natural
trackpad or Magic Mouse gesture; an ordinary wheel follows SDL's normal
direction. Horizontal steps do nothing. The scrollbar shows the bounded offset.

A `Ui` handle gives its theme's `panel` colour to the window
(`Sketch.set_window_background`) on its first frame and when the theme changes:
the title bar of a PXUI host is the ground its chrome is painted on.

## Pointer and keyboard contract

- Buttons, toggles and section headers commit only when the press
  and release both land inside their control; a release outside cancels.
- A compact field's track follows the captured pointer outside its bounds;
  the release position applies before capture ends.
- A compact field opens an inline editor that takes focus (a click, or a
  double-click or Option-click on a field with a track); Enter commits valid
  text, Escape cancels, and a press elsewhere commits valid text and drops
  invalid text.
- Pressing a text field focuses it: `TextInput` appends UTF-8, `TextEditing`
  shows IME composition, Backspace/Delete remove one scalar value. A press
  elsewhere clears focus. `Ui.text_input_focused` lets hosts suppress their
  own shortcuts. Long values scroll horizontally to keep the caret visible;
  Command-Left/Right reveal the start/end, and dragging past either edge
  extends the selection while scrolling.
- The wheel goes to the topmost scroll box under the pointer. A blocking box over it stops the wheel
  only when it is not that box's own content: a field inside a scrolling panel lets the panel
  scroll, a menu over the panel does not.
- `PointerCancelled` ends capture without a release (no click, drag commit,
  or context click) and keeps text focus. `WindowFocusLost` ends capture the
  same way and clears hover, focus, and composition.

- A carry holds one payload on the handle: a `kind` and a `value`, both strings. `Ui.carry ~from`
  holds it once the pointer is 4 points from the left press of the capture owner (a widget calls it
  every frame it is pressed); `Ui.carry` without `~from` holds it at once from a key, with no
  capture, and a left press is then the put (nothing else sees that press). `Ui.drop_target ui box`
  answers `Hover payload` while the topmost box under the pointer is the box or inside it (hover is
  computed for every box during someone else's drag) and `Dropped payload` on the frame of the
  release there, once. The payload is gone after the release, a `PointerCancelled` or a
  `WindowFocusLost`; `Ui.cancel_carry` is the host's cancel and keeps the pressed box from
  carrying again before its next press. The ghost (the value as a label by the pointer) is a root
  box raised with `to_front ~order:max_int` and painted with `draw_over` at the end of `Ui.frame`;
  a frame with nothing carried builds and paints nothing new, so the parity fixtures do not move.
  `Ui.text_area_submit ~on_drop` reports, while a payload is held over the area, the byte under the
  pointer and whether it hovers or was released there (the carry's text caret). There is still one hit
  list and one capture. See `flow.md` §7.12.

## Hosts

- The camera panel (Camera and Render sections, `toggle_ui`/`open_camera`, `navigate`) is the editor's, in
  `rays_editor/viewport.ml`; it uses the shared inspector rows and reads the camera each frame.
- `Editor_core.Store.Settings` persists model values as one
  `(settings :sketch "name" :key value ...)` s-expression, written atomically.
- `Pxui_shell.Inspector.fields` builds standalone parameter rows from a
  schema each frame (folders become inspector sections, keys are field names) and
  applies edits through `Node.apply_parameters`. `Inspector.flow_fields`
  builds editor rows from a node of the workspace text: card pins (●/○), grouped vec3
  controls, drive source and live-value display, and reset. It emits
  typed requests for the host to apply after the PXUI frame.
- `Pxui_graph.Scope.update view ui frame` builds the graph canvas: a clickable,
  scrollable canvas box and one box per visible tile, keyed by path, with
  socket, field and button children. The pane's bucket indices cull
  tiles and resolve wire hits. Nodes flow left to right;
  the first geometry input sits in the header, the others in rows. Wires are straight
  segments bent clear of cards, never authored. The dot grid is on a
  continuous 24-point graph pitch. Scrolling zooms at the pointer, and dragged
  tile boxes move in the frame that receives the pointer motion.
  The node menu (host-opened, `Pxui_graph.Node_menu`) uses `Ui.popup`, with a search row
  that takes focus in the frame it opens; a
  right click opens `Ui.context_menu` for a tile.
  Levels point/chip/card/full are set explicitly (`o`, `p`) and never by the zoom; cards
  show wired and written parameters. Numeric `Ui.value_field` controls scrub when dragged;
  Option-click opens the shared text editor. A right or middle drag pans.
  The pane's keys are `Editor_core.Command` entries with guide membership
  (`Scope.bindings`); the host's status strip and grouped key sheet read that table.
  The pane has a
  marquee (a left drag on empty canvas), titled frames (Shift-G) and inline name, input-default
  and frame-title fields, all built from `Ui.box` and `Ui.value_field`
  (`specification/flow.md` §6 and §7).
- `Ui.popup` uses the last laid-out rectangle for outside-press dismissal;
  an estimated height is used only until the first layout. `Ui.modal` and
  `Ui.context_menu` share that dismissal path. `Ui.modal` centers a panel
  using its last height. Escape and focus loss also dismiss. The node menu
  uses `Ui.fuzzy_match` on labels, keys, and category breadcrumbs, as the
  preset picker does. `Ui.picker` retains its
  cursor and armed-delete row; the host keeps the query and recomputes rows as
  typing changes it. Their golden is `fixtures/kit_overlays_2x.png`.
- `Rays_editor` builds the whole workspace — pane backgrounds, splitters,
  headers, graph, inspector, status — in one `Ui.frame` per application
  frame.  The panes are the leaves of a `Pxui_shell.Layout` tree: every
  leaf has a 24-point header (its kind as a label, a breadcrumb, its tools, the accent square of
  the focused pane), a right-click menu on it (split, close, retype)
  and a collapse chevron (a window has dock and close); a splitter is a one-point gutter whose seven-point drag
  target is built after the panes, so a neighbour's hit rectangle never covers it,
  and a drag is view state until release (one edit of the editor graph, one history
  entry; the same three edits are the keys `/ o v/h/x` and `/ l g/l/t/i/u/m/w` on the
  focused panel).  Every leaf is an instance with its own view state, however many of a kind the layout has
  (`flow.md` §11.11); box keys are seeded by the enclosing box, an explicit `###id` included, so two
  instances never share widget state.  The graph canvas paints its grid, zones and wires in a clipped
  child, so a pane beside it is never painted over. The Lisp pane is a `Ui.text_area` in the same hit tree;
  the list and text
  views use PXUI's retained elastic scroll state.
  `Editor.update_with ~inspector` adds sketch-owned kit widgets
  below the camera sections.

## Regression requirements

Tests for PXUI changes must cover press/release commit and cancellation,
captured drags beyond bounds, final release positions, focus loss and pointer cancellation, text entry with UTF-8 and IME,
numeric-field editing, bounded scrolling, inspector sections,
and identical behaviour at 1× and 2× (`lib/pxui/test_ui`);
offscreen 2x pixel parity of the kit (`test_ui_parity`, in `runtest`); exact UI-pipeline
coverage against Scene2 geometry (`rays_execution/test_ui_pipeline`);
and the graph, inspector, and workspace contracts (`test/test_pxui_graph`,
`test/test_sop_ui`, `lib/pxui_shell/test_shell`, `test/test_rays_editor`).

## Undo history

`Editor_core.History` is the one bounded immutable history that higher-level editors
share instead of keeping private stacks. `commit` makes the current value
undoable and installs a new one (clearing redo), `amend` replaces the current
value without an entry so a continuous pointer edit collapses into one step,
and `undo`/`redo` walk the stack within a fixed capacity. `Rays_editor` keeps
its document (the workspace text and its layout) in one: graph-pane edits, node creation,
paste, delete, and inspector parameter commits are entries, field drags held
under the primary button are amended into the entry opened at press, and
Command/Ctrl-Z, Shift-Command/Ctrl-Z, and Ctrl-Y step it.

## Graph zones

The graph pane of a workspace document adds tokens only: `Theme.zone_for`,
`zone_fold`, `zone_sum`, `zone_fn` and `zone_let` (fill, edge, dashed; light
and dark from the study's palette), and `Theme.ports` entries `text`, `fn`
and `record` (a list draws its element colour on a stacked socket, a function
a diamond, a record a wide pill). `Theme.dark` tells the two palettes apart.
`test_ui_parity` guards them with `fixtures/kit_zones_2x.png`. The
iteration selector is built from ordinary boxes; no widget was added to `Ui`.
The footers (value, sparkline, tags) are painted
the same way: no new token or widget.

Conditional arms use `Theme.zone_branch ~taken`: a solid hairline and a warm
wash, stronger for the selected arm. These zones have a condition rail and no
iteration selector. The 2x zone golden covers both taken and untaken states.

`Ui.table` is a read-only table on the same box, capture, focus, scroll and
draw paths. Header and body rows are 24 points, with right-aligned values on
hairlines and no row fills. Header text determines column widths with a
72-point minimum. The body builds only visible row boxes and requests only
their cells; one extent box supplies the shared scrollbar's full range. Rows
belong to that extent, and the shared wheel/trackpad/coast state advances before
the fixed-size body chooses its visible range.
A clicked row returns its zero-based index as an intent. Its own 2x golden
is `fixtures/kit_table_2x.png`.

`Ui.text_area` is the multiline sibling of `text_field`: one box (clickable,
focusable, scrolling) with a line-number gutter, sharing `text_field`'s focus,
IME composition, clipboard and edit events; it adds Enter, Up/Down, line-scoped
Home/End, Escape to leave, `?readonly`, `?errors` (gutter marks) and `?spans`
(tinted ranges). It draws only additive pixels, so no kit fixture changed. Tab inserts (two
spaces; Shift-Tab takes them off; the box carries `Ui.keep_tab`, so focus traversal leaves it alone),
`~wrap` (rows of a monospaced line, the gutter numbers logical lines) and `text_area_submit`, which also
reports Command/Ctrl-Enter (the text pane's apply). `Ui.key_pressed` lets a dialog see its Enter.
A `?language` (`Ui.language`: `colorize`, `brackets`, `indent`, `pairs`) makes it a code editor without
teaching it a language: coloured runs by byte span, the bracket pair at the caret lit, Enter followed by
the language's indentation, openers typed in pairs (wrapping a selection), closers stepping over
themselves and Backspace taking an empty pair; `?on_context` reports a right-click for the host's menu.
Every text widget edits as macOS does (word selection by double click, line by triple, Option and
Command with the arrows, Backspace and Delete, Shift extending, Command-Z/Shift-Command-Z undo and
redo kept per focused text with consecutive typing as one step); `Ui.signal.clicks` counts a
double or triple click.  A language's optional `rewrite` runs after each edited frame and maps
the caret: the editor's Lisp supplies parinfer's indent mode (`Lisp_text.parinfer_text`), toggled
from the text pane's right-click menu.
`Ui.context_menu` is as wide as its longest row and an empty label is a separator. `Ui.set_font_size`
scales every kit text and row between frames (the editor's Command +/-).
