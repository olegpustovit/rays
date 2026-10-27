# Internal OCaml layer

A sketch is a `.rays` workspace interpreted by `rays`. Rays Lisp names,
defaults, ranges and errors define the product contract. The OCaml modules
below support the editor, tests and integrations; they are not an alternate
sketch-authoring surface. See [Flow](flow.md), [SOP graphs](sop.md) and
[the backend boundary](backend.md).

## Responsibilities

| Module or library | Responsibility |
| --- | --- |
| `Flow` | Workspace syntax, checking, immutable values and evaluation |
| `Flow_ir` | Typed frame and region programs, optimization and CPU blocks |
| `Flow_sop` | Checked Lisp node definitions and geometry host operations |
| `Sop` | Immutable cook graph, payloads, context and bounded sessions |
| `Rdk` | Packed geometry, topology, attributes and native geometry kernels |
| `Rays.Sketch` | Native loop and immutable application model |
| `Rays.Frame` | Immutable logical frame facts and ordered input events |
| `Rays.Scene` | Pure scene data and the render effect boundary |
| `Rays_execution`, `Scene_execution` | Preparation and GPU execution |
| `Runtime` | SDL lifecycle, presentation and native events |
| `Ogpu` | Backend-neutral GPU API with Metal and mock implementations |
| `Pxui.Ui` | The shared immediate-mode UI engine |
| `Rays_editor` | Workspace editor and native host |

The cleanup's target ownership changes are tracked in
[cleanup/status.md](cleanup/status.md). This table describes the current
libraries; it does not claim those changes are complete.

## Native loop

`Sketch.run_state` initializes a model, calls `update model frame`, obtains
a `Scene.t` from `view model frame`, and returns `Ok final_model`. Native
startup, runtime, validation and rendering failures return `Error
Ogpu.Error.t`; the workspace entry point turns startup failures into Lisp
diagnostics. Callback exceptions remain fatal: they write a crash report and
propagate. Callbacks return replacement models. Mutable renderer state belongs
behind the renderer boundary rather than in the application model.

The optional `after_present` callback runs after rendering and returns the
model used by the next frame and by `on_stop`. Resource cleanup belongs in
`on_stop`, while SDL is alive. A finite `max_frames` bounds automated runs;
the native launcher also honors `RAYS_MAX_FRAMES`.

`Sketch.config` contains width, height, title, optional FPS and domain
counts, and a clock. `Realtime` uses elapsed time. `Fixed dt` provides a
deterministic timestep. `Sketch.export_state` runs a fixed sequence and
writes PNG frames; `Sketch.export` is its stateless counterpart. Both return
typed errors for invalid arguments, startup failures and failed PNG writes;
exceptions from sketch callbacks still propagate.

Handle the result at the caller's startup boundary:

```ocaml
match Sketch.run_state ~init ~update ~view () with
| Ok final_model -> final_model
| Error error -> prerr_endline (Sketch.error_to_string error)
```

`Sketch.quit` requests termination. Relative pointer mode, cursor shape,
window background and file dialogs use result-returning calls on the
active runtime. File dialogs return an ID immediately and deliver their
answer through a later `Input.File_dialog` event in `Frame.input.events`.

A fatal callback exception produces a crash report beneath the platform
temporary directory's `rays-crash` folder, or `RAYS_CRASH_DIR`. The report
contains the exception, backtrace, recent input and environment facts.
`crash_dump` can add the current document before the exception propagates.

## Frame and coordinates

`Frame.t` carries `Input.t` as `Frame.input`, alongside backing-pixel scale,
FPS and per-frame pointer delta. `Input.t` contains logical size, time, delta
time, frame and tick counts, pointer position, held keys and buttons, and
ordered events.

Scene positions, UI layout and pointer coordinates use logical points.
Backing-pixel conversion happens once in the GPU backend. A frame's
pointer delta sums that frame's motion and resets on the next frame.
Do not infer pointer movement from the difference between absolute
positions: captured pointer mode can move beyond window edges.

`Frame.mouse_down` queries a held button; `Frame.has_event` queries the
ordered event list. `Input` provides the native host's typed
vocabulary. Host code must preserve text, IME, scroll phases, pinch,
focus, visibility, file-drag and dialog events.

## Pure scene data

`Scene.t` is a list of scene nodes. Constructors build data; `Scene.render`
performs the effect. Shapes include points, lines, rectangles, circles,
polygons, paths, text and images. Groups, translation, rotation, scale and
clipping compose pictures. `Scene.view3d` embeds a 3D scene with a camera
and an optional logical viewport.

Colors use `Rays_math.Color.t`; angles use radians. The convenience 2D
constructors accept integer logical positions; `Rays.Path` aliases the shared
`Scene_command.Path` API and preserves floating-point path commands through
adaptive tessellation. Backend and runtime values must not appear in
sketch-facing scene data.

The `Scene.Private` interfaces support owned lowering and tests. They
are not the language contract. Scene equality ignores transient render
caches; image and resource identity still matter. Shader and retained-buffer
lifetime must remain valid while a prepared scene can be presented.

## Owned resources

Images, textures, fonts, canvases and audio have explicit owners. Create,
use and destroy them on the initial domain. Release them in the loop's
`on_stop`, before the runtime shuts SDL down.

| Resource | Current internal operations |
| --- | --- |
| `Image` | Result-returning `load`, `create`, `upload_rgba`, size queries and `destroy`; `load_exn` is the raising convenience |
| `Canvas` | Result-returning creation, size, scene rendering, pixel access, image conversion, PNG save and `destroy`; `create_exn` is the raising convenience |
| `Font` | `load` or `system`, cached text images, metrics, hinting, `destroy` |
| `Texture` | Owned GPU texture resources used by materials |
| `Audio` | SDL3_mixer-backed loading and playback |

Canvas and Image operations return errors rather than raising when resource
creation, access, rendering, conversion or destruction fails. Their explicit
`*_exn` conveniences raise by name. A failed native backend initialization is
a typed error, never a CPU, OpenGL, browser or SDL2 fallback.

Canvas captures and screen PNGs preserve native framebuffer dimensions.
Canvas density is fixed by its first render. An image borrowed from a
canvas's GPU output stays valid only until the canvas changes or dies.
Automatic text uses bounded renderer-local caches; explicitly cached
font images retain their documented owner lifetime.

## Geometry and cooking

Authored SOP graphs use Lisp calls such as `sop/grid`, `sop/transform`
and `sop/with_attr`. Their checked definitions own parameter metadata.
Do not add a typed OCaml constructor or parameter that Lisp cannot express.

`Sop.Node.t` is an immutable cook node. `Sop.Context` supplies frame,
seed, domains, grain and cancellation facts. `Sop.Session` cooks nodes
and owns bounded cached payloads; close it after use. Geometry payloads
contain `Rdk.Geometry.t`, whose packed topology and attributes have
explicit ownership and validation.

Lisp-output goldens are replacing the old native-twin parity suites.
Until each case is captured, the remaining oracle code is migration
scaffolding; it is not an API to extend.

## UI and editor integration

Build widgets during one `Pxui.Ui.frame` over the host's shared handle.
Return intents and apply them after the frame. Reuse its focus, capture,
text entry, hit list and renderer. Dragging a value between panes uses
`Ui.carry` and `Ui.drop_target`.

Workspace text is authoritative. Checked syntax gestures and document
reducers update it atomically, and one user gesture creates one history
entry. Graph projection, inspector controls and Lisp must express the
same capabilities. See [PXUI](pxui.md) and [Flow](flow.md) for the UI kit
and graph behavior.

`Rays_editor.Workspace.main` returns startup diagnostics to its caller.
The `rays` executable prints them and chooses the exit code. The OCaml
`Rays_editor.Editor.run` boundary returns native startup errors as
`Rays.Sketch.error`.

## Determinism and validation

Pure dense work uses the shared `Parallel` pool over immutable or
disjoint data, with a sequential cutoff and tunable grain. Split immutable
`Rand.t` generators before parallel work. Sequential and parallel results
must be byte-identical. Native resources and caches remain on the initial
domain.

Build the shared launcher once with `dune build tools/check.exe`, then
time `_build/default/tools/check.exe` with the focused alias for the
changed library. Use `@runtest-native` for display-dependent checks and
`@qualification` for machine-specific or long checks. Reserve
`check.exe --ship` for the final shipping check. An internal `.mli`
change is reviewed directly; there is no frozen OCaml API manifest.
