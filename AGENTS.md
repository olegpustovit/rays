# Rays repository guide

## Purpose

Rays is an OCaml creative-coding framework for interactive native desktop
programs. It ships native Apple-Silicon Metal only, through OGPU, whose API is
backend-agnostic by design. Keep the public API small and functional where
practical. Never add a CPU raster, browser/web, SDL2/Tsdl, or OpenGL fallback;
Metal unavailability is a typed startup error. Web export (see Direction) is a
separate compiler target, not a fallback: the native program never selects it.

Nested `AGENTS.md` files hold subsystem rules: `lib/metal`, `lib/ogpu`
(also for `lib/ogpu_core`),
`lib/runtime`, `lib/rdk`, `lib/rays_editor`, `lib/pxui_graph`, `lib/sop_catalog`.
Read the one for the directory you change. Design notes live in `specification/`;
the shared frame/region compiler contract is [specification/core.md](specification/core.md).
Update them when behavior or architecture changes materially.

The SOP network editor is Rays Flow: a left-to-right
typed canvas with value ports, drives, graph/list/text views and a
checked Lisp text form. `specification/flow.md` is the normative design.
Work on `pxui_graph`, the graph pane, parameter drives
or sketch Lisp follows it. Everything the Lisp can say is drawn in the graph and edited
there directly: a call of a node kind is a card whether it is bound in a `let*`, written inside
another call or a step of a `->` (a nested node, path leaf `holder#input`), and its inputs are rows
the pane edits in place. New syntax or sugar ships with its graph projection, its `Flow_edit`
gestures and a test, never as text-only. `specification/flow/prototype/` is an HTML behavioral reference to open
in a browser, never product code and never a web fallback.

## Direction

Lisp is the first-class surface of Rays: a sketch is a `.rays` workspace. `Sketch`, `Frame` and
`Scene` are the internal layer the editor is built on; there are no typed SOP constructors, and the
only OCaml programs are `examples/pathtracer` and the sketches `cube_cage`, `shattered_cube` and
`voxel_wall`. The OCaml API is second-class, used by tests and integrations. When the two could
differ, Lisp decides: its names, defaults, ranges and errors are the contract, and the OCaml function is derived to match (`specification/sop.md`,
"one declaration", for SOPs). A new capability ships in Lisp first; do not add an OCaml-only
parameter or behaviour.

Web export: `dune exec rays-lisp -- web FILE.rays -o DIR` emits a standalone browser
application; `web --watch FILE.rays [-o DIR] [--port 8000]` previews and reloads it.
Its scope is the 2D canvas only (`:context draw` graphs
and the values that feed them). PXUI and the editor are never exported; they are editor chrome and
stay native. `scene` and `sop` graphs are a later phase. 128 of the 162 SOP nodes now have Rays Lisp bodies
(`lib/flow_sop/nodes/*.rays`; the replaced OCaml kernels stay in `lib/rdk` as test oracles); the
31 nodes on exact predicates, spatial indexes and solvers, 3 wiring nodes and 3 native carriers stay
OCaml and have nothing to export. Generated `image/map`
images run as WebGPU kernels; `image/noise` preserves native double arithmetic, `image/render` uses
offscreen WebGPU textures, and static `image/load` files are bundled using the native decoder. The design summary and the
task plan are `specification/web-export/plan.md`.

## Libraries

| Library | Owns |
|---|---|
| `native_layer_token`, `lru`, `param`, `frame_input`, `rays_math` | Leaves with no dependencies: the opaque presentation-layer handle, a bounded LRU, typed parameter schemas (`Sop.Parameter`, `Editor_core.Param`), immutable logical frame facts, and the pure `Vec2`/`Vec3`/`Mat4`/`Quat`/`Color` math that `rays` re-exports |
| `sdl3`, `sdl3_image/ttf/mixer` | SDL3 bindings (foundational) |
| `metal` | Metal bindings: safe layer over a handwritten bridge (foundational) |
| `ogpu_core`, `ogpu` | Portable GPU core and virtual public API |
| `ogpu_metal_native`, `ogpu_metal`, `ogpu_mock` | Native Metal detail and the two OGPU implementations |
| `runtime`, `runtime_input`, `runtime_resources` | SDL3 lifecycle, Metal presentation, frame stats, typed event translation, SDL image/ttf/mixer services |
| `rays_execution` | Private frame coordinator: Scene lowering caches over one window or offscreen `Runtime` |
| `scene_command`, `scene_execution` | Renderer-neutral commands, and prepared GPU execution; neither depends on the other, `rays_execution` joins them |
| `rays` | `Sketch`, `Frame`, pure `Scene`, `Event`/`Input`, resources, renderer behavior |
| `rays_pathtracer` | Hardware ray-traced path tracer |
| `rdk` | The single packed geometry/topology compute core, built from `rdk_core` → `rdk_exact` → `rdk_spatial` → `rdk_attrib` → `rdk_gen`/`rdk_curve` → `rdk_mesh` → `rdk_boolean`; `rdk_rays` is its glue to `rays` meshes |
| `sop` | Immutable SOP graphs over `rdk` operations |
| `sop_catalog` | Inspectable SOP node factories registered by PPX |
| `flow` | UI-free workspace language over `param` and `frame_input`: reader, printer, macros, checker, evaluator, frame folds, packed arrays and typed deferred nodes |
| `flow_ir` | Typed dataflow IR over `flow` evaluations with sharing, hoisting, pruning, fusion and precision passes, and the block-at-a-time CPU kernel tier over `Parallel`; uses the `scene_command` marks contract as pure data; `flow` never depends on it |
| `flow_gpu` | Metal emitter from `flow_ir` packed programs, a pipeline cache of 64 and owned runners; depends on `flow`, `flow_ir`, `ogpu`, `rays_execution`, `lru`; never reaches geometry or UI |
| `flow_web` | WGSL and JavaScript printers from `flow_ir` core programs for the web export; depends on `flow`, `flow_ir`, `param`, `rays_math`, `scene_command`; never reaches `rays`, geometry, GPU or UI libraries |
| `flow_graph` | Domain-neutral graph projection, checked text gestures, exposure and probes over `flow` and `param` |
| `flow_sop` | Typed SOP/value overlay, drives, exposure and environment-owned value lane |
| `editor_core` | Editor state and routing: labelled `History`, `Command`, `Keymap`, `Router`, the shell's panel tree (`Panels`), plus atomic file writes and s-expression user preferences (`Store`, printed by `Flow.Lisp`) |
| `editor_document` | Package-private scene/network/settings model, workspace document (`Workspace_doc`, `Layout_by_path`), validation, object/layer schemas and s-expression presets; no presentation dependencies |
| `pxui` | The one immediate-mode UI engine (`Pxui.Ui`) |
| `pxui_shell` | Editor chrome over PXUI: layout, headers, keys, status, timeline, prompts, frame, `Inspector` |
| `pxui_graph` | SOP-network presentation; emits typed requests, never edits |
| `rays_editor` | Rays Editor: the Houdini-like SOP shell (`Editor`), composed only from public blocks; also the Sop-to-Scene glue (`Surface`, `Packed_pieces`), canvas `Drawing`, `Live_frame` and `Timeline` |

`examples/<name>/` are short teaching programs; `sketches/<name>/` are
experiments. Each has its own `dune`, depends only on what it shows, keeps
framework code out, and runs finitely under `RAYS_MAX_FRAMES`. Scaffold a Lisp
example (`sketch.rays` plus its `dune`) with `dune exec tools/new_example.exe -- <name>`. A sketch that is only a Flow workspace is
`sketches/<name>/sketch.rays` with no `dune` or `main.ml` (`--sketch <name>` scaffolds it): `sketches/dune`
generates its executable with `rays-lisp`, and after adding or removing one you run
`dune build @runtest; dune promote` to update the checked-in `sketches/dune.rays.inc`. Command-S in
its window rewrites the file (comments kept) and an edit of the file reloads the window. Prefer
`Rays_editor.Editor` for SOP sketches, SOP graphs for geometry, and
deterministic seeds.

## Dependency rules

`test/dependency_gate.ml` reads every `lib/**/dune`, builds the transitive
graph, and enforces "may never reach" rules plus a token scan. It lists no
exception.

- Foundational libraries (`sdl3*`, `metal`, `ogpu_core`, `ogpu`, `native_layer_token`,
  `scene_command`) never reach `runtime`, `rays`, or anything above.
  `ogpu_core` depends only on `native_layer_token` (the opaque presentation
  layer handle); virtual `ogpu` depends only on `ogpu_core`. `ogpu_mock`
  stays portable; native Metal detail depends only on `ogpu_core`, `metal`
  and `lru`.
- `Metal.`/`Ogpu_metal_native.` stay within the Metal backend; the runtime,
  path tracer, and their tests use the virtual `ogpu` only, and the gate lists
  no Metal exception.
- `rays` never depends on `pxui`, geometry, sketch libraries, or examples.
- `pxui_shell` depends only on `rays`, `editor_core`, and `pxui`; it never imports
  SOP, graph, geometry, or sketch libraries.
- `rdk` never reaches `sop`; `sop` never reaches UI
  libraries; `pxui` never reaches `sop`; `param` depends on nothing;
  `pxui_graph` never reaches `sop` or `rdk`;
  `flow_graph` depends only on `flow` and `param`; `flow_ir` depends only on `flow`, `param`,
  `rays_math` and `scene_command` and never reaches `rays`, `pxui*`, `sop`, `rdk` or `rays_editor`;
  `flow_gpu` depends only on `flow`, `flow_ir`, `param`, `rays_math`,
  `ogpu_core`, `ogpu`, `rays_execution` and `lru` and never reaches
  `sop`, `rdk`, `flow_sop` or any UI library; `flow_web` depends only on `flow`, `flow_ir`,
  `param`, `rays_math` and `scene_command` and never reaches `rays`, `sop`, `rdk`, `flow_sop`,
  `flow_gpu`, any UI library or the GPU libraries;
  nothing below imports `rays_editor`.
- A boundary change updates the gate, adds focused tests at each affected
  boundary, and updates `specification/backend.md`. Do not expose raw SDL,
  Metal, or runtime values in `Scene` or public sketch code.

## Loops

```sh
dune build @check                 # inner loop: typecheck only
dune build @lib/<name>/runtest    # focused tests
dune build @all && dune runtest && dune build @smoke && git diff --check  # pre-commit
dune build @doc
```

`@smoke` launches two finite native examples; use `@smoke-all` for the full
example/sketch sweep. Both open windows sequentially. For a window-free broad
test run, set `SDL_VIDEODRIVER=dummy SDL_AUDIODRIVER=dummy` on `dune runtest`.

A fatal exception in a running sketch writes a report folder under
`/tmp/rays-crash` (exception, backtrace, recent input, and the editor's
document as text, plus a loadable `.rays` for a workspace); read it first when asked to investigate a crash.

## Test time is a budget

Slow tests cost every agent every edit. Treat test time like a performance bug: measure it, record it,
and fix it in the same change that made it worse.

- Never run the whole `@runtest` to check an edit. Run the focused alias (`@lib/<x>/runtest`,
  `@test/<name>`) through `tools/check.exe`, which kills a run at 180 s (`RAYS_CHECK_TIMEOUT`, `0` = none).
  A run that hits the timeout is a defect to fix, not a limit to raise.
- Time every focused run you do and note the wall time in your report. After a change, read the
  per-action times with `dune trace cat | jq` (or the `test_workspace_shell` section lines) and find the
  slowest ones before you finish.
- Budgets: a focused alias finishes in under 30 s warm; one test section under 10 s. The
  `test_workspace_shell` runner leaves its listed slow sections (`slow` in the file) to `RAYS_SLOW=1`; run them
  only for the final check, in the background.
- A test over budget gets reworked in this order: shrink the fixture (fewer frames, a smaller graph, no
  footer probes); share one setup between its checks; replace a UI-driven path with a direct call to the
  function under test; split the slow exhaustive part into `qualification-scale` or `RAYS_SLOW=1`
  (`enabled_if (= %{env:RAYS_SLOW=0} 1)`). Delete a test that no longer guards a behavior anyone relies on,
  and one that repeats another's check. Never leave a slow test in the default loop "for later".
- Record it. When you find or fix a slow test, add one row to `specification/test-times.md` (date, test,
  before, after, what changed); when you add a test that takes over 5 s, add the row with the reason. Check
  the file before you add a slow one: a similar test may already exist to extend.
- Tests that open a window stay in `@runtest-native` (see Loops); an unbounded wait, a sleep or a real clock
  in a test is a bug.

Default `runtest` is green on a clean checkout. Display-dependent tests live in
`@runtest-native`; long, SDK-, driver-, or machine-specific checks in
`@qualification`. Large algorithm fixtures (studio fracture and high-density
one/four-domain exactness) and exhaustive catalog/control sweeps are optional
under `@qualification-scale` (also included in `@qualification`). Normal
shipping does not run them.
When agents share a worktree, build `tools/check.exe` once, then invoke
`_build/default/tools/check.exe` directly: it queues validation requests before
Dune takes its build lock. It defaults to `@runtest`; pass focused aliases such
as `@lib/rdk/runtest`, or `--ship` for `@all`, `@runtest`, `@smoke` and
`git diff --check`. Use the same launcher for every agent in that worktree.
Keep Dune's cache: do not use `--force` or clean between ordinary validations.
Agents run the focused alias for their change; reserve `--ship` for the final
shipping check. GPU rendering integration and presentation timeouts live in
`@runtest-native`; pure prepared-command validation stays in `@runtest`.
A public `.mli` change shows as a diff of
`tools/api_manifest/api_stable.json`; accept an intended change with
`dune promote`. Warnings are errors. Automated application loops arrange
their own termination. Build, generation, and validation glue is OCaml under
Dune, never Python; external-tool comparisons belong in `../rays-support`.
The generated `lib/sop_catalog/flow_manifest.sexp` is also checked by
`dune build @lib/sop_catalog/runtest`; when SOP metadata changes, review and
accept its diff with `dune promote`.

SDL3 versions live only in `packaging/sdl3.lock` (a floor per library and the
release last tested); `dune exec tools/sdl3/bump.exe` records a new one. Native
structs the stubs read are pinned in `lib/sdl3/abi.sexp`; the build fails and
names a field an SDL release moved (`generate.exe accept` re-probes). See
`specification/sdl3.md`.

Bootstrap: `opam switch create . 5.3.0 --no-install`, then `opam pin add
--no-action --yes --recursive ./packaging`, `direnv allow` (or
`eval "$(opam env --switch=. --set-switch)"`), and `opam install . --deps-only
--with-test --with-doc`. `packaging/` only probes native SDL3 dependencies.

## Public API

- A sketch is a `.rays` workspace. OCaml code on the internal layer (`Sketch`, `Frame`, `Scene`)
  threads an immutable model through `Sketch.run_state`; no global user-state refs.
- A `view` returns `Scene.t`. Scene constructors are pure data;
  `Scene.render` is the effect boundary.
- `Frame.t` carries canvas, time, input, and ordered events.
  `Frame.mouse_delta` sums one frame's pointer motion and resets each frame.
- Conventions: `at:(x, y)` positions, radians, radius for circles, `Color.t`
  colors, short constructor names, sensible defaults with explicit config.
- Resources (textures, fonts, canvases, audio) are owned: release them in
  `Sketch.run_state ~on_stop` while SDL is alive. Audio is SDL3_mixer-backed
  and never silently a no-op.
- No fake placeholders: implement a real `result`-returning boundary or leave
  the operation out. Prefer explicit `result` errors at backend boundaries.
- Every public module has an `.mli`; sibling libraries stay wrapped. Changing
  the high-level design updates `specification/api.md`, an example, and tests.

## Coordinates and native runtime

Sketch sizes, `Frame` sizes, Scene coordinates, mouse positions, and PXUI
layout are logical points. `Frame.drawable_*`/`pixel_scale` are the explicit
native-pixel boundary; conversion to drawable pixels happens once, inside the
GPU backend. Details: `lib/runtime/AGENTS.md`.

## Determinism and domains

- SDL, Metal, texture, font, audio, event, and cache operations run on the
  initial domain. Never create a domain or thread per frame or item.
- Use the shared `Parallel` Domainslib pool for coarse pure work on immutable
  or disjointly owned data, with a tunable grain and a sequential cutoff.
- Parallel results are byte-identical to the sequential path; every parallel
  change has an exact one-domain vs multi-domain regression.
- Use immutable `Rand.t`, never global `Stdlib.Random`, in parallel-capable
  code; split generators before independent work. `Sketch.Fixed dt` and
  `Sketch.export` are artifact-deterministic.

## Performance

Performance is correctness on hot paths. Measure before changing one (wall
time, allocations, input size, domains, profile, machine) and hand off with the
benchmark command and before/after numbers. Packed arrays, not lists, for dense
data; no `List.nth`, `@`, or repeated `List.length` in hot loops; pre-size or
grow geometrically; int keys over polymorphic hash. Every cache has an explicit
capacity. Frame work must not scale with unchanged scene size. Never claim
"linear", "zero allocation", or "production-ready" without evidence. Full
contract: `lib/rdk/AGENTS.md`.

## UI

`Pxui.Ui` is the only UI engine: build widgets each frame inside `Ui.frame`,
keep their values in the immutable model, destroy the handle in `on_stop`.
Every host shares its capture, focus, hit list, and renderer; never add a
second hit-test, capture, text-entry, or painting path. New widgets are
functions over `Ui.box`/`Ui.signal`/`Ui.draw`, drawn with the kit's tokens and marks (rev 3,
`specification/pxui.md`: `Pxui.Theme` inks, lines and fills, `Ui.Paint.cap`/`brackets`/`chevron`,
`Pxui_shell.Kit.button`/`segments`): a field is a value on a hairline, a button is its text, no
radius, shadow or ink fill. `tools/ui_shot.exe FILE.rays OUT.png` renders an editor without a
window; `sketches/ws_layout` is the layout the design reference is compared with. UI code returns intents and does
not mutate the model during `Ui.frame`. A drag that carries a value between panes is `Ui.carry` / `Ui.drop_target` (one payload on the handle,
no second hit-test or capture); `specification/flow.md` §7.12. Preserve the design kit (`Pxui.Theme`,
Pragmasevka, 24-point rows and bars, 20-point controls); `lib/pxui/test_ui_parity` guards it pixel for pixel
against goldens this kit drew with DepartureMono (the test sets `RAYS_UI_FONT`; refresh them only for a
design change, with `RAYS_UPDATE_FIXTURES=<dir>`). Host and editor rules: `lib/rays_editor/AGENTS.md`.
