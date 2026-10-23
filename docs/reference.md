# Workspace, examples and development reference

This preserves the prior detailed guide and historical port notes. The [README](../README.md) gives the current entry point and capture status. Commands below require the documented native Mac environment; they were not run successfully on the Linux review host.

# Rays

> Rays is under active development. The language, the workspace format and
> the qualification tooling may change.

Rays is a creative-coding system for native Apple-Silicon Macs. A sketch is a
Lisp workspace file, `sketch.rays`. Its drawings, geometry, scenes, settings and
UI are graphs that the Rays Editor draws from the text, and edits back into the
text. Rendering goes through Metal, with OGPU as the renderer interface and SDL3
for window, input and media. Metal unavailability is a typed startup error;
there is no other renderer.

The OCaml API (`Sketch`, `Frame`, `Scene`) is an internal layer that the editor
is built on. It is not a user-facing product, and new sketches are written in
Rays Lisp.

## Platform requirements

- An Apple Silicon Mac with Metal support.
- OCaml 5.3 or newer and Dune 3.21 or newer.
- opam and, optionally, direnv.
- SDL3, SDL3_image, SDL3_ttf, and SDL3_mixer development packages.
- The macOS SDK and standard Apple frameworks supplied by the operating-system
  developer tools.

The runtime compiles its small built-in MSL sources at run time. Building
Rays does not require Xcode, the offline Metal compiler, `.air` files, or a
prebuilt `.metallib` pipeline.

With Homebrew:

```sh
brew install opam direnv sdl3 sdl3_image sdl3_ttf sdl3_mixer pkg-config
```

## Build from source

```sh
git clone https://github.com/olegpustovit/rays.git
cd rays

opam init
opam switch create . 5.3.0 --no-install
opam pin add --no-action --yes --recursive ./packaging
direnv allow

opam install . --deps-only --with-test --with-doc
dune build @all
dune runtest
dune build @doc
```

Without direnv, activate the local switch in each shell:

```sh
eval "$(opam env --switch=. --set-switch)"
```

The checked-in `.envrc` only activates the repository-local opam switch and
loads non-secret defaults from `.env` when present. It does not choose a
renderer.

## A sketch

This is `examples/basic/sketch.rays`:

```lisp
(workspace basic

  (graph picture :context draw
    (let* [phase (state [previous 0.0] (+ previous (frame/dt)))]
      (draw/merge (draw/background "#14161c")
                  (draw/circle [(/ (frame/width) 2) (/ (frame/height) 2) 0]
                               (+ 42 (int (* 10 (sin (* phase 2)))))
                               :fill "#5aaaf0")
                  (draw/text [12 12 0]
                             (str (frame/width)
                                  " × "
                                  (frame/height)
                                  "  frame "
                                  (frame/index))))))

  (graph editor :context editor
    (ui/workspace (ui/canvas (ref picture) :focus true)))

  (graph window :context settings
    (settings/config :title "Rays basic sketch" :width 640 :height 360)))
```

A workspace is a list of named graphs. Each graph has a context: `draw` for
pictures, `sop` for geometry, `scene` for 3D scenes, `editor` for the layout
of the window, and `settings` for the window title, size and seed. `(ref name)`
points at another graph. The language reference is
[specification/flow.md](../specification/flow.md).

Run an example or a sketch natively:

```sh
dune exec examples/basic/lisp.exe
dune exec sketches/flow_contours/main.exe
```

Command-S in the window rewrites the file with its comments kept. An edit of
the file on disk reloads the window. A finite run sets `RAYS_MAX_FRAMES` to a
frame count and exits when it is reached; the `smoke` rules use this.

## Examples and sketches

`examples/<name>/` holds a short teaching program. Each Lisp example has a
`sketch.rays` and a `dune` file that generates `lisp.exe`:

```sh
dune exec examples/audio/lisp.exe
dune exec examples/basic/lisp.exe
dune exec examples/drawing/lisp.exe
dune exec examples/file_dialog/lisp.exe
dune exec examples/generative/lisp.exe
dune exec examples/noise/lisp.exe
dune exec examples/particles/lisp.exe
dune exec examples/procedural_modeling/lisp.exe
dune exec examples/pxui/lisp.exe
dune exec examples/recursive_rectangles/lisp.exe
dune exec examples/sop_gallery/lisp.exe
```

`examples/sop_gallery` is a `gallery.rays` catalogue of SOP graphs; its scene shows
`(ref boolean)` and the outline opens the others. One example is still an OCaml
executable:

```sh
dune exec examples/pathtracer/main.exe
```

`sketches/<name>/sketch.rays` is a sketch that is only a workspace, with no
`dune` or `main.ml` of its own. `sketches/dune` generates its executable as
`sketches/<name>/main.exe`. Every sketch except the three below is of this
kind, for example `sketches/flow_contours`, `sketches/flow_terrain` and
`sketches/ws_tunnel`. `sketches/cube_cage`, `sketches/shattered_cube` and
`sketches/voxel_wall` still have their own `main.ml`:

```sh
dune exec sketches/voxel_wall/main.exe
```

Examples and sketches that automation runs must finish on their own under
`RAYS_MAX_FRAMES`.

## Scaffolding

Create an example (`examples/<name>/sketch.rays` and its `dune`):

```sh
dune exec tools/new_example.exe -- my_example
dune exec examples/my_example/lisp.exe
```

Create a sketch (`sketches/<name>/sketch.rays` only). Then add it to the
generated build file, which is checked in as `sketches/dune.rays.inc`:

```sh
dune exec tools/new_example.exe -- --sketch my_sketch
dune build @runtest; dune promote
dune exec sketches/my_sketch/main.exe
```

Both copy `specification/workspace/cases/bloom.lisp` as the starting file. The
name may use only lowercase letters, digits, `_` and `-`.

For the conventions of writing a `.rays` file (names, macros, pixel kernels),
see `.claude/skills/write-rays-lisp/SKILL.md`.

## Build and test

```sh
dune build @check                 # typecheck only
dune build @lib/<name>/runtest    # focused tests for one library
dune build @all && dune runtest && dune build @smoke && git diff --check
dune build @doc
```

- `dune build @smoke` runs two finite native examples. `dune build @smoke-all`
  runs every example and sketch, and opens a window for each one in turn.
- `dune runtest` is the window-free suite. Display-dependent tests run under
  `@runtest-native`, and long or machine-specific checks run under
  `@qualification`.
- To render an editor to a PNG without opening a window, build
  `tools/ui_shot.exe` and run it on a sketch:

  ```sh
  dune build tools/ui_shot.exe
  _build/default/tools/ui_shot.exe sketches/flow_contours/sketch.rays OUT.png
  ```

- A fatal exception in a running sketch writes a report under `/tmp/rays-crash`.

## Layout

```text
examples/      short teaching programs (sketch.rays or main.ml)
sketches/      experiments; sketches/dune generates the workspace-only ones
lib/rays/      public creative-coding API (Sketch, Frame, Scene)
lib/flow*/     the Lisp language, its IR and GPU emitter, graph and SOP layers
lib/rdk/       packed geometry and topology core
lib/sop/ immutable SOP graphs over rdk
lib/pxui*/     immediate-mode UI and the graph pane
lib/rays_editor/ the Rays Editor
lib/metal/, lib/ogpu*/, lib/runtime/, lib/sdl3*/ native layers
specification/ architecture and behaviour
tools/         generators, check runner, ui_shot and benchmarks
test/          automated tests
```

## Specification

- [Rays Flow, the workspace language and editor](../specification/flow.md)
- [Shared frame and region core](../specification/core.md)
- [Workspace cases and iteration](../specification/workspace/iteration.md)
- [Public API](../specification/api.md) and [native backend](../specification/backend.md)
- [Graphics](../specification/graphics.md), [3D rendering](../specification/3d.md)
- [Images](../specification/image.md) and [audio](../specification/audio.md)
- [Input and events](../specification/input.md) and [window](../specification/window.md)
- [Sop SOPs](../specification/sop.md) and [RDK](../specification/rdk.md)
- [PXUI](../specification/pxui.md)
- [SDL3 bindings](../specification/sdl3.md) and [Metal bindings](../specification/metal.md)
- [Packaging](../specification/packaging.md)

Rays is licensed under the MIT License. External SDL3 libraries and Apple
platform frameworks retain their own licenses and terms; see
[licenses](../specification/licenses.md).

## Not yet ported to Rays Lisp

Three OCaml sketches were removed from the tree. Their last OCaml source is
still in git history (`git show <commit>:sketches/<name>/main.ml`). Each one
needs the following before it can be written as a `sketch.rays`:

| Sketch | What it did | What Rays Lisp is missing |
|---|---|---|
| `chromatic_drift` | Generative 1080 x 800 artwork: noise-shaped surfaces built as packed meshes with per-vertex colour and normals, a 256 x 256 texture generated in code and mapped onto a blended quad, a film-grain layer, a panel of sliders with an animate toggle, settings saved to and loaded from a file, a PNG-sequence export, and a geometry benchmark. | Hand-built meshes with per-vertex colour and normals (`scene/geometry` takes SOP geometry only). Textures on 3D surfaces (there is no texture op, and `image/load` gives 2D images). UI sliders bound to sketch parameters, and a save/load of their values (`settings/config` takes only title, size and seed). A sequence export: `host/save_png` writes one picture. |
| `pastel_flow` | Two procedural pastel compositions ("Pearl waves", "Iridescent silk") driven by 56 sliders in five sections. It had per-vertex coloured meshes, a grain layer of about 18,000 points drawn as alpha-blended quads with depth testing off, presets, Save and Load buttons for the control values, one undo entry per slider drag, and a single-PNG export at a chosen size. | Hand-built meshes with per-vertex colour and normals. Alpha-blended 3D layers with a depth state (the `scene/` ops have no blend or depth control). The UI kit ops: `ui/` has no slider, button or undo operation. |
| `code_quadtree` | A map of the repository's source files. It scanned the tree under `--root`, asked an OCaml language server over pipes for the symbols of each file, laid files and symbols out in a quadtree, and let the viewer zoom, pan and click into a cell. Labels appeared at zoom-dependent sizes, and it could save a screenshot. | Reading a directory tree and the contents of text files (no listing or text-read op exists). Starting a child process for a language-server client. A text atlas: many labels at once are one `draw/text` each. Screenshots are already covered by `host/save_png`. |

Checked 2026-10-11: still none of the three. Explicit-point meshes are no longer the gap (`(sop/curve points :filled true)` is a polygon; `sop/set_color` and `sop/normals` write per-point colour and normals). What remains is the UI slider/button/undo ops and settings save/load, a texture op for 3D surfaces, a PNG-sequence export, blend and depth control on `scene/` layers, directory listing and text-file reads, child processes and a text atlas.
