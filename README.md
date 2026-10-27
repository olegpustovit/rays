# Rays

A creative-coding system for Apple-Silicon Macs. A `.rays` Lisp workspace describes drawings, geometry, scenes and editor layout. The native editor renders through Metal and uses SDL3 for its window, input and media services.

This is an active-development project. Native rendering requires an Apple-Silicon Mac with Metal; there is no native CPU, OpenGL or SDL2 fallback. A separate 2D web-export target is described in the [project guide](AGENTS.md), rather than a substitute native renderer.

## Platform and build

The package requires OCaml 5.3 or newer and Dune 3.21 or newer. You also need opam, the macOS developer SDK/frameworks and SDL3, SDL3_image, SDL3_ttf and SDL3_mixer development packages.

On the supported Mac host:

```sh
brew install opam sdl3 sdl3_image sdl3_ttf sdl3_mixer pkg-config
git clone https://github.com/olegpustovit/rays.git
cd rays
opam init
opam switch create . 5.3.0 --no-install
opam pin add --no-action --yes --recursive ./packaging
eval "$(opam env --switch=. --set-switch)"
opam install . --deps-only --with-test --with-doc
dune build @all
```

## Run a workspace

The small [basic workspace](sketches/basic.rays) defines a draw graph, an editor graph and window settings. In the native environment:

```sh
dune exec rays -- sketches/basic.rays
```

A finite run uses the existing frame limit:

```sh
RAYS_MAX_FRAMES=30 dune exec rays -- sketches/basic.rays
```

The application is built around Rays Lisp. `Sketch`, `Frame` and `Scene` are internal OCaml layers, rather than the first-time user's sketch API. The [language reference](specification/flow.md) explains workspace graphs and references.

## Verification

```sh
dune build tools/check.exe
_build/default/tools/check.exe --ship
dune build @doc
```

[AGENTS.md](AGENTS.md) records development commands and dependency boundaries.
The [architecture cleanup tracker](specification/cleanup/status.md) records
implementation and verification against the approved plan.

## License

[MIT](LICENSE). SDL libraries, fonts and Apple platform components retain their own terms; see [license and provenance notes](specification/licenses.md).
