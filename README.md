# Rays

A creative-coding system for Apple-Silicon Macs. A `sketch.rays` Lisp workspace describes drawings, geometry, scenes and editor layout. The native editor renders through Metal and uses SDL3 for its window, input and media services.

This is an active-development project. Native rendering requires an Apple-Silicon Mac with Metal; there is no native CPU, OpenGL or SDL2 fallback. A separate 2D web-export target is described in the [project guide](AGENTS.md), rather than a substitute native renderer.

## Platform and build

The package manifest requires OCaml 5.3 or newer and Dune 3.21 or newer. Its Dune language declaration is not the package's minimum Dune version. You also need opam, the macOS developer SDK/frameworks and SDL3, SDL3_image, SDL3_ttf and SDL3_mixer development packages.

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

These are the repository's native setup instructions. They were source-checked, not qualified on a Mac during this review. The local Linux build attempt stopped on a missing domainslib dependency; it did not establish native rendering, SDL integration or Metal startup behavior.

## Run a workspace

The small [basic workspace](examples/basic/sketch.rays) defines a draw graph, an editor graph and window settings. In the native environment:

```sh
dune exec examples/basic/lisp.exe
```

A finite run uses the existing frame limit:

```sh
RAYS_MAX_FRAMES=30 dune exec examples/basic/lisp.exe
```

The application is built around Rays Lisp. `Sketch`, `Frame` and `Scene` are internal OCaml layers, rather than the first-time user's sketch API. The [language reference](specification/flow.md) explains workspace graphs and references.

## Verification

Native rendering was not tested on the Linux ARM64 review host. This editor targets Apple Silicon with Metal. Design-kit images and the HTML behavioral prototype are not presented as a running editor demo.

To verify native behavior, build on a supported host and run the basic Lisp workspace. The setup text and package-metadata check do not establish a native runtime pass.

```sh
dune runtest
dune build @doc
```

Those full native checks were not completed here. [Development reference](docs/reference.md) preserves the example catalogue, architecture links and historical sketch-port notes. [AGENTS.md](AGENTS.md) records the current direction and dependency boundaries.

## License

[MIT](LICENSE). SDL libraries, fonts and Apple platform components retain their own terms; see [license and provenance notes](specification/licenses.md).
