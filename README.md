# Rays

A Rust creative-coding framework and native editor for `.rays` Lisp workspaces.
Apple Silicon uses Metal; Windows uses Vulkan. Web export is a separate 2D target.

```sh
cargo run -p rays_bin -- sketches/basic.rays
cargo run -p rays_bin -- check sketches/basic.rays
cargo run -p rays_bin -- mem sketches/ws_tree.rays --frames 3
cargo run -p rays_bin -- web sketches/basic.rays -o _out/basic
```

Use the toolchain pinned in `rust-toolchain.toml`. On macOS:
`brew install pkgconf sdl3 sdl3_image sdl3_ttf sdl3_mixer`.
Windows setup: [packaging](packaging/README.md).

Debug builds are the daily workflow. Run `cargo check -p <crate>` while editing, then
`cargo t -p <crate> [filter]` for time-bounded tests. See [development](docs/development.md).

- `crates/`: Rust libraries, native bindings, application, Lisp nodes and runtime assets.
- `sketches/`: runnable workspaces and their assets.
- `tools/`: deadline-enforced tests, architecture gates and build-time measurement.
- `fixtures/`: shared shader, CLI and web goldens.
- `specification/`: language, rendering and editor contracts.
- `docs/history/`: historical port measurements and outstanding qualification evidence.

OCaml lives only in Git history at [ocaml-reference](https://github.com/OlegHQ/rays/tree/ocaml-reference)
(commit `e92f94f0`). Rust is the sole implementation here. Existing unfinished performance and
qualification work is tracked in [known failures](docs/known-failures.md); dated evidence lives in [history](docs/history/README.md).

[MIT license](LICENSE).

Lisp authoring: [generated guide](docs/lisp.md); lookup any name with `rays doc NAME`.
