# SDL3 conf packages

The SDL3 implementation is not in these directories.  It is four ordinary
OCaml/Dune libraries under `lib/sdl3`, `lib/sdl3_image`, `lib/sdl3_ttf`, and
`lib/sdl3_mixer`.  These small `.opam` files only declare and probe the native C
libraries that those OCaml libraries link.  Keeping the probes separate lets
opam install the system dependency before Dune compiles the bindings and lets
applications depend only on the extensions they use.

Rays owns four standalone opam conf packages that probe for a stable SDL3
release at least the floor in `packaging/sdl3.lock`, the only place an SDL
version is written (see `specification/sdl3.md`). They live outside the Dune
package root so opam treats them as probe packages, not as Dune-built
libraries.

For a checkout whose opam repository does not yet contain these packages,
create the local switch without auto-installing the project, then register all
probe packages recursively before installing Rays dependencies:

```sh
opam switch create . 5.3.0 --no-install
opam pin add --no-action --yes --recursive ./packaging
opam install . --deps-only --with-test --with-doc
```

Without `--no-install`, `opam switch create` tries to resolve `rays` before
the checkout-local probe packages are known and correctly reports them as
unknown. The ordering above is therefore part of the supported bootstrap, not
an optional workaround.

The package probes use `pkg-config --atleast-version` with the lock's floor (opam
cannot read a file, so `tools/packaging/test_sdl3_lock` fails when a probe
disagrees with the lock); newer releases are accepted and the layout asserts
(`lib/sdl3/abi.sexp`) catch ABI drift. An OCaml/Dune repository test also
compiles, links, and executes a version probe for each library against the
floor, which catches header/runtime mismatches that `pkg-config` alone cannot
detect. After upgrading SDL, `dune exec tools/sdl3/bump.exe` runs the four
qualification aliases and records the release in the lock's `tested` field.

## Dune discovery

Dynamic linking is the default.  Set `RAYS_SDL3_LINK_MODE=static` to ask
pkg-config for its static/private dependency set and select the component's
`libSDL3*.a` archive.  Static mode fails early when the package metadata or own
archive is absent; it never silently falls back to a dynamic SDL library.
Platform frameworks can remain dynamic even when the SDL archives are static.

Each component also accepts explicit include and library directory overrides:

```text
RAYS_SDL3_INCLUDE_DIR       RAYS_SDL3_LIB_DIR
RAYS_SDL3_IMAGE_INCLUDE_DIR RAYS_SDL3_IMAGE_LIB_DIR
RAYS_SDL3_TTF_INCLUDE_DIR   RAYS_SDL3_TTF_LIB_DIR
RAYS_SDL3_MIXER_INCLUDE_DIR RAYS_SDL3_MIXER_LIB_DIR
```

Override directories must exist.  In static mode the library directory must
contain the component archive, while pkg-config still supplies its transitive
link flags.

The committed OCaml tests exercise core and all extensions through dynamic,
static, and explicit-path discovery with isolated pkg-config metadata.  Run
both profiles and the installed-consumer checks with:

```sh
opam exec -- dune runtest tools/packaging --force
opam exec -- dune runtest --profile release tools/packaging --force
opam exec -- dune exec tools/packaging/check_installed_consumer.exe -- \
  --root . --profile dev
opam exec -- dune exec --profile release \
  tools/packaging/check_installed_consumer.exe -- --root . --profile release
```

The consumer checker installs `rays` into a temporary relocatable prefix,
resolves all four packages from that prefix, then builds and executes a separate
Dune project outside the source checkout.

## Native memory qualification

Set `RAYS_SDL3_SANITIZERS` to `address`, `undefined`, or
`address,undefined` in a dedicated Dune build directory.  The shared
configurator adds the sanitizer to both C compilation and the final native
link; unknown values fail discovery.

The committed `sdl3-memory-tests` alias builds the ten core/extension
conformance executables.  `check_sdl3_memory.exe` runs that identical matrix
under AddressSanitizer, UndefinedBehaviorSanitizer, or macOS Instruments
Leaks and rejects diagnostics even when a tool exits zero.  A normal leak run
is:

```sh
opam exec -- dune build @tools/packaging/sdl3-memory-tests
opam exec -- dune exec tools/packaging/check_sdl3_memory.exe -- \
  --mode leaks --artifacts _build/default \
  --fixtures lib/sdl3_image/fixtures
```

Use an OCaml compiler built with opam's
`ocaml-option-address-sanitizer` for the ASan lane.  During execution the
driver disables ASan's alternate signal stack because Apple ASan otherwise
mis-handles OCaml Domain teardown on this arm64 16 KiB-page platform.  The
native window test also loads `tools/packaging/sdl3_asan.supp`, which suppresses
only a reproducible `pdf_lexer_scan` over-read in macOS 26 CoreUI's system-owned
theme asset.  Rays does not call that function; all Rays and SDL3 stub
interceptors remain enabled.  The complete native window path is independently
run without suppression under UBSan and Instruments Leaks.
