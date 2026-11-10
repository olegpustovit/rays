# SDL3 binding contract

The SDL3 bindings are ordinary Dune libraries under `lib/sdl3`,
`lib/sdl3_image`, `lib/sdl3_ttf`, and `lib/sdl3_mixer`.  They do not load
functions dynamically and they do not execute OCaml from a native callback.
They bind what Rays uses and nothing else: the ownership-aware `.mli` files
are the only API available to Runtime, and each export has a caller outside
the tests (`codemod dead-exports lib/sdl3 ... --users-exclude test_` reports
nothing). SDL stays the platform layer: a later Linux or Windows port would
reuse its window, input, image, font and audio layers as they are.

## Versions: one lock

`packaging/sdl3.lock` is the only place an SDL version is written:

```
((sdl3       (floor 3.4.18) (tested 3.4.18))
 (sdl3_image (floor 3.4.4)  (tested 3.4.6))
 (sdl3_ttf   (floor 3.2.2)  (tested 3.2.2))
 (sdl3_mixer (floor 3.2.4)  (tested 3.2.4)))
```

- `floor` is the oldest release that builds. `tools/packaging/check_sdl3_conf`
  reads it for the compile, link and run probe, and the generated
  `generated_abi.h` asserts the headers are at least the floor. The opam
  probes cannot read a file, so each carries `pkg-config --atleast-version=`
  with the same number and `tools/packaging/test_sdl3_lock` fails when one
  disagrees (it also fails when a test or `test/dune` writes an SDL version).
- `tested` is the release the qualification aliases last passed on. It is
  information for a bug report and gates nothing. `dune exec
  tools/sdl3/bump.exe` runs `@lib/sdl3/qualification`, `@lib/sdl3_image/...`,
  `@lib/sdl3_ttf/...`, `@lib/sdl3_mixer/...` against the installed releases and
  rewrites `tested`, one line each, only when all four pass.
- This is the floor policy `lib/metal` uses for the macOS SDK. An exact pin
  would need SDL from a Nix flake, since Homebrew cannot hold a version.
- Tests assert that both the headers the binding compiled against and the
  linked library are at least the floor, that the library is not older than
  its headers, and that both are the same major.minor
  (`Sdl3_lock.check_installed`). "Compiled" is the headers' own version macro
  (`SDL_VERSION`, `SDL_IMAGE_VERSION`, ...), read through a stub, not a
  checked-in constant.
- The stubs depend on the probed version. `sdl3_probed.h` is written by each
  library's `discover.exe`, which depends on the whole universe, so every build
  asks pkg-config again; the header carries `pkg-config --modversion` as a
  number, and `generated_abi.h` asserts it equals the headers in use. An SDL
  upgrade rebuilds the stubs, and a stale build directory fails instead of
  hiding a mismatch. (With an explicit `*_INCLUDE_DIR` the probed number is 0:
  pkg-config may describe another install.) The discovery test's fake
  pkg-config reports two patch releases above the floor, so the default run
  stays green on a newer SDL.

## The generator binds, it does not inventory

Nothing generated is checked in. `lib/sdl3/generate.exe emit` writes
`generated_abi.h` into `_build` for each library from the lock, the stubs and
`lib/sdl3/abi.sexp`:

- `abi.sexp` is the reviewed size, alignment and field offsets of the SDL
  structs the stubs read or build (12 event, surface and display-mode structs
  and `SDL_DialogFileFilter`); it is a checked-in contract, not a build
  product. The stubs are scanned for what they read (`event->key.scancode`,
  `surface->pitch`, ...) and the build fails when a stub reads a field the file
  does not pin or the file pins a field no stub reads. A newer SDL that leaves
  those fields alone builds without a change; one that moves them fails the
  build and names the field. `dune exec lib/sdl3/generate.exe -- accept`
  re-probes the installed headers with clang and rewrites it for review.
- No SDL constant is hand-copied into OCaml. The stubs map every flag, key,
  button and event kind with SDL's own macros, so the compiler checks them; the
  few numbers shared with OCaml (event tags, window flag bits) are enums next to
  the code that uses them and are exercised by the event round-trip test.
- The extension headers also pin the signature and calling convention of the
  native functions their stubs call.
- Core stubs build with `-Wall -Wextra -Werror` like the Metal bridge.

An existing build directory can retain native objects compiled against an older
SDK; the probe assertion above catches it.

## Thread classes

Every public operation belongs to one of the following classes.  Runtime must
not bypass these classifications through `Private_raw`.

| Class | Public operations | Enforcement |
| --- | --- | --- |
| Pure or any-thread query | error printers; compiled and linked SDL version facts and checks; `Thread.is_initial_domain`; `Thread.is_sdl_main_thread`; immutable handle generations and modes; TTF installed-font path discovery | These either do no native work or call an SDL function whose pinned header says it is safe from any thread.  They never acquire or destroy a resource. |
| Initial OCaml domain and SDL main thread | core `Init`, `Hint`, `Window`, `Cursor`, `Clipboard`, `Text_input`, `Dialog.show`, event polling, and `Metal_view`; all image decode, TTF init/font, and mixer init/mixer/audio/track operations | The safe entry point checks both `Domain.is_main_domain` and `SDL_IsMainThread` before its native call and returns `Wrong_domain` on failure. |
| Any-domain deferred release | GC finalizers for windows, cursors, Metal views, fonts, mixers, audio values, and tracks | A finalizer only appends an opaque release token to an unbounded mutex-protected queue.  It never calls SDL.  The initial-domain safe boundary drains children before parents; an atomic count lets the usual empty drain skip the lock and allocate nothing. |
| Native callback, any thread | the file-dialog callback in `sdl3_dialog.c` | It runs no OCaml, allocates no OCaml value and takes no lock: it copies the outcome into one of eight fixed native slots and publishes it with a release store. See "Callback policy". |
| Native block, the pumping thread | the NSEvent scroll monitor in `sdl3_platform_macos.c` | AppKit calls it inside `SDL_PumpEvents`, on the initial domain. It runs no OCaml and allocates nothing: it writes one entry of a 256-entry ring (the oldest is dropped), which the same poll drains. |
| Blocking initial-domain call | `Sdl3.Window.sync` | The stub releases the OCaml runtime system around `SDL_SyncWindow`; the synchronized window remains rooted and cannot be destroyed from another domain. The runtime is reacquired before returning to OCaml. |

Extension init queries (`Init.initialized` of TTF and mixer) are result-returning
initial-domain queries even when their state is mirrored in OCaml.

On Apple, the shared main-thread query additionally requires `pthread_main_np()`.
SDL accepts any thread until its thread identity is established during
initialization ([SDL implementation](https://github.com/libsdl-org/SDL/blob/release-3.4.2/src/SDL.c#L250-L262));
headless resources can exist before that happens. The platform check closes that
gap without initializing SDL. Same-domain OCaml threads are rejected both before
and after video initialization; other platforms retain SDL's thread semantics.

`Sdl3.validate_version` applies the same minimum linked-version and
stable-release policy to SDL3 and its extensions. Each extension passes its
own compiled header version and maps incompatibility into its typed error.
All four bindings expose narrow version facts and checks without duplicate
`Version` modules.

Handle inspection such as `destroyed` is diagnostic only.  It does not make
concurrent ownership mutation valid; create/use/destroy operations remain in
the initial-domain class.

## Callback policy

The core, image, TTF, and mixer stubs contain no `caml_callback*` call and
install no callback that runs OCaml. Events are polled into one reusable
native `SDL_Event` union that the stub converts to an OCaml value on the
spot, skipping every kind Rays does not read. Image and font results are
returned synchronously as copied CPU bytes. Mixer device work remains inside
SDL_mixer; Rays supplies no OCaml audio callback.

A native callback is allowed when it only queues. The rule it must satisfy is
the one this file always described: a bounded native queue, no OCaml value,
drained on the initial domain before anything reaches the safe API. SDL3's
file dialogs are callback-only and fit that shape:

- `Sdl3.Dialog.show` (initial domain) claims one of eight static slots in
  `sdl3_dialog.c`, copies the filters and default location into it (SDL reads
  them until its callback runs), and calls `SDL_ShowOpenFileDialog`,
  `SDL_ShowSaveFileDialog` or `SDL_ShowOpenFolderDialog`. It returns the
  dialog's id at once. A ninth open dialog is an error: the slots are the
  bound, so the callback never has to drop or grow anything.
- SDL calls `rays_dialog_callback` on whatever thread its platform code
  chooses. It copies the chosen paths (at most 4096 paths and 1 MiB; more is
  a failure outcome), a cancel, or the error text into the slot and publishes
  it with one release store. It calls no OCaml, allocates no OCaml value and
  takes no lock.
- The event poll (initial domain) takes the oldest finished slot with an
  acquire load, converts it into `Sdl3.Event.Dialog { id; outcome }`, and frees
  the slot. A finished dialog comes before any SDL event in the same poll.
- Tests exercise the real callback from another thread without showing a dialog
  (`test_sdl3_events`): chosen, cancelled and failed outcomes, the over-large
  selection, ordering of simultaneous dialogs, no event while a dialog is open,
  and the ninth dialog being refused. The `Show*Dialog` calls themselves need a
  person and are not run unattended.

The event stubs convert the union while SDL still owns its pointer payloads;
every string is copied before the next call.

### Live resize: finding and decision

The first part is a finding about the polling design; the decision follows the measurements.

On macOS the event loop blocks inside AppKit's tracking loop while the user
drags a window edge: the call that pumps events does not return until the drag
ends, so a program that renders from its own loop gets no iteration and
nothing is redrawn during the resize. SDL offers `SDL_AddEventWatch`, which
fires on the main thread from inside that pump; redrawing during a drag would
need one callback into OCaml there. A queue cannot help, because the loop that
would drain it is the one that is blocked.

`lib/runtime/native_qualification/runtime_live_resize.exe` measures it with a
real window and is the evidence to collect before deciding:

- `--auto` runs under `@qualification` and `@runtest-native`. It is the
  control: four programmatic resizes keep every frame under 250 ms, SDL
  announces the new size as `Resized` and `Pixel_size_changed`, a size changed
  behind the runtime's back (what a drag leaves behind) reaches the runtime
  through those events alone, and the runtime follows. A programmatic resize
  does not enter AppKit's tracking loop, so it cannot show the freeze.
- `--drag SECONDS` is for a person: drag an edge while it runs. It prints the
  longest gap between frames and says FROZEN when frames stalled while the
  window was being resized. Only a pointer drag on the window border enters
  the tracking loop, so it cannot run headless or unattended. It has not been
  run for this revision.

Shipped (the exception was accepted): `caml_sdl3_poll_event` installs one
`SDL_AddEventWatch` callback (`live_resize_watch`, `sdl3_stubs.c`). It acts only
when all hold: the call came from inside our own `SDL_PollEvent` (`in_poll`),
no live redraw is already running (`in_live_redraw`), it is the main thread, and
the event is `PIXEL_SIZE_CHANGED`. It then calls the closure registered as
`rays_sdl3_live_redraw` with `caml_callback_exn`, so the callback never raises.
`Sketch` registers that closure for the life of the loop: it tells the
coordinator the window changed and renders the last scene, nothing else (no
poll, update, effects, autosave, reload or clock tick). A failure is written as
a crash report, kept, and re-raised on the next loop turn. Between size events
the old picture stretches during the drag. No window-free test drives it; the
`--drag` run above is the manual check.

## Events and translations

Dune selects `sdl3_platform_macos.c` and its Objective-C/CoreFoundation link
flags on macOS; other systems select `sdl3_platform_portable.c` with no Apple
frameworks. The common binding calls that platform boundary for main-thread
checks, window decorations and phased trackpad events. A compile-only check
builds the portable C branch on the native development host as well.

SDL does not carry everything AppKit says, and `sdl3_platform_macos.c` reads the rest
through the Objective-C runtime (`objc_msgSend`; the stubs stay C and link
`-lobjc`). Each function there does nothing unless the video driver is `cocoa`,
and brackets its AppKit messages with `objc_autoreleasePoolPush`/`Pop`: no run
loop drains a pool for an OCaml program, and restyling the title bar outside
one autoreleased the NSWindow about ninety times, so no closed window was ever
deallocated (390 KiB each).

- Scroll phases. SDL's Cocoa wheel event is `scrollingDelta * 0.1` for a
  trackpad, with no phase, no difference between the fingers and the system's
  momentum, and no event for a zero delta, so the lift is never seen. A local
  NSEvent monitor for scroll-wheel events, installed with the first window,
  queues each precise, phased event as `Sdl3.Event.Scroll { x; y; phase;
  seconds }` (points in the wheel's directions; began or may-begin, changed,
  ended or cancelled, momentum; the NSEvent timestamp). SDL's own wheel events
  are untouched, so a moving `Scroll` has a `Mouse_wheel` twin; the poll looks
  at the ring before and after SDL's queue so both reach the same batch.
- Title bar. `caml_sdl3_create_window` hides the title and makes the title bar
  transparent on the NSWindow from `SDL_PROP_WINDOW_COCOA_WINDOW_POINTER`;
  `Sdl3.Window.set_background` sets the NSWindow background that then shows.

`Sdl3.Event.t` holds the ten kinds Rays reads: a phased trackpad scroll (above); quit; window changes
(shown, hidden, minimized, restored, occluded, focus gained and lost, close
requested, resized in logical points, pixel size changed in drawable pixels);
key down and up; committed text and composition; pointer motion, buttons and
wheel (with direction, pointer position and integer steps); pinch; drag-and-drop
(begin, position, complete, file); and a finished file dialog. Every other kind
SDL queues (touch, pen, gamepad, sensor, audio and display devices, clipboard,
text candidates, window moves and the rest) is skipped in the stubs and never
copied. Timestamps, window and device ids are not carried: nothing reads them.

- Window size: `Pixel_size_changed` and `Resized` are authoritative. The
  runtime does not ask the window for its size each frame; the input pump
  reports the event and the next frame re-reads once (`Runtime.window_changed`).
  A frame rendered after the OS resized the window and before the event was
  pumped fails in the presentation check ("extents differ"); on macOS the
  window changes only inside the pump, so the order never occurs in a sketch.
- Occlusion: SDL sends an event when a window becomes covered and none when it
  stops. `Occluded`, `Hidden` and `Minimized` become "not visible"; while a
  sketch is not visible it reads `Window.state` (named `hidden`, `minimized`,
  `occluded` bits, decoded in C from SDL's flags) once a frame to see it end,
  skips drawing, and idles.
- Keys: the keycode becomes `Sdl3.Key.t` in the stub with SDL's own macros. A
  printable ASCII keycode is `Char` in lower case; on a layout whose own letters
  are not ASCII (Cyrillic, ...) a letter or digit key is still `Char` by its
  position (the scancode names the US-layout key), so shortcuts work. Anything
  else is `Unknown` with SDL's keycode. Modifiers are a list built from
  `SDL_KMOD_*`.
- Control-click: `Hint.control_click_is_right_click` sets
  `SDL_HINT_MAC_CTRL_CLICK_EMULATE_RIGHT_CLICK`; SDL delivers the right button
  itself. The runtime sets it when a window is created.
- Text input: the runtime starts SDL text input when a text field is focused
  and stops it when none is (see the text region in `specification/api.md`);
  it is off the rest of the time.
- Cursor: `Cursor.shape` is default, text (I-beam), and the two resize shapes,
  chosen in the stub from `SDL_SYSTEM_CURSOR_*`.
- Pinch: only the update carries a factor (the scale change since the last
  update; above 1 zooms in).
- Dropped files: the drag position and end reach the application, so a file
  from the OS can join a drag in progress.

## Ownership and errors

Owned native handles have explicit idempotent destruction and a generation.
Access after destruction returns `Destroyed`; a parent with live children
returns `Parent_has_dependents`.  Borrowed strings and event payloads are
copied before returning.  Each fallible stub copies native error text into its
result before the safe layer can issue another SDL call.  Dimension, stride,
length, numeric-range, and embedded-NUL checks happen before native allocation
or decoding.

The constructor/decoder failure matrix is executable, not inferred from happy
paths:

| Boundary | Injected failure |
| --- | --- |
| `Init.init` | nonexistent SDL video driver in an isolated process |
| `Window.create` | video subsystem absent, plus invalid dimensions and embedded-NUL title |
| `Metal_view.create` | dummy-video window without a native Metal layer |
| SDL3_image file decoder | missing file and malformed input with the extension's own hint for every supported still format |
| `Font.open_file` and system discovery | missing/empty font path, invalid size, and invalid `RAYS_UI_FONT` |
| device/memory mixer creation | nonexistent audio driver and invalid sample-rate/channel facts |
| audio byte creation and reload | missing/malformed/empty input, and a failed reload that leaves the original usable |
| `Dialog.show` | NUL byte, empty or too many filters, a destroyed window or a worker domain (all refused before SDL is called), and a ninth open dialog |
| `Music.create` | destroyed parent mixer |

Native error strings are copied into immutable OCaml error records before the
next native call; the failure tests retain an error across a subsequent
version query and compare it exactly.

The binding tests run the wrong-domain matrix, copied event traces through a
real SDL queue (every translated kind, every skipped kind, the non-Latin key
fallback, coalescing), parent/child teardown, stale access, malformed input,
and 100,000-cycle ownership stress.  `tools/bench_sdl3.exe` reports the FFI
call count, wall time, OCaml allocation, collection counts, and heap size for
the same lifecycle categories and for the pixels an image decode and a text
render hand to the resource layer.

The pixel path no longer passes through an SDL surface: the image and TTF stubs
convert and copy once into the OCaml buffer (`Sdl3.rgba`, tightly packed RGBA8,
owned by the caller) and the resource layer takes that buffer as its storage.
`Sdl3.Surface` is gone. Measured with `bench_sdl3` on this machine: a text
render went from 5.95 us and 31.7 kB allocated to 3.4 us and 15.9 kB; a PNG
decode from 32.7 us and 10.2 kB to 31.9 us and 6.0 kB (the decode itself
dominates).

## Extension parity fixtures

SDL3_image conformance decodes all 19 still-image formats enabled by the pinned
stable distribution: AVIF, BMP, CUR, GIF, ICO, JPEG, JPEG XL, ILBM, PCX, PNG,
PNM, QOI, SVG, TGA, TIFF, WebP, XCF, XPM, and XV.  Each fixture is decoded by
path into tightly packed RGBA8 that the caller owns. Separate fixtures cover
alpha, exact EXIF orientation, malformed input for every decoder (a temporary
file with the format's extension), and the reload rule: a failed reload leaves
the previous pixels alone, a successful one replaces them. The binding exports
only the file loader; the byte decoder is reached through it. Animation formats
are outside Rays's existing still-image API and are not silently advertised
by this binding.

SDL3_ttf conformance discovers an installed platform UI font with
`RAYS_UI_FONT` override semantics, then covers empty text, UTF-8, family and
style names, metrics, RGBA rasterization, setters, and 72/144-DPI rendering.
The high-level Font adapter caches rasters in its 256-entry LRU and uploads
them through the renderer-local OGPU texture cache (tested with the resource
layer).

SDL3_mixer conformance covers copied-memory sound loads and reloads, device and
memory mixers, play, loops, gain, fades, pause/resume, stop, the channel bank,
generated PCM, parent/child ownership, and invalid-driver device failure. Native runtime
tests cover the same typed sample/music operations through SDL3_mixer without a
transport or alternate audio backend.

## Packaging and discovery

All four bindings are ordinary `rays.*` Dune libraries.  The standalone
`packaging/conf-sdl3*` opam definitions own only floor probes (see "Versions:
one lock"); they do not contain implementation or build glue.  Each probe
checks pkg-config, while `tools/packaging/check_sdl3_conf.ml` additionally
compiles, links, and runs a header/runtime version probe against the lock's
floor.

One shared OCaml configurator implements discovery for the core and extension
libraries.  Dynamic pkg-config linkage is the default.  With
`RAYS_SDL3_LINK_MODE=static`, it requests private dependency flags and
replaces the component's `-lSDL3*` flag with a resolved archive path.  Missing
metadata or an absent archive is an error rather than a dynamic fallback.
Component-specific `*_INCLUDE_DIR` and `*_LIB_DIR` variables provide validated
explicit paths, with the explicit path ordered before pkg-config headers.

The hermetic discovery test runs every component through dynamic, static, and
explicit-path cases without depending on the host's Homebrew state, in both
development and release profiles.  The installed-consumer checker installs a
relocatable package prefix, confirms `ocamlfind` resolves every SDL3 package
inside it, and builds and executes an independent Dune project outside the
checkout (the same `installed_consumer.ml` the tree builds).  This distinguishes source-tree success from a usable installed
package.

## Native memory qualification

`RAYS_SDL3_SANITIZERS` applies `address`, `undefined`, or both to every
binding stub compilation and native link.  The committed memory driver runs
the same ten tests in all lanes: core ownership, typed events, constructor
failures, 100,000-cycle stress, real CAMetalLayer lifecycle, every image
decoder, font parity/failure, and mixer parity/failure.  It treats sanitizer or
Leaks diagnostics as failures even if the child process returns success.

On the qualified Apple M1/macOS 26 host, Apple ASan's alternate signal stack
cannot be torn down cleanly after OCaml Domain tests on 16 KiB pages, so the
driver sets `use_sigaltstack=0`; ordinary, UBSan, and Leaks runs retain the full
signal-stack behavior.  AppKit window resizing also exposes a system
CoreGraphics `pdf_lexer_scan` over-read while CoreUI loads an Apple-owned theme
PDF.  The address lane uses a function-scoped interceptor suppression for that
system frame only.  The binding does not call the suppressed function, and the
same real native window lifecycle passes without suppression under UBSan and
Instruments Leaks.  No Rays or SDL3 stub suppression is present.

The release, ASan, and UBSan builds consume the same checked
`lib/sdl3/abi.sexp` and lock, so the layout and floor asserts are identical in
all lanes.
