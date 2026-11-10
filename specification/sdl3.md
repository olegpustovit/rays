# SDL3 binding contract

`crates/sdl3` provides the safe platform API. Required C/Objective-C++ ABI work
lives under `crates/sdl3_sys`. Core, image, ttf and mixer cover only Rays's window,
input, image, font and audio needs. Runtime must use the ownership-aware safe API.

## Versions and ABI

`packaging/sdl3.lock` owns version floors and tested versions. Build scripts check
headers against the floor and resolve native dependencies through the current
platform discovery path. Compiled header facts and linked runtime facts must
agree on the supported major/minor and satisfy the floor. A tested version is
qualification evidence, not an exact pin.

`crates/sdl3_sys/abi.sexp` records reviewed sizes, alignment and offsets used by
the bridge. Native compilation verifies the supported ABI. SDL's own macros
supply flags, keys, buttons and event constants. Shared bridge tags and signatures
must be checked through round-trip tests. Header or library mismatches fail;
an older build cannot silently mask them.

## Thread classes

| Class | Contract |
| --- | --- |
| Pure or any-thread query | Error formatting, immutable handle facts, installed version facts and documented thread-safe native queries acquire/destroy no resources. |
| Initial thread | Initialization, hints, windows, cursors, clipboard, text input, dialogs, event polling, native views, decoding, fonts and audio check the initial thread before native calls. |
| Deferred release | A destructor on another thread can only enqueue an opaque token. Drain children before parents on the initial thread. |
| Native callback | Queue an owned result in bounded native storage. Never invoke application update or mutate document state from a callback. |
| Blocking initial-thread call | Preserve the native handle's owner and lifetime through the wait. |

On Apple the check also uses the platform's actual main thread, including before
SDL initializes its thread identity. Diagnostic handle inspection never makes
concurrent ownership mutation valid.

## Callbacks and live resize

Polled events are converted while SDL still owns their pointer payloads; copy
all strings before returning. Decoder/font results are caller-owned packed RGBA8.
The mixer owns audio-device work. Native dialogs return an id immediately, retain
filters/default paths until completion, queue chosen/cancelled/failed outcomes,
and deliver each result exactly once during initial-thread polling. Refuse a
request beyond the fixed native slot capacity; bound copied path count and bytes.

Live resize may redraw the last settled scene from the platform event pump.
The callback is guarded against reentry and only runs on the main thread during
our poll. It performs no update, effects, autosave, reload, event polling or clock
tick. Preserve a failure for the next normal loop boundary. Programmatic resizes
exercise coherent dimensions; a real border-drag check establishes responsiveness.

## Events and coordinates

Select platform-specific ABI code and link flags in the owning sys build script.
AppKit messages require autorelease pools. Window decorations and phased scroll
handling stay behind the platform boundary.

- Preserve queue order for key, pointer, wheel, text/IME, pinch, window, drop and
  dialog events. Skip unsupported kinds without losing later events.
- Keys use native keycode macros. Letter/digit shortcuts retain physical-layout
  fallback, while committed text remains a separate event. Unknown keys preserve
  their native code. Control-click mapping belongs to SDL's configured hint.
- Preserve fractional logical coordinates, wheel direction, pointer position and
  ordered motion. Trackpad phases and timestamps remain distinct from ordinary
  wheel events; do not apply natural-scroll inversion twice.
- Logical resize and drawable-pixel changes update runtime facts coherently.
  Do not poll window dimensions every frame. Hidden, minimized or occluded
  windows idle; visibility recovery must still be observed.
- Activate native text input only while a text field is focused. Preserve IME
  ranges, file-drag positions/end, pinch factors and asynchronous dialog ids.
- Focus loss clears held keys/buttons; pointer cancellation remains distinct.

## Ownership and errors

Explicit destruction is idempotent. Generation checks reject stale use and
parent destruction with live children. Validate dimensions, stride, lengths,
numeric ranges and embedded NULs before allocation or decoding. Copy native
error text before another native call; a retained diagnostic remains unchanged.
A failed image/audio reload preserves the previous usable resource.

Failure coverage includes absent video/audio drivers, invalid dimensions and
names, missing/malformed image/font/audio inputs, unsupported native window
views, invalid dialog filters/paths/capacity, wrong-thread calls and destroyed
parents. Event tests cover every translated/skipped kind and copied payload
lifetimes. Lifecycle checks verify bounded handles and native memory.

Still-image support covers installed decoders and their alpha/orientation/failure
behavior; animation is not silently added. Font tests cover platform discovery,
`RAYS_UI_FONT`, UTF-8, metrics, styles, setters and density. Text caches are bounded.
Mixer tests cover device/memory creation, copied loads/reloads, playback, loops,
gain, fades, pause/resume, stop, channel ownership and failure paths.

Run `cargo t -p sdl3` and `cargo t -p sdl3_sys`; use native qualification for
window/lifecycle checks. Historical migrations and sanitizer findings live in
[history](../docs/history/specification/sdl3.md).
