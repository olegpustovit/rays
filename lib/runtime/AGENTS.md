# lib/runtime rules

## Library ownership boundaries

- `rays` owns target-independent application semantics: `Sketch`, immutable
  `Frame` facts, pure `Scene` data, public `Event`/`Input`, resource APIs, and
  renderer behavior. It may call the narrow `runtime` lifecycle/presentation
  boundary, but it must not implement HTTP, WebSocket, DOM, or browser policy.
- `runtime` owns SDL3 subsystem lifetime, native environment setup/restoration,
  Metal surface presentation scheduling, and typed event translation. It must
  not own widgets, scene constructors, or application models.
- Sibling libraries such as `pxui` depend only on public `rays` semantics.
  PXUI represents text-entry intent as pure `Scene` metadata; it must never
  call native runtime modules or inspect platform internals.
- Cross-library communication uses narrow typed functions. Do not expose raw
  SDL, Metal, or runtime internals in `Scene` or public sketch code.
- A boundary change must include a Dune dependency-direction check, focused
  tests at each affected boundary, and an update to `specification/backend.md`.

## Native runtime contract

- Runtime has one native Metal lifecycle: initialize SDL3 on the initial
  domain, create the high-DPI Metal view and `ogpu_metal_native` surface, translate
  events, acquire/present drawables, drain completion/deferred release, then
  destroy GPU resources before the view/window/SDL.
- Native fixed-pipeline `Scene3` meshes render through Metal with hardware
  transforms, depth/stencil, lighting, culling, blending, and window MSAA.
  Functional shaders require typed MSL/IR support or return a typed
  unsupported-feature error.
- Native GPU access and packed mesh caches belong to `rays`, stay on the
  initial domain, submit through `ogpu_metal_native`, and remain strictly bounded
  under changing procedural meshes. Verify more than the first presented frame.
- A compatible native Metal device and surface are required. Their absence must
  return a typed startup error.
- Any automated application-loop test must arrange its own termination.
- Keep SDL3, Metal, texture, font, audio, event, and cache operations on the
  initial domain. Never create a domain or thread per frame.

## Input translation

`Runtime_input` events come from `Sdl3.Event.t` in `Runtime_input_sdl3.translate`:
keys are the binding's `Sdl3.Key.t` (the keycode is mapped once, in the stubs),
zero or negative sizes a minimizing window reports are dropped, `Occluded`,
`Hidden` and `Minimized` are `Visibility_changed false`, only a pinch update
carries a zoom factor, a phased trackpad scroll is `Scroll` beside the `Wheel` SDL makes of it
(only `Wheel` feeds `wheel_delta`), and a drag over the window, a dropped file and a finished
file dialog are `File_dragged`, `File_dropped` and `Dialog_closed`. The pump
returns the first rejected event as an error and still delivers the rest;
`Sketch` fails loudly on it.

## High-DPI and coordinate contract

- Treat `Sketch` configuration sizes, `Frame.width`/`height`, `Scene`
  coordinates, `Frame.mouse`, mouse event positions, and PXUI layout as
  logical points in one shared coordinate system.
- Keep SDL3 logical size synchronized with the actual window size.
  `SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED` is authoritative: the input pump reports
  it, `Runtime.window_changed` marks the window, and the next frame re-reads it
  once. Never poll the window size every frame.
- Text input runs only while a text field has focus (`Runtime.set_text_input`:
  a region starts it, `None` stops it); never leave it on for the window's
  life. A covered window gets no end event: `Runtime.visible` reads the named
  window state and the sketch loop asks once a frame while hidden. SDL itself
  turns a Control-click into a right click (a hint set in `Runtime.create`);
  there is no Rays-side emulation.
- Native callbacks only queue (`specification/sdl3.md`, "Callback policy"): the
  file-dialog callback copies into a bounded native slot and the event poll on
  the initial domain turns it into `Dialog_closed`. No OCaml runs in a native
  callback, with one shipped exception: live resize redraw (`specification/sdl3.md`,
  "Live resize"). An `SDL_AddEventWatch` callback inside our own `SDL_PollEvent` calls the
  sketch's registered closure, which draws the last scene only; keep it so.
- Do not manually scale mouse events for Retina displays. SDL3 logical event
  coordinates keep drawing and hit testing aligned.
- Query Metal drawable size for physical backing pixels. Preserve
  `Frame.drawable_width`, `drawable_height`, `drawable_size`, and
  `pixel_scale` as the explicit native-pixel boundary.
- Convert a logical `Scene.view3d` sub-viewport to physical drawable edges
  exactly once inside the native GPU backend before configuring a Metal viewport
  or scissor. Never pass logical Retina coordinates directly to Metal.
- `Canvas.capture` and `Canvas.save_screen_png` read and preserve the full native
  framebuffer. Never allocate their readback from logical window dimensions.
- Keep `Scene.text` on an installed platform UI font and interpret `?size` in
  logical points. Font texture caches must include renderer density and
  rerasterize at native resolution. `RAYS_UI_FONT` remains the portable
  override; do not bundle Apple system fonts.
- Keep automatic scene text memory-bounded with the per-renderer 256-entry LRU
  without shortening the documented lifetime of explicit
  `Font.cached_text` images. Preserve empty text as a safe no-op rather than an
  SDL_ttf error.
- Keep renderer-local text caches LRU-bounded to 256 textures, including for
  rapidly changing labels, and preserve empty text as a valid no-op.
- Keep diagnostic text inside the native scene command path; do not reintroduce
  a legacy bitmap-font dependency.

High-DPI changes need coverage for renderer logical/output size, native
capture dimensions, logical mouse alignment, per-frame delta reset, and PXUI
press/drag/release behavior at simulated backing scales.
