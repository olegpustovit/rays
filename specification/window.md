# Native window and application loop

Rays ships one application runtime: SDL3 window/input/audio lifecycle with an
SDL Metal view, checked OGPU command recording, and Metal presentation on Apple
Silicon. `Sketch` is the public entry point.

## Startup and ownership

Startup performs the following ordered work on the initial OCaml domain:

1. initialize the required SDL3 subsystems and extension libraries;
2. create the configured high-DPI SDL3 window;
3. create its Metal view and obtain the `CAMetalLayer`;
4. create the Metal device, OGPU surface, command resources, and native input;
5. expose the first immutable `Frame.t` to user initialization.

Every step is checked. Missing Metal capability, an incompatible device,
window/view failure, or extension initialization failure returns a typed
startup error. No environment variable, hidden profile, provider, or public API
can select a different rendering backend.

The initial domain owns SDL3 windows, events, Metal layers/drawables, image/font
decode and upload, audio devices, and resource destruction. Pure update,
geometry, and scene preparation may use the shared domain pool, then join before
crossing this boundary.

## Title bar

Every Rays window keeps the traffic lights, hides its title text and has a
transparent title bar (`titlebarAppearsTransparent`, `NSWindowTitleHidden`, set
when the window is created). The content view is not full-size: layout and hit
testing start under the bar as before. The bar shows the NSWindow background,
which is the system's until `Sketch.set_window_background color` sets it; a
PXUI host gets its theme's ground from `Pxui.Ui` without asking. The title is
still the window's name in Mission Control and the Window menu. Off the Cocoa
video driver none of this does anything.

## Logical and drawable coordinates

Window configuration, `Window.size`, `Frame.size`, scene coordinates, pointer
events, and PXUI layout use logical points. `Window.drawable_size` and
`Frame.drawable_size` expose physical backing pixels; `pixel_scale` is their
ratio to logical size and may change when the window moves between displays.

Application code does not scale pointer or scene coordinates manually. Runtime
updates the Metal drawable extent after an authoritative SDL3 size change and
performs logical-to-drawable viewport conversion exactly once before encoding.
`Canvas.capture` and `Canvas.save_screen_png` preserve the full drawable-sized
framebuffer, so a logical `800 × 600` Retina window may produce a
`1600 × 1200` image.

## Frame lifecycle

Each loop iteration:

1. resets per-frame pointer delta and polls SDL3 events in order;
2. updates `Input` and constructs immutable `Frame.t` facts;
3. threads the user model through `update`;
4. lowers the pure scene returned by `view`;
5. acquires a Metal drawable, records checked OGPU commands into the owned
   RGBA8 capture target, then uses that same producer queue for the typed
   RGBA8-to-BGRA8 drawable pass and ordered presentation. A classic final pass
   combines both operations in one command buffer; Command4-only final passes
   use the ordered same-queue presentation fallback;
6. applies configured realtime pacing or advances a fixed clock by frame count.

`Sketch.Fixed dt` requires finite positive `dt` and derives time from frame
count. `~max_frames` provides finite native integration runs without changing
the selected backend. `Sketch.export[_state]` uses the same native capture path
and a fixed clock to write deterministic numbered PNG sequences.

## Events and resizing

Keyboard, pointer, scroll, trackpad scroll gesture, pinch, committed text, IME editing, file-drop (and
file-drag position), file-dialog, focus, resize, and close events enter one
ordered `Event.t` stream. Pointer movement is
summed into `Frame.mouse_delta` for the frame. Focus loss clears held input and
cancels capture. File-drop strings are copied before SDL3 releases its payload.

On macOS a trackpad scroll is two events. SDL's wheel event (`MouseScrolled`)
is a tenth of the points moved and merges the fingers with the system's
momentum; it says nothing of the gesture's phases and nothing at all when the
fingers lift. `TrackpadScrolled { delta; phase; time }` carries those: points,
`Touched`/`Moved`/`Lifted`/`Momentum`, and the event's own clock. Each one that
moves has its `MouseScrolled` in the same frame, so a reader of `MouseScrolled`
alone sees what it always did.

The authoritative SDL3 size-change notification (the window is not polled)
updates logical window facts on the next frame, refreshes drawable size, resizes
the Metal surface, and emits one `WindowResized` event. While the window is
minimized or covered the sketch loop skips drawing and idles; SDL announces the
start of occlusion but not its end, so the loop reads the window state once a
frame to see it end. A visible window whose scene is the same as the last one presented
(`Scene.Private.same`: what the nodes draw, resources by handle) is not drawn or presented,
as a native view keeps its last picture; without vsync to pace it the loop blocks on SDL's
event queue for a frame, and after half a second of the same picture for up to 100 ms
(`Sdl3.Event.wait`). A still editor on recursive_rectangles went from 60 presents and about
45% CPU a second to three presents in ten seconds and 3% CPU. While an edge is dragged the last scene is redrawn at the new size (`specification/sdl3.md`, "Live resize"). Fullscreen, show/hide, minimize,
maximize, restore, title, and position operations delegate to the owned native
window.

## Shutdown

`Event.WindowClosed` or `Sketch.quit ()` requests an orderly stop. User
`on_stop` runs while SDL3, Metal, fonts, images, canvases, and audio are still
valid. The runtime then drains completion-owned releases, destroys children
before parents, tears down the Metal surface/view and SDL3 window, and restores
process state. User code should prefer this lifecycle over `exit` or reliance on
GC finalizer timing.
