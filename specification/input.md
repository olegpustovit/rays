# Input and events

Runtime translates SDL3 input into Rays's typed, ordered `Input.event` stream
and folds held input into `Frame.input`. Native event values never expose an
SDL3 pointer or structure.

## Keys and buttons

`Input.key` represents printable characters, arrows, navigation/editing keys,
modifiers, F1–F12, and `Unknown of int` for an unmapped native code.
`Input.mouse_button` covers left, right, middle, and the two common auxiliary
buttons.

Held input lives in `Frame.input`: its `keys`, `buttons`, `pointer`, and
`events` fields are the current immutable snapshot. `Frame.mouse_delta` is the
current-frame aggregate, and `Frame.mouse_down` queries a held button. There is
no global query or public setter. Pressed collections are state facts,
not a replacement for ordered events. Use events for edges and text, and frame
facts for continuous actions.

## Event order

`Frame.input.events` preserves poll order for:

- key press and release;
- pointer motion, press, release, cancellation, and wheel motion;
- committed UTF-8 and IME composition;
- file drops, and files dragged over the window;
- trackpad pinch;
- a finished file dialog;
- logical window resize and focus loss;
- a requested window close.

Runtime folds the same pass into the frame facts. `Frame.mouse_delta` is
the sum of all logical pointer displacement observed during that application
frame and resets before polling the next frame.

## Logical coordinates

Pointer positions and resize dimensions use the same logical-point coordinate
system as `Scene`, `Frame.input.size`, and PXUI. Runtime obtains SDL3 logical
coordinates directly; application code must not multiply them by
`Frame.pixel_scale`. Physical drawable dimensions are a separate render
boundary.

`SDL_EVENT_WINDOW_PIXEL_SIZE_CHANGED` is authoritative for backing-size
changes. The pump reports it, Runtime marks the window and re-reads it once
before the next frame is presented, and updates logical and drawable facts
coherently; the window is not polled every frame.

## Focus and cancellation

`Focus_lost` clears held keys and buttons so a missing release cannot
leave application state stuck. It also gives higher-level interaction systems
a hard boundary for pointer capture and text composition.

`Pointer_cancelled` releases the affected pointer capture without pretending
that the whole window lost focus. PXUI and camera controls must consume this
distinction consistently.

## Text and file-drop lifetime

`Text_input` carries committed UTF-8. `Text_editing` carries composition text and
its range. Pure `Scene.text_input_region` metadata tells Runtime where native
text entry is intended; Runtime owns SDL3 text-input activation and event
translation. Text input is active only while a focused region exists (a focused
region starts it, none stops it), so the system input method is not engaged
while no text field has focus.

`File_dropped` owns a copied OCaml path. The native event payload is released at
the runtime boundary and cannot escape into user code.

## Domain and reset rules

Polling, state mutation, and every SDL3 operation run on the initial OCaml
domain. Each poll resets only per-frame facts; focus loss clears held keys and
buttons. The state is private to `Sketch`.

## Regression requirements

Input changes require tests for:

1. key/button press-release ordering and simultaneous held state;
2. multi-motion delta aggregation and next-frame reset;
3. logical coordinates at multiple drawable scales;
4. focus-loss clearing;
5. pointer cancellation distinct from focus loss;
6. committed UTF-8 and IME composition ordering;
7. resize and pixel-size transitions;
8. copied file-drop lifetime;
9. explicit finite native integration termination;
10. initial-domain enforcement and zero native-handle delta after teardown.
