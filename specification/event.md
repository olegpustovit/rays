# Event stream

`Input.event` is Rays's owned, typed representation of native application
events. Runtime polls SDL3 on the initial OCaml domain, translates supported
events in queue order, and folds held keys, buttons, and pointer position into
`Frame.input`. `Frame.mouse_delta` is the per-frame pointer-motion aggregate.
Polling and held-input state are private to `Sketch`; there is no global query.

## Public events

```ocaml
type Input.event =
  | Key_pressed of Input.key | Key_released of Input.key
  | Pointer_moved of float * float
  | Pointer_pressed of Input.mouse_button * (float * float)
  | Pointer_released of Input.mouse_button * (float * float)
  | Pointer_cancelled of Input.mouse_button
  | Scrolled of float * float
  | Text_input of string
  | Text_editing of { text : string; start : int; length : int }
  | File_dropped of string | File_drag_moved of float * float | File_drag_ended
  | Pinched of float
  | File_dialog of { id : int; result : (string list, string) result }
  | Dialog_opened of int | Resized of int * int
  | Focus_lost | Focus_gained | Closed
  | Pixel_size_changed of int * int | Visibility_changed of bool
  | Trackpad_scrolled of { delta : float * float; phase : Input.scroll_phase; time : float }
```

All strings are copied into OCaml ownership before SDL3 releases the native
event payload.

## Translation rules

- SDL3 key-down/up events carry `Sdl3.Key.t`; `Runtime_input_sdl3` maps each to
  `Input.key` using SDL's keycode macros (`specification/sdl3.md`): a letter or
  digit is `KeyChar` in lower case on any layout, an unlisted key is `Unknown`
  with SDL's keycode. Printable key meaning and text entry remain separate;
  committed text comes from the text-input event.
- A trackpad pinch update is `Input.Pinched factor` (the zoom since the last
  update, above 1 zooms in); PXUI routes it to the scrollable box under the
  pointer as `signal.pinch`, and the graph pane zooms by it where the wheel
  zooms. Files dragged over the window are `Input.File_drag_moved` with the
  position and `Input.File_drag_ended`; a Control-click arrives as the right button because SDL
  converts it (a hint, not Rays code).
- Pointer motion, button, and wheel events preserve poll order. Positions are
  fractional logical points and motion contributes to the current frame's
  float aggregate delta. Wheel deltas retain sub-unit values and SDL's
  device direction. A `Flipped` event is already natural scrolling; the
  runtime does not reverse it again. An ordinary wheel keeps SDL's normal
  direction.
- The authoritative SDL3 pixel-size/window transition updates logical and
  drawable runtime facts coherently on the next frame (the window is not polled
  between events) and emits one logical `Input.Resized` fact. A covered or
  minimized window is not drawn and the loop idles.
- A native file dialog (`Sketch.show_file_dialog`) returns its id at once; its
  outcome is one `Input.File_dialog` event with that id: the chosen paths, an empty
  list for a cancel, or the error.
- Focus loss clears held keys and buttons before user update and emits
  `Input.Focus_lost`.
- A requested native window close maps to `Input.Closed`; user code may request
  the same orderly stop through `Sketch.quit`.
- Pointer cancellation remains distinct from full focus loss.

Runtime does not collapse intermediate motion events. Applications that need
only the latest position use `Frame.input.pointer`; freehand drawing and gesture
code may consume every ordered motion.

## Processing

`Sketch.run_state` polls once per frame and supplies the immutable event list in
`Frame.input.events`. An update folds it explicitly:

```ocaml
let update model (frame : Frame.t) =
  List.fold_left
    (fun model -> function
      | Input.Key_pressed Input.Space -> { model with paused = not model.paused }
      | Input.Closed -> Sketch.quit (); model
      | _ -> model)
    model frame.input.events
```

## Ownership and domain contract

Event polling/waiting, window state changes, text-input activation, and input
snapshot mutation occur on the initial OCaml domain. Blocking waits may release
the OCaml runtime only inside the audited SDL3 binding and reacquire it before
constructing an OCaml value.

No user handler receives native pointers. File-drop and text values survive
after the poll iteration because they are owned copies. `FileDropped` carries
the full dropped path; the event pump never reads the file, and one bad event
never discards the rest of a batch.

## Regression requirements

1. queue order across mixed key, pointer, text, resize, drop, and close input;
2. mapping of known and unknown keys/buttons;
3. logical coordinates and exact aggregate motion delta;
4. focus-loss state clearing and pointer-cancellation separation;
5. UTF-8 and IME composition ranges;
6. copied file-drop lifetime after native payload release;
7. resize/drawable synchronization;
8. wrong-domain rejection;
9. finite multi-frame native integration and orderly teardown.
