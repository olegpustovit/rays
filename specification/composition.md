# Rendering composition

## Paths

`Path.t` is immutable geometry built with pipeline-friendly `move_to`,
`line_to`, quadratic and cubic curves, and `close`. Each `move_to` starts a
separate contour. `Scene.path` supplies fill, stroke, curve detail, and an
explicit `Path.Even_odd` (default) or `Path.Non_zero` fill rule. Curves are
flattened only at the raster boundary, so paths remain reusable data.

Fills use contour-aware tessellation before native command submission. This
supports concave geometry and transparent holes without painting a fake
background shape. Strokes are emitted per contour and never bridge separate
subpaths.

## Scoped clipping

`Scene.clip ~at ~w ~h children` is a scene node. Its rectangle is transformed
to an axis-aligned device-space bound, intersected with any outer clip, and
restored after the children render. This gives nested clipping without leaking
renderer state into later scene nodes.

## Offscreen canvases

`Canvas.t` owns bounded RGBA8 pixel storage with an explicit native resource
mirror. It supports:

- reading one pixel or a row-major immutable pixel copy;
- setting or mapping pixels;
- alpha-masking one same-sized canvas with another;
- creating or refreshing an owned `Image.t` snapshot;
- capturing the current Metal drawable;
- saving the canvas or framebuffer as PNG.

Pixel editing remains deterministic and explicit. Presentation, scene drawing,
image sampling, capture, and readback use the native OGPU/Metal path; Canvas is
not a selectable software renderer or an alternate application backend.

Canvas dimensions are explicit pixels belonging to that offscreen surface.
They are not implicitly multiplied by the display scale. A scene rendered into
a `256 × 256` canvas therefore produces a `256 × 256` image resource.

Screen capture is the native-pixel boundary. `Canvas.capture` queries the
active renderer output size, allocates exactly that many RGBA pixels, and reads
the complete framebuffer. `Canvas.save_screen_png` preserves those dimensions.
Consequently, a logical `800 × 600` window with a `(2., 2.)` pixel scale saves
a `1600 × 1200` PNG. Code that needs the current relationship can compare
`Frame.input.size` with the drawable size supplied by the renderer.

An alpha mask is explicit and composable:

```ocaml
Canvas.map_pixels source (fun ~x:_ ~y:_ color -> color);
Canvas.map_pixels mask (fun ~x ~y _ ->
  if Float.hypot (float (x - 128)) (float (y - 128)) <= 96.
  then Color.white else Color.transparent);
Canvas.apply_mask ~source ~mask
```

Only mask alpha is used, so colored masks behave predictably.

Canvas and image resources are explicitly owned. `Sketch.run_state` provides
`on_stop`, which runs while SDL3 and Metal are still active:

```ocaml
let stop model =
  List.iter (function Ok () -> () | Error message -> prerr_endline message)
    [Image.destroy model.texture; Canvas.destroy model.canvas]
```

## Effect boundaries

- `Path` construction and `Scene` construction are pure.
- Pixel mutation, native-resource uploads, capture, and export are explicit
  effects; native resource operations are initial-domain-only.
- Scene geometry remains in logical coordinates; capture alone exposes native
  drawable pixels.
- `Canvas.pixels` returns a copy; it never exposes backend-owned memory.
- `Canvas.to_image` returns a new owned texture on every call.
- Capture and export return `result` with backend error messages.

This keeps the common view pure while still making deliberate framebuffer work
available when a sketch needs it.
