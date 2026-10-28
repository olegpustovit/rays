# Raster shaders from Rays Lisp (P9)

The P9 gate: a raster pipeline printed from Rays Lisp is no slower on the GPU than the handwritten
shader it replaces, and draws the same pixels (`lib/pxui/test_ui_parity` exact).

## Harness (P9.S1)

`tools/ui_shot.exe FILE.rays OUT.png` renders an editor frame offscreen without a window. With
`UI_SHOT_BENCH=N` it renders the settled frame N more times and prints each render's GPU time (the
difference of `Rays.Canvas.gpu_seconds`, the device's summed command-buffer GPU time, across the
render and a one-pixel readback), their median and their minimum. `UI_SHOT_SCALE=2` draws at two
pixels a point, a Retina window's frame (2880 x 1800 for the default 1440 x 900).

A/B: build `ui_shot.exe` at the base and copy it aside, build it at the change, then alternate the
two binaries in one shell loop, N = 9 renders each, and compare the per-run minimum (another process
on the shared GPU only ever adds time; medians swing from 3.7 ms to 130+ ms while the path tracer or
another agent's GPU test runs, minima do not). Pixels: `cmp` of the two PNGs; the editor's status
bar prints wall-clock cook times, so editor shots differ between any two runs and pixel identity is
checked by `test_ui_parity`, `test_ui_pipeline` and `test_marks_native` instead.

```sh
for i in 1 2 3 4 5; do
  UI_SHOT_BENCH=9 UI_SHOT_SCALE=2 ./base/ui_shot.exe sketches/ws_layout.rays a.png | grep min_gpu
  UI_SHOT_BENCH=9 UI_SHOT_SCALE=2 _build/default/tools/ui_shot.exe sketches/ws_layout.rays b.png | grep min_gpu
done
```

Scenes: `sketches/ws_layout.rays` (the design-reference editor: panels, glyphs, graph pane wires and
its dot grid, plus a Scene3 viewport), `sketches/shattered_studio.rays` (a heavy Scene3/World
viewport), `sketches/million_marks.rays` (the marks pipeline).

## Baseline (handwritten MSL, e1d07389, M1, 2026-10-28)

| scene | load | GPU ms a frame (median of 9) |
|---|---|---|
| ws_layout, 2x | 20 | 3.72 3.75 3.87 3.73 4.07 3.78 3.78 3.82 3.75 3.73 (median 3.78) |
| shattered_studio, 2x | 41 | 13.65 |
| million_marks, 2x | 41 | 28.4, 30.6 |

## Marks and UI from Lisp (P9.S3)

`lib/renderer/shaders/marks.rays` and `ui.rays` replace `marks.metal` and `ui.metal`; the
`.metal` files are now generated (`lib/flow_gpu/test_raster.exe --write lib/renderer/shaders`,
checked by `@lib/flow_gpu/runtest`). Pixels: `test_ui_parity` exact (kit panel, overlays, zones,
table), `test_ui_pipeline` matches Scene2 at 1x and 2x, `test_marks_native` keeps marks pixel for
pixel equal to the UI pipeline for rect, circle and line at 63/64/65/100000 instances, 1x/2x.

ws_layout at 2x, GPU ms a frame, minimum of 9 renders per run, base and change alternated:

| step | load | base (handwritten) | Lisp |
|---|---|---|---|
| one Packed program per varying, discard program first | 25 | 3.54 3.79 3.75 3.74 | 5.37 5.33 5.25 5.30 |
| handwritten fragment, generated vertex (isolating the fragment) | 23 | 3.74 3.77 3.54 | 3.77 3.73 3.77 |
| one program per stage (record outputs) | 7 | 3.72 3.73 3.70 | 4.99 4.96 4.97 |
| kind compared as a whole float, not a 64-bit int | 14 | 3.74 3.69 3.77 | 3.85 4.09 3.92 |
| raster float rounding without 64-bit (floor in the dot grid) | 30 | 3.74 3.73 3.66 3.67 | 3.82 3.84 3.86 3.83 |
| interpolated inputs read where used (Apple GPUs interpolate on read) | 8 | 3.74 3.69 3.68 3.74 | 3.83 3.80 3.78 3.83 |

Where the last 0.1 ms goes was not found. Without its `discard_fragment` the Lisp fragment measured
3.54-3.65 ms (faster than the handwritten 3.7), and moving the discard tests by hand into the branches
that decide them (as ui.metal had them) changed nothing measurable (3.81 against 3.70); division
guards (`b==0?0:a/b`) and the four `sample` calls of a glyph (the compiler shares them) are not it
either. At this load the run-to-run spread of the minimum is about 0.1 ms, so the UI pipeline is at
most 3% slower; that is recorded here, not hidden.

million_marks at 2x (marks pipeline, minimum of 9; PNGs byte-identical in every round):

| load | base (handwritten) | Lisp |
|---|---|---|
| 19 | 27.77 27.34 27.35 27.31 27.46 | 21.43 21.84 21.96 24.83 22.82 |
