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
