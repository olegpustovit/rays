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

## Every raster pipeline from Lisp (P9.S3, S4)

`shaders/scene.rays` prints Scene3 (plain, points, textured), the sun's depth map, Scene3_world and
Scene2's argument-buffer pipeline; `shaders/image.rays` prints the Flow image sink (and the path
tracer's film resolve), which replaces its RGBA8 compute kernel and padded-buffer blit with one
full-screen triangle into the texture. `shaders/metal` holds only generated output.

Facing: the parked port's dark scenes were the renumbering, not the shaders. The batch's
counter-clockwise winding was inferred from "a draw binds vertex buffer 7" (Scene3's instance
transforms); renumbered, no draw bound 7, Metal kept its clockwise default and `[[front_facing]]`
read false on front faces. Winding is now pipeline state of the 3D families.

Pixels: the generated stages reproduce the MSL intrinsics' rounding (`gpu-codegen.md`): `dot` as
its fused chain, `normalize` by `rsqrt`, `cross`, `reflect` and `mix` in their fused forms, every
`a + b*c` the compiler contracted as an `fma`, `ldexp` for the double decode (GPU `pow(2, e)` is not
exact), `1.0f/2.2f` as the float the compiler folded. The image sink rounds to nearest-even in the
shader (k/255 survives the unorm store exactly; the store's own rounding is not ties-to-even).
Checked: `test_ui_parity` exact, renderer native goldens, `test_draw_snapshots`, the image sink's
native ties/clipping/gradient pixels (0 differing), and editor `ui_shot` frames identical outside
the status bar at 1x and 2x against the handwritten build for ws_layout, ws_facade, ws_orrery,
ws_bloom (World, sky, sun shadow), shattered_studio, shattered_cube, sop_gallery,
procedural_modeling, drawing, basic, generative, flow_image, flow_contours, ws_kit, pxui and
million_marks (each also identical base against base; voxel_wall is not deterministic run to run).

GPU minimum of 9 renders at 2x, base (handwritten) and Lisp alternated:

Load 3.4-6.9 (`uptime`), five alternated rounds each, minimum of 9 renders a round:

| scene (pipelines) | base (handwritten) | Lisp |
|---|---|---|
| ws_bloom (Scene3_world, sky, sun map) | 6.008 6.061 6.008 5.863 6.148 | 6.010 5.978 6.017 5.946 5.983 |
| shattered_studio (Scene3 lit, area lights) | 13.061 12.963 12.849 12.889 12.961 | 12.244 12.225 12.118 12.337 12.626 |
| ws_layout (UI, Scene3 viewport) | 3.838 5.351 5.278 5.976 3.843 | 3.834 3.845 3.884 4.321 3.898 |
| drawing (Scene2 argument buffer) | 0.798 0.746 0.746 0.786 0.742 | 0.747 0.741 0.774 0.744 0.747 |
| flow_image_kernel (image sink) | 2.304 2.293 2.401 2.308 2.295 | 2.286 2.295 2.300 2.290 2.289 |

What it took: shadow and area-light loops nested (rows, columns) instead of one counter split by
64-bit `div`/`mod` (shattered_studio 13.8 to 13.1 ms), a raster loop counting a capped pass from a
small constant in 32 bits (13.1 to 12.2 ms: faster than the handwritten 12.9), and `ldexp` for the
double decode (Scene2 0.745 to 0.741 ms).

## Open

The UI pipeline's last 0.1 ms (above). Handwritten MSL left in product code: the presentation blit
(`ogpu_metal/presentation.ml`, a backend detail below the renderer), the resident fold's stream
compaction (`Emit.compaction`: threadgroup memory and barriers, which stages cannot say) and the
`scene/trace` intersection helpers `Emit` splices into traced kernels.
