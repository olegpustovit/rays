# Path tracer benchmark (P8.S1 bar)

`render_workspace.exe --benchmark sketches/pathtracer.rays OUT.csv 16`, 480x840, 4 spp/frame, 5 bounces, M1, native Metal, pathtrace.metal kernel.

| round | wall_s (16 spp) |
|---|---|
| 1 | 0.5219 |
| 2 | 0.5389 |
| 3 | 0.5127 |
| 4 | 0.5200 |
| 5 | 0.5171 |

Median 0.520 s. All rounds RGBA8 sha256 d713e593d57074369a3afa0f919c6746343c3d41080217c8365dd89bd3feda5e. Setup 2.30 s.

## Lisp trace_pixel, first measurement (2026-10-28, indicative: machine load average 60 to 78, so not a gate run)

`lib/rays_pathtracer/test_lisp.ml`, plain scene (plane, one material, one rect light, sky/ground, one dome panel),
480 x 840, 2 spp per frame, 4 bounces, same camera and PCG draw order in both kernels.

Film: byte-identical resolved images on the CPU walker and on Metal at 16 x 12 (0 of 576 channels differ); at
96 x 72 x 16 frames 1 of 20736 channels differs (RMSE 2.7e-5); at 480 x 840 x 2 frames 8 of 1209600 (RMSE 1.0e-5).

GPU time per frame: pathtrace.metal 13.7 ms (three bands of 2.9, 6.6 and 4.3 ms); the Lisp kernel through the Flow
pipeline 93 to 105 ms (five dispatches). About 7x slower: the P8.G2 bar (not slower than today) is NOT met.
The emitted MSL is 8216 lines with 1142 finite checks and 64-bit integer registers for the PCG; the likely costs are
64-bit multiplies for the uint32 PCG, per-operation finite checks and one huge basic-block kernel. Timing must be
rerun under low load (load < 16) for the gate.

## Lisp trace_pixel against pathtrace.metal, interleaved (2026-10-28)

`PT_W=480 PT_H=840 PT_FRAMES=1 PT_BENCH=1 PT_SKIP_CPU=1 _build/default/lib/rays_pathtracer/test_lisp.exe` from
`lib/rays_pathtracer`: the same plain scene, 2 spp, 4 bounces; five pairs in one process, each a pathtrace.metal
frame (the sum of its bands' GPU time) then one Lisp dispatch (its command buffer's GPU time). M1, native Metal.
Load averages are the shared machine's at the run (other agents building).

| step | load | pathtrace.metal ms (pairs 2-5) | Lisp kernel ms (pairs 2-5) | emitted MSL lines |
|---|---|---|---|---|
| before (aa0bc4ac) | 16 | 9.07 9.06 9.18 9.25 | 104.4 98.8 105.1 101.1 | 8,215 |
| per-op atomics into a local word (measured as a hand edit) | 13 | 9.31 9.04 9.12 9.11 | 29.7 28.7 29.5 31.4 | 8,215 |
| + shared guard blocks, traces inside their guard, occlusion rays | 14 | 9.32 9.20 9.23 9.20 | 13.1 13.4 13.7 13.4 | 4,887 |
| + fast tier checks inputs and outputs, loop bodies keep only live code | 37 | 9.49 9.14 9.06 9.03 | 6.79 7.09 6.83 6.70 | 3,532 |
| + 32-bit unsigned narrow Int registers | 10 | 9.15 9.09 9.00 9.00 | 6.50 6.27 6.23 6.28 | 3,532 |
| + single-use values inline, one atomic per kernel (5b5fb376) | 34 | 9.19 9.07 9.09 9.24 | 6.38 6.20 6.14 6.22 | 1,513 |

The first pair of each run warms both pipelines and is left out. The Lisp kernel is now about 1.5x faster than
pathtrace.metal on this scene (6.2 against 9.1 ms), with the same film: 16x12 byte-identical on the CPU walker and
Metal, 480x840 RMSE 1.3e-5 (14 of 1,209,600 channels). The 13.7 ms above was pathtrace.metal's three bands of a
cold frame; interleaved, its bands sum to 9.0-9.3 ms. Static facts fold before emission: the scene's uniforms are
Lisp constants, so the light, panel and bounce counts and the material are literals in the kernel.

Where the 100 ms went (the emitted kernel read in full): 1,192 `atomic_fetch_or_explicit` calls on device memory,
one per checked operation; 1,647 one-instruction `if` blocks re-testing the same guards, with every register
declared at function scope; the NEE and panel shadow rays traced unconditionally (the call sat outside its guard)
and as closest-hit queries with all nine fields computed, where pathtrace.metal uses `accept_any_intersection`;
whole loop bodies kept for finite checks on dead values (unused trace fields); and the uint32 PCG as 63-bit
`long` arithmetic. Static shape now gated in `test_lisp.ml`: one atomic per kernel, no 64-bit arithmetic in the
loops but their bound checks, the emitted kernel within twice pathtrace.metal's lines.

CPU side (no CPU code path changed; base binaries rebuilt from the merge base and alternated with HEAD, five
rounds each, load 9 to 88): `tools/bench_core.exe` scalar core 102.1 -> 98.8 ns/eval, core fast 103.0 -> 98.8;
`bench_core kernels` and `bench_marks` medians move both ways by up to 2x under that load (8-domain rows swing
6 to 29 ms between rounds of the same binary), so they show no regression and no gain. The open scalar Core
regression ([scalar-core.md](scalar-core.md)) is untouched by this change.

Not done: `maxTotalThreadsPerThreadgroup` is not readable through OGPU yet (needs a pipeline-limits query on both
backends); the GPU printers still read Packed's guarded instruction list, rebuilt into blocks, rather than a
structured kernel IR (specification/gpu-codegen.md); pixel x/y split and the float-to-int index conversions keep
64-bit integers.

## World, bevel and static folding (2026-10-28)

Same command and machine; two scenes now (`PT_SCENE=dome|world`, both by default), the material round (0.3, four
bevel probes at the camera hit, one after). `world` is a 16 x 8 baked World with a gradient, a dome disc, a World
rect light and a sun, plus the scene's rect light. Film at 16 x 12 x 4 frames: byte-identical on the CPU walker
and Metal in both scenes; 480 x 840: RMSE 8.7e-6 (dome), 1.2e-5 (World).

| scene | load | pathtrace.metal ms (pairs 2-5) | Lisp kernel ms (pairs 2-5) | emitted lines | maxTotalThreadsPerThreadgroup |
|---|---|---|---|---|---|
| dome, no bevel (1daebeb7) | 23 | 9.47 9.05 9.23 9.03 | 6.47 6.33 6.35 6.42 | 1,796 | - |
| World, no bevel (1daebeb7) | 23 | 9.94 9.78 9.80 9.80 | 7.96 8.35 7.82 7.86 | 1,659 | - |
| dome, bevel (b0aa3bd7) | 24 | 13.15 13.19 13.23 13.18 | 11.39 11.40 11.54 11.02 | 1,309 | 512 |
| World, bevel (b0aa3bd7) | 24 | 13.96 14.38 14.09 14.16 | 12.92 12.62 12.51 12.56 | 1,534 | 448 |

The World terms of the kernel fold away in the dome scene and the dome panels in the World scene:
`Kernel_layout.specialize` decides guards on known conditions (here `u.world`, the light, panel and sun counts,
which the test passes as Lisp constants) and drops loops of no passes. The thread limits are the gate's floor.

## The tracer on its Lisp kernels (P8.S3-S4, 2026-10-28)

`Rays_pathtracer` now traces every mesh shape (flat, instanced, scene, spheres, strands, motion) with
pathtrace.rays through `Lisp_kernel` (one kernel set per scene shape; facts are literals, scene arrays and the
camera are bound per dispatch) and pathtrace.metal is deleted. The A/B was taken just before the deletion with a
binary that kept both kernels (`RAYS_PATHTRACER_KERNEL=metal`), then repeated for HEAD's Lisp kernels against
that binary's pathtrace.metal. `render_workspace.exe --benchmark SKETCH OUT.csv 16`, `RAYS_PATHTRACER_PROFILE=1`,
interleaved processes (Lisp then Metal, five pairs); GPU ms is the sum of round 5's band timings (16 spp, four
frames of 4 spp), wall the median round. M1; load averages are the shared machine's (other agents building).

| sketch (480x840 / 1600x1600) | load | Lisp GPU ms per 16 spp (pairs 1-5) | pathtrace.metal | medians | wall medians (s) |
|---|---|---|---|---|---|
| pathtracer.rays, before the tiling below | 4-16 | 563.6 570.6 573.1 573.4 571.2 | 504.9 509.9 502.6 514.9 504.6 | 571.2 vs 504.9 (+13%) | 0.582 vs 0.521 |
| pathtracer.rays, tiled (e19 binary) | 11-17 | 507.7 502.4 511.0 503.3 501.9 | 518.8 512.0 517.9 513.1 509.1 | 503.3 vs 513.1 (-1.9%) | 0.519 vs 0.521 |
| pathtracer.rays, HEAD (motion/strand port, old kernel deleted) | 21-26 | 495.4 493.2 490.4 492.1 494.0 | 498.7 497.6 494.5 497.1 493.5 | 493.2 vs 497.1 (-0.8%) | 0.505 vs 0.511 |
| shattered_studio.rays, tiled (e19 binary) | 6-66 | 34514 33932 35156 29337 30371 | 38657 38843 33580 35294 34577 | 33932 vs 35294 (-3.9%) | 34.4 vs 34.8 |

Film: pathtracer.rays at 64 spp, Lisp (HEAD) against pathtrace.metal, RMSE 6.9e-5 (36 of 1,209,600 channels
differ). The two M1 exact digests (`test_pathtracer_exact_m1`) still match byte for byte: no re-baseline.
`test_lisp` now compares the device film with the CPU walker: byte-identical at 16x12 (dome and World, flat and
`PT_INSTANCED=1`).

What made the difference: Flow dispatches 1-D (256 threads in a row), pathtrace.metal 16x16 threadgroups. In a
scene of many objects the rows' rays diverge; the tracing kernels now map a pixel buffer that lists the image
in 16x16 blocks of 8x4 SIMD tiles (the films hold pixel p at `order[p]`), which took pathtracer.rays from +13%
to -2%. Bound materials (a table instead of literals) cost about 10% on the one-material test scene (13.2 ms of trace kernel against 11.8 with
the material as literals, 480x840) and drop its thread limit from 512 to 448; `metallic`, `emissive` and `round` fold
when no material uses them. Per frame the resolve and Flow's RGBA8 conversion add about 0.6 ms.

Register pressure (`maxTotalThreadsPerThreadgroup` of the trace kernel): 448 flat (dome and World), 384 for an
instance structure; pathtrace.metal was 384 for every shape. The instanced kernel keeps 64-bit integer index
math in its loops (the instance's first triangle plus the hit prim), so the 64-bit gate covers flat kernels only.

Not met: first-frame compile time. `render_workspace` to a 4-spp PNG takes 2.5-4.0 s against 1.3-2.0 s with
pathtrace.metal (three runs each, load 20+). Of the Lisp side, `Flow.Workspace.check` of the generated workspace
(the three .rays files plus the six kernels) is 0.6-1.2 s, the trace kernel's bind 155 ms, template 52 ms,
emission 15 ms, then the Metal compile of the geometry and trace pipelines about 0.5 s. A shape change (light
count, World on/off, material classes) compiles again; four shapes stay cached per tracer. Next: check the
declarations once per process and reuse them across shapes.
