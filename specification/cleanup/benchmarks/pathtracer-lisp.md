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
