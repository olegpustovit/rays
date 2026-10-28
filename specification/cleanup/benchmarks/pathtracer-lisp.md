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
differ). shattered_studio at 8 spp: RMSE 2.75e-3 (2,670 of 7,680,000 channels, about 37 levels each) against
pathtrace.metal, where two Lisp runs differ by 3.1e-4 (the cooked fracture is not run-to-run identical): above
the 1e-4 bar, in a few pixels, not yet explained (candidates: geometric normals from scene/trace's instance
transform instead of transformed vertices, mirrored instances, visible rect lights in the primary-hit pass). The
two M1 exact digests (`test_pathtracer_exact_m1`) still match byte for byte: no re-baseline.
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

## G3: shattered_studio's film difference is floating-point chaos, not a semantic difference (2026-10-28)

Method: `tools/render_workspace` built from feb9966b (the last commit with both kernels), patched to render the
Lisp kernels, then `Private.use_metal` and pathtrace.metal, then the Lisp kernels again, in one process over one
cooked fracture (so the cook's run-to-run noise is out), dumping each RGBA8 film. Per-pixel diffs, and
pathtrace.metal instrumented to write one sample's hit, normals, NEE and shadow results beside a chosen pixel.

| comparison (shattered_studio) | RMSE | channels differ |
|---|---|---|
| Lisp vs pathtrace.metal, 1600², 8 spp, in-process | 2.70e-3 | 2,635 of 7.68M (1,124 pixels, spread evenly) |
| Lisp vs Lisp, same process | 0 | 0 |
| pathtrace.metal vs pathtrace.metal with one product reassociated in the camera ray (`px * (tan_half * aspect)`, as pathtrace.rays writes it), 1600², 8 spp | 2.12e-3 | 1,522 (622 pixels) |
| Lisp vs pathtrace.metal with Lisp's `normalize` (v * (1 / sqrt dot)) and that camera ray | 1.94e-3 | 1,399 |
| pinhole, 1 spp, 1 bounce: Lisp vs pathtrace.metal | 9.4e-4 | 41 (4 pixels > 2 levels) |
| Lisp vs pathtrace.metal, 400², 8 / 64 / 512 spp | 2.07e-3 / 1.03e-3 / 4.78e-4 | 170 / 816 / 2,406 of 480,000 |
| pathtrace.metal vs its camera-reassociated self, 400², 512 spp (two processes) | 3.76e-4 | 1,350 |
| Lisp vs Lisp, two processes (cook noise), 400², 512 spp | 6.0e-5 | 48 |

Root cause: shattered_studio is a hundred-thousand-triangle fracture with gaps, glossy shards and a 0.33 lens; a
one-ulp change to a ray direction moves a sample's hit across an edge or a gap, after which the two kernels
trace different (equally valid) paths with the same PCG stream, and at 8 spp a firefly-clamped sample is
1/8 of a pixel. pathtrace.metal against itself with one multiplication reassociated differs by 2.1e-3 — the
same size as against the Lisp kernel. The Lisp port computes `normalize`, `dot` and `cross` as scalar IR
(`v * (1 / sqrt (x*x + y*y + z*z))`, fma for cross) where MSL uses the `normalize`/`dot` builtins, and
associates some products differently, so no ulp-exact port exists short of emitting the same builtins in
the same order. Sample-level evidence: pixel (1273, 117), pinhole, 1 spp, 1 bounce: pathtrace.metal's camera
ray hits prim 27,324 of instance 1 at t = 8.23 with an unoccluded NEE sample (cos 0.53, radiance 3.8);
the Lisp kernel's sample there is black (its ray met a different surface). The differences fall as spp
grows (no plateau, so no bias) and at 512 spp sit at the 8-bit quantization floor (most differing channels
by one level). The gate's 1e-4 is below pathtrace.metal's own reproducibility on this scene at any
affordable spp: it is met on pathtracer.rays (6.9e-5 at 64 spp) and must be read against the
self-perturbation row above for shattered_studio.

Semantic differences found while reading both kernels, none in this scene: (1) an instance's geometric normal
is the normal matrix times the object normal (signed by the determinant for motion), where pathtrace.metal
crossed the transformed corners; for a mirrored instance (negative determinant) pathtrace.metal's flip then
pointed the shading normal away and replaced it with the geometric one (flat shading); pathtrace.rays keeps the
interpolated normal, which is the intended behaviour. (2) Round-corner probes also meet analytic spheres. Rect
lights stay invisible to camera rays without a World in both.

## G2: compile time (2026-10-28)

`Flow.Workspace.check ~call_sites` types a `defn` body only where it is called (it is checked again at each
call site anyway); the tracer's declarations are checked whole once in `test_lisp`. A scene shape now checks
its accumulation set (trace, geometry, resolve) when first rendered and its preview set on the first camera
move. Check of the accumulation set: 96 ms (six kernels, full) -> 49 ms. Tracer creation to the first
4-spp frame's flush, pathtracer.rays, M1, interleaved (load 6-22):

| | warm Metal cache (6 runs) | cold Metal cache (3 runs) |
|---|---|---|
| pathtrace.metal (feb9966b) | 200-218 ms | 1,414-1,506 ms |
| Lisp kernels before (feb9966b) | 505-578 ms | - |
| Lisp kernels now | 275-333 ms | 518-550 ms |

Cold, the Lisp kernels compile 2.7x faster than pathtrace.metal (its MSL is the larger compile). Warm, the
Lisp front end still costs ~80 ms per process and shape: the check (49 ms), the trace kernel's template
and emission (16 ms) and its library from source (~20 ms, the Metal cache hit included), where the cached
pathtrace.metal pipeline costs nothing at the first frame. Not met warm.

### The check, second pass (2026-10-28)

`tools/bench_check.exe N pt ptlib FILE.rays` checks in process: `pt` is pathtracer.rays's accumulation set
(`Rays_pathtracer.Private.check_sketch`), `ptlib` the declarations whole, a file as the editor opens it.
Median CPU of the "check" phase, allocation of the whole call, and instructions retired per check
(`/usr/bin/time -l`, 11 runs minus 1, /10), M1, load 7-12, interleaved with HEAD:

| | check ms before -> after | alloc MB | Minstr |
|---|---|---|---|
| pt (first-frame gate) | 45.5-46.4 -> 16.3-16.4 | 86.6 -> 33.1 | 603 -> 224 |
| ptlib | 160-162 -> 52 | 305 -> 108 | 2,007 -> 650 |
| sop_gallery.rays | 715-716 -> 215 | 1,279 -> 437 | 9,377 -> 2,964 |
| shattered_studio.rays | 91 -> 40-41 | 191 -> 109 | 1,696 -> 1,038 |

Causes (macOS `sample`; the List/compare frames lose their callers, so a temporary callstack-sampling shadow
of `List.mem`/`assoc_opt`/`Hashtbl` weighted by comparisons named the sites): half of the samples were
`compare_val` under polymorphic `List.mem`/`List.assoc_opt` on strings, each call a C call. The largest:
`Op.packed_kind` scanned the packed-op name tables on every operator application (now memoized per
builtin), `Syntax.attribute_args` scanned five name lists per call head (now a prefix test first),
`reserved`/`known` re-listed the type names per symbol, eager `Printf` messages for type errors that did
not happen, `called_names` hashing whole terms structurally (now the form id), `derive` sorting empty
`vary`/`groups` unions. Then the algorithmic one: a `defn` body was checked again at every call site even
with equal argument values; `apply_def` now shares the first check's body term for equal inputs (name,
call stack, depth, argument values, no function values), about two thirds of the calls in `pt`. The tracer's films
are byte-identical (render_workspace digests, `test_pathtracer_exact_m1`), as are all goldens.

Tracer creation to the first 4-spp flush (`render_workspace --benchmark sketches/pathtracer.rays OUT 4`,
`pathtracer_first_frame_ms`), warm Metal cache, 12 interleaved pairs HEAD/now, load 6.9-9.3: on a quiet
GPU 275-280 -> 256-260 ms; every pair faster, median 30 ms (pairs 279.8/258.6, 278.9/276.0, 274.9/259.7,
299.4/257.0, 385.8/348.4, 382.4/357.8, 378.9/352.4, 419.9/348.1, 424.5/390.8, 443.7/256.4, 450.9/373.1,
458.3/378.5; the 350-450 runs share the GPU with other processes). The Lisp front end of that frame is now
33 ms: parse 2, check 16, the trace kernel's template 6 and emission 8; `create_library` plus the pipeline
take 0.3 ms with the Metal cache warm (the earlier ~20 ms estimate was not the library). The rest (tracer
creation ~50 ms, World bake ~25 ms, the 4-spp GPU frame ~120 ms) is shared with pathtrace.metal, so 200-218
is not reachable from the front end alone under today's load; the 200-218 rows were not re-measured
(feb9966b not rebuilt). Cold not re-measured: the Metal cache is per user, shared with every process on the
machine; the CPU side is the same cold. Still not met warm.

The golden replay (`REPLAY_JOBS=4 replay.exe data`, 819 rows): 84 -> 68-71 user-s, 29-30 -> 24-27 s wall;
it is now mostly `Flow_ir.Packed`/`Core_region` lowering and stop-the-world minor GCs, not the check.

GPU (render_workspace --benchmark, 16 spp, round 5 band sums, interleaved, Lisp then pathtrace.metal):
pathtracer.rays 492.5 vs 495.4 ms (medians, load 8.7-10.9; pairs 496.1/496.6, 494.6/494.7, 492.5/495.4,
491.7/497.2, 492.1/497.2). shattered_studio at 800 x 800 (the cook is the same each run): 7,823.7 vs 8,661.1 ms (-9.7%; load 6.5-65; pairs 7857/8554, 7740/8750, 7824/8775, 7814/8644, 7878/8661).

Instanced kernels: an analytic hit's triangle is now a select (`(if analytic 0 ...)`) instead of
`(max ... 0)`, whose signed compare forced the instance's first-triangle-plus-prim sum into 64 bits; the
"no 64-bit arithmetic in loops" gate in `test_lisp` covers the instanced dome scene too (thread limit 384).

Bound materials stay bound: folding material values as literals would recompile the kernels on every
material edit in the editor; the classes (metallic, emissive, round) already fold.
