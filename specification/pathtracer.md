# Path tracer

`lib/rays_pathtracer/` is a progressive Monte Carlo path tracer that runs on
OGPU's ray-tracing surface (Metal backend: a primitive, instance, or motion
instance acceleration structure plus an `intersector` whose tags follow the
mesh shape inside one compute kernel). It is the foundation for real-time path-traced
demos; the demo scene is `sketches/pathtracer.rays` (Lisp; the editor's Path traced renderer).

## Scope

- Input: a list of `Rdk.Geometry.t` objects, each with one metallic-roughness
  material (`albedo`, `roughness`, `metallic`, `emission`), plus analytic
  `sphere`s (bounding-box primitives resolved by an intersection function, so
  they are exact and never tessellated) and `strand`s (round polylines of
  constant thickness traced as linear curve segments; they need
  `Ogpu.Caps.Ray_tracing_curves`, so on the M1 `create`/`replace_mesh` return
  a typed error rather than a silent miss).
- Instancing extras: `mesh_instanced ?materials` gives each instance its own
  material through the instance user id (the prototype material is index 0),
  and `?motion` gives each instance a second transform at the end of the
  shutter. Both keyframes are stored per instance; the kernel picks a shutter
  time per sample, so instances blur along their motion while the preview
  renders the shutter midpoint.
- Several objects: `scene_mesh [(transform, mesh); ...]` places prepared
  triangle meshes (flat, instanced, or earlier scenes) at world transforms in
  one trace: a TLAS whose instances reference one BLAS per input over one
  shared vertex buffer. A flat input is one instance, an instanced input one
  instance per copy with its transform premultiplied by the object's.
  Spheres, strands, and motion inputs are rejected (ponytail: the sphere
  intersection function has no per-instance base).
- Moving objects: `move t matrices` re-places the newest `scene_mesh`'s
  objects at new world transforms without re-preparing or re-uploading
  geometry. The layout keeps each object's placement range, world matrix,
  and per-placement local matrix; only changed objects recompute
  `object * local`, then fresh instance records and shader transforms go up
  and only the TLAS rebuilds over the installed BLASes (staged like
  `queue_mesh`; a newer move supersedes a pending one, and a move behind a
  full build waits for it). On the M1, 2001 instances plus a 20k-triangle
  mesh: `move` + `flush` about 2 ms (0.4 ms on the CPU) against about 8.5 ms
  for `scene_mesh` + `queue_mesh` + `flush` (`test_scene`).
- Lighting: rectangle area lights (`rect_light`: centre, target, size, colour,
  intensity; two-sided, analytic, never visible) sampled with next-event
  estimation; `light_of` converts a raster `Rays.Light.t` (1/d²
  attenuation) to the rect of the same irradiance `pi I / d²`: radiance
  `pi I / A`, the inverse of the raster World conversion `L A / pi`; point
  and spot lights become 0.1 x 0.1 rects, a directional light a 20 x 20 rect
  100 units away. Plus a procedural dome that stands in for a studio HDRI: a
  sky/ground gradient with soft rectangular emitter panels defined by a dome
  direction, angular half-extents, edge softness, colour, and intensity.
  Emissive materials also work as BSDF-sampled lights.
- Round corners: a per-material `round` radius applies a render-time shading
  bevel in the style of Redshift's Round Corners / Cycles' Bevel node. At each
  camera-visible hit (and, with a quarter of the probes, the next bounce) short
  rays through random points of a disk of that radius, projected along the
  normal (p = 1/2) or either tangent (p = 1/4 each), gather every surface
  inside the sphere. Each hit normal is weighted by the inverse of the mixed
  projection pdf, so the blend is an unbiased estimate of the area-weighted
  mean normal within the radius, including faces parallel to the shading
  normal. The estimate is stochastic per sample and converges under
  progressive accumulation, so four probes are enough. Flat interiors are
  unchanged; hard mesh edges shade as rounded fillets without extra geometry.
- Output: a borrowed `Rays.Image.t` whose completed film is an OGPU sampled
  texture in a native Sketch. `pixels` requests an explicit RGBA8 readback for
  tests and export. Standalone tracer use without a Sketch still publishes a
  CPU image after command completion.
- Camera: `eye`/`target`/vertical `fov`, pinhole by default. The camera's
  `Camera.lens` is thin-lens depth of field: `aperture` is the lens radius in
  scene units and `focus_distance` the sharp plane along the view direction
  (`None` focuses on the target). Each
  accumulated sample starts from a uniform point of the lens disk and aims
  at the pinhole ray's focus-plane point (two random numbers per sample, no
  extra rays), so bokeh converges with the rest of the image; preview frames
  stay pinhole. A lens change is a camera change. A camera change restarts
  accumulation and renders every primary pixel as an interactive preview:
  one direct-light bounce and one round-corner probe. The preview reprojects
  the last completed linear frame by the current hit position and previous
  camera, rejects depth or normal mismatches, and resolves a 3×3 neighborhood
  only across matching surfaces. History is capped at twelve frames and reset
  on geometry changes. The first frame after the camera rests restarts
  full-quality progressive accumulation.
- Residency: the trace dispatch binds the instance structure and declares
  its bottom-level structures read (`Ogpu.Backend.compute_use_accels`);
  Metal only keeps referenced structures resident when told, and without it
  the BLASes were evicted over minutes, rays missed more and more, and the
  picture faded to the background (a fresh frame after `reset` got darker
  with every frame rendered). The library test renders a large instanced
  prototype for sixty frames and requires a fresh frame's brightness to match
  the first.
- Interaction contract: `render` never blocks. It polls the in-flight command
  buffer and, while the GPU is busy, returns without submitting, so the
  sketch loop, camera, and UI keep the display rate regardless of tracer
  cost; `flush` is the explicit synchronous wait for tests and export. This
  keeps the CPU loop decoupled from GPU frame time. Preview cost still needs
  a measured budget on the M1.
- Geometry swap: `mesh` flattens `(Rdk.Geometry.t * material)` pairs into a
  pure triangle soup that a SOP cook worker may build off the initial domain.
  `queue_mesh` uploads it and submits a replacement acceleration build without
  waiting. The previous scene remains visible until the build completes and
  all earlier renders finish; then the renderer swaps resources and resets
  accumulation. Edits received during a build coalesce to the latest mesh.
  The first frame over newly installed geometry, lights, or World renders
  as a preview, like a camera move, so a live slider drag stays interactive;
  accumulation resumes one frame after edits stop, and `reset` restarts at
  full quality (1200x1560, 4 bounces, rounded voxel scene on the M1: preview
  about 61 ms against 218 ms for a full first sample, `test_main.exe
  bench_scene`).
  `replace_mesh` remains a synchronous version for tests and export.

Not in scope yet: thin-lens depth of field, textures, learned denoising,
file-backed HDRIs (a baked `Rays.World` is the environment; see World path).

## Pipeline

1. The flat `create` path triangulates every object with `Rdk.Triangulate.run`, computes
   vertex normals with a 40° cusp so subdivided/rounded surfaces stay smooth
   while boxes stay hard, and flattens everything into one unindexed
   `packed_float3` position buffer, a matching normal buffer, and one material
   index per triangle. A single primitive acceleration structure is built and
   the scratch buffer is released after the build completes. Packed geometry
   keeps one prototype triangle buffer and BLAS, uploads backend-packed
   instance descriptors (`Ogpu.Backend.pack_instances`) and shading
   transforms, and builds a TLAS. Both builds are encoded on the tracer's
   queue and polled, never waited on, unless `flush` or `replace_mesh` asks.
   Exact prototype-byte matches reuse the GPU prototype and BLAS on later
   edits; only transforms and the TLAS change. A Metal primitive structure
   holds one geometry kind (a triangles-plus-boxes BLAS fails at sizing with
   an unrecognized `vertexBuffer` selector), so OGPU rejects mixed-kind BLAS
   descriptors and a mesh mixing triangles, spheres, and strands becomes a
   scene: one BLAS per kind under identity TLAS instances. A scene's BLASes
   build side by side in slices of one scratch buffer and the TLAS builds in
   the next encoder of the same command buffer, uncompacted (ponytail).
   The `SCENE` kernel binds a per-instance table (first triangle, material or
   `~0` for per-triangle ids) at buffer 20. Every build is
   staged: the primitive build writes its compacted size, the next poll
   compacts it into a right-sized structure, then the TLAS (user-id or motion
   instance records) builds over the compacted one and the uncompacted
   structure is released; `render` never waits on any stage.
2. The embedded MSL is compiled once into one OGPU library at `create` time.
   One `pathtrace` kernel is specialized per mesh shape by the `INSTANCED`,
   `MOTION`, `SPHERES`, `CURVES`, and `SCENE` function constants (mutually exclusive
   structure arguments, a body templated on the structure, intersector, and
   table types) and compiled on first use; a sphere pipeline links the
   `sphere_hit_*` intersection function whose tags match its intersector and
   owns a one-entry intersection table. `resolve_preview` comes from the same
   library.
3. `render` polls the previous submission and returns if it remains busy.
   Once complete, it publishes the completed GPU texture behind the borrowed
   image's stable identity without a CPU transfer or Scene re-upload. It then
   uploads a 288-byte uniform block (camera basis, dome colours, size, frame
   index, spp, bounce cap, exposure, round-corner probe count, light count,
   previous camera, World flags and sun)
   with `set_bytes`, dispatches one thread per pixel directly into the other
   of two RGBA8 OGPU textures, and commits without waiting. The textures
   alternate even during motion, so Scene never samples the texture being
   written. `flush` makes the pending frame synchronous. A
  camera change retains valid completed preview history; an explicit reset
   marks older in-flight frames stale. The accumulation buffer is private device memory holding `float4`
   (RGB sum, sample count) per pixel; frame index zero overwrites it.

## Shading

- Per bounce: geometric normal from positions, shading normal from
  barycentric-interpolated vertex normals, both flipped to face the ray.
- BRDF: Lambert diffuse scaled by `(1 - F)` and `(1 - metallic)` plus GGX
  specular with Smith height-correlated visibility and Schlick Fresnel.
  Roughness is clamped at 0.03 so mirrors stay stable; alpha is
  roughness squared.
- Sampling: one-sample mixture between GGX NDF sampling and cosine sampling
  with the specular probability driven by metallic and the Fresnel term; the
  mixture pdf keeps the estimator unbiased.
- Direct light: one rectangle light chosen uniformly per bounce, a uniform
  point on it, one any-hit shadow ray, area-to-solid-angle pdf. Because the
  lights are not geometry, BSDF-sampled rays never double count them.
- Dome panels use multiple importance sampling. One panel is chosen uniformly
  and a direction is drawn uniformly over its angular rectangle (including
  the soft border); the solid-angle pdf of that mixture is
  `z / ((x² + z²)(y² + z²) · 4 wx wy)` per covering panel in the panel frame.
  Light samples are weighted `p_l / (p_l + p_b)` and BSDF-sampled escapes
  `p_b / (p_l + p_b)` (balance heuristic); camera rays that escape carry the
  full environment. The sky/ground gradient has no sampler of its own, which
  is unbiased because `p_b > 0` on the whole shading hemisphere. The furnace
  check in the library test verifies the estimator against the analytic
  value with and without a panel.
- Russian roulette from bounce three, per-sample luminance clamp at 8 to
  suppress fireflies (biased, documented as a `ponytail:` corner), ACES-style
  tonemap and 2.2 gamma on resolve.

## World path

`set_world t (Some baked)` replaces the procedural dome with a baked
`Rays.World` (`specification/environment.md`); `None` restores the dome,
and a sketch that never sets a World renders bit-exactly as before (the M1
qualification digests are unchanged). The upload happens only when the baked
value changes (physical equality) and restarts accumulation.

- Data: one float32 storage buffer (binding 8): camera map, lighting map
  (both `w x h` RGB, the World's equirect convention), the CDF marginal
  (`h + 1`) and conditional rows (`h x (w + 1)`). Lookups are manual
  bilinear, u wrapping, v clamped, matching `World.lookup`. The uniform block
  (now 288 bytes) carries the flags, map size, background colour, and sun.
- Camera rays that miss read the camera map (sun disc and promoted emitters
  included), or `Color c` as radiance `c` (exposure and tone map still
  apply), or for `Transparent` nothing: the accumulation's `w` then counts
  covered samples, the colour is their mean (straight alpha) and the film's
  alpha is coverage. The preview resolve marks alpha 0 where the centre
  sample missed.
- Later bounces read the lighting map. Next-event estimation draws a
  direction by inverse CDF (binary search on the marginal, then the row;
  the leftover of each uniform places the direction inside the texel). Its
  solid-angle pdf is the CDF's own texel probability times `w h` over
  `2 pi^2 sin theta` at the sampled direction, so sampling and evaluation
  agree exactly. BSDF escapes are weighted against that pdf.
- Sun: a cone of `angular_radius` around `baked.sun.direction`, sampled
  uniformly (`1 - cos` carried as `2 sin^2(r/2)` so small discs keep float
  precision; pdf is one over the cone's solid angle). BSDF escapes inside
  the cone add the sun radiance. The lighting map has no disc, so nothing is
  counted twice.
- Rect lights: `baked.lights` (one-sided, emitting along their normal) join
  the `set_lights`/scene lights (two-sided) in one buffer, and become
  hittable. A camera ray stops at the nearest rect in front of the scene;
  later rays add every rect they cross before the scene hit, weighted against
  the rect NEE pdf (area to solid angle, one light of `n` picked uniformly),
  and continue. Shadow rays ignore rects, so both strategies see the same
  emitters.
- MIS: every light strategy (map, sun, rects) against BSDF sampling with the
  power heuristic; the BSDF pdf is the mixture pdf the sampler uses. The
  dome's `sample_panel` is not used on this path.
- Firefly clamp: only the indirect part (light arriving after the first hit)
  is clamped at luminance 8; direct light is MIS-sampled and unclamped.
- Exposure: the effective exposure is `2 ** baked.exposure` times `create`'s
  `exposure`.
- `~bsdf_only:true` at `create` disables all light sampling
  on the World path (debugging and the unbiasedness test).

Historical native checks: a uniform-radiance
World furnace resolves to 224 like the dome furnace (a mutation that drops
the MIS weights reads 241); a peaked map with a sun converges to the same
image with MIS as with BSDF-only sampling (mean 34.04 vs 33.93 on RGBA8) and
at 16 spp has RMSE 6.6 vs 17.7 against the converged image; a `Color`
background resolves to exactly its tone-mapped value; `Transparent` misses
have alpha 0; a rect light behind the camera appears in a mirror sphere under
BSDF-only sampling (so only by being hit) and MIS agrees.

Current audit (2026-10-11): the formerly empty `test_world.run` now renders
each of these behaviors, reads GPU pixels and checks sample counts and handle
teardown. Its executable builds, but the forced native attempt fails because
this runner has no Metal device; convergence bounds need native validation.
The formerly vacuous qualification `test_parity.run` now renders the four
built-in Worlds on matte/glossy probes through raster and pathtracer, checking
mean luminance and highlight centroids after 1,024 samples. Its explicit
20% mean-error and 8-pixel centroid bounds are provisional and need native
qualification; typechecking, builds and the dependency gate pass, while the
native attempt fails at device acquisition. The historical measurements above
do not establish current coverage. World and pathtracer checks belong to the native/qualification
aliases, rather than the window-free default `runtest`.

## Determinism and tests

The RNG is a PCG hash seeded from pixel index and frame index, so a fixed
camera and frame sequence reproduces byte-identical output. The library test
(`lib/rays_pathtracer/test_pathtracer.ml`) renders a lit sphere over a
floor twice from reset and asserts identical bytes, a non-flat image, and
opaque alpha; it then checks a white analytic sphere in the furnace (energy
conservation through the intersection table), red/green per-instance
materials, motion-blur coverage between an instance's rest and end positions,
typed rejection of strands on devices without curve intersection, mismatched
material and motion counts, and that all sixteen kernel specializations
(including the curve and scene variants) compile against their declared
interfaces. Under `@qualification`, `test_pathtracer_exact_m1` runs the same
test and compares the MD5 of two of its frames (the lit sphere's first four
samples and the instanced sphere) with digests recorded on an Apple M1; another
chip prints that it does not compare. The comment on `exact_m1_digests` says
when a digest may be refreshed and what has to be shown first. `test_scene.ml` checks that a `scene_mesh` of a flat box, an
instanced pair with per-instance materials, and a floor matches the same
meshes composed by hand, that translating an object moves it in the image,
that a mesh of triangles plus an analytic sphere builds and shows both, and
that `light_of` area, point, spot, and directional lights light a white matte
plane to the analytic `albedo I / d²` within 6%, with no handle leak. It skips when no ray-tracing device is present unless
`RAYS_REQUIRE_RAYTRACING` is set. `test_gpu_film.ml` creates a native
renderer, verifies direct texture publication and explicit pixel readback,
compares its frame byte-for-byte with the standalone path, then checks teardown
has no live Metal handle delta after 30 further frames. It also renders the
GPU-backed image through an independent offscreen Canvas target. The Scene execution
Metal test checks exact sampling, zero texture-upload bytes, and rejection of
malformed, foreign, and destroyed GPU textures.

The 2026-10-11 restoration wires the formerly unused pixel counters in
`test_pathtracer` and `test_scene` to explicit GPU film readback, including
lens determinism, preview detail, instance residence and mixed-geometry
visibility. Both executables build and typecheck. Forced native runs on the
managed runner fail at Metal device acquisition, so the numerical assertions
and thresholds remain unverified here.

`test_gpu_film`'s 2026-10-11 restoration now actually captures the standalone
frame before window creation and compares complete shared-device texture,
explicit image-readback and sampled Canvas bytes. Sample counts, dimensions
and standalone lease teardown are checked as well. Its build and dependency
checks pass; the native attempt also fails at device acquisition here.

## Performance

Run the finite native throughput probe with `dune build --force @tools/bench-pathtracer`.
Set `RAYS_PATHTRACER_FRAMES`, `RAYS_PATHTRACER_SCALE`, or
`RAYS_PATHTRACER_SPP` to override its defaults (240, 1, and 1). This alias
opens a native window.

An earlier path-tracer benchmark run (default Dune profile) measured
roughly 35 ms/frame on an Apple M1 (no hardware ray tracing) for the example
scene (seven round-cornered cubes, two rectangle lights, one dome panel) at
480×840, one sample per pixel per frame, five bounces, four round-corner probes
(eight probes: 39 ms). That run preceded GPU film publication; its CPU
readback no longer occurs in a native Sketch. `RAYS_PATHTRACER_SCALE=2` renders at
the Retina drawable size at proportionally lower throughput. Further speed
needs fewer GPU rays per pixel: adaptive probe counts, or an M3-class GPU with
hardware ray tracing.

For the 2,065-instance voxel wall, a one-off probe
on an Apple M1 (arm64, one domain, default Dune profile) measured 9.6 ms and
3.42 million minor words for CPU triangle expansion before prototype decoding
was hoisted out of the instance loop; the same single-run probe measured
5.5–6.1 ms and about 0.79 million minor words afterward. The flattened
24,780-triangle upload and synchronous Metal acceleration build still took
6.2 ms in that probe. `RAYS_PATHTRACER_FRAMES=90 RAYS_PATHTRACER_ORBIT=1
dune exec rays -- sketches/voxel_wall.rays` measured 19.9–29.4 ms per application
frame on warm/cold runs with full-resolution moving visibility; these totals
include startup, cook, presentation, and GPU work, so they do not isolate ray
dispatch. A stationary 90-frame run measured 37.0 ms/frame and 63 accumulated
samples. Peak live memory was not measured.

After moving wall edits to `queue_mesh`, the same 24,780-triangle benchmark
measured 0.4 ms on the caller for upload and submission, with 3.5 ms of GPU
build remaining when explicitly waited on by `flush`. This moves the build
wait out of the application update; it does not reduce flattened geometry or
the total GPU build cost.

The instanced path now uploads the cube prototype as one primitive structure
and compact transform descriptors as a top-level structure. The earlier
2,065-instance synthetic benchmark measured 3.5–4.6 ms for pure transform
preparation, about 0.5 ms to submit the Metal build, and 1.8 ms remaining
when explicitly waited on.
At 560×800 in `--compare-render`, twelve stationary frames averaged 15.5 ms
with instances versus 23.8 ms for flat triangles on this M1. That probe uses
a farther camera than the interactive wall and includes readback; it does not
establish an orbit speedup. The 120-frame native stationary wall run averaged
66.1 ms/frame with 114 samples, including startup and UI work.
The affine 3×3 normal-transform preparation brought the 2,065-instance
worker step to 1.7–1.9 ms and reduced minor allocation to about 0.26 million
words. With the prototype already resident, `--gpu` measured a 0.3 ms queued
edit call and a 1.5 ms explicit build wait. These are single-run observations;
the 560×800 matched-camera probe above is the rendering comparison.
The actual 35×59 division wall has 36×60 = 2,160 points and 25,920 triangles;
the synthetic benchmark now matches that cardinality. Its updated transform
step measured 2.2 ms, and a reuse edit measured 0.4 ms to submit plus 1.6 ms
to wait. The corrected finite orbit stays in front of the wall and, with
temporal reconstruction plus spatial resolve, measured 24.3 ms/application
frame over 90 frames without PNG capture on the same M1. `Canvas.save_screen_png`
adds a large one-time capture cost to the short finite run, so its total is
not used for the orbit throughput figure.
With direct GPU film publication and selected-mode-only geometry preparation,
five warm 90-frame orbits reported 20.3, 19.6, 19.5, 19.8, and 19.6
ms/application frame on the same M1 (median 19.6 ms); a colder run reported
26.7 ms. A stationary 90-frame run reported 39.4 ms/application frame and
74 accumulated samples. The earlier figures are separate runs with differing
changes; they do not isolate a frame-time gain from the film path. The film path eliminates the
per-frame CPU readback and Scene texture upload by construction, and the
native sampled-texture test measures zero upload bytes for that draw.
The matching 2,160-instance `--gpu` benchmark then measured 1.7 ms and
263,432 minor words for pure transform preparation, 9.1 ms for first upload
and BLAS/TLAS build, and 0.3 ms to queue a reuse edit plus 1.5 ms at an
explicit wait. These are single-run measurements under the default Dune
profile, not frame-time percentiles.
At the benchmark's farther fixed camera, 64 stationary samples averaged
18.0 ms/frame with the instance TLAS and 27.5 ms/frame after flattening the
same 2,160 cubes. The images have a 1.229 mean absolute channel difference
on RGBA8 (signed mean 0.004). The mean difference fell from 2.195 at twelve
samples, consistent with stochastic variance. This is a visual and throughput
comparison, not byte-identical output parity.
`RAYS_PATHTRACER_PROFILE=1 RAYS_PATHTRACER_FRAMES=90
RAYS_PATHTRACER_ORBIT=1 dune exec rays -- sketches/voxel_wall.rays` logs the
completed path-tracer command buffer's Metal GPU time per frame. On this M1,
89 completed frames had median 11.43 ms, p95 12.31 ms, mean 11.44 ms, and
maximum 19.42 ms; the 90-frame application mean was 20.4 ms. GPU timestamps
cover tracing and preview resolve, while the application total also includes
startup, cook, UI rendering, scheduling, and presentation. PNG capture is
excluded. This identifies the ray workload as a substantial cost without
attributing the remaining time to a single phase.

World convergence probe (headless, 2026-09-26, Apple M1, default profile):
`dune exec lib/rays_pathtracer/test_main.exe -- bench_world` renders a
floor and a 48x24 UV sphere at 256x256, 1 spp per frame, 6 bounces, in three
interleaved rounds. Wall time per `render`+`flush` frame (Metal GPU
timestamps read 0 on this machine), and RGBA8 RMSE against each
configuration's own 2048-frame reference:

| Configuration | ms/frame (3 rounds) | RMSE @16 | RMSE @64 |
|---|---|---|---|
| procedural dome, 1 panel (no World, before) | 3.62 / 3.97 / 3.93 | 3.52 | 1.68 |
| white room, panels in the maps (CDF + MIS) | 4.11 / 4.22 / 4.40 | 3.01 | 1.36 |
| white room, 6 promoted panels (rect NEE + hittable) | 6.35 / 6.32 / 6.21 | 3.07 | 1.43 |

The references differ per row, so RMSE compares convergence rates, not
images. The promoted room pays for testing six rects on every ray and a
third shadow ray per bounce. Timings exclude tracer creation and one warm-up
frame.

## SOP workflow example

`sketches/voxel_wall.rays` is the Houdini-style network Grid → Wall Depth → Copy
Cubes (cube prototype), hosted in the `Rays_editor.Editor` workspace. Its
`wall_depth` definition is written in Rays Lisp; its typed parameters
(frequency, amplitude, base depth, octaves, seed) drive the inspector and it
writes the per-point `scale` attribute, the equivalent of an Attribute
Wrangle. `copy_cubes` uses the built-in `sop/copy_to_points` with a "Pack and
instance" choice, Houdini's Copy to Points pack toggle. Packed, its
cook is O(prototype + targets): the prototype polygons followed by one loose
point per copy carrying `scale`; there are no fake packed primitives inside
`Rdk.Geometry.t`, and `prepare` splits referenced points (prototype) from
loose points (instance transforms) on the cook worker. The rasterizer draws
that with one indexed Metal instance batch through `Scene3.instances_array`;
the tracer uses `mesh_instanced` to keep a single prototype BLAS and build a
top-level instance structure. Materialized, the node calls
`Rdk.Instance_copy.copy_to_points`. A "Renderer" choice in the inspector's camera panel
(the `~inspector` hook) switches between the path tracer, filled Scene3 raster,
and a Scene3 wireframe made from indexed Metal lines along unique RDK topology
edges. The Box catalog defaults to six quads, so the wireframe follows the
modeled faces without triangulation diagonals. The wireframe uses the Metal
scene path and preserves packed instancing. Native `Scene3.Wireframe` also
supports triangle meshes, where it follows the mesh's triangle edges.
The renderer is a sketch `Settings` choice: switching it is one undo step and
recooks, since `prepare` receives the settings. All
graph and parameter edits go through the workspace's shared undo stack.
Each background preparation now builds only the selected renderer's derived
mesh: the traced prototype, filled mesh, or unique-edge wire mesh. Switching
modes requests a fresh preparation; the prior displayed scene remains valid
until that work completes.
`RAYS_VOXEL_RENDERER=wireframe|raster` selects a mode for finite native
smoke runs; the default remains path traced.
After this selection change, a 90-frame finite orbit on the same M1 reported
19.6 ms/application frame. The earlier 24.3 ms run was a separate measurement;
the difference is not an isolated preparation-speed measurement.

## Dependency direction

`rays_pathtracer` depends on `rays`, `ogpu`, `rdk`,
`runtime_resources`, and `rays_execution`. Every GPU object is an
`Ogpu.Backend` handle on a device leased through
`Rays_execution.acquire_gpu`: the presenting window's device when a
window exists (so Scene samples the film directly), otherwise a shared
headless device released with the last lease. The tracer owns its own queue on
that device so its frames never serialize behind presentation. It imports no
Metal module, and no underlying library imports the tracer.
