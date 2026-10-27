# Environment (`Rays.World`)

A World is pure data: an ordered stack of procedural layers plus sun and
framing settings. `World.bake` turns it into CPU products that both the
raster renderer and the Metal path tracer consume. The interface
(`lib/rays/world.mli`) is the contract; this note summarises it.

## Mapping

- +Y up, linear-light HDR floats, never clamped.
- Equirect: `u = atan2 x (-z) / 2pi + 0.5`, `v = acos y / pi`. -Z (front,
  azimuth 0) is `u = 0.5`; azimuth grows toward +X; row 0 is the zenith.
  Texel `(i, j)` holds the direction at its centre.
- `rotation` turns the whole World about +Y (maps, lights and sun).

## Layers

Bottom to top. Base layers replace everything below; the others add.

| Layer | Kind | Notes |
|---|---|---|
| `Gradient` | base | zenith / horizon / nadir, `sharpness` |
| `Sky` | base | Nishita single scattering (12 view x 4 light samples), baked at 128x64 and upsampled; below the horizon fades to 0.3 x horizon |
| `Room` | base | box interior seen from its centre; ceiling panel grid, ambient-lit walls, cove strip |
| `Sun` | add | disc in the camera map only |
| `Shape` | add | `Rect`, `Strip` (azimuth band), `Circle`, `Ring`; smoothstep softness |
| `Scatter` | add | seeded soft blobs, optional dimmed floor mirror |

Invisible entries and everything below the topmost visible base layer are
dropped.

## Dome and Light

`emit = Dome` bakes an emitter into both maps. `emit = Light` also extracts
it as rect area lights (`baked.lights`): shapes and blobs at distance
`light_radius` (default 10) with their angular size, strips in segments of at
most 60 degrees, room panels at their true box position (box centred on the
origin, ceiling at `+height/2`). A promoted emitter stays in the **camera**
map but is left out of the **lighting** map. The sun disc is likewise camera
only; lighting gets the sun through `baked.sun`. So
`camera - lighting = promoted emitters + sun disc`, and nothing is counted
twice (tested).

## Sun

`Linked` uses `sun_direction` (declination `-23.44 deg cos(2pi (day+10)/365)`,
hour angle `15 deg (hours - 12)`, north = -Z, east = +X); `Manual` takes
azimuth/elevation. One direction drives the sky, the disc and `baked.sun`. A
sun at or below the horizon has no disc and `baked.sun = None`.

## Bake products

- `camera`, `lighting`: full-resolution RGB `Float.Array` maps.
- `cdf`: luminance x sin theta importance sampling of `lighting`
  (marginal + per-row conditionals, both ending at 1, plus the integral).
- `sh9`: irradiance-ready SH9 of `lighting` (cosine lobe folded in),
  projected from the 128x64 box level.
- `specular`: 6 GGX mips, roughness 0 to 1 by 0.2, 128x64 down to 4x2
  (32 Hammersley samples each).
- `lights`, `sun`, and the unbaked `background` and `exposure`.

`bake` is byte-identical for every domain count (rows are independent;
reductions sum per-row partials in fixed order). `bake_cached` keeps 4 bakes
per domain; changing only background or exposure is a hit.

## Parity rule

The raster renderer and the path tracer read the same bake products and use
the same ACES tone map. `background` is consulted at bounce 0 only.

## Raster renderer (`Scene.D3.with_world`)

`Scene.D3.with_world baked scene` switches that Scene3 to the environment-lit
path; a Scene3 without a World renders exactly as before (Blinn-Phong over
`Light.t`, no tone map). Lowering (`scene3_native_lowering.ml`) routes every
non-point drawing to the `Scene3_world` pipeline (`source_scene3_world` in
`lib/runtime/runtime.ml`), which computes, per fragment:

- **Background.** A clip-space triangle drawn first (depth test off, no depth
  write) turns each fragment into a view ray through the inverse
  view-projection and shows the **camera** map (bilinear, u wraps, v clamps)
  or the `Color c` radiance; `Transparent` draws nothing, so the scene's
  clear shows. It goes through the same tone map as surfaces.
- **Material mapping** (`pbr_of_material`, the only one): albedo = diffuse
  x vertex color x texture, sRGB-decoded with gamma 2.2; roughness =
  `sqrt (2 / (shininess + 2))` clamped to [0.03, 1] like the path tracer;
  metallic 0; f0 = 0.04; emissive adds decoded radiance. A material whose
  diffuse, ambient and specular are all black (`Material.unlit`) stays
  unlit: its emissive color is written as is. The scene and material
  ambient terms are ignored (the World replaces them).
- **Image-based light.** Diffuse = albedo / pi x E(n), E from `sh9`
  evaluated like `World.irradiance`. Specular = the prefiltered mip at
  LOD = roughness x 5 in the reflected direction (one `Rgba16_float` texture
  holding the 6 `specular` maps as mips) x Karis' analytic split-sum
  environment BRDF (no LUT). Below roughness 0.2 the reflection blends
  linearly from the full-resolution **camera** map (roughness 0, bound to
  object draws too) to the roughness-0.2 mip, so near-mirrors show sharp
  emitters and the sun disc as the path tracer does; the analytic sun and
  rect specular lobes are scaled by the same weight (roughness / 0.2, their
  diffuse kept) so those emitters are not counted twice. Diffuse is weighted
  by (1 - that specular weight), so a white matte sphere under a constant
  radiance L reads L (the furnace test).
- **Direct light,** through the path tracer's `eval_brdf` (GGX, Smith,
  Schlick, Fresnel-weighted Lambert, copied verbatim): the sun as a
  directional light of irradiance radiance x pi r^2; the scene's `Light.t`
  list with its existing falloff, scaled by pi so intensity 1 lights a
  white matte face to 1 as in Blinn-Phong; and `baked.lights` as 2x2-sampled
  rects of radiance x area with inverse-square falloff. Only the sun is
  shadowed (see **Sun shadow**). Point clouds keep the Blinn-Phong points
  pipeline.
- **Sun shadow.** An explicit `Shadow3` on the Scene3 is used as is (its
  snapshot travels in the World block). Otherwise, when `baked.sun` is set,
  the renderer draws a GPU depth pass: lowering fits an orthographic view
  along the sun to the bounds of the caster drawings (depth-writing
  `Replace`/`Alpha` triangle faces, their meshes' boxes through their
  transforms and instances, 2% margin) and keys it by those drawings' meshes
  (physical), transforms and instance arrays plus the sun direction, so a
  scene rebuilt each frame from the same parts keeps its key (2 fits cached).
  `Scene_execution` renders the same casters through that view into a
  renderer-owned 2048^2 map (24-bit depth packed in RGBA8, the `Shadow3`
  encoding, plus a Depth32 attachment: 32 MB, created on first use, freed with
  the renderer) in its own command buffer before the frame, only when the key
  differs from the last rendered one (`Runtime.stats.sun_shadow_passes`
  counts them); the World fragment samples it with the `Shadow3.create`
  defaults (bias 0.001, normal bias 0.005, 3x3 PCF, strength 1), on the sun
  term only. On the M1 a re-render costs about 2.5-3 ms wall at 25.6k
  triangles (1024^2 saves ~0.5 ms), an unchanged key nothing. ponytail: one
  map per renderer, so a second World view with a different key in the same
  frame samples the first view's map; translucent depth-writing draws cast
  opaque shadows; no cascades, so a large scene gets coarse texels.
- **Light cap.** 64 lights: the scene's own lights come first (more than 64
  is still a typed error); World rects fill the remaining slots in
  decreasing power (luminance x area) and the rest are dropped.
- **Tone map.** `2^exposure`, then the path tracer's `resolved_rgba` (ACES
  fit, clamp, gamma 1/2.2), copied verbatim, into the RGBA8 target; no HDR
  render target.

Uploads: textures are keyed on the physical identity of the map arrays (a
bake that only changes `background` or `exposure` shares them), the small
World block on the physical `baked` value, the shadow and the sun-fit key; each lowering cache
keeps the last 2, and the renderer's texture cache holds each `world:` key
once until evicted by its 256-entry / 256 MB bounds or destroyed with the
renderer. On an M1 (release), a frame with new 512x256 maps takes 8 ms, with
new 2048x1024 maps 85 ms; an unchanged or exposure-only frame uploads no
texture (1.1 ms for a one-sphere frame).

## Budget

`dune exec tools/bench_world.exe --profile release` on an Apple M1 (8 domains
recommended), median ms (2026-09-26):

| Preset | 512x256, 1 domain | 512x256, 8 domains | 2048x1024, 1 | 2048x1024, 8 |
|---|---|---|---|---|
| neon corridor | 13.0 | 4.3 | 83 | 29 |
| soft blobs | 14.8 | 5.0 | 110 | 29 |
| white room | 12.0 | 4.0 | 68 | 20 |
| daylight | 20.6 | 6.1 | 145 | 37 |

The drag-preview budget is 8 ms at 512x256 on the recommended domains. The
first cut took 25-47 ms there: boxed float arguments in the per-texel layer
call (minor GCs stop every domain) and SH/specular work at full resolution.
Keep the texel loop allocation-free.
`RAYS_WORLD_DUMP=dir` makes the bench write each preset's camera map as a
PPM.
