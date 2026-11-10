# Path-tracer source migration baseline

Measured 2026-10-27 on an Apple M1 Mac mini, 16 GB, macOS 27.0.1,
OCaml 5.3.0, Dune dev profile. No window was created. Other native GPU
checks were held while these measurements ran; CPU capture/build jobs
continued. These are before measurements, not an interleaved A/B acceptance.

The embedded MSL is byte-identical to baseline `62582188`:
SHA-256 `76f44149b35dc7c14e7d728225a804ccb58c87899e459087dbd29d0302218fdd`.
The frozen executable is `/tmp/rays-pathtracer-before.hyqNYl/render_workspace.exe`,
SHA-256 `65682ff859e6c9a5f6217e2cbdca53197109bbb69973754d4284da5d8f287cf7`.
Its two copied input workspaces and source hashes are in the same scratch
directory. The repository keeps the measurements and raw GPU logs below.

| Workspace | Authored dimensions | Bounces | Median GPU ms/frame |
| --- | --- | ---: | ---: |
| pathtracer | 480 × 840 | 5 | 126.580125 |
| shattered_studio | 1600 × 1600 | 4 | 9985.928450 |

Each measured frame adds four samples per pixel. Each round resets accumulation,
renders 16 samples (four frames), and flushes each frame. There are five rounds
after one warm render. Each round contains 12 GPU-band timings; GPU ms/frame
is their sum divided by four, with the median taken over the five round means.
GPU durations come from the existing native receipt timing, not wall time.
The output SHA-256 is identical across all five rounds for each workspace.
The studio has 278,368 primitives and 27,390 points. Setup took 2.18 s and
147.39 s respectively; setup includes cooking, upload and pipeline creation,
so it is not a standalone shader-compilation measurement.

Reproduce from the repository root, after building the runner:

```sh
_build/default/tools/check.exe tools/render_workspace.exe
RAYS_PATHTRACER_PROFILE=1 _build/default/tools/render_workspace.exe --benchmark sketches/pathtracer.rays /tmp/pathtracer.csv 16 2>/tmp/pathtracer.gpu.log
RAYS_PATHTRACER_PROFILE=1 _build/default/tools/render_workspace.exe --benchmark sketches/shattered_studio.rays /tmp/shattered_studio.csv 16 2>/tmp/shattered_studio.gpu.log
```

The runner reads the authored camera, resolution, bounce and round-corner
settings; the optional final size argument deliberately overrides resolution.
It writes the CSV atomically only after all five rounds finish. Its ordinary
PNG-render mode is retained. Baseline command wall times were 6.56 s and
363.95 s, including setup and the warm render.

Raw evidence: [pathtracer CSV](pathtracer-before.csv),
[GPU log](pathtracer-before.gpu.log),
[studio CSV](shattered_studio-before.csv),
[GPU log](shattered_studio-before.gpu.log).

P8 remains open. The Lisp implementation still needs interleaved old/new runs
on both scenes, separate pipeline-compilation timing, preview measurements and
linear-film RMSE. The stored RGBA8 hashes prove repeatability of the resolved
image; they do not prove the linear-film error gate.
