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
