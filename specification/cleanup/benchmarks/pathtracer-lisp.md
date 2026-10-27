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
