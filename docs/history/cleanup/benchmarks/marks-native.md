# Native mark-attribute fusion

Measured on 2026-10-27 on the Apple M1 Mac mini (16 GB, macOS 27.0.1),
OCaml 5.3.0, Dune dev profile. GPU work stays on the initial domain; pure
setup uses the default Parallel pool (eight recommended domains on this host).
The preceding full golden capture was
stopped before this run. Compiler/checker work continued concurrently, so
wall times are observations, not an isolated performance acceptance.

The existing benchmark prepares 100,000 and 1,000,000 circles and rectangles
under seven styles. Each case warms once, then measures the median of 11 or
five frames respectively, collecting before each frame. A frame includes
native producer dispatch/completion and mark preparation, without the render
pass. Qualification forces the GPU path; this is not production calibration.

```sh
_build/default/tools/check.exe lib/rays_editor/native_qualification/bench_marks_gpu.exe
_build/default/lib/rays_editor/native_qualification/bench_marks_gpu.exe
```

The frozen executable is
`/tmp/rays-native-fusion-before.2S4X6I/bench_marks_gpu.exe`, SHA-256
`f8b0d7c8801ac71eaf7d8b5c32100600291d59502e8a2e443c3f0036a66a254b`.
The unchanged benchmark source copied beside it has SHA-256
`a215f3bf4fc18794349e17931ceaa98739a120ec2aec45d46056016d53681cba`.
This is a before-fusion checkpoint of the evolving cleanup, not baseline
commit `62582188`. The complete run took 48.22 seconds.

| Mark | Count | Moving extent + fill, ms/frame | Bytes/frame | Kernels |
| --- | ---: | ---: | ---: | ---: |
| Circles | 100,000 | 2.424 | 2,454,176 | 3 |
| Rectangles | 100,000 | 2.401 | 2,454,080 | 3 |
| Circles | 1,000,000 | 19.371 | 24,053,816 | 3 |
| Rectangles | 1,000,000 | 15.613 | 24,053,720 | 3 |

[All 28 cases](marks-native-before.txt) retain the noisy measurements too,
including the 85.935 ms million-circle moving-radius observation. No outlier
was dropped. Allocation grows by about 8 MB for each active million-element
attribute producer, so dispatch fusion alone must not be assumed to remove
input preparation cost. Native attribute fusion and its at-most-six-ms gate
were unproved at this checkpoint. The changed implementation and paired
measurements follow below.

The first grouped-only run now uses one kernel for every style, but moving
both still allocates about 24 MB per million-mark frame. Its complete
[28 cases](marks-native-grouped.txt) took 56.73 s; compiler/capture activity
continued and the final cases may overlap browser qualification. These wall
times are observations only. Grouping does not remove source construction.

After bounded literal Float range reuse, the first complete run took 49.48 s:

| Mark | Count | Moving extent + fill, ms/frame | Bytes/frame | Kernels |
| --- | ---: | ---: | ---: | ---: |
| Circles | 1,000,000 | 2.393 | 27,392 | 1 |
| Rectangles | 1,000,000 | 2.129 | 27,272 | 1 |

The [complete after run](marks-native-after.txt) uses frozen executable
`/tmp/rays-native-fusion-after.FDq6nP/bench_marks_gpu.exe`, SHA-256
`7d40baff3d4b632c353df1fa72b22b8d4c09512ee3246f0b5adfa9697aa61bf5`.
Its relevant source archive has SHA-256
`4bd9c741051f75088ee50ec69cc60c009758a0547e6183bb112b54aa01e97204`.
The benchmark source is unchanged. Static source reuse belongs to each
drawing's display closure, with 16 entries and a 64 MiB limit; dynamic counts
are rebuilt. This measures warm frames, not retained memory or startup cost.
The reference golden capture remains active, so these are not isolated wall
times.

The frozen before/after pair then completed in 41.73 s and 49.28 s, with no
concurrent GPU qualification. The reference capture and ordinary CPU work
continued, so this is a matched pair under background load rather than an
isolated-machine result. The original benchmark, sample counts and allocation
measurement are identical in both executables.

| Mark | Count | Before ms/frame | After ms/frame | Before B/frame | After B/frame | Kernels |
| --- | ---: | ---: | ---: | ---: | ---: | --- |
| Circles | 100,000 | 2.812 | 0.922 | 2,454,176 | 27,464 | 3 → 1 |
| Rectangles | 100,000 | 2.712 | 0.749 | 2,454,080 | 27,344 | 3 → 1 |
| Circles | 1,000,000 | 16.018 | 2.533 | 24,053,816 | 27,392 | 3 → 1 |
| Rectangles | 1,000,000 | 15.386 | 2.212 | 24,053,720 | 27,272 | 3 → 1 |

[Before](marks-native-paired-before.txt) and
[after](marks-native-paired-after.txt) preserve all 28 cases. Both million-mark
moving-both medians are below 6 ms; the earlier after run independently
measured 2.393/2.129 ms. Exact native pixels at three times, two simultaneous
drawings, multi-output selection and generation invalidation pass. Real
Chrome retains native parity for fused synthetic marks and the million
particles, with zero CPU invokes. This accepts the native fusion measurement
for this host, without closing other Flow or shipping gates.
