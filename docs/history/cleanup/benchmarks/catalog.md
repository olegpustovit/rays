# Catalog source preparation benchmark

The golden-capture harness repeats `Catalog.check_workspace` for each
fixture/case. The embedded `.rays` source list is immutable, so `Catalog` now
reuses its lazily parsed node definitions and a dependency index for that list.
Caller-supplied source lists still use the uncached path; no checked workspace
or geometry is retained.

Measured on Darwin arm64 with the same finite two-row polywire capture before
and after the change (`PW_CASES='buckling no smooth' PW_INPUT=3`, fixture
`shared curves`, 12 points / 10 vertices / 3 faces). Rows were byte-identical
(`cmp` exit 0). These runs overlapped the long catalog capture, so the wall
times are observations rather than a speed claim.

| Run | Total wall | Default row: check / lower / cook | Parameter row: check / lower / cook | Major words allocated per row |
|---|---:|---:|---:|---:|
| Before source index | 0.89 s | 0.1516 / 0.0896 / 0.0508 s | 0.1633 / 0.0926 / 0.0929 s | 12.77 M / 13.76 M |
| Source index, before Defnode cache | 1.01 s | 0.2649 / 0.1498 / 0.0423 s | 0.1725 / 0.1324 / 0.0417 s | 6.19 M / 5.96 M |
| Source index and embedded Defnode cache | 2.78 s | 0.8158 / 0.1299 / 0.1249 s | 0.6673 / 0.3286 / 0.0817 s | 6.13 M / 5.92 M |

The first indexed row includes lazy index construction. The final row includes
the cold-process work and ran alongside the full capture; it is not directly
comparable in wall time. Combined source-index and Defnode reuse allocated
roughly half the major words of the initial baseline, with no defensible wall
speedup claim. The focused
`test_node_sources` differential check compares the embedded indexed path with
a fresh-list uncached path for both successful reachability and a checker
diagnostic. After adding lazy embedded Defnode parsing,
`_build/default/tools/check.exe lib/flow_sop/test_node_sources.exe` completed
successfully; running `_build/default/lib/flow_sop/test_node_sources.exe`
passed (0.66 s). The check compares selected definition names and exact
diagnostic strings.

## One-row capture phase profile

The isolated `--profile-golden NODE FIXTURE CASE` path runs one registered row
through the same parse/check/lower/compile/cook helpers and writes no corpus.
On the subdivide box case below, parse is negligible; checking, lowering and
node compilation take most of the fresh-process time. The cook itself is small
for this 8-point input. Its output counts and digest were:

| Row | Total | Parse | Check | Lower | Compile node | Cook | Major words | Result digest |
|---|---:|---:|---:|---:|---:|---:|---:|---|
| `subdivide / box / fvar corners only depth 2 creased` | 1.8830 s | 0.0000 s | 0.7347 s | 0.5250 s | 0.5083 s | 0.1149 s | 23.95 M | `8320a3a410d91ee4c8a1ea88f8af130b` |
| `uv_unitize / quads / defaults` | 0.0474 s | 0.0000 s | 0.0385 s | 0.0078 s | 0.0000 s | 0.0011 s | 0.54 M | `bfb2e106a8c253b5dacbf82fe41e086d` |

The second row's digest exactly matches its completed row in the reviewed
capture corpus. These are isolated process timings, not a full-corpus speed
claim. A non-invasive five-second sample of the active full capture, while it
was cooking a subdivide row, showed packed-kernel chunking and major-GC work as
the dominant stack; that run had accumulated a large heap. This evidence points
to two separate costs: checker/lowering work per unique row in a fresh process,
and heap/GC cost over a long capture. The reviewed capture was left running;
no rows were removed or rewritten.

For three distinct rows (`defaults`, `depth 2`, `depth 3`) on the same
`subdivide/box` fixture, I compared the existing lower-build cache retained
across rows with clearing it after every row. The outcomes and geometry digests
matched exactly. After a full major collection, retained mode ended at 31.47 M
live words; clearing ended at 21.46 M (about 80 MB less). Row wall times were
1.03/1.38/1.54 s retained and 1.00/1.55/1.26 s cleared, so this establishes a
retention reduction, not a speed improvement. The capture-only path now uses
`Fun.protect` to clear `Lower.Private`'s bounded body-build cache after each
row, including on exceptions; ordinary application lowering is unchanged.

The reviewed full capture at `/tmp/rays-cleanup-goldens-reviewed` was stopped
with SIGINT after the replay check found compiler-refusal rows that now execute
successfully. Its files and the read-only snapshot at
`/tmp/rays-cleanup-goldens-key-snapshot-20261027` are retained as rejected
evidence; the corpus is not
an oracle for current behavior and authorizes no parity-twin deletion. At the
stop checkpoint it contained 5,070 rows for 11 finished keys (`polywire`,
`sweep`, `sweep_circle`, `torus`, `transform`, `tube`, `uv_auto_seam`,
`uv_project`, `uv_sphere`, `uv_transform`, `uv_unitize`). Per-key row counts
matched coverage counts, and all embedded `.rays` hashes matched the capture's
startup provenance. The failure is implementation provenance: the capture
executable predates current evaluator/compiler behavior, while provenance only
records Lisp source hashes.

The first exact mismatch was `polywire / one open / valence 1`. The frozen row
expects `E_KERNEL_FORM: Numeric node bodies must compile to packed kernels.`
Current compiled and `Lower.of_checked ~reference:true` paths both produce
geometry digest `9f8684d0af2521fcda4e78ae174a192b` (96 points, 192 vertices, 48
faces). The focused existing native-oracle gate for this fixture and parameter
case passed: native vs Lisp geometry is within its four-ULP tolerance, and
reference bytes equal compiled bytes. A completed `uv_unitize` key (558 rows)
also replayed successfully, but that does not rehabilitate the capture. The
replay mismatch diagnostic now includes expected and actual outcomes and
compares the current reference interpreter on mismatches.

The next encountered mismatch was `sweep / small / defaults`: the frozen row
also expects `E_KERNEL_FORM`, while current compiled and reference replay both
produce digest `b6a25d9da990c4b0f425f327b11c80ea`. The exact filtered native
port check passes for this row. A legitimate error control,
`polywire / one open / sides over 4096`, still returns `invalid_geometry` from
both compiled and reference cook; its filtered existing native gate passes.

Finite rechecks after the capture was stopped used the existing port gate with
fixture/case selectors plus the one-row phase profiler:

| Row | Reference result / wall | Compiled result / wall | Native-oracle gate |
|---|---|---|---:|
| `polywire / one open / valence 1` | `9f8684d0af2521fcda4e78ae174a192b` / 0.212 s | same digest / 0.193 s | pass, ~1.2 s |
| `sweep / small / defaults` | `b6a25d9da990c4b0f425f327b11c80ea` / 0.653 s | same digest / 0.626 s | pass, ~9.7 s |
| `polywire / one open / sides over 4096` | `invalid_geometry` / 0.191 s | `invalid_geometry` / 0.178 s | pass, ~1.4 s |

The first two rows confirm the stored refusals were compiler defects: their
reference and packed executions match by geometry bytes, and each current
compiled result passes its native oracle. The error control still refuses in
all three paths. The capture-only `initial_reference` option now selects the
reference build before the first lower/build attempt; normal parity tests keep
the compiled-first default. The exact registry preflight passed in 0.55 s:
113 unique generic keys plus the 15 explicit supplemental keys equal the 128
current SOP definitions (`prelude.rays` is not a node). The alias `@lib/flow_sop/goldens/runtest`
and the port executable build passed together in 5.0 s; no full capture was
started.
