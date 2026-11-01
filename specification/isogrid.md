# Isogrid compiler implementation

Accepted scope: [Isogrid Compiler Plan, version 1](https://artifacts.microapps.space/a/ckxekybsw9qs),
source SHA-256 `cca9a45b4d447ed8bae424da7eb442ae4ffb9d91e86b11929133d8fb93f40800`.
`PROBLEM.md` supplies the underlying requirements. This log does not replace or
narrow either document. No commits are requested.

The sketch stays frozen at SHA-256
`0fdb9b0ebec8f5fe84227d7c0963dd3ec8c446c6145e145555b4fa4f80744e69`.
No shader placement switch, sketch rewrite, palette removal, triangle-only
substitute or Scene/SOP web export is part of this work.

| Stage | State | Exit evidence still needed |
| --- | --- | --- |
| S0 baseline, counters, goldens | Done | Baseline and correctness evidence below; no performance fix yet |
| S1 typed gather, named refusals | Done | Typed prepared table layout; native/web/error evidence below |
| S2 immutable list tables | Done | Guarded internal tables; exact CPU/native/web and geometry checks below |
| S3 trusted kernels, replay | Done | Unedited contracts, call indices, probes, exhaustive bit verification and full scale gate pass |
| S4 constant inner forms | Done | Slice/map-sum unrolling, guards, CPU/native/web parity and all-recipe 24³/64³ performance |
| S5 Track A exit | Done | All-recipe CLI/warm, CPU-only/empty, exact digests and 200-switch ownership/accounting evidence below |
| S6 CPU deferral | Done | Strict once-only CPU regions at 24³/64³, exact fields/corpora/probes, typed cook/CLI errors, responsive worker, bounded test actions and final smoke-all; exit audit below |
| S7 deferred topology | Done | Non-forcing metadata, single racing materialisation, release orders, CPU consumers; full RDK/SOP/gate checks |
| S8 resident construction | Done | Frozen 24³/64³ zero-payload publication and full parity, typed validation/ownership, host calls, CPU readers, cancellation, capacity replay and strict empty-offset checks; exit audit below |
| S9 coloured quad viewport | Done | Maze/Coral exact raster/modes/orbit/frame/shadow, zero payload, static frames, actual picking/inspection readers, full editor shot and full native gate pass; production policy remains S10 |
| S10 production placement/lifetimes | In progress | Measured record comparison, consumer budgets, 64³ CPU decisions, 200-generation stress and original 24³ 200-selector lifetime gate pass; whole-chain costs, measured mesh guards and resident original-workspace lifetime exit remain |
| S11 close-out | In progress | Incremental private-CMI ordering fixed; specifications, full acceptance audit and shipping/scale/native checks remain |

## S0 measurements

2026-10-31, Apple M1, 8 cores, 16 GiB, macOS, Dune dev build, fast workspace
precision. Times below are quiet single runs, not medians. Rendering is offscreen
at 1280×900, using an already-created Canvas; completion is observed with a
pixel read. Hashing deliberately happens after the presentation measurement.
The counters cover Flow GPU operations, not the renderer's ordinary host-mesh
uploads. `alloc_bytes` measures the calling domain, not all cook workers.

| Recipe 1 | 24³ | 64³ |
| --- | ---: | ---: |
| Editor load to rendered pixel | 2.913 s | 29.872 s |
| Evaluate | 2.786 s | 29.348 s |
| Cook phase | 0.0109 s | 0.2006 s |
| Prepare callback | 0.0082 s | 0.1787 s |
| Calling-domain allocated bytes | 10,546,160,584 | 111,084,399,520 |
| Geometry digest | `b86026e6de49c3d207812da261aea1075a7c3229d08a12b39c041ee1ae993984` | `ca4df802d14ed8240c2581e2bd972cab41b66216a1bec2d2b16d4548b1993bd8` |

Both evaluate measurements are within 2× of the plan's baseline figures; numeric
gates are unchanged. Both profiles show 8 kernel hits and 3 walked maps. At 24³,
the face mask walks 82,944 elements, positions and normals 54,464 each. Directions
and corners refuse as `list:vec3`. Each of the eight kernel maps walks 4,096 head
elements first. No new side-scaled refusal was observed for recipe 1.

The 64³ CLI check passed in 30.02 s wall (29.32 s user, 4.76 s system).
The complete warm sequence passed in 67.64 s including post-measurement
verification. The [raw map and edit log](benchmarks/isogrid/s0-editor.log)
records every presented digest and the exact expected walker digest.

| Warm edit | Present | Evaluate |
| --- | ---: | ---: |
| 24³ recipe 1→3 | 2.971 s | 2.893 s |
| 24³ recipe 3→1 | 3.298 s | 3.222 s |
| Recipe 0 | 1.387 s | 1.330 s |
| Recipe 2 | 2.682 s | 2.614 s |
| Recipe 4 | 3.326 s | 3.256 s |
| Recipe 5 | 1.736 s | 1.674 s |
| Recipe 6 | 1.725 s | 1.666 s |
| Recipe 7 | 1.529 s | 1.471 s |
| Recipe 7, seed 17→18 | 1.536 s | 1.477 s |
| Recipe 7/seed 18, 24³→64³ | 16.762 s | 16.645 s |
| Return to 24³ | 1.548 s | 1.486 s |

The [dedicated 64³ selector run](benchmarks/isogrid/s0-switch-64.log) also passes:
1→3 presents in **33.574 s** (evaluate **33.124 s**), and 3→1 in **34.243 s**
(evaluate **33.794 s**). Its cold load presents in 29.564 s. Total process wall
time, including independent post-measurement verification, is 165.01 s.
It finishes with 3 Flow GPU uploads (34,604,544 bytes), 22,021,632 payload
readback bytes, one pipeline and no held output buffers. This is a baseline,
not an S5 performance-gate pass.

One baseline assumption needed clarification: the existing production
`Workspace_gpu` starts measuring placement after its first presented frame.
Warm edits can consequently place `Bulk_approx` for `sculpture/colors`, returning
the constant palette through float32. This is the existing fast contract,
including the explicit `exact` readback, not stale geometry. For example, the
24³ recipe-3 raw digest is
`528c8c823f5e2490706a0df4e1021b0659c691f734c6bae8abeb998b6b8e000d`;
it equals the walker arrays after **only** converting Cd once to float32.
Fresh CPU compilation and a fresh session confirmed this diagnosis.

CPU goldens remain bit exact. The benchmark first requires their exact digest;
when production used float32 colours, it verifies a CPU evaluation against the
walker golden, then requires the presented arrays to equal precisely that Cd
conversion. It reports both digests and the colour storage. P, topology and N
are never rounded for this check. The map profiler distinguishes these accepted
GPU maps from CPU kernels and measures unsuccessful placement attempts too.

The walker golden-generation commands took 4.93 s (12³), 22.95 s (24³),
and 427.51 s (64³, under concurrent test compilation). These are finite,
one-time reference captures, not default tests or benchmark timings.

Flow GPU counters in the raw log are cumulative. The full sequence made 9 input
uploads (9,005,856 bytes), read 4,483,872 payload bytes, compiled one pipeline and
held no output buffers at its end. Those are colour production/cost-probe
transfers, not a resident mesh path. Explicit post-presentation inspection read
zero additional GPU bytes. Renderer host-mesh uploads are outside these counters.

The independent walker (`Flow.Eval.static`, with no bulk kernel handler) produces
`tools/isogrid_{12,24,64}.expected`: all eight recipes at seed 17, plus recipe 7
at seed 18. Each digest includes ordered P, vertex_points, primitive_offsets,
kinds, N and Cd, with lengths and exact binary64/int64 little-endian words.
The regression checks both compiled evaluation arrays and the cooked RDK mesh.
The 12³ corpus belongs to `@tools/isogrid`; 24³ and 64³ belong to
`@tools/qualification-scale`.

Reproduce from the repository root:

```sh
_build/default/tools/check.exe lib/rays_editor/native_qualification/bench_isogrid.exe
_build/default/lib/rays_editor/native_qualification/bench_isogrid.exe --bench 24 --single
_build/default/lib/rays_editor/native_qualification/bench_isogrid.exe --bench 64 --single
_build/default/lib/rays_editor/native_qualification/bench_isogrid.exe --switch 64
_build/default/lib/rays_editor/native_qualification/bench_isogrid.exe
_build/default/lib/rays_editor/native_qualification/bench_isogrid.exe --source 64 > /tmp/isogrid-64.rays
_build/default/bin/rays.exe check /tmp/isogrid-64.rays
_build/default/tools/check.exe @tools/isogrid
```

The complete benchmark performs real `Editor.edit` gestures for recipes
1→3→1→0→2→4→5→6→7, seed 17→18, and resolution 24→64→24; every
presented geometry is compared with its walker golden. `--write-goldens SIDE`
is an explicit reference-generation command, never part of ordinary testing.
Map reports include site, count, selected tier, refusal, actual walked head/tail
elements, total wall time and head/kernel-attempt/tail time. The bounded
512-entry history stores each event in constant work, including small nested
maps; it is disabled outside `Profile.sample`.

Final S0 functional checkpoint: `check.exe @lib/flow_ir/runtest @lib/flow/runtest
@tools/isogrid bin/rays.exe @test/dependency_gate`, 14.34 s wall including rebuild.
The slowest action was `test_uniforms` at 8.64 s; the new geometry corpus took
6.66 s. The unedited value/error/record contracts pass; the new profiler check
also compares one/eight-domain results, bounds its history and verifies failure
and scope restoration. The dependency gate remains 59 libraries, 36 rules and
29 whitelists, with no new boundary. `git diff --check` passes.
The final typecheck/IsoGrid/dependency/source-guard checkpoint passed in 23.89 s.

## S1 typed gathers

`Core_region.operation` reads the prepared table's element type for an `any`
lookup result. Concrete disagreements refuse with the lookup term, both types
and its span. Integer/Text banks and vector widths follow that element type.
Other uses of `width_of` already have concrete-type guards or name their own
unsupported types; no checker change was needed. `expression` now catches
internal `Unsupported` at the innermost term; the outer safety net also names
its term/type. The former generic message is absent from `lib/flow_ir`.

`test_static_map` compares the walker and compiled results for a `(ref source)`
record field, a local helper, an annotated array helper, an out-of-range index
after element 4,096 and an untaken range error. Successful numeric cases assert
an actual kernel hit with zero tail walking. The failing case asserts the exact
walker diagnostic and real tail calls. A separate Text-region case verifies
the table's Text bank; a corrupted concrete annotation checks the named
refusal at the gather. Existing value/error/record contracts were not edited.

The emitted-JavaScript gather runtime now also erases only the checked lookup
type to `any`, exercising inference from int, float, vec2–4 and Text tables.
It compares exact native bytes and the real runtime with one/eight workers,
including integers beyond 2^53. No shader or web runtime workaround was needed.

Validation: Flow/IR/IsoGrid suites passed in 15.19 s after the compiler change;
the completed new static cases passed in 1.86 s (1.21 s test action). Web suites
and the dependency gate passed in 11.83 s; the additional erased-type runtime
cases passed in 5.61 s (5.00 s action). The frozen sketch hash and 12³ geometry
goldens are unchanged. Full scale verification remains the S3/S11 gate.

The real sketch still reports 8 CPU kernels and the same 3 walked maps:
directions/corners refuse before reaching the newly fixed type. No speedup is
claimed for S1. S2 supplies immutable list tables; S3 removes the mandatory
head walk only after its exactness and replay checks pass.

Three S1 cold 24³ editor runs present in 3.495, 2.907 and 2.875 s (median
2.907 s; evaluate median 2.758 s), against S0's 2.913 s / 2.786 s sample.
This is consistent with the expected lack of whole-sketch improvement before
the list captures compile. All three have the exact same walker geometry
digest. Calling-domain allocation is 10,546,234,312 bytes in the median run,
about 74 KB above S0 across a roughly 10.5 GB allocating load.

Final coherent S1 checkpoint: `check.exe @check @lib/flow/runtest
@lib/flow_ir/runtest @lib/flow_web/runtest @tools/isogrid @test/dependency_gate
@tools/codemod/history-comments` passed in 5.64 s, reusing the functional
actions above. Specification links and `git diff --check` pass. No commit was
made; S2–S11 remain outstanding and the overall goal stays active.

## S2 immutable list tables

Captured nonempty homogeneous lists of int, float and vec2–4 specialize as internal
packed tables. The authored value remains a List. `nth` lowers to Gather with
integer bounds and whole-number checks for float indices. Invalid paths raise
`E_LIST_RANGE`; untaken paths perform no invalid read. Empty/mixed lists refuse
with `E_PACKED_CAPTURE`. Rebinding accepts the same element type and length,
and refuses a changed shape without altering the old template.

The shared guarded Raise instruction now prints in MSL, SPIR-V and WGSL compute
kernels. Status bit 8 preserves the list error code/message on native and web;
raster placements refuse explicit errors because they have no status record.
No GPU API or dependency boundary changed.

Tests compare all numeric widths, signed zero, subnormals and full-width integers
with the walker in one/eight domains. They cover negative, fractional and huge
indices, failures before/after 4,096, unsupported captures and rebinding. The
emitted JavaScript executes list fixtures in one/eight workers; native Metal
checks a valid vec3 table and invalid index; new SPIR-V fixtures validate.
The real Chrome WebGPU qualification passes valid int/float-index lists with
zero differences, selected range/fraction errors and an untaken error branch.
Only the two new SPIR-V fixture hashes were added; existing hashes are unchanged.

Cold recipe-1 measurements on the same M1 and command as S0:

| Measurement | 24³ | 64³ |
| --- | ---: | ---: |
| Present | 1.514 s | 2.177 s |
| Evaluate | 1.382 s | 1.709 s |
| Calling-domain allocated bytes | 5,304,061,008 | 5,712,479,008 |

All 11 maps compile. The 12³/24³ corpus asserts zero tail walking in recipes
1, 2, 3, 5, 6 and 7. Geometry digests and the frozen sketch are unchanged.
Warm 24³ 1→3 evaluates in 1.336 s and 3→1 in 1.613 s; recipe 4 still
takes 1.890 s. These are not the S5 latency gates: the mandatory heads remain.
The full edit sequence, including geometry verification, passed in 19.55 s.
Raw evidence: [cold 24³](benchmarks/isogrid/s2-cold-24.log),
[cold 64³](benchmarks/isogrid/s2-cold-64.log), [edits](benchmarks/isogrid/s2-editor.log).

`check.exe @lib/flow_gpu/runtest @lib/flow_web/runtest
@tools/qualification-web` passed in 41.36 s including browser export and image
comparisons. CPU/native focused checks passed in 10.40 s. The new optional
`@tools/isogrid-24` corpus and default 12³ corpus passed in 7.92 s; two
24³ shards keep the slowest action at 6.63 s (previously 10.48 s unsharded).
The 64³ corpus and every-sketch exactness remain S3/S11 qualification work.

The final coherent S2 checkpoint (Flow/IR/GPU/web, 12³/24³/native IsoGrid,
typecheck, dependency and source guards) passed in 8.14 s. The shard selector is
a CLI argument; the dependency gate correctly rejected the initial environment
variable in a non-binary module, and no gate allowance was added.

## S3 trusted kernels and replay

`Residual.handle` defaults to untrusted. Exact `Flow_ir.static_map` may run first
when recording is absent, or when production explicitly requests closure replay.
Untrusted and approximate handlers keep the old checked head. Failure/refusal
walks from zero. Calls advance by the full count. Named functions adapted into
closures retain their provenance and eager recording head too.

The three original value/record contracts pass unedited. New checks compare
body probes at 0, 4,095, 4,096 and the final element; two consecutive maps share
one closure and preserve their offsets. Replay-mode failures past 4,096 match
the full walker diagnostic, and named-function recording keeps its head. Full
bit verification also runs in one/eight domains.

`@tools/verify-static`, included in `@qualification-scale`, invokes
`Flow_ir.static ~verify:true` across every workspace in the 43 sketch source
files. Every successful exact kernel compares every element with the walker;
a mismatch aborts instead of becoming a refusal. This sweep passed. Two
million-point grid fixtures (`gpu_sea` and `flow_particles_gpu`) use 73² points
in memory, still above 4,096, so reference topology stays bounded. Their files
and the frozen IsoGrid are unchanged. A first unbounded sweep was stopped at
157 s; a second revealed that top-level overrides do not change defaults read
by graph references. The fixture now uses checked document edits, and the
completed focused checkpoint passed in 26.65 s including rebuild and all
Flow/IR/graph/IsoGrid/type/dependency checks. The sweep action was 17.10 s and
was split into four shards, which pass together in 9.36 s. Full scale
qualification remains outstanding.

Quiet S3 measurements, with unchanged exact geometry digests:

| Measurement | 24³ | 64³ |
| --- | ---: | ---: |
| Cold recipe-1 present | 0.203 s | 0.958 s |
| Cold recipe-1 evaluate | 0.0495 s | 0.4346 s |
| Calling-domain allocated bytes | 184,637,816 | 858,700,344 |

All side-scaled recipe-1 maps have zero head and tail calls. Warm 24³ 1→3
presents in 0.128 s (evaluate 0.0465 s), and 3→1 in 0.127 s (0.0443 s).
Recipes 2, 5, 6 and 7 evaluate in 0.0382, 0.0287, 0.0264 and 0.0274 s.
Recipe 0 still evaluates in 0.526 s; recipe 4 in 1.757 s. Those two remaining
forms are S4, not a relaxed performance gate. The complete edit/verification
sequence passes in 4.54 s, with zero Flow GPU transfers/pipelines: trusted
exact CPU kernels supply these static arrays before approximate placement.

The cold 64³ CLI check passes in 1.69 s and peaks at 394.6 MiB RSS, close to
the plan's 386 MiB prototype figure. The complete offscreen editor benchmark
peaks at 1,024.8 MiB; its process also cooks, prepares, renders and hashes the
geometry, unlike the CLI. That larger measurement remains visible for S5's
memory review. Its printed heap is measured after geometry verification.
Evidence: [cold 24³](benchmarks/isogrid/s3-cold-24.log),
[cold 64³](benchmarks/isogrid/s3-cold-64.log), [edits](benchmarks/isogrid/s3-editor.log),
[CLI resource timing](benchmarks/isogrid/s3-check-64.time).

The first full scale gate hit its unchanged 180-s timeout in the pre-existing
monolithic `run_parallel_exact`; the 64³ IsoGrid corpus and exhaustive catalog
had passed. Exactness sections now run independently. All generator checks
pass together in 11.28 s: grouped Facet retains its full geometry and ordered
group, supplied directly rather than parsed from 120,000 IDs (74.27 → 0.45 s).
Spiral retains its ramp/attribute options at 5,005 points, above the kernel
threshold; its reference-evaluated knot folds previously exceeded 180 s at
100,005 points and now finish in 4.87 s. All one/four-domain comparisons remain.
The broad gate also exposed failures that reproduce with replay
disabled: an obsolete fracture operation name, assumptions about internal cook
counts and trace hops, missing normalizer helpers in nested library lowering,
and a strict numeric-source check that visited function/helper bodies without
their parameter bindings. The scoped source check and normalizer fix pass
Flow/IR/SOP/diagnostics together in 27.05 s, retaining the existing strict
refusal contracts. Macro-expanded folds also need their loop/function zone in
the inline memo term key: two expansions can share a source form while using
different local bindings. A repeated-evaluation regression guards that case.
Attribute masks supplied as residual count-based arrays now force through the
same strict CPU resolver as their other concrete arguments, restoring Edge
Transport. Flow/IR/SOP and the SOP qualification executable pass together in
33.10 s. The next full `check.exe -j 2 @qualification-scale` run still reached
the 180-s cutoff (180.29 s wall). Filters and subdivision passed but took
76.17/92.77 s; modeling and combining were still active. Attributes passed in
3.72 s. This intermediate failure did not clear the gate.

The completed geometry suite runs 84 named cases/policies in separate Dune
actions and passes in 57.91 s (`-j 4`). It retains every exactness assertion and
all policies; large Carve/Sweep, constrained flood, two-level crease/detail,
duplicate-volume and edge-straighten fixtures use smaller dense geometries.
Carve still emits over 4,096 points, and the general-profile Sweep emits 6,168.
The group parse and Spiral changes above remain. Splitting also releases each
fixture's lowering/cook working set before another case starts.

All required S3 actions then pass: the frozen 12³/24³/64³ golden corpora, all
43 sketch sources with exact static verification, original and new Flow/IR
contracts, Flow graph and SOP tests, the 819-row Lisp SOP replay, typecheck,
and dependency gate. Studio fracture yields 18,278 closed manifold shards,
exact across one/four domains with volume preserved and four material batches.
The exhaustive catalog checks 1,818 field perturbations (73 correctly rejected).
This execution took 151.09 s, but its command included an erroneous extra
`@test/source_gate` alias. The corrected command exits zero in 1.04 s using
Dune's successful action cache; no failed action or test was accepted.
Evidence: [action log](benchmarks/isogrid/s3-qualification-actions.log),
[corrected gate](benchmarks/isogrid/s3-qualification.log),
[geometry suite](benchmarks/isogrid/s3-parallel.log).

The final quiet editor edit sequence passes again in 4.58 s with unchanged
digests, zero walked heads/tails on eligible maps, and zero Flow GPU transfers.
No numeric gate or timeout changed. S4 can now begin.

## S4 constant inner forms

The region compiler unrolls the two accepted numeric sums at constant Int
counts 0..64, with named refusals for dynamic/larger counts. All map elements
precede the sum, preserving the walker's error order. Slice length comes from
the actual prepared table; first-index and whole-slice guards run even for an
empty slice. Helpers, signed zeros, subnormals, vector widths and wrapping Int
sums match the walker, including one/eight domains and full late-error
diagnostics past 4,096. No region instruction was added. The cap stays 64.

The new CPU test passes in the focused IR alias; the final IR/web/GPU/native,
type/dependency checkpoint passes in 3.12 s with cached successful actions.
The initial native checkpoint was 21.83 s, including rebuild: five actual
Metal kernels match their CPU outputs, and invalid positive/negative slice
indices return the correct typed errors. SPIR-V validates both new fixtures;
only their two golden rows were added beyond S2. Real Chrome reports zero
differences for both forms, typed selected bounds/length failures and no errors
in untaken branches. The full web qualification actions ran in 45.47 s while
the new JS test harness still needed scalar-capture support. After correcting
that harness and its `+ max_int` fixture assumption (exact wrapping requires
`int/add`), the coherent web/IR/qualification checkpoint exits zero in 5.62 s.

All 12³/24³/64³ geometry goldens pass with the previous recipe-0/4 tail-walk
exception removed. Every side-scaled numeric map compiles; maze walking stays
bounded by its fixed four-cell grid. All-sketch exact verification passes too.
The combined focused corpus/type/dependency check takes 25.09 s; the 64³
golden action is 16.86 s under parallel load (previously 61.49 s at S3).

Quiet editor timings, exact geometry and zero Flow GPU transfers:

| Recipe | 24³ present / evaluate | 64³ present / evaluate |
| --- | ---: | ---: |
| 0 | 0.164 / 0.109 s | 0.308 / 0.245 s |
| 1 (return from 3) | 0.131 / 0.045 s | 1.104 / 0.516 s |
| 2 | 0.125 / 0.054 s | 1.062 / 0.725 s |
| 3 | 0.142 / 0.070 s | 0.851 / 0.510 s |
| 4 | 0.092 / 0.023 s | 0.437 / 0.242 s |
| 5 | 0.149 / 0.087 s | 0.387 / 0.246 s |
| 6 | 0.084 / 0.026 s | 0.305 / 0.208 s |
| 7 | 0.085 / 0.027 s | 0.322 / 0.211 s |

The full S4 24³ edit sequence passes in 2.89 s, and the 64³
sequence in 9.60 s. Both beat the per-recipe 1.2/5 s gates. The 64³ sequence
peaks at 3.36 GiB RSS including history, geometry hashing and rendering;
this is not a retained-memory or S5 200-flip pass. Cold 64³ recipe 1 presents
in 0.969 s (evaluate 0.450 s).
Recipe 5's 24³ evaluation is noisier than S3's 0.029 s (now 0.087 s), still
within the 0.150-s S5 latency gate. S5 must measure repeated edits and retained
memory rather than treating these single sequences as its acceptance evidence.
Raw evidence: [24³ edits](benchmarks/isogrid/s4-editor.log),
[64³ edits](benchmarks/isogrid/s4-editor-64.log),
[64³ resource timing](benchmarks/isogrid/s4-editor-64.time),
[corpus check](benchmarks/isogrid/s4-goldens.log),
[final focused gate](benchmarks/isogrid/s4-check.log),
[web actions](benchmarks/isogrid/s4-web-actions.log),
[corrected web gate](benchmarks/isogrid/s4-web.log).

## S5 measurements and retention work

Fresh-process CLI checking passes all sixteen cases on the same Apple M1:
24³ recipes take 0.270–0.341 s (budget 1.2 s), and 64³ recipes take
0.479–0.809 s (recipe 1: 0.809 s, budget 3 s; others: 5 s).
The sixteen-child runner takes 9.37 s wall and uses the unchanged sketch with
checked in-memory selector/resolution edits. [CLI log](benchmarks/isogrid/s5-cli.log).
The explicitly mock-linked CPU-only executable checks the full 24³ mesh and
the all-off empty variant: exact P/topology/N/Cd, 54,464/0 points and
13,616/0 primitives. It passes in 2.93 s including its focused build/gate.
[CPU evidence](benchmarks/isogrid/s5-cpu.log).

`bench_core.exe placement` runs one domain, medians of three, checking exact
reference/kernel values before timing each body. The 4,096/16,384-element
interpreter slopes are 0.615 µs/element for arithmetic, 1.327 µs for captured
list lookup and 26.992 µs for a 64-item inner sum. Cost.table's cheap-body
Interp baseline is updated from 0.710 to 0.615 µs; the 4,096-element dispatch
threshold stays unchanged. The measured cold packed compile+execute time at
16 elements is already below the walker for these three supported bodies,
but that does not justify changing the eager record/error prefix contract or
adding compilation work for every small map. No remaining measured warm
phase besides evaluation exceeds 25% of the edit. The calibration takes
8.62 s wall. [Calibration](benchmarks/isogrid/s5-placement.log).

The first 200-selector-flip run passes every geometry digest and latency gate
(24³ evaluate max 126 ms, presented max 208 ms), but retains an additional
33,464,624 heap words. Instrumentation finds only eight geometry identities,
yet 209 preparation calls: returning to a session-cached geometry rebuilt
its display surface and populated the renderer's caches with duplicate arrays.
A cook-owned prepared cache now reuses those eight preparations. It is bounded
by sixteen entries and 64 MiB, keyed by object, physical settings/output and
the complete cook-context projection; close clears it after joining its worker.
Both display and retint use this shared path. The existing cook regression
checks reuse and invalidation. The next 200-flip run keeps all cache counts
and preparations fixed and removes about 95% of the extra retained heap,
but still grows by about 1.7 million words. This is **not** an S5 memory pass.

A separate PXUI regression proves that smaller frames retained retired paint,
overlay, hit and relative-size callbacks in unused slots. The frame now drops
these callbacks after publishing layout, hits and drawing, and before a new
build following an exception. Weak payloads are collected, and the unchanged
1x/2x visual and interaction checks pass (5.39 s focused run, including rebuild).
The original timing/geometry benchmarks and strict flat-heap assertion remain
the acceptance gate; allocation sampling and reachable-owner diagnostics do
not widen it.

Further owner measurements located retained copies in probe closures (which now
capture only the published lookup lists), the autosave identity marker (now weak),
and whole UI frames retained by three rendering caches. UI staging now replaces
one versioned submission, while unchanged 3D draw lists key only on their prepared
layer. The Scene3 preparer reuses the existing `Scene3.same` check so a new wrapper
around the same immutable mesh does not duplicate its uniforms. Tests verify
unchanged/changed UI versions, collection of superseded packets, equivalent 3D
reuse and material invalidation. The new layer-reuse check first fails in 1.45 s;
the corrected focused scene checks/build pass in 1.67 s. Autosave source/recovery
tests remain green. Empty weak free-name memo entries and buckets are discarded
at a new static pass; live frame forces retain their memoization.

Before the measurement audit, a full 200-flip run still **fails** the strict memory gate: live words
are 20,382,242 → 20,382,327, a remaining 85 words (680 bytes), compared with the
original 33,464,624-word growth. The residual is not waived. Every geometry
digest matches; all measured cache counts and bytes remain identical, including
eight preparations (65,146,140 budgeted bytes), sixteen cook entries (21,715,380
payload bytes), zero Flow GPU pipelines/held outputs and **96 renderer pipelines**.
Evaluate mean/p95/max is 42.4/110.0/116.2 ms; time to presented frame is
100.9/168.7/180.5 ms. Wall time is 29.99 s.
[Latest run, still pending](benchmarks/isogrid/s5-flips-pending.log).

The benchmark now preallocates its baseline storage before either collection,
and repeated edits advance frame time by elapsed time so periodic recovery is
initialized during the first full cycle. No extra selector warmup cycle or
heap tolerance was added. The native all-off case also passes: its exact empty
mesh presents in 105 ms, process wall 0.52 s, no Flow GPU transfers.
[Native empty](benchmarks/isogrid/s5-empty.log).
That typecheck/editor-cook/probe/autosave/PXUI/dependency/history-comment
checkpoint passes in 7.11 s including rebuild. That checkpoint left S5 open for
the 85-word retention difference and the final acceptance audit.

### S5 exit and measurement audit

The remaining bytes are accounted for rather than given a tolerance. A closed
eight-edit cycle is now 1→2→3→4→5→6→7→0→1: both snapshots follow a return to
the starting recipe, instead of comparing recipe 7's first-created scene wrapper
with a later equal wrapper. There is still exactly one eight-edit warmup and
200 measured edits, with every recipe and geometry digest checked. The edit loop
returns before collection, releasing its temporary arguments.

Read-only owner/global measurements locate the residual scheduler growth in
Saturn 1.0.0's task rings (Domainslib 0.5.2, OCaml 5.3.0). A first push installs
a two-word reference; a pop/steal clears its task to `()` but retains that empty
cell. Different slots are first used at different edits. The benchmark counts
only these exact empty cells, rejects any retained task, and requires unchanged
total ring capacity. It also counts the actual distinct boxes of the three
frame/FPS/recovery timestamps: their sharing depends on which frame sampled a
timer. Retained workload words are exactly raw live words minus those counted
empty cells and timer boxes. All other heap growth remains a failure. No cache
is cleared, no arbitrary byte allowance is used, and no numeric latency gate
changes. The probe validates the installed layout and fails if it changes.
The source guard admits only the read-only standard-library symbol lookup at
this qualification tool's path; loading a module or using the lookup in product
code remains rejected by injected checks.

The first full run with this accounting passes in **29.21 s**:

| Retention after one / 26 complete cycles | Warm | Final |
| --- | ---: | ---: |
| Raw live words | 20,829,641 | 20,829,643 |
| Counted empty scheduler-cell words | 400 | 404 |
| Scheduler ring slots (capacity) | 368 | 368 |
| Distinct timer-box words | 4 | 4 |
| Retained workload words | **20,829,237** | **20,829,235** |
| Session entries / payload bytes | 16 / 21,715,380 | 16 / 21,715,380 |
| Preparations / budgeted bytes | 8 / 65,146,140 | 8 / 65,146,140 |
| Renderer pipelines / Flow GPU pipelines / held outputs | 96 / 0 / 0 | 96 / 0 / 0 |

Thus raw heap varies by **16 bytes**, with **32 bytes** of specifically counted
empty scheduler metadata added and **16 bytes less** workload retained. The
workload check passes with no positive allowance. Every digest matches the
independent binary64 golden. Evaluate mean/p95/max is **42.6/109.2/113.3 ms**
(150-ms limit); presented-frame mean/p95/max is **101.3/165.8/181.5 ms**.
Peak RSS is 1,624,768,512 bytes, including transient/native rendering allocations.
[200 edits](benchmarks/isogrid/s5-flips.log),
[resource timing](benchmarks/isogrid/s5-flips.time).

The fresh 64³ sequence passes in **9.31 s**: recipe evaluation maxima are
**498.6 ms** (2-s limit), and the longest warm presented frame is **939.0 ms**.
Cold recipe 1 presents in 924.5 ms; seed and resolution edits also match their
goldens. Peak RSS is 3,040,362,496 bytes; this is not the S10 64³ lifetime gate.
[64³ sequence](benchmarks/isogrid/s5-editor-64.log).
Fresh-process CLI checks pass all sixteen cases in **10.25 s**: 24³ takes
0.284–0.757 s, 64³ 0.464–0.828 s, with recipe 1 at 0.828 s against 3 s.
[CLI](benchmarks/isogrid/s5-cli.log).
The final focused type/CPU-only/dependency/source-guard checkpoint passes in
**4.15 s** (slowest action: dependency gate, 2.83 s); prior successful functional
actions are reused. [Checkpoint](benchmarks/isogrid/s5-check.log).

S5 is complete. Track A's CPU route meets the accepted performance, exactness,
empty-geometry, CPU-only and retention requirements. GPU-resident construction
and drawing remain S6–S11; nothing here claims they are implemented.

## S6 CPU deferral (in progress)

The initial strict-record spike passes in **3.75 s** including its rebuild.
`tools/test_isogrid_defer.ml` captures the actual sculpture mask's residual
environment while host volume maps use the ordinary exact CPU kernels. It then
lowers the sculpture's actual checked numeric bindings and mesh/N/Cd arguments
as one record, before their compact/count/range chain is materialised. The
sketch and checked expressions are not rewritten to simpler arithmetic.

`Core_lower.lower ~strict:true` accepts the record as **five regions**. The check
requires real `Array_compact`, `Array_count`, `Array_range` and `Array_int`
statements, executes with strict diagnostics and no reference rerun, and compares
every output field bit for bit with the reference using one/eight domains.
The executable selects `ogpu_mock` and creates no device. Its focused alias is
`@tools/isogrid-defer-spike`, also included in `@tools/isogrid`.

The editor now enables `qualify_workspace ~defer:true`; independent evaluation
and `rays check` explicitly remain eager. Compilable large static SOP maps use
the evaluator's existing residual path; host and small maps stay eager. The
executing graph context and deferral mode are part of inline memo identity,
and deferred maps reserve their full closure-call index interval. Focused
checks cover these boundaries and preserve authored late-error diagnostics.

New static deferred attributes bypass initial-domain frame placement and device
cost probes. Their mesh and attributes cook through the shared CPU executor.
Caller-owned plane writes now use that executor's nested dispatcher too.
`Core_lower` lowers captured arrays of static SOP residuals as explicit producers
and passes their locals to downstream regions, including compact/count/range
chains. Frame/state-dependent SOPs keep their existing lowering. The emitted
regions have no standalone count origin; the eventual resident path still needs
its count-provenance audit. No second executor or device path was added.

CPU-only checks pass for the actual 24³ full and empty workspace, both eager and
deferred, comparing every P/topology/N/Cd field. The fake-device regression
requires static deferred mesh/attributes to cook at one/eight domains with no
initial-domain frame node or device cost/prepare callback. A synthetic captured
compact/count/range/gather chain also exercises direct Core execution and
caller-owned planes at one/eight domains.

Exploratory native runs produce the exact frozen digests at 24³ and 64³ with
zero Flow GPU transfers. The first placement attempt took **7.57 s** cooking at
24³; preserving the CPU route reduced that to **459 ms**, then explicit Core
producers reduced it to about **89 ms** (24³) / **1.14 s** (64³). Evaluate is
about **32 ms** / **227 ms**, and total presentation **226 ms** / **1.52 s**.
These are diagnostic runs, not an accepted S6 performance result: repeated
source materialization and profile coverage still need investigation, and total
64³ time exceeds S5. All-recipe deferred digests, actual faces/positions/body
probes, malformed-cook/CLI diagnostic parity, responsive 64³ frames and the S6
smoke/golden gate remain unfinished. S6 is not complete.

The combined typecheck/IsoGrid (including CPU-only and the new spike)/dependency/
source-guard checkpoint passes in **5.01 s**, reusing the successful spike action.
Its slowest executed action is the existing 12³ geometry corpus at **4.18 s**.
The frozen sketch SHA-256 and `git diff --check` remain unchanged/clean.
[Spike](benchmarks/isogrid/s6-spike.log),
[checkpoint](benchmarks/isogrid/s6-initial-check.log).

The subsequent focused IR/SOP/CPU-only checkpoint passes in **29.50 s** including
rebuilds. The new SOP CPU-route regression and captured-chain/plane checks pass;
existing driven-transform parity required restricting the new Core capture
lowering to frame/state-independent SOP values. The longest executed actions
were the existing 128-row Lisp replay (**21.58 s**) and aggregate SOP unit runner
(**21.46 s**); uniforms took **11.89 s** under the combined load. The four-case
CPU-only IsoGrid action took **6.16 s**. These per-action figures expose existing
over-budget actions; they are not S6 performance acceptance. Temporary candidate
diagnostics are removed. The sketch SHA and `git diff --check` still pass.
[Focused checkpoint](benchmarks/isogrid/s6-compiled-captures.log).

Fresh continuation audit (2026-10-31): the artifact still requires all S0–S11;
S7–S11 are not implemented and the overall objective is not complete. The
focused `check.exe @tools/isogrid-cpu @tools/isogrid-deferred
@lib/rays_editor/test/test_isogrid_deferred @lib/flow_sop/runtest` passes in
**42.25 s** wall. Actual faces/positions and selected body probes match eager
evaluation; the 64³ worker-barrier check completes three editor frames before
publication; malformed deferred topology returns `E_TOPOLOGY` and leaves no
published session entry. All 128 Lisp SOP golden rows pass.

The saved Dune trace exposes over-budget actions under this combined load:
SOP units **36.19 s**, replay **35.59 s**, deferred 12³ corpus **12.27 s**,
workspace zone **11.93 s**, CPU-only geometry/probes **10.15 s**, and worker
responsiveness **8.38 s**. These are evidence of a test-budget problem, not
accepted performance figures. Isolated measurement and smaller independent
actions are still required; no timeout or fixture correctness was relaxed.
`check.exe @smoke-all` also passes, **140.01 s** process wall including its
validation queue; the sequential window action itself takes **131.89 s**.
The all-nine-case deferred 24³ and 64³ geometry corpora both pass unchanged
walker digests. Their queued launcher takes **182.09 s** wall including waiting
for smoke-all; this is not a test timeout (exit 0). The 24³ action is **4.70 s**.
Remaining S6 work is the source-materialisation/performance audit and isolated
test-budget follow-up; S7–S11 remain required.

## S6 continuation and S7 deferred topology

The CLI check exposed one missing S6 boundary: a `with_error` geometry result
is a residual root, so its inner mesh is absent from the static plan-node scan.
`rays check` now cooks frame/state-independent residual geometry roots through
the ordinary host. The existing mapped-error, unused-mesh and malformed-mesh
cram checks pass unchanged; the authored `E_WRAPPED` diagnostic is preserved.
No live geometry is cooked by this check.

The deferred 12³ corpus uses two independent shards; the 64³ corpus uses three.
Every recipe/seed row and digest comparison remains. The coherent sharded,
typecheck, CLI and dependency checkpoint passes in **24.11 s**, with default
12³ shard actions **9.03/9.64 s** under the combined load. The 64³ shards take
15.90–16.89 s under that load, but an isolated unchanged shard takes **4.46 s**.
This replaces the previous **18.49-s** monolithic 64³ action; there is no
fixture reduction or timeout increase.

A fresh 64³ deferred cold run presents in **1.270 s**, comprising **268.6 ms**
evaluate, **784.0 ms** cook and **136.9 ms** prepare; calling-domain allocation
is **573,419,952 bytes**. The digest is the exact frozen CPU golden, with zero
Flow GPU transfers/pipelines. A preceding noisier sample presents in 2.210 s
(998.2 ms evaluate / 951.9 ms cook). The raw first sample is retained as
[cold audit](benchmarks/isogrid/s6-cold64-audit.log). S6's source-materialisation
and profile audit remains open; the spread is not a measured regression fix.

S7 adds `Topology.deferred` on the existing abstract topology, with known
counts, optional homogeneous kind/corner-count shape, `Packed.Deferred` holder
and release contract. Owned arrays remain direct fields. A mutex-protected lazy
materialisation behind the deferred branch runs once even for racing readers,
caches errors, validates the returned counts/shape and drops the thunk/holder
after success. It must not recursively read its own topology. Metadata,
`Geometry.create`, payload accounting and shape-answered `all_triangles` do not
force; all payload queries and `View.view` use the materialised arrays.
`Geometry.releasers` now includes topology, so the existing session ownership
path sees it without a GPU dependency or a second mesh type.

The core regression covers four-domain racing readers, triangle/quad/empty
shape, bad counts/shape, held release before reading, reading before final
release, unheld release before reading, cached materialisation failure,
Point_index, Topology_index, Triangulate and Normal_ops at one/four domains.
The full required `check.exe @lib/rdk/runtest @lib/sop/runtest @test/runtest`
passes in **35.22 s** including rebuild. Core action **4.23 s**, dependency gate
**3.66 s**; the pre-existing draw snapshot is the slowest action at **17.13 s**.
The earlier focused core check takes **2.72 s**. The dependency graph remains
59 libraries / 36 rules / 29 whitelists, with no new edge.

Million-vertex topology constructor, M1, dev profile, seven-sample medians:
one domain **0.941→0.945 ms**, four domains **0.275→0.263 ms**. Calling-domain
allocation for the valid one-domain constructor is **840→872 bytes** (the
four metadata fields); no proportional allocation was added. Malformed cases
preserve their errors and timings. Command:
`_build/default/tools/bench_rdk/bench_rdk.exe topology 1000000`.
[Before](benchmarks/isogrid/s7-topology-before.log),
[after](benchmarks/isogrid/s7-topology-after.log).

The broader CLI cram alias also found an open census/specification mismatch:
`core_census.t` reports new IsoGrid local heads and missing primitive rules.
That check remains failing and must be resolved for S11; the focused CLI
`@tools/lisp/test/check` itself passes. S8–S11 remain unimplemented.

Final S7 core/CLI/dependency checkpoint passes in **6.63 s** after adding bad
metadata, empty payload and released-before-read cases. Its actions are CLI
**5.07 s**, core **3.12 s**, dependency gate **2.78 s**. `git diff --check` passes
and the frozen sketch hash is unchanged. The quieter S6 sample is retained
[separately](benchmarks/isogrid/s6-cold64-second.log); no claim depends on hiding
the first sample.

## S8 resident value execution (in progress)

`Core_exec.execute ?resident` now runs stateless value programs through the
existing resident callbacks, rather than adding a second executor. Regions and
compactions may stay resident; `array/count` uses a settled count callback
without reading its payload. Ordinary CPU primitive and host consumers first
materialise their arguments, recursively through records/lists/descriptors.
The success result settles and retains its placeholders until the caller adopts
or materialises them and closes the execution. Errors close it. A capacity
overflow closes, restores the execution checkpoint and reruns from the start.
State-slot value programs refuse with `E_CORE_RESIDENT` before any dispatch;
the drawing/fold state path retains its existing contract.

The new small emulator regression supplies deliberately wrong placeholder
contents and excess capacity. It proves correct counts, record escape,
unsupported-region CPU fallback, CPU reductions, empty/growing/shrinking
inputs, error cleanup and one overflow retry at one/four domains, against the
reference evaluator. The native Metal runner adds the corresponding dynamic
mask → compact → points/indices/count record, at 9,000 / 5,001 / 0 / 16,385 /
1,025 inputs. Publication reads zero payload bytes, two or more device regions
run for the nonempty cases, and explicit materialisation matches the CPU
exactly. These values remain below the float32 integer ceiling; this does not
close the uint32 topology requirement.

`Host.stats` now separates payload, status, count and bounds readbacks.
The compatibility `readback_bytes` remains payload plus bounds, but now includes
previously omitted resident `finish` reads and lazy kept-output reads. Worker
materialisation uses atomic byte accounting; a cached second read adds nothing.
Every actual successful resident count read contributes four bytes. The native
record test checks zero payload bytes before explicit reading and increased
payload bytes afterwards, alongside count/status totals. The mock lifetime
test checks worker read accounting separately from memoised bounds bytes.
The validation category is still to be added with the topology-status dispatch.

The IR/GPU/native/dependency checkpoint passes in **3.92 s**. The native
record extension passes with all existing resident fold/history tests in
**2.70 s** (native action **1.59 s**). After transfer accounting, the coherent
IR/GPU/native/gate checkpoint passes in **4.14 s** (native action **1.53 s**;
dependency gate **3.22 s**). The final combined type/IR/GPU/native/CPU-IsoGrid/
dependency check passes in **1.24 s**, reusing successful functional actions.
The preceding attempt passed those functional checks but had stale Dune CMI
assumptions after the interface change; the ordinary rerun resolved them,
without cleaning or forcing the cache.

Shared web suites plus native IsoGrid legality/parity pass in **12.20 s**:
92 JS goldens, 83 runtime programs / 369 frames, exact gather/control cases,
and the existing five native IsoGrid pipelines. No web SOP/Scene target was
added. The frozen sketch hash and `git diff --check` still pass.

Still required for S8: exact uint32 storage through mesh integer producers and
compaction (the existing resident path uses float buffers), topology validation
by a small status read, ownership transfer out of resident slots into deferred
P/topology, production mesh integration and the isolated native mesh alias.
There is no claim yet that the actual `sop/mesh` cook publishes a resident mesh.
S6's materialisation/profile audit and S9–S11 remain open as well.

The final executor follow-up also tests repeated identical frames with
`remember_loops:true`, a resident array fold that retains a pre-fold array,
and an ordinary CPU text fold over resident indices. Value executions bypass
the draw's loop-result cache and loop-close scope, keeping their buffers alive
until their own caller closes; CPU iterators materialise their source first.
IR plus native GPU checks pass in **12.00 s** including rebuilds after these
checks. This closes an internal lifetime hole in the new API, not the pending
mesh-carrier ownership transfer.

## S8 exact uint32 buffers and compaction

The native emitter can select scalar output buffers as uint32, and select
explicit source/gather buffers as raw uint32. Buffer widths, binding numbers
and uniform layout remain unchanged; default float32 emission is preserved.
Integer outputs bypass float32 conversion and validate their 0..2^32−1 range
with status bit 64 (`E_GPU_UINT32_RANGE`). `Run.readback` decodes exact raw
words as `Int_array`; planar reading converts those decoded integers directly
to doubles. No NaN payload is routed through mapped-float conversion.

Uint32 source/table selections require explicit device bindings, refusing
`E_GPU_UINT32_BINDING` before any upload if missing. They cannot implicitly
bind the existing float32 holders or silently upload float32 host arrays.
`Run.keep` still refuses `E_GPU_UINT32_KEEP`: typed topology ownership transfer
is pending, so a raw uint32 buffer cannot escape through the float32 holder.

`Run.compact ~uint32:true` reads uint32 masks and writes uint32 indices. Both
storage entry points expand one hygienic macro in `shaders/compact.rays`, with
the same count/scan/scatter algorithm and count/scratch layout. The print tool
derives both Metal and SPIR-V from that Lisp; there is no handwritten shader,
new OGPU capability, duplicate executor or new library edge.

New isolated native alias: `check.exe @lib/flow_gpu/test_uint32`, also in
`runtest-native`. It checks outputs above 2^24, 2^32−1, mixed float/uint32
outputs, empty output, invalid negative/overflow results, and exact raw source
and table chaining. The producer is not read back before its consumer: the
test verifies its payload counter stays unchanged. Missing typed bindings
refuse. Compaction covers empty, 1, 1,023, 1,024 and 1,025 entries, device
counts shorter than capacity, and a sparse 16,777,219-entry mask whose only
three outputs are **16,777,216 / 16,777,217 / 16,777,218**. The middle index
would be lost by the old float32 path. All native handles return to baseline.

Emitter unit checks reject invalid output/input/table selections and retain
the existing float ABI. Mock/emitter/SPIR-V/native checks pass in **3.10 s**
with the new compaction, then **1.25 s** after adding raw source/table chaining
(new native action **0.42 s**, emitter **0.39 s**). The final coherent type/GPU/
native/dependency checkpoint passes in **1.03 s**, reusing successful actions.
The first typecheck after the layout interface changed had the known stale
Dune CMI assumptions; ordinary rerun passes, without cleaning or forcing.

Real Chrome WebGPU qualification passes after the shared compaction-macro
change, including count/compaction, typed error cases, live sketches and image
parity. Launcher wall is **61.91 s** including waiting for the preceding check;
the slowest qualification action is image E2E **44.98 s**, followed by all-page
opening **38.30 s**. These stay in the optional qualification lane.
The frozen sketch hash and `git diff --check` remain unchanged/clean.

Reproduce the isolated test and its resource measurement:
`_build/default/tools/check.exe @lib/flow_gpu/test_uint32` and
`/usr/bin/time -l _build/default/lib/flow_gpu/test_uint32.exe`.
[Raw native timing](benchmarks/isogrid/s8-uint32-native.log).

The opt-in `Host.resident ~uint32:true` path now selects typed integer sources,
gathers, outputs and scalar captures, with separate float/uint32 site caches.
Ordinary CPU arrays upload through checked uint32 packing; implicit float
holders cannot be reinterpreted. Exact maximum-uint packing uses integer
conversion (the native float-to-int32 conversion saturated above signed max).
Integer ranges are generated on the device from `shaders/indices.rays`, rather
than staged in size-dependent CPU arrays. Typed range preparation recognizes
the authored `:type int` argument and preserves reference errors for negative
counts instead of silently clamping them to zero.

Native checks cover chained map/copy/gather records above 2^24 with zero payload
readback before explicit materialisation, exact scalar/frame captures, checked
CPU uploads, and direct range/compaction at lengths 0/1/1025/2049. The latter
also exercises growing range caches and scratch cleanup; native handles return
to baseline. The isolated expanded executable measures **0.10 s**, **167 MB**
maximum RSS (raw timing linked above). The focused IR/GPU/native checkpoint
before the direct-range regression passed in **0.49 s**, reusing cached actions;
the additional native regression passes too.

The final combined type/IR/GPU/native/web/dependency/Chrome qualification
checkpoint passes in **1.24 s**, reusing successful actions. The preceding
**69.66 s** attempt completed the Chrome checks but hit the known stale editor
CMI assumptions after the interface update; the ordinary rerun passes without
cleaning or forcing. Slowest qualification actions were image E2E **39.85 s**
and open-all **33.06 s**, both in the optional lane.

This remains an internal execution boundary, not production mesh placement.
Typed ownership transfer, topology-validation integration, deferred publication, mesh
lifetimes and S9–S11 remain outstanding. `Run.keep` still refuses uint32
outputs instead of handing them to float-only holders.

### S8 topology validation boundary

`shaders/topology.rays` now checks uint32 topology in one 256-lane workgroup,
with strided row scans and nine uniform collective sums. Its fixed **36-byte**
record reports missing/nonzero initial offsets, wrong terminal/counts, invalid
point references, unordered offsets, bad kind codes, insufficient corners and
a caller-supplied maximum polygon size. `Run.validate_topology` checks buffer
capacities before dispatch and retains native topology diagnostic messages;
`Host.validate_topology` caches one status buffer and counts successful status
reads as `validation_readback_bytes`, including rejected topology. There is no
payload read or ownership transfer.

Native cases cover every rule, empty meshes, 1,025 primitives, bad unsigned
encodings of negative values, exact references above 2^24 and near 2^32,
success after rejection, status-buffer reuse, and invalid capacities rejected
before any status read. Host counters remain zero for payload and exactly
36 bytes per validation. Handles return to baseline. Generated SPIR-V passes
`spirv-val --target-env vulkan1.1`.
Additional valid/invalid vertex streams come directly from dispatched uint32
kernels; validation consumes their buffers without reading producer payload
(native follow-up **1.44 s** including rebuild).

GPU/native/dependency checks pass in **3.51 s**; the expanded uint32 test
passes in **1.46 s** including rebuild. The combined type/GPU/native/gate
checkpoint passes in **1.04 s** cached after the usual stale editor CMI rerun
(first attempt **11.38 s**). The complete isolated native executable measures
**0.12 s / 168 MB RSS**; [raw timing](benchmarks/isogrid/s8-topology-validation-native.log).
Production mesh publication still needs to call this boundary and transfer
resident topology/P/N/Cd ownership into the deferred carriers.

### S8 exact output holders

`Run.keep_uint32` now owns an exact unsigned output through the existing
bounded buffer pool, GC pressure, retirement budget and close lifecycle. Its
integer materialisation decodes raw words once, including values above 2^24;
it never registers a float input binding. The typed holder exposes a settled
unread stream borrow for native consumers. Materialised/freed holders decline
new borrows. A runner close materialises live holders by default; discarded
outputs remain unread. The common initial-domain materialisation boundary now
checks dispatch status instead of merely waiting, so a failed typed output
cannot escape validation.

Native checks retain one output across another dispatch, pin it, read it
concurrently at four domains and sequentially at one, and verify one payload
read plus a cached integer-array identity. They also cover idempotent unpin,
release-before-read, retirement followed by close and late reads, failed
uint32 status before materialisation, and zero leaked native handles.
GPU/mock/native checks pass in **2.74 s**; final type/GPU/native/dependency
checkpoint passes in **1.03 s** cached after the known stale editor CMI rerun
(first combined attempt **12.83 s**). The expanded isolated executable is
**0.12 s / 170 MB RSS**; [raw timing](benchmarks/isogrid/s8-uint32-holder-native.log).

This covers runner outputs. Resident-executor slots currently own separate
buffers: transferring those into these holders, including generated ranges,
and wiring validated publication through both mesh call paths remain open.

### S8 resident ownership transfer

`Host.resident_scope` now exposes final-stream retention alongside the shared
executor. It settles before transfer, retains actual counts, memoises aliases
to one holder and detaches payload ownership from slot cleanup while returning
count-cell scratch. Generated integer ranges leave the host range cache when
retained. One lazy ownership runner per host tracks these holders through the
existing pool, retirement budget, worker materialisation counters and shutdown.
`Run.adopt_uint32` and `adopt_float` accept completed, status-validated buffers
without copying or reading payload. Unmapped adopted buffers are destroyed
after use rather than mistaken for reusable mapped output slots.

The native checks retain a record of aliased high integers, copy/gather fields
and float3 positions, close the executor, and only then materialise it from
eight readers at four domains. All results match the reference exactly; no
payload was read before scope close, and repeated materialisation adds no
traffic. Direct range/compaction retention covers lengths 0/1/1025/2049,
including transfer from the host's index cache. All native handles return to
baseline. GPU/mock/native checks pass in **3.32 s**, then **2.29 s** with the
range/float3 cases. Final type/GPU/native/dependency checkpoint passes in
**3.10 s** (native action **0.44 s**, dependency gate **2.32 s**), after the
known stale editor CMI rerun (first attempt **11.41 s**).
The isolated expanded executable is **0.11 s / 171 MB RSS**;
[raw timing](benchmarks/isogrid/s8-resident-transfer-native.log).

Both mesh paths still need to consume this through the backend-neutral GPU
configuration, validate the retained streams and install deferred P/topology
and fused N/Cd. This transfer API alone does not establish resident geometry
publication or viewport rendering.

### S8 backend-neutral residency

`Flow_ir.Gpu.backend.residency` now supplies the resident executor, owned
integer/float retention and topology validation over Flow values, without an
OGPU handle in Flow IR or SOP. Backends may decline with `None`. Native fast
precision supplies it; exact precision declines before acquiring an owner.
`Executor.force ?resident` carries the scope through ordinary and cached root
execution, retaining strict execution on the cached path. The workspace GPU
service routes every scope callback, including retention, validation and
cleanup, to its owner domain. Settled holder reads/releases remain any-domain.

The expanded native test uses the neutral backend and `Executor.force`, not
a direct Core call, for two consecutive live inputs through one compiled
program. A 342-triangle mesh-shaped record validates with exactly 36 status
bytes, transfers topology and float3 arrays, closes its scope without payload
reads, then matches reference at four domains. Alias identity and repeated
read accounting still pass. A mock workspace-service worker exercises eleven
scope operations and asserts owner-domain execution of each. No new dependency
edge was added.

IR/native/service/gate checkpoint passes in **11.81 s**; expanded service and
type checks pass in **6.24 s** (service action **0.83 s**). Final combined
type/IR/GPU/native/service/dependency checkpoint passes in **12.21 s**, after
the known stale editor CMI rerun (preceding type attempt **17.00 s**).
Slowest actions are existing uniforms **8.46 s**, IR **7.88 s**, core regions
**5.15 s**; all remain inside the section budget. The isolated expanded native
executable measured **0.26 s / 172 MB RSS** during build load;
[raw timing](benchmarks/isogrid/s8-neutral-residency-native.log).

Mesh node cooking has not yet adopted the capability: wiring the two mesh
paths, installing validated deferred geometry and preserving fused N/Cd are
the next S8 work. S9–S11 and the S6 profile audit remain open.

### S8 first deferred mesh publication

Shared `geometry_arrays_node_in` cooking now offers mesh records to resident
execution under fast Qualification policy. Both ordinary nodes and compiled
host mesh calls use this constructor. `Topology_kernel_impl.mesh_resident`
validates before retaining fields, installs deferred Float3 positions and
Topology with metadata-only counts, and keeps the integer holders in the
topology's existing extensible carrier. CPU access forces those carriers.
Unsupported stream shapes materialise before scope close and use the ordinary
mesh constructor. Errors release partial retention; cancellation after
construction releases unpublished carriers. Reuse keys now include GPU policy.

The first concurrent native cook exposed a real gap in the previous holder
checks: scheduling sometimes left all reads on the main domain, hiding that
raw `read_buffer` is illegal on a Metal worker. Resident slots and generated
indices now allocate mapped buffers. `Backend.mapped_view` retrieves their
existing owned view; uint32 materialisation invalidates it and uses a noalloc
C word accessor, preserving all bits (including NaN-shaped uint32 maximum)
without a GPU operation on the worker. Adopted buffers require mapped storage
and reuse the existing pool. Shared mock/Metal conformance covers mapped-view
identity and destroyed/unmapped refusal. The new native publication test forces
a major GC before eight readers at four domains, making this boundary explicit.

`@lib/flow_sop/test_mesh_resident` cooks a 342-triangle Lisp mesh with `exact`
fields twenty times through fresh host/session lifetimes. Each publishes with
**0 payload bytes / 36 validation bytes**, deferred P/topology, exact arrays on
concurrent CPU access and no extra traffic on a later read. A GPU-generated
out-of-range point returns `E_TOPOLOGY`, zero payload bytes and no publication.
Every iteration returns native handles to baseline. The test uses a fixed
host clock and no window or wait. Its isolated **21-case** executable measures
**1.37 s / 107 MB RSS**;
[raw timing](benchmarks/isogrid/s8-mesh-publication-native.log).

Mapped/uint32/native/conformance checks pass in **5.34 s**. Full focused SOP,
type, native mesh, GPU, OGPU and dependency checks pass in **23.80 s** after
the policy-key update; the final native follow-up passes in **3.50 s** including
queue/rebuild (native action **1.85 s**). Earlier stale editor CMI assumptions
resolved with the ordinary rerun; no cleaning or timeout changes.

This does not yet prove the frozen sketch's publication: its constant
`array/int` kinds need device generation, and fused N/Cd must stay resident.
Host-call native cases, remaining malformed/count/empty/cancel cases, measured
placement, S9–S11 and the S6 audit remain required.

### S8 constant fills and frozen-workspace publication

`Core_exec.resident.fill` now offers validated `array/int` construction to the
device. The existing generated range kernel also writes constant uint32 words,
without count-sized CPU staging. Values outside uint32 decline to ordinary
signed CPU arrays; invalid buffer capacities reject before dispatch. Filled
arrays follow the same slot/retention lifecycle and workspace owner routing.
Native coverage includes 0/1/1025/2049 lengths, exact uint32 maximum, signed
decline, over-capacity refusal, and mesh kinds created by authored `array/int`.
The service mock checks twelve routed scope operations. SPIR-V validates.

The frozen 24-cubed workspace now cooks through Qualification policy with
**54,464 points / 13,616 primitives**, resident P/topology, **0 payload bytes**,
**36 validation bytes**, 12 status bytes, 24 count bytes and zero bounds bytes
at publication. The new isolated
`@lib/rays_editor/native_qualification/test_isogrid_resident` alias checks that
publication and reports attribute residency. Its implementation is the
read-only `bench_isogrid_resident.exe`; it verifies the frozen hash and never
rewrites the sketch. [Raw workspace probe](benchmarks/isogrid/s8-workspace-residency.log)
measures **0.82 s / 159 MB RSS**. These are publication counts, not viewport
timings or a full materialised-geometry parity result.

The same probe shows **N and Cd are not resident**. Qualification now permits
deferred attribute writers to attempt the existing GPU route; Measured policy
retains its CPU guard pending consumer cost evidence. Permitting this route
alone did not retain N/Cd: inspect their prepared data/record-region refusals
and fusion before claiming completion. Full frozen-geometry CPU parity, the
remaining S8 cases, S9–S11 and the S6 audit remain open.

Full focused type/SOP/GPU/IR/native/service/dependency checks pass in
**24.03 s**. The final combined type/native/workspace/service/gate checkpoint
passes in **7.00 s** after ordinary stale-editor-CMI retry (first interface
checkpoint **18.02 s**). Slowest native actions: mesh **2.44 s**, frozen
workspace **1.24 s**, service **0.86 s**; gate **3.08 s**. Expanded uint32/fill
executable alone is **0.13 s / 172 MB RSS**;
[raw timing](benchmarks/isogrid/s8-integer-fill-native.log).

### S8 fused N/Cd residency and frozen CPU parity

The writer's two elementwise regions both had unknown counts/origins, and
rewriting the color capture had assigned fresh IDs to its copy of `vertices`.
`Core_fuse.merge` now also proves equal Zip cardinality from equal ordered
immutable source sequences. It resolves aliases and reuses the existing
structural/declaration keys rather than forcing counts or inventing a cache;
operator implementations must agree and stateful sources decline the new
proof. `Residual.residual_authored_key` exposes the existing lazy checked
declaration key. Tests fuse copied unknown-count sources at lengths 0/1/1025
with exact one/four-domain output, and refuse different counts/declarations.

The frozen 24-cubed native qualification now publishes **P, topology, N and Cd
all resident**, with **0 payload bytes / 36 validation bytes / 16 status bytes /
24 count bytes / 0 bounds bytes**. The native alias asserts all four carriers,
then forces a CPU reference record and verifies exact topology and every P/N/Cd
component within **1e-6**. The first CPU read transfers exactly **2,287,492
bytes**, the sum of those six arrays; another complete read adds zero bytes.
[Full raw parity run](benchmarks/isogrid/s8-workspace-fused-parity.log) measures
**5.11 s / 172 MB RSS**. The frozen file hash is unchanged.

Type/IR/native/SOP/GPU/gate checks pass in **23.61 s**; the final type/IR/native/
gate checkpoint passes in **7.44 s**, native parity action **5.75 s**. A combined
interface rebuild previously hit stale editor CMI assumptions and ordinary
rerun passes without cleaning. Temporary tracing has been removed.

Performance is not qualified: the equivalent publication-only command is
**3.62 s / 161 MB RSS**, versus the earlier **0.82 s / 159 MB** probe with
CPU-backed N/Cd. [New publication timing](benchmarks/isogrid/s8-workspace-fused-publication.log)
uses `bench_isogrid_resident.exe --publication-only`; the full native alias
also runs parity. This is Qualification policy, not evidence for production
device placement. Profile preparation/count forcing before S10; keep the
Measured guard until the consumer cost evidence exists.

The combined run measured existing `test_uniforms` at 13.88 s; isolated it is
**4.98 s**, confirming load sensitivity. Existing SOP units/replay actions
still measured **19.69 / 19.08 s** in the combined run; their previously tracked
S6 test-time audit remains open. Remaining S8 malformed/empty/growth/cancel/
host-call cases, S9–S11 and the S6 profile audit still prevent completion.

### S8 preparation profile and packed evaluator reuse

Sampling the fused publication found reference walking, structural hashing and
allocation on the main domain, rather than device execution. Count expressions
for index-only regions and uniform roots bypassed packed evaluation; their
nested maps were walked element by element. Preparation now uses its existing
packed evaluator for both. Unsupported roots offer stateless nested kernels
back to the same evaluator with the current resolver. Stateful and unsupported
kernels keep reference semantics. The source comparison already used to prove
fusion also compacts repeated immutable inputs; no new cache is involved.

Three-sample medians on Apple M1, Dune dev, frozen 24³ and one cook domain:
publication **2.940580 → 0.398420 s** (86.5% less), main-domain allocation
**10,214,878,592 → 344,869,056 bytes** (96.6% less). These measure load/lower/cook
through publication, excluding CPU parity and teardown. The baseline holds
source compaction constant to isolate count/uniform preparation.
[Before](benchmarks/isogrid/s8-prepare-before-2.log) and
[after](benchmarks/isogrid/s8-prepare-after-3.log); all three samples of each
are beside them, with intermediate count-only measurements. Allocations are
main-domain allocations, not the shared pool's total. Historical logs label
the requested cook domain count as `domains`; the probe now says `cook_domains`.

New focused checks cover nested-map index counts and scalar uniforms at
0/1/5001 elements and assert the packed kernel runs. Unknown-count fusion and
source compaction retain exact one/four-domain parity and refusal cases.
The focused IR alias passed in **12.63 s**; slowest sections uniforms **8.88 s**,
IR **8.12 s**, core regions **7.24 s**. The final type/IR/GPU/native/web/Chrome/
dependency checkpoint passed in **47.87 s**, including full frozen CPU parity.
Publication still has **zero payload / 36 validation bytes**, all P/topology/
N/Cd resident; CPU reads still transfer **2,287,492 bytes once**. The production
Measured guard remains: this resolves preparation cost, not S10 placement.

S9 tracing confirms the current Surface rejects point Cd and forces triangle
topology while constructing its device mesh. The host triangulator uses
position-dependent projected ear clipping (including degeneracy checks), not
a fixed 0–2 diagonal. Device triangulation must preserve that decision and
the host fallback for unsupported inputs; merely enabling quads would be wrong.

### S8 empty placeholder identity and changing mesh counts

The native mesh regression now recooks primitive counts **0 → 342 → 683 →
1025 → 0 → 342** through one GPU host and session, clearing the session cache
between cooks while holding every previous geometry unread. Eight readers on
four domains then compare every topology word and position, including all old
meshes; repeated reads add no payload. Handles return to the baseline after
release. The original twenty publication/GC iterations and malformed GPU-origin
rejection remain in the same executable.

The first empty frame exposed a real identity bug: OCaml's empty arrays share
an atom, so unrelated integer/position placeholders could name one another's
buffers and turn vec3 points into integers on fallback. Every placeholder
allocation (including coerce and post-count ranges) now reserves at least one
element for a distinct identity while retaining its actual logical count zero.
This changes no payload count or public array result and needs no new type or
cache. Empty maps keep their existing host route; a warmed offset site can read
its one four-byte word. Nonempty publications in the sequence remain resident
with zero payload and 36 validation bytes each. Empty topology and points are
checked exactly, so no triangles survive a shrinking count.

The expanded mesh/uint32 focused check passes in **3.32 s**. Final type/GPU/
mesh/frozen-parity/service/dependency checks pass in **7.06 s**. Frozen publication
is **0.405868 s**, still zero payload with all four carriers resident; full CPU
parity and its exact once-only readback pass. Frozen source hash and
`git diff --check` pass. This closes the exercised empty/growth/shrink/held-old
mesh case, not the remaining S8 overflow/cancellation/host-call qualifications
or S9–S11.

The uint32 holder test also directly asserts independent empty range, fill and
compaction outputs retain distinct holders, closing the zero-length identity
case even when all their materialised arrays would compare equal. Its final
focused checkpoint passes in **1.44 s** (uint32 action **0.56 s**).

### S8 malformed publication, CPU consumer and cancellation

The existing isolated native mesh executable now runs 32 cases. In addition to
its twenty GC/publication iterations and changing-count sequence, it checks
out-of-range and negative vertices, missing/incorrect initial offsets,
incorrect terminal offsets, non-monotone offsets, inconsistent kind counts,
invalid kinds and too-small primitives. Every failure matches the CPU message
and code (`E_TOPOLOGY`, or the established `E_TOPOLOGY_KIND`), publishes no
geometry and retains no session cache entry. GPU-validated malformed cases
read zero payload and one 36-byte validation record. Missing offsets currently
take the host fallback and read **19,156 payload bytes** before their error;
that malformed-input readback remains an open S8 optimization/qualification.
The lower-level native validator separately covers all nine flags, including
the caller-selected maximum polygon size and empty buffer lengths.

The signed negative-index case found a more serious problem: after one field
failed, Core closed the scope but Executor resumed the record walk with an
earlier field's invalid placeholder. This could produce an intermittent
`E_NONFINITE` or interpret uninitialised host memory. Executor now propagates
resident failures out of the whole walk and retries the entire CPU value under
the original strict policy. Partial placeholders never escape that failed
attempt. This fixes the shared executor rather than special-casing mesh fields.
The negative case now reports the CPU topology error with **zero payload and
zero validation reads**. The test poisons every float placeholder with NaN,
so the former accidental host use is deterministic. A small mock regression
fails the second of two independently forced record fields after the first
became resident, then checks the complete CPU result without a payload read.

A native **Clean** SOP downstream reverses the triangle winding with degenerate
removal disabled. Its positions and topology match the expected host result,
and it reads exactly **19,156 bytes once**; another complete read adds nothing.
A callback cancels after device settlement during the cook, checks no
publication/cache entry and no payload/validation reads, then recooks through
the same host/session successfully with a fresh context. Every case returns
native handles to its starting baseline.

Host-call residency is still open. Tracing a geometry-count-dependent branch
confirmed `flow.geometry_select` executes its body without a resident scope;
Core's `Host_source` materialises arguments and Lower's `force_args` forces
mesh arguments before `host_geometry_arrays`. Reusing the final mesh
constructor alone does not retain arrays through this bridge. This needs a
scope-aware handoff, while all other host readers must continue to materialise.
The production Measured policy, remaining overflow/scale audits and S9–S11
remain unchanged and incomplete.

The IR/native/uint32 checkpoint passes in **13.61 s**. The added two-field mock
regression's focused rerun passes in **1.04 s** (other IR actions cached).
Combined type/IR/SOP/GPU/native/service/web/dependency checks complete after
the known stale-editor-CMI retry: **39.89 s** for the initial run (two CMI
compilations failed; test actions passed), **1.26 s** for the successful retry,
without cleaning. The initial trace's slowest existing actions are SOP units
**28.71 s**, golden replay **28.02 s**, web grade/index **18.73 s**, web gather
**13.65 s**; the existing S6 test-time audit remains open. Frozen parity action
is **4.11 s** and passes. The frozen source hash is unchanged, temporary tracing
is removed, and `git diff --check` passes.

After the combined run, the 32-case native executable alone passes in
**2.05 s**. Frozen publication alone is **0.361449 s** with **344,868,848
main-domain allocated bytes**, one cook domain, zero payload bytes and all
P/topology/N/Cd resident. This is one follow-up sample, not a new median.

### S8 structured host calls and retained input borrows (2026-11-01)

The exercised host-call gap is closed for `sop/mesh`. Stateless structured
geometry selection now acquires a Qualification scope and offers raw host
source arguments before the normal materialising boundary. Mesh fields go
through the same `Topology_kernel_impl.mesh_resident` constructor as ordinary
mesh cooking. Other sources and unsupported shapes retain their host route.
A device failure closes the scope and retries the selection on CPU; cancellation
does not retry. Core's offer is language-neutral and rejects wrong types or
unresolved source results. Constant `array/int` descriptors reuse their existing
validated decoder and device fill callback, which keeps the kind stream native.

Publication inside a program requires its arrays to remain usable afterwards.
Retained buffers are now pinned until scope close, detached from reusable slots
without invalidating their input placeholders, and their count cells return only
at close. CPU reads use the holder cache. Integer prefix views over one range
buffer keep separate logical counts and share one physical owner/read. This
also fixes the zero-vertex/one-offset alias in a device point cloud. Different
length views retire together through the existing GC/pressure machinery; the
explicit `ponytail:` comment names reference-counted release as an upgrade only
if pressure warrants it. No dependency, mesh type, kernel copy or OGPU API was
added.

The native executable now has **40 cases**. New structured-host cases assert
the `flow.geometry_select` route and cover publication, two mesh calls reusing
the same arrays, native Clean readback, cancellation plus recook, malformed
vertices, signed-index device refusal with CPU error parity, changing counts,
and point clouds. Host mesh publication reads **zero payload / 36 validation
bytes**; two constructors read 72 validation bytes. Clean reads the same
19,156-byte payload once; signed refusal reads no payload. Eight readers on four
domains still verify exact old/new arrays and repeat-read stability, and every
case returns native handles to baseline. The typed stream test also retains
17/1025-element views in both orders, checks logical pin lengths, reads/releases
the smaller view, then compacts the original larger input on GPU. One physical
4,100-byte read serves both views, and the subview array is cached. Core tests
cover accepted/declined offers and wrong-type/unresolved results.

Full type/IR/SOP/GPU/native/service/web/dependency checks completed after the
known stale-CMI retry (**49.18 s + 1.23 s**, no cleaning). The final focused
type/IR/native/service/gate check passes in **15.51 s**, including frozen CPU
parity. The 40-case focused native check passes in **4.54 s**; the preceding
38-case action was **2.94 s** warm. Larger combined load raised existing
uniforms to 11.23 s (5.59 s with the small native run), and existing SOP
units/replay to 28.67/29.17 s; their earlier test-time audit remains open.

Three publication-only samples on Apple M1, Dune dev, frozen 24³, one cook
domain give median **0.341484 s / 344,871,544 main-domain allocated bytes**.
[Raw samples](benchmarks/isogrid/s8-host-publication-1.log) and its adjacent
`-2`/`-3` logs retain all five readback categories. All four carriers remain
resident with zero payload / 36 validation / 16 status / 24 count / 0 bounds
bytes. This shows no observed regression against the preceding 0.398420 s
median; it is not a causal speedup claim. Frozen hash and diff checks pass.

S8 native mesh overflow qualification, the missing-offset fallback readback,
remaining scale/profile/lifetime audits, S9 viewport drawing and S10–S11 remain
open. Measured production placement and unsupported host consumers are unchanged.

### S8 native counted-capacity recovery and missing-offset rejection

The native test now includes a GPU filter/compaction over **8,196 indices**
(a partial final workgroup), feeding points and vertices. Across primitive
counts **0 → 342 → 683 → 1025 → 0 → 342**, both ordinary mesh construction and
the structured host call perform **two actual capacity-overflow replays**.
Nonempty publications remain resident with **zero payload / 36 validation
bytes**, all old geometries stay unread until the sequence ends, and eight
readers on four domains then verify exact old/new topology and positions.
Empty results use the existing zero-payload host path when the one-word offset
map declines device placement. No stale or excess vertices survive shrinkage.
[Raw 42-case native run](benchmarks/isogrid/s8-counted-native.log): **3.27 s /
116 MB RSS**, Apple M1, Dune dev.

The test exposed three shared preparation issues. Frame-dependent records were
forced field by field, range filters used the mask/compaction lowering only in
drawings, and record programs with regions were prepared anew each force,
losing their capacity history. Resident records now use one whole-value Core
program, structured values make array producers explicit and reuse the same
filter lowering, and the existing root cache retains region programs. The
rootless scalar fast path still requires `Core_exec.direct`; region programs
continue through the per-force dispatcher/resolver. No new cache or compiler
mode was added. Structured host bodies use the same structured-value lowering.
Their mesh eligibility check settles via the count callback, which avoids the
post-fold transition that would disable capacity history for later regions.

This also closes the previously logged missing-offset readback: the dynamically
empty offset stream now reaches the GPU validator, returning the exact CPU
error with **zero payload / 36 validation bytes**, instead of draining 19,156
bytes. Signed-index refusal still reruns the whole CPU value and returns its
topology error with zero payload. The native executable now has **42 cases**,
including the direct and host counted-recovery cases; handle baselines and
repeated-read counters pass. Temporary tracing is removed.

The final type/IR/SOP/GPU/native/service/web/dependency checkpoint passes after
the known stale-CMI retry (**52.27 s + 1.25 s**, no cleaning). The initial run's
only failed actions are the two existing editor bytecode compilations; runtime
test actions pass. Under that combined load, native cases take 12.12 s versus
3.27 s alone; existing units/replay take 31.41/31.05 s. The earlier default-test
budget audit remains open. Native rendering, measured placement, remaining
scale/profile/lifetime audits and S9–S11 are not claimed complete here.

Real Chrome WebGPU qualification also passes in **45.79 s**. Three final
publication samples give median **0.350908 s / 344,868,024 main-domain
allocated bytes** on the same Apple M1/frozen 24³ configuration; see
[sample 1](benchmarks/isogrid/s8-counted-publication-1.log) and adjacent
`-2`/`-3` logs. Publication remains zero payload / 36 validation / 16 status /
24 count / 0 bounds bytes.

### S9 borrowed index draw plumbing (2026-11-01)

The existing renderer registration now optionally borrows a UInt32 index
buffer and its settled count. Scene3 submissions carry empty host index bytes;
the ordinary indexed draw and sun-shadow draw use the borrowed buffer at
offset zero. Replay fingerprints include its identity, and producer validity
guards retained draws. Count/size mismatches, simultaneous indirect indices,
and host bytes accompanying device indices are rejected. CPU mesh combination
continues to require actual host indices. A fully borrowed mesh keeps only a
fixed four-byte backing allocation for the existing cached metadata record;
it uploads no mesh payload.

The mock regression checks actual index bindings, no mesh uploads, unchanged
replay uploads, stale replay and malformed inputs. A real Metal offscreen
comparison draws the borrowed positions/indices for three frames and compares
every pixel with the host-index draw. Focused renderer/scene-execution checks
and the dependency gate pass; the native checkpoint takes **2.70 s**.
The final type/renderer/scene-execution/gate check passes after the known
stale editor-CMI retry (**17.64 s + 1.04 s**, no cleaning). The slowest action
is the dependency gate at **4.11 s**; the frozen sketch hash and diff check pass.
This is draw plumbing only: the quad index kernel, colour stream, Surface
integration and full viewport/consumer qualification remain open, so S9 stays
in progress.

### S9 resident colour stream (2026-11-01)

The same registration now optionally borrows packed float3 or float4 colours.
The existing attribute float binding (4) carries them; no shader parameter,
pipeline family or native API is added. Surface uniform word 81 carries the
component width, checked against the registration before drawing. Float3 is
opaque; float4 preserves alpha. These untextured device colours use zero UV.
Buffer widths/sizes and unsupported draw families are rejected, and replay
signatures retain the colour buffer identity and producer validity.

The Scene3 Lisp shader converts channels through the existing packed-colour
unpacker, matching the host's clamped 8-bit `Color.of_floats` before
interpolation. Naïve float32 multiplication failed native pixel parity for
`0.7`: its exact product with 255 lies below the half-byte boundary, but the
rounded product lands on it. The shader uses the FMA multiplication residual
to resolve that tie. The native regression covers both component widths,
varying RGB/alpha, out-of-range channels and this rounding edge, comparing
every framebuffer byte over three frames per width. Buffers remain owned
until the renderer releases retained draw resources.

The native/renderer/scene-execution checkpoint passes in **2.91 s**. Final
type/renderer/native/scene-execution/GPU/gate checks pass after the known
stale editor-CMI retry (**16.76 s + 1.05 s**, no cleaning); generated SPIR-V
passes `spirv-val` (**0.92 s** action). The slowest gate action is **4.11 s**.
This verifies the renderer/shader path, not the original sketch's viewport:
Mesh metadata, Surface/native binding, quad generation and the full S9 exit
checks remain open.

### S9 mesh metadata and payload-free face lowering (2026-11-01)

`Mesh.View.create_device` now carries an optional device triangle index count
and float colour width alongside its existing key, normals flag, lifetime and
lazy host copy. Counts, identity comparison and triangle counts inspect only
metadata. An indexed device mesh has no host identity-index allocation.
Construction rejects invalid counts/widths and conflicting host indices,
colours or UV. Face staging emits empty vertex/index/attribute bytes, the
settled count and colour-width uniform, without forcing the host copy.

Scene execution accepts the named empty attribute record only when a valid
colour registration supplies it. It skips host storage preparation and binds
the existing colour buffer at the shader's float and unused word bindings;
no size-dependent placeholder attribute allocation/upload is needed. Real
Metal RGB/RGBA parity now runs with empty host attribute bytes. Mock checks
reject an empty attribute record without registered colours.

Wire/vertex modes of a device-indexed mesh currently use its memoised host
copy; textured use also takes the host route rather than attempting an
unsupported device layout. A focused regression verifies non-forcing face
staging/counts, metadata errors, distinct colour-width identity and a single
host materialisation shared by those modes and a CPU reader. The focused
Rays/renderer/native/scene-execution/gate checkpoint passes in **2.91 s**.
The final type/owner/native/gate check passes after the known stale editor-CMI
retry (**18.06 s + 1.04 s**, no cleaning); the slowest action is the dependency
gate at **3.95 s**. The face regression also verifies unchanged-frame staging
identity. Frozen sketch hash and diff checks pass.
Surface/native geometry binding, the exact quad kernel and full S9 viewport
qualification remain open; these host mode transitions are explicit remaining
consumer evidence, not a resident-draw completion claim.

### S9 topology input borrow (2026-11-01)

The host quad rule is position-dependent: Newell's normal selects a dominant
projection, projected area chooses orientation, then the first unblocked ear
is emitted in corner order. A convex quad generally emits `(3,0,1)` followed
by `(1,2,3)`, not a fixed `(0,1,2)/(0,2,3)` split. `Triangulate` additionally
handles one collapsed adjacent edge; the display bridge's actual consumer
must be matched, with unsupported cases taking the recorded host route.

Before the kernel can use the resident topology, its exact integer carriers
must keep their native borrow. They now preserve an extensible backend token,
using the same native holder and ownership. `Run.pin_integers` applies the
existing native pin rules without a payload read or another registry.
`Topology_kernel.resident` retrieves vertices/offsets/kinds and checks their
logical counts without forcing topology; native handles stay outside the
neutral library types.

Focused IR/GPU/native-mesh/gate checks pass in **17.00 s**. The final native
UInt32/mesh/gate checkpoint passes in **5.15 s** (native mesh action **3.89 s**,
UInt32 **0.51 s**). Checks cover unsettled, foreign-domain, materialised and
non-native-token refusal, idempotent unpin, exact >2²⁴ integers, and non-forcing
pins of every nonempty stream through all 42 mesh cases, including dynamic
growth/shrink and capacity replay. Publication payload remains zero and handle
baselines pass. The quad kernel and Surface binding remain open.

The final type/IR/SOP/GPU/native/service/gate checkpoint passes in **7.31 s**
including the validation queue wait. The preceding **32.00 s** run completed
the SOP runtime actions but exposed one worker-service mock integer record
missing the new absent token, alongside the known stale editor CMIs; that
fixture now declares `storage = None` and the service passes (**0.66 s**
action). The final UInt32 action is **0.54 s**, gate **2.67 s**. A second-backend
test token explicitly declines native pinning. No native token or handle is
required by portable/CPU mocks.

### S9 position-dependent index kernel (2026-11-01)

`shaders/triangles.rays` implements Newell projection, oriented projected area
and the host's ordered first-ear test. It reads resident positions and exact
UInt32 topology, writing indices at `3 * (offsets[p] - 2*p)`. Triangles keep
their order; quads emit the first ear and the remaining corners in host order.
One pass writes per-group rejection counts; a second reduces them to a
four-byte status. No new OGPU/Metal command or capability is needed.

Float decisions conservatively refuse near projection/ear boundaries, using
`1e-4 * max_abs_position² + 1e-10`; a known ceiling is small faces far from the
origin. Tied dominant axes, uncertain/collapsed quads, non-finite quad
positions and unsupported primitives return `E_GPU_DISPLAY_TRIANGLES` for
host fallback. No guessed diagonal or stale extra triangle is published.

`Run.triangle_indices` validates metadata, completes dispatch and reads only
that status. `Host.triangle_indices` owns/reuses output and scratch in the
existing display pool (four free buffers per size, 64 sizes), now shared with
derived normals. Release is idempotent and creating-domain only, before the
GPU lease closes. New display allocations are counted. Host accounting shows
four validation bytes per dispatch, zero input upload and zero payload read.

`@lib/flow_gpu/test_triangles` compares every index with `Rdk.Triangulate.run`
for convex, concave and nonplanar quads, four rotations, both windings and all
projections. Mixed triangles/quads, 255/256/257/1025 primitive boundaries and
**65,537 quads** cover partial groups and the second reduction chunk. It also
checks typed refusals, square→concave→square replacement, pool/pipeline reuse,
idempotent release, derived normals and the handle baseline. It is included in
`@runtest-native`, outside the default loop. SPIR-V passes `spirv-val`.

Final native/GPU/gate checks pass in **2.91 s** (triangle action **0.47 s**,
gate **2.33 s**). The full type/GPU/native-mesh/service/gate checkpoint passes
after the known stale editor-CMI retry (**13.73 s + 3.14 s**, no cleaning);
runtime actions pass in the first run. Frozen hash and diff checks pass.
Surface/native binding and the original sketch's viewport image, consumer and
performance gates remain open.

### S9 Surface binding and frozen-sketch viewport parity (2026-11-01)

Surface now offers resident P/N/Cd and topology without first reading corners.
CPU-only deferred topology keeps its host preparation path; resident topology
requires authored point N. The workspace pins typed integer inputs, runs the
index kernel, unpins those inputs after completion, then registers P/N/Cd and
the owned index buffer. Every partial failure releases acquired pins/storage
and records `display_refusal`. The callback returns its actual index count
and colour width with the existing key/lifetime, so a CPU reader forcing
deferred data cannot change Surface's interpretation of a successful borrow.

`@lib/rays_editor/native_qualification/test_isogrid_viewport` loads the frozen
sketch, changes only the checked in-memory recipe literal through `Flow_edit`,
and cooks through the real workspace GPU service under Qualification. For
Maze (0, **3,000 quads**) and Coral (3, **11,866 quads**), every framebuffer
channel is **bit exact** against host drawing: faces, orbit, `frame_bounds`,
World sun/shadows, wire and vertices. Face publication/drawing reads **zero
payload**, adds **four validation bytes**, and uploads only **5,456 uniform
bytes** in the renderer. Repeated face frames upload nothing, create no GPU
buffers/pipelines and allocate **17,136 main-domain bytes/frame** for both
geometry sizes. Bounds use at most **24,576 bounds bytes**, no payload, and
match the subsequent CPU scan exactly. The already-built device mesh still
draws the same pixels after CPU materialisation.

Consumer evidence: faces and shadows borrow the GPU indices/colours; framing
uses the small resident bounds reduction. Path-tracer preparation explicitly
uses the memoised host mesh (confirmed by successful `scene_mesh` composition,
which refuses the direct-device tracer variant). Wire/vertex modes currently
share that host copy and read no additional payload. The picking/inspection
audit below exercises their actual CPU readers before tracer materialisation.
The editor's existing `Loop_model.compose` reuses drawn pieces while graph,
prepared result and texture identity are unchanged, including camera-only
updates; no second geometry/display cache was added.

The window-free affected type/RDK/Rays/renderer/SOP/native-service/tracer/gate
checks pass in **28.41 s**. Existing loaded SOP units/replay take **19.25/
21.80 s**; their earlier S6 test-time audit remains open. The expanded native
viewport/service/gate check passes in **5.99 s**, viewport action **1.60 s**;
the final focused check passes in **2.91 s**. Raw evidence is
[s9-viewport.log](benchmarks/isogrid/s9-viewport.log); paired images are emitted
with `RAYS_S9_IMAGES`. Frozen source hash and diff checks pass. The full editor
shot subsequently passed in **3.63 s** (`s9-editor-shot.log`); production
placement, final native exit, remaining S8/scale/profile audits and S10–S11
are not claimed complete.

The final no-Cd audit removed the per-count default white attribute cache:
plain device meshes now emit empty attribute bytes and use constant opaque
white/zero UV in the shader (word 81 = -1). Real Metal pixels match the old
host white record. Textured device meshes, including the older position-only
variant, consistently take the memoised host path; pure staging regressions
cover both layouts. Final type/Rays/renderer/native-viewport/scene-execution/
GPU/gate checks pass in **11.62 s**; the viewport action is **1.42 s** and
SPIR-V validation **1.02 s**. Repeated affected tests stay cached.

The fallback audit found `Surface.host` silently turning conversion errors
into empty point meshes. The following checkpoint closes that error path;
valid Maze/Coral images alone did not prove it.

### S9 deferred conversion diagnostics (2026-11-01)

`Surface.scene3_result` now returns a deferred host conversion failure with
the original RDK cause. The existing `scene3` convenience raises the typed
`Conversion_error`; later lazy host-copy requests use the same error. No
failed conversion fabricates an empty point mesh, and invalid device metadata
also remains a result error. The editor catches this conversion error and
keeps its cause alongside the existing cached drawing, so unchanged frames
neither retry the conversion nor lose the message. A corrected prepared
object replaces that entry and clears its error.

The regression exposed an older diagnostic cache coupling: cached scene
resolution reused the global combined context error, which could preserve an
old draw failure after correction. The existing resolution cache now keeps
its own optional cause, separate from per-drawing/texture/image errors. No
additional cache or dependency was added. The RDK test compares a deferred
bow-tie's result error with its owned geometry's exact cause; the editor test
checks visible error retention across cached/recomposed frames, one draw call,
separate scene-resolution diagnostics, and clearing after a real size edit.
Focused type/RDK/editor/native-viewport/gate checks pass in **3.55 s** (editor
scene-tree action **1.60 s**). A stale private-module native alias required
recompilation during this checkpoint; no cache clean or forced build was used.

### S9 native exit regressions (2026-11-01)

The full native gate completed in **105.78 s** with three failures: particle
export parity, two-root dusk/noon brightness, and GPU image resize/CPU fallback/
GPU return. These were real exit failures, despite the new resident mesh checks
passing. The image transition exposed stale dimensions in `Rays.Image`:
`get_size` read the live resource, while the internal drawing helper read its
old wrapper fields. Drawing now reads the live size, retaining the old fields
only for an already-destroyed resource. The full image transition test now has
zero differing channels, including the formerly failing 65-by-17 return from
65-by-3 CPU storage. Rays plus native image checks pass in **6.27 s**.

The particle OCaml reference truncated fractional positions through
`Scene.point`; its first correction still used the non-antialiased rectangle.
It now uses the existing `Ui_batch.Builder.shape_rect` used by Lisp point sets,
with the independently computed particle coordinates. All four frames of the
OCaml, Lisp A/B and one/eight-domain CPU exports match byte for byte (**8.75 s**).
No tolerance or assertion was weakened. Two-root lighting passes after the
image fix without a lighting change, both on a diagnostic screenshot run
(**10.80 s**) and with its original screenshot path restored (**10.17 s**,
action **7.52 s**).

The full native rerun finished in **78.62 s** with those three failures cleared,
but the native Shattered Cube VIEW test expected internal operation `box` for
the migrated Lisp card, whose operation is `flow.card`. It now compares the
semantic `Node.label` on both the displayed node and actual drawing callback.
The camera, selected result, changed geometry after parameter edit and return
to the scene assertions remain intact. Its focused check passes in **19.81 s**;
its fixture/runtime remains part of the test-time audit. A final combined
renderer/Rays/full-native check subsequently passed in **23.21 s** (all aliases exit zero;
unchanged successful native actions reused Dune's cache). The remaining S9
work is the explicit picking/inspection consumer audit; native raster success
alone does not close that requirement.

### S9 consumer audit (2026-11-01)

The native frozen-workspace check now calls the actual functions used by
`Pick` and `Spreadsheet` before the tracer can materialise their inputs. Maze
starts with `Geometry_view.surface` and `cast_material`: a ray from just above
the first quad hits it at the expected distance, after **216,004 payload bytes**
for positions/topology. Coral starts with all four sheet owners and first,
middle and last cells, including Cd, after **1,708,704 payload bytes** for its
position/attribute streams; topology remains resident. Repeating either reader
adds **zero bytes**. The following tracer preparation still materialises the
remaining streams, and the previously bound device mesh retains exact pixels.
Summary and row counts read no payload. These are the UI's shared readers,
not substitutes implemented in the qualification test.

| Consumer | Actual path and transfer behaviour |
| --- | --- |
| Raster | Surface device mesh; borrowed P/N/Cd and generated triangle indices, zero payload/size-dependent upload |
| Wire and vertices | `Mesh.View` memoised host copy; materialising, later modes reuse it |
| Picking | `Pick.surface`/`cast_material` alias `Geometry_view`; CPU surface index reads P/topology once |
| Inspection | `Spreadsheet.columns`/`count` alias `Geometry_view`; metadata nonforcing, visible cells read host streams once |
| Framing/shadow bounds | `Float3.bounds` small category reduction; no geometry payload; exact host bounds |
| Sun shadow | Same indexed buffer as raster; native/host pixels exact |
| Path tracer | Device-indexed mesh uses its host thunk; counted materialisation, no resident-tracer claim |

Unsupported Surface layouts now retain `Surface.host_reason`, covering CPU
positions, invalid stream layouts, topology and unsupported attributes. Later
native borrow refusals remain on `Workspace_gpu.display_refusal`. A resident
point cloud with UV exercises the host reason. This also exposed an older
host conversion defect: an empty primitive Cd was promoted to empty corner Cd
and indexed as though it contained point colours, while empty corner N could
reach an assertion. The shared converter now ignores corner attributes when
there are no corners and does not spread primitive colours when there are no
primitives. The regression keeps valid point UV, empty primitive Cd and empty
corner N on one point cloud. Focused RDK/native-viewport/gate checks pass in
**2.91 s**. The full RDK/renderer/Rays/native exit subsequently passes in
**82.59 s**, with no failures or timeout; raw output is
[s9-native-exit.log](benchmarks/isogrid/s9-native-exit.log). This closes S9's
native and consumer acceptance. It does not close S8's remaining scale census,
S10's production measured placement or S11's full acceptance audit.

The VIEW regression's original 50-cutter fixture exceeded the section budget.
Its in-memory copy now uses eight cutter points, leaving the source sketch
unchanged and retaining camera, selection, parameter edit and changed-geometry
checks. The action drops from **16.10 s** to **5.36 s**; combined VIEW/viewport
checks pass in **8.33 s**.

### S10 completed calibration and shared comparison (2026-11-01)

The existing workspace calibration timed fast dispatch submission even though
fast dispatches return before GPU completion. Each sample now retains the
output planes, settles completion and kernel status, and releases them without
materialising payload, including failed status validation. The same bounded
measurement cache remains. `test_placement_native` uses ordinary Measured
policy: a 32,768-element scalar kernel measures **0.000227 s** in the combined
check (**0.000195–0.000302 s** across the focused runs), reads **16 status
bytes** for one warm-up plus three samples, and reads **zero payload**. A
cached query adds no work. An overflowing kernel yields no estimate, caches
that refusal, reads no payload and returns all native handles on close. The
focused native check passes in **1.44 s**. These are completed calibration wall
times, not production presentation benchmarks; compilation and input assembly
remain outside that stopwatch and must be accounted for by the chain model.

`Cost.choose` compares total supplied CPU/device costs, declines missing,
negative/nonfinite evidence and keeps CPU on ties. Its named reasons are
`unsupported_form`, `missing_consumer`, `validation_read`, `cpu_consumer`,
`no_cost_evidence`, `cpu_measured_cheaper`, `device_measured_cheaper` and
`held_on_device`. The existing display/readback path now calls this comparison
with its existing measured sink/transfer estimates, instead of duplicating
the inequality, and removes its 1,024-element placement floor. Qualification
still exercises legal kernels without a measured choice. The fixed SOP
`Cost.sop_device` rule and Qualification-only resident mesh guards are **not**
claimed replaced yet: they require the known-consumer model, followed by the
unforced original 64³ production measurement and lifetime exit.

`Profile.placements` exposes at most 512 recent decisions, one per authored
owner, with optional cardinality, total costs and CPU-consumer count. An
unknown count stays absent. Explicit program profiles and scoped domain-local
profiles use the same recorder. Regression checks cover invalid evidence,
ties, resident input, CPU consumers, actual display selection and named
refusals, plus 520 distinct real programs proving the history bound. Type/IR/
workspace-service/native-placement/frozen-viewport/dependency checks pass in
**6.05 s** on the final run (unchanged native actions cached); the IR action
is **4.71 s**. The first broad typecheck hit stale private-module CMIs after
the Flow IR interface change; the normal rerun passed, without cleaning or
forcing Dune.

Native image/mark/index checks pass in **6.00 s**. Removing the Qualification
size floor exposes four additional small SOP capture dispatches; the geometry
capture assertion now checks exactly eight statuses and still zero payload.
The image CPU/GPU authority-transition fixture uses CPU-only `hypot` rather
than relying on a small image staying on CPU. All resize/return pixel checks
remain zero-difference. The frozen sketch hash and whitespace checks pass.

### S10 SOP record comparison and CPU-input calibration (2026-11-01)

The fixed `Cost.sop_device` size/noise rule is deleted. `record_on_device`
uses `Cost.choose` with a known optional CPU-reader count; absence records
`missing_consumer` and declines before preparing inputs. Zero promises a known
resident consumer. With known consumers, each distinct resident input's host
readback is charged to the CPU alternative. Each output's materialisation is
charged once to the device alternative when any CPU reader exists, because
the immutable deferred storage shares that copy among readers. Negative reader
counts refuse at the API boundary. Unknown counts remain absent in profile
entries. The lowerer's separate backend suppression for deferred attribute
data is removed: the shared decision now supplies the explicit refusal.

Control-path regressions exercise a two-reference resident source without
forcing it, credit one input readback, switch a cheaper device write to CPU
when an output reader makes the transfer more expensive, and prove a second
CPU reader adds no second materialisation charge. Qualification still reaches
the legal native route without that measured choice. Unsupported forms and
absent backends have named refusals. A later output-borrow failure now releases
all earlier borrowed outputs immediately before returning the CPU fallback;
the injected second-output rejection checks exactly one immediate release and
the refusal reason. This closes an older partial-output ownership gap.

Calibration also reused one dummy CPU array identity for all samples, allowing
the upload cache to hide the transfer after warm-up. Each sample now uses new
immutable CPU input/table identities before its timer; normal runner packing,
upload, completion and status checks remain timed. Known index-range sources
use the existing index-buffer binding and need no dummy count-sized input
array. Native checks require **524,288 uploaded bytes** over the four samples
of 32,768 scalar CPU floats, and the same amount for CPU integer inputs after
a range calibration; cached queries add no work. Payload readback remains zero.
The latest CPU-input median is **0.000256 s**, the range-body completed median
**0.000254 s**. These remain calibration measurements, not a 64³ presentation
claim, and input assembly and the full consumer chain still need their own
production accounting.

The broad IR/SOP/service/native/gate checkpoint passes in **35.90 s**. Loaded
SOP units/replay are **23.48/22.74 s** and remain in the S6 test-time audit;
this is not a warm-alias budget exit. Final type/IR/service/image/native
placement/frozen-viewport/gate checks pass in **5.85 s** (IR action **4.64 s**),
then **6.66 s** with transfer-aware calibration and **3.12 s** after the final
integer-origin check (unchanged actions cached). Raw final output is
[s10-record-calibration.log](benchmarks/isogrid/s10-record-calibration.log).
The frozen sketch and whitespace checks pass.

Still open in S10: supply actual downstream consumer metadata from production
cooks, account for whole-chain CPU/conversion/sink costs, remove the remaining
Qualification-only resident construction guards through that measured decision,
measure the original 64³ workspace unforced against S5, and complete the retained
mesh/two-viewport/cancellation/200-flip lifetime exit. Known-reader API tests do
not substitute for that production evidence.

### S10 production CPU-consumer plumbing (2026-11-01)

SOP nodes now declare whether selected inputs are CPU-read, forwarded unchanged,
or opaque. Session cooks compute distinct downstream CPU readers through forward
chains and diamonds, preserve enclosing facts in nested cooks, and cache the
latest root census. Unknown uses and ambiguous node identities stay unknown;
they never become an implicit zero-reader proof. Pure checks cover fan-out,
selected inputs, nested scopes, identity ambiguity, negative counts and census
reuse at one/four domains.

The production `flow.capture` wrapper declares its CPU input use. Per-context
specialisation and Lisp body cooks forward the enclosing output budget. Stateless
fast attribute writes choose placement during cook, after consumer facts exist,
rather than materialising in the frame prepass. Stateful CPU-owned folds retain
that prepass. The small authored mesh/attribute test checks an opaque root declines
resident record placement and a capture supplies one reader and selects the
cheaper device. This does not prove all production consumers are classified.

CPU-materialised metadata fields folded into a record still prevent that record
from becoming a packed device candidate. The points constructor exposed this
mixed-record limitation; the consumer plumbing check uses plain mesh input to
exercise an otherwise eligible record. Mixed-origin whole-chain placement,
remaining resident mesh guards, original unforced 64³ measurement and lifetime
exit remain open.

Final owner checks (`@lib/sop/runtest`, `@lib/flow_sop/runtest`, the explicit
consumer check, `@lib/flow_native/runtest` and dependency gate) pass in **3.29 s**
with unchanged actions cached; slowest executed action is the gate **2.71 s**,
consumer-facts action **0.92 s**. The new authored consumer check also passes
at live times zero and one. The earlier Flow SOP run executed its existing
units/replay checks successfully; its combined typecheck encountered the known
stale private-CMI dependency issue and an ordinary rerun passed `@check`.
Native image capture, measured calibration and both frozen viewport recipes pass
after the frame-prepass change. Raw final owner output is
[s10-consumers.log](benchmarks/isogrid/s10-consumers.log). Frozen sketch SHA-256
remains `0fdb9b0ebec8f5fe84227d7c0963dd3ec8c446c6145e145555b4fa4f80744e69`.

### S10 static-prefix fusion correction (2026-11-01)

The points-constructor failure above is now resolved at the shared write fuser.
Its static-prefix caching condition previously guarded only one alternative;
the non-indexed-write alternative bypassed it. The condition now covers every
fusion alternative and also applies when a device backend is present. A static
prefix stays separately cached before a changing write, except for a field
rewritten later in the device chain (the existing indexed-producer optimization).
This keeps CPU metadata out of an otherwise eligible live position record rather
than uploading that metadata merely to make the record fusible.

The same authored consumer test now covers both plain mesh and `sop/points`
inputs at time zero and one, verifies all sourcepoint/sourceindex integers, and
checks the unknown-reader CPU and known-reader measured-device decisions.
Focused consumer check: **2.50 s** including build. Affected Flow SOP/default,
native image, frozen viewport, type and dependency checks: **24.04 s** wall;
slowest actions replay **21.42 s**, units **20.47 s**, native images **7.69 s**.
The units/replay section budget audit remains S6 work; this does not claim it
closed. Native pixel parity remains exact and publication/display payload zero
in both viewport recipes. This resolves that static-prefix instance of mixed
records; it does not establish the full mixed-origin cost model or close S10.

### S10 measured attribute lifetime stress (2026-11-01)

`@lib/rays_editor/native_qualification/test_placement_lifetimes` now exercises
ordinary `Measured` configuration and native calibration, without a test cost
override or Qualification. An explicit zero-CPU-reader output budget represents
the known resident drawing consumer. A CPU-origin 65,536-point carrier receives a
live P write; 200 distinct time generations are dispatched. Two native canvases
hold the preceding and current device meshes together, render the old frame after
the new dispatch, and compare its pixels exactly with its earlier snapshot.
A 32-point CPU-origin counterpart stays on CPU with a recorded CPU cost reason
(the small eager SOP-input path records its one CPU reader).

After generation 16, native buffers stay **19 -> 19**, pipelines **1 -> 1**,
runner count unchanged, held buffers **4**, display registrations at most **6**.
Retained heap grows **74,517 words** through generation 200, below the test's
262,144-word bound; this is bounded stress evidence, not a flat-heap claim or the
original selector-flip acceptance. Publication and rendering read **zero payload
bytes** until explicit CPU inspection. Pre-cancelled cook publishes nothing;
recook succeeds. Re-lowering edited Lisp changes the kernel while the old frame
still draws the same pixels. One generation is read while displayed; two others
are read after display release, concurrently through the shared four-domain pool,
then re-read with no additional payload. Every display unregisters after its two
retirement frames. Workspace close returns native handles to the starting count.

The native lifetime action takes **3.03 s**; combined lifetime/calibration, type
and dependency checks take **4.34 s** through `tools/check.exe`. It is included in
`@qualification-scale`. Raw output:
[s10-lifetimes.log](benchmarks/isogrid/s10-lifetimes.log).
This does not close S10: original 64³ unforced placement, whole-chain costs,
measured resident mesh construction/topology changes and original selector flips
remain open. The stress uses the production attribute path over a CPU carrier,
not the still-Qualification-only resident mesh constructor.

### S10 original 64³ measured probe and editor preparation budget (2026-11-01)

`@tools/isogrid-measured-64` runs the existing editor benchmark's 64³ cold
recipe 1, switch to 3, and return to 1. It uses normal production configuration,
waits for an offscreen presented pixel, and checks each frozen geometry golden
after timing. It now prints each recorded placement's owner/count/reason/costs,
plus cumulative payload/status/validation/count/bounds transfer categories.

The first run selected CPU but the normal-write decision reported
`missing_consumer`. The editor now supplies a conservative one-CPU-reader budget
for its published preparation/probe/framing boundary. These callbacks may read
geometry on CPU; readers share one payload materialisation even when a supported
display instead borrows the buffers. A focused editor-cook regression observes
this budget at the actual cooked root. This is not a zero-reader resident promise.

With that budget, the original normal record's decisions are:

| Recipe | Count | CPU estimate | Device + CPU-reader estimate | Reason |
| --- | ---: | ---: | ---: | --- |
| 1 | 998,592 | 16.500 ms | 49.470 ms | `cpu_consumer` |
| 3 | 836,544 | 13.826 ms | 41.744 ms | `cpu_consumer` |

All three exact binary64 digests pass. Presented times are **1.695 / 1.353 /
0.583 s**, calling-domain allocations **611 / 486 / 568 MB**. They include the
current editor/CPU construction route; these are not the estimated normal-region
times. Flow GPU totals after calibration are **256 uploaded bytes**, **16 status
bytes**, **zero payload/validation/count/bounds bytes**, one pipeline and zero
held outputs. Renderer host-mesh transfers remain a separate boundary.

S5's corresponding recorded presented times were **0.925 / 0.786 / 0.939 s**
with **866 / 668 / 826 MB** calling-domain allocations. The current run does not
prove a device win: its cold and first-switch presentations are slower, and
whole-chain input assembly/conversion still needs accounting. Before supplying
the consumer budget, the current CPU-only probe took **1.082 / 0.966 / 0.588 s**.
The measured record comparison currently assembles inputs before deciding; its
possible duplicate work on a CPU refusal remains an explicit investigation item.
Do not interpret a recorded CPU decision as completion of the still-guarded
resident mesh-construction placement model.

The combined editor-cook/benchmark/type/gate check passes in **7.71 s** (benchmark
**5.98 s**, editor-cook **0.96 s**) after the known stale-private-CMI ordinary
rerun. Final editor-cook/native images/frozen viewport/lifetime/service/gate
checks pass in **6.27 s**, unchanged actions cached; native images **4.68 s**,
viewport **1.44 s**. Frozen sketch and whitespace checks pass. Raw evidence:
[s10-measured64.log](benchmarks/isogrid/s10-measured64.log),
[before budget](benchmarks/isogrid/s10-measured64-before-budget.log),
[owner checks](benchmarks/isogrid/s10-prepared-checks.log).

### S10 CPU refusal before input preparation (2026-11-01)

The record comparison previously called `Packed.Host.prepare` before asking the
cost model. A CPU refusal then returned to the attribute writer's CPU execution,
so candidate input evaluation/conversion was work that the eventual CPU program
could repeat. The consuming writer already knows its required owner count.
`record_on_device ~expected_count` now compares first when no resident-input
resolver is supplied and all uniforms are scalar. A definite CPU refusal returns
without evaluating sources. Missing early evidence permits input preparation;
the device candidate always compares its actual prepared inputs, even when the
count is unchanged. Negative expected counts are rejected.

Attribute writers offer this preflight only when every cooked geometry input has
no deferred releaser. Any possibly deferred input keeps the existing resident
resolver and transfer-aware comparison after preparation; the early CPU rule
cannot discard a resident input's readback advantage. The expected target count
is not a replacement for the ordinary CPU/input/storage count validation.

Focused regressions make source resolution fail if a CPU refusal touches it,
check comparison at both expected and different actual counts, reject a negative
count, and keep the existing resident-input `held_on_device` check with an
expected count supplied. The existing full IR/Flow SOP/consumer, editor-cook,
native image, viewport, measured lifetime, type and dependency actions pass.
The first combined rebuild took **44.47 s** under concurrent compilation/test
load (replay **28.90 s**, units **28.55 s**, IR **10.62 s**) and hit the known
private-CMI issue. The ordinary rerun passes in **3.74 s**, unchanged tests cached;
measured lifetime action **2.74 s**, editor-cook **0.86 s**. No default alias's
section-budget audit is claimed closed by that cached rerun.

A fresh benchmark process after validation (no concurrent build) runs
`_build/default/lib/rays_editor/native_qualification/bench_isogrid.exe --switch 64`
in **4.58 s** wall. Cold/3/return-1 presented times are **1.076 / 0.914 / 0.634 s**;
calling-domain allocations **655 / 504 / 595 MB**. All exact digests pass.
The normal records retain `cpu_consumer`, one CPU reader, **256 calibration upload
bytes**, **16 status bytes**, and zero Flow GPU payload/validation/count/bounds
bytes. Earlier timings included concurrent rebuilds; do not derive a speedup
from them. The no-source-evaluation regression proves the eliminated speculative
work directly. Whole-chain CPU/device/conversion costs and measured mesh guards
remain open, and this is not a device-win claim against S5.
Raw output: [checks](benchmarks/isogrid/s10-preflight-checks.log),
[quiet 64³ run](benchmarks/isogrid/s10-preflight-64.log).

### S8 64³ scale, empty offset and exit audit (2026-11-01)

The existing frozen-workspace native probe now accepts `--resolution N` through
its standard argument parser. It edits the checked in-memory input with the
ordinary `Flow_edit.Set_input_default`; it never rewrites the sketch. The new
`@lib/rays_editor/native_qualification/test_isogrid_resident_64` alias is in
`@qualification-scale`, outside default tests.

At 64³ under the explicitly required S8 Qualification policy, the real workspace
publishes **998,592 points / 249,648 quads** with P/N/Cd/topology still resident.
Before publication: **0 payload**, **36 validation**, **16 status**, **24 count**,
**0 bounds bytes**. Validation is the topology status record; count reads are
six words of compacted-array/cardinality metadata, not mesh payload. Publication
takes **2.066 s**, **1,547,765,728 calling-domain allocated bytes**, one cook
domain. Explicit CPU inspection then reads **41,940,868 payload bytes** and
matches exact topology / P,N,Cd within the existing 1e-6 contract. Repeating
inspection reads nothing. The full parity probe takes **30.85 s** (CPU reference
included), so it stays in scale qualification. Revalidation after the empty-path
change also passes (publication **3.732 s** under concurrent checks).

A warmed empty mesh could leave a single offset word resident while returning
owned empty P/vertex/kind arrays, then read four payload bytes in its host fallback.
The bridge now normalises only empty integer streams through the existing
zero-length native fill, validates all topology streams, and builds the canonical
owned empty mesh after successful validation. No new OGPU/Metal operation or
second carrier is introduced. The 42-case native test now requires **zero**
publication payload for all empty/growth/shrink cases. A separate deliberately
invalid resident offset `[1]` over empty points is rejected as `E_TOPOLOGY` with
**36 validation bytes / zero payload**; canonicalisation cannot hide malformed
input. CPU-owned empty topology may still need no GPU validation at all.

S8 requirement audit:

| Requirement | Authoritative evidence |
| --- | --- |
| Shared structured executor and typed backend-neutral scope | `Core_exec.execute ?resident`, native/service tests and dependency gate; no second executor |
| Real workspace zero-payload publication and retained N/Cd | Native frozen 24³ and new 64³ probes |
| Exact topology / fast float parity and repeated read once | Both full-workspace probes, independent CPU reference and counters |
| Every malformed topology rule, no publication | Current native `test_mesh_resident`, including invalid empty resident offset |
| Empty, partial workgroups, growth/shrink, counted-capacity replay | Direct/host 8,196-index compaction fixture, two actual replays each; zero publication payload |
| Cancel during dispatch, then recook | Native direct/host cancel cases; no cancelled cache entry and successful recovery |
| Integer topology beyond 2^24 | Current `test_uint32`, exact uint32 maximum, mixed/empty outputs and partial tails |
| Representative downstream CPU SOP and concurrent readers | Native Clean case, retained old/new arrays, eight readers over four domains, no repeated payload |
| Boundary/capability invariant | Dependency gate: 59 libraries, 36 rules, 29 whitelists; existing native operations only |

The final default Flow SOP/native mesh/uint32/64³/census/type/gate check passes
in **20.62 s**, unchanged native 64³ action cached. Slowest executed actions:
replay **18.57 s**, units **18.37 s**, manifest/catalog **4.89 s**. The targeted
empty/native/census/type/gate checkpoint passes in **5.42 s**. S8 is closed by
these checks; CPU deferral's remaining S6 profile/test-time audit and S10's
production whole-chain placement/lifetimes remain distinct open work.
Raw evidence: [64³](benchmarks/isogrid/s8-resident-64.log),
[empty/native](benchmarks/isogrid/s8-empty-resident.log),
[exit](benchmarks/isogrid/s8-exit-audit.log).

### S11 live authored-head census repair (2026-11-01)

The live census gate had begun counting typed `fn` parameter names and their
array/record types as call heads. It now excludes those declarations exactly as
it already excluded top-level definition parameters, while traversing defaults
and function bodies. Its self-check includes a record/array-typed parameter and
an executable default. Actual IsoGrid primitive/helper call heads now each have
one documented rule in `core.md`, based on their real checked lowering. The
existing deliberately unknown `future/operator` still fails the gate. The live
Lisp CRAM corpus and tool self-checks pass; no pinned census snapshot or exemption
was added. This closes that S11 gate defect, not S11 overall.

Final census/tool/dependency recheck after reviewing the documented helper
bodies passes in **7.11 s**. [Raw output](benchmarks/isogrid/s11-census.log).

### S6 materialisation and test-budget audit (2026-11-01)

The CPU-only strict sculpture-record probe now counts all packed executions,
including nested source work. At both 24³ and 64³, all **five** declared numeric
regions execute **exactly once** at one/eight domains, with no reference
execution. Every output field remains byte-identical to the reference result;
P/N/Cd still share the compacted face count origin, distinct from another
compilation's origin. The 24³ focused probe takes **3.74 s**; the 64³ probe
**30.79 s** including its full reference, and lives in scale qualification.
These checks close the previously unverified duplicate-source materialisation
question; they do not claim zero allocation.

Isolated unit and golden runners measured **15.20 / 15.83 s**, below the
30-second focused-alias budget but too coarse to identify their slow sections.
The same 27 unit modules now run as two independent executables, with no fixture
or assertion removed. Their isolated warm launchers take **8.95 / 6.88 s**.
The combined split runner takes **11.25 s** (core **10.46 s**, geometry **8.12 s**
under mutual contention); the isolated figures establish the warm action budget.
`@lib/flow_sop/test_units` still checks both groups; the `_core` and `_geometry`
aliases permit focused runs. Golden replay also has an explicit `replay` alias
and a diagnostic `replay_profile` command using its existing per-phase timers.

Per-row profiling exposed a real defect: `sweep/small` cooks for **14.615 s**
with only four backbone and five cross-section points. Sampling identifies
`Packed.Host.bind_arguments` recursively rebuilding shared residual captures
through every incoming edge. It now memoises each physical residual for the
current bind, preserving the existing binding/validation semantics, and discards
that memo at the bind's end. Rebound views with equal IDs remain distinct.
The regression requires shared root/source/stage copies to be physically shared
and retains its exact numerical check. Sweep now cooks in **0.234 s**, with the
same golden; all **128** default rows pass. The slowest profiled row is now
`attribute_composite/six mixed`, **3.326 s** total (mostly lowering). These are
per-row one-domain measurements; the batches ran under different background
loads, so no whole-suite speedup is inferred from their wall times.

A separate no-backend guard now avoids forcing the lazy device record candidate
at all when no backend exists. It still records `no_cost_evidence`, with the
available owner/target count. A trace regression proves that no candidate
lowering occurs. This avoids speculative compiler work; it is not the cause of
the measured Sweep improvement.

The full audit exposed a deterministic worker-test deadlock after the new editor
CPU-consumer budget enabled owner-served cost queries. The test owner waited on
its prepare barrier while the worker waited for that owner. The test now uses
the service's existing pump/relax loop until the guarded entered/finished flag,
then retains the same prepare barrier and three complete editor-frame assertions.
The **180 s** timeout was a defect, not raised or waived; the corrected focused
check takes **3.32 s**, with no sleeps or clock-based synchronization added.

Native lifetime qualification at 65,536 points was near the measured crossover
and legitimately selected CPU under the combined build load; its isolated run
still passed. The stress fixture now uses **262,144 points**, retaining the
ordinary measured comparison, explicit drawing-consumer budget and 32-point CPU
counterexample. It does more lifetime work rather than forcing a device policy.
A refusal prints the actual measured reason/costs. The combined-load native
200-generation stress passes in **9.55 s**, with 19 buffers (**+0 after warmup**),
one pipeline, four held buffers, bounded registrations and **74,566 retained
heap words** added (within its existing bound). All old pixels, zero pre-inspection
payload, cancellation/source-edit/read-release assertions remain intact.

The final IR/default SOP/CPU/probe/64³ deferred-corpus/worker/native uint32/native
viewport/lifetime/CLI/type/dependency checkpoint passes in **17.35 s**, unchanged
actions cached. The three freshly executed 64³ corpus shards take **14.33–15.57 s**
under the combined load; worker responsiveness **7.26 s** and native lifetime
**9.55 s**. Earlier failed incremental attempts hit the known private-CMI issue;
no clean, force, timeout increase, corpus reduction or golden update was used.
The final windowed smoke-all check is recorded separately below.

Raw evidence: [24³](benchmarks/isogrid/s6-materialisation-24.log),
[64³](benchmarks/isogrid/s6-materialisation-64.log),
[replay before](benchmarks/isogrid/s6-replay-before.log),
[replay after](benchmarks/isogrid/s6-replay-after.log),
[core](benchmarks/isogrid/s6-core-isolated.log),
[geometry](benchmarks/isogrid/s6-geometry-isolated.log),
[worker](benchmarks/isogrid/s6-worker-pump.log),
[checkpoint](benchmarks/isogrid/s6-final-audit.log).

S6's final `@smoke-all` passes in **115.56 s** process wall, sequential bounded
native windows. The frozen sketch SHA remains unchanged and whitespace checks
pass. [Raw smoke output](benchmarks/isogrid/s6-smoke-final.log).

S6 exit audit:

| Requirement | Authoritative evidence |
| --- | --- |
| Strict sculpture record, compact/count/range/int, shared N/Cd origins | CPU-only 24³/64³ probe, all five numeric regions once, distinct compilation namespaces |
| No new executor and no GPU execution in the CPU deferral check | Shared `Core_exec` and `Packed` over `ogpu_mock`; explicit CPU/no-device regression and dependency gate |
| Identical P/topology/N/Cd and every recipe | Frozen eager/deferred 12³/24³/64³ corpora, CPU/full/empty checks, one/eight-domain field bytes |
| Actual faces/positions/body probes | Current `@tools/isogrid-cpu`, plus earlier S6 probe coverage; eager values unchanged |
| Malformed cook/CLI diagnostics, no publication | Focused CPU/deferred tests and live Lisp CRAM errors, including wrapped residual geometry roots |
| Editor remains responsive while worker holds the unpublished 64³ cook | Condition-controlled prepare barrier, three full frames, worker-domain assertion; owner queries pumped |
| Smoke/golden gates | Current default SOP/CLI/corpus checkpoint and final smoke-all |
| Materialisation/profile and test-budget follow-up | Five once-only regions at both scales; shared-capture root fix, unchanged Sweep golden, replay row profile and isolated split-unit timings |

S6 is closed. Its purpose was CPU deferral, not a device performance win. S10's
whole-chain cost model, measured resident mesh guards and original-workspace
selector/lifetime exit still remain, followed by S11's full shipping audit.

### S10 prepared-input calibration (2026-11-01)

`Gpu.backend.cost` accepts optional prepared inputs. Both record and display
comparisons recheck the actual prepared count, scalar uniforms, frame, lookup
tables and resident bindings. Shape-only estimates cannot reject table-dependent
programs before those tables exist. Missing early evidence permits preparation;
missing actual evidence still keeps CPU execution.

Native measurement retains resident bindings and gives CPU arrays fresh upload
identities for each of the warm-up plus three measured samples. Its bounded
64-entry cache distinguishes shape estimates from prepared measurements, exact
prepared counts, source/table sizes and CPU/device origins. Prepared failures
are not cached: a later frame with the same layout may succeed. Cache hits do
not allocate count-sized placeholder arrays or dispatch work.

Native assertions prove 1,024/2,048-entry tables upload 16,384/32,768 bytes across
four samples. A two-source map uploads 1,048,576 bytes with CPU/CPU sources and
524,288 with resident/CPU sources. All calibration payload readback deltas are
zero; cached queries add no work, and explicitly inspecting the resident source
afterwards confirms its contents are unchanged. An overflowing frame fails
measurement, a valid frame with the same layout succeeds, and that success is
cached. IR checks cover a backend that can price only prepared inputs.

The 200-generation Measured lifetime fixture now performs sixteen nested sines
per point. The prior cheap CPU-origin map could legitimately choose CPU after
real uploads were charged; increasing count alone did not establish a reliable
device advantage. The new workload retains the small CPU counterexample and
all two-canvas, cancellation, edit, release-order and immutable-generation checks.
It uses neither Qualification nor a cost override. After warm-up it creates no
additional buffers or pipelines and reads no payload before explicit inspection.

The editor image test now asserts the actual warmed-frame work: two GPU image
kernels, with the small captured SOP geometry evaluated using its creation-time
Measured policy. Its previous absolute status count conflated calibration with
dispatch and incorrectly described those SOP captures as GPU work. It reports
12 total status reads and zero payload bytes; warmed time 1 accounts for two
reads. This is mixed CPU-geometry/GPU-image evidence, not resident-SOP evidence.

The focused IR/native/SOP/consumer/image/lifetime/viewport/type/dependency set
passes in **9.54 s**. Slowest native actions are images **7.54 s** and placement
**6.94 s** under combined load. Evidence:
[prepared-input calibration](benchmarks/isogrid/s10-prepared-input-calibration.log).
S10 remains open: whole-chain conversion/cost accounting, measured mesh guards,
and the original-workspace selector/lifetime exit are not proved by these checks.
The final interface-documentation rebuild reproduced S11's known incremental
private-CMI assumption defect; an ordinary `@check` rerun passes without cleaning.

### S10 static and display readback comparison (2026-11-01)

The legacy static approximable-map path now uses `Cost.choose` and records its
reason through the active profile, with one CPU consumer. It validates backend
prices before adding transfers, permits missing shape evidence to reach actual
prepared inputs, and rejects missing actual evidence. Its separate 1,024-element
cutoff and duplicate inequality are deleted. This cutoff was redundant: the
evaluator offers this static path only after its 4,096-element head. Tests use
4,097 elements to reach the real callback rather than claiming below-cutoff
static execution.

Static, display/readback and record comparisons now reuse output readback
accounting for every output and lane. Display/readback previously charged one
lane even for vec4, which could select a device after underpricing its transfer.
A 64-element vec4 regression gives the backend a price between scalar and vec4
break-even points and proves CPU selection with the full four-lane cost.
Static callback checks cover prepared-only evidence, invalid negative prices,
missing actual evidence, an expensive GPU price, and the same scalar/vec4
counterexample. They verify callback selection and reference results using a CPU
readback callback; they do not claim native shader or timing evidence.

The focused IR run passes in **5.60 s**. The SOP/native-image/200-generation
consumer set passes in **21.00 s** combined; final IR/native/consumer/type/gate
checkpoint passes in **3.14 s**, mostly cached (slowest uncached editor action
**2.29 s**). Logs:
[IR](benchmarks/isogrid/s10-static-placement-ir.log),
[consumers](benchmarks/isogrid/s10-static-placement-consumers.log),
[checkpoint](benchmarks/isogrid/s10-static-placement-final.log).
This removes a redundant selector and fixes transfer undercounting. It does not
price Core loops, fills, host calls and mesh validation as a complete chain;
the mesh construction/selection Qualification guards remain S10 work.

### S11 incremental private-CMI ordering (2026-11-01)

The repeated first-build inconsistent-Flow_ir-assumption failure was a missing
dependency edge, not stale cache contents. Inspection of `dune rules` for the
scene-tree test shows an external-library dependency on Rays Editor's
`public_cmi` group, while the test also reaches private `Rays_editor__.Loop_model`
and sibling aliases. Those private CMIs could still contain the preceding
Flow_ir interface when the test compiler started. Private module access from
outside its owning library is outside Dune's documented public dependency
contract ([library reference](https://dune.readthedocs.io/en/latest/reference/dune/library.html)).

`rays_editor.mli` now explicitly opens the final private `Editor_loop`
coordinator, forcing its private dependency chain to finish before Dune publishes
the public interface that tests wait for. It adds no exported declaration or
dependency on another library. The deliberate build dependency has a
`ponytail:` comment: remove it when these tests use only the public boundary.
The generated interface rule now depends on `rays_editor__Editor_loop.cmi`,
`rays_editor__Loop_model.cmi` and their private prerequisites.

To exercise the actual incremental failure trigger, change the public
`Cost.choose` doc comment in `flow_ir.mli`, then run the following exactly once:

```sh
_build/default/tools/check.exe @check \
  @lib/rays_editor/test/test_isogrid_deferred \
  @lib/rays_editor/test/test_scene_tree @test/dependency_gate
```

The forward edit passes first time in **18.27 s**; restoring the original
interface comment and running the same command once passes in **18.18 s**.
Neither run cleans, forces, retries or prebuilds the editor separately. The
second trace's slowest test actions are deferred cook **4.65 s** and scene-tree
**2.24 s**. Logs:
[forward interface edit](benchmarks/isogrid/s11-private-cmi-forward.log),
[reverse interface edit](benchmarks/isogrid/s11-private-cmi-reverse.log).
A separate OCaml client referencing `Editor.close` and `Editor.Private.status_text`
also compiles with only Rays Editor's `public_cmi` directory on its include path;
clients need no private editor CMI to use those public functions.

This closes the recorded incremental private-CMI defect. It does not close S11's
shipping/scale/native gates or S10's whole-chain placement and lifetime audit.

### S10 original selector lifetime gate and idle scratch (2026-11-01)

`@tools/isogrid-selector-lifetimes`, included in `@qualification-scale`, runs
the existing editor benchmark over the frozen workspace at 24³: one complete
eight-recipe warmup, then 200 measured selector edits. Every geometry digest is
checked. Its existing strict retained-heap and cache equality checks are retained;
the snapshots now also compare display registrations, Flow buffer creations and
all live OGPU handles. Every edit checks the 128-registration ceiling, and closing
the workspace/canvas must restore the initial native handle count. An allocation
summary reports all 200 edits even when Dune abbreviates the per-edit output.

The first current-code run failed: retained workload grew from **21,207,248** to
**21,598,824 words**, while every recorded cache count stayed flat. Root inspection
found unchanged editor, cook and GPU-owner roots. The existing sampled allocation
profiler located the growth in `Packed.take_registers` on pool workers: strong
domain-local idle scratch kept the largest past kernels according to scheduling.
This was workload retention, not scheduler metadata, and was not subtracted or
given a tolerance. Evidence:
[failure](benchmarks/isogrid/s10-selector-before.log),
[roots](benchmarks/isogrid/s10-selector-roots.log),
[allocation profile](benchmarks/isogrid/s10-selector-retainers.log).

The same four-bank domain-local scratch cache now holds its idle list weakly.
Borrowed banks remain strongly owned through execution and exception cleanup;
GC may reclaim idle banks between chunks. No shared lock, new pool or dependency
is added. Numerical one/eight-domain, ordered-error and SOP checks remain green.

The final ordinary Measured run passes in **27.69 s** including rebuild:

| After warmup / 200 edits | Warm | Final |
| --- | ---: | ---: |
| Retained workload words, exact scheduler/timer accounting | 17,693,776 | 17,692,211 |
| Session entries / payload bytes | 16 / 21,715,380 | 16 / 21,715,380 |
| Preparations / budgeted bytes | 8 / 65,146,140 | 8 / 65,146,140 |
| Flow pipelines / renderer pipelines | 1 / 96 | 1 / 96 |
| Flow buffer creations / held buffers | 59 / 0 | 59 / 0 |
| Display registrations / live OGPU handles | 0 / 357 | 0 / 357 |

Workspace close returns handles to **0**, the initial baseline. Mean presented
latency before/after is **80.854 / 81.151 ms**; evaluate mean **26.477 / 26.501 ms**.
Final presented p95/max is **148.386 / 199.924 ms**; evaluate p95/max
**93.089 / 100.758 ms**, below the unchanged 150-ms gate. All 200 edits allocate
**165,831,187 bytes per edit on average** on the calling domain. No allocation-free
claim is made. Measured maps select CPU with one recorded CPU consumer; warmed
cached recipes add no Flow dispatches. Flow payload readback remains zero.

The IR/SOP/native-viewport/type/dependency checkpoint passes in **26.14 s** under
combined load. Its slowest executable actions are uniforms **15.06 s**, core SOP
units **14.13 s**, geometry units **12.63 s**, core regions **10.77 s** and IR
**9.66 s**; these aggregate actions contain multiple sections, and this combined
run is not an isolated per-section budget audit. Final selector action **26.27 s**.
Logs: [selector](benchmarks/isogrid/s10-selector-final.log),
[numerical/consumer checks](benchmarks/isogrid/s10-scratch-validation.log).
The sketch's frozen SHA remains unchanged. This proves the ordinary original
24³ selector/close gate; it does not prove measured resident mesh construction
or replace S10's remaining whole-chain and original resident-lifetime work.

### S10 complete-cook measurement before whole-chain selection (2026-11-01)

`bench_isogrid_resident` now also accepts `--cpu`, `--domains N` and `--repeat N`.
It measures actual uncached SOP cooks on one host, separating lowering/publication
from cook time. Repeated cooks disable Session result caching and relinquish the
previous result before the next dispatch; they are cost measurements, not the
overlapping-generation lifetime test. Existing full-field parity remains the
default. CPU mode asserts owned CPU streams and no Flow GPU work; resident mode
asserts resident P/N/Cd/topology, zero payload reads and one 36-byte topology
validation per cook. The original frozen source remains untouched.

The resident backend's scope callbacks are timed and counted separately: region
invocation, fills, ranges, compaction, coercion, counts, materialisation, settle,
close, retaining integer/float streams and topology validation. These are scope
callback timings, not GPU timestamp durations or a complete dispatch census.
Ordinary backend kernels outside that scope (including attribute work), Core
control/host work and conversion remain included in complete cook wall time.

`@lib/rays_editor/native_qualification/bench_isogrid_cost`, also in
`@qualification-scale`, runs the frozen 64³ workspace in two fresh processes:
CPU and Qualification residency, each with eight domains and three uncached
cooks. Both produce **998,592 points / 249,648 quads**. Measurements on this M1:

| Cook | CPU seconds / calling-domain allocated bytes | Resident seconds / calling-domain allocated bytes |
| --- | ---: | ---: |
| First | 0.515403 / 391,772,584 | 0.369571 / 288,033,232 |
| Repeat 1 | 0.481848 / 392,101,792 | 0.341443 / 186,693,048 |
| Repeat 2 | 0.461159 / 386,453,576 | 0.345094 / 148,417,696 |

These are two repeat samples, not a claim that every cache has reached steady
state. Worker-domain allocations are not included. Resident totals across three
cooks: **7,040,632 uploaded bytes**, **zero payload readback**, **108 validation**,
**48 status**, **72 count** and **zero bounds bytes**. CPU Flow transfer totals
are zero. Callback counts are nine invokes, three fills, three ranges, three
compactions, fifteen counts, six finish queries, three settles/closes, nine
integer retains, three float retains and three topology validations. Their
reported time totals about **0.070 s** across **1.056 s** of resident cooks; a
sum of these callback prices alone cannot explain or price the complete chain.
The remaining time includes uninstrumented backend work as well as CPU work;
it must not all be labelled CPU control time.

A separate one-domain cold comparison takes CPU **1.896788 s** and resident
**1.363035 s**, demonstrating why a single-domain baseline must not stand in for
the editor's eight-domain alternative. These are cook measurements without a
viewport, display conversion, pixel presentation or a CPU-reader penalty. They
do not prove the plan's required time-to-present win and do not authorize a
production device selection by themselves.

The paired eight-domain benchmark passes in **6.00 s** including rebuild.
The existing default 24³ full-field resident/reference check plus type/dependency
checks passes in **5.22 s** (native parity action **2.04 s**): exact topology,
P/N/Cd within 1e-6, first inspection **2,287,492 bytes**, repeat inspection zero.
Evidence: [one domain](benchmarks/isogrid/s10-chain-cost-single-domain.log),
[eight-domain repeats](benchmarks/isogrid/s10-chain-cost-warm.log),
[parity/type/gate](benchmarks/isogrid/s10-chain-cost-validation.log).
Production policy is unchanged. S10 still requires a whole-chain decision that
accounts for these missing boundaries and removes the Qualification-only mesh
guards, followed by its original resident-workspace lifetime exit.

### S10 native cook-to-pixel comparison (2026-11-01)

The same probe's `--present` mode now uses the real workspace GPU owner for
construction and display. Each uncached cook builds `Rdk_rays.Surface.scene3`,
renders the coloured quad mesh to a 1280×900 native canvas and waits for a pixel
read before stopping its timer. Both routes use the same fixed camera and eight
CPU domains. The last complete image must contain non-background pixels. The
resident mesh must expose float3 colours and six device triangle indices per
quad; its presentation must add no Flow payload readback and less than 64 KiB
of renderer uploads. Previous registrations are released before relinquishing
the previous geometry. The ordinary full-field reference mode still passes.

`@lib/rays_editor/native_qualification/bench_isogrid_present`, included in
`@qualification-scale`, runs two fresh processes and four uncached cooks per
route. An isolated run passes in **6.66 s** including launcher overhead:

| Iteration | CPU cook-to-pixel seconds | Qualification resident cook-to-pixel seconds |
| --- | ---: | ---: |
| First | 0.787376 | 0.431499 |
| Repeat 1 | 0.679655 | 0.336823 |
| Repeat 2 | 0.678628 | 0.324572 |
| Repeat 3 | 0.670788 | 0.324398 |

CPU renderer uploads are **41,946,320 bytes** first, then **41,940,864 bytes** per
repeat. Resident renderer uploads are **5,456 bytes** first, then **zero**.
Resident presentation adds **four validation bytes** per mesh and no Flow payload
or bounds readback. Across four resident cooks/presentations, totals are
**160 validation / 64 status / 96 count / zero payload / zero bounds bytes**.
CPU Flow transfer totals remain zero; its renderer uploads are reported
separately, not hidden in that zero.

Calling-domain allocations through pixel completion are CPU
**769,083,432 / 780,710,000 / 758,620,064 / 778,171,032 bytes**, resident
**303,815,984 / 166,844,976 / 158,168,064 / 186,125,088 bytes**. These include
cook, surface/display conversion, rendering and pixel synchronization, but not
worker allocations or the final image-content assertion after timing. Repeat 3
spends **0.455361 s** in CPU cooking and **0.215427 s** afterwards, versus
resident **0.319539 s** cooking and **0.004859 s** afterwards. Omitting display
conversion would omit a material part of this CPU alternative.

The native parity/type/dependency checkpoint passes in **7.68 s** while the
presentation benchmark runs concurrently. Because that contention changes CPU
timings, the table above comes from the later isolated run. The default 24³
parity remains exact for topology and within 1e-6 for P/N/Cd; its intentional
first inspection reads **2,287,492 bytes**, its repeated inspection zero.
Evidence: [isolated presentation](benchmarks/isogrid/s10-present-isolated.log),
[parity/type/dependency checkpoint](benchmarks/isogrid/s10-present-validation.log).

This is a geometry cook-to-native-pixel comparison, including display conversion.
It excludes workspace checking/lowering, editor chrome, async scheduling and
interactive edits, so it is not the full editor switch-latency exit. Residency
is still explicitly Qualification in this measurement. It establishes a
reproducible candidate advantage for pricing; production selection and S10's
remaining full-workspace lifetime/placement audit are still open. The frozen
sketch SHA is unchanged.
