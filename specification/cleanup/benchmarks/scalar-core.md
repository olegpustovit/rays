# Scalar Core execution after removing `scalar_tree`

Command: `_build/default/tools/check.exe tools/bench_core.exe`, then
`_build/default/tools/bench_core.exe`. The benchmark checks bit-identical
results against the reference walker, then reports the median of three samples
of 200,000 evaluations, one domain, for
`(+ (* (sin t) 0.5) (* t t))` at `t = 0.37`. Allocations are bytes per
evaluation. Host: Darwin 27.0 arm64, Apple M1.

Latest checkpoint: scope-local frame sharing lowers current Executor allocation
to 360 B/evaluation. Alternating CPU-time pairs measure 91.0/90.7 ns against
63.8/63.6 ns for the frozen baseline: about 43% slower. This improves the
earlier roughly 67% regression below, but the no-regression gate remains open.

The initial before/after comparison in `/tmp/rays-bench-core-s3-before.txt`
was not load-matched and should not be used for the gate. I rebuilt HEAD in an
isolated worktree, then alternated before and after binaries. The old binary
was SHA-256 `54defdbfc6592f3d34c6960e12269527c6220344d3e264a5554d831059ae5686`
(HEAD `625821883f6491b14ef479c596215c43d35cb3ae`; baseline source blobs:
`tools/bench_core.ml` `99dc93d4b53274b2e7e5f5bd086a966b1c678ffc`,
`lib/flow_ir/core_exec.ml` `f78fb75a00bfed25cdf44b01bd450b1bd176bc41`).
The after binary was SHA-256
`b138d6a362ddb2be97d17cfccba3dab3cc8b50e2d3ec28fd84be4a08dd88c603`
(current source blobs: `tools/bench_core.ml`
`b7d73fb87196e3d42e81fd5723e19db9d05ed638`, `lib/flow_ir/flow_ir.ml`
`c30e0c92ecac23b79ccb2b255b5bb013a6aa6a73`,
`lib/flow_ir/core_exec.ml` `ff90d047e823c5c8afe27abf834e0865ec09c7e6`).
Both used OCaml 5.3.0 and Dune 3.24.2 with the same default native build
settings. The current harness adds a direct `Core_exec.fast` row; timing code
for the reference walker and Executor is unchanged.

| Alternating run | Reference walker | Core_exec | Core fast path | Executor |
|---|---:|---:|---:|---:|
| Before A | 1221.2 ns / 6344 B | — | — | 65.5 ns / 584 B |
| After A | 1224.5 ns / 6304 B | 107.5 ns / 392 B | 106.7 ns / 392 B | 110.4 ns / 408 B |
| Before B | 1229.1 ns / 6344 B | — | — | 66.4 ns / 584 B |
| After B | 1216.0 ns / 6304 B | 107.2 ns / 392 B | 106.5 ns / 392 B | 110.7 ns / 408 B |

The paired results show a real roughly 67% CPU time regression for this scalar
case after deleting `scalar_tree`, while reducing allocation by 176 B per
evaluation. The current Executor adds about 4 ns over `Core_exec.fast`, so the
remaining regression is in general Core execution itself. S3's no-regression
gate remains open until that path is improved and remeasured.

A source audit finds seven `E.Float` constructions per force for this body:
three time loads, `sin`, two multiplies and the addition. Prepared statement
closures pass boxed values between steps. This identifies a candidate cost,
not a measured attribution of the entire regression. A new float-only bank
would need retained numeric proofs: `Ty.Float` alone does not establish the
runtime representation because arithmetic preserves Int/Int results. Packed
already has separate register classification and typed banks, so duplicating
that machinery in Core is not an accepted fix. No bank change was made.

### Follow-up with CPU-time accounting

I added `Unix.times` CPU accounting and alternated the frozen baseline binary
with the current engine while port-capture PID 88011 was active. The baseline
binary (`/tmp/rays-scalar-before/_build/default/tools/bench_core.exe`, SHA-256
`1a9b5642fdb087137763579cd71637f9dc0dc3c4bbde94a81a0c8969201b36b1`) used
the same scalar input and compiler flags. The current all-pooled experiment
(`/tmp/rays-scalar-current-pool-all.exe`, SHA-256
`5755585807c704d1e7733875cd2194cedd28271b3b081afe954f619b039b4de5`) used
the current Core engine with the temporary `n > 0` reuse policy. CPU time was
65.7 and 66.6 ns/evaluation for baseline Executor; current `Core_exec.execute`
was 105.3 and 105.8 ns, `Core_exec.fast` was 106.1 and 108.1 ns, and Executor
was 109.9 and 108.8 ns. Allocation was 584 B/evaluation for baseline,
336 B for Core execution, and 352 B for Executor. These capture-loaded runs
support the earlier conclusion that most of the regression is in Core
execution, while the Executor dispatch itself adds little. The capture was
concurrent, so these readings are evidence, not an isolated acceptance run.

I also compared the all-pooled binary against a build restoring the baseline
`n > 256` reuse threshold (SHA-256
`6428681934e2dc9bc9bae4f9b919e10c82a4bf18c6243c7ae011ee775d2dce9d`). Two
alternating pairs gave 132.8 vs. 131.3 and 141.2 vs. 119.4 ns for
`Core_exec`; `Core_exec.fast` gave 131.3/119.1 vs. 119.2/111.3 ns. Allocation
was 336 B with all-pooled and 392 B with the restored threshold. The noisy
timings do not establish a speedup, but the threshold retains the prior
measured policy and avoids pooling tiny arrays, so the source keeps `n > 256`.
No scalar no-regression claim is made; the S3 performance gate remains open.

### Scope-local frame sharing

`Core_lower` now reuses an already-read immutable frame field through its
existing scoped cache. Cache copies retain branch/loop dominance: a read first
emitted inside an untaken branch or empty loop cannot escape that scope. This
removes two repeated time loads in the scalar body, reducing seven `E.Float`
constructions to five without a second executor or a new register bank.

The full focused IR alias passes in 5.99 s; its slowest executed test is
`test_ir` at 3.56 s. The new direct/reference checks cover changing frame facts,
branch-first reads and empty loops. Regenerated JS frame goldens and Node
execution pass in 2.81 s: 92 goldens, 83 programs and 369 frames. Expected
differences remove repeated reads and renumber locals/statements.
The complete focused Web alias also passes in 11.37 s; its slowest executed
action is the existing grade/index check at 9.88 s.

Two new alternating pairs, with other agents holding new validation and the
frozen reference capture still running, give:

| CPU-time run | Reference walker | Core_exec | Core fast path | Executor |
|---|---:|---:|---:|---:|
| Before A | 1176.6 ns / 6344 B | — | — | 63.8 ns / 584 B |
| After A | 1175.2 ns / 6304 B | 87.7 ns / 344 B | 86.5 ns / 344 B | 91.0 ns / 360 B |
| Before B | 1175.6 ns / 6344 B | — | — | 63.6 ns / 584 B |
| After B | 1177.8 ns / 6304 B | 86.7 ns / 344 B | 86.9 ns / 344 B | 90.7 ns / 360 B |

The current frozen binary is SHA-256
`b2e698c8422deb5854ab48890c1606ba534345f6b4bf90628f8dae2cf0181906`;
its source archive is
`00b292cb4f2f42eb1bfe26ef9926f37c03b9c62d0a850cd1e9500e07786cd8ef`.
The baseline binary is unchanged from the CPU-accounting follow-up. Commands,
raw outputs and frozen paths are in [scalar-frame-sharing.txt](scalar-frame-sharing.txt).
The allocation reduction is 224 B versus baseline and 48 B versus the previous
current engine. These capture-loaded pairs still show a substantial regression,
so neither S3 nor FL2 is accepted. A direct-destination closure experiment
measured no material gain and was discarded; its code is absent from this build.

### Per-evaluation overhead cuts (P3.S9 pass)

Two costs that every `Core_exec` call paid regardless of the program were removed, both
measured in isolation (`ocamlfind ocamlopt -O3`, loaded host, ns per operation): `Array.make n x`
is a C call (16.3 ns for six slots, 8.4 ns even for zero slots) while a pooled `Atomic.exchange` plus
`Atomic.set` is 7.7 ns.

- `calls` is the empty array when the program has no call slots (the scalar body has none).
- Programs whose locals are all plain scalars (`Float Int Bool Vec2-4 Text`, nothing that can hold a
  large or builder-owned array) now reuse their locals array across calls the way programs over 256
  locals did; the pool holds `[||]` while taken, so no `Some` is allocated per call, and the
  release closure is gone.

Allocation falls from 360 to 304 B per evaluation for the Executor row (344 to about 288 for
`Core_exec.execute`). No second VM and no new register bank were added.

Timing is not recorded: during this pass the host load average was 100-115 on 8 cores (other
agents' builds and replays), and alternating pairs of the frozen baseline, the previous head and
this build gave baseline 88-92 ns, previous 137-139 ns, current 146-167 ns CPU, i.e. the same
order of noise as the effect (the frozen binary itself reads 88-92 where it read 64-66 on an idle
host). The no-regression gate (decision 11) therefore stays open; what remains structural is five
`E.Float` boxes (two allocations each) and one closure call per statement and per primitive, which
only a typed float path in Packed (not a Core-side bank) can remove. Re-run
`tools/bench_core.exe` against the frozen binary on an idle host before accepting S3.
