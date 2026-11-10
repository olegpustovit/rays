# Carried Region loops

2026-10-27, Apple M1 / 16 GB, macOS 27, OCaml 5.3, Dune dev profile.
This is P3.S8 partial evidence, not acceptance of complete language coverage.

## CPU execution

Build through `_build/default/tools/check.exe tools/bench_core.exe`, then run
`_build/default/tools/bench_core.exe loops`. Each fixture has 1,024 rows and
16 counted steps. The tool first requires byte-identical reference and strict
Packed output, then reports the median of three CPU-clock samples and the
allocation in that sample. GC occurs outside each sample. The eight-domain
case selects the shared pool; this small fixture fits one chunk and does not
measure parallel scaling.

| Fixture | Reference CPU | Packed CPU, 1 domain | Reference allocation | Packed allocation |
| --- | ---: | ---: | ---: | ---: |
| Scalar accumulator | 14.008 ms | 0.463 ms | 64,974,560 B | 222,072 B |
| Record accumulator | 18.935 ms | 0.490 ms | 57,224,928 B | 255,024 B |

The reference is the actual walker with compiler interception disabled; this
is a same-program engine comparison, not a frozen before/after revision
comparison. Eight-domain selections measured 0.497/0.486 ms and the same
allocation. Full command time was 1.26 s. The frozen reference capture PID
88011 continued concurrently, so these are loaded-machine CPU measurements.
[Raw output](region-loops-cpu.txt).

## Ordinary maps

The existing `bench_core.exe scratch` body is unchanged: 47 instructions,
Float/Int sources, counts 1/11/65/1,024, one/eight domains, three samples of
100 forces, with exact reference checks. Before/after executables are frozen:

- Before: `/tmp/rays-region-loops-before.8F4yxV/bench_core.exe`, SHA-256
  `2e18418273dbf5822c1d6645f099915a353e1c438f44af84f94a66d4546a90de`.
- After: `/tmp/rays-region-loops-after.bKgmxM/bench_core.exe`, SHA-256
  `5cd37cb7b953cc373c1854582ba94c2df458748077b2cc18d58ae3124fb04bcb`.
- After compiler/emitter/benchmark source archive:
  `/tmp/rays-region-loops-after.bKgmxM/source.tar.gz`, SHA-256
  `9c76a1ef385817704f042a8e5789e3c2a02d968d1f1a22d109cf3468610ad429`.

Run each frozen executable with `scratch`. Final wall times were 5.46 s
before and 4.27 s after, with the capture and concurrent build/replay load.
These noisy times do not establish a speedup or a precise regression bound.
Float allocation is unchanged; Int allocation increases by a fixed 120 B
per force across all four sizes. An earlier implementation added 2,064 B
through control-dispatch closures; the final no-loop path bypasses that
dispatch. At 1,024 elements, final Int allocation is 820,897 B versus
820,777 B. [Before](region-loops-before.txt), [after](region-loops-after.txt).

## Device and web checks

`@lib/flow_gpu/test_run` executes six 4,096-row fixtures: dynamic bounds,
empty/nested folds, simultaneous record/vector/Bool updates, outer guards,
and carried uint32 permutation state using ordinary `int/*` plus masks.
Native Metal results equal strict CPU and walker arrays exactly. Additional
checks reject a selected oversized range and leave an untaken oversized
range unevaluated. Existing native numeric/noise/resident-fold/lifetime checks
also pass.

The same six fixtures are emitted through `Kernel_layout.iter` to fast WGSL.
Generate the table with `test_wgsl.exe --write-fast-table DIR`, then run
`node tools/web/test/gpu_fast_check.mjs DIR lib/web_export/runtime/gpu.js`.
Real Chrome reported zero bad values and zero error for every new fixture;
the existing nine fast fixtures and count/compaction checks also passed in
7.08 s. Ordinary and fast JavaScript region tests cover the same controls,
Text state, and exact bounds above 2^53. Existing source goldens were unchanged.

S8 remains open: more loop forms,
complete helper/capture coverage and SPIR-V loop emission still need work.

## Iterate and early exit

The same controls now lower `iterate` with its uncapped Int count and optional
`:until`. The predicate reads the previous and next state before simultaneous
commit, then exits the actual loop. Negative counts leave both bodies
unevaluated. Captured helpers and checked named definitions inline through the
same compiler. CPU checks include Int wrap, Text-carried records, selected
predicate errors, untaken later errors, and 32,769-row one/eight-domain parity.
Production image qualification also accepts the fast loop kernels.

Ten 4,096-row fixtures, including the new iterate cases, match exactly on real
Metal. The same ten match real Chrome fast WGSL with zero bad values and zero
error; its nine existing fast fixtures and count/compaction checks pass too
(1.93 s). Ordinary/fast JavaScript agrees with native. The coherent focused
IR/Web/GPU/native request takes 8.06 s warm: JS 5.88 s, CPU region parity
3.37 s, native GPU 1.12 s. The separate production qualification check takes
4.55 s, including a 3.68 s test action.

An alternating frozen CPU comparison uses the earlier after binary above and
`/tmp/rays-iterate-after.igGXEG/bench_core.exe` (SHA-256
`92ff992dd3707c7abd1592154702bb3457ff17aa2067e78c2aafba01529f4873`). Its
source archive is `/tmp/rays-iterate-after.igGXEG/source.tar.gz`, SHA-256
`9cce952911223e2da86abe82f7ed443e0ead4a8b1ba94eebd71ecebc86f45409`.
Both binaries use OCaml 5.3.0 on M1 with the live frozen capture continuing.
The two existing fold fixtures retain exactly 222,072/255,024 allocated bytes;
one-domain packed times remain 0.46–0.51 ms. This small loaded comparison does
not establish a precise speedup or regression bound.

The added benchmark carries a record across three actual passes from a
`max_int` count. For 1,024 rows it checks exact walker/packed outputs before
timing: walker 9.746/9.852 ms CPU and 27,439,088 B, strict packed 0.347/0.367 ms
and 370,272 B. It fits one chunk; selecting eight domains is not scaling
evidence. These are engine comparisons, not revision speedups. The four
commands take 0.54/1.45/0.69/0.96 s wall.
[Before A](iterate-before-a.txt), [after A](iterate-after-a.txt),
[before B](iterate-before-b.txt), [after B](iterate-after-b.txt).

Ordinary structured Core now supplies a stop local to `Counted` as well;
native and emitted JavaScript no longer visit the remaining passes after an
iterate predicate succeeds. A shared `max_int` fixture throws on the first
unexpected later body pass, so a regression fails promptly. Its frame golden
and all 92 frame goldens / 83 actual programs / 369 frames pass. The final
focused IR/Web/GPU/native request is 28.20 s. The frozen benchmark above
predates this separate Counted change and measures only Region execution.

Review found a carried-type boundary: an Int seed above 2^53 must stay Int
through its first step even when the loop's joined type is Float. The shared
lowerer now refuses mixed carried kinds, including record fields, and the
existing structured Core path retains exact first-step subtraction. Scalar
iterate, fold and nested-record regressions verify the actual Core result
against the reference and assert no incorrectly accepted region. This case
remains part of S8's unfinished representation work.

JavaScript loop termination also stays exact: Region controls compare their
existing BigInt or fast integer-pair indices directly; uncapped ordinary Core
loops use BigInt ordinal counters. They no longer convert a large loop length
to a Number counter that cannot progress above 2^53. Bounded list loops retain
their Number counter. The updated 92 frame goldens and 369 frames pass, with
the final golden generation taking 2.65 s.

## Loop-aware fusion

Fusion now remaps counted controls in producer and consumer regions, preserving
their lexical scopes and contiguous carried registers. It also merges sibling
loop outputs. Inputs alias the already-evaluated producer value, including
guarded Int and Text leaves. Strict CPU checks cover these cases, an ordered
reduction, a shortened zip's failing producer tail, and 32,769-row one/eight
domain parity. The focused CPU request passes in 3.74 s; its test action is
3.18 s.

`_build/default/tools/bench_core.exe loop-fusion` compares the same program with
fusion disabled/enabled, one domain, 4,096 rows. Each variant must equal the
actual walker byte for byte before timing. It reports the median of three
CPU-clock samples with GC outside each sample. Command wall time is 0.39 s;
the frozen reference capture continues concurrently.

| Loop location | Unfused CPU | Fused CPU | Unfused allocation | Fused allocation |
| --- | ---: | ---: | ---: | ---: |
| Producer, 16 steps | 1.793 ms | 1.759 ms | 407,232 B | 355,112 B |
| Consumer, 16 steps | 1.794 ms | 1.788 ms | 379,656 B | 338,648 B |
| Both, 3 steps each | 0.619 ms | 0.606 ms | 599,768 B | 486,504 B |

These are same-binary fusion comparisons, not revision speedups. The small
timing differences under load do not establish a speedup; allocation falls
by 41–113 KB. [Raw output](loop-fusion.txt). Frozen executable:
`/tmp/rays-loop-fusion.sxTz2g/bench_core.exe`, SHA-256
`dc1cf6c601303bc04a54c36637a40b57a9335d7bceb2e9da21e00833fe0069d7`.
Its CPU compiler/benchmark source snapshot is `source.tar.gz` in that folder,
SHA-256 `323bba5460411852d00f2b4c79589db6f3b8ee41e2cb10615c5dfd2658c2ad92`.

The four fused fixtures also run on Metal and ordinary/fast JavaScript. A new
integer-carry fixture exposed fast targets wrapping at 64 bits instead of the
source Int domain's signed63 wrapping. Int arithmetic and abs now normalize
their result; logical right shifts discard the representation's extra sign
bit before shifting. The separate 64-bit whole-Float arithmetic helpers keep
their existing semantics. An additional shared boundary fixture checks
overflow, multiplication, logical shift, abs(min_int) and min_int / -1.

All fifteen 4,096-row loop fixtures match strict CPU/reference exactly on
Metal, and real Chrome WGSL reports zero error for all fifteen. Its nine
existing fast fixtures and count/compaction checks pass their recorded
tolerances. Browser wall time is 2.54 s; table generation takes 0.72 s in
`/tmp/rays-loop-fusion-web.15kgZZ`. The focused CPU/JS/emitter/native/SPIR-V
request passes in 7.22 s: JS 5.96 s, CPU 3.51 s, Metal 1.12 s. Twenty-two
deterministic SPIR-V binaries, including a loop-free version of the shared
signed63 boundary fixture, validate for Vulkan 1.1; the original eleven
digests remain unchanged. Carried-loop SPIR-V execution remains unfinished.
