# Packed small-map scratch benchmark

`_build/default/tools/check.exe tools/bench_core.exe` followed by
`_build/default/tools/bench_core.exe scratch` measures a 47-instruction float
or typed-integer map at 1, 11, 65 and 1,024 rows. Each row is the median of
three samples of 100 strict forces; allocation and elapsed time are reported
per force. Before/after runs use the same executable source except for the
block-size change, the same host and domain settings, and each checks exact
strict Packed/reference output parity at one and eight domains before timing.

Baseline captured 2026-10-27 on Darwin arm64, before capping block size to the
actual row count:

The paired measurements below used the frozen baseline binary followed
immediately by the changed binary, with no source/build changes between runs.
Every case reports median seconds and allocated bytes per force. The allocation
reduction is consistent; wall times show substantial host scheduling noise,
especially for eight domains, so they are recorded without a speedup claim.

| Type | Rows | Domains | Before seconds | After seconds | Before bytes | After bytes |
|---|---:|---:|---:|---:|---:|---:|
| Float | 1 | 1 | 0.000132568 | 0.000008299 | 398,185 | 13,593 |
| Float | 1 | 8 | 0.000240450 | 0.000006840 | 398,841 | 14,249 |
| Float | 11 | 1 | 0.000217822 | 0.000004890 | 398,345 | 17,513 |
| Float | 11 | 8 | 0.000105159 | 0.000021319 | 399,001 | 18,169 |
| Float | 65 | 1 | 0.000087681 | 0.000025489 | 399,209 | 38,681 |
| Float | 65 | 8 | 0.000030930 | 0.000024581 | 399,865 | 39,337 |
| Float | 1,024 | 1 | 0.000335219 | 0.000436690 | 414,553 | 414,609 |
| Float | 1,024 | 8 | 0.000831211 | 0.000674720 | 415,209 | 415,265 |
| Int | 1 | 1 | 0.000195940 | 0.000014369 | 779,305 | 10,065 |
| Int | 1 | 8 | 0.000277278 | 0.000002449 | 779,305 | 10,065 |
| Int | 11 | 1 | 0.000151680 | 0.000007379 | 779,705 | 17,985 |
| Int | 11 | 8 | 0.000152588 | 0.000006590 | 779,705 | 17,985 |
| Int | 65 | 1 | 0.000225682 | 0.000039041 | 781,865 | 60,753 |
| Int | 65 | 8 | 0.000209720 | 0.000045660 | 781,865 | 60,753 |
| Int | 1,024 | 1 | 0.000900860 | 0.001120191 | 820,225 | 820,281 |
| Int | 1,024 | 8 | 0.001487701 | 0.001462040 | 820,225 | 820,281 |

The unchanged 1,024-row block size retains essentially the same allocation;
the 56-byte difference is present in the after binary, which also includes the
validated Region/lazy-flags refactor. For 1, 11 and 65 rows, float scratch
allocation drops from about 399 KB to 14–39 KB; typed allocation drops from
about 780 KB to 10–61 KB. Strict packed/reference outputs matched at one and
eight domains for every measured case.
