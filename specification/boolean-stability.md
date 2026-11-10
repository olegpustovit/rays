# Boolean stability gate

This protocol is the release gate for Boolean robustness claims. Passing unit,
visual, and generated regressions is necessary but does not establish parity
with another production modeler; cross-engine comparisons live outside this
repository.

## Reproducible local campaign

Run the native Boolean checks with `cargo t -p rdk_ops boolean`. Record the
seed, operation, input topology and failing geometry so each issue becomes a
small deterministic regression. Qualification sweeps use `RAYS_SLOW=1` and
retain the runner's fixed hard deadlines. Previous campaign commands and
results are preserved in [history](../docs/history/README.md).

## Current campaign status

On 2026-08-05 the generated quick and standard campaigns both passed all 24
product rows with exact one-/four-domain equality. Standard includes the
deliberately self-intersecting seven-torus drill bank under explicit
right-input resolution. Its exact constructions create bit-identical rounded
vertices, zero-area slivers, and ULP-scale non-adjacent contacts; mandatory
repair now coalesces, deletes, or contracts them only under the certificates
described in [boolean.md](boolean.md), with complete ancestry composition and
final rounded-surface verification. Standard four-domain medians for that bank
are 0.377 s Union, 1.112 s Intersection, 1.180 s Difference, 0.342 s reverse
subtraction, 1.260 s XOR, and 2.357 s Shatter. The measured complete campaign peaked at
119,000 KiB RSS. Third-party real-model runs are still unavailable.
