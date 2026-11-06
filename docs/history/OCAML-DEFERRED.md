# Deferred work

Open product issues beyond routine implementation. Completed items are deleted.
The approved architecture cleanup is tracked in
[specification/cleanup/status.md](specification/cleanup/status.md).

- Restore missing Metal coverage for render encoders, render ICB pixels and
  ownership, acceleration build, M1 ray queries, argument reflection/encoding,
  presentation and the stress lanes; compare against the earlier suites.
- Replace the real-clock live-resize assertion with a deterministic boundary
  that distinguishes event-pump stalls from presentation stalls.
- Reproduce the reported particle decay to 10 FPS with Graph and Lisp panes;
  exact window/launch conditions remain unknown. The recorded toggle benchmark
  did not reproduce it.
- Share one compiled/evaluated residual across recorded paths and state reads;
  count walker executions directly. Keep unsupported loops in the performance
  inventory until the widened shared region IR handles them.
- Reduce packed compilation's region validation/copy overhead; rerun
  `tools/bench_core.exe kernels` before changing it.
- Retain other static inline map inputs beyond literal Float `array/range`.
- Reduce static million-mark startup allocation (historically 288 MB, 375 MB
  peak RSS). Host-buffer Marks need exact corner arithmetic, pinned retained
  source lifetime and unchanged refusal behavior before replacing Shape_batch.
- Keep large static web maps as device regions when eligible instead of
  shipping their materialized binary64 arrays; preserve CPU parity and fallback
  semantics. Halving constants to binary32 does not meet the numeric contract.
- Revisit the native lifecycle footprint test's obsolete window-floor baseline
  and tighten its bound using current repeated-run measurements.
