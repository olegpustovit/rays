# Isogrid compiler contract

The checked Lisp sketch remains authoritative. Optimizations preserve authored
values, topology, attributes, diagnostics, determinism and exact sequential/parallel
results. See [Flow](flow.md), [core](core.md) and [backend](backend.md).

- Captured homogeneous numeric lists may use internal packed tables. Their public
  value remains a list. Gather operations retain element types and bounds errors.
- Trusted static kernels may execute before records replay on demand. A refusal
  restarts from element zero; partial results cannot leak into the fallback.
- Large static SOP mesh programs may defer until cooking. `rays check` disables
  deferral so invalid constant topology remains a checking diagnostic.
- Counts, identity and ownership metadata never force deferred payloads. The first
  payload read materializes once across racing workers and caches success or failure.
- Device topology uses exact unsigned indices. Validate all counts, offsets and
  bounds before publication; a failure publishes nothing and returns `E_TOPOLOGY`.
- Resident display borrows position, normal, colour and index buffers for one owned
  lifetime. Picking, inspection, wire/vertex modes and consumers requiring host data
  materialize explicitly and account for the transfer. Closing the backend releases
  placement state and all borrowed resources.
- Device placement uses measured CPU/device cost for an authored site and size class.
  Include display sinks and required CPU readers in the measurement. Missing cost
  evidence stays on the CPU; fixed guessed size thresholds are not evidence.

Historical implementation stages, measurements and logs are preserved in
[the implementation record](../docs/history/isogrid-implementation.md).
