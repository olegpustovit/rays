# Retired port provenance

The original port notes remain available in Git at checkpoint `9515dda21af337a8f7143333c503f6e38bb96674`.
The native cleanup removes source-level translation comparisons; it does not reinstate the retired toolchain.

The remaining capture scripts are in `ocaml-oracles/`. The SOP translation table and image fixture
regeneration notes are archived separately. A tracked `_build/trace.csexp` was stale machine-specific
build output and was deleted; no runtime fixture consumes it.

The substantive changes are native Rust shortest round-trip decimals, standard parsing and trim,
full-width caret/state/arithmetic bounds, and direct use of the existing byte and Unicode quote owners.
Byte-string escaping, exact geometry tie order, numerical operation order without FMA contraction,
resource dependencies, edit precedence, and paint/hit priority remain current contracts.

The archived native lowering gate retains its original 36/45/205/260 ms thresholds; its source data
is documented in `eval-design.md`. Cold compile and runtime timings must be measured for the native
change, independently of this historical evidence.
