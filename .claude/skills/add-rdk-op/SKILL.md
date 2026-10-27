---
name: add-rdk-op
description: Add a packed geometry operation to rdk in rays with its interface, tests, and bench. Use for new mesh, curve, attribute, spatial, or Boolean algorithms.
---

# Add a RDK operation

1. Read `lib/rdk/AGENTS.md` first. Pick the sublibrary by what the op
   depends on (`lib/rdk/dune` and its subdirectories list them, lowest
   first). Never import `geom`, `sop`, or anything above `rdk`.
2. One family module with an `.mli`. Its doc comment states complexity,
   parallel grain, determinism, and cancellation. The entry point is
   `run ?cancel ?grain ~required geometry` returning `(_, Error.t) result`;
   malformed input is an `Error.t`, never an exception. Reuse `Packed`,
   `Topology_index`, `Spatial_index`, `Group`, and `Attribute` helpers; do
   not add a second remap, face normal, or incidence structure.
3. Parallel work goes through `Parallel` with a grain and a sequential
   cutoff, over immutable or disjointly owned data, with `Rand.t` split
   before independent work. Output must be byte-identical for 1 and N
   domains, including attribute and index order.
4. Tests in `lib/rdk/test_<family>.ml` with a `let run () =` entry: expected
   result, malformed input, cancellation, output cardinality, and an exact
   1-domain vs N-domain comparison. Add the module to the `test_main`
   stanza in `lib/rdk/dune` and to the table in `lib/rdk/test_main.ml` under
   its group. Run `dune build @lib/rdk/runtest` (or `@lib/rdk/test_<group>`).
5. High-density paths get `tools/bench_<family>.ml` and a dune stanza next to
   the other benches; record input size, domains, and the median in your
   handoff. Never claim linear or allocation-free without that number.
6. Export it from the `Rdk` facade (`lib/rdk/rdk.ml`), then `add-sop` if the
   op needs a node. Finish with `@all` and `git diff --check`.
