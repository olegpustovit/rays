# lib/rdk rules

`rdk_core` owns the packed storage and topology foundation. `rdk_geom` owns exact
predicates and planar algorithms, indices, proximity queries and point clustering (directories
`exact/` and `spatial/`). `rdk_ops` owns attribute and group operations, the point generators, curve
sampling, editing and sweeps, and the modeling operations (`attrib/`, `curve/`, `mesh/`).
`rdk_boolean` owns Boolean stages.
`rdk` re-exports the stable public module paths; renderer conversion is isolated in
`rdk_rays`. `sop` wraps RDK operations as SOPs.

Sublibraries have one-way dependencies: core → `rays_math`, exact → core,
spatial → exact/core, attrib → spatial/exact/core,
curve → spatial/exact/core, mesh → curve/attrib/spatial/exact/core, and each higher family may
depend only on lower families. Add the Dune edge
and its injected-forbidden dependency-gate check together. Move a module with
its `.mli`, or add the missing `.mli` during the move. Keep module aliases in
`Rdk` so consumers do not change paths; keep implementation-only modules
private. Run the family tests and a one-domain/four-domain exact comparison
before marking a split complete.

RDK has no parity-only code: a kernel exists only for a native SOP (exact predicates, spatial indexes, solvers, wiring, carriers) or a Lisp host operation. Never add an OCaml twin of a Lisp node or keep a replaced kernel as a test oracle; run `prune-dead-code` after removing a caller.

`lib/rdk/test_*` and `lib/sop/test_*` open `Rdk_test_support`
(`lib/rdk/test_support`) for `fail`, `check`, `get_ok`, the geometry
comparisons and the like; a test file defines only helpers that differ.
definition (`--open`), and does the same for library helpers in
`Rdk_core.Support` (`--qualify Support`). Hot `[@inline]` helpers stay local:
dev builds are opaque, so a cross-module call is not inlined.

Geometry libraries follow this additional direction:

```text
sop ──> rdk ──> rdk_core ──> rays_math
     └───────> rdk_rays ──> rays
```

`sop` uses `rdk` for packed SOPs and `rays_math` for mathematical
values. `rdk` must never depend on `sop`.
`sop` and `pxui` must never depend on each other; typed parameter
schemas live in the dependency-free `param` library (`Param`)
and `Pxui_shell.Inspector` renders them.
`pxui_graph` and `Pxui_shell.Inspector` are presentation adapters, not graph authorities:
selection lives in returned immutable UI state, network topology lives in
`Sop.Edit_graph`, and the `rays_editor` host applies typed editor commands
before compiling a cookable DAG. Parameter edits replace the selected node in
that same immutable document (or use `Node.apply_parameters` for a standalone
node). `sop_catalog` may attach
PPX-derived schemas through `Node.parameterize`, but delegates cooking to
ordinary Sop SOPs. `sketch_ui` composes these leaves and must not move
widgets, camera policy, or render lifecycle into `sop` or `rdk`.

## Procedural geometry scope

- Target a complete production toolset for procedural geometry modeling:
  mesh and curve construction, topology editing, attributes and groups,
  selections, spatial queries, subdivision, booleans, repair, remeshing,
  reduction, instancing, and deterministic import/export.
- Existing UV projection/flatten/relax utilities remain supported, but UV
  feature-parity expansion is not a modeling priority until requested again.
- Rigging/KineFX, crowds, fluids, pyro, Vellum, MPM, dynamics solvers,
  compositing, terrain/heightfield systems, USD pipelines, and general VFX
  simulation are explicitly out of scope until requested separately.
- Parity claims against other modelers are made only from the external
  comparison workspace (`../rays-support`) and apply only to the in-scope
  polygon/curve modeling surface; never imply parity with a full product.

## Single geometry core

- `rdk` is the only owner of packed mesh topology, reverse incidence,
  half-edge/edge indexing, spatial acceleration, attribute interpolation and
  promotion, and high-density modeling algorithms.
- `rays_math` owns mathematical values such as vectors, bounds, matrices,
  noise, random streams, and parallel execution policy.
- `sop` wraps RDK operations as immutable SOP nodes and must not
  reimplement packed geometry algorithms inside graph cooks.
- When replacing a public algorithm, compare a captured public result before
  and after the change, then add direct RDK correctness, malformed-input,
  cancellation, cardinality, and one-domain/multi-domain exactness tests.
- Shared topology structures must be packed integer arrays/bytes with explicit
  ownership and O(points + vertices + primitives + edges) storage. Public
  immutable wrappers may expose safe queries; audited kernels may borrow
  read-only planes through a narrow `View` module.
- Robust operations separate combinatorial decisions from approximate metric
  calculations. Use adaptive/exact predicates for orientation, incircle, and
  intersection signs where floating-point ambiguity can change topology;
  tolerances remain explicit policy, not a substitute for robust predicates.
- External native geometry libraries require an explicit dependency, license,
  portability, determinism, and native-build review. Prefer a small audited
  native OCaml kernel for core operations; reuse a mature library only when it
  materially improves robustness and the boundary preserves RDK ownership.
- Production mesh Booleans use an exact surface-arrangement/Weiler pipeline,
  not BSP polygon clipping, centroid classification, voxel/SDF resampling, or
  a tolerance-welded triangle soup. Preserve symbolic/implicit intersection
  constructions until output materialization; all predicates involving those
  points must be filtered exact or exact.
- The Boolean kernel is staged and reusable: deterministic broad phase, exact
  intersection classification, implicit seam construction, per-face
  constrained Delaunay refinement, coincident-facet handling, radial ordering,
  patch/cell classification, Boolean-expression extraction, ancestry-aware
  payload transfer, one-time rounding, and bounded seam cleanup/verification.
  Detection and floating intersection-analysis nodes are diagnostics only and
  must never be silently reused for topology-changing decisions.
- Standard normal payload is orientation-aware: when extraction reverses a
  source facet, transferred point/vertex `N` must be reversed and normalized
  with it. Terminal packed-piece expansion must preserve those authored
  normals instead of silently replacing them with per-triangle normals.
- Boolean product scope includes variadic expressions, union/intersection/
  subtraction/XOR, seam, shatter, solid/surface treatment, self-intersection
  resolution, coplanar overlap, non-manifold arrangement edges, and stable
  source ancestry. Expose a public SOP only as each advertised mode reaches
  exact one-domain/multi-domain parity, adversarial degeneracy coverage, and a
  measured scale baseline.

## Production performance contract

Rays targets interactive procedural graphics and high-density offline mesh
generation. Performance is a correctness property for hot paths, not a later
cleanup step.

- Establish a benchmark and allocation baseline before changing a hot path.
  Report wall time, promoted/major allocations, peak live memory when
  practical, input size, domain count, compiler profile, and machine details.
- Document asymptotic time and auxiliary-memory complexity for public
  geometry algorithms. A green functional test is not evidence that a
  million-element workload is production-ready.
- Keep immutable public values, but use locally owned mutation internally:
  pre-sized arrays, growable buffers, hash tables, bitsets, and disjoint output
  slices are preferred over allocation-heavy persistent rebuilding inside an
  algorithm.
- Use packed numeric storage (`float array`, integer arrays, Bigarray, or
  structure-of-arrays layouts) for high-density geometry. Do not represent a
  million-element hot buffer as boxed lists or repeatedly convert it between
  lists and arrays. Public list conveniences must stay outside inner loops.
- Never use `List.nth`, repeated `List.length`, `@`, `Array.append`, nested
  `List.concat_map`, or per-element `Option` boxes in a measured hot loop.
  Linear builders must be amortized O(1) per append and materialize once.
- Mesh generators must compute output cardinality up front when topology makes
  it knowable, allocate once, and fill by index. For variable output, use a
  geometric-growth builder. Avoid generate-list → reverse → convert-array
  pipelines for dense output.
- Spatial and topology algorithms must use integer/index keys and compact
  adjacency storage where possible. Avoid polymorphic comparison/hash in hot
  paths when a specialized integer key is available. Avoid repeated global
  scans for local queries.
- Reuse the process-wide Domainslib pool. Never create or tear down domains per
  frame, algorithm iteration, collection, or asset. Parallel APIs must expose a
  tunable grain, retain a benchmarked sequential cutoff, and write only to
  disjoint owned output ranges.
- Split deterministic work by stable index ranges. Parallel execution must
  produce byte-identical ordered results to the sequential path for fixed
  inputs; do not let work-stealing order leak into mesh indices, hashes,
  palettes, random streams, exports, or diagnostics.
- Every parallel refactor needs a regression that compares one-domain and
  multi-domain results exactly. For geometry, compare vertex attributes,
  indices, primitive modes, and ordering; for rendering, compare captured
  native-framebuffer pixels or byte-identical exported PNGs.
- Run representative visual scenes in both one-domain and multi-domain modes.
  Treat unexplained pixel drift, missing primitives, changed winding, seams,
  or nondeterministic frame artifacts as correctness failures, not acceptable
  performance tradeoffs.
- Do not parallelize SDL, renderer, texture, image decode/upload, font, audio,
  event, or cache mutation. Parallelize pure sampling, field evaluation,
  transforms, classification, and independently owned geometry preparation,
  then join before the backend boundary.
- Frame hot paths must avoid work proportional to unchanged scene/resource
  size. Cache immutable derived data by stable identity with bounded lifetime;
  invalidate precisely on source mutation or renderer-density changes.
- Rendering inner loops must not allocate per pixel, sample, light, fragment,
  or triangle edge. Reuse scratch storage and precompute invariant material,
  light, transform, clipping, and texture state outside raster loops.
- Long-running workloads must be memory-bounded. Every cache needs an explicit
  capacity/eviction policy; the topology caches in `Point_index` and
  `Topology_index` use `Support.Identity_cache` (64 ephemeron entries each,
  first-in first-out, released with their topology); temporary
  arenas/builders must become unreachable after a job; resource destruction
  remains explicit at the owning boundary.
- Prefer algorithmic wins over micro-optimization: eliminate quadratic scans,
  reduce topology passes, cull early, stream where possible, and avoid storing
  derivable duplicates before tuning arithmetic.
- Add scale tests for regressions in output cardinality and auxiliary storage.
  Add or update `tools/bench_*` for any new high-density path. Timing thresholds
  in CI must be broad and diagnostic; deterministic allocation/cardinality
  ceilings may be strict.
- A performance-sensitive handoff must include the benchmark command and
  before/after evidence. Do not claim “zero allocation”, “linear”, “parallel”,
  or “production-ready” without measurement or code-level proof.
