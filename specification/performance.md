# Performance and memory architecture

## Runtime performance

Rays is designed for live creative coding and high-density deterministic
offline generation. Public APIs remain immutable; implementation hot paths may
use locally owned mutation and packed storage without exposing mutable aliases.

## Workload classes

| Class | Typical scale | Primary constraint |
|---|---:|---|
| Per-frame interaction | 60–240 updates/s | predictable latency and bounded allocation |
| Dense procedural mesh | 1–100 million vertices/indices | packed memory, linear passes, coarse parallelism |
| Scalar/voxel field | 128³–1024³ samples | streaming/slab memory and parallel field evaluation |
| Offline sequence | thousands of frames | byte determinism and no cumulative cache/resource growth |
| Native Metal rendering | millions of vertices/fragments per frame | bounded command/resource reuse and stable GPU submission |

## Representation rules

- Mesh positions and normals are retained as structure-of-arrays XYZ float
  planes. Core renderer access borrows those planes without copying;
  user-facing list functions and the legacy boxed private view are explicit
  convenience/materialization boundaries, never renderer input. Packed
  generators can transfer plane ownership directly, avoiding a second boxed
  output at peak memory.
- Dense numeric working sets use packed float/integer arrays or Bigarray.
  Record/list forms are acceptable for small control geometry, diagnostics,
  and API boundaries.
- Known-size generators allocate exact output arrays. Variable-size algorithms
  use geometric-growth builders and trim once.
- Topology uses stable integer indices, specialized integer/edge hashes, and
  compact adjacency. Algorithms must not repeatedly recover topology from
  boxed face lists.
- Persistent spatial structures are for incremental/query workloads. Bulk
  construction may use mutable staging and freeze once.
- A graph-pane frame costs what is in view, never the size of the graph: `Pxui_graph.Scope`
  builds bucket indices over tiles and wire bounds with the geometry and asks them each
  frame (`lib/pxui_graph/AGENTS.md`). Navigation and node movement do
  no procedural cook work.
- `Iso3.extract` streams deterministic XY-plane windows in two passes. It
  retains exact-sized output and finite-difference normals without the former
  four full XYZ scalar/gradient volumes; fixed ring buffers make temporary
  allocation and auxiliary sampling memory O(XY). Its final positions and
  normals are filled directly into the mesh's packed XYZ planes. `Iso3.Field` evaluates
  built-in constant, sphere, gyroid, and metaball fields directly without
  per-sample point allocation; custom fields borrow one packed XYZ sample per
  domain.
- Extrude, lathe, and sweep generators precompute vertex/index cardinality and
  fill owned arrays directly. Angle-aware smoothing uses indexed triangle
  arrays and scalar normal accumulators rather than materializing face and
  index lists.
- Duplicate-vertex merging uses exact-position hashing at zero tolerance and a
  packed open-addressed spatial-cell table at positive tolerance. Candidate
  chains contain representatives only, preserve the earliest compatible
  source, and still compare every present normal, color, and UV attribute so
  seams are not collapsed. Extreme finite coordinate/epsilon ratios use exact
  float-bit components instead of overflowing the grid index.
- Mesh topology and repair traverse borrowed triangle arrays. Connected
  components are joined directly from edge incidence with union-find, and
  filtering/reorientation fills exact owned index arrays. T-junction repair
  builds one 60-bit Morton-sorted hierarchy with packed subtree bounds over
  finite vertices, performs bounded edge queries, and splits faces iteratively
  in stable source order instead of rescanning every vertex and restarting the
  complete face list per split.
- RDK reverse topology uses packed integer CSR/half-edge planes and a
  specialized open-addressed integer edge table.
- Filtered exact RDK orientation predicates expose packed SoA/index calls so
  the certified fast path does not box coordinates.
- The Boolean stability runner's standard-density campaign additionally covers
  mandatory binary64 representability repair on an explicitly self-resolved
  seven-torus cutter bank. All 24 products are exact between one and four
  domains.
- Fuse discovers earliest-representative proximity clusters in stable point order. Cluster
  member CSR planes make each numeric reduction independent and parallel while
  preserving the exact source-order summation for every domain count. Its
  expected complexity is O(points + neighboring candidates + payload), with an
  explicitly documented quadratic worst case for a single dense tolerance
  neighborhood.
- Fuse and Facet normal consolidation share that one private packed clustering
  core. Discovery arrays are reused as representative, size, and CSR cursor
  planes after their spatial phase instead of allocating phase-local copies.
- The fixed-target Fuse planner builds one packed target-cell table, then runs
  allocation-free candidate searches over disjoint query ranges.
- Same-input Modify Target converts its packed target links into stable
  minimum-root components, then fills exact position, payload, and topology
  ranges.
- Fuse point-attribute and group rules retain stable cluster membership and
  ordered output while reducing independent packed planes in parallel.
- Ray multi-sampling traverses the shared packed collision BVH with one
  O(samples) result/order scratch set per worker range. It never retains a
  points-by-samples hit matrix: average attribute import performs a second
  deterministic traversal after exact CSR prefix sizing.
- Attribute Combine fuses the complete ordered layer stack into one output
  allocation and one parallel element traversal.
- Attribute Interpolate resolves mixed-owner fields into specialized packed
  job sets and shares one primitive-coordinate traversal across all payloads.
- Closest-surface Attribute Transfer builds one deterministic packed polygon
  AABB hierarchy and reuses primitive/triangle IDs, barycentric coordinates,
  and squared distances for every typed payload plane. Deterministic ear
  clipping retains original primitive/corner IDs.
- `Attribute_pattern` compiles name selection once into literal, wildcard, and
  256-bit byte-class atoms. Matching performs no allocation and retains stable
  attribute order.
- Batch Attribute Delete/Rename compiles patterns before mutation, scans stable
  metadata through owner-local name tables, and commits one attribute array
  while sharing packed payloads.
- Attribute Swap extends the same atomic metadata store with geometric-growth
  entries and one final commit.
- Connected Poly Extrude precomputes stable region/point associations and exact
  output cardinalities, then fills packed layer, topology, attribute, and group
  ranges directly.
- Poly Fill plans complete one-sided polygon boundary components once, derives
  every point/corner/primitive cardinality before allocation, and fills stable
  loop ranges directly.
- Clean's degeneracy classifier uses a packed triangle fast path and a robust
  normalized fallback only when finite cross arithmetic overflows.
- Enumerate computes selected counts independently per stable chunk, scans the
  small chunk-count plane in order, then fills disjoint attribute ranges. The
  integer path allocates only its exact output plus O(chunks) words.
- Sort builds one stable new-to-old permutation and materializes remapped
  packed output once. Point order applies an inverse map to every corner while
  structurally sharing unchanged vertex/primitive payloads; primitive order
  fills CSR spans and vertex/primitive payloads together while sharing point
  storage. Stable comparison sorting is sequential O(n log n); key preparation
  and disjoint payload fills are parallel.
- Duplicate computes output cardinality before allocation, precomputes stable
  transform powers, and fills copy-major position/topology/normal ranges in
  parallel. Ordinary payloads use bulk replication and groups use packed
  repeated membership.
- UV Project partitions primitives into stable ranges so every corner is owned
  by exactly one worker; UV Transform partitions its point/vertex owner plane.
  UV Auto Seam computes face normals and edge classifications in parallel into
  byte-packed flags, then assigns deterministic islands with ranked union-find.
  UV Unitize parallelizes edge continuity, primitive bounds, and disjoint UV
  output; its stable island reduction is sequential. Edge Group uses the same
  shared face-normal kernel and now publishes its final packed edge bits
  directly. Incident-edge angles precompute normalized SoA directions once,
  then scan shared-point CSR adjacency without per-pair allocation. A mutex-protected weak cache
  stores at most one reverse index per live immutable topology; cache data does
  not retain its weak topology key.
- Group Promote and fixed-step Group Expand classify disjoint output-byte
  ranges against the same cached reverse topology.
- Ordered wildcard Group Promotions preserve rule dependencies serially while
  running each matched conversion over disjoint packed ranges.
- Group Promote Boundary composes ordinary conversion with the same packed
  attribute-boundary classifier.
- Group from Attribute Boundary uses the shared reverse-topology index and one
  packed output edge plane. Selected numeric attribute planes are validated in
  parallel before classification; edge bytes, point bytes, and primitive bytes
  are independent ranges. No temporary storage scales with the number of
  selected attribute boundaries or incidence visits.
- Packed procedural instances retain one SOP prototype plus exactly 16 floats
  (128 payload bytes) per transform. `Scene3.instances_array` copies that array
  once, and the renderer traverses one batch descriptor instead of constructing
  a drawing list per frame.
- Orphan-point compaction scans topology once, builds stable old/new point
  maps, then remaps positions, point payloads, ordinary/native edge groups, and
  vertex references through disjoint packed ranges. Bounds and Match Size scan only positions;
  Match Size delegates normal handling to the established inverse-transpose
  transform boundary.
- Copy to Points precomputes one packed scale, complete orientation basis, and
  optional pivot-adjusted translation per target, then writes copy-major source
  ranges independently. `orient` takes priority; otherwise scale-safe `N` or
  fallback `v` plus `up` synthesis aligns source +Z/+Y, with a shortest-arc
  fallback that remains stable for zero or parallel up vectors. A post-basis
  `rot`, source-local `pivot`, and additive `trans` complete the standard point
  transform stack without materializing per-copy matrices.
- Curve Carve scans source segments once, computes exact output cardinality,
  maps retained source breakpoints directly, and performs only two O(log n)
  arc-location searches per primitive. Packed position and numeric payload
  fills use disjoint ranges.
- CSG streams polygon classification without temporary vertex/type arrays.
  BSP construction and clipping route one polygon at a time, allocate new
  geometry only for actual plane crossings, and use scalar plane math so
  classification does not box float results. Whole-tree collection, inversion,
  and clipping use explicit work stacks to tolerate long convex BSP chains.
- Loop and Catmull-Clark subdivision share borrowed
  triangle/attribute arrays and sorted edge arrays. Known triangle counts fill
  exact owned index buffers, including backward fills where legacy prepend
  order is observable. Position stencils run directly over packed geometry;
  weighted term graphs are constructed lazily only when color or UV attributes
  need interpolation.

## Parallel execution

Only pure CPU phases run on Domainslib. Work partitions use stable integer
ranges and disjoint output slices. Scheduling grain is configurable, with a
sequential cutoff chosen from benchmarks. The reusable pool is process-scoped;
creating domains per frame or geometry operation is forbidden.

`Parallel.run` selects and exclusively borrows the cached pool for the complete
operation. Each parallel helper opens a bounded Domainslib task scope and
submits stable contiguous chunks; domain-local pool context is installed once
per chunk rather than once per element. Nested helpers reuse that context and
still honor their sequential grain cutoff. Keeping task scopes at helper
boundaries avoids retaining scheduler state across unrelated algorithm phases.

Parallel candidates include field sampling, point transforms, independent
vertex attributes, classifications, and per-cell geometry counts/fills.
Topology joins, stable prefix sums, output ordering, SDL3 work, renderer caches,
and resource upload remain deterministic join/initial-domain phases.

Renderer-derived mesh data uses bounded weak-key caches. Stable meshes retain
one prepared native plan and upload once; changing geometry invalidates only
its identity/generation entry. Retained plan, pipeline, image, font, and canvas
caches have explicit capacities and release resources through completion-owned
queues rather than relying on GC timing.

SDL3 owns the window and Metal view, while OGPU/Metal owns command encoding,
depth attachments, sampled resources, MSAA, and presentation. Resize
replaces drawable-sized resources instead of accumulating extents. The initial
domain records and submits GPU work; pure mesh/scene preparation may run in the
shared pool and joins before upload.

Frame performance is qualified with the cleanup plan's P3 protocol:

## Measurement contract

The focused RDK benchmarks report elapsed
time and GC allocation for their declared geometry fixtures. Run them with the
release profile and record input cardinalities and domain count.

Performance changes require before/after measurements on the same machine and
compiler profile. Correctness tests additionally enforce deterministic output,
expected cardinality, and bounded cache/resource behavior. Timing is diagnostic
unless a stable dedicated benchmark runner is available.

`tools/bench_gpu.exe` reports native fixed costs with separate upload, synchronized dispatch,
GPU duration and readback columns at 1,024, 65,536 and 1,000,000 elements.

`tools/bench_shattered_renderer.exe` is the native frame benchmark: it cooks
the shattered-cube graph, renders the packed result through the sketch runtime
at 1200×760 logical points, and prints one JSON line with frame percentiles,
allocation, RSS and the live drawable size and scale.

Every parallelized operation is also exercised inside `Parallel.run ~domains:1`
and with multiple domains. Geometry output must be exactly equal, including
attribute and index ordering. Renderer-facing changes require a native
framebuffer capture or exported-PNG comparison of representative scenes. The existing
deterministic export test compares PNG digests across repeated runs; visual
coverage must grow alongside new renderer features and optimized drawing paths.

RDK plane clipping classifies canonical or numeric point-coordinate planes in
parallel. Polygon-only unfilled clipping counts source/side outputs in stable
parallel slots, performs one deterministic prefix, allocates exact corner and
primitive planes, and fills disjoint slices. Curves, caps, and repeated-corner
fallbacks retain the geometric-growth builder; cap-loop topology remains
sequential. Shared-edge token ordering fixes point IDs independently of task
scheduling.

A concave polygon with more than one retained boundary run deliberately leaves
the dense exact planner. Its k clipping endpoints are sorted along their widest
stable plane axis, paired through the source interior, and linked as a compact
chain permutation before output cycles are materialized. This uncommon path is
O(vertices + k log k) time and O(vertices + k) transient storage per affected
polygon; ordinary convex/triangle/quadrilateral workloads allocate none of
that reconstruction scratch.

Nested Clip caps keep direction while tracing boundary segments, classify
opposite-winding containment, normalize coordinates around a local anchor,
insert deterministic non-crossing visibility bridges, and ear-clip the weakly
simple ring into ordinary triangles. A packed X-interval sweep rejects
self-intersections and inter-contour touching in O(c log c + i) expected time
for i active bounding-box candidates, with O(c^2) worst case. For c original
contour vertices and h holes in one component, bridging is O(h*c^2) worst case,
the bridged ear pass is O((c + 2h)^2), and scratch/output planning is O(c + h).
Independent contour components remain in stable token order.

Typed Clip restriction first promotes the selection once, then marks selected
primitive incidence and only allocates isolation tokens for source points
actually shared across selected and unselected primitives.

The shared RDK subdivision kernel computes exact child cardinalities and one
stable CSR stencil plan per level. Catmull-Clark/bilinear emit four-corner
polygons directly instead of triangulating and rebuilding them; Loop emits four
triangles per source triangle. Face-varying data writes from face-local source
corners, so no seam weld or tuple expansion is hidden in the hot loop. Numeric
planes, including semi-sharp position/point primvars, evaluate in parallel;
topology indexing, full edge/vertex-fan validation, and stable planning remain
sequential only where prefix cardinality or stable topology order requires it.
Exact-sized point-stencil and output-topology ranges fill independently.

Primitive-group refinement partitions selected and unselected topology once,
retains free points once, and duplicates only boundary points. A union-find
pass splits selected faces that touch only at a point into valid independent
fans without breaking edge-connected stencils.

Second-input crease preparation reuses the source topology index when the
topologies are identical. Attribute-driven crease reduction runs once per
unique edge into disjoint slots; a sparse override input instead walks only
its selected primitive incidence and probes the source integer-edge table.

The first resulting-group implementation constructed and retained a complete
target reverse-topology index merely to find child edge numbers.

Hole refinement uses the complete source adjacency for stencils while a
primitive-prefix plan allocates and fills only visible descendant topology.

OpenSubdiv point-boundary policy adds no alternate geometry representation.
Edge Only and Edge and Corner share the packed subdivision planner; the latter
changes only one-face boundary point stencils. None scans the cached integer
reverse topology, writes packed point/primitive boundary bitsets in cancellable
disjoint byte ranges, and then reuses the ordinary hole path.

Face-varying policy classification is cardinality-first and retains exact
source-order summation. Continuous fields reuse the geometry plan; fully split
one-face fields bypass a second reverse-topology index; mixed fields build one
packed value topology and CSR stencil plan shared by every tuple component.

Smooth Triangles is a branch inside the existing Catmull-Clark interior-edge
stencil fill, not an alternate topology pass. It reads the two already indexed
incident face arities, computes the exact 0.470/0.25 interpolated mask, and
writes the same pre-sized CSR slots. Complexity and auxiliary storage remain
O(points + edges + vertices + output stencil terms).

Uniform and Chaikin creasing share one packed child-edge plan. Each source
point owns the two endpoint slots of its incident edges, so the O(points +
edges) child pass is parallel without atomics. Six O(points) packed rule,
transition, and crease-neighbor planes then encode exact parent/child vertex
masks; positions and every compatible numeric plane reuse them. Resulting
vertex weights and native edge groups consume the same two-E child array rather
than rescanning neighborhoods.

Houdini detail controls add one sequential scan of the immutable attribute
metadata and five scalar enum resolutions before the existing plan. They do
not add a second topology or per-element branch.

The no-second-input all-edge override is not implemented by first creating a
vertex `creaseweight` field. Its scalar fills the E-entry sharpness plane in
the first refinement plan, after which ordinary residual fields drive deeper
levels.

Point-normal Subdivide retains the same cardinality-first point stencil used
for `P`; it does not add a topology pass in the default interpolation mode.
Opt-in recomputation runs the shared deterministic area-weighted point-normal
kernel once after the complete recursive/local/mixed cook, rather than once per
subdivision level.

Polygon-curve Subdivide uses exact point/stencil/vertex cardinalities and
packed integer topology throughout.

Point, vertex, primitive, Blast, and Split filtering share one RDK deletion
planner. It counts retained primitives/corners/points before allocation,
materializes each map once, preserves ascending source order, and shares point
positions/attributes/groups when the point map is identity. Healing performs
no tuple/list allocation per primitive.

RDK Convert Line builds the shared reverse-topology index once, filters each
unique edge once, and uses stable 16-bit radix passes to produce canonical
point-number ordering without comparison-sort tuple allocation. It allocates
exact two-corner CSR output, shares point/detail payloads, and fills topology,
primitive lengths, and topology-affine group bytes through disjoint ranges.

RDK PolyPath consumes the same cached unique-edge index but avoids Convert
Line's two-corner primitive for every intermediate edge. With endpoint
connection disabled, the common self-edge-free path borrows the index endpoint
planes directly; only endpoint rewiring allocates an open-addressed deduplication
graph. One sequential stable trace permutation visits every edge exactly once,
while topology and every requested payload plane materialize into disjoint
owned ranges. Optional ancestry planes are omitted when their owner has no
attribute or group. Complexity is O(vertices + unique edges + payload) time and
O(points + unique edges + output + payload) storage.

Convert Line's Connect Path mode is fused into that packed graph rather than
materializing and then re-indexing 3,002,000 two-corner primitives.

The directed Line source normalizes extreme finite directions with scaled
arithmetic, allocates its position and optional identity topology planes once,
and fills both in the same disjoint point pass.

Circular Sweep/PolyWire now precomputes one sine/cosine cross section instead
of evaluating trigonometry at every ring, uses arithmetic primitive offsets,
and partitions independently owned spine curves with a grain derived from
average curve size.

PolyFrame is topology-linear: every style takes O(points + vertices +
primitives) time and O(points + vertices + primitives) auxiliary/output
storage, with a fixed number of packed numeric/index planes. The first runnable
361,201-point implementation returned or retained boxed values in element
loops.

Every final path promoted zero bytes.

Facet Unique Points is O(points + vertices + primitives + attribute/group
payload + edge incidences) time and auxiliary/output storage. It derives exact
cardinality before allocation, fills a target-point-to-source-point plane, and
uses it for positions, all eight point storage kinds, and ordered point groups.
Native edge groups use corner ancestry so a selected shared edge expands to
both unshared descendants.

Orient Polygons is O(vertices + unique edges + primitives + reversed vertex
payload) time and O(topology index + primitives + reversed output) storage.
Stable component BFS remains sequential so orientation cannot depend on work
stealing; only topology/payload materialization uses disjoint parallel ranges.

Primitive-restricted Unique Points is O(points + vertices + primitives +
selected output payload) time. Its selection analysis uses saturated point
incidence, selected-reference, and unselected-reference byte planes instead of
a complete edge/topology index. Selected/unselected corner masks for normal
adjustment are likewise direct packed byte planes.

Typed Facet point and vertex promotion is O(vertices + primitives) time with
one packed primitive bitset; predicates scan each primitive's contiguous corner
range independently. Native-edge promotion additionally builds the standard
O(points + vertices + unique edges) topology index to translate topology-affine
edge ordinals. The promoted mask is then shared by every enabled Facet stage.

Facet Cusp Polygons is O(primitives + corners + unique edges + output payload)
apart from inverse-Ackermann union/find cost, with O(corners + unique edges +
output) auxiliary/output storage. Face normals and edge classification use
disjoint parallel planes. Stable point/incidence traversal assigns output
numbers, and iterative path compression with deterministic union-by-rank avoids
both scheduler-dependent numbering and recursive stack growth. The cusp stage
preserves the original geometry by identity when no fan splits are required.

Remove Inline Points is O(points + corners + primitives + output payload +
native-edge incidences) time and O(points + corners + primitives + output)
auxiliary/output storage. Its per-primitive cyclic queue is exact-sized; each
deletion schedules at most two adjacent corners, so iterative simplification
remains linear. Classification and target-plane fills use disjoint stable
primitive ranges, while point compaction is a deterministic ordered prefix.
The initial correct hot loop used polymorphic float min/max and allocated a
local enqueue closure per deletion.

Consolidate Normals is O(points + neighboring candidates + normal elements)
expected time and O(points + clusters + normal output) auxiliary/output
storage, with the same documented quadratic dense-cell worst case as Fuse.
Greedy earliest-representative discovery is stable and intentionally
sequential; point/vertex validation, per-cluster scale-safe reductions, and
disjoint normal fills parallelize.

Make Planar is O(points + corners + primitives + output point payload) time and
storage. Scale discovery, centroid/Newell-plane construction, planarity
classification, and projection run independently over stable primitive ranges.
A sequential source-point claim pass assigns deterministic originals or
conflict copies; packed payload materialization then reuses the common ancestry
kernels.

Every final path promoted zero bytes.

The million-point polygon Circle source replaces its former boxed-tuple plus
builder pipeline with exact position/index planes filled together.

The production Box generator derives every cardinality before allocation and
fills packed lattice, surface, topology, normal, UV, and group planes in stable
disjoint ranges. Its integer-coordinate hot helpers avoid boxed float
arguments on the non-Flambda compiler.

UV Sphere precomputes O(segments+rings) trigonometric tables, fills positions
and point normals together, and emits fixed proven outward index patterns
instead of evaluating latitude trigonometry per point and recomputing winding
from three positions per triangle.

Torus precomputes O(rows+columns) trigonometric and normalized-parameter
tables, fills positions/point normals together, and writes surface, curve,
cap, normal, and UV planes in disjoint stable ranges.

Revolve plans compact axis poles and stable per-profile-edge primitive ranges
before allocating the output.

Tube precomputes O(rows+columns) radius, height, trigonometric, and normalized-
parameter tables, shares a single point at a zero-radius end, and emits no
degenerate tip ring or quad. Exact output planes cover side topology, optional
independent cap rims, point/vertex normals, UVs, and cap membership; stable
side-band ranges cook in parallel while cap writes remain bounded. Work and
published output are O(rows*columns), with O(rows+columns+parallel ranges)
auxiliary storage.

Platonic Solids keeps normalized canonical point, face, and face-normal tables
once at module initialization. Each cook allocates only its fixed packed result
and copies the relevant immutable topology; all forms contain at most 60 points
and 180 corners, so domain dispatch would cost more than the complete cook and
the kernel deliberately remains sequential.

Spiral shares one radius/height/angle/trigonometric table across phase copies
and fills exact-sized packed position, topology, and optional attribute planes
in stable ranges. Equal-arc placement integrates analytic speed in a partition
that includes every ramp knot, then performs ordered prefixing and independent
bounded inversions. Cumulative distance uses fixed 4,096-segment blocks. The
frame loop uses specialized float comparisons and range-local scratch; no
point, frame, quaternion, sample, or distance iteration allocates a box.

Attribute Promote materializes at most one packed incidence plane per scalar
component.

Piece promotion assigns deterministic integer/text partitions, constructs one
CSR union of unique ascending source indices, and maps the reduced piece plane
back through disjoint destination ranges. Same-owner identity pieces use a
stable linear CSR build instead of pair sorting; non-identity topology uses
16-bit radix ordering. Incidence-adjusted grain gives large pieces enough work
per task.

Plural pattern promotion builds that topology/piece plan once, then applies
each selected packed payload against it in stable source-attribute order.
Aligned multi-term destination and index capture rewriting is compiled and
preflighted before that plan. Exclusions consume no replacement term, and
later positive matches take precedence; rewrite work is proportional only to
selected attribute-name bytes.

The optional scalar source-index output is fused into the first/last/extremum
pass; its only dense retained addition is the required destination integer
plane.

The production extension keeps tuple provenance shape instead of returning a
single misleading index: float2/3/4 extrema reduce components independently
and publish one fixed-width integer CSR row per destination.

Attribute Blur reuses the topology cache, precomputes optional original-edge
lengths and boundary pins once, and keeps exactly two mutable plane sets for
Jacobi iterations. Each iteration launches stable point ranges once and loops
all selected components inside a range. Neighbor alpha was initially called
through a float-returning closure in the innermost edge/component loop; that
boxed roughly one float per visit.

Attribute Randomize validates/unpacks tuple parameters once, allocates exact
output planes, and fills stable element/component identities through
`Rand.float_at`.

The uniform-volume regression independently checks centered first moments and
the analytic 3D expectation `E[r²] = 3/5`; constrained-volume tests retain that
radial moment while checking cone support and axis bias. Direction/orientation
regressions check unit length and cone containment.

Peak, Mountain, and topology-preserving normal generation share packed
structure-of-arrays deformation planes.

Point Jitter uses the same indexed component stream as the equivalent
Attribute Randomize uniform-add operation, but writes canonical `P` directly.

Edge Collapse uses a 500x400 quad grid with 200,901 points, point/vertex UV and
enumeration payload, point and native-edge groups, sparse disjoint horizontal
edge contractions, degenerate cleanup, and point-normal recomputation.

PolyReduce uses a 420x320 alternating-triangle grid (134,400 points and
267,322 input triangles) with vertex UVs, point/primitive enumeration fields,
a point group, a native boundary-edge group, a surviving-face group, strict
boundary preservation, equal-length weighting, and a 40% polygon target.

Precomputing each normalized face plane once per round removes allocation from
the point-incidence loop. Reusing maximum-capacity packed quadric, face,
cost/incidence, lock, and stamp planes across the seven shrinking contraction
rounds removes the remaining repeated planner buffers; topology/payload output
still allocates new immutable current/next snapshots by design.

The original-position variant disables boundary locking and contracts to the
stable lower-numbered endpoint.

Remesh uses the same 420x320 alternating-triangle grid as PolyReduce: 134,400
points, 267,322 input triangles, vertex UVs, and point/primitive enumeration
payload. One topology iteration selects 133,661 long edges, creates 268,061
points and 534,644 triangles, then contracts a topology-safe independent set
to 225,234 points and 448,990 triangles.

The production path fuses triangle subdivision into one cardinality-planned
output with fixed eight-case topology emission. Point/vertex interpolation,
ordinary and ordered groups, primitive ancestry, and topology-affine native
edges retain the same documented semantics; the all-edge case deliberately
uses the isotropic four-triangle pattern instead of an arbitrary N-gon ear fan.
Edge Collapse now defers native-edge remapping until cleanup has produced the
final topology and composes source-to-cluster and cluster-to-output point maps,
eliminating two reverse-topology indexes and an intermediate edge-group
payload. Projection builds the immutable source BVH once per cook and reuses
exact-sized result planes across iterations.

Intersection Analysis measures point/provenance materialization in addition
to mixed-piece traversal.

Sparse and self modes scale through BVH and disjoint two-pass narrow-phase
ranges.

The mixed index replaces the former triangle-only traversal without building
a redundant triangle surface index.

PolyBevel uses 10,000 disconnected attributed boxes: 80,000 points, 60,000
source polygons, 240,000 source corners, and all 120,000 unique edges selected.
The chamfer case emits a 1,460,000-element cardinality checksum. The divided
round case uses four profile divisions plus point/vertex integer payload,
point-distance scale, and generated groups, and emits 4,700,000 elements.

Point Split uses a 500x400 quad grid with 200,901 points, 200,000 primitives,
and 800,000 corners.

Point Replicate uses 100,000 source points and emits 600,000 points. Sources
carry a count-scale Float, stable integer `id`, `pscale`, `N`, `v`, and a
generic Float3 vector; the selected payload is copied and a generated group is
written. A separate diagnostic
measures the canonical four-point instancing bases (400,000 frame points) and
the Point Generate emission stage so transform compatibility overhead remains
visible rather than hidden in one aggregate number.

Edge Cusp selects every edge of a 300x250 triangle grid carrying point normals,
point UV/ID fields, a vertex float2 field, an ordered-capable point group, and
a native edge group. It expands 75,551 input points to 450,000 fan-separated
points while retaining 450,000 corners.

Edge Straighten measures 100,000 independent three-point bends (300,000
points/vertices and 200,000 selected edges) with point integer payload, a point
group, and output native-edge-group creation. The correctness audit found that
a single-axis power seed can be exactly orthogonal to the dominant covariance
eigenvector.

## Hot-path review checklist

1. Confirm asymptotic complexity and identify the dominant allocation.
2. Check whether output size is knowable and allocate exactly when it is.
3. Remove list/array conversion cycles and per-element option/closure boxes.
4. Hoist invariant transforms, materials, light data, and lookup tables.
5. Partition only sufficiently coarse independent work.
6. Verify sequential/parallel byte-identical ordering.
7. Measure elapsed time, minor/major allocation, and live memory.
8. Run correctness, finite-native, documentation, and diff checks.

## Benchmark commands

Commands as recorded in the log; fixture sizes, per-operation filter names and
per-stage environment are with each transcript there. Geometry benches run in
the release profile and are repeated with `RAYS_BENCH_DOMAINS=4`.

| Area | Command |
|---|---|
| Exact predicates | `RAYS_PREDICATE_BENCH_COUNT=1000000 RAYS_PREDICATE_BENCH_REPEATS=5 dune exec --profile release tools/bench_rdk/bench_rdk.exe -- predicates` |
| Circle from Edges | `RAYS_CIRCLE_EDGE_POINTS=1000000 RAYS_CIRCLE_EDGE_REPEATS=5 RAYS_BENCH_DOMAINS=1 dune exec --profile release tools/bench_rdk/bench_rdk.exe -- circle_from_edges` |
| Graph Color | `RAYS_GRAPH_COLOR_ELEMENTS=1000000 RAYS_GRAPH_COLOR_REPEATS=5 RAYS_BENCH_DOMAINS=1 dune exec --profile release tools/bench_rdk/bench_rdk.exe -- graph_color` |
| Convex Hull | `RAYS_HULL_REPEATS=3 RAYS_BENCH_DOMAINS=1 dune exec --profile release tools/bench_rdk/bench_rdk.exe -- convex_hull` |
| Extract Centroid | `RAYS_CENTROID_SIZE=1000000 RAYS_CENTROID_REPEATS=3 RAYS_BENCH_DOMAINS=1 dune exec --profile release tools/bench_rdk/bench_rdk.exe -- extract_centroid` |
| Extract Point from Curve | `RAYS_EXTRACT_REPEATS=5 RAYS_BENCH_DOMAINS=1 dune exec --profile release tools/bench_rdk/bench_rdk.exe -- extract_point_curve` |
| Cold Scene3 display packing | `dune exec tools/bench_scene3_packing.exe -- 1000000 7` (mesh construction excluded; cold coordinate planes, median/p95 and allocated bytes) |
| Scene3 float64/float32 gallery pixels | `_build/default/tools/check.exe @test/test_scene3_float32_gallery` (native device required; records each scene's maximum channel difference and changed-pixel count) |
| Material assignment | `dune exec tools/bench_material_assign.exe -- 100000 1` |
| Editor held drag and undo | `dune exec tools/bench_rays_editor.exe -- 200 1000 2000` |
| SOP graph scale smoke | `dune build lib/rays_editor/test/test_main.exe`, then `_build/default/lib/rays_editor/test/test_main.exe test_pxui_graph_smoke` |
| Source digest polling | `dune exec tools/bench_source_poll.exe -- sketches/sop_gallery.rays` |
| Named-pane owner lookup | `SDL_VIDEODRIVER=dummy SDL_AUDIODRIVER=dummy dune exec tools/bench_named_owner.exe` |
| Live light contexts | `SDL_VIDEODRIVER=dummy SDL_AUDIODRIVER=dummy dune exec tools/bench_live_lights.exe` |
| Shattered-cube native workflow | `RAYS_SHATTER_FRAMES=1001 /usr/bin/time -p dune exec rays -- sketches/shattered_cube.rays` |
| Test selection timing | `/usr/bin/time -p env SDL_VIDEODRIVER=dummy SDL_AUDIODRIVER=dummy _build/default/tools/check.exe` |

The branch tools accept `auto` (fresh session), `learned` (first cook excluded,
then cache cleared while retaining measured node costs), `forced` (test gate)
or `off` after the repeat count. Report the placement mode and actual fanout
count with the timings: a completely cold session cannot use historical costs.
`caller_allocated_bytes` retains the old calling-domain measurement for
comparison; `program_allocated_bytes` samples whole-program GC counters after a
minor collection outside the timed region, including allocations on workers.
+
## Historical baselines retained from the export migration

These measurements were recorded on Apple M1 at the baseline available in Git
at `62582188`; they are comparison data, not verification of the current
architecture cleanup. Detailed migration logs remain in that commit.

Prepared, warm, one-domain Core evaluation; median of three runs. Scalar
timing covers 200,000 forces. Particle timing covers 600 frames at 800×600,
excluding native events, editor panels and rendering.

| Workload | Before | After | Allocation before | Allocation after |
| --- | ---: | ---: | ---: | ---: |
| Scalar residual | 146.2 ns/evaluation | 54.9 ns/evaluation | 1,912 B | 568 B |
| Particles | 3.192 ms/frame | 0.925 ms/frame | 12,893,656 B/frame | 499,624 B/frame |

Native editor, one domain, 10,000 particles, 800×600 logical points:
20 warm frames and 600 measured frames per layout. Both builds included
the Core repair; the changed variable was the probe compiler callback.

| Layout | Update before | Update after | Allocation before | Allocation after |
| --- | ---: | ---: | ---: | ---: |
| Canvas | 2.058 ms | 2.032 ms | 8.01 MB | 8.01 MB |
| Graph | 9.639 ms | 2.842 ms | 42.65 MB | 8.57 MB |
| Lisp | 3.277 ms | 3.151 ms | 9.15 MB | 9.15 MB |
| Graph and Lisp | 10.589 ms | 3.893 ms | 44.10 MB | 10.03 MB |

Both-panel median frame period was 18.605 → 16.687 ms; p95 was
25.189 → 17.822 ms. This setup did not reproduce the reported decay to
10 FPS. Reproduction commands are `tools/bench_rays_editor.exe --particles-toggle 600`
and `tools/bench_core.exe frames sketches/particles.rays` (optionally `--boxed`).

Native autorelease repairs reduced recorded per-frame footprint retention
from 4.0 KiB to zero; `runtime_autorelease` in `@runtest-native`
guards missing pools. These historical numbers do not replace current
lifecycle or shader performance qualification.
