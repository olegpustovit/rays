# Packed Development Kit

Status: first production-oriented vertical slice implemented. RDK holds only live kernels: the 37 native SOPs' operations, exact predicates, spatial indexes, solvers and Boolean stages. Operations that a Lisp node expresses live in `lib/flow_sop/nodes/*.rays`; their former OCaml kernels are deleted, not kept as oracles.

## Purpose and boundary

`rays.rdk` is Rays's native OCaml compute-geometry layer. It is inspired
by the data-oriented parts of Houdini's HDK, not a source-compatible HDK or an
interpreted VEX clone. Its public values are immutable and target-independent;
builders and kernels use locally owned mutation over packed storage.

```text
sop / examples ──> rdk ──> rdk_boolean ──> rdk_ops
                                  rdk_rays ──> rdk_ops + rays
                                  rdk_ops ──> rdk_geom ──> rdk_core ──> rays_math
rays ──> rays_math
```

RDK must not import Sop, Runtime, SDL3, Metal, or platform code.
The renderer boundary is `Rdk_rays.Rays_mesh`, which converts to and
from `Rays.Mesh.t`. `rdk` and `rdk_core` do not link Rays or the GPU
stack. `Rdk.Color_by_height` accepts normalized RGBA float tuples;
the procedural SOP converts public `Rays_math.Color.t` inputs at its boundary.
`rdk_core` owns packed storage, topology, groups, and geometry; `rdk_geom`
owns exact planar algorithms, indices and intersection queries; `rdk_ops` owns attribute, group,
point-generator, curve and mesh operations, and Boolean stages live in `rdk_boolean`. `Rdk` retains the public
module paths. `Ops` keeps public aliases for extracted mesh kernels:
`Triangulate` owns the deterministic polygon-to-triangle plan and installs its
result through `Topology_remap.preserving_points`. The shared select, attribute,
and group remappers also serve Sort and Delete payload ancestry. `Reverse_faces`
owns primitive winding reversal and corner shifts; it retains a specialized
vertex remap because the generic preserving-points path measured slower on a
90,601-point grid. `Mirror_geometry` owns reflected topology, normals, and
edge-group ancestry. `Resample_curves` owns deterministic curve sampling,
interpolated payload, diagnostics, and edge ancestry. Their checked public
operations remain under `Ops`.

## Geometry model

A geometry detail has four attribute owners: detail, point, vertex, and
primitive. Vertices are primitive corners and reference shared points. Forward
topology is stored as packed integer arrays:

- `vertex_point[vertex]` identifies the referenced point;
- `primitive_offsets[primitive..primitive+1]` selects the primitive's vertices;
- a compact primitive-kind byte plane distinguishes open/closed polylines and
  polygons. A point cloud has points and no primitives.

This representation preserves hard normals and UV seams without duplicating
point positions. Render-oriented triangle expansion happens only at the mesh
bridge. Topology validation is linear in points + vertices + primitives and
requires every point reference and primitive span to be in bounds.

`Topology_index` is the shared reverse-topology layer for modeling kernels. It
builds primitive/corner adjacency, next/previous corners, manifold opposites,
undirected edge incidence, and point-to-corner CSR planes with a specialized
open-addressed integer table. It uses expected O(vertices + edges) time and
O(points + vertices + edges) memory without tuple/list allocation in its edge
insertion loop. Boundary and non-manifold edges remain explicit rather than
being forced into a two-sided manifold representation.

Native edge groups are deliberately separate from point/vertex/primitive
bitset groups, matching the topology-affine nature of HDK edge groups. An
`Edge_group.t` indexes the unique undirected edges of one immutable
`Topology_index.t`; cross-topology installation is rejected instead of
silently reinterpreting bits. The reverse topology is held in a thread-safe
weak cache keyed by physical immutable topology identity, so consecutive SOPs
share it without retaining dead geometry. `Topology_index.create_uncached`
keeps cold construction measurable and explicit.
`Edge_group.remap` maps canonical source endpoints through an explicit
source-to-target point plane, drops deleted/collapsed edges, and unions
many-to-one results. `Topology_remap.edge_groups` shares the index setup and
result propagation used by Sort, Delete, point compaction, and other topology
changes. Point/primitive Sort, Triangulate, Reverse, Delete with
optional compaction, direct point compaction, Merge, Fuse, and Mirror use this
boundary or its exact equivalent. Duplicate and Copy to Points validate their
copy-major topology and replicate edge ordinals directly, avoiding a large
target reverse index. Copy to Points filters target selections by membership
and retains increasing target-point order, including for explicitly ordered
groups. Piece matching keeps that target order and skips unmatched IDs.
Target attribute and group rules use the last matching pattern, including a
`Nothing` rule that preserves the source payload. Arithmetic acts on the
chosen output owner; a missing numeric source starts from the target value
(negated for subtraction), while a missing group copies target membership
for addition/multiplication and remains empty for subtraction.
Subdivide derives its child-edge ordinals directly from
the deterministic quad/triangle refinement plan, so propagated and resulting
crease groups do not retain a full target reverse index;
Clip maps only fragments whose endpoint provenance lies on that source edge;
Poly Extrude maps bottom and every selected-component layer copy; curve
Resample maps intervals;
Edge Divide maps each selected source edge to every resulting child segment;
and Sweep maps source edges to longitudinal edges. Newly generated diagonals,
cuts, caps, verticals, and rings are not mistaken for selected source edges.

Copy-to-Points piece matching accepts a target point integer/text attribute and
prefers a same-named source primitive attribute, then a source point attribute.
Absent source attributes permit only integer primitive-number selection.
Primitive pieces compact referenced points; point pieces retain every matching
point, including free points, and include only primitives whose corners all
reference that same piece. Dense piece IDs, primitive/vertex maps, and target
partitions are planned in shared passes. Unique piece batches reuse the uniform
copy kernel, then one packed point and primitive permutation restores stable
target-major order. Attribute/group payload, ordered memberships, detail facts,
and native edge groups follow the same maps. For source size S, target size T,
and expanded output O, expected work is O(S log S + T + O) in the current
partition implementation; auxiliary storage is O(S + T + O), excluding the
immutable result. Fixed piece count and already coherent point pieces reduce
planning to linear work. Unmatched targets contribute no output, cancellation
is atomic, output cardinalities are checked before batch cooking, and domain
scheduling cannot affect element order.

Poly Extrude retains its exact allocation-tight individual-face compatibility
path. Its general planner accepts a primitive group, splits connected regions
at topology-affine edge groups, assigns stable component IDs in source-
primitive order, and creates one `(component, point)` association per shared
front point. Selected-face normals are accumulated in stable corner order and
normalized once per association; straight division layers are then filled into
exact-sized packed position/topology arrays. Only selected-region boundary
corners create side quads. Unselected polygons and curves pass through, while
selecting a curve or touching a non-manifold edge in connected mode fails
atomically. Point, vertex, primitive, and detail attributes and ordinary groups
remap by stable ancestry; stale point/vertex `N` is removed. Source native edge
groups map to the retained bottom and every generated layer, and optional
front/back boundary groups are native edges. The edge-output path constructs a
specialized endpoint table in target edge-number order instead of retaining a
complete target reverse-topology index.

For fixed division count, the planner is expected O(points + vertices +
primitives + edges + output payload) time and O(selected topology + output)
auxiliary memory. Division work is linear in the generated side rows and front
points. Face normals, positions, topology spans, attributes, groups, and edge
table preparation use disjoint stable ranges where profitable; component union
and association numbering remain sequential so one-domain and multi-domain
outputs have identical point/primitive ordering.

Poly Fill discovers one-sided polygon boundary components through the shared
topology index. With no explicit edge group every valid hole participates; a
native edge group may name one edge or several edges in a hole and the planner
auto-completes the entire component. Components are ordered by their smallest
stable source edge. Every selected component must have exactly one incoming
and one outgoing boundary half-edge at each point, so branches, open chains,
point-touching loops, and inconsistent source winding fail before output is
allocated. A supplied non-boundary or curve edge is also an error rather than
an implicit topology guess.

Single Polygon emits one reversed-boundary N-gon. Triangles use deterministic
dominant-plane ear clipping and preserve concave contours. Triangle Fan adds
one scale-safe arithmetic-mean center and one triangle per edge. Patches oppose
the source boundary half-edges by default, producing a consistently oriented
two-sided manifold; explicit reversal is a deliberate modeling control.
Boundary points remain shared unless unique patch points are requested.
Existing topology is a byte-exact prefix. Generated boundary points and
corners copy stable point/vertex ancestry, numeric fan-center attributes use a
scale-safe mean, discrete/array centers select the first stable boundary
ancestor, and patch primitive fields select an adjacent source primitive.
Existing ordinary primitive/vertex groups do not acquire generated elements;
duplicated boundary points retain point-group ancestry, an optional patch
primitive group marks the new faces, and source native edge groups remain on
their source edges.

The edge-group path does not construct an unrelated target point/corner CSR.
Because source topology is an unchanged prefix, every source edge ordinal is
also a target prefix ordinal. Single shared patches add no edge; fan spokes and
all unique-point edges have cardinality known directly; shared triangulation
checks only its stable ear diagonals against the source endpoint table. The
resulting exact target edge count is enough to extend source packed bits.
Boundary planning plus N-gon/fan output is O(points + vertices + primitives +
edges + output payload); ear clipping is O(sum(loop_size squared)). Auxiliary
storage is linear. Candidate classification, loop validation/triangulation,
center computation, topology fills, attribute interpolation, and packed group
passes use deterministic disjoint ranges where profitable.

Ordinary `Group.t` values retain packed membership and may additionally carry
an explicit element-order plane. `Group.iter` deliberately keeps its legacy
increasing-index contract; `Group.iter_ordered` observes the explicit sequence
when present. Ordered construction rejects duplicate or out-of-range elements
and public order access returns a defensive copy. Union and xor retain the
left sequence followed by unseen right members, intersection follows the left
explicit sequence (or the right when only it is ordered), and difference
filters the left sequence. Complement becomes unordered because newly selected
elements have no source sequence. Sort, deletion, duplication, curve topology
edits, subdivision, clipping, and the shared ancestry remappers preserve order
without changing packed membership behavior; newly derived elements with no
single ancestor append in stable target order.

`Spatial_index` is the shared balanced point-query layer. It stores one
deterministic median-partitioned source-index plane, orders equal distances by
source index, and supports allocation-free bulk nearest/k-nearest traversal
into fixed-width disjoint result slices. Attribute Transfer builds this plan
once and reuses its source IDs/distances for every selected typed attribute;
it never repeats an all-pairs search per attribute. Optional packed point
groups restrict indexed sources and queried targets while preserving original
source IDs and untouched destination values. Construction ranks globally
non-flat axes by span, omits flat dimensions from its split cycle, and builds
disjoint subtrees through the reusable domain pool above a tunable grain. This
avoids pathological planar-grid traversal through constant coordinates while
preserving exact query-result ordering.

Point and primitive-barycenter Attribute Transfer may use nearest,
inverse-distance, or compact Links, RenderMan, and Hart source weighting. The
three compact formulas match the exact expressions published by SideFX; a
finite non-negative radius is independent of the destination distance
threshold, and the positive maximum sample count bounds k-nearest query
storage. A zero radius or a row with no supported candidates falls back to the
stable closest source. Numeric fields share one normalized coefficient table;
the query distance table is rewritten in place only after blend-band influence
has consumed its closest distance, avoiding another O(targets * samples)
plane. Discrete fields retain the stable closest source.

`Surface_index` is the corresponding packed polygon-query layer. It validates
finite non-degenerate simple polygon contours, deterministically ear-clips
N-gons into an internal triangle plane without materializing replacement
geometry, and retains every original primitive and corner identity. Optional
primitive and vertex bitsets intersect the eligible surface. Vertex selection
uses an explicit all-corners or any-corner rule on each emitted triangle;
primitives with no selected corner are skipped before validation, and an empty
eligible surface is a valid index whose queries all miss. A packed
median-split AABB hierarchy emits closest primitive/triangle IDs, squared
distances, and barycentric coordinates into disjoint fixed-size planes.
Triangulation, validation/bounds preparation, hierarchy subtrees, and query
ranges parallelize at explicit grains. Equal-distance ties select the lower
original primitive and then the lower stable internal triangle. Ear clipping
is O(sum(c squared)) over polygon corner counts; BVH construction is O(t log t)
time/O(t) storage and ordinary queries are expected O(log t), while overlapping
bounds retain the documented O(t) worst case.

Every immutable snapshot carries a process-local geometry identity, a topology
data ID, and data IDs for changed attributes and groups. These IDs are cache
invalidation facts, not serialized content hashes. Published buffers must not
be mutated or aliased by callers.

Clean is an explicit ordered composition over the same packed geometry core,
not an alternate repair representation. Optional NaN-point removal precedes
point consolidation so the Fuse spatial index never receives an invalid
coordinate. Consolidation precedes degeneracy classification because welding
may collapse faces or curves. Overlap deletion and winding reversal operate on
the surviving topology, followed by optional point compaction and atomic
attribute/group metadata cleanup. Every intermediate value is immutable; a
later validation or cancellation failure cannot expose a partially cleaned
snapshot.

Polygon degeneracy compares vector area against the square of the user's
edge-length tolerance. The common triangle path computes directly in packed
ranges; finite arithmetic overflow falls back to coordinate-normalized cross
products, preserving decisions through extreme finite magnitudes. General
polygons use the same signed vector-area rule, and polygon curves compare
robust accumulated edge length directly. Point-identical polygon overlaps are
canonical under cyclic rotation and reversed winding. Triangles use a sorted
three-index signature; N-gons use linear-time minimal cyclic rotations and a
specialized open-address table. Stable source order selects the retained
representative. Expected time is O(points + vertices + primitives + output)
for bounded primitive degree, with O(points + primitives + output) temporary
storage. Classification/signature preparation is parallel; stable overlap
insertion and deletion planning remain sequential so domain count cannot alter
ancestry or diagnostics.

## Attributes and groups

Attributes are resolved by name, owner, storage, tuple width, and semantic role
before a hot loop. Numeric data uses packed component arrays; a point is not an
allocated OCaml record. `Attribute_pattern` compiles whitespace-separated
full-name globs once. Blank/`*`, leading-exclusion defaults, left-to-right
include/exclude overrides, `*`, `?`, byte classes/ranges, negated classes, and
backslash escapes have structured validation. Matching is allocation-free and
plural operators retain the geometry's stable attribute order. The initial
storage types cover integers, floats, and
two/three/four-component float tuples. Standard semantics include `P`
(position), `N` (normal), `Cd` (color), `uv`, `v`, `id`, `pscale`, `scale`,
`orient`, and `up`.

Attribute lifecycle has exact-name compatibility operations and atomic batch
operations. `Attribute_ops.delete` accepts independent point, vertex,
primitive, and detail patterns, a delete-non-selected keep mode, and optional
reference geometry whose owner-matched names act as leading inclusions before
the explicit pattern. `Attribute_ops.rename` applies ordered owner-specific or
all-owner wildcard-capture rules; earlier outputs may match later rules and
destination conflicts explicitly skip, error, or overwrite.
`Attribute_ops.swap` applies ordered owner-specific Copy, Move, and Swap pairs
with aligned wildcard captures across every packed storage kind. Missing-side
Swap copies the side that exists. Canonical point `P` participates as float3:
moving `P` is Copy, while moving another float3 into `P` consumes its source.
All three operations rebuild attribute metadata no more than once, share
packed payloads, retain exact geometry identity on a no-op, and fail or cancel
without publishing partial metadata. Delete is O(attributes + reference
attributes) for a fixed pattern; rename and swap are O(attributes × rules),
with O(attributes + generated attributes) auxiliary metadata.

Attribute Copy is the topology-correspondence path for source and destination
geometry that do not require a spatial search. A point, vertex, or primitive
group supplies ordered source/destination element pairs; cyclic matching wraps
the shorter source selection, integer/text value matching selects the highest
matching source element, and destination integer indices provide explicit
source references. Attribute ownership is independent of group ownership, so a
single correspondence projects deterministically through incident points,
corners, and primitives and is reused by all copied fields of that owner.
Compiled include/exclude patterns and one-glob rewrites select all packed
storage kinds; canonical `P` is explicit opt-in. Identity, ungrouped,
same-cardinality plans structurally share source planes in O(attributes)
metadata work. General plans take O(group elements + projected incidence +
copied payload) time and one destination-sized integer map per selected owner;
conflict-free projections and payload planes fill disjoint ranges in parallel.

Attribute Combine resolves an ordered stack of numeric layers before entering
its hot loop. Every point, vertex, primitive, or detail element independently
reads sources and blend masks from the primary or additional geometry inputs,
using direct element numbers or a cached integer/text correspondence. Duplicate
match values select the highest source element; unmatched sources are zero and
unmatched blend elements have zero influence. Copy, add, subtract, multiply,
zero-safe divide, minimum, and maximum share source scale/add and
reciprocal/clamp/complement/threshold preprocessing. Scalar destinations read
tuple length, scalar sources replicate into tuples, and tuple sources truncate
or zero-extend. Existing destination kind is retained, checked integer
conversion preserves unselected full-width values exactly, and canonical point
`P` participates as float3.

The complete layer stack, clamped per-element blending, overall scale,
component threshold, and component clamps run in one fused traversal over the
typed selection. Output storage is allocated once; no layer-sized temporary is
materialized. Missing-destination inference, scalar creation, missing-attribute
policy, and primary-input source cleanup publish atomically with stable
metadata order. For [n] output elements, [l] layers, and tuple width [w <= 4],
work is O(n*l*w). Auxiliary storage is O(n*w), plus one O(n) output mapping and
one bounded-load packed open-address table per referenced matched input.
Selected elements write disjoint output slots, so one- and multi-domain values
and ordering are exact.

Attribute Interpolate supports both persistent primitive coordinates and
explicit weighted element rows. In primitive mode, destination point, vertex,
primitive, or detail elements carry an integer source primitive number and
float3 source UVW; one fused cook samples explicit mixed-owner point, vertex,
primitive, and detail fields. Triangle barycentrics, Houdini's
bilinear quad orientation, the implicit n-gon center fan, and open polygon-curve
parameterization follow SideFX primitive space. RDK closed polygon curves wrap
the same piecewise-linear boundary parameter. Numeric point/vertex fields use
the corner weights, primitive/detail fields are constant over the referenced
primitive, and integer/text fields choose the greatest-weight corner with
stable local-corner ties. Canonical point `P` and standard float3 `N` are
handled explicitly; `N` is normalized after a nonzero destination blend.

The alternative drivers are immutable CSR integer-number and float-weight
attributes with identical row offsets. Point-number rows accept point/detail
fields, vertex-number rows resolve point/vertex/primitive/detail fields, and
primitive-number rows accept primitive/detail fields. Numeric fields use the
weighted sum; detail numeric values are therefore multiplied by the row sum.
Integer/text fields choose the greatest coefficient with stable row-order
ties. Integer-array and float-array fields use the same stable greatest-
coefficient rule to copy one whole opaque CSR row. RDK does not perform
element-wise arithmetic across variable-length rows: destination blend below
one half retains the existing row, blend at or above one half selects the new
row, and the keep/default miss policy retains/clears it. Array output plans one
source/existing row choice per destination, computes exact packed cardinality,
and copies disjoint value spans in parallel. Optional normalization divides a
nonzero row by its signed sum and uses
the pre-normalization magnitude with the positive threshold to ramp existing
versus interpolated influence. A negative total is divided by that signed
total, yielding a positive unit sum without discarding algebraic coefficient
signs. A signed sum whose magnitude is at most `1e-20` has zero new influence:
existing numeric, discrete, and group values are preserved. Pre-scale is
applied before normalization.
Empty rows use the explicit keep/default miss policy; malformed layouts,
non-finite weights, and out-of-range source numbers fail before publication.

Primitive mode may atomically emit equivalent point- or vertex-number/weight
CSR attributes. Selected valid destinations receive one row per primitive
corner, selected misses follow the miss policy, and unselected/existing rows
are preserved as pairs. The computed rows can immediately drive the matching
weighted mode with exact numeric and discrete results.

Fields may be listed explicitly or expanded from independent compiled
point/vertex/primitive/detail include/exclude patterns in stable source metadata
order. Point patterns can include canonical `P`; duplicate destinations across
explicit and expanded fields fail before cooking.
With group matching enabled, the same point/vertex/primitive patterns select
ordinary source groups. Membership is evaluated as a weighted 0/1 field,
blended with existing destination membership, and thresholded inclusively at
one half after the destination blend. Same-name groups from different source
owners are rejected because
they would collide in the single destination-owner namespace. Output remains
packed: workers own complete destination bytes, so no domain performs a racing
read-modify-write and no byte-per-element boolean plane is allocated.

All selected fields share one topology traversal. Jobs are resolved once into
point/vertex/primitive/detail and numeric/discrete planes, primitive kind is
loaded once per destination, packed output planes are allocated exactly once,
and disjoint destination elements fill in parallel. Invalid primitive numbers
have an explicit keep/default policy. Non-finite coordinates, wrong driver
storage, incompatible destinations, duplicate outputs, and cancellation fail
without publishing a partial snapshot. Positions and ordinary metadata commit
atomically. For `d` destinations, `a` fields, `c` referenced corners or
weighted elements, and `r` copied array values, work is O(d*c*a + r), field
output is O(d*a + r), and auxiliary job metadata is O(a) plus one O(d)
row-choice plane per array field. Computed CSR output adds O(d + total
referenced corners) storage. Structural geometry operations preserve, remap,
concatenate, or select CSR rows directly; numeric operations without a defined
array policy reject them instead of silently dropping data.

Groups use packed bitsets and deterministic increasing-index iteration. Union,
intersection, difference, and symmetric difference are linear in the bitset
word count. Selections and ranges must remain packed interval/bitset values;
hot traversal must not materialize lists or per-element options.

Group Promote resolves one named source before entering its hot loop and uses
the shared reverse topology to convert all point, corner, primitive, and native
edge owner pairs. Touching, complete-containment, and primitive shared-edge
tests scan packed incidence directly without allocating closures per element.
Output bytes are disjoint parallel ranges; same-owner promotion shares the
immutable source bits. Ordinary outputs may instead be emitted directly as one
exact owner-sized 0/1 integer attribute; native edges reject attributes. Group
Promotions compiles include/exclude group-name patterns and aligned wildcard
capture substitutions once per ordered rule. Every rule snapshots its matching
source entries before publishing output, so an output that overwrites another
pending source name cannot change that source's promotion. Later rules observe
earlier groups and may re-promote them. Ordinary and boundary rules share one
cook-local group store and publish group metadata once; integer attributes are
merged once after the group commit. Blank patterns are disabled, unmatched
patterns preserve physical geometry identity, and output count/generated
payload are preflight-bounded before each allocation. Rule order is semantic
and serial, while every matched topology and packed-output pass retains the
reusable parallel kernel.

Group Promote Boundary is the typed boundary-only composition: it borrows the packed
source membership as a virtual integer plane, unions selection discontinuities
with optional point/vertex/primitive attribute seams and requested
polygon/curve unshared edges, promotes those boundary edges, then intersects
with ordinary conversion so elements on the opposite side cannot leak into the
result. It supports every source/destination owner pair, ordinary integer-mask
output, and optional primitive expansion to selected faces sharing any boundary
point. Group Expand uses the same adjacency planes. Fixed
signed steps alternate two packed bitsets and stop when membership stabilizes;
flood fill uses one exact-size integer queue, marking elements as they enter it,
so each element is queued at most once. Empty, full, and zero-step selections
share their existing immutable membership storage. Optional step output is one
exact owner-sized integer plane: base/unreached elements are zero, fixed-step
changes record their first positive iteration, and flood traversal records the
minimum multi-source BFS distance. Native edges reject step output because RDK
does not invent an edge-attribute owner. The constrained point and
edge-connected-primitive paths compile point/vertex/primitive attribute
discontinuities through the same typed boundary kernel used by Group Promote
Boundary and Group Range. Collision groups are independently typed, converted
once to native seam bits, and optionally promoted once to a containment mask
and boundary mask. Growth checks candidate policy and seam crossing before
publishing membership; shrinking treats compiled seams as forced first-step
boundaries. Normal-limited growth compares adjacent target-owner unit vectors.
Geometric normals come from the shared normal kernel; custom point, vertex, or
primitive float3 values map to the target through normalized incident-corner
means. The three mapped normal planes double as disjoint per-target
accumulators, avoiding per-element heap boxes. Zero or non-finite input is
handled explicitly: non-finite values fail atomically and zero mapped vectors
cannot cross a normal-constrained transition. Constraint compilation and
validation are O(elements + incidence + selected attribute payload), fixed
steps are O(abs(steps) * incidence), and flood remains O(elements + incidence).
Vertex/edge constrained targets and seam-constrained shared-point primitive
growth are rejected until a precise topology-boundary contract is available.

Named-group catalog operations share one cook-local mutable name table and
publish one immutable geometry snapshot. Group Range classifies directly into
packed bytes over absolute, relative, length, or balanced-partition bounds;
base patterns are resolved once and periodic filtering performs only integer
arithmetic in the element loop. Its optional connected point/primitive mode
reuses the single connectivity classifier, assigns stable local indices in
ascending element order, and evaluates both range and periodic phase
independently inside every component. Compiled same-owner attribute patterns
reuse Group from Attribute Boundary's zero-copy numeric/discrete planes and
finite float-tolerance checks. Independently typed collision group patterns
become native seam bits: native edges are borrowed directly, while ordinary
point/vertex/primitive membership discontinuities are classified once.
Attribute and collision seams are unioned before the flood. With a non-empty
seam plane, point components omit seam edges and primitive components use
shared non-seam edge incidence; the no-seam path retains shared-point primitive
compatibility. Collision-boundary elements can be excluded from local indexing
through one packed promoted include mask or retained explicitly.

A stable component ID can restrict the pattern to one region. Remove Other
Regions clears every other component; disabling it passes only their existing
base membership through unchanged. The legacy [Range_disconnected] constructor
retains its strict remove-other behavior. The global path retains only its
packed output; connected mode retains component/local-index planes, two
component-bound planes, and configured packed boundary/include masks until the
output is published. The classifier reuses its union-find parent and rank
storage as the stable class output rather than allocating separate root and
class maps.
`group_ranges` exposes Houdini's Number of Ranges multiparm as ordered typed
rules. Blank output names are disabled slots. Later rules deliberately observe
groups published by earlier rules, enabling base, collision, and same-name
merge chains. The functional boundary is atomic to its caller: failure or
cancellation returns no intermediate snapshot, while an empty/all-disabled
batch preserves physical input identity. Rule-level execution stays ordered;
each rule retains the same packed parallel validation, boundary, and output
passes as `group_range`.
Group Combine and Invert operate on bitset bytes rather than materializing
member indices. Group Rename and Delete change only metadata and preserve exact
geometry identity when no rule changes it.

Group Copy builds one target-to-source map per owner rule, then reuses it for
every selected source group. Index matching needs no dense map for points,
primitives, or edges. Default vertex matching uses primitive/local-corner
coordinates. Integer/text attribute matching uses a compact open-addressed
table whose slots store source indices only; the key remains in the borrowed
source attribute plane. Duplicate keys retain the first source index. Output
groups are exact packed target-sized bitsets, group-name conflicts are resolved
before their fill, and target metadata is rebuilt once.

## Construction and execution

Known topology is cardinality-first: allocate exact arrays once, then fill by
index. A builder is single-domain owned. Parallel work may fill only
preallocated disjoint ranges and may not grow topology or mutate the attribute
schema concurrently.

`Rdk.Geometry.generate_point_ranges` is the expert VEX-like surface. Bindings are validated once, then
the same native OCaml loop runs through stable chunks using the process-wide
`Parallel` pool. It never creates a pool per operation, task, or frame.
Topology mutation is not permitted inside an element kernel. Reductions use a
deterministic two-phase design and combine partial values in stable chunk order.

The convenience API may expose callbacks returning tuples. Such callbacks are
not described as allocation-free unless benchmark evidence proves it. The fast
path exposes packed arrays and chunk bounds so inner loops allocate neither
element objects nor vector tuples.

## Rays mesh bridge

The bridge owns polygon triangulation and conventional attribute mapping. When
the topology and packed attribute layout already match Rays's triangle mesh
layout, immutable shared construction through
`Mesh.Private.create_packed_shared` avoids repacking. Otherwise the bridge
computes the exact output cardinality, allocates once, and expands
point/vertex attributes in deterministic primitive order.

The bridge returns structured errors for invalid topology, missing or
incompatible attributes, unsupported primitives, numeric failures, and
cancellation. It does not render, load SDL resources, or inspect the selected
render target.

Public RDK operations use `Rdk.Error.t` with a stable operation, code, message,
and optional corrective hints. Long-running operators accept `Rdk.Cancel.t`; polling is
outside arithmetic inner loops or amortized over fixed blocks. Sop cook
contexts share this token directly, so cancellation cannot publish a partial
snapshot.

## Implemented operator surface

The eager layer includes packed point/line/polyline/circle/grid/box/sphere sources;
raw-matrix and typed selected Transform, whole-geometry Match Axis, stable point/primitive Sort,
attribute-driven point/primitive blast, merge,
reverse, triangulate, normals, clean, primitive deletion,
noise displacement, height color, surface scatter, copy to points with the
standard quaternion, normal/velocity-up, post-rotation, pivot, translation,
and scale stack plus packed affine transform overrides and typed source/target
restriction, with last-match-wins packed target-attribute broadcast,
`Nothing` overrides, and target-group boolean algebra,
arbitrary-transform instance materialization, materialized
cumulative/non-cumulative Duplicate, individual-face poly
extrusion, arc-length curve resampling, and circular sweep.

Reverse applies either a winding reversal or a signed cyclic corner shift to
all polygon/curve primitives or a typed primitive group. The same permutation
is applied to fixed-width and ragged vertex attributes, ordinary and ordered
vertex groups, and native edge groups. Point, primitive, and detail planes are
shared. Winding reversal removes stale point/vertex `N`; a cyclic shift
preserves and remaps vertex `N` because it does not change the surface. Empty
selections and shifts that wrap every selected primitive to its original
corner order preserve geometry identity. The kernel validates before commit,
supports cancellation, and fills topology and fixed-width vertex payload in
stable disjoint ranges. Work and owned output are O(vertices + vertex payload),
with no reverse-incidence index required. Houdini's separate U/V hull and
surface controls do not apply to RDK's polygon/curve primitive model.

Transform composes six explicit scale/shear, Euler-rotation, and translation
orders and all six radian Euler orders into one affine matrix. A translated and
rotated pivot frame wraps that matrix, and inverse mode rejects singular
output before cooking. Point, vertex, primitive, and topology-affine edge
selections reuse `Element_selection` promotion to one packed point mask;
positions and attached point/vertex `N` planes then fill disjoint ranges.
Normals use the inverse transpose and may normalize, preserve their original
length, or be rebuilt for every pre-existing normal owner. Singular arbitrary
matrices remove normal planes rather than publishing invalid directions.
Identity and empty selections preserve the immutable geometry object.
Validation/promotion is O(points + selected incidence), while position and
normal work is O(points + affected normal elements) with three exact output
planes per changed float3 field and no per-element heap allocation.

Soft Transform reuses the same selected-point promotion and matrix convention,
then blends each point between its original and fully transformed position.
The Radius metric builds the shared packed point index only over source points
and performs allocation-free parallel nearest queries. Edge distance performs
an exact multi-source Dijkstra traversal over `Topology_index` using geometric
edge lengths. Its fixed point-sized indexed decrease-key heap queues each point
at most once, gives stable point-number ties, and avoids duplicate relaxation
entries. Attribute mode consumes a finite point-float plane either as raw
distance or as a direct extrapolating weight. Linear, quadratic, and cubic
rolloff plus an optional exact output weight plane are common to all modes.
Complete geometric selections bypass distance construction with a constant-one
plane. Position/falloff fills are parallel; edge queue order is deliberately
serial and deterministic. Radius work is expected O(selected log selected +
points log selected); edge work is O((points + edges) log points), bounded by
the radius, with O(points) distance/heap storage. Existing normals are rebuilt
by default or invalidated explicitly.

Distance Along Geometry promotes independent typed start and affected
selections through the same `Element_selection` boundary and publishes the
edge-path kernel as reusable point-float data. Raw distance uses `-1` for a
point unreachable from every start. Existing output values outside the
affected group survive exactly; a missing distance or mask plane starts with
`-1` or zero respectively. Masks use linear, quadratic, or cubic falloff over
either a finite positive fixed radius or the maximum finite distance among
affected reachable points. A zero maximum maps reachable zero-distance starts
to one and disconnected points to zero.

When raw distance or maximum normalization is requested, stable Dijkstra visits
the complete reachable component. A fixed-radius mask-only cook passes that
radius into the queue and never visits farther points. Start-complete and empty
start sets bypass topology-index construction. The queue is serial and stable;
affected maximum reduction is deterministic, while output planes fill in
parallel disjoint ranges and commit together. Complexity is O((visited points
+ visited edges) log points) time and O(points) scratch, plus exactly one
point-sized array per enabled output. Surface and heat geodesics require
separate robust metric kernels and are not approximated by edge distance.

Distance From Geometry uses the same output and falloff policy across two
reference feature families. Point mode promotes the optional reference
selection to points and queries `Spatial_index`; primitive mode promotes it to
primitives and queries the triangulated polygon `Surface_index`. The affected
selection is independently promoted on the source input. Empty reference sets
produce raw `-1` and mask zero. Existing output values outside the affected
set survive, and both output planes commit atomically.

Each index now exposes a distance-only packed batch query. It writes one
squared-distance plane in stable parallel ranges and retains infinity for
bounded misses, without allocating point IDs, counts, primitive IDs, triangle
IDs, or barycentric planes. Fixed-radius mask-only cooks bound traversal before
the square-root/materialization pass; raw distance and maximum-radius masks
query the complete index. Point-index construction is expected O(r log r) and
queries O(q log r); polygon BVH construction is O(t log t) and ordinary queries
are expected O(q log t), with O(r + q) or O(t + q) owned storage. Degenerate
overlapping BVH bounds can degrade surface queries to O(q*t). Signed
inside/outside distance is deliberately absent until RDK has a robust closed-
solid classification contract; it is not inferred from polygon winding alone.

Distance From Target is the analytic member of the same field family. It
measures each affected point from an origin, an infinite axis, or an infinite
plane. Cylindrical and planar directions must be finite and non-zero and are
normalized once per operation. Planar raw values may be signed, positive along
the supplied normal; spherical and cylindrical signed requests are rejected
rather than given an ambiguous convention. Masks always consume magnitude.

The kernel validates every parameter and source position before publishing an
attribute, preserves pre-existing values outside the affected selection, and
commits raw and mask planes together. Fixed-radius mask-only cooks write the
mask directly without a distance scratch plane. Maximum-radius mask-only cooks
use one point-sized temporary plane; if raw distance is enabled that output
plane doubles as reduction input. Work is O(points), auxiliary storage is
O(points) only for maximum mask-only cooking, and all fills use deterministic
disjoint Parallel ranges.

Triangulate deterministically ear-clips selected simple polygons after a robust
dominant-axis projection. A typed primitive group can restrict the operation;
unselected polygons and curves keep exact topology and payload, while selecting
a curve is a structured error. Point data stays shared. Stable source maps
replicate/remap every fixed-width or ragged vertex/primitive field, ordinary or
ordered group, and native source edge without marking generated diagonals.
Empty and already-triangular selections preserve geometry identity. Planning
computes exact output cardinality before allocation; independent primitive
blocks own their clipping scratch and fill disjoint output ranges. Complexity
is O(primitives + sum selected corner_count² + output payload) time and
O(primitives + output vertices) auxiliary/output storage. Constrained Delaunay
point-set triangulation is a separate future operator, not an alternate hidden
mode of this topology-preserving kernel.

Normals uses one packed kernel for modeling operations, deformation
recomputation, and the public SOP. It emits point, vertex, primitive, or detail
float3 fields using face-area, equal-corner, or vertex-angle weighting. Vertex
output supports a radian cusp angle; the fully smooth case accumulates once per
point and copies to corners, while cusped output precomputes each corner angle
once and evaluates stable incident fans in parallel. Typed point, vertex,
primitive, and native-edge selections are promoted to the requested owner.
Existing output fields remain exact outside the selection; missing vertex
fields receive Houdini-style smooth initialization, while missing fields on
other owners compute over the full relevant owner. Zero results can preserve
an existing value, and computed values can be reversed.

Point-incidence CSR is cached independently from edge incidence so normals do
not construct unused edge hash/half-edge planes. `Topology_index` reuses those
same point arrays when a later edge operation needs the full reverse topology;
there is no second authoritative incidence representation. Point, primitive,
detail, and fully smooth vertex modes are topology-linear. Cusped vertex mode
is O(vertices + sum point_degree²), uses O(points + vertices + primitives)
packed storage, and writes each output corner independently.

The Line source
robustly normalizes a finite direction and fills exact evenly-spaced polygon-
curve or free-point output in parallel. Convert Line emits one canonical
two-point curve per selected unique topology edge, optionally computes
primitive length, compacts unused points, and preserves native edge provenance
while intentionally discarding source corner/face payloads. Its Connect Path
mode invokes the packed PolyPath graph directly—without first materializing
two-corner edge primitives—and exposes finite endpoint distance, endpoint-only,
and isolated-loop closure controls. Length in this mode is the robust total
arc length of each final path.
PolyPath instead cleans the complete unique-edge graph directly: it removes
self/duplicate edges, traces maximal paths through degree-two points, stops at
endpoints and branches, and optionally rewires transitive nearby endpoint
components onto the lowest point number before tracing. Isolated loops are
open curves with a repeated endpoint by default or closed polygon surfaces
explicitly. Point/detail data
retains identity, vertex data follows a stable source corner, primitive fields
reduce to the lowest contributing primitive, and primitive/native-edge groups
use union ancestry. The normal no-rewire path borrows the cached topology
index's unique endpoint planes; it does not allocate or hash a second graph.
Curve Carve extracts normalized arc-length or uniform-edge intervals from an
optional primitive group with packed payload interpolation and exact
source-edge provenance. Unselected polygons and curves preserve their point
sharing, kinds, and payload. Keep Outside emits two parameter-ordered pieces
for open curves but one seam-crossing complement for closed curves; keeping
inside and outside emits all cut pieces with duplicated cut endpoints and
complete primitive/group ancestry. Away from breakpoint mode, a positive Cut
division count partitions the retained inside interval into equal,
parameter-ordered open pieces in one cardinality-planned cook; the default one
preserves the unsplit result. Primitive float attributes can replace or
scale First/Second U independently; only selected values are required to be
finite and ordered. Polygon breakpoint mode moves the interval inward to source
vertices and either retains the two outer breakpoints or cuts/extracts every
vertex within the interval. Its exact-copy path preserves payload and native
edge ancestry without interpolation scratch. Point-extraction mode otherwise
emits an exact number of free points over an inclusive parameter interval,
optionally retaining the selected source curves; point fields and groups follow
the same interpolation policy while removed vertex/primitive payload becomes
correctly empty. Ends changes U closure for selected polygon faces and polygon
curves. Open drops the closing segment; straight close emits a polygon face and
removes a repeated shared terminal corner on round-trip. Shared-seam unroll
repeats the first point reference, while new-seam unroll duplicates the first
position and every fixed/ragged point payload and ordered-group row. Vertex
payload follows exact corner ancestry. Primitive/detail data remains shared,
original closing edges retain native-edge membership, and authored straight
closing edges remain unselected. Unique-corner topology uses primitive-local
edge numbering; shared topology uses one lightweight target endpoint table and
the cached authoritative source reverse index. The curve-only `curve_ends`
entry point delegates to the same kernel. Curve
Join builds either stable input-ordered chains or a globally greedy nearest-end
sequence anchored at the first selected curve, with closest-end orientation,
finite endpoint welding, connected-only partitioning, wrapping, and explicit
generated-edge semantics. A positive subgroup size starts a new result after
each N selected curves. Keep Originals retains source primitives in stable
order and appends joined chains that share the immutable source point plane;
the general topology-index path preserves native-edge ancestry even where a
welded joined corner substitutes a coincident source point. Global ordering
uses a removable balanced endpoint
k-d tree with widest-extent splitting and stable primitive/end ties rather than
an all-pairs scan. Its dependent greedy walk is sequential; endpoint
preparation and topology/payload materialization retain disjoint parallel
ranges.

PolyLoft consumes selected polygon curves or polygon faces as an authored
sequence of cross-sections and constructs triangles without creating or
duplicating points. Each neighboring pair advances one side of a deterministic
zipper according to either the next cross-edge distance or a three-edge
triangle-perimeter objective. Open sections may reverse onto their closest
endpoint pair. Closed sections choose a closest seam through a direct search
for small products or a packed spatial index for large products, then choose
the destination orientation from the adjacent distances. An equal-point-count
rest snapshot can own those geometric pairing decisions while the current
positions remain the rendered output. U wrap closes authored open sections;
V wrap connects the final section back to the first.

Generated corners inherit exact source-section corner ancestry and generated
faces inherit the preceding source section. Unselected primitives remain in
stable order; selected cross-sections are removed by default or retained as a
prefix. The shared topology remapper preserves every packed attribute,
ordinary/ordered group, and endpoint-affine native edge group. A generated-face
group is optional. Stale point/vertex normals are removed and an existing
normal owner may be rebuilt. Zero collinearity tolerance preserves every
triangle whose point numbers are distinct, so metric roundoff cannot change
topology; a positive dimensionless sine threshold is explicit approximate
filtering policy.

Pair planning and filling are O(output triangles); small closest-seam searches
are O(a*b), while large closed pairs are expected O((a+b) log b) with linear
scratch. Exact cardinalities allocate topology and ancestry once. Independent
section pairs, retained topology copies, remapped payload, and normal fills use
stable disjoint domain ranges. The hot triangle loop uses typed `Float.max`
rather than polymorphic extrema and allocates no per-triangle boxes.

Skin is a second public operation over this same packed loft kernel, not a
second topology implementation. For equal-cardinality neighboring sections it
emits one ordered quad `[a_i; a_(i+1); b_(i+1); b_i]` per segment, reducing
primitive and corner storage while retaining the exact PolyLoft alignment,
selection, U/V wrap, source-retention, payload/group ancestry, cancellation,
and normal contracts. Unequal cardinalities use the shared deterministic
triangle zipper. Planning and materialization are O(output corners), with one
fixed-arity plan per independent section pair and no per-face size boxes.
Native spline surfaces and two-input bilinear cross-skins are intentionally not
represented by this polygon kernel.

PolyBridge first decomposes two topology-affine edge selections into simple
degree-two-or-less paths and loops. Open paths begin at their least numbered
available endpoint; cycles and all component ties use stable point/primitive
keys. Source and destination component arrays pair in authored order or by
independently sorting their scale-safe centroids on X/Y/Z and stable component
index. The latter is O(k log k), retains a one-to-one mapping, and avoids the
quadratic greedy matching scan that an early implementation used.

Each paired boundary becomes an indexed section in the same PolyLoft/Skin
planner. Equal cardinalities emit quads and unequal cardinalities emit zipper
triangles. Reverse controls reorder either indexed boundary; automatic closest
endpoint/seam alignment runs before an explicit destination shift for closed
loops. The direct bridge shares existing boundary points and appends to input
topology by default. Exact source-corner/source-primitive ancestry drives the
common topology remapper, so packed payload, ordinary groups, and native edge
groups retain their established policies; generated cross-edges acquire no
invented ancestry.

Path extraction and materialization are O(vertices + edges + output corners)
with O(edges + output) storage. Component arrays use geometric-growth local
buffers rather than allocating the complete selected-edge count per component.
Independent plans, topology fills, payload maps, and normal generation use
stable disjoint pool ranges. One giant bridge retains sequential ordered path
tracing and zipper planning; many independent bridge pairs expose coarse
parallel work. Spine subdivision, curved/external paths, and thickness/twist
are handled separately from this direct no-new-points path.

For equal-cardinality pairs, positive PolyBridge divisions now allocate
`(divisions - 1) * boundary_points` intermediate points and emit one quad per
boundary segment per division. Positions and all numeric point/vertex payload
use scale-safe linear interpolation; integer, text, ragged payload, and
ordinary groups use the nearest endpoint with an exact half-way destination
tie. Detail payload remains shared, primitive payload follows the source
footing, and native edge groups map only through unchanged original boundary
points. Interpolation maps are allocated only when their owner actually has
payload or groups. Stale normals never pass through the topology change, and an
existing point or vertex normal owner can be regenerated after the full cook.

Intermediate point and quad-validity fills parallelize one giant pair by
stable ranges; independent pairs parallelize at pair granularity. Positive
collinearity filtering uses a packed byte decision plane, then allocates exact
face/corner cardinalities. Unequal-cardinality divided bridges are rejected
until a topology-transition policy can preserve explicit source/destination
footing rows; curved/external paths, non-uniform spacing, and thickness/twist
remain audited extensions rather than approximations.

Circular Sweep/PolyWire exposes one operation over two cardinality-specialized
paths. The measured fixed-resolution, unrestricted compatibility path remains
byte-identical. The general path replaces only selected primitive curves,
passes polygons and curves outside the group through exactly, preserves free
points, and samples every selected source edge by a constant or point-integer
endpoint-averaged segment count. A point-integer division attribute selects
each sampled ring's radial cardinality; unequal neighboring rings use a stable
minimal triangle/quad zipper with `a + b - gcd(a,b)` faces and
`3a + 3b - 2gcd(a,b)` corners.

Both paths use scale-safe rotation-minimizing frames, closed-curve holonomy
correction, point-float radius scaling, point-integer snapped seams, optional
point-authored V and projected joint-up fields, and seam-safe side UVs. The
general path supports a constant or outgoing-corner float2 first/last interior
segment placement in normalized source-edge coordinates. Endpoints remain at
zero and one; inclusive placement may deliberately create coincident sampled
rings, while the source edge itself must remain finite and non-zero. Generated
UVs can be disabled without discarding an authored source vertex `uv` field.
Constant U/V ranges or outgoing-corner float4 `(u0,u1,v0,v1)` ranges apply per
source edge; an authored point V field has precedence over the V range. It also
supports opt-in joint buckling prevention at every non-end source point. The
joint tangent is the scale-safe bisector of the incident unit edge directions;
each radial ring point is enlarged by the exact cylinder-intersection miter
factor and clamped by a finite constant or point-float maximum of at least one.
Straight joints remain unchanged, bend-axis-independent sides retain unit
scale, source endpoints remain ordinary rings, and control-only neighbor,
direction, and limit planes are not allocated when prevention is disabled. It
also supports constant Smooth Point, point-float smooth values with the
documented 0.5 threshold, and optional maximum selected-edge valence. A broken
joint duplicates only its source ring and omits the cross-joint transition;
tangent search and frame transport restart independently at each run. Closed
curves rotate deterministically to their first broken source point and become
one or more open runs. Break ends remain uncapped even when ordinary open-curve
caps are enabled. Selected-edge valence uses the shared packed topology index,
and the no-control path allocates no smooth-run planes. An outgoing-corner
integer segment-seam field cyclically remaps the physical ring correspondence
for every transition on that source edge. The same shift is reduced safely for
unequal ring cardinalities, applies to cap ordering and native-edge ancestry,
and preserves logical seam-safe U coordinates without varying halfway through
a subdivided edge. It also supports
optional open-spine N-gon caps carrying seam-aware planar UVs, hard vertex
normals, and a primitive cap group. The general path records exact two-endpoint
ancestry for every sampled ring: numeric fixed-width point/vertex payload is
linearly interpolated; integer, text, ragged payload and ordinary groups use a
documented destination tie at one half; primitive/detail payload remains exact.
Native edge groups propagate to retained edges and to only those longitudinal
wire edges whose normalized radial coordinates coincide, never zipper
diagonals. Ring points, global transitions, caps, payload, groups, and native
edge bitsets fill independent stable ranges; the ordered frame prefix of each
curve remains sequential.

General-profile Sweep is a separate two-input packed kernel rather than a
second mode hidden inside PolyWire. It forms stable backbone-major Cartesian
pairs of selected polygon curves and emits the same point/row/column/quad and
triangle connectivity vocabulary as Grid. Cross sections are interpreted in
local XY with +Y up and +Z along the backbone, while full local XYZ offsets are
retained. Average-edge, central-difference, previous/next-edge, and fixed-Z
tangents feed scale-safe rotation-minimizing frames; closed-loop correction is
performed after distance-weighted roll/twist so a non-integral twist cannot be
concentrated into the closing edge. Conventional point `orient`, `N`, `up`,
`pscale`, and float3 `scale` fields optionally replace or scale that frame.

Both inputs retain point/corner/primitive/detail ancestry. Cross-section fields
and ordinary/native groups use an explicit prefix (default
`cross_section_`) so the two sources do not silently overwrite one another;
backbone and profile native edges map only to longitudinal and ring edges,
respectively, never generated triangle diagonals. Ancestry planes are allocated
only for owners that actually carry payload or edge provenance. Ordered frame
transport remains per curve, but independent curves, global output points,
topology, UVs, attributes, groups, and edge classification use stable disjoint
pool ranges. See the SOP audit for the deliberately unclaimed SideFX controls.

Resample uses the same polygon-curve boundary with either an exact segment
count, a maximum segment length, or both as a deterministic ceiling. It can
equalize every output edge or retain a shorter final edge. Primitive groups
restrict cooking without reordering the stream; primitive float length and
integer count fields override controls, with non-positive fields disabling the
corresponding control and exact pass-through when both are disabled. It
preserves open and closed endpoints/topology, interpolates all point/vertex payloads, and retains
primitive/detail, ordinary-group, and exact traversed native-edge ancestry.
Optional point fields expose input polygon `curveu`, source curve number,
half-adjacent-edge coverage distance, and normalized output tangent. One packed
cumulative-length plane is shared by stable output chunks, so a single long
curve can parallelize without an O(output log input) per-sample search.

PolyFrame constructs normalized coordinate fields without changing topology.
First Edge, Two Edges, Primitive Centroid, and Texture UV produce point-owned
frames; Texture UV Gradient and Attribute Gradient produce vertex-owned frames
so discontinuous UV or arbitrary float2/float3 fields retain their seams.
Empty texture names resolve to `uv`. Typed component selection, configurable
output names, disabled tangent/bitangent outputs, and optional orthogonal
projection with explicit right-/left-handed output are explicit. Existing output values outside the
selection survive unchanged. The implementation shares packed topology and
face/point-normal passes, computes local attribute derivatives directly, and
fills independently owned point, vertex, and primitive ranges through the
reusable domain pool.

Facet is an ordered modeling pipeline rather than a bag of independent flags.
Its current packed stages follow pre normals/unit normalization, Unique Points,
mutually exclusive point or normal consolidation, inline-point removal, polygon
orientation, dihedral-angle polygon cusping, degenerate cleanup, post normals,
and final normal reversal.
The entire ordered pipeline can be restricted to a primitive group. A private
primitive bitset follows topology deletion between stages and is removed before
publication. Empty selection preserves object identity; full selection takes
the original allocation-tight compatibility path. Unselected primitive corners,
winding, and primitive payload remain exact. Point-owned values intentionally
follow referenced-point semantics: a shared point is selected when at least one
selected primitive references it.
The public typed selection boundary also accepts point, vertex, and native-edge
groups. Point membership promotes every incident primitive, vertex membership
promotes its owning primitive, and edge membership promotes every incident
primitive. Promotion validates ordinary owner/length or native topology
affinity, scans stable primitive corner ranges, and materializes one packed
primitive mask before any stage runs. Supplying both the typed boundary and the
legacy pre-resolved primitive group is rejected atomically.
Unique Points gives every referenced corner an
independent point while retaining isolated points; all point storages and
ordered groups follow exact target-to-source ancestry, and one selected shared
native edge expands to every new boundary-edge descendant. Orient Polygons
solves stable manifold component parity, reverses vertex fields/groups with
their corners, retains edge groups by endpoint identity, and rejects
non-manifold or contradictory orientation constraints atomically. Cusp
Polygons splits the incident corner fans at manifold edges whose dihedral angle
strictly exceeds the threshold. It retains the first fan at each source point,
appends later fans in stable incidence order, duplicates every point payload,
and maps native edge groups to every descendant. Classification and packed
materialization are parallel; deterministic iterative union-by-rank keeps fan
connectivity stack-safe and independent of domain scheduling.

Remove Inline Points performs stable, iterative polygon simplification against
the previous-to-next segment with a finite world-space distance threshold. A
linked cyclic work queue revisits only neighbors of removed corners, never
reduces a polygon below three vertices, and passes non-polygon primitives
through unchanged. Exact target-to-source planes preserve point, corner,
primitive, detail, ordinary-group, and native-edge ancestry; pre-existing
isolated points survive while points orphaned by simplification are compacted.
Scale-normalized predicates remain translation robust at extreme finite
coordinates, and unchanged inputs retain geometry identity.

Consolidate Normals uses the same deterministic representative-bounded spatial
clustering core as Fuse without changing point positions or topology. Point
`N` averages cluster members; vertex `N` averages every incident corner of the
cluster's points. Both owners may coexist, arithmetic averages retain their
natural magnitude, and scale-normalized accumulation prevents finite extreme
vectors from overflowing. Stable cluster-member CSR planes preserve exact
source-order sums across domain counts, while disjoint cluster reductions and
normal fills use the reusable domain pool. Missing normals are an identity
operation; invalid storage and non-finite values fail atomically.

Make Planar projects every selected non-planar polygon onto its centroid plane
using a scale-normalized Newell normal. Triangles, polygon curves, and polygons
already planar to the floating-point noise floor preserve identity. A selected
polygon with a non-finite corner, no stable plane, or a projection outside the
finite coordinate range fails atomically with its stable primitive index.
Exclusive points move in place. When one source point belongs to an unchanged
primitive or to several independently projected polygons, only the conflicting
corners receive duplicated points; this guarantees every transformed polygon
is planar without moving an unselected neighbor. All point storage kinds,
ordinary groups, and native edge ancestry follow the exact source-point map.
Classification and projection use disjoint primitive/corner ranges; stable
point claims and output numbering remain independent of domain scheduling.

Circle is the packed polygon-curve implementation of SideFX's circle/ellipse
and arc contract. The compatible default remains a closed XZ circle. Independent
radii produce ellipses; XY, XZ, YZ, and scale-safe custom axes place the curve;
center, positive uniform scale, in-plane radians rotation, and reversed
traversal complete the polygon transform surface. Typed arc modes emit a full
closed curve, an open sampled arc, an endpoint-chord closed arc, or a closed
pie-slice boundary with one center point. Closed RDK topology is canonical and
does not duplicate the first point.

For `s` segments, a full circle emits exactly `s` points/vertices, an open or
chord-closed arc emits `s+1`, and a sliced arc emits `s+2`; every mode emits one
curve primitive. Position and identity-index planes are allocated once and
filled together in fixed disjoint ranges. Exact arc endpoints bypass accumulated
parameter error. Work and output are O(points); auxiliary error storage is
O(ceil(points / grain)). No point tuple, list, builder growth, or temporary
geometry survives the source cook.

Grid accepts either division counts or Houdini-style point counts. Its output
can be free points; one open polyline per row, column, or both; quads; regular
or reverse diagonal triangles; or checkerboard alternating triangles. The
plane is XZ, XY, YZ, or a caller-defined pair of axes. Custom axes use
scale-safe normalization and Gram-Schmidt rejection, then an optional in-plane
rotation; the resulting normal is the normalized vertical-by-horizontal cross
product. Width and height are independent, `size` remains the compatible
shared default, and an optional point float2 field stores normalized lattice
coordinates.

For `u` by `v` output points, Grid emits exactly `uv` points. Rows and columns
emit respectively `(uv, v)` and `(uv, u)` vertices/primitives; both emit
`(2uv, u+v)`. With `c=(u-1)(v-1)`, quads emit `(4c, c)` and triangle modes emit
`(6c, 2c)` vertices/primitives. All cardinalities are checked before allocation.
Point rows, line primitives, surface cells, and offset planes are written to
disjoint stable ranges through the reusable pool. Time is
O(points + vertices + primitives); auxiliary storage beyond the exact output is
O(ceil(rows / row-grain)) for deterministic numeric-error reporting. There are
no per-point objects, lists, append pipelines, or scheduling-order-dependent
indices.

Surface Scatter plans only selected polygon faces, ignores curve primitives,
and ear-clips simple concave N-gons directly into source-corner triples without
materializing a second geometry. A Walker alias table gives O(1) triangle
selection per output point. Fixed-total density can be point, vertex,
primitive, or detail float storage; negative values contribute zero. For
point/vertex density, triangle integrals use the exact mean of the linear
corner field and samples use the corresponding mixture of Dirichlet(2,1,1)
distributions, rather than merely weighting whole faces. General attribute and
ordinary-group patterns reuse Attribute Interpolate through exact three-entry
source-vertex/weight rows; those CSR rows and original primitive numbers can
also remain as public provenance for later deformation cooks. Direct triangle
meshes avoid triangle-map storage. Position, normal/color, provenance, and
alias buffers are exact-sized, point ranges allocate nothing per sample, and
all published ordering is exact across domain counts. Planning is O(sum of
selected polygon corner-squared ear work), sampling is O(output points), and
auxiliary storage is O(emitted triangles + output provenance).

Ray and closest-surface projection share `Surface_index`, the single packed
polygon collision core. The index ear-clips simple N-gons while retaining the
original primitive and source-corner identities, then builds a median-split
AABB hierarchy over compact structure-of-arrays bounds and triangle maps.
Directional traversal normalizes constant or point-owned directions robustly,
supports forward/reverse/bidirectional and first/last-surface policies, and
uses an exact closest-triangle check for world-space border tolerance instead
of a shape-dependent barycentric epsilon. Selected queries and output/import
ranges write disjoint exact arrays. Hits can retain original primitive plus
source-vertex/weight CSR provenance, so later operations reconstruct collision
fields without exposing internal triangulation. Multi-ray projection retains
one exact unjittered direction and derives every extra cone-disk sample from a
stable seed/point/sample key. Each worker owns O(samples) result/order scratch;
average, upper-median, shortest, and longest reduction cannot leak scheduling
order into output. Average attribute import sizes exact CSR rows first and
repeats only the chosen directional sample set, avoiding a
points-by-samples temporary hit matrix. Collision construction is
expected O(corners + triangles log triangles) time and O(triangles) storage;
ordinary queries are expected O(log triangles) each and sampled projection is
O(points * samples * log triangles), with an additional O(samples log samples)
per point for the median combiner. Queries can degrade to
O(triangles) for completely overlapping bounds. Operation storage is
O(triangles + source points + requested imported output + domains * samples),
apart from output-sized average provenance.

`Ops.materialize_instances` is the single editable-topology boundary for an
arbitrary ordered transform array. It computes point, vertex, and primitive
cardinalities before allocation, emits exact transform-major topology, repeats
ordinary attributes/groups and topology-affine edge groups with stable
ancestry, and transforms typed point/vertex normals by normalized inverse
transpose. If any applied matrix is singular, typed normals are removed
atomically; other payloads remain intact. Zero transforms produce valid empty
topology while preserving detail attributes, and `apply_transform=false`
materializes prototype-space copies. Position, topology, attribute, group, and
normal planes use deterministic disjoint ranges, cancellation never publishes
a partial snapshot, and auxiliary storage is O(instances) beyond exact output.
Unrestricted `Duplicate` constructs its transform sequence and invokes this
same exact-copy kernel. Its primitive-restricted path retains the full input as
an exact prefix and plans stable selected primitive, corner, and referenced
point maps once. It then fills only appended selected payload, with ordinary
and ordered group ancestry, native-edge ancestry, and normalized
inverse-transpose normals. Optional one-based copy groups are built as bounded
packed bitsets in one metadata commit; same-name primitive membership is
replaced or explicitly preserved by union. No compact/materialize/merge
intermediate is retained, so auxiliary memory remains linear in input maps plus
exact output rather than duplicating the output snapshot.

UV Project creates packed vertex float2 coordinates through arbitrary
planar frames or cylindrical/spherical frames, with finite-input validation,
primitive restriction, output ranges, per-primitive wrap correction, and pole
repair. UV Transform applies translation, non-uniform scale, rotation, and
pivot to point- or vertex-owned float2 UVs with owner-matched group
restriction. UV Auto Seam classifies compact undirected topology using angle,
boundary/non-manifold, primitive partition, and existing-UV continuity rules;
it emits a native edge group and optional stable primitive island IDs while
retaining an outgoing-corner compatibility view. Edge Group exposes
the same native topology layer directly with incidence, length, and primitive
filters. Its angle basis is either the dihedral between two incident polygon
faces or the angle between every pair of selected edges sharing a point; an
edge matches the latter when any pair lies in the inclusive range. Edge
directions use normalized SoA scratch, including near `max_float`, and the
final topology-affine membership is written directly as a bitset. Dihedral
classification is O(edges + polygon corners); incident-edge classification is
O(edges + sum(point degree squared)), which is the required pairwise work,
with O(edges) numeric scratch. UV Unitize fits either individual faces or connected UV islands to the
unit square, with optional aspect preservation and explicit seam cuts. UV
Flatten splits corner charts on native or compatibility seams, validates each
triangle island as a manifold disk, maps its boundary by 3D arc length to a
convex circle, and solves positive mean-value harmonic coordinates. UV Relax
uses the same chart and solver while preserving existing boundary coordinates.
The matrix-free diagonally preconditioned conjugate-gradient passes report
non-convergence, collapsed triangles, and flips instead of publishing partial
UVs. The
normal/edge and bounds/output stages use deterministic disjoint parallel
ranges; island union is sequential so work-stealing cannot affect component
IDs. These operations preserve topology, positions, unrelated
attributes, and groups by identity. Deterministic Fuse supports exact/tolerance
clustering, named point restriction, explicit position/attribute policies,
point-group union, and optional attribute-seam matching. Unselected points are
neither queries nor implicit targets. Its fixed-target planner accepts a
read-only second geometry or a separate same-geometry target group. Packed
near-point cells select either the least eligible target number or closest
target with stable ties; an alternate direct integer map implements specified
targets. Non-negative point radii expand the distance threshold, scalar
float/integer/text match fields filter equal or unequal candidates, and output
metadata records mapped queries and target numbers. Query ranges and position
writes are parallel and allocation-free inside the candidate loop. Snap-only
mode shares unchanged topology and payload; optional exact consolidation
reuses the original Fuse cluster/remap core. Same-input Modify Target converts
stable query/target links to deterministic minimum-root components and reduces
both sides. Its position policies cover least/greatest point, average,
component minimum/maximum/mode/upper-median/sum/sum-squares/RMS, weighted
average/sum, and minimum/maximum weight; weighted policies consume one finite
scalar point plane and fail atomically on an invalid or zero-denominator
result. Keep Fused Points rewires topology but keeps all point records.
Optional cleanup removes consecutive duplicates and resulting sub-cardinality
polygons/curves, with either newly orphaned or all-unused stable point
compaction. Complete vertex/primitive attribute, ordered-group, and native-edge
ancestry is preserved. Ordered point-attribute rules provide numerical,
string, weighted, scalar-to-array, and existing-array concatenation policies;
point-group rules provide least/greatest, union, intersection, and strict
majority. Fixed targets copy matching payload and target-only groups through
the selected target map, while Modify Target reduces both sides. Rule patterns
are compiled and generated output metadata is committed afterward. Grid Snap
writes selected positions into
exact packed planes using positive per-axis spacing, normalized offsets, stable
nearest/down/up rounding, and optional Euclidean movement tolerance. Its moved
membership uses one packed bit per point with byte-aligned worker ownership;
optional consolidation delegates to Fuse at zero tolerance instead of owning a
second weld algorithm. Snap-only work is O(points + output payload) time and
three position planes plus O(points / 8) transient state. Fuse cluster discovery
is stable source order so earliest-representative semantics do not depend on
scheduling; exact-sized position, attribute, topology, and group remaps are
parallel.

Bound converts an optional typed point, vertex, primitive, or native-edge
selection to referenced points through the shared topology index. Range-local
min/max values reduce in stable order; cheap million-point reductions retain a
measured sequential cutoff, while larger or incidence-heavy inputs keep the
parallel path. Divided boxes compute all six face cardinalities before
allocation and fill disjoint face-local position, normal, triangle, and winding
ranges. Bounding spheres start from the selected AABB diagonal, then apply
asymmetric padding as center/radius changes to the shared UV-sphere generator.
That generator lives in the `Uv_sphere` test generator with a direct `Rdk.Uv_sphere.run`
entry point; `Rdk.Uv_sphere` retains its typed-error boundary. It allocates
exact packed position, normal, and topology arrays
and fills points/triangles directly, with no per-triangle point tuples. Optional
detail center/radii and the all-output primitive group are committed only after
successful geometry construction. Bounds reduction is O(points + selected
incidence); box output is O(surface divisions), sphere output is O(segments ×
rings), and auxiliary/output memory is exact linear storage plus range-local
min/max values.

Match Size separates three roles that Houdini exposes independently: the
component selection to move, the source selection used for justification
bounds, and the target selection used for reference bounds. Each typed
selection resolves to referenced points through the shared topology index.
The target may instead be an origin-centered unit box or an explicit finite
center/size box, so the eager RDK API does not require a synthetic geometry
input for numeric matching. Source and target anchors are independently
continuous from minimum through center to maximum on each axis. Translation
enable, world offset, non-uniform scale enable, and the scale multiplier are
also per-call immutable facts.

Scale modes include independent stretch, uniform X/Y/Z axis matching,
contain, cover, and total primitive perimeter, surface-area, or signed-volume
matching. Metric modes reuse `Analysis` and convert ratios to linear scale by
identity, square root, or cube root; non-primitive metric selections and
measureless inputs fail explicitly. Selected positions fill exact copied SoA
planes in disjoint ranges. Point and vertex `N` use the selected diagonal
inverse transpose and normalization; singular output removes stale normals.
Incidence-based move selections compile once to a one-byte-per-point mask so
position and both normal traversals do not repeat topology scans. Bounds are
O(points + selected incidence); bounds-only fitting then costs O(points +
normal payload), while metric fits add O(vertices) perimeter work or the
documented polygon-measure triangulation. Transform stash/restore remains out
until RDK has an unambiguous detail-matrix storage type.

Sort reorders point or primitive ownership while preserving every dependent
packed plane. Point permutations rebuild topology point references and remap
point attributes, ordinary/ordered groups, and native-edge endpoints. Primitive
permutations rebuild CSR spans and remap vertex/primitive payload and groups.
Restricted value sorts replace only the selected destination slots and retain
stable input order for equal keys.

Point By Vertex Order assigns referenced points by first packed-corner
appearance; By Primitive Index uses the lowest incident primitive. Both place
unconnected points first in stable point-number order. Spatial Locality maps
finite point positions or primitive centroids into a scale-normalized 21-bit
box per axis and sorts the resulting positive 63-bit Morton codes. Coincident
cells retain stable input order. The Morton plane fills in parallel without
tuple allocation and remains finite for coordinates near `max_float`.

Seeded random sorting uses an immutable `Rand.t` stream and an in-place O(n)
Fisher-Yates permutation with no per-element allocation; as in the documented
Houdini contract, this mode rejects group restriction. Reorder-by-index accepts
only an exact integer permutation of all destination indices, or of the actual
selected slots for a restricted sort. Duplicate, missing, out-of-range, wrong-
owner, and wrong-storage fields fail before remapping.

Indirect Sort Indices computes the same permutation but publishes its inverse
destination-rank integer plane without changing topology. Combine mode first
restores the stable order encoded by an existing valid rank plane, then applies
the next stable value key; chained multi-key sorts therefore avoid intermediate
geometry rebuilds. Shuffle and index validation are O(n); value sorting is
O(n log n). Key preparation, inverse-rank output, and all payload remaps use
deterministic disjoint parallel ranges, while comparison order and shuffle are
serial by contract.

Blast by Attribute classifies one same-owner scalar float or integer plane over
points or primitives. Threshold mode is strictly lower-than; range and
center/width modes include both endpoints. An optional packed base group
restricts both normal and inverted selection. The result either replaces one
ordinary same-owner group while sharing all other geometry, or delegates the
selection to the authoritative Delete planner. Primitive deletion may compact
all unused points; point deletion follows Delete's destroy-touched-primitive
contract.

Mode, owner, name, base cardinality, output policy, and numeric data validate
before commit. Operated floating values must be finite; values outside a base
group are deliberately not read or validated. Classification is O(elements)
time and one packed bit per element. Independent output bytes fill through the
reusable domain pool with no per-element boxing; destructive output then has
Delete's O(points + vertices + primitives + payload) time and linear remap
storage. One- and multi-domain results preserve exact element and payload order.

### PolyCut

PolyCut breaks selected polygon primitives and polygon curves through a single
packed fragment planner. The point mode marks selection-eligible endpoints of
invalid edges. Remove drops marked endpoints and both adjacent segments; Cut
retains them as independently owned endpoints on the neighboring fragments.
The edge mode instead removes invalid edges or inserts disconnected cut
endpoints along them. A primitive group restricts the source paths, while the
secondary restriction is owner-exact: a point group in point mode and a
topology-affine native edge group in edge mode.

Three detection policies are explicit. All treats every eligible edge as
invalid. Crossing accepts a scalar point float or integer attribute and uses a
scale-normalized interpolation ratio, including exact endpoint crossings; an
edge whose two values both equal the threshold is removed without inventing a
cut position, matching SideFX's published rule. Change accepts scalar or
float2/3/4 point storage, measures absolute or scale-safe Euclidean change,
and, under Cut, emits `ceil(change/threshold)` disconnected subsegments. A
positive threshold is required for that finite-subdivision case. `P` is
available to Change through the canonical packed position plane but is
deliberately rejected by scalar Crossing.

Open fragments remain open. Fragments from a closed polyline or polygon may be
closed when they contain at least three corners; the corresponding source kind
is retained and its new closure edge has no invented source ancestry. Source
points remain shared where no break requires separation. Inserted interior
cuts receive two independently owned points, and point cuts clone only the
outgoing side. Point Remove compacts a source point only when no retained
corner uses it, while preserving pre-existing free points. Edge Remove keeps
the original point domain, as deleting an edge does not imply deleting its
endpoints.

Point and vertex floating payload is linearly interpolated, including normal
renormalization through the shared curve interpolation boundary. Integer,
text, and packed CSR rows use the nearest endpoint. Primitive attributes and
groups follow exact fragment ancestry, detail values are shared, ordered
groups retain source traversal order, and every retained or subdivided native
edge inherits its source edge membership. All output planes are allocated from
precomputed cardinalities and published only after validation.

For `p` points, `v` corners, `r` primitives, and `k` emitted cut subsegments,
time and auxiliary storage are `O(p + v + r + k)` plus linear payload remap.
Edge Remove has a specialized no-interpolation path that shares positions and
point/detail payload, avoids expanded-segment/sample planes, and writes stable
fragment ranges directly. Other modes classify directed edges in parallel,
prefix exact segment/fragment cardinalities, fill disjoint primitive/corner
ranges, compact retained points once, and interpolate output planes through
the reusable domain pool. Lowest malformed operated-field diagnostics,
ordering, attributes, groups, mesh conversion, and framebuffer bytes are exact
between one and four domains.

### Triangulate 2D

`Rdk.Voronoi2.cells` owns bounded pairwise half-plane clipping for ordered
2D sites. It removes near-duplicate sites in first-occurrence order, preserves
polygon vertex order, rejects non-finite inputs, and returns a typed cancellation
error. The ordered five-cell fixture is checked directly at the RDK boundary
in one and four domains.

`Rdk.Iso_surface` streams XY slabs while extracting a six-tetrahedra
isosurface into packed RDK point positions, triangle topology, and point
normals. It supports boxed and dense scalar fields, typed malformed-input and
cancellation errors, and deterministic one/four-domain output. A captured
one-cell plane fixture fixed all 24 emitted positions, identity indices, and
normals across migration.

`Ops.triangulate_2d` is the first public adapter over the shared packed
`Delaunay2` core. It accepts a typed point selection and projects source points
through PCA best fit, XY/YZ/ZX, an explicit origin/normal frame, or a point
float2/float3 attribute. Triangles continue to reference the original point
numbers and therefore restore the authored 3D positions without a reverse
metric mapping. Exact projected duplicates use the lowest selected source
point. Optional duplicate removal deletes only the non-representative selected
points after topology and groups are finalized; it uses the shared stable
Deletion remap, so unrelated unused or unselected points and their payload
remain intact. Point/detail payload and point groups are shared by default; topology-
dependent payload and stale normals are removed.

Ignore Non-Constraint Points mode first derives the complete exact constraint
set and uses only its source endpoints in stable source order. Explicit native
edge and primitive constraints are prefiltered before projection and Delaunay
construction; silhouette mode retains the full orientation pass, then rebuilds
from the exact derived boundary. Other selected source points remain in the
immutable point payload but are isolated from output topology; a separate
Remove Unused Points policy may compact them. The reduced seed and
endpoint remap use exact-sized packed arrays and disjoint stable fills.
Requesting this mode without any constraint is an explicit error rather than
silently producing an empty mesh.

After topology and output groups are finalized, optional Remove Unused Points
uses the shared stable packed compaction kernel, preserving exact fixed/ragged
payload and group ancestry. Optional Recompute Point Normals invokes the shared
Normals kernel only when the input already contained point `N`; it does not
invent a normal schema for inputs that did not request one. Both controls are
part of immutable SOP identity rather than hidden post-cook mutation.

The core uses packed counter-clockwise triangle/neighbor planes, adjacency
walking, local Bowyer-Watson cavities, and recycled triangle slots. Filtered
orientation and incircle tests fall back to exact dyadic signs whenever a
binary64 decision is ambiguous, underflows, or overflows. Ordinary ranges use
a finite power-of-two supertriangle; extreme finite inputs use exact
homogeneous coordinates, so no finite world-space bound is assumed. Seeded
insertion only shuffles fixed spatial blocks and never reads global random
state. Projection and output point-number fills use stable disjoint ranges;
the dependency-ordered topology insertion remains serial.

Native-edge and primitive-perimeter constraints first enter the shared
`Planar_constraints` arrangement. A packed median BVH filters segment pairs;
exact orientation signs classify proper crossings, endpoint-on-segment
junctions, and collinear overlaps. Proper crossings are rejected unless the
explicit splitting policy is enabled. When enabled, exact homogeneous
line-line constructions are deduplicated before deterministic atomic segments
are emitted. Existing constraint endpoints split T junctions and overlaps in
either mode. Authored Delaunay representatives are queried against the same
packed segment BVH in two passes. Exact orientation decides point-on-segment
incidence; prefix-summed counts permit disjoint parallel fills and stable
output. Exact-coordinate duplicates select the lowest source point, including
when an authored point already occupies a proper crossing. Expected work is
O((segments + embedded points) log segments + incidences), with linear
auxiliary storage and unavoidable output proportional to incidences.
New points are inserted by exact containing-face or containing-
edge splits and then enter the shared `Planar_cdt` kernel. It traces the exact
sequence of triangles crossed by each atomic segment, removes that strip as one
cavity, exact-ear-clips the two boundary chains against the new segment, and
repairs only unconstrained diagonals to a stable constrained-Delaunay state.
This avoids the cycling failure of repeated arbitrary Lawson flips and
preserves triangle cardinality. A specialized integer edge table owns
incidence and constraint affinity; recovered segments can be emitted as a
topology-affine native edge group. Split points append to the original point
planes. Their 3D position and continuous point fields use the deterministically
lower-indexed source constraint; discrete/ragged fields and point groups use
its nearest endpoint. A generated-point group makes this policy inspectable.
Authored points that split a constraint retain their existing identity and are
not reported as generated.

Sequential CDT builds may borrow an exclusively owned private workspace with a
reserved point capacity and stable edge-key stride. The first build fills its
geometrically growing triangle planes, point-to-triangle seeds, and open-
addressed edge table. Passing that exact packed result back by physical
identity with a contiguous suffix of inserted point numbers continues the live
incidence directly; an empty suffix reuses it for coordinate-only Delaunay
repair. Canonical previous/current constraint arrays are merge-differenced in
linear time. Removed edges are unmarked, added edges alone are recovered and
marked, and triangle replacement reaffirms any unchanged current constraint,
so split or removed constraints cannot leave stale affinity. A copied,
discontinuous, over-capacity, clipped, or winding-filtered snapshot takes the
audited full-rebuild path.

Every returned public value owns a separate packed triangle array. A failed or
cancelled continuation invalidates only its scratch session, cannot mutate an
earlier result, and permits a later full rebuild in the same workspace.
Successful full/incremental counters expose the path to focused diagnostics.
Final canonical ordering writes one packed array and applies the sorted
permutation in place by disjoint permutation cycles instead of allocating a
second packed triangle array. Standalone calls retain the same result ownership
and ordering without requiring a workspace.

Optional quality refinement operates on that already clipped constrained-
Delaunay mesh rather than reconstructing the original convex hull. Each stable
generation classifies bad triangles by minimum angle, maximum area, or target
edge length. It proposes a scale-normalized binary64 circumcenter and accepts
that construction only after exact orientation predicates certify it inside
the retained parent triangle; a rounded centroid is the bounded fallback.
Predicates remain exact for every accepted binary64 construction. An
encroached constraint is split at its exact homogeneous midpoint when
constraint splitting is enabled. All accepted points are exact-deduplicated,
constraints are re-atomized in stable order, and the new batch is inserted
into the current clipped CDT. Consequently refinement cannot resurrect a
previously removed exterior region.

The point budget includes arrangement crossings as well as refinement points;
an arrangement that already exceeds the requested budget fails before a
partial result is committed. Reaching the interior refinement budget returns
the best completed generation and records the limit rather than exceeding the
bound. Minimum angle is in radians and must be below pi/3. Minimum edge length
suppresses further work on a bad triangle whose longest edge is smaller than
the threshold. Generated positions and continuous point payload use stored
parent barycentric weights recursively; exact constraint midpoints use equal
endpoint weights. Discrete, ragged, and group payload select a stable parent
representative. Split-point and refinement-point output groups remain
distinct.

Triangle classification and candidate construction fill disjoint arrays in
parallel; the classification predicate returns one immediate boolean rather
than allocating a float tuple per triangle. Refinement and regularization
share one private CDT workspace across their sequential generations. CDT
recovery remains dependency-ordered and serial. Contiguous generations retain
triangle incidence and insert only their new point suffix; coordinate-only
regularization retains the same incidence. Every generation still scans the
complete edge table to seed Delaunay legalization and emits a globally sorted
canonical snapshot; only the constraint delta is replayed. For one
generation, classification is O(T), CDT update has the documented
`Planar_cdt` expected cost, and a shared packed bounds index makes constraint
encroachment and re-atomization expected O(S log S + (Q+B) log S + I), for Q
quality candidates, B accepted points, S atomic constraints, and I conservative
broad-phase incidences. Certified implicit-coordinate intervals and an
outward-rounded L1 circle bound prevent the index from dropping an exact
candidate; exact diametral and point-on-segment predicates remain authoritative.
Packed point, triangle, constraint, provenance, index, and scratch planes
require O(P + T + S + Q + B) live storage.

An explicit non-negative Regularization Steps count runs after the final
refinement generation. Each step builds one packed unique-edge/point-incidence
index, fixes every hull and constraint point, and moves eligible generated
interior points toward the stable compensated average of their one-ring
neighbors. Allow Movement of Interior Input Points extends eligibility to
original projected interior points. Candidate coordinates are rounded
binary64 constructions, then a deterministic global backtracking factor is
halved until every moved point remains in its old star and every triangle has
strictly positive exact orientation under the simultaneous move. The shared
CDT repair restores constrained-Delaunay topology after the accepted step.

Repeated movement appends to a compact topologically ordered provenance DAG.
Generated 3D positions and numeric point payload evaluate this DAG once;
discrete, ragged, and group payload follow its stable greatest-weight source
representative. With the default restoration policy, original input positions
remain authored while their relaxed projections may affect connectivity; the
disabled policy below materializes those relaxed coordinates. One step costs
O(T log T + E + P) for its packed index and relaxation work plus CDT repair;
live relaxation scratch is O(T + E + P), and provenance grows only with
explicitly moved generated points per requested step. Centroid, construction,
containment, and orientation ranges are parallel; stable index construction, acceptance, DAG
commit, and CDT repair remain serial.

Restore Original Point Positions defaults on and evaluates the same provenance
DAG against authored 3D `P`. When disabled, the adapter instead materializes
the final relaxed 2D coordinates on the selected projection plane. XY, YZ, and
ZX projection set the dropped world component to zero; an explicit plane uses
its authored origin and normal; PCA best fit uses its scale-normalized fitted
world plane. A point float2 or float3 coordinate attribute uses exactly its
first two finite components and emits `(x, y, 0)`; later float3 components are
irrelevant to both topology and output position. Participating source points,
constraint intersections, and refinement points are embedded, while isolated
source points excluded from the triangulation retain authored `P`. Every
embedded binary64 result must remain finite. The output allocates three exact-
cardinality position planes and fills disjoint local-point ranges in parallel,
so this mode adds O(output points) time and output storage without changing
topology decisions or point-payload interpolation.

Optional Keep Primitives constructs one cardinality-first output topology with
all input primitives except members of the explicit constraint-primitive group
as a stable prefix and all generated triangles as a suffix. Source curves and
polygons retain their kind, corner order, and point identities. Fixed-width and
ragged vertex/primitive attributes copy exact retained ancestry; generated
triangle entries receive the storage-kind default (numeric zero, empty text, or
empty CSR row). Ordinary and ordered groups preserve retained ancestry, while
the triangle output group selects only the suffix. Source native-edge groups
remap once by unchanged point endpoints; an explicitly named constrained-edge
output replaces a colliding source name, reusing the same target edge index.
Point/detail payload continues to use
the interpolation policy above. When duplicate removal is also enabled,
retained corners and native edges are first rewired to the stable projected-
coordinate representative, so compacting an otherwise referenced duplicate
cannot destroy a kept primitive. Planning and packed fills are O(source
primitives + source vertices + output triangles + retained payload), with
O(source primitives + output topology + retained payload) auxiliary/output
storage; primitive and corner fills write disjoint stable ranges in parallel.

Optional hull-boundary outside removal seeds every triangle incident to an
unconstrained convex-hull edge and performs one packed breadth-first flood
through unconstrained adjacency. Constrained edges are exact barriers. The
result compacts retained triangles and constrained-edge metadata in stable
order. Cavity retriangulation also detects vertices interior to a traced strip
and deterministically reinserts them, preventing a valid non-hull constraint
from silently dropping authored points.

Constraint-polygon outside removal carries signed directed winding through
every split atomic segment. Duplicate and overlapping segments aggregate their
signed multiplicity. Starting from exact hull-side values, one packed dual
breadth-first traversal propagates winding across every triangle edge; no
rounded centroid or ray test decides membership. Non-zero regions remain.
Consequently disjoint loops form a union, overlapping same-oriented loops stay
filled, same-oriented nesting remains filled, opposite-oriented nesting forms
a hole, and coincident opposite loops cancel. Edge-group and open-curve
constraints carry zero winding and therefore remain barriers without inventing
polygon interiors.

Projected-silhouette mode classifies every selected polygon with the exact
packed area sign. It chooses one global facing orientation, cancels directed
incidence across same-facing internal edges, and retains boundary or
opposite-facing edges as the silhouette. Reversing every source face therefore
does not change the result. Face classification and per-topology-edge winding
write disjoint stable ranges through the shared domain pool; validation and
constraint emission remain serial so errors and edge order are deterministic.
Partially selected polygons are rejected, while zero-area projected faces do
not invent an interior.

### Attribute Mirror

`Ops.attribute_mirror` copies point, vertex, or primitive attributes from a
source element to selected destination elements while retaining topology and
all unselected payload. Explicit mapping reads a same-owner integer field as a
destination-to-source index and requires a same-owner destination group. Plane
mapping robustly normalizes the plane, classifies exact-plane elements as a
seam, reflects negative-side point positions or primitive bounding-box centers,
and queries the shared packed `Spatial_index` for positive-side matches within
the finite non-negative tolerance. Plane correspondence for vertex attributes
and topology traversal are rejected rather than approximated by a second
topology kernel.

Compiled attribute patterns cover Float, Int, Text, Float2, Float3, Float4, and
packed integer/float rows. Copy is exact. UV reflection uses a normalized line;
vector reflection omits plane translation; point reflection includes it.
Vector/point transforms therefore require plane mode. Literal text replacement
is destination-only. Optional mapping output writes the source number on both
members of every pair and `-1` elsewhere; optional source and destination groups
may overlap for explicit self/cross mappings and are built deterministically.
An owner-matched restriction may describe either the source or destination
side. Explicitly selecting point `P` changes positions and invalidates stale
point/vertex normals unless the corresponding `N` field is also selected.

Explicit mapping is O(elements + copied payload) time and O(elements + output)
auxiliary storage: one integer source plane and one destination bit-plane are
the ordinary correspondence floor. Optional output maps/groups allocate only
when requested. Plane mapping adds O(elements log source-elements) index build
and nearest queries plus O(elements) packed spatial scratch. Cardinality,
owner, storage, pattern, finite-value, transform, name, tolerance, and
cancellation failures commit no partial geometry. Stable element scans and
disjoint packed writes make one- and multi-domain results byte-identical.

## Performance contract

- Packed dense operations are O(n) time and O(n) output storage unless a
  documented topology algorithm states otherwise.
- Stable sequential and multidomain runs must produce byte-identical topology,
  attributes, group membership, and ordering.
- Kernel grain is explicit and small workloads retain a sequential path.
- No per-element name lookup, boxed vector, list rebuilding, `List.nth`, `@`,
  or repeated array append is allowed in measured hot loops.
- Benchmarks report release profile, input/output cardinality, domains, grain,
  wall time, promoted/major allocation, and peak RSS when practical.
- Scale regressions cover exact cardinality and retained-byte estimates in
  addition to broad diagnostic timing thresholds.

### Benchmark commands

| Area | Command |
|---|---|
| Attribute Laplacian | `dune build --profile release tools/bench_rdk/bench_rdk.exe`, then `RAYS_BENCH_DOMAINS=1 RAYS_LAPLACIAN_POINTS=1000000 RAYS_LAPLACIAN_REPEATS=3 _build/default/tools/bench_rdk/bench_rdk.exe laplacian`; repeat with `RAYS_BENCH_DOMAINS=4` |
| Triangulate 2D | `RAYS_DELAUNAY_POINTS=100000 RAYS_DELAUNAY_REPEATS=3 RAYS_REFINEMENT_POINTS=100000 RAYS_REGULARIZATION_POINTS=10000 RAYS_DELAUNAY_DOMAINS=1 dune exec -j 1 tools/bench_rdk/bench_rdk.exe -- delaunay2` |

### Kernel and generator contracts

Spatial-index and surface-index construction now split deterministic disjoint
subtrees above the configured grain. Attribute-transfer queries and payload
interpolation, mirror fills, fuse reductions/remaps, point compaction, and
Match Size also write disjoint output ranges in parallel.
The hashes verify exact ordering and payload equality across domain counts.

Clip accepts canonical `P` or scalar/int/float2/float3/float4 point coordinates;
only the first three components participate and missing components are zero.
Its finite distance translates the plane along the robustly normalized normal,
and its native clipped-edge output replaces a same-named source edge group.
Polygon-only unfilled cooks count source/side results in parallel, prefix them
in stable order, allocate exact output plans, and fill disjoint topology slots.

Subdivision topology indexing, manifold/fan validation, exact stencil-plan
construction, and stable child topology emission are sequential. Position and
each typed numeric payload plane evaluate into disjoint ranges in parallel.

Deletion planning is stable and sequential; position/attribute/group payload
copies use disjoint parallel ranges.

Box treats positive axis divisions `(dx, dy, dz)` as one integer lattice and
maps its six outward-wound faces into that lattice in stable right, left, top,
bottom, front, back order. Face-local output has
`2*((dx+1)*(dy+1) + (dy+1)*(dz+1) + (dz+1)*(dx+1))` points. Consolidated
surface output has `(dx+1)*(dy+1)*(dz+1) -
(dx-1)*(dy-1)*(dz-1)` boundary points, while the volume-lattice mode retains
the complete first product. The surface has
`2*(dx*dy + dy*dz + dz*dx)` cells: quads emit one primitive/four corners per
cell and triangles emit two primitives/six corners. Cardinalities are checked
before allocation.

Position, normal, UV, topology, offset, and face-group payloads are exact-sized.
Integer-coordinate helpers compute transforms and attributes inside their
single call sites, avoiding boxed float arguments on non-Flambda OCaml. Stable
point/cell ranges write disjoint slices; only the six face ranges and bounded
error flags are auxiliary. Smooth welded point normals are the normalized sum
of incident orthogonal face normals, so face interiors remain flat, edges
average two faces, and corners average three. Work is O(points + vertices +
primitives), auxiliary storage beyond published output is O(dx + dy + dz +
parallel ranges), and one/four-domain geometry is byte-identical.

UV Sphere treats `segments = s >= 3` as the longitude count and `rings = r >=
2` as the number of pole-to-pole latitude bands. There are `s*(r-1)` interior
points. Modes containing poles add two shared points or `2*s` unique
per-meridian points; row-only output omits unused poles. Regular and alternating
triangles emit `2*s*(r-1)` primitives and three times as many corners. Quad
output emits `s*r` primitives and `4*s*r` corners with logical quad poles, or
`4*s*(r-2) + 6*s` corners with triangular poles. Rows emit `r-1` open curves
and `s*(r-1)` corners; columns emit `s` open curves and `s*(r+1)` corners; the
combined mode is their exact sum. Every product and offset is checked before
allocation.

Latitude and longitude sine/cosine tables are O(r+s), computed once in stable
parallel ranges, and shared by position and normal generation. The compatible
triangle path retains its historical point/primitive order and exact normal
bits while replacing per-point latitude trigonometry and per-triangle geometric
winding classification with fixed outward index patterns. Ellipsoid normals
use inverse-radius gradient weights scaled by the minimum radius; a logarithmic
fallback covers finite anisotropy whose ratios underflow. Surface UVs are
vertex-owned so the longitude seam never crosses a primitive. Position,
normal, UV, topology, offset, and kind planes are exact-sized and filled in
disjoint stable ranges. Work/output are O(s*r), auxiliary storage beyond
published output is O(s+r+parallel ranges), and one/four-domain output is
byte-identical.

When triangular poles are disabled, each pole band intentionally remains a
four-corner polygon with one coincident adjacent edge. The shared triangulation
boundary recognizes exactly one such collapsed edge, verifies that the three
remaining positions have non-zero scale-normalized area, emits one triangle,
and remaps vertex attributes from the surviving corners. Arbitrary degenerate
or non-simple polygons continue to fail rather than being silently repaired.

Torus treats `rows = r` as U samples along the major sweep and `columns = c`
as V samples around the cross-section. A wrapped axis has one cell per sample;
an open axis includes both requested angle endpoints and has one fewer cell.
Writing `u = r` or `v = c` at a wrapped topology seam reuses point zero while
retaining vertex UV 1, so no polygon crosses the normalized seam. The point
count is exactly `r*c`. With `ur = r` or `r-1` and `vc = c` or `c-1`, surface
output has `ur*vc` cells: quads emit one primitive/four corners per cell and
triangle modes emit two primitives/six corners. Row output has `c` U curves
and `r*c` corners; column output has `r` V curves and `r*c` corners; combined
output is their sum.

For open U, U caps add two polygon primitives and `2*c` corners. For open V,
the V closure adds `ur` quads or `2*ur` triangles and four or six times `ur`
corners. Cap requests are rejected when their axis wraps, when connectivity is
not polygonal, or when a requested boundary is geometrically degenerate.
Signed U/V ranges are supported in every combination: surface winding is
reversed exactly when one parameter direction descends, and cap order/normals
follow the signed sweep. Smooth point normals retain the toroidal field; vertex
normals make U/V caps hard.

Sine, cosine, and normalized-parameter tables require O(r+c) auxiliary
storage. Position, point-normal, topology, vertex-normal, UV, offset, and kind
planes are exact-sized, and all dense fills use stable disjoint ranges with
cancellation polling. Work/output are O(r*c), auxiliary storage beyond the
published result is O(r+c+parallel ranges), and one/four-domain output is
byte-identical. No per-point/corner float boxes remain in the measured paths.

Tube treats `rows = r` as axial samples and `columns = c` as radial samples.
Each nonzero-radius row owns `c` points; a zero-radius end owns one shared apex.
When caps do not consolidate corner points, each capped nonzero end adds `c`
private points. For polygon output, let `t` be the number of apex-adjacent
bands (zero or one), `b = r - 1 - t`, and `k` the number of capped nonzero
ends. Quad mode emits `(b+t)*c+k` primitives and
`(4*b+3*t+k)*c` corners. Either triangle mode emits
`(2*b+t)*c+k` primitives and `(6*b+3*t+k)*c` corners. Thus a cone tip is a
ring of real triangles sharing one point, never a ring of coincident points or
degenerate quads. Longitudinal rows use `c` open curves and `r*c` corners;
radial columns use one closed curve and `c` corners for every nonzero row.

Side normals use the analytic frustum gradient and remain smooth across the
radial seam. Vertex normals keep cap corners hard; independent cap points also
give point-normal output a hard rim. Side UVs preserve `u = 1` at the topology
seam without duplicating positions, while cap UVs are planar. Position,
topology, normal, UV, and group planes are exact-sized. Radius/height and
trigonometric tables require O(r+c) auxiliary storage; generation is O(r*c)
time and output, with stable disjoint band fills, cancellation polling, and
byte-identical one/four-domain results.

Platonic Solids stores one normalized immutable coordinate/topology/face-normal
table for each regular polyhedron. Tetrahedron, cube, octahedron, icosahedron,
and dodecahedron retain their natural three-, four-, or five-corner faces. The
soccer ball is the exact edge-third truncation of the icosahedron: its 60
directed-edge points form twelve pentagons followed by twenty hexagons, with 90
two-manifold edges, black/white primitive `Cd`, and optional arity groups.
Every point lies at the requested circumsphere radius. Point normals are radial;
vertex normals are constant per polygon and preserve hard edges through terminal
N-gon triangulation.

All cardinalities are fixed and bounded by 60 points, 180 corners, and 32
primitives. Cook time, output, and auxiliary memory are therefore O(1).
Parallel dispatch is intentionally skipped because its setup exceeds the whole
solid generation cost; outputs remain byte-identical under one- and four-domain
sketch contexts.

Revolve consumes selected open or closed polygon curves and assigns one
deterministic output lattice to each profile. A full revolution has `d`
angular samples and wrapped edges; an open arc has `d+1` samples and includes
both requested signed-angle endpoints exactly. Points, angular rows, profile
columns, both curve families, quads, and the regular/reverse/checkerboard
triangle splits share one cardinality plan. Reversing the cross section changes
profile traversal and winding without mutating the source.

Each source corner normally owns one angular ring. A corner exactly on the
rotation axis instead owns one point: adjacent cells become `d` triangles and
an axis-to-axis cell emits nothing. This avoids coincident points and
degenerate pole quads while preserving stable profile order. Full polygon
surfaces may add one N-gon for each non-axis endpoint of an open profile;
axis endpoints are already closed and never receive a degenerate cap.

Source point, vertex, primitive, and detail payload and ordinary groups follow
packed ancestry maps. Point/vertex `N` is invalidated because revolution
changes its frame. Native input edges map to every generated profile edge by
parallel classification of the target unique-edge plane. Normalized profile
arc length is U and angular position is V; polygon surfaces store seam-safe
vertex UVs, while free-point output stores point UVs.

For selected profile corners `v`, emitted surface primitives `f`, and topology
corners `c`, work is O(v*d + f + c) and published storage is O(v*d + f + c).
Auxiliary storage is O(source vertices + output primitives), including ring,
profile-distance, and stable per-edge output-range planes. Surface edges write
disjoint ranges through the reusable domain pool; rule order, primitive order,
and every output hash are independent of domain count. All cardinalities,
finite positions, curve kinds/lengths, axis normalization, UV/group names, and
cancellation are validated before an immutable geometry is published. The
polygon subset is audited against the current
[SideFX Revolve SOP](https://www.sidefx.com/docs/houdini/nodes/sop/revolve.html).

Spiral emits one open polygon curve per phase copy. With `d` divisions and `s`
spirals, positions, vertices, and every requested point attribute have exactly
`s*(d+1)` entries, primitive offsets have `s+1` entries, and distance resets to
zero at every curve. Per-turn divisions use `ceil(turns*divisions_per_turn)`
segments and still include the exact requested endpoint. Archimedean radius is
linear in turns; logarithmic radius is geometric. Both accept change-per-turn
or exact end-radius controls before a positive global radius scale and a
piecewise-linear radius ramp. Height is `height*t*height_ramp(t)`.

Equal-angle sampling fills the shared per-curve parameter table directly.
Equal-arc sampling integrates the analytic ramped curve speed with fixed
five-point Gauss-Legendre quadrature. Its ordered integration partition merges
uniform parameter bins with every interior height/radius ramp knot, so a
derivative discontinuity is never integrated as a smooth interval. An ordered
prefix sum followed by binary bin lookup and four bounded Newton corrections
produces deterministic sample parameters. Multiple phase copies reuse the
same radius/height/trigonometric tables.

Requested X/Y/tangent axes form a right-handed orthonormal point frame. The Y
axis is the central axis projected perpendicular to the finite-difference
tangent; a stable radial fallback handles locally axial tangents. `orient`
encodes the same basis as a normalized float4 quaternion. Cumulative polygon
distance uses fixed 4,096-segment blocks: block totals cook independently,
their prefixes are accumulated in curve/block order, and disjoint final fills
retain byte-identical one/four-domain results. Point/frame hot loops allocate
no per-element boxes.

Let `n=d+1`, `s` be spiral count, and `k` be the total number of ramp keys.
Equal-angle work is O(n+s*n), while equal-arc work is
O(k*log k+n+k+n*log(n+k)+s*n); sorting is limited to interior ramp keys.
Published output is O(s*n). Shared profile/integration tables use O(n+k)
auxiliary storage, frame/distance block state is O(s*n/4096), and no output is
generated before cardinality and OCaml array limits are validated.

## Boolean intersection detection

`Rdk.Boolean.run` is the public exact polygon-product boundary over the single
RDK arrangement core. It builds the arrangement once, evaluates union,
intersection, either subtraction direction, or XOR for independently typed
solid/surface operands, transfers the complete supported payload schema through
exact ancestry, verifies rounded output, and optionally splits seam points or
detriangulates safe same-source patches. Point conflicts are explicitly
rejected or promoted to vertex ownership. Cut components with multiple
boundary cycles stay triangulated because the packed polygon representation
does not encode holes. Solid Shatter reuses that arrangement and emits stable
A-only/overlap/B-only boundaries with optional distinct primitive groups. The
function returns structured errors, honors the
shared cancellation token/grain, and produces exact ordered output across
domain counts.

`Rdk.Boolean.seam` exposes the same arrangement's seam products without
reimplementing intersection decisions. Curve output can name left-self,
between-input, and right-self primitive groups independently; coincident output
returns exact coincident triangle patches with an optional named group. Both
forms retain cancellation and exact ordered one-/multi-domain behavior.

`Rdk.Boolean_detect` is the packed diagnostic stage shared by one-input
AxA self detection and two-input AxB detection. It triangulates selected simple
polygons only inside immutable `Surface_index` values, preserving the source
topology and every existing payload plane. A two-pass BVH traversal first
counts and then fills exact-size triangle-pair arrays; no candidate list or
boxed bounds object is allocated in a leaf loop.

AxB retains left-triangle order. AxA retains only increasing internal-triangle
pairs from different source primitives, so reverse duplicates and one polygon's
triangulation diagonals never enter the narrow phase. Range-local float and
integer scratch normalizes each pair around a shared local origin, classifies
crossing/touching and optional coplanar overlap, and distinguishes genuine
self-overlap from contact that exists only through shared point IDs. Each AxA
primitive pair is written in both directions before per-primitive heapsort and
stable deduplication, producing symmetric `Int_array` rows.

Optional outputs are primitive groups, primitive `Int_array` intersecting-ID
rows, and primitive integer row counts. AxA and AxB output names must be
distinct within each metadata namespace. Installation is functional and
atomic: a later validation/cancellation failure cannot expose already
constructed intermediate metadata. Surface indexes, broad/narrow ranges, row
sorts, and group fills use the reusable `Parallel` pool; source topology,
positions, unrelated attributes, and groups remain structurally shared.

Expected time is O((A+B) log B + C + sum k log k) for AxB and
O(A log A + C + sum k log k) for AxA, with O(A+B+C+I) auxiliary storage for
surface indexes, candidates, and retained primitive entries. Exact sparse,
coplanar, and self-crossing baselines are maintained in
`specification/performance.md`. Classification uses an explicit locally scaled
floating tolerance; it is not the adaptive/arbitrary-precision predicate
required to construct topology-changing Boolean seams.

## Intersection point analysis

`Rdk.Intersection_analysis` consumes a packed BVH of triangle and
polygon-curve segment pieces and uses the same `Triangle_intersection` narrow
phase as Boolean Detect for triangle pairs. Its result is point-only geometry.
With one input it visits unordered AxA pairs;
with a collision input it visits ordered AxB pairs. Input geometry is never
passed through. Ordinary self-contact at a shared source vertex or edge is
suppressed, while duplicate faces and coplanar overlap beyond a shared edge
produce events.

The triangle/triangle, segment/segment, and segment/triangle narrow phases
write at most twelve pair-local records into worker-owned fixed scratch. A
count pass preflights exact event cardinality; a second pass fills disjoint
packed position and parameter planes. Stable spatial welding
then merges the same intersection location discovered through several
piece pairs. The weld threshold is the larger of the explicit world-space
tolerance and a bounded extent/ULP numerical floor. The earliest raw event
owns the published position, so work-stealing order cannot change coordinates
or point numbering.

Optional point-owned attributes default to `sourceinput`, `sourceprim`,
`sourceprimuv`, and `sourcepoint`. Their CSR rows are aligned by incident
primitive incidence. Triangle `sourceprimuv` records are barycentric;
polygon-curve records are `(u, 0, 0)`, where `u` spans the complete curve.
Every record has three floats, and `sourcepoint` stores the coincident source
point number or `-1`. Provenance is deduplicated by packed parameter incidence
in stable first-incidence order without a per-output hash table. Adjacent
segments share their internal endpoint incidence, while two distinct
parameters remain visible when one curve primitive crosses itself.

For A and B input linear pieces, C broad-phase candidates, E raw events, and R
retained provenance records, expected time is
O((A+B) log(A+B) + C + E) and auxiliary storage is O(A+B+C+E+R).
Mixed-piece index construction, count/fill narrow phases, and representative
position extraction use disjoint parallel ranges. Stable event welding and
provenance aggregation are currently serial and dominate dense coplanar
workloads. Selected polygons must be triangles; open and closed polygon curves
are processed as stable segments without a second topology representation.

## Research basis

The design was checked through 2026-08-04 against SideFX's primary documentation:

- [HDK geometry introduction](https://www.sidefx.com/docs/hdk/_h_d_k__geometry__intro.html)
  for point/vertex/primitive ownership, offsets, topology, and data IDs;
- [GA users guide](https://www.sidefx.com/docs/hdk/_h_d_k__g_a__using.html)
  for page handles, splittable ranges, and parallel traversal;
- [Geometry attributes 101](https://www.sidefx.com/docs/hdk/_h_d_k__geometry__attributes.html)
  and [GA Attribute](https://www.sidefx.com/docs/hdk/class_g_a___attribute.html)
  for typed bulk access and write-concurrence rules;
- [GA Range](https://www.sidefx.com/docs/hdk/class_g_a___range.html) and
  [GA PointGroup](https://www.sidefx.com/docs/hdk/class_g_a___point_group.html)
  for compact selections and group algebra;
- [Attribute Randomize](https://www.sidefx.com/docs/houdini/nodes/sop/attribrandomize.html)
  and [Attribute Remap](https://www.sidefx.com/docs/houdini/nodes/sop/attribremap.html)
  for artist-facing owner, group, distribution, range, extrapolation, and ramp
  contracts;
- [Attribute Copy](https://www.sidefx.com/docs/houdini/nodes/sop/attribcopy.html)
  for ordered cycling, value/index matching, class-independent copying,
  wildcard renaming, and explicit position policy;
- [Attribute Swap](https://www.sidefx.com/docs/houdini/nodes/sop/attribswap.html)
  for ordered copy/move/swap pairs, wildcard pairing, destination replacement,
  missing-side copy behavior, and canonical position move policy;
- [Enumerate](https://www.sidefx.com/docs/houdini/nodes/sop/enumerate.html)
  for selected point/vertex/primitive sequences, integer/text piece identity,
  per-piece element numbering, dense piece numbering, and text prefixes;
- [Rest Position](https://www.sidefx.com/docs/houdini/nodes/sop/rest.html)
  for store/extract/swap behavior, missing-rest Store fallback, second-input
  reference positions, and optional rest normals;
- [Point Velocity](https://www.sidefx.com/docs/houdini/nodes/sop/pointvelocity.html)
  for basic velocity initialization, finite-difference deformation,
  acceleration, point-ID matching, groups, and final additive velocity;
- [Dissolve 2.0](https://www.sidefx.com/docs/houdini/nodes/sop/dissolve.html)
  for selected/complementary edge removal, bridge and boundary policies,
  inline-point cleanup, unused-point removal, and normal regeneration;
- [Crease](https://www.sidefx.com/docs/houdini/nodes/sop/crease.html) for
  unique-edge selection, Add/Set/Delete creaseweight behavior, Subdivide
  integration, and optional vertex-color visualization;
- [Attribute Fade](https://www.sidefx.com/docs/houdini/nodes/sop/attribfade.html)
  for scalar point fading, point restriction, role-specific start/hold inputs,
  start retiming, timing attributes, transition ramps, and visualization;
- [Attribute Composite](https://www.sidefx.com/docs/houdini/nodes/sop/attribcomposite.html)
- [Attribute Mirror](https://www.sidefx.com/docs/houdini/nodes/sop/attribmirror.html)
- [Rewire Vertices](https://www.sidefx.com/docs/houdini/nodes/sop/rewire.html)
  for ordered weighted inputs, independent owner patterns, explicit P policy,
  same-owner alpha distribution, and Mean/Max/Min/Over/Under formulas;
- [PolyCut](https://www.sidefx.com/docs/houdini/nodes/sop/polycut.html) for
  point/edge restrictions, Remove/Cut behavior, crossing/change detection,
  threshold interpolation/subdivision, and closed-fragment policy;
- [Separate Pieces](https://www.sidefx.com/docs/houdini/nodes/sop/separatepieces.html)
  for integer/text identity, stored Move Back translation, and two-input
  union-bound intent;
- [Edge Equalize](https://www.sidefx.com/docs/houdini/nodes/sop/edgeequalize.html)
  for equal-length intent, average/longest/shortest initial targets, and
  transformed-edge output grouping; the numerical solver is not published;
- [Edge Relax](https://www.sidefx.com/docs/houdini/nodes/sop/edgerelax.html)
  for reference positions, individual/scale-independent edge targets,
  selection, pinning, iteration/step policy, shorten-only mode, and the
  intentionally deferred slide-on-surface and anisotropic controls;
- [Edge Transport](https://www.sidefx.com/docs/houdini/nodes/sop/edgetransport.html)
  for Each Curve, Edge Network, Parent Attribute, direction, root, operation,
  normalization, split/merge, constant/distance integration, and vector
  rotation behavior;
- [Attribute Combine](https://www.sidefx.com/docs/houdini/nodes/sop/attribcombine.html)
  for ordered arithmetic layers, source adjustment, tuple conversion,
  cross-input matching, blending, postprocessing, and cleanup policy;
- [Attribute Interpolate](https://www.sidefx.com/docs/houdini/nodes/sop/attribinterpolate.html)
  and [primitive parametric spaces](https://www.sidefx.com/docs/houdini/model/primitive_spaces.html)
  for destination ownership, primitive/UVW and explicit-weight modes, field
  ownership, and exact polygon/curve interpolation coordinates;
- [Ray](https://www.sidefx.com/docs/houdini/nodes/sop/ray.html) for closest and
  directional projection, direction/surface policies, distance bounds,
  tolerance, bounded seeded multi-ray jitter, combine policies, hit transforms,
  provenance, groups, and attribute import;
- [Fuse 2.0](https://www.sidefx.com/docs/houdini/nodes/sop/fuse.html) for query
  and target restrictions, near/grid/specified snapping, separate snapping and
  consolidation, rounding/tolerance, output groups, and payload heuristics;
- [Bound](https://www.sidefx.com/docs/houdini/nodes/sop/bound.html) for typed
  restriction, box/sphere/rectangle modes, divisions, padding, empty/keep
  policy, output groups, and transform/radii metadata;
- [Match Size](https://www.sidefx.com/docs/houdini/nodes/sop/matchsize.html) for
  independent move/justification groups, second-input and numeric references,
  per-axis alignment, offsets, scale-fit axes and perimeter/area/volume modes,
  plus transform stash/restore policy;
- [CGAL HalfedgeDS](https://doc.cgal.org/latest/HalfedgeDS/index.html) for the
  edge/corner incidence model and separation of combinatorics from geometry;
- [Shewchuk's adaptive predicates](https://www.cs.cmu.edu/~quake/robust.html)
  for the robust orientation/incircle boundary required by future topology-
  changing kernels;
- [UV Auto Seam](https://www.sidefx.com/docs/houdini/nodes/sop/uvautoseam.html),
  [UV Flatten](https://www.sidefx.com/docs/houdini/nodes/sop/uvflatten.html),
  and [Labs UV Unitize](https://www.sidefx.com/docs/houdini/nodes/sop/labs--uv_unitize.html)
  for artist-facing seam inputs, island output, and face/island unit fitting.
- [Mean value coordinates](https://doi.org/10.1016/S0167-8396(03)00002-5)
  for the positive harmonic weights and convex-boundary injectivity basis.
- [Line](https://www.sidefx.com/docs/houdini/nodes/sop/line.html),
  [Circle](https://www.sidefx.com/docs/houdini/nodes/sop/circle.html),
  [Grid](https://www.sidefx.com/docs/houdini/nodes/sop/grid.html),
  [Box](https://www.sidefx.com/docs/houdini/nodes/sop/box.html),
  [Sphere](https://www.sidefx.com/docs/houdini/nodes/sop/sphere.html),
  [Convert Line](https://www.sidefx.com/docs/houdini/nodes/sop/convertline.html),
  [Carve](https://www.sidefx.com/docs/houdini/nodes/sop/carve.html),
  [Join](https://www.sidefx.com/docs/houdini/nodes/sop/join.html),
  [Ends](https://www.sidefx.com/docs/houdini/nodes/sop/ends.html), and
  [PolyWire](https://www.sidefx.com/docs/houdini/nodes/sop/polywire.html) for
  polygon-curve parameters and the explicit spline/surface-only remainder;
- [PolyFrame](https://www.sidefx.com/docs/houdini/nodes/sop/polyframe.html) for
  frame styles, output ownership, optional orthogonalization, and the explicit
  MikkT/sign remainder;
- [Facet](https://www.sidefx.com/docs/houdini/nodes/sop/facet.html) for ordered
  normal, point-sharing, cleanup, orientation, cusp, and planar stages;
- [PolyFill](https://www.sidefx.com/docs/houdini/nodes/sop/polyfill.html) for
  all-hole/partial-boundary selection, fused versus unique patches, fill-mode,
  winding, point-normal, smoothing/deformation, and patch-group semantics.
- [PolyLoft](https://www.sidefx.com/docs/houdini/nodes/sop/polyloft.html) for
  closest-end pairing, two/three-point minimization, U/V wrap, rest-guided
  topology, source retention, generated grouping, and collinearity policy.
- [Skin](https://www.sidefx.com/docs/houdini/nodes/sop/skin.html) for linear
  polygon cross-section skinning, source retention, V wrap, explicit polygon
  output, and the audited spline/two-input surface boundary.
- [PolyBridge](https://www.sidefx.com/docs/houdini/nodes/sop/polybridge.html)
  for edge-loop/path inputs, multiple bridge pairing, reverse/shift controls,
  unequal boundary compensation, input retention, and the explicitly deferred
  spine/division/thickness/twist surface.
- [PolyBevel](https://www.sidefx.com/docs/houdini/nodes/sop/polybevel.html) for
  edge eligibility, cutback/profile/collision controls, connected junctions,
  scale attributes, generated groups, and the explicitly audited remainder.
- [Point Split](https://www.sidefx.com/docs/houdini/nodes/sop/splitpoints.html)
  for selected shared-point separation, vertex/primitive seam attributes,
  per-dimension tolerance, group seams, and optional point promotion;
- [Extract Centroid](https://www.sidefx.com/docs/houdini/nodes/sop/extractcentroid.html)
  for whole-detail, primitive, and piece center modes and output metadata;
- [Extract Point from Curve](https://www.sidefx.com/docs/houdini/nodes/sop/extractpointfromcurve.html)
  for constant, primitive-attribute, and current-time target sources, point and
  primitive payload patterns, curve U, cut count, and source-curve metadata;
- [Circle from Edges](https://www.sidefx.com/docs/houdini/nodes/sop/circlefromedges.html)
  for selected geometry boundaries, optional best-fit/explicit radius and
  scale, and transformed-edge group output;
- [Graph Color](https://www.sidefx.com/docs/houdini/nodes/sop/graphcolor.html)
  for point/primitive connectivity, optional color sorting, and workset fields;
- [Labs Measure Curvature](https://www.sidefx.com/docs/houdini/nodes/sop/labs--measure_curvature-3.0.html)
  for the exposed curvature families, smoothing, visualization, and modeling
  uses;
- [Measure 2.0](https://www.sidefx.com/docs/houdini/nodes/sop/measure.html)
  for attribute Laplacian, integrated/area-divided output, and its smoothing
  and sharpening interpretation;
- [Laplacian](https://www.sidefx.com/docs/houdini/nodes/sop/laplacian.html)
  for cotangent and Tutte weight policy, mass separation, and the distinct
  sparse-matrix output use case;
- [Meyer, Desbrun, Schroder, and Barr, Discrete Differential-Geometry Operators for Triangulated 2-Manifolds](https://authors.library.caltech.edu/records/0rsjd-50h08)
  for mixed Voronoi areas, cotangent Laplace-Beltrami mean curvature, and angle
  defect Gaussian curvature;
- [Wardetzky et al., Discrete Laplace Operators: No Free Lunch](https://diglib.eg.org/items/43d99127-69ae-464d-aa71-497b72b41a0b)
  for the incompatible symmetry, locality, linear-precision, positivity, and
  convergence properties that motivate explicit signed/positive/uniform modes;
- [Point Generate](https://www.sidefx.com/docs/houdini/nodes/sop/pointgenerate.html)
  for origin and input-point emission, count scaling versus probability,
  original-point retention, attribute-copy patterns, generated grouping, and
  source-point/local-index metadata.
- [Point Replicate](https://www.sidefx.com/docs/houdini/nodes/sop/pointreplicate.html)
  for shaped source-point clouds, standard instancing transforms, stable ID and
  rest behavior, quasi sampling, velocity controls, source payload/provenance,
  custom shape ancestry, and generated groups.
- [Boolean 2.0](https://www.sidefx.com/docs/houdini/nodes/sop/boolean.html)
  for the Detect-mode AxA/AxB group/list contract, one-input self behavior,
  polygon triangulation policy, and separation from Seam/Resolve output.
- [Intersection Analysis](https://www.sidefx.com/docs/houdini/nodes/sop/intersectionanalysis.html)
  for one/two-input roles, point-only output, primitive restrictions, linear
  triangle/polygon-curve scope, and optional input/primitive/UVW/point
  provenance attributes.

No SideFX implementation was copied. This is a native OCaml design based on
the documented data model and execution constraints.
