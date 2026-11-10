# Modeling kernel architecture and research

Status: active migration guide. Last reviewed 2026-08-04.

## Product boundary

The target is a production procedural-geometry modeling toolset: polygon and
curve construction, attributes/groups, topology editing, subdivision, UVs,
instancing, spatial queries, repair, Boolean operations, remeshing, reduction,
and deterministic exchange. Rigging, crowds, dynamics, fluids, pyro, Vellum,
MPM, compositing, and general VFX solver systems are not part of this program.

Real-time iterative creative coding remains in scope. A sketch may feed the
previous immutable geometry snapshot into the next fixed-timestep cook, but
feedback crosses the `Sketch.run_state` model boundary explicitly; a per-frame
SOP graph remains acyclic and its cache remains bounded.

## One authoritative core

```text
Sop ──> RDK ──> Rays.Mesh
```

RDK owns packed geometry, reverse topology, attribute interpolation/reduction,
spatial acceleration, and high-density modeling algorithms. `rays_math`
owns mathematical values; `rdk_rays` converts cooked geometry to render
meshes. Sop owns graph composition, context dependencies, diagnostics,
and bounded evaluation. RDK never imports upward.

`Rdk.Topology` remains the compact forward point/corner/primitive structure.
`Rdk.Topology_index` is a derived, immutable reverse view with packed
next/previous corners, manifold opposites, integer-key undirected edges,
edge-incidence CSR, and point-corner CSR. This follows the established
separation between combinatorial half-edge storage and geometric
interpretation described by the
[CGAL HalfedgeDS manual](https://doc.cgal.org/latest/HalfedgeDS/index.html),
while using index planes rather than per-element objects.

## Kernel ownership

| Existing area | Current value | RDK migration decision |
|---|---|---|
| Weld | Attribute-compatible spatial weld | `Rdk.Fuse_grid.fuse` owns the packed implementation |
| Topology | Triangle adjacency and immutable editing | `Rdk.Topology_index` owns the reverse index |
| Extrusion/revolve/sweep | Cardinality-first generators | `Rdk.Poly_extrude.run`, `Rdk.Sweep_modeling.revolve`, and `sweep` own topology and payload output |
| Loop/Catmull-Clark | Surface subdivision | `Rdk.Subdivide.subdivide` owns the packed kernel |
| Edge subdivision | One packed owner | `Rdk.Subdivide.edge_divide` owns the kernel; Sop wraps it |
| Edge collapse | No former single owner | `Rdk.Edge_collapse` owns selected-edge component planning and delegates packed reduction, rewiring, and cleanup to the single Fuse core; it is the contraction primitive for future reduce/remesh work |
| Blend Shapes | No former single owner | `Rdk.Blend_shapes` owns target ordering, masks, point-ID matching, and packed fixed-width point-field interpolation; Sop stores only immutable target descriptors and never caches mutable deltas inside a node |
| Attribute Composite | No former single owner | `Rdk.Attribute_composite` owns independent owner-pattern discovery, ordered weighted/alpha composition, direct-index cardinality and finite-value validation, exact packed output planes, and stale-normal policy; Sop stores only immutable ordered input descriptors and complete cache identity |
| Attribute Mirror | No former single owner | `Rdk.Attribute_mirror` owns reflected point/primitive correspondence, explicit point/vertex/primitive mapping, packed payload remap and transformations, pair/side metadata, validation, and stale-normal policy; Sop stores only names and immutable parameters, resolves groups at cook time, and never retains a competing correspondence or topology kernel |
| Rewire Vertices | No former single owner | `Rdk.Rewire_vertices.run` owns owner-dependent selection promotion, recursive point-map resolution, corner-to-point mutation, newly-unused point compaction, and union corner-edge ancestry; Sop stores only typed names/policies and never edits topology or retains mutable connectivity |
| Polygon reduction | No former single owner | `Rdk.Poly_reduce` owns adaptive QEM scoring, deterministic independent contraction batches, hard-feature/boundary policy, manifold link and foldover checks, and delegates every committed packed contraction/remap to the shared Edge Collapse/Fuse core; Sop only resolves named groups and cache identity |
| Boolean intersection detection | Formerly only boxed `Csg3` BSP classification | `Rdk.Boolean_detect` owns deterministic surface triangulation, two-pass packed AxB and unordered AxA BVH triangle-pair discovery, locally normalized narrow-phase classification, topology-contact suppression, symmetric self-pair aggregation, cancellation, and exact-size group/CSR outputs; Sop resolves optional one/two-input roles, two named primitive restrictions, and immutable cache identity. This reusable detection stage does not replace the future robust corefinement kernel |
| Intersection event analysis | No former packed owner | `Rdk.Intersection_analysis` owns one packed mixed triangle/curve-piece BVH, reuses the shared `Triangle_intersection` decision/event kernel for triangle pairs, supplies fixed-scratch segment/segment and segment/triangle events, preflights exact raw cardinality, welds stable point identities, and emits aligned input/primitive/parameter/incident-point CSR provenance; Sop owns only graph roles and named-group resolution |
| Polygon bevel | No former single owner | `Rdk.Poly_bevel.run` owns selected-edge eligibility, face-ring slide/collision planning, cross-face ring splits, connected continuation/corner topology, profile sampling, cardinality-first packed output, payload ancestry, and generated groups; Sop only resolves a named native edge group and immutable parameters |
| Point splitting | One packed owner | `Rdk.Point_split` owns selected-incidence classification, mixed vertex/primitive attribute and named-group seam tuple clustering, stable point allocation, promotion, and one-to-many point/group/native-edge ancestry; Sop only resolves typed groups and immutable seam policy |
| Point generation | No former single owner | `Rdk.Point_generate` owns exact cardinality planning, deterministic per-source emission, every point-storage copy path, provenance, generated grouping, and retained-topology extension; Sop supplies generator/modifier graph identity and resolves an optional named point group |
| Point replication | No former single owner | `Rdk.Point_replicate` reuses Point Generate cardinality/payload planning and the canonical Copy-to-Points basis transform, then exclusively owns source-keyed local shape sampling, copied-vector/normal transformation, quasi coordinates, rest-space noise, velocity synthesis, and custom-shape ancestry; Sop only resolves graph inputs/groups and immutable identity |
| Geometry distance fields | No former single owner | `Rdk.Transform_ops.distance_along_geometry` owns exact edge-path propagation, `Rdk.Transform_ops.distance_from_geometry` owns point/surface reference queries, and `Rdk.Transform_ops.distance_from_target` owns analytic point/axis/plane projection; all reuse the shared falloff/output policy and packed storage rather than placing distance kernels in Sop graph cooks |
| Packed element ordering | No former single owner | `Ordering.sort` owns stable point/primitive permutation, deterministic random shuffle, strict index-permutation validation, indirect destination ranks, and complete payload/topology remapping; Sop contributes only immutable cache identity and named-group resolution |
| Attribute-driven deletion | No former single owner | `Rdk.Blast_by_attribute` owns packed scalar point/primitive classification and delegates every topology/payload/group/native-edge mutation to the single `Deletion.delete` planner; Sop resolves only the optional named base group and immutable node identity |
| Edge crease authoring | No former single owner | `Rdk.Crease.crease` owns unique-edge reduction, coherent incident-corner `creaseweight` updates, and optional vertex-color endpoint visualization for the existing Subdivide kernel; Sop resolves only a named topology-affine edge group and cache identity |
| Frame-domain attribute fading | No former single owner | `Rdk.Attribute_fade` owns scalar point-driver validation, affine frame retiming, in/hold/out ramp evaluation, independent reference-cardinality policy, packed output, and grayscale visualization; Sop declares the exact Frame dependency, resolves only a named point group/input roles, and never embeds mutable solver state in the cook |
| Polygon-curve cutting | No former single owner | `Rdk.Poly_cut.cut` owns point/edge event classification, threshold interpolation, change subdivision, fragment planning, point compaction, every-owner payload/group/native-edge ancestry, and closed-fragment policy; Sop resolves only named primitive/point/edge groups and immutable parameters |
| Reversible piece separation | No former single owner | `Rdk.Separate_pieces` owns integer/text identity compilation, point/primitive rigidity validation, stable projected bounds and layout, same-owner translation metadata, Move Back arithmetic, overflow checks, and packed position fills; Sop contributes immutable parameter identity only |
| Edge flip | No former single owner | `Rdk.Edge_flip` exclusively owns manifold polygon-boundary rotation, corner-payload cycling, validity checks, and native-edge ancestry; Sop contributes immutable selection/parameter identity only |
| Edge cusp / Facet cusp | Formerly Facet-local fan splitting | One `Facet.split_points_on_edge_ends` packed kernel owns point-fan partitioning and one-to-many payload/group/native-edge ancestry; `Rdk.Facet.edge_cusp` supplies explicit path-end masks while Facet supplies dihedral masks |
| Edge straightening | No former single owner | `Edge_ops.straighten` owns selected-edge components, scale-normalized covariance fitting, deterministic principal-axis selection, and packed point projection; Sop only resolves named groups and node identity |
| Edge length equalization | No former single owner | `Edge_ops.equalize` owns target reduction, selected incidence planning, the independent-edge exact path, deterministic connected projection, convergence and finite-result policy, stale-normal invalidation, and packed coordinate output; Sop only resolves named groups and immutable solver parameters |
| Reference edge relaxation | No former single owner | `Edge_relax.relax` owns matching-topology validation, individual/scale-independent reference targets, movable/pinned incidence planning, shorten-only policy, a closed-form independent-edge path, and delegates connected iterations to the shared `Edge_constraints` projector; Sop owns only two-input roles, named-group resolution, and immutable parameters |
| Boolean | Exact corefinement | `Rdk.Boolean` owns the arrangement and extraction pipeline |
| Delaunay/Voronoi | Planar modeling | `Rdk.Delaunay2` and `Rdk.Voronoi2` own exact-predicate results |
| Isosurface | Scalar-field extraction | `Rdk.Iso_surface` owns packed output |

Public algorithm replacements keep captured compatibility and scale fixtures
before an old implementation is removed.

## Algorithm requirements

### Subdivision

Subdivision is not just a position stencil. The production contract includes
scheme selection, boundary interpolation, vertex/edge creases, holes,
face-uniform attributes, vertex/varying attributes, and independently indexed
face-varying data such as UV seams. Pixar OpenSubdiv explicitly separates these
channels and options in its
[scheme options](https://graphics.pixar.com/opensubdiv/docs/doxy_html/a01316.html),
[topology representation](https://graphics.pixar.com/opensubdiv/docs/doxy_html/a01121.html),
and [primvar refiner](https://graphics.pixar.com/opensubdiv/docs/doxy_html/a01029.html).

RDK precomputes a stable refinement plan from `Topology_index`: exact
output cardinalities, parent-child topology, and packed weighted stencils.
Position and each compatible attribute plane then evaluate those immutable
stencils into disjoint output slices. Vertex, varying, face-uniform, and
face-varying interpolation remain distinct. This makes repeated deformation
over fixed topology cheap and makes parallel scheduling unable to change
indices or summation order.

The implemented production subset provides Catmull-Clark, Loop, and bilinear
schemes; exact recursive cardinality; OpenSubdiv None, Edge Only, and Edge and
Corner point-boundary interpolation; all six OpenSubdiv face-varying
interpolation policies (None, Corners Only, Corners Plus 1, Corners Plus 2,
Boundaries, and Linear All), including tuple-wide seam classification and
crease/corner precedence; standard and Smooth Triangles Catmull-Clark edge
masks with triangle/quad interpolation and sharp-edge precedence;
point/vertex/primitive/detail payload remap; group propagation;
semi-sharp `creaseweight` with Uniform or Chaikin child decay and exact
parent/child edge and vertex-rule transitions; uniformly decayed `cornerweight`;
and rejection of repeated polygon
points; point-number topology-input creases; stencil-contributing/finally
removed hole faces; the complete local refinement/crack-closure matrix; and
rejection of non-manifold edges, invalid boundary incidence, and disconnected
vertex fans. Remaining OpenSubdiv-relative work is explicit in the SOP audit:
hierarchical edits, detail-attribute option overrides, and limit output.

### Robust topology changes

Ends is a cardinality-first closure edit in the packed curve core. It remaps
corners and optional duplicated seam points through explicit target-to-source
planes, rather than rebuilding topology in Sop. Native-edge ancestry is
derived from source directed corners, so face unroll can split one shared edge
into multiple exact descendants without accidentally selecting a newly authored
closing edge.

Tolerance is a user policy for proximity; it is not a replacement for a
correct orientation or intersection sign. Ear clipping, Delaunay, planar
arrangements, clipping, corefinement, and remeshing need filtered predicates:
fast ordinary floating-point evaluation when safely separated from zero, with
adaptive exact evaluation near ambiguity. Shewchuk's
[adaptive orientation/incircle work and public-domain predicates](https://www.cs.cmu.edu/~quake/robust.html)
is the reference for the predicate layer.

Constructed intersection coordinates remain exact homogeneous values until
the final materialization boundary. Cached floating approximations and
certified intervals accelerate ordinary decisions, but all combinatorial
branching uses a consistent exact predicate policy. Degenerate and coplanar
cases receive explicit classifications and deterministic tie breaks; they
must never depend on hash iteration or work-stealing order.

`Rdk.Predicates` now provides the first production predicate layer: filtered
exact `orient2d` and `orient3d` signs over every finite binary64 input. The
fallback converts the original IEEE mantissa/exponent pairs into bounded
base-2 dyadic integers, so cancellation, overflow, and underflow cannot change
the returned sign. Scalar entry points are conveniences; packed SoA/index
entry points are the topology-kernel boundary and allocate nothing on the
certified floating fast path. The same module supplies exact packed
segment/triangle and non-coplanar triangle/triangle classification. Intersection
events are canonical pairs of the lowest-dimensional source feature on each
triangle, so edge/edge events discovered from both directions deduplicate by
integer identity without a coordinate tolerance.

The Boolean kernel now also owns a single exact dyadic construction engine and
the first implicit-point layer. Explicit vertices, line-plane intersections
(LPI), projected coplanar line-line intersections, and triple-plane
intersections (TPI) retain their source-index recipes
and exact normalized homogeneous coordinates. Ordinary LPIs retain a compact
recipe plus certified scalar coordinate-error bounds and materialize/cache the
homogeneous value atomically only when an ambiguous predicate needs it;
ill-conditioned construction falls back eagerly to exact arithmetic. Equality, axis ordering,
projected orientation, and 3D orientation operate directly on those
homogeneous values, including mixed explicit/LPI/TPI inputs; certified
outward-rounded coordinate bounds keep ordinary comparisons, projected/3D
orientations, and projected incircle tests on zero-allocation filters, while
ambiguous signs fall back to exact homogeneous arithmetic. Cartesian
approximations are acceleration/presentation facts only. Construction rejects
parallel lines/planes, dependent planes, non-finite sources, and invalid
indices explicitly. Projected line-line construction has the same certified
deferred fast path and exact fallback; regressions include all three
projections, reversed recipes, 1,500 deterministic integer constructions, and
an almost-parallel subnormal case. Exact three-point centroids support cell
queries without manufacturing a floating offset, and an exact/filterable
perpendicular radial dot predicate resolves coplanar angular ties.

`Rdk_boolean.Boolean_constraints` now implements phases 2 and 3 for two
surfaces and the packed input to phase 4. It concatenates source coordinate
planes once, gets stable BVH candidates, classifies candidates exactly in
parallel ranges, constructs LPI endpoints, sorts/deduplicates them by exact
homogeneous identity, and emits typed-operand point/segment constraints plus
per-operand triangle CSR. The common clean-input path performs only the A×B
broad phase and borrows its packed candidate planes directly. Exact A×A and
B×B broad phases are independently opt-in, matching Boolean 2.0's per-input
self-intersection controls without charging known-clean solids for two extra
BVH traversals. Ordinary shared vertices/edges are suppressed by exact
topological tests, while a crossing that extends from shared topology and
positive-area coplanar folds remain arrangement events. Coplanar and
degenerate pairs are retained in separate queues;
they are never silently treated as ordinary crossings. Equivalent edge/plane
recipes prefer the source edge with more exactly constant components, which
preserves singleton coordinate certificates and avoids unnecessary exact
materialization without changing the point. Constraint regressions cover
crossings, single-point contact, coplanar/disjoint pairs, shared-face endpoint
deduplication, same-operand transverse/coplanar crossings, ordinary-topology
suppression, the clean-input policy, cancellation, and exact one/four-domain
output. Coplanar arrangement and face CDT remain deliberately downstream of
this candidate and constraint plan.

`Rdk_boolean.Boolean_coplanar` now consumes every exact-coplanar candidate pair
as a separate planar arrangement. It classifies both triangles' vertices with
exact projected orientations, constructs every proper edge crossing as an
exact projected line-line point, deduplicates by homogeneous identity, and
extracts the convex overlap boundary with an exact monotone hull. The result
distinguishes empty AABB false positives, point contacts, coincident segments,
and positive-area polygons. Regressions cover six-edge overlap, containment,
identical opposite-winding facets, shared-edge and point contact, disjoint
AABB candidates, cancellation, and identical one/four-domain results. Pair
jobs write only to disjoint stable output slots. Exact construction remains
allocation-bound, so concurrency is not claimed as a speedup until the packed
expansion arena lands.

The next private stages consume both non-coplanar constraints and coplanar
overlap boundaries per affected face. The exact face arrangement inserts
proper crossings as TPI, line-plane, or projected line-line points, propagates
multi-way/T-junction splits to every incident segment, orders split points with
exact axis comparisons, and removes duplicate subsegments. The face CDT then
inserts all points into the source triangle, recovers every constraint by
exact crossing tests and convex flips, and applies exact projected incircle
tests to unconstrained edges. A cocircular zero uses a stable canonical
diagonal key, providing the required deterministic symbolic tie-break. Batch
refinement runs independently affected faces over stable parallel ranges and
produces identical triangle/constraint arrays for one and four domains.

The downstream private pipeline is now executable end to end. `Complex`
deduplicates all exact vertices, merges identical refined facets into one
geometric facet while retaining every operand/source-face/winding contribution,
and builds packed edge-incidence CSR. `Radial` orders three-or-more incident
charts exactly around non-manifold seams; ordinary one/two-chart edges take a
no-predicate fast path. `Weiler` connects the positive/negative half-facet
sides through each radial sector and builds local cell shells with signed
two-operand winding deltas. `Cells` chooses an exact facet centroid, rejects
axis rays that hit a boundary, cascades over both signs of X/Y/Z, seeds one
shell per disconnected graph component, and propagates exact integer winding
vectors. If all six axes are boundary-degenerate, it uses the formal
positive-infinitesimal direction `(1, epsilon, epsilon^2)` and the first
non-zero coefficient of each exact ray-edge and normal polynomial; there is no
retry bound and no coordinate is nudged or rounded. A source triangle that
contains the seed but is absent from its exact refined-facet membership is an
invariant error with source IDs. A packed component AABB tree eliminates
provably zero winding components before triangle predicates. Signed-axis
queries use a certified floating filter over packed source/query values and
fall back to one exact arena decision; a private exhaustive component scan and
the former point-object formulation remain differential oracles. `Extract`
evaluates a typed
`Left`/`Right`/`Not`/`And`/`Or`/`Xor` expression, orients every boundary from
inside to outside, rounds exact vertices once, and rejects open/odd-incidence
solid output. Its optional ancestry product selects a stable contributing
member per output facet and retains operand, original polygon primitive,
internal source triangle, refined child, source point and source vertex IDs,
output-relative winding, and exact-construction-derived barycentric weights
for every output corner. Transverse regressions reconstruct every rounded seam
corner from those weights; n-gon regressions keep primitive, triangulation,
point, and vertex identities distinct. Geometry-only extraction does not
allocate or calculate this optional payload. Regressions cover disjoint, nested, transverse, identical, and
face-touching tetrahedra; union, intersection, both differences, XOR, and a
custom expression; one/four-domain exactness; outward volume; and region
volume partition identities.

`Rdk_boolean.Boolean_seam` derives a second product from that same prepared complex
without repeating intersection or face refinement. It classifies exact complex
edges as left-self, between-operands, or right-self; source-native edge ancestry
prevents an ordinary non-manifold input edge from becoming a false
self-intersection. Stable vertex/kind/edge ordering chains every selected edge
exactly once into open paths or closed loops, terminating at branch vertices,
and retains complex-edge CSR ancestry per curve. Facets owned by both operands
or by multiple faces of one operand are emitted once as a separate
coincident-area geometry. Exact source-edge ancestry uses direct
explicit-edge/implicit-point arena predicates rather than rebuilding retained
explicit point objects per incidence. Both products reuse
the extraction stage's post-rounding collision validation. Packed masks,
degree/adjacency planes, exact cardinality upper bounds, cancellation, a
benchmarked sequential cutoff, and tunable stable parallel classification keep
the stage deterministic and bounded. Tests cover transverse open seams, closed
solid loops, partial and full coincidence, both self-intersection sides,
native non-manifold adjacency, cancellation, malformed scheduling parameters,
and forced one-/four-domain output identity.

After binary64 materialization, seam curves pass through a packed
linear-piece BVH and an exact 3D segment/segment contact predicate. Exact
coplanarity rejects skew pairs before a stable XY/YZ/ZX projection;
non-collinear point contact and positive-length collinear overlap remain
distinct, including across the full normal/subnormal exponent range. A
deterministic range reduction reports the lexicographically first invalid
curve/segment pair without materializing the potentially quadratic candidate
list. Shared point IDs are valid incidence, while a crossing without shared
identity or any overlap is an output error. The same pass verifies curve-edge
ancestry cardinality and uniqueness. Houdini's documented “Collapse tiny
seam-adjacent edges” applies to the extracted surface around seams, not to the
polyline seam product; that separate topology-changing cleanup remains gated
on explicit threshold, payload, ancestry, and re-verification policy.

`Rdk_boolean.Boolean_payload.copy_primitives` consumes that ancestry without
re-running extraction. It transfers the union of both operands' primitive
attribute schemas across Float, Int, Text, Float2/3/4, Int-array, and
Float-array storage; a field absent on the selected source operand receives
its typed zero/empty default, while a same-name storage conflict is an explicit
error. Primitive groups use the same selected source primitive and default to
false when absent. Fixed-width planes and CSR row count/fill passes write
disjoint output ranges, cancellation is atomic, and the output structurally
shares the Boolean positions/topology. Non-primitive payload already installed
on the extracted geometry is retained. Ordered primitive groups retain each
operand's source traversal, merge the left operand before the right, and keep
stable output-facet order among the one-to-many descendants of one source
primitive.

`Rdk_boolean.Boolean_payload.copy_points_and_vertices` completes the private
corner-payload boundary over the same ancestry. It supports every RDK storage
kind independently on Point and Vertex owners. Float and Float2/3/4 values use
the stored exact-construction barycentrics; `N` Float3 values are normalized
with a scale-safe norm. Int, Text, and Int-array values use the stable dominant
barycentric source corner. Equal-width Float-array rows interpolate
componentwise, while unequal rows use that same dominant-corner rule. Missing
operand fields receive typed defaults and same-name storage mismatches are
errors. Vertex payload remains Vertex. Point payload either agrees at every
incident output corner under an explicit finite, non-negative metric tolerance
or the caller explicitly promotes it to Vertex; discrete payload and group
membership always require exact agreement. Promotion name collisions are
errors rather than implicit overwrites. Ordinary and ordered point/vertex
groups follow the same policy, with ordered traversal stable within the left
operand before the right. The output shares the extracted positions/topology
and retains already copied Primitive/Detail payload. Native edge groups use a
separate exact directed-edge CSR: extraction examines every coincident complex
member, retains a `(side, source edge)` token whenever both exact output
endpoints lie on an original topology edge, and payload transfer unions those
tokens by group name. This deliberately does not derive edge ownership from
the preferred primitive member. It therefore preserves groups authored only
on a non-preferred identical operand and follows source edges split by an
intersection construction. Missing-side membership defaults to false, and
unrelated preinstalled output edge groups remain intact.
Direct tests cover all storage classes, unequal ragged rows, affine seam
agreement, discontinuous attribute/group seams, both conflict policies,
cancellation, malformed schema/target/tolerance inputs, cross-operand and
same-operand coincident edge members, split native edges, and exact
one/four-domain output.

Ancestry extraction now computes each corner's barycentrics directly between
the packed explicit source triangle and its implicit output point. One
domain-local exact arena certifies coplanarity, selects a non-degenerate
projection, evaluates all three oriented-area ratios, and releases its scratch;
no temporary explicit `Implicit_point.t` objects or per-facet point array are
retained. Native-edge ancestry uses the same direct source-segment containment
boundary. The former explicit-object implementation remains a differential
oracle, not a fallback authority.

Before topology creation, materialization rejects two exact vertices that
round to one binary64 coordinate and rejects any output triangle that becomes
exactly collinear after rounding. Rounded-coordinate identity uses a packed
open-addressed table; triangle validation chooses the dominant approximate
normal projection and certifies it with the exact binary64 orientation
predicate. Failures report `rounding_collision` or `rounding_degenerate`
instead of returning a silently invalid solid. Tiny-edge consolidation and a
post-cleanup seam-intersection verification remain separate release gates.

`Rdk_boolean.Boolean_solid` is the private transactional boundary over those
stages. `prepare` owns one exact arrangement, radial graph, and classified cell
complex; repeated `extract` calls evaluate different typed expressions without
repeating intersection or classification work. It validates stage identity,
rejects degenerate source surfaces, exposes independent left/right
self-intersection resolution policies, propagates cancellation, and has exact
one/four-domain regressions. Transverse overlapping shells resolve by integer
winding, coincident duplicate shells collapse to one boundary, and
multi-member ancestry remains available to payload transfer. This
build-once/query-many value is also the
intended cache unit for a future public variadic CSG graph, but the current
transaction deliberately remains two-operand and private.

This is not yet the final high-density face implementation. Face arrangement
exact-canonicalizes its initial input points with a stable O(p log p) sort,
then uses certified implicit-point intervals in an adaptive projected
sweep for segment/segment and point/segment candidates. Only conservative
two-axis AABB candidates reach exact orientation/construction predicates.
Candidate pairs are restored to legacy `(segment, segment/point)` order before
mutation, so the optimized and reference paths retain identical point IDs,
segments, and one/four-domain output. Candidate storage is explicitly capped.
An interval-dense face switches to a packed stable-index BVH whose balanced
index ranges stream conservative candidates in the same order with linear
index/scratch storage; the unculled traversal remains a forced differential
oracle rather than the production fallback. Constructed crossings are interned
online in a packed exact-coordinate AVL index, and an integer pair set removes
duplicate `(segment, point)` incidences. A bounded two-level identity check
reuses an already certified exact event when many segment-pair observations
meet at the same point, without allowing high-valence lookup to become cubic.
The CDT trusts this exact uniqueness and tests only its three source corners at
the adapter boundary, removing another unique-point quadratic scan.

The CDT now maintains a specialized packed open-addressed edge-incidence table
through point splits and diagonal flips. Walking point location follows that
live adjacency and falls back atomically to the retained exact global scan if
it cannot progress. Default constraint recovery walks the first endpoint's
incident triangle fan, traces the constraint through the crossed triangle
chain, and flips the smallest-key convex crossing in that chain. This preserves
the earlier deterministic recovery order while making the common work local.
A bounded minimum-key dirty-edge heap scans packed table
slots on refill and rechecks only the two changed triangles during Delaunay
repair. Constraint-edge tests use certified projected interval rejection
before exact orientation; neither a walk nor an interval filter makes a
combinatorial decision approximately. An unexpected broken trace falls back
to the complete exact edge oracle instead of dropping an event. Dense tests
compare both point locators, both recovery modes, every triangle and recovered
constraint, and one/four-domain output exactly.

The arrangement now has a linear-memory dense BVH path, but its honest output-
sensitive worst case remains O(segments² + segments*points): a face may contain
quadratically many distinct crossings. Multi-way coincident events aggregate
to one exact point and one incidence per segment. The CDT's traced path is
local in endpoint valence and
crossed edges/flips; its O(constraints*active_edges) exhaustive mode is now
only a compatibility oracle and an audited invariant-failure fallback. The exact
arrangement/Weiler path remains private because several production gates are
still open: relation-level topological aggregation as a separately reported
diagnostic or materialized-import recovery policy, a public schema policy
over the implemented primitive/point/vertex/native-edge payload transfer,
surface/shatter/seam products, bounded tiny-edge rounding cleanup and seam
self-intersection verification,
and a packed retained-construction representation for dense LPI/TPI ancestry.
Nonescaping exact orientation, comparison, incircle, ray-edge,
normal-direction, and radial-dot temporaries now use a reusable domain-local
limb arena; the immutable implementation remains a differential oracle. No
public SOP claims Houdini parity while
those contracts remain incomplete.

### Boolean operations and repair

A production Boolean is an exact mesh-arrangement problem, not merely BSP
polygon classification. Rays targets the capabilities of Houdini Boolean
2.0 while using the more recent exact Weiler-arrangement model as its internal
correctness contract. The required result is a conforming simplicial complex
whose intersection curves are topology edges, with enough radial/cell
information to evaluate multiple Boolean expressions without repeating
intersection work.

The design is based on complementary primary references:

- [Mesh Arrangements for Solid Geometry](https://www.cs.columbia.edu/cg/mesh-arrangements/)
  supplies variadic operand labels, generalized winding vectors, dirty-input
  and self-intersection semantics, and expression-based cell extraction.
- [Interactive and Robust Mesh Booleans](https://arxiv.org/abs/2205.14151)
  supplies the performance shape: implicit line-plane/triple-plane points,
  indirect filtered predicates, parallel arrangement construction, connected
  surface patches, and one cascaded exact ray per patch rather than per face.
- [Exact and Efficient Intersection Resolution for Mesh Arrangements](https://doi.org/10.1145/3687925)
  supplies indirect-offset predicates plus face localization and dimension
  reduction for sorting, deduplicating, and locating intersection points. Its
  claimed order-of-magnitude gain is a benchmark target, not evidence that an
  unmeasured RDK port has the same performance.
- [A Robust Approach to Detect Intersections between Triangles with Different Numerical Representations](https://arxiv.org/abs/2507.08478)
  supplies the reference matrix for exhaustive triangle contacts when
  endpoints are explicit binary64, exact rational, or implicit constructions.
  RDK keeps its own compact event representation, but every representation
  pairing must reach the same canonical contact classification.
- [Exact Predicates, Exact Constructions and Combinatorics for Mesh CSG](https://arxiv.org/abs/2405.12949)
  supplies exact constructed points, symbolic-perturbation constrained
  Delaunay remeshing, coincident-triangle elimination, exact radial ordering,
  and the Weiler volumetric model.
- [Deterministic Linear Time Constrained Triangulation using Simplified Earcut](https://arxiv.org/abs/2009.04294)
  supplies a proven linear pocket retriangulation step for segment insertion.
  RDK may use this behind the current traced CDT recovery only after its
  restricted simple-pocket preconditions are checked exactly; it is not a
  general polygon triangulator.
- [trueform: Fast And Robust Mesh CSG Via Topological Aggregation](https://arxiv.org/abs/2607.15905)
  supplies a newer performance-oriented arrangement design: canonical
  vertex/edge/face event types, face-local graphs with two-level identity,
  exact radial decisions over original planes, topological aggregation across
  otherwise disagreeing local observations, build-once/query-many domain
  partitions, and first-class self-overlap/open-sheet semantics. Rays uses
  these as specification and benchmark targets; adopting its exact-without-
  construction representation would require proving compatibility with RDK's
  existing exact implicit-point ancestry rather than introducing a second
  Boolean kernel.
- The [Houdini Boolean 2.0 contract](https://www.sidefx.com/docs/houdini/nodes/sop/boolean.html)
  defines the artist-facing modes: solid/surface treatment, union,
  intersection, both subtraction directions, XOR/custom expressions,
  shatter/resolve, seam output, self-intersection handling, source groups,
  attribute interpolation, and tiny seam cleanup with diagnostics.

CGAL corefinement remains a useful reference but is not the target ceiling.
Its public model is documented in the
[Boolean/corefinement reference](https://doc.cgal.org/latest/PMP_Boolean_operations/group__PMP__corefinement__grp.html).
The libigl Boolean path pulls GPL-licensed CGAL code, so it is not an acceptable
Rays dependency. Three permissively licensed implementations were audited
as executable specifications: Cherchi et al.'s MIT reference implementation,
Geogram's BSD-3-Clause exact CSG/Weiler implementation, and Manifold's
Apache-2.0 packed parallel Boolean. The newer trueform implementation is
PolyForm Noncommercial/commercial and therefore is not copied, linked,
translated, or used as a Rays dependency. Its published paper and public
behavior are research references only. Rays independently implements the
hybrid contract in RDK: exact dyadic constructions/CDT/Weiler remain
authoritative, while canonical simplex identity, carrier-plane radial fast
paths, and relation-level aggregation are separately specified and tested.
The paper's majority vote is statistical rather than worst-case and is not, by
itself, a production correctness proof.
Manifold's manifold-input guarantee and epsilon-valid model are valuable for
fast-path and construction patterns but are narrower than the chosen
dirty-input arrangement semantics.

The detailed evidence, input/output contract, phase invariants, adversarial
matrix, licensing decisions, and promotion gates are recorded in
[`boolean.md`](boolean.md). That document is normative for the private kernel;
this section remains the architecture summary.

The packed RDK pipeline is split into explicit phases:

1. Normalize only for filter conditioning; merge exactly duplicate input
   vertices; reject non-finite data; retain stable operand, primitive, corner,
   attribute, and group ancestry.
2. Generate stable triangle candidate pairs with the existing packed surface
   BVH. Run exact intersection classification in disjoint ranges, including
   vertex/edge/face contacts, coplanar overlap, adjacent self-intersections,
   and duplicate facets.
3. Store new locations as compact implicit constructions over original input
   IDs: explicit point, line-plane intersection, projected coplanar line-line
   intersection, or three-plane intersection.
   Approximate coordinates are caches only. Orientation, ordering, equality,
   in-circle, and segment-incidence decisions involving constructed points use
   indirect filtered predicates with exact dyadic/expansion fallback.
4. Build a deterministic constraint set per source triangle, deduplicate it by
   exact symbolic identity, and compute the unique constrained Delaunay
   refinement. Stable prefix sums allocate the global point/corner/primitive
   planes once; independent faces cook in parallel.
5. Merge identical refined facets in coplanar regions while retaining every
   operand/winding contribution. Build packed radial bundles around
   non-manifold seam edges and sort incident charts with exact predicates to
   form the Weiler adjacency between volumetric regions.
6. Classify each connected patch/cell once. Use a cascade from certified float
   ray tests to indirect exact ray tests, deterministic perturbation for
   vertex/edge hits, and winding-vector propagation through radial adjacency.
7. Evaluate a typed Boolean expression over region winding vectors. Extraction
   emits consistently oriented boundary facets, seam polylines, or all shatter
   patches while preserving source ancestry and interpolating numeric payloads
   from exact barycentric construction data.
8. Round constructed coordinates to binary64 exactly once at materialization,
   then run bounded duplicate/tiny-edge cleanup and an exact seam-only
   self-intersection verification. Rounding must not retroactively change the
   combinatorial arrangement; an unverifiable result is an error with element
   IDs, never a best-effort mesh.

The ordinary predicate path must allocate nothing. Nonescaping exact fallback
uses a reusable domain-local packed limb arena that grows only to the widest
predicate seen on that domain; no arena value may escape a call. Retained
implicit constructions remain immutable until a job-local packed ancestry
representation proves the same exact identity and cancellation behavior. Broad phase,
pair classification, face constraint preparation, local triangulation,
attribute interpolation, and patch ray queries are parallel over stable index
ranges. Radial/cell graph assembly uses deterministic packed sorting and prefix
sums. Every stage publishes its cardinality, cancellation points, asymptotic
cost, and offending element IDs on invariant failure.

Repair remains a collection of explicit operations. The
[CGAL repair manual](https://doc.cgal.org/latest/PMP_Mesh_repair/index.html)
correctly separates duplicate/degenerate cleanup, self-intersection refinement,
hole filling, refinement, and fairing rather than presenting “repair” as one
opaque operation.

`Rdk.Boolean_detect` now supplies the first reusable stage: it builds both
surface indexes, emits deterministic candidate pairs without per-candidate
lists, classifies crossing/touching and optionally coplanar pairs in a local
scale-normalized frame, and aggregates sorted unique B primitive IDs per A
primitive. Its explicit floating tolerance is suitable for selection and
diagnostics; it is not an adaptive/exact sign predicate and therefore is not
used to make topology-changing seam decisions. Corefinement must add filtered
orientation/intersection signs, constructed seam vertices, both-surface edge
splitting, patch classification, and stitching rather than promoting the
detector into a solid Boolean by name alone.

`Rdk.Intersection_analysis` exposes the next reusable stage without
changing either input. It materializes pair-local crossing/coplanar event
positions and triangle barycentric or complete-curve parameters, welds
coincident events deterministically, and retains aligned source-input and primitive identities.
These points are suitable inputs to attribute interpolation and future seam
planning. They are not yet corefinement vertices: topology-changing Boolean
work still requires filtered predicates and consistent edge/face split plans.

The same kernel supplies Boolean Detect's AxA mode without constructing the
quadratic ordered product. BVH leaves retain only pairs with increasing
internal triangle IDs and different source primitives. The narrow phase marks
shared point IDs, rejects an intersection that exists only at those topological
vertices or edges, retains duplicate faces and overlap beyond a shared edge,
then writes both directions of each retained primitive pair before stable row
deduplication. One-input Sop cooks therefore build one surface index and
never manufacture a second geometry snapshot.

RDK therefore needs the exact/implicit stages above before `Csg3` can be
replaced. `Csg3` remains an explicitly labeled creative-coding compatibility
utility and is never a fallback for `Rdk.Boolean`. No public production
Boolean node is added until the complete union/intersection/subtraction path is
topology-safe on its declared input model. Each intermediate stage remains
private or carries a name that describes its actual diagnostic output.

### Fuse, transfer, and spatial queries

Fuse uses a packed open-addressed spatial grid and stores only stable cluster
representatives in candidate chains. The earliest compatible representative
wins; optional point-attribute seam matching is bound once outside the search
loop. Cluster members are materialized as CSR once, allowing average reductions
over independent destinations with the same member order on every domain. The
same cluster CSR drives component-wise minimum/maximum/mode/upper-median, sum,
sum-of-squares, scale-safe RMS, weighted average/sum, and extrema-by-weight
position policies. Mode and median sort one owned work plane by disjoint
cluster ranges; all other reductions are linear packed passes.

Fixed-target Fuse uses a second packed spatial plan rather than merging target
geometry into the query payload. Target insertion is stable, while independent
query ranges reduce candidates to either the least target number or closest
target with least-number ties. Specified-point mode reads an integer query map
directly. Optional scalar match views and point-radius views are resolved once;
the candidate loop allocates nothing and uses scale-normalized distance tests
so large finite coordinates do not overflow. Snap-only output copies only the
position planes and shares unchanged topology/payload, while exact post-snap
consolidation delegates to the original CSR clustering kernel. A second input
is immutable and never enters the output ownership graph.

Same-input Modify Target turns stable query-to-target links into minimum-root
union/find components and reduces both query and target records. Keep Fused
Points uses the same reduced payload but preserves all point records while
rewiring topology to component representatives. Optional post-fuse cleanup
first removes consecutive duplicate references in parallel primitive ranges,
cardinality-plans the surviving topology once, and remaps packed
vertex/primitive payload and native edge ancestry. Selective point cleanup
distinguishes points orphaned by deleted primitives from points that were
already unused; all-unused cleanup performs stable packed compaction.

Attribute and point-group rule lists compile Houdini-style name patterns once;
the last matching rule wins. Numerical scalar/tuple reductions fill disjoint
cluster planes, text mode/median sort one global work plane by independent CSR
ranges, and concatenation cardinality-plans packed scalar-to-array or existing
CSR rows before one fill. Fixed-target rules use the already selected target
map directly, copying matched point payload and target-only groups without a
second spatial query. Generated snap metadata is installed after Modify Target
rules, so wildcard patterns cannot consume protocol/output fields.

Attribute Copy remains separate from spatial Attribute Transfer: it builds an
ordered, value-keyed, or explicit-index correspondence and projects that map
through packed topology once per destination owner. Same-owner ungrouped
identity plans share immutable payload planes, which is the common fast path
when an iterative sketch carries unchanged fields into its next snapshot.
Non-identity payload writes use disjoint stable ranges; topology conflicts use
deterministic destination traversal rather than scheduling order.

Attribute Combine is a fused element kernel rather than a procedural chain of
single-layer attribute rewrites. It resolves numeric source/mask planes and
cross-input correspondence once, allocates the final destination once, and
applies every ordered arithmetic/blend layer plus postprocessing inside one
parallel element traversal. Integer/text match tables use packed open
addressing; output publication and optional source cleanup are atomic. This is
the preferred low-allocation path for composing masks and coefficients inside
iterative sketches.

Attribute Interpolate consumes persistent primitive-number/UVW coordinates on
the destination. It resolves mixed-owner source fields once, partitions them by
owner and numeric/discrete storage, and evaluates every field during one
parallel destination traversal. Triangle, bilinear-quad, implicit n-gon-fan,
and polygon-curve weights are independent of source positions, so deforming
source geometry can be re-sampled without rebuilding correspondence. This is
the low-allocation attachment path for iterative sketches. Explicit weighted
element lists remain blocked on packed ragged-array attribute storage rather
than being emulated with boxed lists.

Point and primitive Attribute Transfer use the shared balanced
`Rdk.Spatial_index`, not an all-pairs scan per attribute. Points index canonical
positions; primitives first fill arithmetic corner barycenters. A bulk query
produces fixed-width stable source IDs and squared distances once, then every
selected typed payload plane applies that plan in parallel. Max distance,
source-index tie policy, neighbor count/power, and miss defaults are explicit.
Owner-matched source and target groups remain packed bitsets; selected IDs keep
their original numbering. Full-strength distance thresholds can extend through
linear, smoothstep, or fixed uniform-bias blend bands; zero-width transfer has
no influence-plane allocation. Globally flat axes are omitted from the KD split
cycle, and large disjoint subtrees build through the reusable domain pool.

Polygon-surface transfer uses the separate shared `Rdk.Surface_index`.
Deterministic ear clipping emits internal triangles while preserving original
primitive and corner IDs; a packed AABB hierarchy produces closest IDs,
squared distances, and barycentric weights once. Point/vertex payloads
interpolate, primitive payloads copy, and targets may be points, referenced
vertex positions, or primitive barycenters. Non-simple, degenerate, curved, or
non-finite source contours fail before publication. Packed source-primitive and
destination-owner groups restrict work without element lists;
linear/smoothstep/uniform bands taper influence beyond the full-strength threshold.
Detail transfer structurally shares immutable payloads. `transfer_all` composes
point, primitive, vertex, and detail tabs without nesting parallel kernels; one
query plan is shared by every field of a spatial owner. Generated metadata is
committed once with expected-linear lookup, avoiding immutable-table quadratic
rebuilding for large patterns. Attribute selection
compiles stable include/exclude globs once for each operator; exact-name lists
remain strict. Sop source/destination group patterns union same-owner
packed bitsets in one disjoint byte pass. Source-vertex partial surfaces and
the remaining Houdini kernel functions remain explicit operator work rather
than alternate geometry cores.

### Packed instance materialization

Render instances retain one immutable prototype and an ordered matrix array;
they do not masquerade as editable RDK primitives. The explicit Unpack boundary
preflights `instances × source cardinality`, allocates each output plane once,
and fills transform-major ranges independently. Attribute and ordinary/native
group repetition uses the same ancestry order as topology, and normal planes
use one inverse-transpose matrix per instance. Unrestricted Duplicate only
prepares identity/transform powers and delegates to this kernel. Restricted
Duplicate uses one cardinality-first RDK ancestry map because the retained
original and selected copies have different shapes; it does not build a
compact geometry or merge intermediate. Both paths share the same packed
attribute/group semantics, so arbitrary Unpack and regular duplication cannot
drift into alternate public geometry cores.
Keeping output immutable does not require persistent rebuilding: all arrays,
bitsets, and inverse matrices are job-local owned mutation and become visible
only after a complete successful cook.

### Measurement

RDK owns one packed measurement kernel for polygon area, polygon/curve
perimeter, and oriented signed volume. Triangle meshes use a direct numeric
path; simple N-gons reuse deterministic ear clipping so concavity cannot make
fan magnitudes overcount. Primitive-group selection compiles once, workers
write disjoint primitive slots, and detail/throughout totals sum those slots in
stable primitive order. Signed volume uses oriented tetrahedra around the
geometry bounds center to improve translation conditioning; it intentionally
does not conceal open or inconsistently wound input behind an absolute value.

The public complexity is O(vertices) for perimeter and O(sum(c²)) for polygon
corner counts c, with O(primitives + chunks × max(c)) auxiliary/output storage.
Future curvature and differential measurements belong in this same analysis
core, not in a second Sop implementation.

### Attribute blur and iterative relaxation

Connectivity blur is a packed Jacobi iteration over the cached point-edge CSR.
All selected scalar/tuple components share that immutable topology; each pass
reads one plane set and writes another, so work stealing cannot expose a
partially updated neighbor. Neighbor traversal remains in stable edge order,
making uniform and inverse-original-edge-length sums exact across domain
counts. Optional alpha changes neighbor influence, a scalar weight changes the
receiving displacement, and border pins are precomputed once from one-sided
edges.

This double-buffered kernel is safe to compose inside one SOP cook or across
the explicit `Sketch.run_state` feedback boundary. A future rest-geometry input
may select a second immutable topology/metric plan; it must not create hidden
mutable state inside the node. Proximity blur needs the shared spatial-index
family and explicit radius/neighbor bounds, while volume-preserving smoothing
needs a separately specified stable step schedule. Neither is approximated by
the connectivity implementation.

### Element proximity

Group Transfer uses one private packed proximity kernel for polygon/curve
primitive and native-edge queries. Each entity expands once into stable
triangle or segment features; source features form a median-split AABB tree,
while target entities retain contiguous feature ranges. Queries compute exact
segment-segment, segment-triangle, or triangle-triangle separation, including
crossing and containment cases, and select the lower source entity on an exact
tie. This avoids the incorrect but tempting primitive-centroid and
edge-midpoint approximations.

Construction is expected O(f log f) time with O(f) packed auxiliary storage.
Ordinary queries are expected O(q log f), with the documented O(q*f)
overlapping-bound worst case. Feature fills, independent hierarchy subtrees,
and target entity ranges use the shared domain pool. The query hot loop keeps
fixed worker scratch, and its distance function is forced inline so candidate
visits do not box returned floats.

### Plane clipping

Clip classifies packed positions by a normalized signed plane distance and
snaps values inside an explicit tolerance. Shared topology-edge IDs identify
intersections, so adjacent polygons reuse the same output point. Stable
above-then-below construction is independent of domain scheduling, while
numeric point and corner attributes interpolate at the same edge parameter.
Optional fills trace degree-two intersection graphs and share their boundary
points with the clipped surface; cap corners carry plane normals so the seam
can remain hard without opening the topology.

Concave polygons with several retained boundary runs sort their plane
intersections, pair intervals through the source interior, and stitch the runs
into the actual connected output cycles. This preserves N-gon fragments and
lets every cut span participate in manifold cap tracing instead of silently
bridging disconnected pieces. Cap segments retain their directed surface
winding. Oppositely wound contained loops become holes, same-winding nested
solids remain separate caps, and alternating containment restores islands.
Since the packed topology deliberately gives each polygon one contour, hole
components are visibility-bridged and ear-clipped into ordinary triangles with
the original boundary tokens. Open, non-manifold, inconsistently wound,
intersecting, and unbridgeable cap graphs still produce corrective diagnostics.

### Topology deletion

Delete, Blast, and Split use one packed retained-element plan. Point and vertex
selection can either destroy touched primitives or heal them by preserving the
remaining corner order; polygons/closed curves below three corners and open
curves below two are discarded. A second pass derives the exact point map,
including explicit selected-point removal and optional orphan compaction.
Every attribute/group owner follows its matching stable map. Whole-primitive
deletion keeps normals and shares unchanged point payloads; healed faces remove
point/vertex normals because their surface meaning changed.

### Simplification and remeshing

Reduction needs a mutable job-local topology staging structure over packed
source IDs, a priority queue with deterministic tie breaks, boundary/seam/
crease constraints, and a final exact compaction pass. The public geometry
remains immutable. Remeshing similarly composes bounded edge split, collapse,
flip, and smoothing passes; every mutation validates its local link condition
before commit. Neither algorithm should rebuild the complete immutable detail
after each edge operation.

## Reuse policy

The preferred implementation is a small audited native Rust kernel over RDK
planes. Reusing an external library remains possible when it materially
improves correctness, but it requires a written review of:

- license and redistribution obligations;
- Apple-Silicon macOS and availability on supported Cargo/native toolchains;
- deterministic element ordering and floating-point behavior;
- cancellation and thread ownership;
- conversion cost for points, corners, attributes, groups, and face-varying
  seams;
- native build behavior and isolation from renderer/GPU implementation details.

OpenSubdiv is the semantic reference and a possible optional accelerator for
subdivision, but a binding would not replace RDK ownership or its fallback
contract. CGAL is the robustness reference for corefinement/repair; adopting a
native dependency would need a separate license/build/ABI decision. Until such
a review is accepted, Rays implements against the published algorithms and
tests rather than exposing an optional backend with different results.

## Acceptance gates

Every production modeling kernel requires:

1. documented preconditions, failure diagnostics, time, and auxiliary memory;
2. exact cardinality and attribute/group preservation tests;
3. degenerate, boundary, non-manifold, seam, and malformed-input fixtures;
4. cancellation before publication for long work;
5. byte-identical one-/multi-domain output and stable element ordering;
6. a representative scale benchmark with allocation and peak-RSS evidence;
7. native framebuffer or PNG comparison for renderable output;
8. Captured compatibility when replacing an existing public operation.
