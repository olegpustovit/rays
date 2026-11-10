# Exact Boolean kernel specification

This document is the normative research and conformance contract for RDK's
mesh Boolean. The implementation remains private until every public-product
gate below is closed. `Csg3` and `Rdk.Boolean_detect` are useful narrower
tools, but neither is a fallback for this kernel.

## Evidence hierarchy

The target is not a literal clone of one package. It combines the strongest
published guarantees under RDK's single-core, deterministic, native Rust
constraints.

| Evidence | What RDK adopts | What RDK does not infer |
| --- | --- | --- |
| [Houdini Boolean 2.0](https://www.sidefx.com/docs/houdini/nodes/sop/boolean.html) | Artist-facing solid/surface combinations, both subtraction directions, custom winding-depth extraction, shatter, seam, detect, per-input self-intersection policy, detriangulation, seam-point splitting, payload transfer, and tiny-seam diagnostics | SideFX does not publish the kernel. Product behavior is a black-box contract, not source code or proof |
| [Mesh Arrangements for Solid Geometry](https://www.cs.columbia.edu/cg/mesh-arrangements/) | Variadic labeled arrangements, generalized winding vectors, self-intersecting/non-manifold polygon-soup semantics, build once and evaluate many expressions | Its CGAL-backed implementation is not a suitable native dependency |
| [Fast and Robust Mesh Arrangements](https://doi.org/10.1145/3414685.3417818) and [Interactive and Robust Mesh Booleans](https://arxiv.org/abs/2205.14151) | Implicit intersections, indirect filtered predicates, parallel face work, connected patch classification, and deterministic constrained insertion | The 2022 reference requires manifold, watertight, oriented, self-intersection-free inputs and exits on a fully implicit classification patch; those restrictions are below RDK's target |
| [Exact and Efficient Intersection Resolution](https://doi.org/10.1145/3687925) | Indirect-offset predicate formulations, face localization, dimension-reduced ordering/deduplication, and parallel local work | Reported speedups are targets until reproduced by RDK benchmarks |
| [Exact Predicates, Exact Constructions and Combinatorics for Mesh CSG](https://arxiv.org/abs/2405.12949) | Exact co-refinement, unique symbolic-perturbed CDT, coincident-facet elimination, exact radial order, Weiler regions, and arbitrary Boolean expressions | No claim is made until every degeneracy needed by the proof is represented in RDK |
| [Deterministic Linear Time Constrained Triangulation](https://arxiv.org/abs/2009.04294) | Linear ear removal for the restricted simple pockets produced by inserting one constraint into an existing triangulation | It is not valid for arbitrary polygons, holes, or unchecked self-intersecting pockets |
| [Fast Exact Booleans for Iterated CSG](https://arxiv.org/abs/2103.02486) | Plane ancestry, homogeneous exact points, fixed-width fast paths, locality, and persistent build/query separation | Octree-embedded BSP is useful for repeated asymmetric machining-style CSG, but it does not replace the surface arrangement core or its payload ancestry |
| [Robust mixed-representation triangle intersections](https://arxiv.org/abs/2507.08478) | Exhaustive contact-type oracle across explicit, rational, and implicit endpoints | RDK retains its own compact event encoding and must prove canonical equivalence |

The newest research can change optimization choices, but cannot weaken exact
topology, deterministic ordering, cancellation, or bounded-memory contracts.

## Open-source audit and reimplementation boundary

The source audit is deliberately broader than the papers. Revisions below are
the reproducible research snapshot inspected on 2026-08-05; moving repository
heads are not a specification:

| Implementation | Audited revision | License/dependency result | Use in Rays |
| --- | --- | --- | --- |
| Cherchi et al. reference arrangement/Boolean | [`bf7eb71`](https://github.com/gcherchi/FastAndRobustMeshArrangements/tree/bf7eb71da991a61ff5414946a4b2754bbd327e41) | MIT | Executable specification for phase order, implicit predicates, constraint insertion, patch classification, and benchmarks. Independently expressed in Rust/RDK storage |
| Geogram exact CSG | [`b6f545a`](https://github.com/BrunoLevy/geogram/tree/b6f545a179ab963879482010eb4ec423b4d352d8) | BSD-3-Clause | Executable specification for exact points, constrained face refinement, radial bundles/polylines, Weiler construction, and expression classification |
| Manifold | [`ff42ddc`](https://github.com/elalish/manifold/tree/ff42ddc885e2287faa176873e38e795572e95992) | Apache-2.0 | Packed/parallel engineering and payload-provenance reference. Its manifold-input and epsilon-valid contract is a deliberately narrower fast-path model |
| kigumi | [`374327c`](https://github.com/unageek/kigumi/tree/374327c6d5732c91949e70a9fc0d3bf40182c9b2) | MIT code over CGAL | Useful local radial-classification comparison for open/non-manifold boundaries, but its documented input excludes degenerate faces and self-intersections. CGAL's GPL triangulation remains transitive and is not introduced |
| libigl Boolean | External release oracle | MPL-2.0 library with a CGAL/copyleft Boolean path | Differential oracle only; not linked or copied into Rays |
| CGAL Polygon Mesh Processing | 6.2 documentation | GPL/commercial for the relevant package | External oracle only unless a future explicit license/portability review approves a dependency. Its iterative snap-rounding result is valid only when its explicit success result is true |
| trueform | [`2c2c6b0`](https://github.com/polydera/trueform/tree/2c2c6b09b4f1ccf6c39d8ae70e454540db6ee143) | PolyForm Noncommercial/commercial | Paper, public API behavior, adversarial cases, and benchmark protocol only; no code copying, linking, translation, or vendoring |

Permissive licensing does not by itself justify importing a second geometry
kernel. RDK owns topology, exact identity, packed storage, deterministic
ordering, attributes, groups, cancellation, and Apple-Silicon portability. Source
is inspected to extract invariants and adversarial cases; implementation uses
RDK types and tests. Any substantially copied permissive code would need its
notice preserved explicitly, but the current kernel is an independent Rust
implementation.

## Clean-room algorithm decision

RDK uses a hybrid whose parts have different proof status. This distinction is
normative; a faster research result does not silently replace a stronger exact
contract.

| Mechanism | RDK decision | Reason |
| --- | --- | --- |
| Exact binary64 input domain | Keep | Every finite input float is an exact dyadic. Snapping all inputs to a fixed integer grid would change the declared geometry before the operation starts |
| Exact implicit LPI/line-line/TPI constructions | Keep and pack | They preserve source ancestry and permit exact predicates after construction. Dense retained objects are an engineering problem, not permission to discard exact identity |
| Unique symbolic-perturbed constrained Delaunay refinement | Keep | Equivalent coplanar face arrangements must triangulate identically so duplicate facets can be eliminated combinatorially |
| Five canonical contact families, `VV`/`VE`/`VF`/`EE`/`EF` | Adopt as the public research taxonomy | They are a useful minimum partition and stable identity vocabulary. RDK may retain finer proper/coplanar/coincident subtypes internally |
| Two-level point identity | Adopt | A topological key says two observations are the same construction; a separate exact geometric merge unifies distinct constructions at the same location. Approximate coordinate hashing is never authoritative |
| Radial comparison from original carrier planes | Add as a certified fast path | It avoids repeatedly expanding constructed edge coordinates. It must be differentially identical to the existing exact implicit-point radial predicate for every supported contact type |
| Relation grouping by radial polyline and incident-component set | Adopt | One polyline can carry several relations when a non-manifold flap begins or ends. Treating the whole polyline as one relation loses topology |
| Weighted-majority topological aggregation | Diagnostic/recovery path only | The trueform paper explicitly gives a statistical, not worst-case, guarantee. A majority can be adversarially wrong, so it cannot by itself close RDK's exact production gate |
| Build once, evaluate many expressions | Keep | Domain winding vectors and source ancestry are immutable products of the arrangement; union/intersection/differences/XOR/custom extraction must not repeat intersection work |
| Post-materialization verification and repair | Strengthen | Exact internal topology does not prevent new binary64 crossings after rounding. A result is published only after bounded cleanup and exact verification succeed |

This is a clean-room reimplementation boundary. The papers define algorithms
and proof obligations; permissive implementations provide differential outputs,
phase timings, and failure corpora. RDK code is written from this specification
against RDK representations. Noncommercial trueform source is excluded even
from line-by-line translation.

### Canonical identity contract

Every intersection observation has a stable topological key before Cartesian
construction:

- operand, primitive, and open-simplex IDs for both participants;
- one canonical ordering of the participant pair, independent of traversal
  direction or worker scheduling;
- a contact family (`VV`, `VE`, `VF`, `EE`, or `EF`) plus the finer exact RDK
  contact subtype;
- for a crossing created while arranging a face, the sorted carrier-face set
  and, when coplanar contact makes that set ambiguous, the canonical pair of
  carrier constraints;
- a separate exact-location representative that merges different keys only
  after certified equality.

The topological key provides cross-face identity by construction. The exact
merge handles multi-way coincidences. Neither uses rounded position as a hash
key. Stable prefix sums assign packed point IDs only after keys and merge
classes are final, so one-domain and multi-domain numbering is identical.

### Radial relation and aggregation contract

Non-manifold complex edges first form maximal radial polylines under shared-end
adjacency. A relation is the pair `(polyline, sorted incident shell set)`, not
just a polyline. For each edge in a relation:

1. orient its axis by the deterministic polyline traversal;
2. compute the cyclic incident-shell order with exact signs, preferably from
   original carrier planes and otherwise from exact implicit points;
3. rotate the cycle to its lexicographically least stable shell/facet key;
4. retain reversal as a distinct observation unless the axis-orientation proof
   establishes equivalence;
5. reject an incident chart with no exact carrier plane from the proof path,
   reporting its source IDs rather than assigning an arbitrary angle.

Inside an exact RDK build, all observations of one relation must induce the
same wedge adjacency. Disagreement is an invariant diagnostic and triggers a
certified alternate exact formulation; it is not settled by a vote. A future
materialized-arrangement import tool may expose weighted majority as an
explicit recovery policy with the vote distribution, weight definition,
confidence, stable tie failure, and affected relation IDs in its result. That
recovery tool is not the exact Boolean SOP.

### Classification contract

Winding/inclusion is a potential on the exact domain-adjacency graph. Crossing
a facet adds its signed operand contribution; propagation must be path
independent. Surface-connected domains are seeded from exact radial/Weiler
adjacency. Only disconnected nesting needs a geometric seed.

The final classifier must use at most one exact seed ray per disconnected
component and then propagate combinatorially. Both signs of the three coordinate
axes are cheap exact attempts. If they are boundary-degenerate, the classifier
uses the formal direction `d(epsilon) = (1, epsilon, epsilon^2)` and selects the
sign of the first non-zero exact polynomial coefficient. This is one symbolic
ray, not a finite list of increasingly arbitrary floating directions. For a
valid nondegenerate arrangement it cannot pass through a non-incident source
edge or vertex in a way that changes the crossing: an identically-zero edge
determinant only produces the boundary result when the query lies on the closed
source triangle, and the exact refined-facet membership set removes every such
incident triangle. A seed on any other source triangle is therefore an
arrangement-membership invariant failure with source IDs, not a
perturb-and-continue case. The symbolic normal
polynomial selects the seeded half-facet, and the signed crossings preserve
integer winding, including magnitude greater than one. Random retry and
floating tolerance are not production fallbacks.

Closed source connected components are indexed by a packed conservative AABB tree.
The query interval can reject a component only when its exact seed cannot be
inside that box; every retained triangle is still classified by exact signs.
Tree construction is O(c log c), storage is O(c), and a query is O(log c + k)
for separated boxes but remains O(c + t) when all component boxes overlap,
where `c` is component count, `k` the returned components, and `t` their
triangles. The exhaustive component scan is retained only as a private
differential oracle.

### Materialization contract

The exact arrangement is rounded to binary64 once. Then:

1. detect rounded point collisions, zero/collinear facets, non-vertex seam
   crossings, overlaps, and invalid surface incidence exactly in binary64;
2. identify tiny candidates only among extracted surface edges adjacent to an
   exact seam; Houdini's documented control applies to these surface edges,
   not to the seam polyline product;
3. collapse only when the explicit threshold, link/incidence conditions,
   orientation preservation, source ancestry, and schema-reduction policy all
   succeed;
4. rerun the exact rounded-output verifier after every cleanup batch;
5. return a stable diagnostic with element/source IDs if no verified result is
   possible.

CGAL 6.2's bounded iterative snap-rounding API is an external behavioral
oracle for this phase: it exposes grid size and iteration count and explicitly
reports failure. RDK does not inherit its grid or algorithm, but does inherit
the rule that a partial unsuccessful cleanup is not a valid result.

## Declared input model

- Coordinates are finite binary64 values. Invalid coordinates are rejected
  with source element IDs before broad phase.
- Input is polygonal geometry triangulated with retained polygon/corner
  ancestry. Triangles may be disjoint, touching, coincident, non-manifold, or
  self-intersecting according to each operand's explicit resolve policy.
- `Solid` means an oriented boundary whose generalized winding depth defines
  regions. `Surface` means an oriented sheet with no implicit filled interior.
- Exact duplicate vertices/facets are canonicalized without erasing operand,
  winding, group, or attribute contributions.
- Tolerance may select cleanup policy or filter candidates. It never replaces
  an exact sign when the sign changes topology.

## Output products

One prepared arrangement must support, without repeating intersection work:

- union, intersection, A minus B, B minus A, XOR, and a typed predicate over
  per-region integer winding vectors;
- resolved arrangement, shatter pieces, and independently selectable A-A,
  A-B, and B-B seam products;
- solid/solid, solid/surface, and surface/surface semantics, including
  coincident surface patches and intentional double walls where required;
- consistently oriented triangles plus optional source-polygon
  detriangulation;
- unique or split seam points as an explicit topology policy;
- point/vertex interpolation, primitive copying, group ancestry, and stable
  source IDs for every emitted element;
- diagnostics for rejected input, unresolved symbolic degeneracy,
  post-rounding collisions, tiny/collinear facets, open result incidence, and
  seam self-intersection.

## Kernel invariants

1. Broad phase may over-report but never miss a possible contact. Candidate
   output is stable and duplicate-free before exact classification.
2. Contact classification distinguishes disjoint, vertex-vertex,
   vertex-edge, vertex-face, edge-edge, edge-face, proper segment, coplanar
   area, identical facet, and ordinary shared-topology contact.
3. Every constructed point retains an exact recipe over original input:
   explicit, line-plane, projected line-line, triple-plane, or an explicitly
   reviewed representation-neutral composition.
4. Approximate Cartesian coordinates are acceleration/presentation caches.
   Equality, ordering, orientation, incidence, incircle, ray, and radial
   decisions use certified filters with exact fallback.
5. Exact fallback temporaries are domain-local, reusable, and nonescaping.
   Retained exact constructions have explicit job ownership and bounded
   lifetime.
6. Each source face receives one exact planar arrangement. Multi-way events
   share one point identity and one segment incidence. Every constraint is an
   edge of the output CDT.
7. Cocircular CDT ambiguity is resolved by a stable symbolic key. Equivalent
   coplanar faces produce the same triangulation, allowing exact duplicate
   removal without geometric guessing.
8. Refined facets retain all source contributions. Radial bundles sort charts
   exactly around every non-manifold edge and define the Weiler adjacency.
9. Winding propagation is integer and path-independent. A disagreement is an
   invariant error and enters a certified alternate exact formulation. A
   statistical topological vote may diagnose or recover imported materialized
   arrangements, but never silently settles the exact build.
10. Expression extraction keeps a facet exactly when its two incident regions
    evaluate differently, orienting it from selected interior to exterior.
11. Coordinates round once at materialization. Cleanup cannot alter the exact
    region decision; it must preserve seam topology or return a diagnostic.
12. One-domain and multi-domain results are byte-identical in point, corner,
    primitive, attribute, group, and diagnostic order.

## Performance shape

- Surface BVHs and face-local indexes use packed integer/float planes.
- Candidate classification, independent face arrangements, payload
  interpolation, and pure classification use stable disjoint parallel ranges.
- Cardinalities are preflighted where knowable; variable event streams use
  geometric-growth builders and materialize once.
- Ordinary filtered predicates allocate zero. Exact fallback reuses one
  domain-local limb arena. No SDL or renderer object enters the kernel.
- Face arrangements use sweep/BVH candidates, online exact event interning,
  and bounded multi-way aggregation. The unavoidable worst case remains
  output-sensitive quadratic when the arrangement itself has quadratically
  many distinct crossings.
- Prepared arrangements are build-once/query-many values. Caches and temporary
  arenas have explicit capacities and do not grow with sketch frame count.

## Required conformance corpus

Promotion requires all of the following in exact one- and multi-domain modes:

- disjoint, nested, transverse, tangent, touching-vertex, touching-edge,
  face-coincident, opposite-winding duplicate, and near-coplanar solids;
- every triangle contact class and every explicit/LPI/line-line/TPI endpoint
  combination, compared with immutable exact reference predicates;
- same-operand transverse, coplanar, coincident, adjacent, non-manifold, open,
  and self-intersection-resolution fixtures;
- multi-way points, coincident constraints, cocircular CDT, high-valence
  radial bundles, disconnected shells, cavities, zero winding, and winding
  magnitude greater than one;
- all Boolean expressions, both subtraction directions, shatter, seam, and
  solid/surface product matrices;
- every-storage attribute/group ancestry, seam splitting, normal
  normalization, discrete conflict policy, and source-ID stability;
- cancellation at every phase, exact cardinality/auxiliary-memory ceilings,
  repeatable hashes, post-rounding verification, and fuzz minimization;
- Thingi10K/ThingiCSG-style pathological corpora where redistribution permits,
  plus generated exponent-range and symbolic-degeneracy suites checked into
  the repository as compact recipes rather than opaque binaries.

## Current status and promotion gates

Implemented privately: exact/filtered predicates, implicit constructions,
typed A-A/A-B/B-B constraints, coplanar overlay, face arrangements, exact CDT,
coincident ownership, radial charts, Weiler cells, winding propagation,
union/intersection/both differences/XOR/custom extraction, payload transfer,
post-rounding collision/collinearity checks, and a packed A-A/A-B/B-B seam
product. The seam stage emits every selected complex edge exactly once in
deterministically chained open/closed curves, retains complex-edge ancestry,
separates coincident-area facets, and distinguishes true same-operand
intersections from source-native non-manifold adjacency. Cancellation and
exact one-/multi-domain regressions cover the product. Exhaustive paths remain
as differential oracles. Rounded seam curves are then indexed with a packed
BVH and checked using exact binary64 3D segment contact. Intentional
shared-vertex incidence is preserved; non-vertex crossings and positive-length
overlaps are rejected with a stable curve/segment diagnostic. The verifier
streams candidates with O(1) pair storage per worker instead of retaining an
O(candidate count) list, and curve-edge ancestry is checked for exact
incidence, bounds, monotonicity, uniqueness, and cardinality.
Prepared operands now carry an explicit `Solid` or `Surface` treatment. A
surface remains in exact refinement, radial adjacency, seams, and payload
ancestry but contributes zero to cell winding, so an open sheet cannot poison
the path-independence proof for a closed operand. Treatment-aware extraction
supports the five named products for solid/solid, solid/surface,
surface/solid, and surface/surface without rebuilding the arrangement. Mixed
products retain inside/outside sheet patches and emit deterministic paired
opposite-facing walls for solid-minus-surface cuts. Surface/surface coincident
patch union, intersection, both differences, and XOR are selected by exact
complex membership; transverse intersection remains a curve in the seam
product rather than inventing area. Packed ancestry products are concatenated
by exact complex-vertex identity. Basic transverse/coincident fixtures,
closed-output rejection, cancellation, and exact one-/four-domain signatures
cover this private treatment matrix. Primitive attributes and groups on an
intentional paired surface cut copy from exact ancestry on both walls, default
correctly on the solid operand, and are exact across one/four domains.
Extraction now retains packed output-point to exact-complex-vertex and
output-primitive to exact-complex-facet ancestry. The seam product retains its
packed per-complex-edge classification instead of discarding it after curve
materialization. Post-rounding policy uses those identities to mark only
surface edges incident to exact seam-touching facets before applying the
explicit metric threshold. A shortest-first independent contraction batch
requires two-manifold triangle incidence, the link condition, and a
conservative exact three-projection orientation certificate. It contracts to
the stable least-ID endpoint and publishes only after rerunning seam
self-contact, rounded point/facet, closed-incidence, and exact surface
self-contact verification. Coplanar carrier pairs enter the exact coplanar
overlay before they are diagnosed as actual point, segment, or area contact.
The shared surface index no longer treats a fixed `1e-30` area as topological
degeneracy; exact projected orientation signs admit every finite,
non-collinear binary64 triangle independent of scale.
The public product now exposes the finite non-negative threshold, positive
batch bound, and strict/diagnostic continuation policy. After every batch,
candidate adjacency is reconstructed from current primitive-to-extraction
ancestry, exact seam edges are remapped, source maps are composed, and the
complete verifier reruns. Contraction uses the shared Edge Collapse policy:
least-ID endpoint position, stable lowest-member point payload/groups,
Fuse-owned vertex/primitive/detail/ordered/native-edge ancestry, degenerate
primitive cleanup, and existing point-normal recomputation. Strict mode returns
`unresolved_cleanup` with the stable remaining count when the bound is reached
or no remaining candidate has a certified contraction.
Source-triangle barycentrics and native-edge containment operate directly
between packed explicit source coordinates and implicit output points through
domain-local exact arenas. Extraction no longer rebuilds three retained
explicit-point objects and an array for every output facet; the allocating
path remains only as a differential oracle.
Cell seeding now ends its cheap six-axis cascade with exact
positive-infinitesimal `(1, epsilon, epsilon^2)` ray-edge and normal signs.
The packed axis source-triangle classifier first certifies projected edges and
the carrier plane using conservative approximation-error bounds, then falls
back atomically to the exact arena if any sign is uncertain. The symbolic
source-triangle classifier returns miss, signed hit, origin boundary, or
degenerate carrier without constructing retained point objects.
It is covered against the immutable exact oracle, against the exhaustive
component scan, and in one-/multi-domain cell results. An unrelated boundary
hit is reported as the exact arrangement-membership invariant failure described
above; it is not hidden by perturbing the origin.

`Rdk.Boolean.run` and the immutable two-input `Sop.boolean` now promote the
five named treatment-aware products plus solid/solid Shatter. Their typed policy includes per-input
solid/surface and self-intersection treatment, exact point-payload reject or
vertex-promotion behavior, shared/split seam points, triangle/unchanged/all
source-polygon detriangulation, flatness assumption, and closed-output
validation. The SOP delegates to the single RDK core, forwards cancellation
and session grain, and includes every policy in static cache identity. Public
RDK/SOP regressions cover payload conflicts, malformed parameters,
cancellation, bounded cache reuse, and exact one-/four-domain topology. A
native framebuffer export is byte-identical across domain counts and differs
visibly from the unsubtracted source.

Shatter concatenates stable A-only/overlap/B-only ancestry, preserves
intentional duplicate walls, and emits optional distinct named primitive
groups that exactly partition surviving output primitives after topology
policies. Source-polygon detriangulation and seam-point splitting are public
through the product boundary and retain their exact one-/four-domain
regressions. `All_polygons`
now dissolves a cut triangle component only when its retained boundary is one
degree-two cycle. Components with holes or internal retained seams stay
triangulated instead of publishing a repeated-bridge, non-simple polygon.

`Rdk.Boolean.seam` and `Sop.boolean_seam` reuse the same exact arrangement for
diagnostic and downstream modeling output. Curve mode emits independently
named left-self, between-input, and right-self seam groups. Coincident mode
emits the exact coincident triangle patches with its own optional primitive
group. Treatment and per-input self-intersection policy are part of immutable
SOP cache identity, and both modes have exact one-/four-domain regressions.

Materialization coalesces only distinct exact vertices whose three rounded
binary64 coordinates are bit-identical. Stable exact per-corner ancestry is
retained even when several exact vertices share one packed output point, so
payload interpolation and exact seam mapping do not fall back to proximity.
Zero-length seam edges created by that unavoidable representational boundary
are omitted from the public curve geometry while remaining marked in the exact
surface-seam relation. Degenerate triangles and invalid incidence remain hard
errors at a direct extraction boundary; the public product defers them to a
mandatory ancestry-preserving representability repair. That repair removes
only duplicate-paired or edge-incidence-closed zero-area subcomplexes, then
tries link/orientation-certified edge contractions. Its fallback considers
only edges within eight local binary64 ULPs, groups disjoint one-ring stars,
and publishes a speculative contraction only after degeneracy, closed
incidence, and non-adjacent self-contact all pass. Every deletion/contraction
composes point, vertex, primitive, payload, group, and native seam ancestry.
The fallback is bounded to 64 monotonic batches and honors cancellation.
Coalescing and repair are not tolerance snapping and never bypass the rounded
self-contact verifier.

Still required before a production-complete Boolean SOP:

- public resolved-arrangement output mode;
- source primitive restriction and output-piece/group naming policies, plus
  any remaining schema conflict modes beyond the explicit point reject or
  promotion policy;
- the full declared treatment corpus, especially non-manifold/open
  surface arrangements, multi-way coincident sheets, opposite-orientation
  duplicate sheets, and payload/group exactness for intentional double walls;
- packed retained LPI/TPI ancestry that removes dense construction heap
  pressure without changing exact identities;
- corpus-scale differential and fuzz results plus benchmark evidence for each
  declared complexity/allocation claim, including a reproducible real-model
  stress campaign. The executable protocol and claim boundary are specified in
  [boolean-stability.md](boolean-stability.md).

Until these gates close, documentation must say “private exact arrangement
kernel,” not “parity,” “production-ready,” or “most advanced.”
