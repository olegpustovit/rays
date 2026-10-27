# Geometry nodes in Rays Lisp: port plan, tiers 1–4

2026-10-12. Source of truth: this file. It was exported from the planning document the owner approved on that date.

## Recorded decisions for A.0

2026-10-12 implementation baseline: questions 1 and 4 retain the owner's explicit
answers; questions 2, 3, 5 and 6 use the recommendations already accepted by the
approved plan. This records that existing authorization, not a new owner answer.

| Question | Decision |
| --- | --- |
| 1, float gate | Accept 4 ulp by default for floats and exact integer data; Lisp's unfused arithmetic defines the result. |
| 2, golden refresh | Allow only per-node changes after its verification gate passes; list every changed golden in that node's commit. |
| 3, delegation | Allow calls to other node kinds, including OCaml-bodied kinds; record direct calls in the manifest. |
| 4, OCaml-backed built-ins | Allow the fixed geometry/graph primitives. Owner correction, 2026-10-13: they need no JavaScript twin; `lib/rdk` will be compiled with js_of_ocaml for the web target later, and the same OCaml code serves both. Twins already written stay until that build exists and are not required for new built-ins. |
| 5, kernels after port | Keep the OCaml kernels as test oracles throughout this round. |
| 6, speed gate | Superseded by the owner on 2026-10-13: speed no longer blocks a port. Every node that passes the Verification gate is registered; both timings at 1,000,000 elements are recorded, and a ratio above 2x is listed as a known regression with its cause in "Speed regressions accepted with the ports". |

Evidence, metadata census, incomplete gates and measurements are tracked in
[`results.md`](results.md). A task remains unchecked until its stated gate passes.

## Decision and scope

This round moves 128 of the 162 geometry nodes from OCaml bodies to Rays Lisp bodies, in five phases ordered by the language features they need. 31 nodes that depend on exact predicates, spatial indexes, solvers or data-dependent mesh surgery stay OCaml, and 3 are scene wiring with nothing to port.

The round exists because the web export can only emit what is written in Lisp: a node whose body is OCaml has nothing to export (`AGENTS.md`, Direction). Web export of 3D is not part of this round. This round makes the bodies exist.

| Tier | Nodes | What defines it | Phase |
| --- | --: | --- | --- |
| 1 | 10 | Per-point map over vec3 attributes, topology untouched | B |
| 2 | 51 | Same, plus float, int and text attributes, other owners, groups | B (35), C (12), D (4) |
| 3 | 27 | Builds geometry with formula-derived topology, or concatenates and replicates | C |
| 4 | 40 | Needs adjacency, components or iteration | C (1), D (27), E (12) |
| 5 | 31 | Exact predicates, spatial indexes, solvers, mesh surgery | stays OCaml |
| Scene | 3 | `switch`, `null`, `exploded_view` | nothing to port |

Every node was classified from its implementation, not its name. About a third of the rows rest on the entry function, outline and index usage rather than a full read of the body; the inventory marks the uncertain ones.

### What "ported" means for one node

1. Its body is a Lisp `defnode` (Phase A adds the form) in `lib/flow_sop/nodes/<key>.rays`.
2. `(sop/<key> ...)` resolves to that body. The OCaml registration in `lib/sop_catalog/sop_catalog.ml` is removed in the same commit.
3. Its parameters, defaults, ranges, labels, folders and validation errors are unchanged.
4. It passes the gate in Verification against the OCaml kernel, which stays in `lib/rdk` as the test oracle.
5. Its card in the graph pane, its inspector rows and its cook cache behave as before.

### Three rules that keep the scope honest

- **One body per node.** A node is ported whole or not at all. There is no node with some modes in Lisp and some in its own old OCaml body.
- **A Lisp body may call other nodes.** When one mode needs a tier 5 capability that another kind already owns, the body delegates to it: `sop/snap_to_grid` with `:fuse_points true` calls `sop/fuse`. The node is ported; it becomes fully exportable only when everything it calls is.
- **A node with a tier 5 mode that no other kind owns stays OCaml this round.** The inventory lists these as blocked.

## What Rays Lisp can do with geometry today

Today Lisp can read and write one thing: a point-owned vec3 attribute, one value per point. Everything else a geometry node needs is missing, and the gaps below define the language work in this plan. All facts were read from source at `dev` `df43b92c`.

### Geometry access

| Operator | What it does | Limits |
| --- | --- | --- |
| `(sop/attr geo :P)` | Reads a point attribute as `(array vec3)`. A deferred source, resolved when the input is cooked. | Point owner only, vec3 only. `E_ATTR_TYPE` otherwise. `lib/flow_sop/attribute_kernel.ml:36-51` |
| `(sop/with_attr geo :P values)` | Writes one vec3 per point. One cook node (`flow.with_attr`), kernel prepared once, topology preserved, copy-on-write for `P`. | Point owner only, vec3 only. `E_ATTR_COUNT` on a different count, `E_NONFINITE`. `attribute_kernel.ml:62-90` |
| `sop/iso_surface :field (fn [p] ...)` | The only catalog node with a Lisp function slot (`fn(vec3)->float`). The lattice and extraction stay OCaml. | Body must compile to a packed kernel (`E_KERNEL_FORM`), columns are float or vec3 only. `lib/sop/shapes.ml:9-96` |
| `sop/point_list`, `sop/piece_list` | Iterate points or pieces in a one-clause `for`. | At most 4,096 elements, interpreter only. |

Lisp has no access to topology, groups, element counts as values, vertex, primitive or detail attributes, float or int attributes, or creating and deleting points.

### Array language

| Capability | Today | Evidence |
| --- | --- | --- |
| Element types | `float`, `vec2`, `vec3`, `vec4`. No int, bool or record arrays. | `lib/flow/value.ml:9-12`, `ty.ml:60` |
| `map` over 1–3 arrays, one-clause `for` | CPU kernel tier and GPU. | `lib/flow_ir/core_region.ml:114-135` |
| `sum`, `fold`, `scan`, `reduce +`, `array/sum` | CPU kernel tier, element order. Never GPU. | `packed.ml:84-87` |
| Min or max of an array | No named reduction. `(fold [m seed] [p a] (min m p))` should lower but is untested. | `op.ml:321-322` |
| `filter`, `sort-by`, `array/slice`, `array/concat`, `array/nth` | Interpreter only. No kernel instruction. | `op.ml:196-218`, `eval.ml:715-740` |
| Read an array at a computed index inside a kernel (gather) | Not possible. No indexed-read instruction; a captured array cannot be a uniform. | `core.ml:74-78`, `core_region.ml:163-174`, `306-307` |
| Integer arrays | Do not exist. `array/range` returns floats; an int-typed kernel body is refused. | `core_region.ml:282` |
| Fixed-count repetition | `(fold [acc init] [i (range n)] body)`, at most 4,096, literal count. With a geometry accumulator it makes n cook nodes. No `loop` or `iterate` form. | `workspace.ml:919-923` |
| Several arrays out of one map | Not expressible. A map result cannot be a record. Multi-output regions exist only through attribute fusion. | `workspace.ml:1086-1087`, `core.ml:91` |
| Kernel instruction set | `+ - * / mod pow min max`, comparisons, `and or not`, `sin cos sqrt abs`, `length`, `noise3`, select. 64 registers per kernel. | `lib/flow/packed_ops.ml:5-21` |

### Declaring a node

A node kind can only be declared in OCaml today. Catalog kinds come from PPX-annotated OCaml records (`[@@deriving sop_params, sop_node]`) and the checked-in `lib/sop_catalog/flow_manifest.sexp`.

`defn` exists (`(defn name :context ctx [(p : type default)] body)`) and is callable by keyword, but it is inlined at each call site. It makes no card, has no parameter ranges or labels, no kernel facts, no cook identity, and its name cannot contain `/`, so `(sop/bend ...)` can never resolve to it (`lib/flow/symbol.ml:3-4`, `workspace.ml:1208-1223`).

### Precision

A packed `map` result is tainted approximate, and every `sop/` argument is an exact sink, so a kernel result must be wrapped in `(exact ...)` before it re-enters the SOP graph (`E_APPROX_SINK`). A kernel that reads cooked attributes is never placed on the GPU today; it runs on the CPU kernel tier on the cook worker (`lib/flow_sop/lower.ml:553-554`).

One existing sketch does exactly what tier 1 needs: `sketches/flow_kernel.rays` displaces a grid along its normals with `noise3`, and `lib/flow_sop/test_attribute_kernel.ml:497-510` proves it byte-identical to the OCaml `Deform.noise_displace`.

## Language additions

The ports need 28 additions to the language, grouped by the phase that introduces them. Names and signatures are proposals: the implementing agent confirms each against the checker before building it, and may rename, but may not drop a capability without moving the nodes that depend on it.

Every addition follows the repo's existing rules:

- **Graph projection.** New syntax ships with its graph projection, its `Flow_edit` gestures and a test (`AGENTS.md`).
- **Core contract.** A new operator gets a row in `specification/core.md` and cases in `tools/core_fixtures.ml`. A pure Core operator (array, math, text) also gets an implementation in `tools/web/runtime/prims.js`; a built-in that wraps `lib/rdk` (`sop/*` host sources, `graph/*`) does not, because `lib/rdk` will be compiled with js_of_ocaml (decision 4). Both pass `tools/lisp/test/web_prims.t` and `core_census.t`. This keeps the web target able to run the bodies later.
- **Kernel instructions.** A new kernel instruction is added to `lib/flow_ir/core.ml`, the CPU executor, the reference walker, and the JS printer in `lib/flow_web`. The Metal and WGSL emitters may refuse it with `E_GPU_FORM`.
- **Determinism.** Results are byte-identical at 1 and 8 domains.

### Phase A: declaring a node

| # | Addition | Proposed form | Notes |
| --- | --- | --- | --- |
| A1 | Node definition | `(defnode sop/peak :label "Peak" :category "Deform" :facts {...} [(geo : geometry) (distance : float 0.1 :min -10 :max 10 :primary true) ...] body)` | Carries everything the PPX record carries: default, label, description, folder, soft and hard range, kind, unit, primary, choices, validation. Allows `/` in the name. Makes one card and one cook node. `lib/flow/symbol.ml:3-4`, `workspace.ml:1694-1741` |
| A2 | Node sources in the build | `lib/flow_sop/nodes/*.rays`, embedded as strings by a dune rule (the pattern `tools/lisp/embed.ml` uses), registered beside the OCaml factories in `Flow_sop.Catalog`. | The manifest entry gains `(body lisp)` and `(calls ...)`. |
| A3 | Test entry | `Lisp_sop.workspace_node : string -> Sop.Node.t` and `Rdk_test_support.close_geometry ~ulps` | See Verification. |

### Phase B: elementwise work

| # | Addition | Proposed form | Unlocks |
| --- | --- | --- | --- |
| B1 | Typed attribute read and write | `(sop/attr geo :N :owner vertex :type vec3)`, `(sop/with_attr geo :Cd values :owner primitive)`. Owners `point vertex primitive detail`. Types `float int vec2 vec3 vec4 text float_array int_array`. CSR array-valued attributes are records of integer `offsets` and typed flat `values`; fixed 16-float matrix rows use this representation. `(sop/has_attr geo :N :owner point :type vec3)` is a cooked Boolean predicate for explicit native fallbacks. Defaults stay `point` and `vec3`; absent query type matches any storage. | Every tier 2 node |
| B2 | Integer arrays | `(array int)` as a packed type, int registers in the kernel tier, `int/add sub mul div mod and or xor shl shr`, comparisons, `float`, `floor`, `round`. | Int attributes, masks, all of Phase C |
| B3 | Text arrays | `(array text)`, interpreter tier only. | `groups_from_name`, `name_from_groups`, `material`, text modes of `enumerate`, `connectivity` |
| B4 | Element counts | `(sop/count geo :points)`, also `:vertices :primitives :edges`. | Generators, masks |
| B5 | Groups as masks | `(sop/group geo :name "top" :owner primitive)` returns `(array int)` of 0 and 1. `(sop/with_group geo :name "top" mask :owner primitive)`. Ordered groups carry an index array. Pattern resolution: `(sop/group_names geo "wall*" :owner point)`. | 14 group nodes, the `group` parameter of every deformer |
| B6 | Unique edges | `(sop/edges geo)` returns `{:a (array int) :b (array int)}` from `Topology_index`. Edge is the fourth group owner. | Edge-owner modes of the group nodes |
| B7 | Attribute and group metadata | `sop/rename_attr`, `sop/delete_attr`, `sop/rename_group`, `sop/delete_group`, by name or pattern. | 7 metadata nodes |
| B8 | Math | `tan atan2 acos asin exp log floor ceil sign dot cross normalize`, 4x4 matrix and quaternion helpers written in Lisp over `vec4`. | Deformers, generators |
| B9 | Index-keyed random | `(rand/at seed index)` reproducing `Rand.float_at` bit for bit (`lib/rays_math/rand.ml:20-30`), and `noise3` fractal form matching `Noise.fbm3`. | `point_jitter`, `group_random`, `attribute_randomize`, `mountain`, `attribute_noise` |
| B10 | Named reductions | `array/min`, `array/max`, componentwise, CPU kernel tier. | Bounds for `match_size`, `color_by_height` |
| B11 | Segmented reduce | `(array/segment_sum values offsets)`, also `_min _max`: one result per contiguous segment. Primitive-position inputs additionally need C1/C2; this operator does not supply indexed gathering. | `measure` with its C.15 prerequisites, `group_non_planar`, `group_backface`, face normals |

### Phase C: building and reindexing geometry

| # | Addition | Proposed form | Unlocks |
| --- | --- | --- | --- |
| C1 | Geometry from arrays | `(sop/mesh :points p :vertex_points vp :primitive_offsets po :kinds k)`, `(sop/topology geo)` returning the three index arrays. Wraps `Topology.create_owned` and `Geometry.create` (`lib/rdk/core/topology.mli:8-14`). | Every generator |
| C2 | Gather in a kernel | `(array/nth a i)` with an int index inside a `map` body, as a new kernel instruction. CPU tier this round. | Table lookups, reindexing, all of Phase D |
| C3 | Payload remap | `(sop/remap geo :points pmap :vertices vmap :primitives primmap)` carries every attribute and group through index maps. Wraps `Topology_remap` (`lib/rdk/core/topology_remap.ml`). | `blast`, `sort`, `mirror`, `duplicate`, `copy_to_points`, `merge` |
| C4 | Compaction | `(array/compact mask)` returns the kept indices; `array/scan_sum` over ints on the kernel tier. | `blast`, `compact_points`, `rewire_vertices`, count-then-fill generators |
| C5 | Stable grade | `(array/grade keys)` returns the stable sorting permutation. | `sort`, `enumerate` per piece, `separate_pieces` |
| C6 | Scatter-add | `(array/scatter_add indices values size)`, ordered. | Point normals from face normals, `extract_centroid` |
| C7 | Concatenation on the kernel tier | `array/concat` and `array/slice` lowered to the kernel tier for all element types. | `merge`, replication |
| C8 | First-match lookup | `(array/index_of keys queries)` for int and text keys. | `group_copy`, `blend_shapes` by id, `point_velocity`, `enumerate` |

C.15 additionally requires a generic canonical triangulation data capability
with original-primitive mapping. It may reuse RDK's shared ear clipping, but
must preserve its triangle emission order, projection/tolerance rules and
refusals. A triangle fan, a hidden measure callback, or an unverified call to
`sop/triangulate` does not satisfy this prerequisite: the latter has a
collapsed-quad shortcut that `Analysis.measure` does not use.

### Phase D: adjacency and iteration

| # | Addition | Proposed form | Unlocks |
| --- | --- | --- | --- |
| D1 | Adjacency arrays | `(sop/adjacency geo)` returns a record of int arrays from `Topology_index.Private.view`: `primitive_of_vertex`, `next_vertex`, `previous_vertex`, `edge_of_vertex`, `opposite_vertex`, `edge_a`, `edge_b`, `point_edge_offsets`, `point_edges`, `point_vertex_offsets`, `point_vertices`. | Every tier 4 node |
| D2 | Components | `(sop/components geo :over edges :mask m)` returns `{:id (array int) :count int}`. Wraps the union-find in `Analysis.classify_connectivity`. | `connectivity`, 6 nodes that hand-roll it today |
| D3 | Fixed-count iteration | `(iterate n init (fn [x] body))` runs the body n times inside one cook node. `n` is a parameter, not a literal. Optional `:until (fn [prev next] bool)` for a tolerance stop. | `smooth`, `attribute_blur`, `edge_relax`, `edge_equalize`, `group_edge_depth`, `group_expand` |

### Phase E: sequential graph algorithms

Three built-ins, each backed by the existing OCaml code. They need no JavaScript twin: the web target will run the same `lib/rdk` code through js_of_ocaml (decision 4). They cover algorithms that have no array formulation with the same output: a heap-ordered search, a greedy walk, a greedy colouring.

| # | Addition | Proposed form | Unlocks |
| --- | --- | --- | --- |
| E1 | Shortest-path tree | `(graph/shortest_paths adjacency lengths roots)` returns distance and parent arrays, with the index tie-breaks of `lib/rdk/mesh/transform_ops.ml:302-368`. | `distance_along_geometry`, `edge_transport`, `group_find_path` |
| E2 | Chain walk | `(graph/chains adjacency mask)` returns ordered point chains as a ragged array. | `poly_path`, `carve`, `poly_bridge`, `poly_loft`, `skin` |
| E3 | Greedy colouring | `(graph/color adjacency order)` | `graph_color` |

## Node inventory

All 162 nodes, grouped by the phase that ports them. A node sits in the phase where its hardest non-tier-5 mode becomes expressible. "Needs" refers to the numbered language additions. Kernel paths are relative to `lib/rdk/` unless they start with `lib/`. The implementing agent re-reads each kernel before porting it and moves a node to a later phase when the read shows it needs more.

### Phase B: elementwise, 43 nodes

| Node | Tier | Needs | Notes | OCaml kernel |
| --- | --- | --- | --- | --- |
| `noise_displace` | 1 | none | Pilot. Already proven byte-identical in `lib/flow_sop/test_attribute_kernel.ml:497-510`. | `mesh/deform.ml:586` |
| `match_axis` | 1 | B1 B8 | Vertex N inverse transpose and explicit absent/wrong-storage fallback need typed attribute/query support. Second pilot. | `mesh/match_size.ml:25` |
| `transform` | 1 | B1 B5 B8 | Vertex N by inverse transpose. | `mesh/transform_ops.ml:195` |
| `match_size` | 1 | B5 B10 B11 | Perimeter, area and volume fit modes use B11. | `mesh/match_size.ml:207` |
| `peak` | 1 | B1 B5 | Missing N calls `sop/normals`. | `mesh/deform.ml:244` |
| `mountain` | 1 | B1 B5 B9 | Same fallback. Optional height output. | `mesh/deform.ml:495` |
| `bend` | 1 | B1 B5 B8 | Optional capture attribute. | `mesh/deform.ml:361` |
| `snap_to_grid` | 1 | B8 | `:fuse_points` calls `sop/fuse`. | `mesh/fuse_grid.ml:313` |
| `point_jitter` | 1 | B1 B2 B9 | Hash keyed by an id attribute. | `gen/point_jitter.ml:31` |
| `rest_position` | 1 | B1 | Rest normals mode calls `sop/normals`. | `mesh/motion.ml:135` |
| `distance_from_target` | 2 | B1 | Analytic point, axis, plane. | `mesh/transform_ops.ml:626` |
| `uv_transform` | 2 | B1 |  | `mesh/uv_ops.ml:356` |
| `attr_from_image` | 2 | B1 | Needs an image sample usable in a kernel body. Verify one exists before starting. | `attrib/attribute_image.ml:5` |
| `attribute_noise` | 2 | B1 B9 |  | `attrib/attribute_generate.ml:1204` |
| `attribute_noise_quaternion` | 2 | B1 B9 | Shares the kernel above. | `attrib/attribute_generate.ml:1204` |
| `attribute_remap` | 2 | B1 | Has a ramp parameter. Verify Lisp can declare and evaluate a ramp. | `attrib/attribute_generate.ml:1374` |
| `attribute_randomize` | 2 | B1 B3 B9 | 1,040 lines, many distributions. Largest Phase B port; do it last. | `attrib/attribute_generate.ml:1000` |
| `attribute_fade` | 2 | B1 |  | `attrib/attribute_fade.ml:130` |
| `color_by_height` | 2 | B1 B10 | Writes vec4 `Cd`. | `attrib/color_by_height.ml:3` |
| `set_float`, `set_int`, `set_vector`, `set_orient` | 2 | B1 B4 | Constant attributes. B4 owner counts allow creating missing attributes at vertex/primitive/detail owners. | `lib/sop/attributes.ml:1291`, `1322`, `1352`, `1482` |
| `set_color` | 2 | B1 B5 | Optional group. | `lib/sop/attributes.ml:1612` |
| `set_transform` | 2 | B1 B4 | Writes a 16-float array attribute per point. B1 CSR storage and B4 point counts cover absent attributes and empty geometry. | `lib/sop/attributes.ml:1523` |
| `swap_attributes` | 2 | B1 B7 | Includes `P`. | `attrib/attribute_lifecycle.ml:371` |
| `delete_attributes`, `rename_attributes` | 2 | B7 | Pattern rules. | `attrib/attribute_lifecycle.ml:43`, `156` |
| `delete_edge_group`, `rename_edge_group` | 2 | B7 |  | `core/geometry.ml:168`, `175` |
| `group_delete`, `group_rename` | 2 | B7 |  | `attrib/group_ops.ml:3836`, `3791` |
| `group_combine` | 2 | B5 | Boolean fold over groups by pattern. | `attrib/group_ops.ml:2871` |
| `group_invert` | 2 | B5 B6 |  | `attrib/group_ops.ml:3811` |
| `group_random` | 2 | B5 B6 B9 |  | `attrib/group_ops.ml:2577` |
| `group_bounds` | 2 | B5 B6 | Analytic box and sphere tests. | `attrib/group_ops.ml:2778` |
| `group_non_planar` | 2 | B5 B11 |  | `attrib/group_ops.ml:3388` |
| `group_backface` | 2 | B5 B11 | Threshold `64 * epsilon`. | `attrib/group_ops.ml:3485` |
| `ordered_group` | 2 | B5 | Needs ordered groups, not only masks. | `core/group.ml:49` |
| `groups_from_name`, `name_from_groups` | 2 | B3 B5 | Text attributes. | `attrib/group_ops.ml:2063`, `2197` |
| `material` | 2 | B1 B3 | Primitive text and vec4. | `attrib/material_assign.ml:1` |

### Phase C: building and reindexing, 42 nodes

| Node | Tier | Needs | Notes | OCaml kernel |
| --- | --- | --- | --- | --- |
| `uv_project` | 3 | B1 B8 B11 C1 C2 | Whole port moved from B: writes vertex vec2, with topology/gather and segmented seam/pole reductions. No projection mode omitted. | `mesh/uv_ops.ml:154` |
| `measure` | 3 | B1 B5 B8 B10 B11 C1 C2; canonical triangulation + primitive mapping | Whole port moved from B. Primitive perimeter/area/signed volume, group selection, both accumulations, preserved unselected values and optional detail total; n-gons require the native ear-clipping data contract. | `core/analysis.ml:75-239`, `487-519` |
| `line` | 3 | C1 | 91 lines. First generator. | `gen/line_geometry.ml:15` |
| `points` | 3 | C1 |  | `gen/point_generate.ml:276` |
| `circle`, `grid` | 3 | C1 |  | `gen/plane_generators.ml:95`, `188` |
| `box` | 3 | C1 | 144 fused multiply-adds in the OCaml object. | `gen/box_generator.ml:25` |
| `uv_sphere` | 3 | C1 |  | `gen/uv_sphere.ml:66` |
| `torus`, `tube` | 3 | C1 | 661 fused multiply-adds in this object. | `gen/parametric_generators.ml:94`, `619` |
| `platonic` | 3 | C1 C2 | Table driven. | `gen/parametric_generators.ml:1293` |
| `spiral` | 3 | C1 | 733 lines with ramps. | `gen/spiral.ml:227` |
| `bound` | 3 | C1 B10 | Box or sphere of the input. | `mesh/bound.ml:225` |
| `merge` | 3 | C3 C7 | Any number of inputs. | `mesh/mesh_merge.ml:259` |
| `mirror` | 3 | C3 C7 | Reversed polygons on the copy. | `mesh/mirror_geometry.ml:3` |
| `duplicate` | 3 | C3 C7 | Cumulative matrix per copy. | `mesh/instance_copy.ml:744` |
| `copy_to_points` | 3 | C3 C7 C8 | Piece mode interns names. | `mesh/instance_copy.ml:1495` |
| `reverse` | 2 | C3 | Permutes vertices within primitives. | `mesh/reverse_faces.ml:18` |
| `sort` | 3 | C3 C5 B9 | Morton key for the spatial mode. | `mesh/ordering.ml:333` |
| `blast`, `blast_by_attribute` | 2 | C3 C4 |  | `mesh/deletion.ml:388`, `mesh/blast_by_attribute.ml:68` |
| `compact_points` | 2 | C3 C4 |  | `mesh/compact_points.ml:3` |
| `rewire_vertices` | 2 | C3 C4 | Recursive option chases pointers to a fixed point. | `mesh/rewire_vertices.ml:164` |
| `ends` | 3 | C1 C3 | At most one vertex added per primitive. | `curve/curve_ops.ml:629` |
| `resample` | 3 | C1 C4 B11 | Max-length mode has data-dependent size. | `curve/resample_curves.ml:67` |
| `revolve` | 3 | C1 | On-axis vertices collapse. | `mesh/revolve.ml:6` |
| `sweep` | 3 | C1 | Frame transport is an ordered scan per curve. | `curve/sweep.ml:642` |
| `sweep_circle`, `polywire` | 3 | C1 C2 | Both run `curve/polywire.ml` (1,434 lines). The joint and zipper code was not read. May move to Phase E or blocked. | `mesh/sweep_circle.ml:637`, `curve/polywire.ml:92` |
| `point_generate` | 3 | C1 C4 B9 | Sum of per-point counts. | `gen/point_generate.ml:276` |
| `point_replicate` | 3 | C1 C4 B9 | Random, radical inverse and noise. | `gen/point_replicate.ml:249` |
| `extract_point_from_curve` | 3 | C1 C4 | Count then fill. | `mesh/extract_point_curve.ml:66` |
| `separate_pieces` | 2 | C5 C8 | Uses `Float.next_after` (`:224`); needs that operator. | `mesh/separate_pieces.ml:439` |
| `enumerate` | 2 | C5 C8 B3 | Per-piece rank. | `attrib/attribute_enumerate.ml:252` |
| `group_copy` | 2 | C8 B5 | Match by int or text key. | `attrib/group_ops.ml:4027` |
| `blend_shapes` | 2 | C8 B1 | Match by id. | `attrib/blend_shapes.ml:232` |
| `point_velocity` | 2 | C8 B1 | Match by attribute across frames. | `mesh/motion.ml:483` |
| `attribute_interpolate` | 2 | C2 B1 | Array-valued attributes. 1,209 lines. | `attrib/attribute_interpolate.ml:774` |
| `edge_transport_curves` | 4 | B11 | Running sum per curve: a segmented scan. | `mesh/edge_transport.ml:867` |

### Phase D: adjacency and iteration, 31 nodes

| Node | Tier | Needs | Notes | OCaml kernel |
| --- | --- | --- | --- | --- |
| `connectivity` | 4 | D2 | First Phase D port. | `core/analysis.ml:533` |
| `normals` | 4 | D1 C6 | Default is vertex owner with a cusp angle. Unblocks the fallbacks in `peak`, `mountain`, `rest_position`. | `core/normal_ops.ml:449` |
| `attribute_blur` | 4 | D1 D3 |  | `attrib/attribute_blur.ml:323` |
| `smooth` | 4 | D1 D3 | Thin wrapper over the blur kernel. | `mesh/smooth.ml:105` |
| `attribute_laplacian` | 4 | D1 | Uniform and cotangent. | `mesh/laplacian.ml:241` |
| `measure_curvature` | 4 | D1 D3 | Triangulates n-gons internally. | `mesh/curvature.ml:66` |
| `polyframe` | 4 | D1 |  | `mesh/polyframe.ml:564` |
| `edge_relax` | 4 | D1 D3 | Two inputs, same topology. | `mesh/edge_relax.ml:39` |
| `edge_equalize` | 4 | D1 D3 | Tolerance stop. | `mesh/edge_ops.ml:555` |
| `edge_straighten` | 4 | D1 D2 | Closed-form 3x3 eigen solve per component. | `mesh/edge_ops.ml:274` |
| `circle_from_edges` | 4 | D1 D2 | Ordered walk per loop. May need E2. | `curve/circle_from_edges.ml:123` |
| `crease` | 4 | D1 B6 |  | `mesh/crease.ml:238` |
| `edge_divide` | 4 | D1 C1 C3 | Adds k points per selected edge. | `mesh/subdivide.ml:4347` |
| `edge_cusp` | 4 | D1 D2 C3 | Splits points along edges. | `mesh/facet.ml:911` |
| `point_split` | 4 | D1 C3 C5 | Per-point sort and dedupe. | `mesh/point_split.ml:377` |
| `convert_line` | 4 | D1 C1 | `connect_path` mode needs point clusters: blocked unless it can call `sop/fuse`. | `curve/curve_ops.ml:535` |
| `uv_auto_seam` | 4 | D1 D2 |  | `mesh/uv_ops.ml:540` |
| `uv_unitize` | 4 | D2 B11 | Per-face mode needs only B11. | `mesh/uv_ops.ml:706` |
| `promote_attributes` | 4 | D1 C5 C6 | Twelve reductions including median and mode. 1,403 lines. | `attrib/attribute_promote.ml:1393` |
| `attribute_copy` | 2 | D1 C8 | Cross-owner projection uses adjacency. | `attrib/attribute_copy.ml:460` |
| `group_edges` | 4 | D1 B6 B11 | Dihedral angle from incident face normals. | `mesh/edge_ops.ml:65` |
| `group_unshared` | 4 | D1 |  | `attrib/group_ops.ml:2390` |
| `group_edge_depth` | 4 | D1 D3 |  | `attrib/group_ops.ml:2546` |
| `group_expand` | 4 | D1 D3 | 870 lines of constraint modes, skimmed only. | `attrib/group_ops.ml:1642` |
| `group_boundary_components` | 4 | D1 D2 | Number of output groups depends on data. | `attrib/group_ops.ml:2403` |
| `group_promote_boundary` | 4 | D1 |  | `attrib/group_ops.ml:868` |
| `group_promotions` | 4 | D1 |  | `attrib/group_ops.ml:1906` |
| `group_normal` | 2 | D1 B5 | Point and edge owners need reverse incidence. | `attrib/group_ops.ml:3158` |
| `group_range`, `group_ranges` | 2 | D2 B5 | Connected mode needs components. | `attrib/group_ops.ml:3603`, `3746` |

### Phase E: sequential graph algorithms, 12 nodes

| Node | Needs | Notes | OCaml kernel |
| --- | --- | --- | --- |
| `distance_along_geometry` | E1 | Heap-ordered search with decrease-key. | `mesh/transform_ops.ml:393` |
| `edge_transport` | E1 D2 | Tree build with index tie-breaks. Bit parity from arrays is doubtful without the built-in. | `mesh/edge_transport.ml:107` |
| `edge_transport_parent` | D3 C8 | Breadth-first over a tree given by an attribute. | `mesh/edge_transport.ml:521` |
| `group_find_path` | E1 | Depth, length and index tie-breaks. | `mesh/group_path.ml:343` |
| `graph_color` | E3 | Sequential greedy per component. | `mesh/graph_color.ml:40` |
| `poly_path` | E2 | `connect_end_points` mode needs point clusters: blocked unless it can call `sop/fuse`. | `curve/poly_path.ml:66` |
| `carve` | E2 C4 | About 1,300 lines in three modes; only the entry points were read. | `curve/curve_ops.ml:2857` |
| `poly_bridge` | E2 C1 | Greedy pairing and alignment. | `mesh/poly_bridge.ml:452` |
| `poly_loft`, `skin` | E2 C1 | Same engine. Greedy zipper between sections (`:216`). Equal-count sections reduce to a regular grid. | `mesh/poly_loft.ml:303` |
| `edge_flip` | D1 C3 | Rewires two polygons per edge, rejects overlapping selections. Half of the body was not read. | `mesh/edge_flip.ml:19` |
| `subdivide` | D1 D3 C1 C3 | 4,607 lines. The whole-mesh path is stencils and formulas. The partial-group path stitches cracks, which is tier 5 with no other kind to call: blocked unless that path is split into its own kind first. Last port of the round. | `mesh/subdivide.ml:4198` |

### Stays OCaml this round: 31 tier 5 nodes

| Reason | Nodes |
| --- | --- |
| Exact predicates | `boolean`, `boolean_fracture`, `boolean_seam`, `convex_hull`, `triangulate_2d` |
| Spatial index (nearest point, closest surface point, ray) | `ray`, `fuse`, `clean`, `join_curves`, `boolean_detect`, `intersection_analysis`, `distance_from_geometry`, `group_transfer`, `attribute_mirror`, `soft_transform`, `attribute_transfer`, `attribute_transfer_surface`, `attribute_transfer_all` |
| Data-dependent mesh surgery | `clip`, `edge_collapse`, `dissolve`, `poly_bevel`, `poly_extrude`, `poly_fill`, `poly_cut`, `triangulate`, `facet`, `remesh`, `poly_reduce` |
| Sparse solver | `uv_flatten`, `uv_relax` |

Six of the spatial-index nodes (`group_transfer`, `attribute_mirror`, `soft_transform` and the three `attribute_transfer` nodes) become a gather plus a blend once a nearest-point built-in exists. That is the natural first step of a later round.

### Nothing to port: 3 nodes

`switch`, `null` and `exploded_view` pass their input through. They are graph wiring.

## Phases and gates

Five phases, 58 tasks, plus two optional GPU tasks in Placement and fusion. Each phase builds its language additions first, then ports its nodes in batches, then closes with a review task that runs the full gates. A phase does not start until the previous review task is ticked.

Task kinds: **L** adds language, **P** ports nodes with the recipe below, **R** reviews. The "Depends" column lists what must be ticked first.

### The port recipe (every P task, per node)

1. Read the OCaml kernel named in the inventory and its SOP wrapper in `lib/sop/`. Write down every mode, every attribute read and written, and every place float order matters.
2. If the read shows a need the phase does not have, stop and move the node to the phase that has it. Record the move in the ledger.
3. Copy the node's parameter block from `lib/sop_catalog/flow_manifest.sexp` into the `defnode` header unchanged.
4. Write the body in `lib/flow_sop/nodes/<key>.rays`. Keep the same order of arithmetic as the OCaml source so differences come only from fused multiply-add.
5. Add its function to `lib/sop/test_lisp_ports.ml` covering every row of the Verification gate.
6. Run the test with both bodies registered. When it passes, remove the OCaml registration from `lib/sop_catalog/sop_catalog.ml`, promote the manifest, and run `dune build @lib/sop/runtest @lib/flow_sop/runtest @lib/sop_catalog/runtest @test/dependency_gate`.
7. Run `dune build @runtest-native`. Refresh only the goldens this node changed and list them in the commit message.
8. Record wall time at 1,000,000 elements for the Lisp body and the OCaml kernel in `specification/sop-lisp/results.md`. The ratio does not block the port (decision 6); a ratio above 2x is added to "Speed regressions accepted with the ports" in this file with its cause.
9. One commit per node. Tick the node in the ledger.

### Phase A: a node can be declared in Lisp

Implementation prerequisite move, 2026-10-12: the complete `noise_displace`
pilot needs the cook-context seed and deletion of invalidated point/vertex N.
`sop/context_seed` and the narrow owner/pattern `sop/delete_attr` primitive are
therefore advanced into A.6. The latter is part of B.6's broader metadata work;
this move does not complete B.6. Their language, native/Web contract and graph
gates are tracked with the pilot in `results.md`. Both `height_2d` and
`normal_3d`, and explicit/context seed selection, remain required; no mode is
delegated to the old `noise_displace` body or dropped.

| Task | Kind | Depends | Files | What | Gate |
| --- | --- | --- | --- | --- | --- |
| A.0 | R |  | this plan | Owner answers the open questions in Risks. Record the answers at the top of `specification/sop-lisp/plan.md`. | Answers recorded |
| A.1 | L | A.0 | `lib/rdk/test_support/rdk_test_support.ml`, `test/lisp_sop/lisp_sop.ml`, `.mli` | `close_geometry ~ulps` and `Lisp_sop.workspace_node`. | Helper tests: equal passes, one-ulp difference passes at `~ulps:1`, a changed int fails |
| A.2 | L | A.0 | `lib/flow/workspace.ml`, `symbol.ml`, `macro.ml`, `specification/flow.md` | `defnode` reader and checker (A1): parameter metadata, `/` in names, typed slots, validation clauses. | Checker tests for each `E_SHAPE` case, each metadata key, a name clash with an OCaml kind |
| A.3 | L | A.2 | `lib/flow_sop/lower.ml`, `catalog.ml`, new `lib/flow_sop/lisp_node.ml` | Lower a `defnode` call to one cook node with a parameter schema, kernel facts and a cook cache key. | A test `defnode` cooks; cache hit on equal values, miss on each changed parameter |
| A.4 | L | A.3 | `lib/flow_sop/nodes/`, `lib/flow_sop/dune`, `manifest.ml`, `tools/flow_manifest.ml` | Embed node sources, register them in the catalog, write `(body lisp)` and `(calls ...)` in the manifest (A2). | Manifest diff promoted; `@test/dependency_gate` passes |
| A.5 | L | A.4 | `lib/flow_graph`, `lib/pxui_graph`, `lib/flow_sop` edit tests | Graph card, inspector rows and `Flow_edit` gestures for a Lisp-bodied kind are identical to an OCaml-bodied one. | Projection test: the two cards are structurally equal |
| A.6 | P | A.1 A.5 | `nodes/noise_displace.rays` | Pilot port of `noise_displace`. | Full Verification gate |
| A.R | R | A.6 | `specification/sop.md`, `specification/flow.md`, `lib/sop_catalog/AGENTS.md` | Review the form against the pilot. Update the specs: "one declaration" now has a Lisp form. | `check.exe --ship`, `@runtest-native` |

### Phase B: elementwise, 43 nodes

| Task | Kind | Depends | What | Gate |
| --- | --- | --- | --- | --- |
| B.1 | L | A.R B.2 B.3 | Typed attribute read and write for all owners and types (B1), CSR offsets/values and cooked presence/type queries. Float/tuple work may proceed alongside the integer/text foundations; completion waits for every storage type. | Round trip per owner and type; lazy query fallback, `E_ATTR_TYPE`, `E_ATTR_COUNT` cases |
| B.2 | L | A.R | Integer arrays and int kernel ops (B2). | Fixtures in `tools/core_fixtures.ml`; 1 and 8 domain equality |
| B.3 | L | B.2 | Text arrays, interpreter tier (B3). | Round trip of a text attribute |
| B.4 | L | B.2 | Element counts (B4) and unique edges (B6). | Counts and edge arrays equal `Topology_index` on five fixtures |
| B.5 | L | B.2 | Groups as masks, ordered groups, pattern resolution (B5), and source-owner membership promoted to affected points. | Round trip per owner; pattern cases from `Attribute_pattern` tests; native any-incidence point masks, count origins and direct graph edits |
| B.6 | L | B.1 | Attribute and group metadata operators (B7). | Rename, delete, conflict policy |
| B.7 | L | A.R | Math operators and the matrix and quaternion helpers (B8). | Fixtures per operator against `Stdlib` and `Rays_math` |
| B.8 | L | B.2 | `rand/at` and fractal `noise3` (B9). | Bit equality with `Rand.float_at` and `Noise.fbm3` over 100,000 indices |
| B.9 | L | B.2 | `array/min`, `array/max` (B10) and segmented reduce (B11). | Equality with a reference fold; 1 and 8 domains |
| B.10 | P | B.1 B.5 B.7 B.8 B.9 | Deformers: `match_axis`, `transform`, `match_size`, `peak`, `mountain`, `bend`, `snap_to_grid`, `point_jitter`, `rest_position`. | Recipe |
| B.11 | P | B.1 B.4 B.5 | Setters: `set_float`, `set_int`, `set_vector`, `set_orient`, `set_color`, `set_transform`. | Recipe |
| B.12 | P | B.6 | Metadata: `delete_attributes`, `rename_attributes`, `swap_attributes`, `delete_edge_group`, `rename_edge_group`, `group_delete`, `group_rename`. Generic Lisp-authored encoded parameter validation/canonicalization must preserve native schema views and cook keys. | Recipe and encoded parameter boundary |
| B.13 | P | B.4 B.5 B.8 B.9; C.2 for indexed packed-word unpacking | Groups: `group_combine`, `group_invert`, `group_random`, `group_bounds`, `group_non_planar`, `group_backface`, `ordered_group`. Gather alone does not supply a packed-word group source or satisfy whole-node performance. | Recipe |
| B.14 | P | B.3 B.5; C.2 and typed Text leaf kernels for indexed Text assignment | Text: `groups_from_name`, `name_from_groups`, `material`. Numeric gather alone does not accelerate Name From Groups' Text result. | Recipe |
| B.15 | P | B.1 B.7 B.8 B.9 | Attributes: `distance_from_target`, `uv_transform`, `attr_from_image`, `attribute_noise`, `attribute_noise_quaternion`, `attribute_remap`, `attribute_composite`, `attribute_fade`, `color_by_height`. Whole ports moved after their native prerequisite audits: `uv_project` to C.14 and `measure` to C.15. | Recipe |
| B.16 | P | B.15 | `attribute_randomize`. | Recipe |
| B.F | L | B.10 | Fuse adjacent Lisp-bodied nodes into one kernel pass (see Placement and fusion). | A chain of four deformers cooks with one kernel run; bytes equal to the unfused chain |
| B.R | R | B.10–B.16 B.F | Review Phase B. Update `specification/core.md` and `flow.md`. | `check.exe --ship`, `@runtest-native`, `@qualification-web` |

### Phase C: building and reindexing, 42 nodes

| Task | Kind | Depends | What | Gate |
| --- | --- | --- | --- | --- |
| C.1 | L | B.1 B.2 B.4 | `sop/mesh` and `sop/topology` (C1). Advanced to the completed typed attribute/integer/topology foundations for the full Peak normal fallback and Match Size metric/topology mapping; the former B.R dependency created a B.10 → C.1 → B.R → B.10 cycle. Indexed consuming recipes additionally use C.2. | Build each of five plain fixtures from its own arrays; topology + P geometry bytes equal; no implicit preservation of absent constructor payloads |
| C.2 | L | B.1 B.2 B.3 | Gather instruction in the kernel tier (C2), advanced before B.R for B.13/B.14's indexed recipes. Numeric gathers and the required Text leaf extension have separate proofs; see `gather-design.md`. | Fixtures; out-of-range index is `E_ARRAY_RANGE`; 1 and 8 domains; lazy/error-order, ownership, actual JavaScript and GPU refusal checks |
| C.3 | L | C.1 | `sop/remap` (C3). | Identity maps reproduce the input; a permutation carries every attribute and group |
| C.4 | L | C.2 | `array/compact`, `array/scan_sum` (C4), `array/scatter_add` (C6). | Reference equality; 1 and 8 domains |
| C.5 | L | C.2 | `array/grade` (C5), `array/index_of` (C8). | Stability on equal keys; missing key result |
| C.6 | L | B.R | `array/concat` and `array/slice` on the kernel tier (C7). | All element types |
| C.7 | P | C.1 | Simple generators: `line`, `points`, `circle`, `grid`, `box`, `uv_sphere`, `torus`, `tube`, `platonic`, `spiral`, `bound`. | Recipe |
| C.8 | P | C.3 C.4 C.6 | Reindexing: `merge`, `mirror`, `duplicate`, `copy_to_points`, `reverse`, `sort`, `blast`, `blast_by_attribute`, `compact_points`, `rewire_vertices`. | Recipe |
| C.9 | P | C.1 C.4 | Curves: `ends`, `resample`, `revolve`, `sweep`, `edge_transport_curves`. | Recipe |
| C.10 | P | C.9 | `sweep_circle`, `polywire`. Read `curve/polywire.ml` fully first and re-tier. | Recipe, or a recorded move |
| C.11 | P | C.1 C.4 C.5 | Point generators: `point_generate`, `point_replicate`, `scatter`, `extract_point_from_curve`, `extract_centroid`. | Recipe |
| C.12 | P | C.5 | Matching: `separate_pieces`, `enumerate`, `group_copy`, `blend_shapes`, `point_velocity`, `attribute_interpolate`. | Recipe |
| C.13 | P | C.4 | `iso_surface`. | Recipe |
| C.14 | P | B.1 B.7 B.9 C.1 C.2 | `uv_project`: complete native mode/axis/seam/pole behavior, advanced to this phase for topology, gather and segmented reductions (language B1/B8/B11+C1/C2). | Recipe |
| C.15 | P | B.1 B.5 B.7 B.9 C.1 C.2; canonical triangulation + primitive mapping capability | `measure`: complete perimeter/area/signed-volume modes, primitive-group selection, per-element/throughout accumulation, preserved unselected values and optional detail total. Provide generic canonical native ear-clipping data with original-primitive mapping before this port; reuse shared RDK rather than a hidden measure callback. | Recipe; native concave/degenerate/curve, selected/empty-group, reversed-volume and original-primitive mapping cases |
| C.R | R | C.7–C.15 | Review Phase C. | `check.exe --ship`, `@runtest-native`, `@qualification-web` |

### Phase D: adjacency and iteration, 31 nodes

| Task | Kind | Depends | What | Gate |
| --- | --- | --- | --- | --- |
| D.1 | L | C.R | `sop/adjacency` (D1). | Every array equals `Topology_index.Private.view` on five fixtures |
| D.2 | L | D.1 | `sop/components` (D2). | Ids equal `Analysis.classify_connectivity` |
| D.3 | L | C.R | `iterate` with optional `:until` (D3), its graph projection and gestures. | n passes run in one cook node; projection test |
| D.4 | P | D.2 | `connectivity`, `normals`. Then replace the `sop/normals` fallbacks' dependency note in the manifest. | Recipe |
| D.5 | P | D.1 D.3 | Smoothing: `attribute_blur`, `smooth`, `attribute_laplacian`, `measure_curvature`, `polyframe`, `edge_relax`, `edge_equalize`. | Recipe |
| D.6 | P | D.2 | Edge shaping: `edge_straighten`, `circle_from_edges`, `crease`, `edge_divide`, `edge_cusp`, `point_split`, `convert_line`. | Recipe |
| D.7 | P | D.2 | UV and attributes: `uv_auto_seam`, `uv_unitize`, `promote_attributes`, `attribute_copy`. | Recipe |
| D.8 | P | D.2 D.3 | Groups: `group_edges`, `group_unshared`, `group_edge_depth`, `group_expand`, `group_boundary_components`, `group_from_attribute_boundary`, `group_promote_boundary`, `group_promotions`, `group_normal`, `group_range`, `group_ranges`. | Recipe |
| D.R | R | D.4–D.8 | Review Phase D. | `check.exe --ship`, `@runtest-native`, `@qualification-web` |

### Phase E: sequential graph algorithms, 12 nodes

| Task | Kind | Depends | What | Gate |
| --- | --- | --- | --- | --- |
| E.1 | L | D.R | `graph/shortest_paths` (E1). | Distances and parents equal the OCaml search on five fixtures, including ties |
| E.2 | L | D.R | `graph/chains` (E2). | Chains equal `poly_path` on five fixtures |
| E.3 | L | D.R | `graph/color` (E3). | Colours equal `graph_color` |
| E.4 | P | E.1 | `distance_along_geometry`, `edge_transport`, `edge_transport_parent`, `group_find_path`. | Recipe |
| E.5 | P | E.2 E.3 | `graph_color`, `poly_path`, `carve`, `poly_bridge`, `poly_loft`, `skin`, `edge_flip`. | Recipe |
| E.6 | P | E.5 | `subdivide`. Split the partial-group crack path into its own OCaml kind first, then port the whole-mesh path. | Recipe |
| E.R | R | E.4–E.6 | Close the round: count ported, blocked and moved nodes; fill `specification/sop-lisp/results.md`; update `AGENTS.md` Direction; list which OCaml kernels are now reachable only from tests. | `check.exe --ship`, `@runtest-native`, `@qualification`, `@qualification-web` |

### Standing rules for every task

- Never `dune --force`, never clean `_build`. Warnings are errors. Build glue is OCaml under Dune.
- Never weaken a test or loosen a threshold. A golden changes only under step 7 of the recipe.
- Do not run the full `@runtest` per task; it includes an 11-minute corpus sweep. Run the focused aliases, and `check.exe --ship` in review tasks.
- A public `.mli` change is accepted with `dune promote tools/api_manifest/api_stable.json` in its own commit.
- Forbidden tokens under `lib/`, `examples/`, `sketches/`: `RAYS_WEB`, `RAYS_BACKEND`, `RAYS_RENDERER`, `RAYS_HEADLESS`, `--backend`, `--renderer`, `--headless`, `Dynlink`.
- `flow` never depends on geometry. The new `sop/` operators are declared in `lib/flow_sop`, as `sop/attr` is today.

### Keeping it lean

- Wrap, do not rewrite. `sop/mesh`, `sop/remap`, `sop/adjacency`, `sop/components` and the `graph/` operators are thin wrappers over functions that already exist in `lib/rdk`. No new algorithm is written for them.
- Extend existing operators before adding new ones. Typed attributes extend `sop/attr` and `sop/with_attr`; gather is `array/nth` given a kernel instruction; `array/concat` and `array/slice` gain a kernel lowering.
- No addition without a node. Before building an addition, check that at least one unported node still needs it. Drop it otherwise and note that in the ledger.
- Helpers shared by several bodies (matrix, quaternion, bounds) are `defn`s in one file, `lib/flow_sop/nodes/prelude.rays`. No helper is copied into a node file.
- One test file for all ports, one function per node, shared input fixtures.
- A node body that grows past its OCaml kernel in lines is a signal to stop and check whether a missing primitive is being emulated.

## Verification

Every port keeps its OCaml node as the oracle until the port passes, and the comparison is exact for everything except the last bits of floats. The reason for that exception is the first open question in Risks.

### Why floats cannot be compared bit for bit

`ocamlopt` 5.3.0 on arm64 fuses `a *. b +. c` into one `fmadd` instruction wherever it can. The source has no `Float.fma` at those sites. Counted with `objdump -d` on the built objects: 661 fused instructions in `Parametric_generators`, 255 in `Spiral`, 144 in `Box_generator`, 76 in `Noise`, 52 in `Transform_ops`, 43 in `Deform`.

Ordinary Lisp multiplication and addition remain separately rounded, as
specified in `specification/flow.md`. Explicit `fma` and checked FMA are now
available in the reference, CPU and JavaScript paths; the JavaScript path
emulates binary64 single rounding exactly. Ports can therefore reproduce
verified native fused sites explicitly. Automatic contraction of ordinary
multiply/add expressions would change their rounding contract and remains
undecided; explicit FMA support does not enable it.

### The gate for one ported node

| Check | Rule | How |
| --- | --- | --- |
| Integer data | Exactly equal to the OCaml node: point, vertex and primitive counts, `vertex_points`, `primitive_offsets`, `primitive_kinds`, int attributes, group bits and order, edge groups, attribute names, owners and order. | Compare each array with `=`. |
| Float data | Every float within the node's stated tolerance of the OCaml value. Default tolerance: 4 units in the last place of the larger magnitude, per component. A node that needs more states its bound and why in its test. | New helper `Rdk_test_support.close_geometry ~ulps`. |
| Self-consistency | The Lisp body is bit-identical with itself at 1 and 4 domains, at grains 1, 97 and 4096, and between the reference interpreter and the CPU kernel tier. | `Rdk_test_support.geometry_bytes` (Marshal, bit-exact). Never `equal_geometry`, which treats `0.` and `-0.` as equal. |
| Parameters | Same parameter names, defaults, soft and hard ranges, labels, folders and validation errors as the manifest entry before the port. | `lib/sop_catalog/flow_manifest.sexp` diff shows only the facts line and the body marker. |
| Cache identity | A changed parameter misses the cook cache; an unchanged one hits. | The existing `cache_identity` pattern, `lib/sop/test_nodes.ml:389-416`. |
| Refusals | Non-finite parameters and wrong input types are refused with the same error code as before. | The existing `reject_nonfinite` pattern, `test_nodes.ml:338-357`. |
| Inputs | At least: the node's manifest defaults, one non-default value per parameter, an empty geometry, a single element, a 4,225-point grid, and a mixed polygon and polyline input. | Inputs built with `Lisp_sop.snapshot`. |

### Where the tests live

One new file, `lib/sop/test_lisp_ports.ml`, holds one function per ported node, in the alias `@lib/sop/runtest`. It follows the pattern `lib/sop/test_nodes.ml:444-475` already uses for `sop/iso_surface`: build the Lisp node through the real lowering, build the OCaml node with `from_factory Nodes.X.factory values inputs`, cook both, compare.

`Lisp_sop.node_result` wraps its text in a single `graph` (`test/lisp_sop/lisp_sop.ml:51`), so it cannot load a node defined in another file. Phase 0 adds `Lisp_sop.workspace_node`, which takes a whole workspace text.

### Goldens that will change

A port changes float bits at fused sites, so anything that digests or renders geometry made by that node can change: `test/sop_render_parity.ml` PNG bytes, the path tracer film digests, PXUI parity goldens that show a viewport. Each such change is refreshed in the same commit as the port, listed by file in the commit message, and only after the gate above passes. No golden is refreshed for a node that was not ported in that commit.

### Commands

```sh
dune build @lib/sop/runtest            # the port oracles
dune build @lib/flow_sop/runtest       # lowering and attribute kernels
dune build @lib/sop_catalog/runtest    # manifest diff, accept with dune promote
dune build @test/dependency_gate
dune build @runtest-native             # render parity after a golden refresh
_build/default/tools/check.exe --ship  # once per phase
```

## Placement and fusion

Ported nodes run on the parallel CPU kernel tier by default, and a qualifying kernel may be placed on the GPU with a readback. Two optional tasks, G.1 and G.2, extend the existing GPU route to ported nodes. Fusion across adjacent Lisp nodes is task B.F.

### The GPU option

"Exact" in Rays means explicitly materialized, not 64-bit. A GPU result is 32-bit and can differ between devices, so the checker refuses to let one flow silently into a catalog call, a cache key, saved state or an export (`E_APPROX_SINK`). `(exact x)` is the author's explicit readback point: the GPU result is read back once and is an ordinary CPU array from there on (`specification/flow.md:1193-1198`).

That route already works for geometry in one case. For a kernel that reads no cooked attributes, `sop/with_attr` with `(exact x)` dispatches on the GPU on the initial domain before the cook starts, and the cook worker receives a CPU array (`flow.md:1669-1672`, `lib/flow_sop/lower.ml:545-560`). A kernel that reads cooked attributes stays on the CPU today, because its input does not exist until a worker has cooked it and GPU work must run on the initial domain.

**Owner decision, 2026-10-12: geometry results may differ between devices.** So a ported node needs no author-written `(exact ...)`. The SOP input sink becomes an implicit readback point: placement picks CPU or GPU by measured cost, and a GPU result is read back automatically where it enters the graph. The explicit-`exact` rule stays for cache keys, saved state and exports. On one device the result stays deterministic: the same graph on the same placement gives the same bytes.

| Task | Depends | What | Gate |
| --- | --- | --- | --- |
| G.1 | C.7 | Input-independent kernels in ported nodes use the existing route. This covers generator positions, normals and UVs. Make \`Sop\_input\` an implicit readback sink in \`Flow\_ir.place\` and the checker (\`lib/flow\_ir/flow\_ir.ml:461-472\`, \`lib/flow/workspace.ml:1278-1279\`), then confirm a `defnode` body qualifies as a producer the way an inline `map` does. | A 1,000,000-point `grid` places its position kernel on the GPU when the measured cost wins; result within 32-bit tolerance of the CPU body |
| G.2 | B.F G.1 | Staged cook for kernels that read cooked attributes: after the input cooks, upload the attribute columns it reads on the initial domain, dispatch, read back, continue the cook. | The fused four-deformer chain at 1,000,000 points runs on the GPU when measured cost wins; before and after times recorded |

Rules for both tasks, all of which exist today:

- Placement is by measured cost, with a 1,024-element floor; an unmeasured backend keeps the CPU (`lib/flow_ir/flow_ir.ml:412-437`).
- Fixed-step runs, exports and reference comparisons always use the CPU, so artifacts stay deterministic.
- Kernels with reductions, gather, compaction or integer work stay on the CPU. That limits the GPU to tier 1 deformers and generator kernels in this round.
- The Verification gate is run against the CPU body. A GPU placement is checked against the CPU body within 32-bit tolerance.

**Status 2026-10-16.** G.1 is closed (see its row below and `results.md`). G.1 and G.2 are closed (see their rows below and `results.md`).

### What the compiler does with a ported chain

| Situation | Today | After B.F |
| --- | --- | --- |
| One Lisp node | One cook node, one kernel prepared once, 1,024-element blocks across domains. | Same |
| Several maps inside one body with the same count | Fused into one region when they share the count origin (`@sop-points`, `lib/flow_sop/attribute_kernel.ml:92-111`). | Same |
| Lisp node feeding a Lisp node | Two cook nodes, two passes, one intermediate geometry. | One kernel pass when the intermediate has exactly one consumer and no viewer, probe or cache pin reads it |
| Lisp node feeding an OCaml node | Two cook nodes. | Same: the OCaml node needs the cooked geometry |
| A node changes topology or element count | Its output starts a new count origin. | Fusion stops at that node |

### B.F acceptance

- A chain `transform` → `bend` → `noise_displace` → `color_by_height` on a 1,000,000-point grid cooks with one kernel run, measured by a run counter in the test.
- The fused result is byte-identical to the unfused chain.
- Displaying or probing an intermediate node splits the chain at that node, and the result does not change.
- The 64-register limit per kernel (`lib/flow/packed_ops.ml:5`) splits a chain that would exceed it; it never refuses the graph.
- Before and after wall time at 1,000,000 points is recorded in `specification/sop-lisp/results.md`.

### Limits a port must respect

| Limit | Value | Where |
| --- | --- | --- |
| Registers per kernel | 64 | `lib/flow/packed_ops.ml:5` |
| Kernel block size | 1,024 elements, 16 blocks per chunk | `lib/flow_ir/packed.ml:208-210` |
| Interpreter iteration bound | 4,096 for lists; packed loops are exempt | `lib/flow/eval.ml:799-800` |
| Call depth | 64 | `lib/flow/workspace.ml` (`E_DEPTH`) |
| Elements per `for` over geometry | 4,096 | `lib/sop/zone.ml:8` |
| Adjacency cache | 64 entries, keyed by topology id | `lib/rdk/core/topology_index.ml:158-166` |

## Risks and open questions

The owner approved this plan on 2026-10-12. Questions 1 and 4 were decided explicitly; questions 2, 3, 5 and 6 stand at their recommendations and can be revisited in task A.0. Each has a recommendation, and the plan above is written assuming the recommendation is accepted.

### Open questions for the owner

| # | Question | Recommendation | If the answer is no |
| --- | --- | --- | --- |
| 1 | **Float gate.** Ports cannot match OCaml floats bit for bit because the OCaml compiler fuses multiply-add on arm64. Accept a last-bits tolerance (4 ulp default) with exact integer data? | Decided yes by the owner on 2026-10-12. Lisp's unfused arithmetic becomes the definition, as `specification/flow.md:1229-1232` already states. Explicit FMA is feasible on web: an isolated exact BigInt prototype matches native `Float.fma` bits in 10,011 cases. | Reproduce each compiler-selected fused site by hand from disassembly. Automatic Mul/Add contraction would change the rounding contract and remains undecided; explicit FMA feasibility does not authorize it. |
| 2 | **Golden refresh.** Ports will change render-parity PNG bytes and path tracer digests. Allow a per-node refresh under recipe step 7? | Yes, one commit per node with the changed files listed. | No port can land that feeds a rendered golden. |
| 3 | **Delegation.** May a Lisp body call an OCaml-bodied kind for one of its modes (`snap_to_grid` calling `fuse`)? | Yes. The manifest records `(calls ...)` so export readiness stays visible. | At least 5 more nodes stay OCaml: snap\_to\_grid, peak, mountain, rest\_position, extract\_centroid. |
| 4 | **Built-ins backed by OCaml.** `sop/adjacency`, `sop/components`, `sop/remap` and the three `graph/` operators wrap existing OCaml code. Accept them as built-ins? (Originally: each would need a JavaScript twin. Corrected 2026-10-13: no twin; `lib/rdk` is compiled with js_of_ocaml later.) | Decided yes by the owner on 2026-10-12. They are 6 fixed primitives against 43 nodes unlocked. | Phases D and E have no array formulation with identical output; 43 nodes stay OCaml. |
| 5 | **The OCaml kernels after a port.** Keep `lib/rdk` kernels as test oracles, or delete them with the registration? | Keep them through the round. E.R lists which are reachable only from tests; deleting them is a separate decision. | Oracles would have to be frozen as checked-in data. |
| 6 | **Speed gate.** How much slower than the OCaml kernel may a Lisp body be at 1,000,000 elements? | Decided by the owner on 2026-10-13: no bound. Record every number and register the port; ratios above 2x are tracked as regressions to optimize later. | n/a |

### Risks

| Risk | Why it matters | Mitigation |
| --- | --- | --- |
| `defnode` cannot carry everything the PPX record does | Any parameter kind it misses blocks every node using that kind. Ramps and choice lists are the likely gaps. | A.2 starts with a census of parameter kinds across the 128 manifest entries. |
| `Rand.float_at` uses 63-bit native-int mixing | Bit-exact `rand/at` is easy natively and awkward in JavaScript, which has no 64-bit integers outside `BigInt`. | B.8 writes the JavaScript twin with paired 32-bit arithmetic and tests it against the same 100,000 indices. |
| Classification rests on partial reads for about a third of the nodes | A node may need more than its row says. | Recipe steps 1 and 2: read fully, move the node, record the move. E.R reports the final counts. |
| Sequential algorithms hidden in tier 3 | `scatter` builds an alias table sequentially; `sweep` and `polywire` transport frames along a curve; `separate_pieces` advances a cursor. | Ordered `scan` already exists on the CPU kernel tier. Where it is not enough the node moves to Phase E. |
| Large single nodes | `subdivide` 4,607 lines, `polywire` 1,434, `promote_attributes` 1,403, `attribute_interpolate` 1,209, `attribute_randomize` 1,040. | Each is its own task or the last in its batch, after the phase's simpler nodes have exercised the language. |
| Interpreter fallback hides a slow port | A body that fails to compile to a kernel silently runs in the tree walker at microseconds per element. | A `defnode` body that does not lower to the kernel tier is a check error, not a fallback. Text-array work is the one exception and is marked in the manifest. |
| Iteration bound | `fold` over `(range n)` is capped at 4,096 and needs a literal count. | D.3 adds `iterate`, whose count is a parameter. |
| The cook cache keys on node identity and facts | A Lisp body with wrong kernel facts caches wrongly or never. | A.3 derives `reads`, `writes` and `topology` from the body, and the gate's cache-identity check covers each node. |
| Scope growth | 28 language additions is a language release, not a port. | Each addition lists the nodes that need it. One with no remaining nodes is dropped. |

Before shipping any production `defnode` with text-array work, derive and
review its text execution exception in the manifest. B.3 currently supplies
structured compiled Core loops for text arrays, with strict numeric regions
still required and no silent interpreter fallback. The eventual marker must
describe that execution truthfully; it must not imply that a text body ran as
a numeric packed kernel. There is no production text-array port yet, so the
foundation alone does not change the production manifest.

### Not in this round

- Web export of `sop` and `scene` graphs.
- GPU placement of kernels that use reductions, gather, compaction or integer work.
- The 31 tier 5 nodes. The plan for them is to compile `lib/rdk` with js_of_ocaml for the web target, not to rewrite them. The same build serves the OCaml-backed built-ins.
- A nearest-point built-in, which would move six tier 5 nodes into reach.
- Deleting OCaml kernels.

## Sources

Everything in this plan was read from the repository at `dev` commit `df43b92c` on 2026-10-12. Nothing was built or run for it except `objdump -d` on already-built objects for the fused multiply-add counts.

| Topic | Files |
| --- | --- |
| Project rules | `AGENTS.md`, `lib/rdk/AGENTS.md`, `lib/sop_catalog/AGENTS.md`, `lib/rays_editor/AGENTS.md` |
| Language and kernels | `specification/flow.md`, `specification/core.md`, `specification/workspace/iteration.md`, `lib/flow/op.ml`, `workspace.ml`, `eval.ml`, `ty.ml`, `value.ml`, `packed_ops.ml`, `symbol.ml` |
| Kernel tier and placement | `lib/flow_ir/core.ml`, `core_region.ml`, `core_lower.ml`, `packed.ml`, `flow_ir.ml`, `flow_ir.mli`, `lib/flow_web/target.ml`, `lib/flow_gpu/emit.ml` |
| Lisp to SOP | `lib/flow_sop/operators.ml`, `attribute_kernel.ml`, `function_kernel.ml`, `lower.ml`, `catalog.ml`, `manifest.ml`, `value_lane.ml` |
| Node declaration | `specification/sop.md`, `ppx/ppx_rays/ppx_rays.ml`, `lib/sop/shapes.ml`, `attributes.ml`, `groups.ml`, `sop_topology.ml`, `custom.ml`, `node.ml`, `session.ml`, `lib/sop_catalog/sop_catalog.ml`, `flow_manifest.sexp` |
| Geometry core | `lib/rdk/core/geometry.ml`, `topology.ml`, `topology_index.ml`, `topology_remap.ml`, `attribute.mli`, `group.ml`, `packed.mli`, `kernel.ml`, `analysis.ml` |
| Node kernels | `lib/rdk/gen/`, `lib/rdk/mesh/`, `lib/rdk/attrib/`, `lib/rdk/curve/`, at the lines given in the inventory |
| Determinism | `lib/rays_math/rand.ml`, `noise.ml`, `parallel.mli` |
| Tests used as oracles | `lib/sop/test_nodes.ml`, `test_parallel_exact.ml`, `test/test_sop_catalog.ml`, `test/sop_render_parity.ml`, `test/lisp_sop/lisp_sop.ml`, `lib/rdk/test_support/rdk_test_support.ml`, `lib/flow_sop/test_attribute_kernel.ml`, `test/test_custom_sop.ml` |
| Working precedent | `sketches/flow_kernel.rays` |

Items searched for and not found: a `defnode` or `defsop` form; any catalog node with a Lisp body; integer array values; a kernel gather instruction; a named min or max reduction; a `loop` or `iterate` form; a module or namespace system; a public `Analysis.bounds` function; a function returning point neighbours as an array.

## Ledger

Tick a task when its gate passes. Record moved or blocked nodes under the task that found them.

- [x] A.0 — existing approved decisions recorded above; evidence in `results.md`.
- [x] A.1 — helper equality/ULP/integer gates passed; command and coverage in `results.md`.
- [x] A.2 — typed declaration, metadata/refusal and clash checks passed; evidence in `results.md`.
- [x] A.3 — public cache identity, parameter/typed slots including optional/rest Fn, strict kernel/exactness and scoped helper context-dependency gates passed; latest evidence in `results.md`.
- [x] A.4 — embedded sources/catalog/manifest integration, dependency gate and production save/reopen regression passed; promoted digest in `results.md`.
- [x] A.5 — structural card/inspector/gesture parity, actual menu addition and production source/editor rerun passed; evidence in `results.md`.
- [x] A.6 — complete production noise_displace port; oracle, reference/domain/grain, metadata, cache, refusal, native and million-point timing gates passed; evidence in results.md.
- [x] A.R — final check.exe --ship and @runtest-native passed after the integration audit; specs and declaration guidance updated.
- [x] B.1 — all-owner/type attribute round trips, strict query guards, native/web and final integration gates passed. The full vec2/3/4 Float-plane Record extension, actual menu/graph/cooked-source checks and fixed before/after costs are recorded in `planar-attribute-validation.md` and `results.md`.
- [x] B.2 — exact integer arrays/operators, native/JavaScript fixtures and one/eight-domain checks passed; final shipping/native/web gates passed.
- [x] B.3 — owned Text arrays, all-owner round trips and compiled structured Core/JavaScript checks passed; unsupported numeric producers retain strict refusals.
- [x] B.4 — owner counts and canonical unique-edge arrays match five native fixtures; focused language/source/graph gates passed.
- [x] B.5 — all-owner masks, ordered groups and native pattern rules pass round trips, cache and live-source gates. Optional affected-point promotion also passes native/reference/domain/grain, count-origin fusion, graph and JavaScript boundary checks; evidence in `results.md`.
- [x] B.6 — native metadata rename/delete/conflict, ordered groups and canonical P pass focused language and JavaScript gates; complete node ports remain B.12.
- [x] B.7 — scalar/rounding math, matrix/quaternion helpers and native four-ULP audit pass; explicit native-order cross/matrix FMA matches reference/CPU/JavaScript bits without automatic arithmetic contraction.
- [x] B.8 — 100,000 exact random keys and 300,054 native noise samples passed with strict reference/domain/JavaScript parity; evidence in results.md.
- [x] B.9 — named and CSR segmented reductions match ordered references at one/eight domains, including empty/malformed rows and captured compiled sources.
- [x] B.10 — every node of the row is registered (see the Status list below); gates and benches in its registration commit.
- [x] B.11 — all six whole setter ports passed native/reference/schema/cache/domain/grain/editor and fixed performance gates; final Set Color evidence in results.md.
- [x] B.12
- [x] B.13 — every node of the row is registered (see the Status list below); gates and benches in its registration commit.
- [x] B.14 — every node of the row is registered (see the Status list below); gates and benches in its registration commit.
- [x] B.15 — every node of the row is registered (see the Status list below); gates and benches in its registration commit.
- [x] B.16 — every node of the row is registered (see the Status list below); gates and benches in its registration commit.
- [x] B.F — adjacent Lisp point deformers fold into one kernel run at lowering (`Context_kernel.fusable`/`fuse`, `Lower.fused_view`). Eligibility is derived, never written: one `let*` body with a single `sop/with_attr in0 :P` binding, no `sop/*` call besides attr/with_attr/delete_attr/context, no reducer, fold, loop, error or state, no `if`/`cond`/`case` whose test reads anything but literals and parameters, no validation, scalar parameters only, static arguments, and the producer has exactly one reader and does not use a cook-context operator. The consumer reads the producer's `P` values (its outer `exact` dropped) and the producer's own cook is skipped. Gate: `test_lisp_fusion.ml`, four `noise_displace` nodes, one kernel run, bytes equal to four native passes at 1 and 4 domains, a reparameterized middle node recooks, and a second reader of an intermediate splits the chain. At 1,000,000 points the chain cooks in 0.30 s (1 domain) and 0.105 s (4 domains) fused and unfused alike: the cook is noise-bound, so the saving is the passes and intermediates, not wall time. `bend`, `peak`, `point_jitter`, `uv_transform`, `snap_to_grid` and `color_by_height` keep their validity scans and are not fused.
- [x] B.R — review closed 2026-10-16: ` `, ``, `-native`, `-web` and `` exit 0 at `-j 2` (failures found and fixed in 5928c9dd, e159b885, 0983c05f; `results.md`, "Closing gates").
- [x] C.1 — owned topology queries and required-array mesh construction match five native fixtures and authored packed reconstruction across domains/grains; cache, lazy errors/state, cards/menu, JavaScript host and production 2D refusal gates pass. Evidence in `geometry-arrays-validation.md` and `results.md`.
- [x] C.2 — numeric and immutable Text gathers pass typed bounds, lazy/error-order, ownership, strict/reference/domain, actual JavaScript/export/worker and GPU refusal checks; separate Text cost evidence remains above 2× and qualifies no whole port.
- [x] C.3 — exact identity/permutation/all-owner payload transfer, ordered groups, explicit edge ancestry, raw storage, strict/reference/domain/grain, cache/editor and native/JavaScript gates pass; original public Rewire bytes and before/after timings retained. Evidence in `payload-remap-validation.md` and `results.md`.
- [x] C.4 — ordered compact indices, exclusive Int offsets and scatter-add pass native/reference/Core/domain, ownership/error/state, geometry composition, graph/editor, actual JavaScript runtime/worker and final shipping gates; evidence in the three bulk-array validation notes and `results.md`.
- [x] C.5 — stable Int/Float/byte-Text grade and exact first-match Int/Text lookup pass native/reference/Core/domain, ownership/error/state, consumer, graph/editor, actual JavaScript runtime/worker and final shipping gates; evidence in `grade-index-validation.md` and `results.md`.
- [x] C.6 — six physical array types compose through standalone Core and invariant packed gathers, including scoped lazy count/captures, exact domains, graph defaults/edits and actual JavaScript workers. Final native/shipping gates pass; evidence in `array-composition-validation.md` and `results.md`.
- [x] C.7 — all eleven generators registered (2026-10-13/14): `line`, `points`, `grid`, `circle`, `box`, `uv_sphere`, `tube`, `torus`, `platonic`, `spiral`, `bound`; each passed the port gate against its OCaml oracle. `bound`, `grid` and `circle` are in the speed regressions table.
- [x] C.8 — every node of the row is registered (see the Status list below); gates and benches in its registration commit.
- [x] C.9 — every node of the row is registered (see the Status list below); gates and benches in its registration commit.
- [x] C.10 — every node of the row is registered (see the Status list below); gates and benches in its registration commit.
- [x] C.11 — every node of the row is registered (see the Status list below); gates and benches in its registration commit.
- [x] C.12 — every node of the row is registered (see the Status list below); gates and benches in its registration commit.
- [x] C.13 — every node of the row is registered (see the Status list below); gates and benches in its registration commit.
- [x] C.14 — every node of the row is registered (see the Status list below); gates and benches in its registration commit.
- [x] C.15 — every node of the row is registered (see the Status list below); gates and benches in its registration commit.
- [x] C.R — review closed 2026-10-16: ` `, ``, `-native`, `-web` and `` exit 0 at `-j 2` (failures found and fixed in 5928c9dd, e159b885, 0983c05f; `results.md`, "Closing gates").
- [x] D.1 — `sop/adjacency` equals `Topology_index.Private.view` on five fixtures, reference/compiled, 1 and 8 domains (`lib/flow_sop/test_adjacency.ml`, commit `6fc00ced`). Built ahead of C.R in parallel with the ports.
- [x] D.2 — `sop/components` ids equal `Analysis.classify_connectivity`; `:over "edges"` labels points, `:over "points"` labels primitives, optional 0/1 `:mask` (commit `7c28cf3b`).
- [x] D.3 — `iterate` with `:until`: one cook node, body region prepared once, graph card and gestures, reference/kernel and 1/8-domain bytes (`test_iterate.ml`, `test_iterate_graph.ml`, commit `b85f76da`). No wrap gesture yet; about 1.8 ms per pass at 10,000 points, unoptimized.
- [x] D.4 — every node of the row is registered (see the Status list below); gates and benches in its registration commit.
- [x] D.5 — every node of the row is registered (see the Status list below); gates and benches in its registration commit.
- [x] D.6 — every node of the row is registered (see the Status list below); gates and benches in its registration commit.
- [x] D.7 — every node of the row is registered (see the Status list below); gates and benches in its registration commit.
- [x] D.8 — every node of the row is registered (see the Status list below); gates and benches in its registration commit.
- [x] D.R — review closed 2026-10-16: ` `, ``, `-native`, `-web` and `` exit 0 at `-j 2` (failures found and fixed in 5928c9dd, e159b885, 0983c05f; `results.md`, "Closing gates").
- [x] E.1 — `graph/shortest_paths` over the shared `Rdk.Shortest_paths` heap search, JavaScript twin fixture-checked (commit `89cd7040`). Unreached points are distance -1, parent -1.
- [x] E.2 — `graph/chains` equals `Poly_path` traversal on five fixtures, JavaScript twin (commit `89cd7040`).
- [x] E.3 — `graph/color` equals `Graph_color.run` in all three connectivity modes, JavaScript twin (commit `89cd7040`). Record fields of the `graph/` and `sop/components` sources resolve separately, so each algorithm runs once per field read.
- [x] E.4 — every node of the row is registered (see the Status list below); gates and benches in its registration commit.
- [x] E.5 — every node of the row is registered (see the Status list below); gates and benches in its registration commit.
- [x] E.6 — every node of the row is registered (see the Status list below); gates and benches in its registration commit.
- [x] E.R — review closed 2026-10-16: ` `, ``, `-native`, `-web` and `` exit 0 at `-j 2` (failures found and fixed in 5928c9dd, e159b885, 0983c05f; `results.md`, "Closing gates").
- [x] G.1 — closed 2026-10-16: sink readback, `bulk_approx`, 32-bit fma, grid float-index planes (`port_tests grid --full` PASS), measured Metal cost; `lib/flow_sop/test_gpu_bulk.ml`, native `test_grid_gpu_native` (1M points: planes 8-11 ms GPU vs 11-19.5 ms CPU), see `results.md`.
- [x] G.2 — closed 2026-10-16: device service for worker-domain cooks (`Gpu.set_service`/`pump`/`available`), `Executor.force_into` device branch; `lib/flow_sop/test_gpu_bulk.ml` (second block), native `test_chain_gpu_native` (1M points: CPU 0.131-0.154 s, GPU 0.122-0.178 s, difference 2.9e-7), see `results.md`.

### Completed whole-node ports

Each checked node passed the full Verification recipe and has its own source
commit. Evidence and timings are in `results.md`. The other 112 inventory
nodes remain required; grouped phase milestones stay unchecked until every
node in that group has passed.

- [x] `noise_displace` — A.6 pilot.
- [x] `set_float` — B.11, all owners.
- [x] `set_int` — B.11, all owners.
- [x] `set_vector` — B.11, all owners.
- [x] `set_orient` — B.11, native quaternion parity.
- [x] `set_transform` — B.11, all 16 matrix fields.
- [x] `set_color` — B.11, all owners, group modes and Cd storage; raw exceptional bytes and all 78 timing rows pass.
- [x] `delete_edge_group` — B.12, exact literal names, metadata and graph/editor parity.
- [x] `rename_edge_group` — B.12, exact literal names and native error order, metadata and graph/editor parity.
- [x] `delete_attributes` — B.12, all owners/storages, optional references, native diagnostics, metadata and graph/editor parity.
- [x] `rename_attributes` — B.12, all owners/storages, conflicts and rewrite/error order, metadata and graph/editor parity.
- [x] `group_delete` — B.12, all four owners, unused groups, ordered rules and native error/schema/cache/graph parity.
- [x] `group_rename` — B.12, all owners and conflict modes, ordered unions, rewrite/error order and native schema/cache/graph parity.
- [x] `swap_attributes` — B.12, all owners, storage and copy/move/swap modes, canonical P, eager rewrite validation and native schema/cache/graph parity.
- [x] `line` — C.7, both output kinds, all fields, native FMA/error/capacity parity, actual production menu and all eight performance rows; native/shipping gates pass.
- [x] `points` — C.7, complete bounded zero-input generator, all four fields/counts/groups/raw names, native/reference/domain/cache/editor parity and all 48 fixed performance rows; native/shipping gates pass.

### Stop-request checkpoint — 2026-10-13

The owner requested finishing in-flight work, recording progress and stopping.
Accepted totals are 16/128 whole ports (12.5%), 112 remaining, and 25/60 milestones
(41.7%). C.6 array composition is committed as `193a28d7`; the full planar
attribute extension closes the current B.1 work with evidence in
`planar-attribute-validation.md`. This extension accepts no additional node or
milestone, and its direct Record/native-copy costs do not establish a whole
SOP performance pass.

Grid and Circle source recipes and verification helpers are prepared but
unaccepted: neither candidate is applied, timed or registered in production.
Their frozen source artifacts remain `/tmp/rays-grid-candidate.patch` and
`/tmp/rays-circle-candidate.patch`, with reviewed loader/menu/error/capacity
follow-ups. Their complete recipe, fixed performance tables and production
gates are still required if work resumes. Older Group Invert, Name From Groups
and Point Jitter drafts remain unaccepted in the worktree; payload-concat and
IEEE source proposals are also held. None contributes to the completed counts.

All helper agents are idle. No new port batch starts at this checkpoint.

### Draft tracker — resumed 2026-10-13

Process change: every port is verified by the table-driven gate in
`lib/flow_sop/port_tests/` (one `port_<key>.ml` registration per node; run
`main.exe <key>` and `main.exe <key> --bench`). A draft is `nodes/<key>.pending`
and is not registered. "Gate" below is the full Verification gate against the
OCaml oracle; "speed" is Lisp/native at 1,000,000 elements, 1 and 4 domains.
On 2026-10-13 the owner removed the 2× speed gate (decision 6): every draft
that passes the gate is registered, and its slowdown is recorded below as a
regression to optimize later.

Profile (reverse, compact_points, empty cook): the cost is runtime, not the
tree walker. Each `sop/mesh`/`sop/remap` host call lowers a synthetic
workspace; `sop/remap` built an unneeded edge index; `array/index_of` used a
generic hash; packed maps cost about 40 ns per element against about 5 ns for a
whole fused native pass. Three shared fixes are in (`3bffaa15`, `8ba568d7`,
`d445b404`). `8ba568d7` no longer validates an explicit `:edges` CSR when the
source has no edge groups. Projection: 2× is not reachable for reindexing
nodes without ancestry-aware host operators.

**Status 2026-10-16: 128/128 registered, B.R–E.R closed. Only the optional GPU tasks G.1 and G.2 remain.**
The earlier per-session tables are superseded; each registration commit
("Register …", "port-draft <key>: PASS-SLOW Nx") records its gate and bench.

Registered (Lisp body is the production node; the OCaml kernel stays in `lib/rdk` as the oracle):
`attr_from_image`, `attribute_blur`, `attribute_composite`, `attribute_copy`, `attribute_fade`, `attribute_interpolate`, `attribute_laplacian`, `attribute_noise_quaternion`, `attribute_noise`, `attribute_randomize`, `attribute_remap`, `bend`, `blast_by_attribute`, `blast`, `blend_shapes`, `bound`, `box`, `carve`, `circle_from_edges`, `circle`, `color_by_height`, `compact_points`, `connectivity`, `convert_line`, `copy_to_points`, `crease`, `delete_attributes`, `delete_edge_group`, `distance_along_geometry`, `distance_from_target`, `duplicate`, `edge_cusp`, `edge_divide`, `edge_equalize`, `edge_flip`, `edge_relax`, `edge_straighten`, `edge_transport_curves`, `edge_transport_parent`, `edge_transport`, `ends`, `enumerate`, `extract_centroid`, `extract_point_from_curve`, `graph_color`, `grid`, `group_backface`, `group_boundary_components`, `group_bounds`, `group_combine`, `group_copy`, `group_delete`, `group_edge_depth`, `group_edges`, `group_expand`, `group_find_path`, `group_from_attribute_boundary`, `group_invert`, `group_non_planar`, `group_normal`, `group_promote_boundary`, `group_promotions`, `group_random`, `group_range`, `group_ranges`, `group_rename`, `group_unshared`, `groups_from_name`, `iso_surface`, `line`, `match_axis`, `match_size`, `material`, `measure_curvature`, `measure`, `merge`, `mirror`, `mountain`, `name_from_groups`, `noise_displace`, `normals`, `ordered_group`, `peak`, `platonic`, `point_generate`, `point_jitter`, `point_replicate`, `point_split`, `point_velocity`, `points`, `poly_bridge`, `poly_loft`, `poly_path`, `polyframe`, `polywire`, `promote_attributes`, `rename_attributes`, `rename_edge_group`, `resample`, `rest_position`, `reverse`, `revolve`, `rewire_vertices`, `scatter`, `separate_pieces`, `set_color`, `set_float`, `set_int`, `set_orient`, `set_transform`, `set_vector`, `skin`, `smooth`, `snap_to_grid`, `sort`, `spiral`, `subdivide`, `swap_attributes`, `sweep_circle`, `sweep`, `torus`, `transform`, `tube`, `uv_auto_seam`, `uv_project`, `uv_sphere`, `uv_transform`, `uv_unitize`.

`@lib/sop/runtest` after the round (merge `045225ab`): the native-node suite (`run_nodes`) now
checks the Lisp bodies and found port bugs the quick gate missed: `attribute_copy` (unordered
groups), `snap_to_grid` (blank rules), `group_find_path` (primitive dual sized by point count),
`attribute_randomize` (register overflow in "Inside sphere cone"), `point_replicate` and
`Context_kernel.depends` (false seed dependencies), name-parameter facts of `attribute_noise`,
`attribute_noise_quaternion`, `normals`, `attr_from_image`. A declared `:facts` is now the node's
contract (`$param`, `$param|fallback`, `$param~prefix` name tokens). `noise3` takes 1 to 64 octaves
on the CPU and web runtime (Metal and WGSL keep 32). Declared four-ULP ports are compared with four
ULP or 1e-12 absolute (largest observed 2e-14). `run_nodes` takes about 70 minutes at `-j 2`.

`iso_surface` is proved by the `PORT_ISO=1` oracle harness (bit-exact positions and normals on 5 fields
× 6 parameter sets); the generic gate cannot feed a function slot.


Fidelity gap: `group_from_attribute_boundary` does not raise native's non-finite-attribute error
(the gate has no non-finite fixture).
`attribute_composite` refuses more than 8 rest-slot layers (`invalid_attribute_composite`); native has
no cap. A geometry that is a fold element or a dynamic `nth` is not a data source yet.
`blend_shapes` refuses more than 8 rest-slot shapes and reports `E_NONFINITE` for non-finite masks
(native: `invalid_blend_shapes`).
`attr_from_image` reads pixels through a new native carrier node `sop/image_texels` (one point per
texel, Point vec4 `rgba`); a blank or `P` attribute gives one combined message.
`copy_to_points` raises frame errors for every target (native: only targets with a piece in piece
mode) and treats a NaN `max` operand as non-finite; no gate case reaches either.
`subdivide` delegates partial primitive groups (crack closing) and meshes holding polylines to the
native carrier `sop/subdivide_cracks` (a copy of the native kind); curve refinement is not ported.
`scatter` with a non-finite density attribute fails `E_NONFINITE` at the language boundary instead of
native's `invalid_geometry`; its `--full` gate skipped most cases on the 4,225-point grid for time.

Compiler hazard (OCaml 5.3.0, arm64, found 2026-10-15): in `Attribute_pattern.escaped_character`,
`if s.[i] = c then … else Ok (s.[i], …)` let ocamlopt keep the address `s + i` across the
allocation of the `Ok` pair; a minor GC moved the minor-heap string and the stale address read a
clobbered byte (0.5% of glob class compiles, the flaky `E_ARRAY_RANGE` in `attribute_composite`).
Fixed by reading the byte into a `let` first (40fc746d, stress test in `test_rdk_sources.ml`).
Unaudited elsewhere: grep for a repeated `s.[i]` / `Bytes.get b i` inside an allocating
constructor after a guard on the same index.

Unexplained flake: `attribute_interpolate --full` failed once on "computed half / patterns vertex" and passed
on rerun (2026-10-15); suspect the arm64 GC hazard below first.

Known language limits found by the ports: `cond`/`if` over records ("Cooked selection cannot
compile record"); a host call whose data-dependent array arguments are shared with a second host
call (`E_KERNEL_FORM`); plan data inside `iterate` bodies is about 20x slower; `sop/adjacency` /
`sop/group` of a derived geometry inside a fold ("Topology source requires cooked geometry"); 2,048
terms per lowered term; 64 kernel registers; `E_SHADOW` forbids rebinding outer names.

Test-time regressions fixed 2026-10-14/15: exponential walks over shared checked subterms
(`Workspace.called_names`, `Context_kernel.map`, 365dc74a) hung the catalog tests; each Lisp
instance was lowered twice and re-specialized every prelude helper (d03a92ff), which took
`test_workspace_cook` from about 19 min to about 3 min. An explicit `~sources` library is pruned to
the reachable definitions unless `~library:true`.

Lost fusion (fixed 2026-10-15): a Lisp body with branches compiles to one opaque
`flow.geometry_select`, so the owned-node walk saw `topology=Changed`, `elementwise=None` and the
node broke point-count origins. `lower.ml` now takes the node's declared `:facts` (`:elementwise`,
`:topology`) when the walk cannot prove them, so `(sop/transform g :translate [0 0 1])` feeding an
attribute kernel is 3 fused stages again (`test_attribute_kernel.ml`). Cross-node fusion of whole
deformers is B.F above.

### Speed regressions accepted with the ports

Owner decision 6 (2026-10-13): these nodes are registered with Lisp bodies
although they are slower than the OCaml kernels they replace. Ratios are
Lisp/native wall time at about 1,000,000 elements, one cold cook, median of
three, 1 and 4 domains, from `port_tests/main.exe <key> --bench` on an Apple
M1. Most were measured while other agents loaded the machine (load average
20-60), so treat them as order-of-magnitude and re-measure on a quiet machine
before optimizing. The OCaml kernels remain in `lib/rdk` as oracles.

| Node | @1 | @4 | Cause class |
| --- | ---: | ---: | --- |
| `box` | 1.7× | 1.6× | within the old gate on a clean run |
| `group_random` | 2.3× | 3.7× | B |
| `distance_along_geometry` | 4.6× | 3.5× | D |
| `name_from_groups` | 4.9× | 3.0× | B, T |
| `point_jitter` | 5.7× | 31× | K, P |
| `compact_points` | 6.2× | 6.7× | H, K |
| `group_copy` | 6.5× | 5.5× | B, H |
| `distance_from_target` | 6.7× | 5.8× | K |
| `graph_color` | 8.0× | 8.8× | D, K |
| `color_by_height` | 11× | 13× | K |
| `match_axis` | 14× | 4.4× | K |
| `blast` | 15× | 16× | H, K |
| `uv_transform` | 20× | 15× | K |
| `enumerate` | 24× | 18× | K, T |
| `circle` | 24× | 40× | K, H |
| `group_invert` | 24× | 65× | B |
| `mirror` | 32× | 79× | H, K |
| `group_unshared` | 39× | 121× | B, A |
| `ends` | 49× | 65× | H, A |
| `edge_relax` | 53× | 97× | I, K |
| `attribute_blur` | 68× | 81× | I, K |
| `edge_transport_curves` | 69× | 55× | K, H |
| `edge_equalize` | 74× | 104× | I, K |
| `group_edge_depth` | 77× | 106× | I, B |
| `point_velocity` | 80× | 112× | K, H |
| `reverse` | 119× | 148× | H, K |
| `grid` | 164× | 114× | K, H; not profiled |
| `edge_transport` | 5.6× | | D |
| `poly_bridge` | 13× | 9× | K |
| `attribute_laplacian` | 12.8× | 13.6× | K, A |
| `measure` | 13.7× | 29.6× | K, P |
| `group_find_path` | 13.7× | 15.3× | D |
| `measure_curvature` | 15.8× | 15.3× | K, A |
| `sweep_circle` | 18.4× | 18.5× | K, H |
| `polywire` | 22.1× | 25.8× | K, H |
| `point_replicate` | 10.7× | 11.0× | not profiled |
| `promote_attributes` | 10.8× | 16.0× | not profiled |
| `extract_centroid` | 10.3× | 6.6× | not profiled |
| `edge_flip` | 10.9× | 9.8× | not profiled |
| `attr_from_image` | 3.0× | 3.9× | texels read through `sop/image_texels` carrier |
| `attribute_interpolate` | 11.8× | 22.6× | not profiled |
| `attribute_copy` | 15.2× | 8.2× | not profiled |
| `point_split` | 17.5× | 27.2× | not profiled |
| `match_size` | 23× | | K |
| `separate_pieces` | 39.3× | 31.6× | O(pieces²) placement scan |
| `skin` | 43× | 34× | K, H |
| `carve` | 53× | 47× | K, H |
| `group_from_attribute_boundary` | 82× | 228× | fold carries geometry; state kept as attributes |
| `scatter` | 28.4× | 73.6× | shared bindings re-evaluated per reader; split into carrier geometries |
| `attribute_composite` | 62.1× | 92.8× | per-input passes over attribute names |
| `blend_shapes` | 35.6× | 48.0× | rest slot unrolled to 8 |
| `copy_to_points` | 36.1× | 44.8× | pack instances through native `sop/pack_instances` carrier |
| `subdivide` | 53.2× | 46.8× | whole-mesh path in Lisp; partial groups and curves via `sop/subdivide_cracks` |
| `iso_surface` | 51.8× | 93× | field sampled into a carrier mesh; small kernels under the 64-register limit (harness `PORT_ISO=bench`) |
| `edge_transport_parent` | 85.8× | 75.7× | D, I |
| `poly_loft` | 90× | 108× | K, H |
| `group_boundary_components` | 163× | | A, D |
| `group_expand` | 226× | | I, A |
| `group_edges` | 239× | 550× | A, B |
| `ordered_group`, `groups_from_name` | >200× | | T |
| `poly_path` | 363× | 353× | D, H |
| `group_promotions` | 798× | | T, B |
| `sweep` | 2943× | 8684× | worst port: staged maps through point carriers; nested calls hit the lowering budget |
| `material` | n/a | n/a | not benched |

Cause classes, from the profile of `reverse`, `compact_points` and an empty
cook, and from the agents' per-node notes. Only H and K were measured; the
others are attributions to confirm.

- **K, kernel tier.** A packed `map` runs one loop per instruction over
  1,024-element blocks through scratch registers, scalarizes vectors, computes
  both sides of `if`, and each `map` materializes a full array (about 40-55 ns
  per element per map against about 5 ns for a whole fused native pass).
  Fixes: fuse adjacent maps (B.F), emit OCaml from `flow_ir` at build time for
  node kernels, GPU placement (G.1/G.2) for gather-free kernels.
- **H, host calls.** `sop/mesh`, `sop/remap`, `sop/delete_attr` lower a
  synthetic one-node workspace per call and marshal their array arguments
  twice for cache keys; reindexing bodies recompute edge ancestry through
  `array/index_of`. Three fixes are in (`3bffaa15`, `8ba568d7`, `d445b404`).
  Remaining: build the host node directly from evaluated arguments, an
  identity flag instead of identity index arrays, ancestry-aware reindex
  operators so edge ancestry is not recomputed.
- **B, bit-packed groups.** Native works on packed membership words; Lisp
  unpacks to one int per element, maps and repacks. Fix: word-level group
  operators, or a group-combine host operator.
- **A, adjacency.** `sop/adjacency` and `sop/edges` build a `Topology_index`
  with hash-based dedupe (about 170-200 ns per edge) and copy every array.
  Fix: sort-based dedupe, borrowed arrays.
- **I, iteration.** `iterate` runs the body region once per pass with fresh
  arrays each pass (about 1.8 ms per pass at 10,000 points). Fix: reuse
  buffers across passes, fuse the pass body.
- **D, record sources.** `graph/*` and `sop/components` results resolve once
  per field read, so the algorithm runs two or three times. Fix: share one
  evaluation across fields.
- **T, text.** Text arrays run on the structured Core loop, not a numeric
  kernel.
- **P, parallel scaling.** Lisp gains less from 4 domains than native, so the
  4-domain ratio is worse (16 blocks per chunk; small maps land in one chunk).

A fixed cost of roughly 15-20 ms per cook (workspace print and digest per host
call) was removed by `3bffaa15`; re-measure empty-input cooks.

### Remaining work after the first registration batch — 2026-10-13

Owner direction: the interpreter (block executor, host calls, checker) is made
solid and fast first. Compiled kernels are a later step through a trimmed LLVM
(ahead of time for shipped nodes; a runtime tier is undecided); no OCaml
emitter and no JIT in this round.

Executor pass, measured (Lisp seconds at 1,000,000 elements, 1 / 4 domains,
before → after `b401ec2b`, `6f026ad9`, `e233e907`): `grid` 3.36 / 1.19 →
1.85 / 0.87; `attribute_blur` 8.24 / 3.46 → 4.8 / 2.7; `color_by_height`
0.091 / 0.073 → 0.092 / 0.049; `distance_from_target` 0.279 / 0.133 →
0.275 / 0.087; `point_jitter` 0.534 / 0.224 → 0.525 / 0.182; `uv_transform`
0.106 / 0.037 → 0.098 / 0.033; `match_axis` and `circle` unchanged. The
executor costs about 2.5 ns per instruction per element, near a tight OCaml
loop, so cause class K is instruction count and repeated work, not dispatch.
Map-to-map materialization is about 10%. Reverted for no gain: per-domain
scratch reuse, 256-element blocks, constant prefill, specialized integer loops.

1. Interpreter speed, by expected payoff:
   1. One evaluation state across the regions of a node body, so attribute
      reads and reductions run once per cook (needs globally unique residual ids).
   2. Host calls built directly from evaluated arguments; no marshalled array
      cache keys; an identity flag instead of identity index arrays.
   3. Adjacency: faster `Topology_index` build, borrowed arrays, one evaluation
      for record sources (`graph/*`, `sop/components`).
   4. `iterate`: reuse buffers across passes.
   5. Word-level group operators.
   6. Scaling: parallel array built-ins and attribute reads; a GC policy
      (`o=200` took `circle` from 0.44 s to 0.34 s in one test).
   7. A checked-library cache: `check_workspace` costs 0.115 s on a trivial
      workspace against 0.011 s before the ports. Lowering is fixed (`044d3cb6`).
2. Interpreter correctness:
   - Register the four verified drafts held back: `blast_by_attribute`
     (`W_UNKNOWN_GROUP` for its temporary group in every workspace),
     `group_promote_boundary`, `rest_position`, `smooth` (they call OCaml-bodied
     kinds the web primitive fixtures do not know).
   - `connectivity` is approximate for UV on edges with three or more uses.
   - Non-finite attribute data reports `E_NONFINITE` where native reports its
     own code; refusal cases dropped from registrations before the gate lowered
     eagerly are restored.
   - Untested: the hand-edited `noise.js` golden; Metal/WGSL refusal of
     non-default `noise3` lacunarity and gain.
3. Language gaps that block ports: type-agnostic attribute operators
   (concatenate, float planes, weighted carry); segmented arg-max and folds
   with array-record state; attribute prepend; parameter-dependent `:facts`;
   canonical triangulation data (C.15); image sampling in a geometry body; a
   tree scan over a parent array.
4. Ports: 19 pending drafts and 65 nodes without a draft (tables above).
5. Closure: B.F, the review tasks B.R/C.R/D.R/E.R with `check.exe --ship`, and
   the final `results.md` write-up. New ports wait for 1.1 and 1.2.

Interpreter speed 1.2 and 1.1, measured (Lisp seconds at 1,000,000 elements,
1 / 4 domains, baseline → after `35cd2f33`, `73d1a0ed`, `490a8954`; full port
gate and focused aliases exit 0): `reverse` 1.0 / 0.82 → 0.58 / 0.42;
`compact_points` 0.22 / 0.19 → 0.054 / 0.042; `blast` 0.99 / 0.72 →
0.71 / 0.50; `grid` 2.1 / 1.0 → 1.35 / 0.39; `circle` 0.4 / 0.2 →
0.205 / 0.07; `ends` 1.45 / 1.27 → 0.94 / 0.79; `color_by_height`
0.092 / 0.052 → 0.073 / 0.038; `mirror`, `uv_transform`,
`distance_from_target` unchanged. Host calls now build `sop/mesh`,
`sop/mesh_planes` and `sop/remap` nodes directly; large array constants are
shared by identity; forced captures are shared inside one outermost run.
Not done: an identity option for `sop/remap` (estimated under 3% of
`reverse`, unmeasured), a direct build for `sop/delete_attr`, sharing between
two separate outermost runs in one body. Remaining top costs: kernel passes in
`grid`; edge index, `array/index_of` and gathers in `reverse`; `mirror` is
not profiled.
