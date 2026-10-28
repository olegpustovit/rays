
val run_checked :
  ?cancel:Cancel.t ->
  ?grain:int ->
  ?source_primitives:Group.t ->
  ?collision_primitives:Group.t ->
  ?tolerance:float ->
  ?include_coplanar:bool ->
  ?intersecting_group:string option ->
  ?intersections_attribute:string ->
  ?count_attribute:string ->
  ?self_intersecting_group:string ->
  ?self_intersections_attribute:string ->
  ?self_count_attribute:string ->
  collision:Geometry.t ->
  Geometry.t ->
  (Geometry.t, Error.t) result
(** Detect source/collision polygon intersections without changing source
    topology or payload. Both inputs are deterministically triangulated only
    inside packed surface indexes. [source_primitives] and
    [collision_primitives] restrict their respective inputs. Touching and,
    by default, coplanar-overlapping triangles count as intersections within
    the non-negative world-space [tolerance].

    [intersecting_group] defaults to the source primitive group
    [boolean_intersections]; pass [None] to omit it. The optional packed
    integer-array [intersections_attribute] stores the sorted unique collision
    primitive numbers intersecting each source primitive, while
    [count_attribute] stores each row length. At least one output is required.
    Existing same-name metadata is replaced atomically.

    [self_intersecting_group], [self_intersections_attribute], and
    [self_count_attribute] request the corresponding AxA results on the source
    surface. Every unordered triangle pair is tested once, triangulation pairs
    from one source primitive are omitted, and ordinary contacts at shared
    vertices/edges are suppressed. Genuine overlap beyond a shared boundary
    and duplicate faces remain intersections. Self lists are symmetric: if
    primitive [a] names [b], [b] names [a].

    Surface construction costs O((A + B) log(A + B)); candidate traversal is
    expected O((A + B) log B + C), narrow-phase triangle testing is O(C), and output
    sorting is O(sum k log k) over source rows. Auxiliary storage is
    O(A + B + C + I), where [C] is the BVH candidate count and [I] the retained
    primitive-pair payload. Broad phase, narrow testing, row sorting, and packed group output
    use deterministic disjoint parallel ranges. *)
