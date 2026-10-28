(** Deterministic, target-independent geometry analysis. *)

val signed_volume : Geometry.t -> float
(** Divergence-theorem sum over polygon fans: meaningful for consistently wound closed shells, exact
    for non-convex faces. *)

val classify_connectivity :
  ?primitives:Group.t ->
  ?points:Group.t ->
  ?seams:Edge_group.t ->
  Attribute.owner ->
  Geometry.t ->
  (int array * int, Error.t) result
(** Classify point or primitive connected components. Primitive and point include groups behave as
    deleted topology and excluded output elements are [-1]. Point mode joins topology edges and may
    omit native [seams]. Primitive mode normally joins shared-point incidence; a native seam group
    instead joins only across non-seam shared edges.

    Default classification is O(points + vertices + primitives). Seam/UV mode is O(vertices +
    sum(edge incidence squared)) for non-manifold edges and linear on manifold topology. Auxiliary
    memory is O(points + primitives) plus the shared topology index. IDs are compact and assigned by
    the first included output element in each component. *)
