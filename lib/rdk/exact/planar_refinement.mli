(** Bounded deterministic quality refinement for a planar constrained Delaunay
    triangulation. *)

type t

val build :
  ?cancel:Cancel.t ->
  grain:int ->
  initial_points:Implicit_point.t array ->
  initial_triangle_points:int array ->
  constraint_points:int array ->
  constraint_winding:int array ->
  minimum_angle:float option ->
  maximum_area:float option ->
  target_edge_length:float option ->
  minimum_edge_length:float ->
  maximum_new_points:int ->
  allow_constraint_splitting:bool ->
  regularization_steps:int ->
  allow_movement_of_interior_input_points:bool ->
  unit ->
  (t, string) result
(** Refine in deterministic stable batches. Scale-normalized rounded
    circumcenters are accepted only when exact predicates certify that the
    resulting binary64 construction remains inside its parent face; otherwise
    a rounded face centroid is used. When
    constraint splitting is allowed, an encroached segment is replaced by its
    exact midpoint children before updating the already clipped shared CDT.
    Every accepted point is certified inside its parent retained face, so
    outside classification does not need to be replayed per generation.
    [minimum_angle]
    is in radians. [minimum_edge_length] gates a bad face by its longest edge.
    [maximum_new_points] is a hard termination bound. Regularization moves
    generated interior points, and optionally initial interior points, toward
    their one-ring neighbor centroid. Constraint and hull points remain fixed;
    every simultaneous move is exact-predicate certified before CDT repair. *)

val new_point_count : t -> int
val triangle_points : t -> int array
val constraint_points : t -> int array
val point_provenance_ref : t -> int -> int
(** Root value reference for a generated output point. References below the
    initial point count name initial points; later references name provenance
    nodes in stable construction order. *)

val provenance_node_count : t -> int
val provenance_parent_first : t -> int -> int
val provenance_parent_second : t -> int -> int
val provenance_parent_third : t -> int -> int
val provenance_parent_weights : t -> int -> float * float * float

module View : sig
  val approximate_x : t -> float array
  val approximate_y : t -> float array
  (** Borrowed final projected-coordinate planes for audited adapters. *)
end
